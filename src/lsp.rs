use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, LazyLock, RwLock},
};

use extension_fn::extension_fn;
use regex::Regex;
use tower_lsp::{
    Client, LanguageServer, async_trait,
    jsonrpc::Error,
    lsp_types::{
        CompletionItem, CompletionItemKind, CompletionOptions, CompletionOptionsCompletionItem,
        CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
        Position, Range, SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
        SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
        WorkDoneProgressOptions,
    },
};

use crate::{
    ast::{
        AddOrSubExpression, AndExpression, ElseOrElseIf, EntityID, EqualsOrNotEqualsExpression,
        Expression, Factor, LessOrGreaterExpression, MulOrDivExpression, NewExpression,
        OrExpression, Primary, PrimaryLeft, Program, ReturnExpression, Spanned, Statement,
        TypeInfo,
    },
    future::SharedManualFuture,
    lexer::{Lexer, TokenKind},
    name::{NameResolverContainer, resolve_name_for_program},
    parser::parse_program,
    report::ToReport,
    semantics::validate_semantics_for_program,
    types::{Type, TypeEnvironment, infer_type_for_program},
};

pub struct ToystLanguageServer {
    client: Client,
    map: RwLock<HashMap<Url, SharedManualFuture<ToystASTInfo>>>,
}

pub struct ToystASTInfo {
    pub source: String,
    pub tokens: Vec<(Position, usize, SemanticTokenType)>,
    pub completions: Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
}

static SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::TYPE,
];
static SEMANTIC_TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[];

#[async_trait]
impl LanguageServer for ToystLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult, Error> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: Some(false),
                            },
                            legend: SemanticTokensLegend {
                                token_types: SEMANTIC_TOKEN_TYPES.iter().cloned().collect(),
                                token_modifiers: SEMANTIC_TOKEN_MODIFIERS.iter().cloned().collect(),
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![".".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(true),
                    },
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(false),
                    }),
                }),
                ..Default::default()
            },
            server_info: None,
        })
    }

    async fn shutdown(&self) -> Result<(), Error> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.text_sync(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        for event in params.content_changes {
            self.text_sync(params.text_document.uri.clone(), event.text)
                .await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>, Error> {
        let info = {
            let map = self.map.read().unwrap();
            let Some(info) = map.get(&params.text_document.uri).cloned() else {
                return Ok(None);
            };
            info
        };

        let info = info.get().await;
        let mut tokens = Vec::new();

        let mut prev_line = 0;
        let mut prev_column = 0;
        for (position, length, ty) in info.tokens.iter() {
            if position.line != prev_line {
                prev_column = 0;
            }

            tokens.push(SemanticToken {
                delta_line: position.line - prev_line,
                delta_start: position.character - prev_column,
                length: *length as _,
                token_type: SEMANTIC_TOKEN_TYPES
                    .iter()
                    .position(|token_type| token_type == ty)
                    .unwrap_or(0) as _,
                token_modifiers_bitset: 0,
            });

            prev_line = position.line;
            prev_column = position.character;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>, Error> {
        let info = {
            let map = self.map.read().unwrap();
            let Some(info) = map
                .get(&params.text_document_position.text_document.uri)
                .cloned()
            else {
                return Ok(None);
            };
            info
        };

        let info = info.get().await;

        let position = params
            .text_document_position
            .position
            .to_byte_position(&info.source);

        let mut completions = Vec::new();
        for (range, items) in info.completions.iter() {
            if (range.start..=range.end).contains(&position) {
                for (text, kind) in items.iter() {
                    completions.push(CompletionItem {
                        label: text.clone(),
                        kind: Some(*kind),
                        insert_text: Some(text.clone()),
                        ..Default::default()
                    });
                }
            }
        }

        completions.push(CompletionItem {
            label: "print".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            insert_text: Some("print".to_string()),
            ..Default::default()
        });
        completions.push(CompletionItem {
            label: "print_int".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            insert_text: Some("print_int".to_string()),
            ..Default::default()
        });

        Ok(Some(CompletionResponse::Array(completions)))
    }
}

impl ToystLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            map: RwLock::new(HashMap::new()),
        }
    }

    async fn text_sync(&self, url: Url, source: String) {
        let future = SharedManualFuture::new();
        {
            let mut map = self.map.write().unwrap();
            map.insert(url.clone(), future.clone());
        }

        let mut lexer = Lexer::new(source.as_str());
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &mut errors, &[]);

        let mut diagnostics = Vec::new();
        for error in errors {
            diagnostics.push(Diagnostic {
                range: error.span.to_lsp_span(&source),
                severity: Some(DiagnosticSeverity::ERROR),
                message: error.to_report(url.as_str()).0,
                ..Default::default()
            });
        }

        let mut errors = Vec::new();
        validate_semantics_for_program(&ast, &mut errors);

        for error in errors {
            diagnostics.push(Diagnostic {
                range: error.span.to_lsp_span(&source),
                severity: Some(DiagnosticSeverity::ERROR),
                message: error.to_report(url.as_str()).0,
                ..Default::default()
            });
        }

        let mut container = NameResolverContainer::new();
        let resolver = container.new_resolver(None);
        let mut errors = Vec::new();
        let mut resolved_map = HashMap::new();

        resolve_name_for_program(
            &ast,
            &mut errors,
            resolver,
            &mut container,
            &mut resolved_map,
        );

        for error in errors {
            diagnostics.push(Diagnostic {
                range: error.span.to_lsp_span(&source),
                severity: Some(DiagnosticSeverity::ERROR),
                message: error.to_report(url.as_str()).0,
                ..Default::default()
            });
        }

        let mut env = TypeEnvironment::new();
        let mut errors = Vec::new();
        infer_type_for_program(
            &ast,
            &resolved_map,
            &Spanned::new(Type::Void, 0..source.len()),
            &mut env,
            &mut errors,
        );

        for error in errors {
            diagnostics.push(Diagnostic {
                range: error.span.to_lsp_span(&source),
                severity: Some(DiagnosticSeverity::ERROR),
                message: error.to_report(url.as_str()).0,
                ..Default::default()
            });
        }

        self.client
            .publish_diagnostics(url, diagnostics, None)
            .await;

        let mut tokens = BTreeMap::new();
        for token in Lexer::new(&source).enable_comment_token() {
            let token_type = match token.kind {
                TokenKind::Class => SemanticTokenType::KEYWORD,
                TokenKind::Or => SemanticTokenType::KEYWORD,
                TokenKind::And => SemanticTokenType::KEYWORD,
                TokenKind::Let => SemanticTokenType::KEYWORD,
                TokenKind::Function => SemanticTokenType::KEYWORD,
                TokenKind::New => SemanticTokenType::KEYWORD,
                TokenKind::If => SemanticTokenType::KEYWORD,
                TokenKind::Else => SemanticTokenType::KEYWORD,
                TokenKind::Return => SemanticTokenType::KEYWORD,
                TokenKind::StringLiteral => SemanticTokenType::STRING,
                TokenKind::NumericLiteral => SemanticTokenType::NUMBER,
                TokenKind::Comment => SemanticTokenType::COMMENT,
                _ => continue,
            };
            tokens.insert(token.span.start, (token.span.clone(), token_type));
        }

        let type_map = env.dump();
        let mut completions = Vec::new();

        collect_analyze_info_for_program(
            &ast,
            ast.span.clone(),
            &type_map,
            &mut tokens,
            &mut completions,
        );

        future
            .complete(Arc::new(ToystASTInfo {
                tokens: tokens
                    .into_values()
                    .map(|(span, ty)| {
                        (
                            to_line_column(&source, span.start),
                            source[span.clone()].chars().count(),
                            ty,
                        )
                    })
                    .collect(),
                source,
                completions,
            }))
            .await;
    }
}

#[extension_fn(Position)]
fn to_byte_position(&self, code: &str) -> usize {
    for (line, line_matched) in LINE_REGEX.find_iter(code).enumerate() {
        if line as u32 == self.line {
            let column_byte = line_matched
                .as_str()
                .char_indices()
                .nth(self.character as _)
                .map(|(position, _)| position)
                .unwrap_or(line_matched.len());

            return line_matched.start() + column_byte;
        }
    }

    code.len()
}

#[extension_fn(std::ops::Range<usize>)]
fn to_lsp_span(&self, code: &str) -> Range {
    Range {
        start: to_line_column(code, self.start),
        end: to_line_column(code, self.end),
    }
}

static LINE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[^\n\r]*(\n|\r\n|$)").unwrap());

fn to_line_column(code: &str, byte: usize) -> Position {
    let mut last_line_index = 0;
    let mut last_line = "";
    for (line, line_matched) in LINE_REGEX.find_iter(code).enumerate() {
        if (line_matched.start()..line_matched.end()).contains(&byte) {
            let column = code[line_matched.start()..byte].chars().count();

            return Position::new(line as _, column as _);
        }

        last_line_index = line;
        last_line = &code[line_matched.start()..line_matched.end()];
    }

    Position::new(
        last_line_index as _,
        last_line
            .chars()
            .filter(|&char| char != '\n' && char != '\r')
            .count() as _,
    )
}

fn collect_analyze_info_for_program(
    ast: &Program,
    block_span: std::ops::Range<usize>,
    type_map: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    let mut items = Vec::new();
    for statement in ast.statements.iter() {
        match statement {
            Statement::LetStatement(let_statement) => {
                if let Some(name) = &let_statement.name {
                    items.push((name.value.to_string(), CompletionItemKind::VARIABLE));
                }
            }
            Statement::FunctionDefine(function_define) => {
                if let Some(name) = &function_define.name {
                    items.push((name.value.to_string(), CompletionItemKind::FUNCTION));
                }
            }
            Statement::ClassDefine(class_define) => {
                if let Some(name) = &class_define.name {
                    items.push((name.value.to_string(), CompletionItemKind::CLASS));
                }
            }
            _ => {}
        }
    }
    completions.push((block_span, items));

    for statement in ast.statements.iter() {
        match statement {
            Statement::LetStatement(let_statement) => {
                if let Some(name) = &let_statement.name {
                    tokens.insert(
                        name.span.start,
                        (name.span.clone(), SemanticTokenType::VARIABLE),
                    );
                }

                if let Some(expression) = &let_statement.expression {
                    collect_analyze_info_for_expression(expression, type_map, tokens, completions);
                }
            }
            Statement::Assignment(assignment) => {
                collect_analyze_info_for_primary(&assignment.left, type_map, tokens, completions);

                if let Some(right) = &assignment.right {
                    collect_analyze_info_for_expression(right, type_map, tokens, completions);
                }
            }
            Statement::Expression(expression) => {
                collect_analyze_info_for_expression(expression, type_map, tokens, completions);
            }
            Statement::FunctionDefine(function_define) => {
                if let Some(name) = &function_define.name {
                    tokens.insert(
                        name.span.start,
                        (name.span.clone(), SemanticTokenType::FUNCTION),
                    );
                }

                for argument in function_define.arguments.iter() {
                    tokens.insert(
                        argument.name.span.start,
                        (argument.name.span.clone(), SemanticTokenType::VARIABLE),
                    );
                    collect_analyze_info_for_type_info(
                        &argument.type_info,
                        type_map,
                        tokens,
                        completions,
                    );
                }

                if let Some(return_type) = &function_define.return_type {
                    collect_analyze_info_for_type_info(return_type, type_map, tokens, completions);
                }

                if let Some(block) = &function_define.block {
                    collect_analyze_info_for_program(
                        &block.program,
                        block.span.clone(),
                        type_map,
                        tokens,
                        completions,
                    );
                }
            }
            Statement::ClassDefine(class_define) => {
                if let Some(name) = &class_define.name {
                    tokens.insert(
                        name.span.start,
                        (name.span.clone(), SemanticTokenType::TYPE),
                    );
                }

                for field in class_define.fields.iter() {
                    tokens.insert(
                        field.name.span.start,
                        (field.name.span.clone(), SemanticTokenType::PROPERTY),
                    );
                    collect_analyze_info_for_type_info(
                        &field.type_info,
                        type_map,
                        tokens,
                        completions,
                    );
                }
            }
            Statement::IfStatement(if_statement) => {
                if let Some(condition) = &if_statement.first.condition {
                    collect_analyze_info_for_expression(condition, type_map, tokens, completions);
                }
                if let Some(block) = &if_statement.first.block {
                    collect_analyze_info_for_program(
                        &block.program,
                        block.span.clone(),
                        type_map,
                        tokens,
                        completions,
                    );
                }

                for chain in if_statement.chain.iter() {
                    match chain {
                        ElseOrElseIf::Else { block, span: _ } => {
                            if let Some(block) = block {
                                collect_analyze_info_for_program(
                                    &block.program,
                                    block.span.clone(),
                                    type_map,
                                    tokens,
                                    completions,
                                );
                            }
                        }
                        ElseOrElseIf::ElseIf {
                            condition,
                            block,
                            span: _,
                        } => {
                            if let Some(condition) = condition {
                                collect_analyze_info_for_expression(
                                    condition,
                                    type_map,
                                    tokens,
                                    completions,
                                );
                            }
                            if let Some(block) = block {
                                collect_analyze_info_for_program(
                                    &block.program,
                                    block.span.clone(),
                                    type_map,
                                    tokens,
                                    completions,
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

fn collect_analyze_info_for_expression(
    ast: &Expression,
    type_map: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    match ast {
        Expression::OrExpression(or_expression) => {
            collect_analyze_info_for_or_expression(or_expression, type_map, tokens, completions);
        }
        Expression::NewExpression(new_expression) => {
            collect_analyze_info_for_new_expression(new_expression, type_map, tokens, completions);
        }
        Expression::ReturnExpression(return_expression) => {
            collect_analyze_info_for_return_expression(
                return_expression,
                type_map,
                tokens,
                completions,
            );
        }
    }
}

fn collect_analyze_info_for_new_expression(
    ast: &NewExpression,
    type_map: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    if let Some(name) = &ast.name {
        tokens.insert(
            name.span.start,
            (name.span.clone(), SemanticTokenType::TYPE),
        );
    }

    if let Some(ty) = type_map.get(&EntityID::from(ast)) {
        if let Type::Class {
            entity_id: _,
            name: _,
            fields,
            span: _,
        } = ty
        {
            let fields = fields.lock().unwrap();

            completions.push((
                ast.span.clone(),
                fields
                    .iter()
                    .map(|(name, _)| (name.value.to_string(), CompletionItemKind::PROPERTY))
                    .collect(),
            ));
        }
    }

    for field_assign in ast.field_assign.iter() {
        tokens.insert(
            field_assign.name.span.start,
            (field_assign.name.span.clone(), SemanticTokenType::PROPERTY),
        );

        collect_analyze_info_for_expression(
            &field_assign.expression,
            type_map,
            tokens,
            completions,
        );
    }
}

fn collect_analyze_info_for_return_expression(
    ast: &ReturnExpression,
    type_map: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    if let Some(expression) = &ast.expression {
        collect_analyze_info_for_expression(&expression, type_map, tokens, completions);
    }
}

macro_rules! collect_analyze_info_for_2op {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            type_map: &HashMap<EntityID, Type>,
            tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
            completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
        ) {
            $next(&ast.first, type_map, tokens, completions);
            for (_, chain) in ast.chain.iter() {
                $next(chain, type_map, tokens, completions);
            }
        }
    };
}

collect_analyze_info_for_2op!(
    collect_analyze_info_for_or_expression,
    OrExpression,
    collect_analyze_info_for_and_expression
);
collect_analyze_info_for_2op!(
    collect_analyze_info_for_and_expression,
    AndExpression,
    collect_analyze_info_for_equals_expression
);
collect_analyze_info_for_2op!(
    collect_analyze_info_for_equals_expression,
    EqualsOrNotEqualsExpression,
    collect_analyze_info_for_less_expression
);
collect_analyze_info_for_2op!(
    collect_analyze_info_for_less_expression,
    LessOrGreaterExpression,
    collect_analyze_info_for_add_expression
);
collect_analyze_info_for_2op!(
    collect_analyze_info_for_add_expression,
    AddOrSubExpression,
    collect_analyze_info_for_mul_expression
);
collect_analyze_info_for_2op!(
    collect_analyze_info_for_mul_expression,
    MulOrDivExpression,
    collect_analyze_info_for_factor
);

fn collect_analyze_info_for_factor(
    ast: &Factor,
    type_map: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    if let Some(primary) = &ast.primary {
        collect_analyze_info_for_primary(primary, type_map, tokens, completions);
    }
}

fn collect_analyze_info_for_primary(
    ast: &Primary,
    type_map: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    completions: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    match &ast.first {
        PrimaryLeft::Literal {
            literal,
            function_call,
        } => {
            if let Some(function_call) = function_call {
                for argument in function_call.arguments.iter() {
                    collect_analyze_info_for_expression(argument, type_map, tokens, completions);
                }
            }

            match function_call {
                Some(_) => {
                    tokens.insert(
                        literal.span.start,
                        (literal.span.clone(), SemanticTokenType::FUNCTION),
                    );
                }
                None => {
                    tokens.insert(
                        literal.span.start,
                        (literal.span.clone(), SemanticTokenType::VARIABLE),
                    );
                }
            }
        }
        PrimaryLeft::NumericLiteral { literal } => {
            tokens.insert(
                literal.span.start,
                (literal.span.clone(), SemanticTokenType::NUMBER),
            );
        }
        PrimaryLeft::StringLiteral { literal } => {
            tokens.insert(
                literal.span.start,
                (literal.span.clone(), SemanticTokenType::STRING),
            );
        }
    }

    for chain in ast.chain.iter() {
        tokens.insert(
            chain.span.start,
            (chain.span.clone(), SemanticTokenType::PROPERTY),
        );
    }

    let mut prev_type = type_map
        .get(&EntityID::from(&ast.first))
        .unwrap_or(&Type::Unknown);

    let mut last_chain_position = ast.first.span().end;
    for chain in ast.chain.iter() {
        if let Type::Class {
            entity_id: _,
            name: _,
            fields,
            span: _,
        } = prev_type
        {
            let fields = fields.lock().unwrap();

            completions.push((
                last_chain_position..chain.span.end,
                fields
                    .iter()
                    .map(|(name, _)| (name.value.to_string(), CompletionItemKind::PROPERTY))
                    .collect(),
            ));
        }

        prev_type = type_map
            .get(&EntityID::from(chain))
            .unwrap_or(&Type::Unknown);
        last_chain_position = chain.span.end;
    }

    // object.field. の形式だった場合
    if last_chain_position < ast.span.end {
        if let Type::Class {
            entity_id: _,
            name: _,
            fields,
            span: _,
        } = prev_type
        {
            let fields = fields.lock().unwrap();

            completions.push((
                last_chain_position..ast.span.end,
                fields
                    .iter()
                    .map(|(name, _)| (name.value.to_string(), CompletionItemKind::PROPERTY))
                    .collect(),
            ));
        }
    }
}

fn collect_analyze_info_for_type_info(
    ast: &TypeInfo,
    _: &HashMap<EntityID, Type>,
    tokens: &mut BTreeMap<usize, (std::ops::Range<usize>, SemanticTokenType)>,
    _: &mut Vec<(std::ops::Range<usize>, Vec<(String, CompletionItemKind)>)>,
) {
    tokens.insert(
        ast.name.span.start,
        (ast.name.span.clone(), SemanticTokenType::TYPE),
    );
}
