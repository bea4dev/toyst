use crate::{
    ast::{
        AddOrSub, AddOrSubExpression, AndExpression, Assignment, Block, ClassDefine, ClassField,
        ElseOrElseIf, EqualsOrNotEqualsExpression, EqualsOrNotEqusls, Expression, Factor,
        FieldAssign, FunctionArgument, FunctionCall, FunctionDefine, IfBranch, IfStatement,
        LessOrGreaterExpression, LessOrGreaterThan, LetStatement, Literal, MulOrDiv,
        MulOrDivExpression, NewExpression, OrExpression, Primary, PrimaryLeft, PrimaryRight,
        Program, ReturnExpression, Spanned, Statement, TypeInfo,
    },
    error::{ParseError, ParseErrorKind},
    lexer::{GetKind, Lexer, TokenKind},
};

/// リカバリ用
/// 特定のトークンが出るまでトークンを捨てる
fn recover_until<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
    kind: ParseErrorKind,
    until: &[TokenKind],
) {
    let anchor = lexer.cast_anchor();

    loop {
        let token_kind = lexer.current().get_kind();

        if token_kind == TokenKind::None {
            break;
        }

        if until.contains(&token_kind) {
            break;
        }

        lexer.next();
    }

    errors.push(ParseError {
        kind,
        span: anchor.elapsed(lexer),
    });
}

pub fn parse_program<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
    until: &[TokenKind],
) -> Box<Program<'input>> {
    let anchor = lexer.cast_anchor();

    let mut statements = Vec::new();

    loop {
        if until.contains(&lexer.current().get_kind()) {
            break;
        }
        if lexer.current().get_kind() == TokenKind::None {
            break;
        }

        let Some(statement) = parse_statement(lexer, errors) else {
            break;
        };
        statements.push(statement);
    }

    Box::new(Program {
        statements,
        span: anchor.elapsed(lexer),
    })
}

fn parse_statement<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<Statement<'input>> {
    if let Some(let_statement) = parse_let_statement(lexer, errors) {
        validate_smicolon(lexer, errors);
        return Some(Statement::LetStatement(let_statement));
    }
    if let Some(assignment) = parse_assignment(lexer, errors) {
        validate_smicolon(lexer, errors);
        return Some(Statement::Assignment(assignment));
    }
    if let Some(expression) = parse_expression(lexer, errors) {
        validate_smicolon(lexer, errors);
        return Some(Statement::Expression(expression));
    }
    if let Some(function_define) = parse_function_define(lexer, errors) {
        return Some(Statement::FunctionDefine(function_define));
    }
    if let Some(class_define) = parse_class_define(lexer, errors) {
        return Some(Statement::ClassDefine(class_define));
    }
    if let Some(if_statement) = parse_if_statement(lexer, errors) {
        return Some(Statement::IfStatement(if_statement));
    }

    None
}

fn validate_smicolon<'input>(lexer: &mut Lexer<'input>, errors: &mut Vec<ParseError>) {
    if lexer.current().get_kind() == TokenKind::SemiColon {
        lexer.next();
    } else {
        let error = ParseError {
            kind: ParseErrorKind::MissingSemiColon,
            span: lexer.current_span(),
        };
        errors.push(error);
    }
}

fn parse_if_statement<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<IfStatement<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::If {
        return None;
    }
    lexer.next();

    let Some(condition) = parse_expression(lexer, errors) else {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::InvalidIfStatement,
            &[TokenKind::SemiColon],
        );
        return Some(IfStatement {
            first: IfBranch {
                condition: None,
                block: None,
                span: anchor.elapsed(lexer),
            },
            chain: Vec::new(),
            span: anchor.elapsed(lexer),
        });
    };

    let Some(block) = parse_block(lexer, errors) else {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::InvalidIfStatement,
            &[TokenKind::SemiColon],
        );
        return Some(IfStatement {
            first: IfBranch {
                condition: Some(condition),
                block: None,
                span: anchor.elapsed(lexer),
            },
            chain: Vec::new(),
            span: anchor.elapsed(lexer),
        });
    };
    let first_span = anchor.elapsed(lexer);

    let mut chain = Vec::new();

    loop {
        let anchor = lexer.cast_anchor();

        if lexer.current().get_kind() != TokenKind::Else {
            break;
        }
        lexer.next();

        if lexer.current().get_kind() == TokenKind::If {
            let Some(condition) = parse_expression(lexer, errors) else {
                break;
            };
            let Some(block) = parse_block(lexer, errors) else {
                break;
            };

            chain.push(ElseOrElseIf::ElseIf {
                condition: Some(condition),
                block: Some(block),
                span: anchor.elapsed(lexer),
            });
        } else {
            let Some(block) = parse_block(lexer, errors) else {
                break;
            };

            chain.push(ElseOrElseIf::Else {
                block: Some(block),
                span: anchor.elapsed(lexer),
            });
        }
    }

    Some(IfStatement {
        first: IfBranch {
            condition: Some(condition),
            block: Some(block),
            span: first_span,
        },
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_let_statement<'input>(
    lexer: &mut Lexer<'input>,
    error: &mut Vec<ParseError>,
) -> Option<LetStatement<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Let {
        return None;
    }
    lexer.next();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => {
            recover_until(
                lexer,
                error,
                ParseErrorKind::MissingLetName,
                &[TokenKind::SemiColon],
            );
            return Some(LetStatement {
                name: None,
                expression: None,
                span: anchor.elapsed(lexer),
            });
        }
    };

    if lexer.current().get_kind() != TokenKind::Equal {
        return Some(LetStatement {
            name: Some(name),
            expression: None,
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let expression = parse_expression(lexer, error);

    Some(LetStatement {
        name: Some(name),
        expression,
        span: anchor.elapsed(lexer),
    })
}

fn parse_assignment<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<Assignment<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(left) = parse_primary(lexer, errors) else {
        return None;
    };

    if lexer.current().get_kind() != TokenKind::Equal {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let right = parse_expression(lexer, errors);

    if right.is_none() {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::MissingRightExpressionInAssign,
            &[TokenKind::SemiColon],
        );
    }

    Some(Assignment {
        left,
        right,
        span: anchor.elapsed(lexer),
    })
}

fn parse_function_define<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<FunctionDefine<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Function {
        return None;
    }
    lexer.next();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => {
            recover_until(
                lexer,
                errors,
                ParseErrorKind::MissingNameInFunction,
                &[TokenKind::SemiColon],
            );
            return Some(FunctionDefine {
                name: None,
                arguments: Vec::new(),
                return_type: None,
                block: None,
                span: anchor.elapsed(lexer),
            });
        }
    };

    if lexer.current().get_kind() != TokenKind::ParenthesesLeft {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::MissingFunctionArgument,
            &[TokenKind::SemiColon],
        );
        return Some(FunctionDefine {
            name: Some(name),
            arguments: Vec::new(),
            return_type: None,
            block: None,
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let mut arguments = Vec::new();
    loop {
        let Some(argument) = parse_function_argument(lexer) else {
            break;
        };
        arguments.push(argument);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::InvalidFunctionArgument,
            &[TokenKind::ParenthesesRight],
        );
    }
    if lexer.current().get_kind() == TokenKind::ParenthesesRight {
        lexer.next();
    }

    let return_type = match lexer.current().get_kind() {
        TokenKind::ThinArrow => {
            lexer.next();

            parse_type_info(lexer)
        }
        _ => None,
    };

    let block = parse_block(lexer, errors);

    if block.is_none() {
        let error = ParseError {
            kind: ParseErrorKind::MissingBlock,
            span: lexer.current_span(),
        };
        errors.push(error);
    }

    Some(FunctionDefine {
        name: Some(name),
        arguments,
        return_type,
        block,
        span: anchor.elapsed(lexer),
    })
}

fn parse_function_argument<'input>(lexer: &mut Lexer<'input>) -> Option<FunctionArgument<'input>> {
    let anchor = lexer.cast_anchor();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => return None,
    };

    if lexer.current().get_kind() != TokenKind::Colon {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let Some(type_info) = parse_type_info(lexer) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    Some(FunctionArgument {
        name,
        type_info,
        span: anchor.elapsed(lexer),
    })
}

fn parse_type_info<'input>(lexer: &mut Lexer<'input>) -> Option<TypeInfo<'input>> {
    let anchor = lexer.cast_anchor();

    match lexer.current().get_kind() {
        TokenKind::Literal => Some(TypeInfo {
            name: parse_as_literal(lexer),
            span: anchor.elapsed(lexer),
        }),
        _ => None,
    }
}

fn parse_block<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<Block<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        return None;
    }
    lexer.next();

    let program = parse_program(lexer, errors, &[TokenKind::BraceRight]);

    if lexer.current().get_kind() != TokenKind::BraceRight {
        // 多分リカバリである必要はない
        // ここに来る時点でトークンをすべて捨ててしまっているはずなので
        recover_until(
            lexer,
            errors,
            ParseErrorKind::UnclosedBrace,
            &[TokenKind::BraceRight],
        );
    }
    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(Block {
        program,
        span: anchor.elapsed(lexer),
    })
}

fn parse_class_define<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<ClassDefine<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Class {
        return None;
    }
    lexer.next();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => {
            recover_until(
                lexer,
                errors,
                ParseErrorKind::MissingClassName,
                &[TokenKind::SemiColon],
            );
            return Some(ClassDefine {
                name: None,
                fields: Vec::new(),
                span: anchor.elapsed(lexer),
            });
        }
    };

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::MissingClassField,
            &[TokenKind::SemiColon],
        );
        return Some(ClassDefine {
            name: Some(name),
            fields: Vec::new(),
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let mut fields = Vec::new();
    loop {
        let Some(field) = parse_class_field(lexer) else {
            break;
        };
        fields.push(field);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    if lexer.current().get_kind() != TokenKind::BraceRight {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::InvalidClassField,
            &[TokenKind::BraceRight],
        );
    }
    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(ClassDefine {
        name: Some(name),
        fields,
        span: anchor.elapsed(lexer),
    })
}

fn parse_class_field<'input>(lexer: &mut Lexer<'input>) -> Option<ClassField<'input>> {
    let anchor = lexer.cast_anchor();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => return None,
    };

    if lexer.current().get_kind() != TokenKind::Colon {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let Some(type_info) = parse_type_info(lexer) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    Some(ClassField {
        name,
        type_info,
        span: anchor.elapsed(lexer),
    })
}

fn parse_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<Expression<'input>> {
    if let Some(or_expression) = parse_or_expression(lexer, errors) {
        return Some(Expression::OrExpression(or_expression));
    }
    if let Some(new_expression) = parse_new_expression(lexer, errors) {
        return Some(Expression::NewExpression(new_expression));
    }
    if let Some(return_expression) = parse_return_expression(lexer, errors) {
        return Some(Expression::ReturnExpression(return_expression));
    }

    None
}

fn parse_or_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<OrExpression<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_and_expression(lexer, errors) else {
        return None;
    };

    let mut chain = Vec::new();
    loop {
        if lexer.current().get_kind() != TokenKind::Or {
            break;
        }
        let op = lexer.next().unwrap();

        let Some(right) = parse_and_expression(lexer, errors) else {
            break;
        };
        chain.push((op.span, right));
    }

    Some(OrExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_and_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<AndExpression<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_equals_or_not_equals_expression(lexer, errors) else {
        return None;
    };

    let mut chain = Vec::new();
    loop {
        if lexer.current().get_kind() != TokenKind::And {
            break;
        }
        let op = lexer.next().unwrap();

        let Some(right) = parse_equals_or_not_equals_expression(lexer, errors) else {
            break;
        };
        chain.push((op.span, right));
    }

    Some(AndExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_equals_or_not_equals_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<EqualsOrNotEqualsExpression<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_less_or_greater_expression(lexer, errors) else {
        return None;
    };

    let mut chain = None;
    for _ in 0..1 {
        let op = match lexer.current().get_kind() {
            TokenKind::DoubleEqual => {
                Spanned::new(EqualsOrNotEqusls::Equals, lexer.next().unwrap().span)
            }
            TokenKind::NotEqual => {
                Spanned::new(EqualsOrNotEqusls::NotEquals, lexer.next().unwrap().span)
            }
            _ => break,
        };

        let Some(right) = parse_less_or_greater_expression(lexer, errors) else {
            break;
        };
        chain = Some((op, right));
    }

    Some(EqualsOrNotEqualsExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_less_or_greater_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<LessOrGreaterExpression<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_add_or_sub_expression(lexer, errors) else {
        return None;
    };

    let mut chain = None;
    for _ in 0..1 {
        let op = match lexer.current().get_kind() {
            TokenKind::LessThan => {
                Spanned::new(LessOrGreaterThan::LessThan, lexer.next().unwrap().span)
            }
            TokenKind::LessThanOrEqual => Spanned::new(
                LessOrGreaterThan::LessThanOrEquals,
                lexer.next().unwrap().span,
            ),
            TokenKind::GreaterThan => {
                Spanned::new(LessOrGreaterThan::GreaterThan, lexer.next().unwrap().span)
            }
            TokenKind::GreaterThanOrEqual => Spanned::new(
                LessOrGreaterThan::GreaterThanOrEquals,
                lexer.next().unwrap().span,
            ),
            _ => break,
        };

        let Some(right) = parse_add_or_sub_expression(lexer, errors) else {
            break;
        };
        chain = Some((op, right));
    }

    Some(LessOrGreaterExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_add_or_sub_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<AddOrSubExpression<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_mul_or_div_expression(lexer, errors) else {
        return None;
    };

    let mut chain = Vec::new();
    loop {
        let op = match lexer.current().get_kind() {
            TokenKind::Plus => Spanned::new(AddOrSub::Add, lexer.next().unwrap().span),
            TokenKind::Minus => Spanned::new(AddOrSub::Sub, lexer.next().unwrap().span),
            _ => break,
        };

        let Some(right) = parse_mul_or_div_expression(lexer, errors) else {
            break;
        };
        chain.push((op, right));
    }

    Some(AddOrSubExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_mul_or_div_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<MulOrDivExpression<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_factor(lexer, errors) else {
        return None;
    };

    let mut chain = Vec::new();
    loop {
        let op = match lexer.current().get_kind() {
            TokenKind::Asterisk => Spanned::new(MulOrDiv::Mul, lexer.next().unwrap().span),
            TokenKind::Slash => Spanned::new(MulOrDiv::Div, lexer.next().unwrap().span),
            _ => break,
        };

        let Some(right) = parse_factor(lexer, errors) else {
            break;
        };
        chain.push((op, right));
    }

    Some(MulOrDivExpression {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_factor<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<Factor<'input>> {
    let anchor = lexer.cast_anchor();

    match lexer.current().get_kind() {
        TokenKind::Minus => {
            let minus = lexer.next().unwrap().span;

            let primary = parse_primary(lexer, errors);

            if primary.is_none() {
                recover_until(
                    lexer,
                    errors,
                    ParseErrorKind::MissingPrimary,
                    &[TokenKind::SemiColon],
                );
            }

            Some(Factor {
                minus: Some(minus),
                primary,
                span: anchor.elapsed(lexer),
            })
        }
        _ => {
            let Some(primary) = parse_primary(lexer, errors) else {
                return None;
            };

            Some(Factor {
                minus: None,
                primary: Some(primary),
                span: anchor.elapsed(lexer),
            })
        }
    }
}

fn parse_primary<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<Primary<'input>> {
    let anchor = lexer.cast_anchor();

    let Some(first) = parse_primary_left(lexer, errors) else {
        return None;
    };

    let mut chain = Vec::new();
    loop {
        if lexer.current().get_kind() != TokenKind::Dot {
            break;
        }
        lexer.next();

        let Some(right) = parse_primary_right(lexer) else {
            break;
        };
        chain.push(right);
    }

    Some(Primary {
        first,
        chain,
        span: anchor.elapsed(lexer),
    })
}

fn parse_primary_left<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<PrimaryLeft<'input>> {
    match lexer.current().get_kind() {
        TokenKind::Literal => Some(PrimaryLeft::Literal {
            literal: parse_as_literal(lexer),
            function_call: parse_function_call(lexer, errors),
        }),
        TokenKind::NumericLiteral => Some(PrimaryLeft::NumericLiteral {
            literal: parse_as_literal(lexer),
        }),
        TokenKind::StringLiteral => Some(PrimaryLeft::StringLiteral {
            literal: parse_as_literal(lexer),
        }),
        _ => None,
    }
}

fn parse_primary_right<'input>(lexer: &mut Lexer<'input>) -> Option<PrimaryRight<'input>> {
    let anchor = lexer.cast_anchor();

    match lexer.current().get_kind() {
        TokenKind::Literal => Some(PrimaryRight {
            literal: parse_as_literal(lexer),
            span: anchor.elapsed(lexer),
        }),
        _ => None,
    }
}

fn parse_function_call<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<FunctionCall<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::ParenthesesLeft {
        return None;
    }
    lexer.next();

    let mut arguments = Vec::new();
    loop {
        let Some(argument) = parse_expression(lexer, errors) else {
            break;
        };
        arguments.push(argument);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    if lexer.current().get_kind() != TokenKind::ParenthesesRight {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::UnclosedParen,
            &[TokenKind::ParenthesesRight],
        );
    }
    if lexer.current().get_kind() == TokenKind::ParenthesesRight {
        lexer.next();
    }

    Some(FunctionCall {
        arguments,
        span: anchor.elapsed(lexer),
    })
}

fn parse_new_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<NewExpression<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::New {
        return None;
    }
    lexer.next();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => {
            recover_until(
                lexer,
                errors,
                ParseErrorKind::MissingNewName,
                &[TokenKind::SemiColon],
            );
            return Some(NewExpression {
                name: None,
                field_assign: Vec::new(),
                span: anchor.elapsed(lexer),
            });
        }
    };

    if lexer.current().get_kind() != TokenKind::BraceLeft {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::MissingNewFieldAssign,
            &[TokenKind::SemiColon],
        );

        return Some(NewExpression {
            name: Some(name),
            field_assign: Vec::new(),
            span: anchor.elapsed(lexer),
        });
    }
    lexer.next();

    let mut field_assign = Vec::new();
    loop {
        let Some(assign) = parse_field_assign(lexer, errors) else {
            break;
        };
        field_assign.push(assign);

        if lexer.current().get_kind() != TokenKind::Comma {
            break;
        }
        lexer.next();
    }

    if lexer.current().get_kind() != TokenKind::BraceRight {
        recover_until(
            lexer,
            errors,
            ParseErrorKind::UnclosedBrace,
            &[TokenKind::BraceRight],
        );
    }
    if lexer.current().get_kind() == TokenKind::BraceRight {
        lexer.next();
    }

    Some(NewExpression {
        name: Some(name),
        field_assign,
        span: anchor.elapsed(lexer),
    })
}

fn parse_field_assign<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<FieldAssign<'input>> {
    let anchor = lexer.cast_anchor();

    let name = match lexer.current().get_kind() {
        TokenKind::Literal => parse_as_literal(lexer),
        _ => return None,
    };

    if lexer.current().get_kind() != TokenKind::Colon {
        lexer.back_to_anchor(anchor);
        return None;
    }
    lexer.next();

    let Some(expression) = parse_expression(lexer, errors) else {
        lexer.back_to_anchor(anchor);
        return None;
    };

    Some(FieldAssign {
        name,
        expression,
        span: anchor.elapsed(lexer),
    })
}

fn parse_return_expression<'input>(
    lexer: &mut Lexer<'input>,
    errors: &mut Vec<ParseError>,
) -> Option<ReturnExpression<'input>> {
    let anchor = lexer.cast_anchor();

    if lexer.current().get_kind() != TokenKind::Return {
        return None;
    }
    lexer.next();

    let expression = parse_expression(lexer, errors);

    Some(ReturnExpression {
        expression: expression.map(|expression| Box::new(expression)),
        span: anchor.elapsed(lexer),
    })
}

fn parse_as_literal<'input>(lexer: &mut Lexer<'input>) -> Literal<'input> {
    let token = lexer.next().unwrap();

    Literal {
        value: token.text,
        span: token.span,
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, parser::parse_program};

    #[test]
    fn parser() {
        let source = "
class Test {
    field: int
}

function test(a: int, b: int) -> int {
    return a + b
}

let a = 100 + 200;
let b = new Test { field: 100 };
        ";

        let mut lexer = Lexer::new(source);
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &mut errors, &[]);

        dbg!(ast);
        dbg!(errors);
    }
}
