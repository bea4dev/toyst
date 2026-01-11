use crate::{
    ast::{Assignment, ElseOrElseIf, Primary, PrimaryLeft, Program, Statement},
    error::{SemanticsError, SemanticsErrorKind},
};

pub fn validate_semantics_for_program(ast: &Program, errors: &mut Vec<SemanticsError>) {
    for statement in ast.statements.iter() {
        match statement {
            Statement::LetStatement(_) => {}
            Statement::Assignment(assignment) => {
                validate_semantics_for_assignment(assignment, errors)
            }
            Statement::Expression(_) => {}
            Statement::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block {
                    validate_semantics_for_program(&block.program, errors);
                }
            }
            Statement::ClassDefine(_) => {}
            Statement::IfStatement(if_statement) => {
                validate_semantics_for_program(&if_statement.first.block.program, errors);

                for chain in if_statement.chain.iter() {
                    match chain {
                        ElseOrElseIf::Else { block, span: _ } => {
                            validate_semantics_for_program(&block.program, errors);
                        }
                        ElseOrElseIf::ElseIf {
                            condition: _,
                            block,
                            span: _,
                        } => {
                            validate_semantics_for_program(&block.program, errors);
                        }
                    }
                }
            }
        }
    }
}

fn validate_semantics_for_assignment(ast: &Assignment, errors: &mut Vec<SemanticsError>) {
    if !is_validate_as_assign_left(&ast.left) {
        let error = SemanticsError {
            kind: SemanticsErrorKind::InvalidAssignLeft,
            span: ast.left.span.clone(),
        };
        errors.push(error);
    }

    fn is_validate_as_assign_left(ast: &Primary) -> bool {
        match ast.chain.len() {
            0 => match &ast.first {
                PrimaryLeft::Literal {
                    literal: _,
                    function_call,
                } => function_call.is_none(),
                PrimaryLeft::NumericLiteral { literal: _ } => false,
                PrimaryLeft::StringLiteral { literal: _ } => false,
            },
            _ => true,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, parser::parse_program, semantics::validate_semantics_for_program};

    #[test]
    fn semantics() {
        let source = "
let a = 100 + 200;
b = a;
b + a = a; // Error
        ";

        let mut lexer = Lexer::new(source);
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &mut errors, &[]);

        dbg!(&ast);
        dbg!(errors);

        let mut errors = Vec::new();
        validate_semantics_for_program(&ast, &mut errors);

        dbg!(&errors);

        assert!(!errors.is_empty());
    }
}
