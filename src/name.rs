use std::{collections::HashMap, ops::Range};

use crate::{
    ast::{
        AddOrSubExpression, AndExpression, ElseOrElseIf, EntityID, EqualsOrNotEqualsExpression,
        Expression, Factor, LessOrGreaterExpression, MulOrDivExpression, NewExpression,
        OrExpression, Primary, PrimaryLeft, Program, ReturnExpression, Statement, TypeInfo,
    },
    error::{NameResolveError, NameResolveErrorKind},
};

pub struct NameResolverContainer<'input> {
    resolvers: Vec<NameResolver<'input>>,
}

impl<'input> NameResolverContainer<'input> {
    pub fn new() -> Self {
        Self {
            resolvers: Vec::new(),
        }
    }

    pub fn new_resolver(&mut self, parent: Option<NameResolverID>) -> NameResolverID {
        self.resolvers.push(NameResolver {
            parent,
            map: HashMap::new(),
        });
        NameResolverID(self.resolvers.len() - 1)
    }

    fn define(&mut self, resolver: NameResolverID, name: &'input str, define: Define) {
        let resolver = &mut self.resolvers[resolver.0];
        resolver.map.insert(name, define);
    }

    fn resolve(&self, resolver: NameResolverID, name: &'input str) -> Option<&Define> {
        let resolver = &self.resolvers[resolver.0];
        match resolver.map.get(name) {
            Some(define) => Some(define),
            None => match resolver.parent {
                Some(parent) => self.resolve(parent, name),
                None => None,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NameResolverID(usize);

pub struct NameResolver<'input> {
    parent: Option<NameResolverID>,
    map: HashMap<&'input str, Define>,
}

#[derive(Debug, Clone)]
pub struct Define {
    pub kind: DefineKind,
    pub entity_id: EntityID,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefineKind {
    UserType,
    Function,
    Variable,
}

pub fn resolve_name_for_program<'input>(
    ast: &Program<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    // クラスと関数の定義を先に回収する
    // こうすることで、あとに書いたものでも名前解決できる
    //
    // test()
    //
    // function test() {}
    for statement in ast.statements.iter() {
        match statement {
            Statement::ClassDefine(class_define) => {
                if let Some(name) = &class_define.name {
                    container.define(
                        resolver,
                        name.value,
                        Define {
                            kind: DefineKind::UserType,
                            entity_id: EntityID::from(class_define),
                            span: name.span.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match statement {
            Statement::FunctionDefine(function_define) => {
                if let Some(name) = &function_define.name {
                    container.define(
                        resolver,
                        name.value,
                        Define {
                            kind: DefineKind::Function,
                            entity_id: EntityID::from(function_define),
                            span: name.span.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match statement {
            Statement::LetStatement(let_statement) => {
                if let Some(name) = &let_statement.name {
                    container.define(
                        resolver,
                        name.value,
                        Define {
                            kind: DefineKind::Variable,
                            entity_id: EntityID::from(name),
                            span: name.span.clone(),
                        },
                    );
                }
            }
            Statement::Assignment(assignment) => {
                resolve_name_for_primary(
                    &assignment.left,
                    errors,
                    resolver,
                    container,
                    resolved_map,
                );
                if let Some(right) = &assignment.right {
                    resolve_name_for_expression(right, errors, resolver, container, resolved_map);
                }
            }
            Statement::Expression(expression) => {
                resolve_name_for_expression(expression, errors, resolver, container, resolved_map);
            }
            Statement::FunctionDefine(function_define) => {
                let resolver = container.new_resolver(Some(resolver));

                for argument in function_define.arguments.iter() {
                    container.define(
                        resolver,
                        argument.name.value,
                        Define {
                            kind: DefineKind::Variable,
                            entity_id: EntityID::from(argument),
                            span: argument.name.span.clone(),
                        },
                    );
                    resolve_name_for_type_info(
                        &argument.type_info,
                        errors,
                        resolver,
                        container,
                        resolved_map,
                    );
                }

                if let Some(return_type) = &function_define.return_type {
                    resolve_name_for_type_info(
                        return_type,
                        errors,
                        resolver,
                        container,
                        resolved_map,
                    );
                }

                if let Some(block) = &function_define.block {
                    resolve_name_for_program(
                        &block.program,
                        errors,
                        resolver,
                        container,
                        resolved_map,
                    );
                }
            }
            Statement::ClassDefine(class_define) => {
                let resolver = container.new_resolver(Some(resolver));

                for field in class_define.fields.iter() {
                    resolve_name_for_type_info(
                        &field.type_info,
                        errors,
                        resolver,
                        container,
                        resolved_map,
                    );
                }
            }
            Statement::IfStatement(if_statement) => {
                let new_resolver = container.new_resolver(Some(resolver));

                resolve_name_for_expression(
                    &if_statement.first.condition,
                    errors,
                    new_resolver,
                    container,
                    resolved_map,
                );

                let new_resolver = container.new_resolver(Some(resolver));

                resolve_name_for_program(
                    &if_statement.first.block.program,
                    errors,
                    new_resolver,
                    container,
                    resolved_map,
                );

                for chain in if_statement.chain.iter() {
                    match chain {
                        ElseOrElseIf::Else { block, span: _ } => {
                            let new_resolver = container.new_resolver(Some(resolver));

                            resolve_name_for_program(
                                &block.program,
                                errors,
                                new_resolver,
                                container,
                                resolved_map,
                            );
                        }
                        ElseOrElseIf::ElseIf {
                            condition,
                            block,
                            span: _,
                        } => {
                            let new_resolver = container.new_resolver(Some(resolver));

                            resolve_name_for_expression(
                                condition,
                                errors,
                                new_resolver,
                                container,
                                resolved_map,
                            );

                            let new_resolver = container.new_resolver(Some(resolver));

                            resolve_name_for_program(
                                &block.program,
                                errors,
                                new_resolver,
                                container,
                                resolved_map,
                            );
                        }
                    }
                }
            }
        }
    }
}

fn resolve_name_for_expression<'input>(
    ast: &Expression<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    match ast {
        Expression::OrExpression(or_expression) => {
            resolve_name_for_or_expression(or_expression, errors, resolver, container, resolved_map)
        }
        Expression::NewExpression(new_expression) => {
            resolve_name_for_new_expression(
                new_expression,
                errors,
                resolver,
                container,
                resolved_map,
            );
        }
        Expression::ReturnExpression(return_expression) => {
            resolve_name_for_return_expression(
                return_expression,
                errors,
                resolver,
                container,
                resolved_map,
            );
        }
    }
}

macro_rules! resolve_name_for_2op {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name<'input>(
            ast: &$ty,
            errors: &mut Vec<NameResolveError>,
            resolver: NameResolverID,
            container: &mut NameResolverContainer<'input>,
            resolved_map: &mut HashMap<EntityID, Define>,
        ) {
            $next(&ast.first, errors, resolver, container, resolved_map);
            for (_, chain) in ast.chain.iter() {
                $next(chain, errors, resolver, container, resolved_map);
            }
        }
    };
}

resolve_name_for_2op!(
    resolve_name_for_or_expression,
    OrExpression<'input>,
    resolve_name_for_and_expression
);

resolve_name_for_2op!(
    resolve_name_for_and_expression,
    AndExpression<'input>,
    resolve_name_for_eq_ne_expression
);

resolve_name_for_2op!(
    resolve_name_for_eq_ne_expression,
    EqualsOrNotEqualsExpression<'input>,
    resolve_name_for_less_greater_expression
);

resolve_name_for_2op!(
    resolve_name_for_less_greater_expression,
    LessOrGreaterExpression<'input>,
    resolve_name_for_add_sub_expression
);

resolve_name_for_2op!(
    resolve_name_for_add_sub_expression,
    AddOrSubExpression<'input>,
    resolve_name_for_mul_div_expression
);

resolve_name_for_2op!(
    resolve_name_for_mul_div_expression,
    MulOrDivExpression<'input>,
    resolve_name_for_factor
);

fn resolve_name_for_factor<'input>(
    ast: &Factor<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    if let Some(primary) = &ast.primary {
        resolve_name_for_primary(primary, errors, resolver, container, resolved_map);
    }
}

fn resolve_name_for_primary<'input>(
    ast: &Primary<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    if let PrimaryLeft::Literal {
        literal,
        function_call,
    } = &ast.first
    {
        match container.resolve(resolver, literal.value) {
            Some(define) => {
                resolved_map.insert(EntityID::from(literal), define.clone());
            }
            None => {
                let error = NameResolveError {
                    kind: NameResolveErrorKind::UnknownName,
                    span: literal.span.clone(),
                };
                errors.push(error);
            }
        }

        if let Some(function_call) = function_call {
            for argument in function_call.arguments.iter() {
                resolve_name_for_expression(argument, errors, resolver, container, resolved_map);
            }
        }
    }
}

fn resolve_name_for_new_expression<'input>(
    ast: &NewExpression<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    if let Some(name) = &ast.name {
        match container.resolve(resolver, name.value) {
            Some(define) => {
                resolved_map.insert(EntityID::from(name), define.clone());
            }
            None => {
                let error = NameResolveError {
                    kind: NameResolveErrorKind::UnknownName,
                    span: name.span.clone(),
                };
                errors.push(error);
            }
        }
    }

    for field_assign in ast.field_assign.iter() {
        resolve_name_for_expression(
            &field_assign.expression,
            errors,
            resolver,
            container,
            resolved_map,
        );
    }
}

fn resolve_name_for_return_expression<'input>(
    ast: &ReturnExpression<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    if let Some(expression) = &ast.expression {
        resolve_name_for_expression(expression, errors, resolver, container, resolved_map);
    }
}

fn resolve_name_for_type_info<'input>(
    ast: &TypeInfo<'input>,
    errors: &mut Vec<NameResolveError>,
    resolver: NameResolverID,
    container: &mut NameResolverContainer<'input>,
    resolved_map: &mut HashMap<EntityID, Define>,
) {
    match container.resolve(resolver, ast.name.value) {
        Some(define) => {
            resolved_map.insert(EntityID::from(&ast.name), define.clone());
        }
        None => {
            let error = NameResolveError {
                kind: NameResolveErrorKind::UnknownName,
                span: ast.name.span.clone(),
            };
            errors.push(error);
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        lexer::Lexer,
        name::{NameResolverContainer, resolve_name_for_program},
        parser::parse_program,
    };

    #[test]
    fn name() {
        let source = "
let a = 100 + 200;
a = 100;
b = 100; // Error
        ";

        let mut lexer = Lexer::new(source);
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &mut errors, &[]);

        dbg!(&ast);
        dbg!(errors);

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

        dbg!(resolved_map);
        dbg!(&errors);
        assert!(errors.len() == 1);
    }
}
