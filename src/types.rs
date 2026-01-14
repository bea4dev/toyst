use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    sync::{Arc, Mutex},
};

use crate::{
    ast::{
        AddOrSubExpression, AndExpression, ElseOrElseIf, EntityID, EqualsOrNotEqualsExpression,
        Expression, Factor, LessOrGreaterExpression, MulOrDivExpression, NewExpression,
        OrExpression, Primary, PrimaryLeft, Program, ReturnExpression, Spanned, Statement,
        TypeInfo,
    },
    error::{TypeError, TypeErrorKind},
    name::{Define, DefineKind},
};

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Void,
    String,
    Class {
        entity_id: EntityID,
        name: String,
        fields: Arc<Mutex<Vec<(Spanned<String>, Type)>>>,
        span: Range<usize>,
    },
    Function {
        arguments: Vec<Type>,
        return_type: Box<Type>,
        span: Range<usize>,
    },
    Unknown,
    Unreachable,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Int | Type::Float => true,
            _ => false,
        }
    }

    pub fn to_display_string(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::String => "string".to_string(),
            Type::Class {
                entity_id: _,
                name,
                fields: _,
                span: _,
            } => name.to_string(),
            Type::Function {
                arguments,
                return_type,
                span: _,
            } => {
                format!(
                    "({}) -> {}",
                    arguments
                        .iter()
                        .map(|ty| ty.to_display_string())
                        .collect::<Vec<_>>()
                        .join(","),
                    return_type.to_display_string()
                )
            }
            Type::Unknown => "unknown".to_string(),
            Type::Unreachable => "!".to_string(),
        }
    }
}

pub struct TypeEnvironment {
    map: HashMap<EntityID, TypeOrEntityID>,
    variables: Vec<Spanned<EntityID>>,
}

#[derive(Debug, Clone)]
pub enum TypeOrEntityID {
    Type(Type),
    EntityID(EntityID),
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            variables: Vec::new(),
        }
    }

    pub fn register_variable(&mut self, variable: Spanned<EntityID>) {
        self.variables.push(variable);
    }

    fn define_entity_type(&mut self, entity_id: EntityID, ty: Type) {
        self.map.insert(entity_id, TypeOrEntityID::Type(ty));
    }

    fn unify(
        &mut self,
        left: Spanned<EntityID>,
        right: Spanned<EntityID>,
        errors: &mut Vec<TypeError>,
    ) {
        let left_resolved = self.resolve_entity_id(left.value);
        let right_resolved = self.resolve_entity_id(right.value);

        if left_resolved == right_resolved {
            return;
        }

        match (
            self.map.get(&left_resolved).cloned(),
            self.map.get(&right_resolved).cloned(),
        ) {
            (None, None) => {
                self.map
                    .insert(left_resolved, TypeOrEntityID::EntityID(right_resolved));
            }
            (None, Some(_)) => {
                self.map
                    .insert(left_resolved, TypeOrEntityID::EntityID(right_resolved));
            }
            (Some(_), None) => {
                self.map
                    .insert(right_resolved, TypeOrEntityID::EntityID(left_resolved));
            }
            (Some(left_type), Some(right_type)) => {
                let TypeOrEntityID::Type(left_type) = left_type else {
                    unreachable!()
                };
                let TypeOrEntityID::Type(right_type) = right_type else {
                    unreachable!()
                };

                if !self.unify_type(&left_type, &right_type) {
                    let error = TypeError {
                        kind: TypeErrorKind::TypeMismatch {
                            left: Spanned::new(left_type.clone(), left.span.clone()),
                            right: Spanned::new(right_type.clone(), right.span.clone()),
                        },
                        span: left.span.clone(),
                    };
                    errors.push(error);
                }

                if let Type::Unknown | Type::Unreachable = left_type {
                    self.map
                        .insert(left_resolved, TypeOrEntityID::EntityID(right_resolved));
                } else if let Type::Unknown | Type::Unreachable = right_type {
                    self.map
                        .insert(right_resolved, TypeOrEntityID::EntityID(left_resolved));
                }
            }
        }
    }

    fn unify_type(&self, left: &Type, right: &Type) -> bool {
        if let Type::Unknown | Type::Unreachable = left {
            return true;
        }
        if let Type::Unknown | Type::Unreachable = right {
            return true;
        }

        match left {
            Type::Int => {
                if let Type::Int = right {
                    true
                } else {
                    false
                }
            }
            Type::Float => {
                if let Type::Float = right {
                    true
                } else {
                    false
                }
            }
            Type::Bool => {
                if let Type::Bool = right {
                    true
                } else {
                    false
                }
            }
            Type::Void => {
                if let Type::Void = right {
                    true
                } else {
                    false
                }
            }
            Type::String => {
                if let Type::String = right {
                    true
                } else {
                    false
                }
            }
            Type::Class {
                entity_id: left_entity_id,
                name: _,
                fields: _,
                span: _,
            } => {
                if let Type::Class {
                    entity_id: right_entity_id,
                    name: _,
                    fields: _,
                    span: _,
                } = right
                {
                    left_entity_id == right_entity_id
                } else {
                    false
                }
            }
            Type::Function {
                arguments: left_arguments,
                return_type: left_return_type,
                span: _,
            } => {
                if let Type::Function {
                    arguments: right_arguments,
                    return_type: right_return_type,
                    span: _,
                } = right
                {
                    if left_arguments.len() != right_arguments.len() {
                        return false;
                    }

                    for (left, right) in left_arguments.iter().zip(right_arguments.iter()) {
                        if !self.unify_type(left, right) {
                            return false;
                        }
                    }

                    if !self.unify_type(&left_return_type, &right_return_type) {
                        return false;
                    }

                    true
                } else {
                    false
                }
            }
            Type::Unknown => unreachable!(),
            Type::Unreachable => unreachable!(),
        }
    }

    fn give_type(
        &mut self,
        entity_id: Spanned<EntityID>,
        ty: Spanned<Type>,
        errors: &mut Vec<TypeError>,
    ) {
        let resolved = self.resolve_entity_id(entity_id.value);

        match self.map.get(&resolved).cloned() {
            Some(entity_type) => {
                let TypeOrEntityID::Type(entity_type) = entity_type else {
                    unreachable!()
                };

                if !self.unify_type(&ty.value, &entity_type) {
                    let error = TypeError {
                        kind: TypeErrorKind::UnexpectedType {
                            expected: ty.clone(),
                            found: Spanned::new(entity_type, entity_id.span.clone()),
                        },
                        span: entity_id.span.clone(),
                    };
                    errors.push(error);
                }
            }
            None => {
                self.map.insert(resolved, TypeOrEntityID::Type(ty.value));
            }
        }
    }

    fn validate_numeric_type(
        &mut self,
        entity_id: Spanned<EntityID>,
        numeric_type_span: Range<usize>,
        errors: &mut Vec<TypeError>,
    ) {
        let resolved = self.resolve_entity_id(entity_id.value);

        match self.map.get(&resolved).cloned() {
            Some(entity_type) => {
                let TypeOrEntityID::Type(entity_type) = entity_type else {
                    unreachable!()
                };

                if !entity_type.is_numeric() {
                    let error = TypeError {
                        kind: TypeErrorKind::NotNumericType {
                            expected: numeric_type_span,
                            found: Spanned::new(entity_type, entity_id.span.clone()),
                        },
                        span: entity_id.span.clone(),
                    };
                    errors.push(error);
                }
            }
            None => {
                let error = TypeError {
                    kind: TypeErrorKind::UnknownTypeAtThisPoint,
                    span: entity_id.span.clone(),
                };
                errors.push(error);
            }
        }
    }

    fn resolve_entity_id(&self, entity_id: EntityID) -> EntityID {
        match self.map.get(&entity_id) {
            Some(resolved) => match resolved {
                TypeOrEntityID::Type(_) => entity_id,
                TypeOrEntityID::EntityID(entity_id) => self.resolve_entity_id(*entity_id),
            },
            None => entity_id,
        }
    }

    fn resolve_entity_type(&self, entity_id: EntityID) -> Option<Type> {
        match self.map.get(&entity_id) {
            Some(resolved) => match resolved {
                TypeOrEntityID::Type(ty) => Some(ty.clone()),
                TypeOrEntityID::EntityID(entity_id) => self.resolve_entity_type(*entity_id),
            },
            None => None,
        }
    }

    pub fn dump(self) -> HashMap<EntityID, Type> {
        let mut result = HashMap::new();

        for &entity_id in self.map.keys() {
            result.insert(
                entity_id,
                self.resolve_entity_type(entity_id).unwrap_or(Type::Unknown),
            );
        }

        result
    }
}

pub fn infer_type_for_program(
    ast: &Program,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    // 名前解決で先に処理しているので、こちらも先に型を解決しておく
    for statement in ast.statements.iter() {
        match statement {
            Statement::ClassDefine(class_define) => {
                if let Some(name) = &class_define.name {
                    let ty = Type::Class {
                        entity_id: EntityID::from(name),
                        name: name.value.to_string(),
                        fields: Arc::new(Mutex::new(Vec::new())),
                        span: class_define.span.clone(),
                    };
                    env.define_entity_type(EntityID::from(name), ty);
                }
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match statement {
            Statement::FunctionDefine(function_define) => {
                let mut arguments = Vec::new();
                for argument in function_define.arguments.iter() {
                    arguments.push(get_type(&argument.type_info, resolved_map, env, errors));
                }

                let mut return_type = Type::Void;
                if let Some(return_type_info) = &function_define.return_type {
                    return_type = get_type(return_type_info, resolved_map, env, errors);
                }

                let ty = Type::Function {
                    arguments,
                    return_type: Box::new(return_type),
                    span: function_define.span.clone(),
                };

                if let Some(name) = &function_define.name {
                    env.define_entity_type(EntityID::from(name), ty);
                }
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match statement {
            Statement::LetStatement(let_statement) => {
                if let Some(name) = &let_statement.name {
                    env.register_variable(Spanned::new(EntityID::from(name), name.span.clone()));
                }

                if let Some(expression) = &let_statement.expression {
                    infer_type_for_expression(expression, resolved_map, return_type, env, errors);

                    if let Some(name) = &let_statement.name {
                        env.unify(
                            Spanned::new(EntityID::from(name), name.span.clone()),
                            Spanned::new(EntityID::from(expression), expression.span()),
                            errors,
                        );
                    }
                }
            }
            Statement::Assignment(assignment) => {
                infer_type_for_primary(&assignment.left, resolved_map, return_type, env, errors);

                if let Some(right) = &assignment.right {
                    infer_type_for_expression(right, resolved_map, return_type, env, errors);

                    env.unify(
                        Spanned::new(
                            EntityID::from(&assignment.left),
                            assignment.left.span.clone(),
                        ),
                        Spanned::new(EntityID::from(right), right.span()),
                        errors,
                    );
                }
            }
            Statement::Expression(expression) => {
                infer_type_for_expression(expression, resolved_map, return_type, env, errors);
            }
            Statement::FunctionDefine(function_define) => {
                for argument in function_define.arguments.iter() {
                    let ty = get_type(&argument.type_info, resolved_map, env, errors);
                    env.define_entity_type(EntityID::from(&argument.name), ty);
                }

                let mut return_type = Spanned::new(Type::Void, function_define.span.clone());
                if let Some(return_type_info) = &function_define.return_type {
                    return_type = Spanned::new(
                        get_type(return_type_info, resolved_map, env, errors),
                        return_type_info.span.clone(),
                    );
                }

                if let Some(block) = &function_define.block {
                    infer_type_for_program(&block.program, resolved_map, &return_type, env, errors);
                }
            }
            Statement::ClassDefine(class_define) => {
                if let Some(name) = &class_define.name {
                    let Type::Class {
                        entity_id: _,
                        name: _,
                        fields,
                        span: _,
                    } = env
                        .resolve_entity_type(EntityID::from(name))
                        .unwrap()
                        .clone()
                    else {
                        unreachable!()
                    };

                    let mut fields = fields.lock().unwrap();
                    for field in class_define.fields.iter() {
                        fields.push((
                            Spanned::new(field.name.value.to_string(), field.span.clone()),
                            get_type(&field.type_info, resolved_map, env, errors),
                        ));
                    }
                }
            }
            Statement::IfStatement(if_statement) => {
                if let Some(condition) = &if_statement.first.condition {
                    infer_type_for_expression(condition, resolved_map, return_type, env, errors);

                    env.give_type(
                        Spanned::new(EntityID::from(condition), condition.span()),
                        Spanned::new(Type::Bool, condition.span()),
                        errors,
                    );
                }

                if let Some(block) = &if_statement.first.block {
                    infer_type_for_program(&block.program, resolved_map, return_type, env, errors);
                }

                for chain in if_statement.chain.iter() {
                    match chain {
                        ElseOrElseIf::Else { block, span: _ } => {
                            if let Some(block) = block {
                                infer_type_for_program(
                                    &block.program,
                                    resolved_map,
                                    return_type,
                                    env,
                                    errors,
                                );
                            }
                        }
                        ElseOrElseIf::ElseIf {
                            condition,
                            block,
                            span: _,
                        } => {
                            if let Some(condition) = condition {
                                infer_type_for_expression(
                                    condition,
                                    resolved_map,
                                    return_type,
                                    env,
                                    errors,
                                );

                                env.give_type(
                                    Spanned::new(EntityID::from(condition), condition.span()),
                                    Spanned::new(Type::Bool, condition.span()),
                                    errors,
                                );
                            }

                            if let Some(block) = block {
                                infer_type_for_program(
                                    &block.program,
                                    resolved_map,
                                    return_type,
                                    env,
                                    errors,
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

fn infer_type_for_expression(
    ast: &Expression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match ast {
        Expression::OrExpression(or_expression) => {
            infer_type_for_or_expression(or_expression, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span()),
                Spanned::new(EntityID::from(or_expression), or_expression.span.clone()),
                errors,
            );
        }
        Expression::NewExpression(new_expression) => {
            infer_type_for_new_expression(new_expression, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span()),
                Spanned::new(EntityID::from(new_expression), new_expression.span.clone()),
                errors,
            );
        }
        Expression::ReturnExpression(return_expression) => {
            infer_type_for_return_expression(
                return_expression,
                resolved_map,
                return_type,
                env,
                errors,
            );

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span()),
                Spanned::new(
                    EntityID::from(return_expression),
                    return_expression.span.clone(),
                ),
                errors,
            );
        }
    }
}

fn infer_type_for_or_expression(
    ast: &OrExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match ast.chain.len() {
        0 => {
            infer_type_for_and_expression(&ast.first, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                errors,
            );
        }
        _ => {
            infer_type_for_and_expression(&ast.first, resolved_map, return_type, env, errors);

            env.give_type(
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                Spanned::new(Type::Bool, ast.span.clone()),
                errors,
            );

            for (_, chain) in ast.chain.iter() {
                infer_type_for_and_expression(chain, resolved_map, return_type, env, errors);

                env.give_type(
                    Spanned::new(EntityID::from(chain), chain.span.clone()),
                    Spanned::new(Type::Bool, ast.span.clone()),
                    errors,
                );
            }

            env.give_type(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(Type::Bool, ast.span.clone()),
                errors,
            );
        }
    }
}

fn infer_type_for_and_expression(
    ast: &AndExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match ast.chain.len() {
        0 => {
            infer_type_for_eq_expression(&ast.first, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                errors,
            );
        }
        _ => {
            infer_type_for_eq_expression(&ast.first, resolved_map, return_type, env, errors);

            env.give_type(
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                Spanned::new(Type::Bool, ast.span.clone()),
                errors,
            );

            for (_, chain) in ast.chain.iter() {
                infer_type_for_eq_expression(chain, resolved_map, return_type, env, errors);

                env.give_type(
                    Spanned::new(EntityID::from(chain), chain.span.clone()),
                    Spanned::new(Type::Bool, ast.span.clone()),
                    errors,
                );
            }

            env.give_type(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(Type::Bool, ast.span.clone()),
                errors,
            );
        }
    }
}

fn infer_type_for_eq_expression(
    ast: &EqualsOrNotEqualsExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match &ast.chain {
        None => {
            infer_type_for_less_expression(&ast.first, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                errors,
            );
        }
        Some((_, chain)) => {
            infer_type_for_less_expression(&ast.first, resolved_map, return_type, env, errors);

            let last_entity_id = Spanned::new(EntityID::from(&ast.first), ast.first.span.clone());

            let chain_entity_id = Spanned::new(EntityID::from(chain), chain.span.clone());

            env.unify(last_entity_id.clone(), chain_entity_id.clone(), errors);

            infer_type_for_less_expression(chain, resolved_map, return_type, env, errors);

            env.give_type(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(Type::Bool, ast.span.clone()),
                errors,
            );
        }
    }
}

fn infer_type_for_less_expression(
    ast: &LessOrGreaterExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match &ast.chain {
        None => {
            infer_type_for_add_sub_expression(&ast.first, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                errors,
            );
        }
        Some((_, chain)) => {
            infer_type_for_add_sub_expression(&ast.first, resolved_map, return_type, env, errors);

            env.validate_numeric_type(
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                ast.span.clone(),
                errors,
            );

            let last_entity_id = Spanned::new(EntityID::from(&ast.first), ast.first.span.clone());
            let chain_entity_id = Spanned::new(EntityID::from(chain), chain.span.clone());

            env.unify(last_entity_id.clone(), chain_entity_id.clone(), errors);

            infer_type_for_add_sub_expression(chain, resolved_map, return_type, env, errors);

            env.give_type(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(Type::Bool, ast.span.clone()),
                errors,
            );
        }
    }
}

fn infer_type_for_add_sub_expression(
    ast: &AddOrSubExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match ast.chain.len() {
        0 => {
            infer_type_for_mul_div_expression(&ast.first, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                errors,
            );
        }
        _ => {
            infer_type_for_mul_div_expression(&ast.first, resolved_map, return_type, env, errors);

            env.validate_numeric_type(
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                ast.span.clone(),
                errors,
            );

            let mut last_entity_id =
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone());
            for (_, chain) in ast.chain.iter() {
                let chain_entity_id = Spanned::new(EntityID::from(chain), chain.span.clone());

                env.unify(last_entity_id.clone(), chain_entity_id.clone(), errors);

                infer_type_for_mul_div_expression(chain, resolved_map, return_type, env, errors);

                last_entity_id = chain_entity_id;
            }

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                last_entity_id,
                errors,
            );
        }
    }
}

fn infer_type_for_mul_div_expression(
    ast: &MulOrDivExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match ast.chain.len() {
        0 => {
            infer_type_for_factor(&ast.first, resolved_map, return_type, env, errors);

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                errors,
            );
        }
        _ => {
            infer_type_for_factor(&ast.first, resolved_map, return_type, env, errors);

            env.validate_numeric_type(
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone()),
                ast.span.clone(),
                errors,
            );

            let mut last_entity_id =
                Spanned::new(EntityID::from(&ast.first), ast.first.span.clone());
            for (_, chain) in ast.chain.iter() {
                let chain_entity_id = Spanned::new(EntityID::from(chain), chain.span.clone());

                env.unify(last_entity_id.clone(), chain_entity_id.clone(), errors);

                infer_type_for_factor(chain, resolved_map, return_type, env, errors);

                last_entity_id = chain_entity_id;
            }

            env.unify(
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                last_entity_id,
                errors,
            );
        }
    }
}

fn infer_type_for_factor(
    ast: &Factor,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    if let Some(primary) = &ast.primary {
        infer_type_for_primary(primary, resolved_map, return_type, env, errors);

        if ast.minus.is_some() {
            env.validate_numeric_type(
                Spanned::new(EntityID::from(primary), primary.span.clone()),
                ast.span.clone(),
                errors,
            );
        }

        env.unify(
            Spanned::new(EntityID::from(ast), ast.span.clone()),
            Spanned::new(EntityID::from(primary), primary.span.clone()),
            errors,
        );
    }
}

fn infer_type_for_primary(
    ast: &Primary,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    match &ast.first {
        PrimaryLeft::Literal {
            literal,
            function_call,
        } => {
            match resolved_map.get(&EntityID::from(literal)) {
                Some(define) => match define.kind {
                    DefineKind::UserType => {
                        let error = TypeError {
                            kind: TypeErrorKind::InvalidVariableByUserType {
                                user_type: define.span.clone(),
                            },
                            span: literal.span.clone(),
                        };
                        errors.push(error);

                        env.define_entity_type(EntityID::from(literal), Type::Unknown);
                    }
                    DefineKind::Function => {
                        env.unify(
                            Spanned::new(define.entity_id, define.span.clone()),
                            Spanned::new(EntityID::from(literal), literal.span.clone()),
                            errors,
                        );
                    }
                    DefineKind::Variable => {
                        env.unify(
                            Spanned::new(define.entity_id, define.span.clone()),
                            Spanned::new(EntityID::from(literal), literal.span.clone()),
                            errors,
                        );
                    }
                },
                None => match literal.value {
                    "print" => env.define_entity_type(
                        EntityID::from(literal),
                        Type::Function {
                            arguments: vec![Type::String],
                            return_type: Box::new(Type::Void),
                            span: literal.span.clone(),
                        },
                    ),
                    "print_int" => env.define_entity_type(
                        EntityID::from(literal),
                        Type::Function {
                            arguments: vec![Type::Int],
                            return_type: Box::new(Type::Void),
                            span: literal.span.clone(),
                        },
                    ),
                    _ => env.define_entity_type(EntityID::from(literal), Type::Unknown),
                },
            }

            match function_call {
                Some(function_call) => {
                    let ty = env.resolve_entity_type(EntityID::from(literal)).unwrap();

                    match ty {
                        Type::Function {
                            arguments,
                            return_type: function_return_type,
                            span,
                        } => {
                            if arguments.len() != function_call.arguments.len() {
                                let error = TypeError {
                                    kind: TypeErrorKind::InvalidArgumentCount {
                                        expected: Spanned::new(arguments.len(), span.clone()),
                                        found: Spanned::new(
                                            function_call.arguments.len(),
                                            function_call.span.clone(),
                                        ),
                                    },
                                    span: function_call.span.clone(),
                                };
                                errors.push(error);
                            }

                            let mut argument_entity_ids = Vec::new();
                            for argument in function_call.arguments.iter() {
                                infer_type_for_expression(
                                    argument,
                                    resolved_map,
                                    return_type,
                                    env,
                                    errors,
                                );

                                argument_entity_ids
                                    .push(Spanned::new(EntityID::from(argument), argument.span()));
                            }

                            for (argument_entity_id, argument_type) in
                                argument_entity_ids.into_iter().zip(arguments.iter())
                            {
                                env.give_type(
                                    argument_entity_id,
                                    Spanned::new(argument_type.clone(), span.clone()),
                                    errors,
                                );
                            }

                            env.give_type(
                                Spanned::new(
                                    EntityID::from(&ast.first),
                                    literal.span.start..function_call.span.end,
                                ),
                                Spanned::new(
                                    function_return_type.as_ref().clone(),
                                    function_call.span.clone(),
                                ),
                                errors,
                            );
                        }
                        _ => {
                            let error = TypeError {
                                kind: TypeErrorKind::NotFunctionType {
                                    found: Spanned::new(ty, literal.span.clone()),
                                },
                                span: literal.span.clone(),
                            };
                            errors.push(error);

                            env.define_entity_type(EntityID::from(&ast.first), Type::Unknown);
                        }
                    }
                }
                None => {
                    env.unify(
                        Spanned::new(EntityID::from(&ast.first), literal.span.clone()),
                        Spanned::new(EntityID::from(literal), literal.span.clone()),
                        errors,
                    );
                }
            }
        }
        PrimaryLeft::NumericLiteral { literal } => {
            let ty = match literal.value.contains(".") {
                true => Type::Float,
                false => Type::Int,
            };
            env.give_type(
                Spanned::new(EntityID::from(&ast.first), literal.span.clone()),
                Spanned::new(ty, literal.span.clone()),
                errors,
            );
        }
        PrimaryLeft::StringLiteral { literal } => {
            env.give_type(
                Spanned::new(EntityID::from(&ast.first), literal.span.clone()),
                Spanned::new(Type::String, literal.span.clone()),
                errors,
            );
        }
    }

    let mut last_entity_id = Spanned::new(EntityID::from(&ast.first), ast.first.span());

    for chain in ast.chain.iter() {
        match env.resolve_entity_type(last_entity_id.value) {
            Some(ty) => match &ty {
                Type::Class {
                    entity_id: _,
                    name: _,
                    fields,
                    span,
                } => {
                    match fields
                        .lock()
                        .unwrap()
                        .iter()
                        .find(|(name, _)| &name.value == chain.literal.value)
                        .cloned()
                    {
                        Some((_, ty)) => {
                            env.define_entity_type(EntityID::from(chain), ty);
                        }
                        None => {
                            let error = TypeError {
                                kind: TypeErrorKind::NoClassFieldFound {
                                    not_found: chain.literal.value.to_string(),
                                    class: Spanned::new(ty.clone(), span.clone()),
                                },
                                span: chain.span.clone(),
                            };
                            errors.push(error);

                            env.define_entity_type(EntityID::from(chain), Type::Unknown);
                        }
                    }
                }
                _ => {
                    let error = TypeError {
                        kind: TypeErrorKind::NotClassType {
                            found: Spanned::new(ty.clone(), last_entity_id.span.clone()),
                        },
                        span: chain.span.clone(),
                    };
                    errors.push(error);

                    env.define_entity_type(EntityID::from(chain), Type::Unknown);
                }
            },
            None => {
                let error = TypeError {
                    kind: TypeErrorKind::UnknownTypeAtThisPoint,
                    span: last_entity_id.span.clone(),
                };
                errors.push(error);
            }
        }

        last_entity_id = Spanned::new(EntityID::from(chain), chain.span.clone());
    }

    env.unify(
        Spanned::new(EntityID::from(ast), ast.span.clone()),
        last_entity_id,
        errors,
    );
}

fn infer_type_for_new_expression(
    ast: &NewExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    let Some(name) = &ast.name else { return };
    let Some(define) = resolved_map.get(&EntityID::from(name)) else {
        return;
    };

    match define.kind {
        DefineKind::UserType => {
            let ty = env.resolve_entity_type(define.entity_id).unwrap();
            let Type::Class {
                entity_id: _,
                name: _,
                fields,
                span: class_span,
            } = &ty
            else {
                unreachable!()
            };

            let fields = fields.lock().unwrap();

            for field_assign in ast.field_assign.iter() {
                match fields
                    .iter()
                    .find(|(name, _)| &name.value == field_assign.name.value)
                {
                    Some((name, ty)) => {
                        infer_type_for_expression(
                            &field_assign.expression,
                            resolved_map,
                            return_type,
                            env,
                            errors,
                        );

                        env.give_type(
                            Spanned::new(
                                EntityID::from(&field_assign.expression),
                                field_assign.expression.span(),
                            ),
                            Spanned::new(ty.clone(), name.span.clone()),
                            errors,
                        );
                    }
                    None => {
                        let error = TypeError {
                            kind: TypeErrorKind::UnknownField {
                                field: Spanned::new(
                                    field_assign.name.value.to_string(),
                                    field_assign.name.span.clone(),
                                ),
                                class: Spanned::new(ty.clone(), class_span.clone()),
                            },
                            span: field_assign.span.clone(),
                        };
                        errors.push(error);
                    }
                }
            }

            let field_assign_names: HashSet<_> = ast
                .field_assign
                .iter()
                .map(|assign| assign.name.value)
                .collect();

            for (origin_field_name, _) in fields.iter() {
                if !field_assign_names.contains(&origin_field_name.value.as_str()) {
                    let error = TypeError {
                        kind: TypeErrorKind::MissingField {
                            missing: origin_field_name.clone(),
                        },
                        span: ast.span.clone(),
                    };
                    errors.push(error);
                }
            }

            env.define_entity_type(EntityID::from(ast), ty.clone());
        }
        DefineKind::Function => {
            let error = TypeError {
                kind: TypeErrorKind::InvalidClassTypeByFunction {
                    function: define.span.clone(),
                },
                span: name.span.clone(),
            };
            errors.push(error);

            env.define_entity_type(EntityID::from(ast), Type::Unknown);
        }
        DefineKind::Variable => {
            let error = TypeError {
                kind: TypeErrorKind::InvalidClassTypeByVariable {
                    variable: define.span.clone(),
                },
                span: name.span.clone(),
            };
            errors.push(error);

            env.define_entity_type(EntityID::from(ast), Type::Unknown);
        }
    }
}

fn infer_type_for_return_expression(
    ast: &ReturnExpression,
    resolved_map: &HashMap<EntityID, Define>,
    return_type: &Spanned<Type>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) {
    let ty = match &ast.expression {
        Some(expression) => {
            infer_type_for_expression(&expression, resolved_map, return_type, env, errors);

            env.resolve_entity_type(EntityID::from(expression.as_ref()))
                .unwrap()
        }
        None => Type::Void,
    };

    if !env.unify_type(&return_type.value, &ty) {
        let error = TypeError {
            kind: TypeErrorKind::UnexpectedType {
                expected: return_type.clone(),
                found: Spanned::new(ty, ast.span.clone()),
            },
            span: ast.span.clone(),
        };
        errors.push(error);
    }

    env.define_entity_type(EntityID::from(ast), Type::Unreachable);
}

fn get_type(
    ast: &TypeInfo,
    resolved_map: &HashMap<EntityID, Define>,
    env: &mut TypeEnvironment,
    errors: &mut Vec<TypeError>,
) -> Type {
    match resolved_map.get(&EntityID::from(&ast.name)) {
        Some(define) => match define.kind {
            DefineKind::UserType => env.resolve_entity_type(define.entity_id).unwrap(),
            DefineKind::Function => {
                let error = TypeError {
                    kind: TypeErrorKind::InvalidTypeByFunction {
                        function: define.span.clone(),
                    },
                    span: ast.name.span.clone(),
                };
                errors.push(error);
                Type::Unknown
            }
            DefineKind::Variable => {
                let error = TypeError {
                    kind: TypeErrorKind::InvalidTypeByVariable {
                        variable: define.span.clone(),
                    },
                    span: ast.name.span.clone(),
                };
                errors.push(error);
                Type::Unknown
            }
        },
        None => match ast.name.value {
            "int" => Type::Int,
            "float" => Type::Float,
            _ => Type::Unknown,
        },
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        ast::Spanned,
        lexer::Lexer,
        name::{NameResolverContainer, resolve_name_for_program},
        parser::parse_program,
        types::{Type, TypeEnvironment, infer_type_for_program},
    };

    #[test]
    fn infer() {
        let source = "
let a;
let b;
a = b;
b = 100;
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

        let mut env = TypeEnvironment::new();
        let mut errors = Vec::new();
        infer_type_for_program(
            &ast,
            &resolved_map,
            &Spanned::new(Type::Void, 0..source.len()),
            &mut env,
            &mut errors,
        );

        dbg!(errors);

        for variable in env.variables.iter() {
            println!(
                "{}: {}",
                &source[variable.span.clone()],
                env.resolve_entity_type(variable.value)
                    .unwrap_or(Type::Unknown)
                    .to_display_string()
            );
        }
    }
}
