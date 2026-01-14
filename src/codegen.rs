use std::collections::HashMap;

use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{
        AddOrSub, AddOrSubExpression, AndExpression, ElseOrElseIf, EntityID,
        EqualsOrNotEqualsExpression, EqualsOrNotEqusls, Expression, Factor,
        LessOrGreaterExpression, LessOrGreaterThan, MulOrDiv, MulOrDivExpression, NewExpression,
        OrExpression, Primary, PrimaryLeft, Program, ReturnExpression, Statement,
    },
    name::Define,
    types::Type,
};

fn into_llvm_value_type<'ctx>(context: &'ctx Context, ty: &Type) -> Option<BasicTypeEnum<'ctx>> {
    match ty {
        Type::Int => Some(context.i64_type().into()),
        Type::Float => Some(context.f64_type().into()),
        Type::Bool => Some(context.bool_type().into()),
        Type::Void => None,
        Type::String => Some(context.ptr_type(AddressSpace::default()).into()),
        Type::Class {
            entity_id: _,
            name: _,
            fields: _,
            span: _,
        } => Some(context.ptr_type(AddressSpace::default()).into()),
        Type::Function {
            arguments: _,
            return_type: _,
            span: _,
        } => None,
        Type::Unknown => None,
        Type::Unreachable => None,
    }
}

pub fn codegen_for_program<'ctx>(
    ast: &Program,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) {
    for statement in ast.statements.iter() {
        match statement {
            Statement::ClassDefine(class_define) => {
                let name = class_define.name.as_ref().unwrap();

                let ty = type_map.get(&EntityID::from(name)).unwrap();
                let Type::Class {
                    entity_id: _,
                    name: _,
                    fields,
                    span: _,
                } = ty
                else {
                    unreachable!()
                };
                let fields = fields.lock().unwrap();

                let struct_type = context.struct_type(
                    fields
                        .iter()
                        .map(|(_, ty)| into_llvm_value_type(context, ty).unwrap().into())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                );

                class_map.insert(EntityID::from(name), struct_type);
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match statement {
            Statement::FunctionDefine(function_define) => {
                let name = function_define.name.as_ref().unwrap();

                let function_type = type_map.get(&EntityID::from(name)).unwrap();

                let Type::Function {
                    arguments,
                    return_type,
                    span: _,
                } = function_type
                else {
                    unreachable!()
                };

                let return_type = into_llvm_value_type(context, &return_type);

                let function_type = match return_type {
                    Some(return_type) => return_type.fn_type(
                        arguments
                            .iter()
                            .map(|argument| into_llvm_value_type(context, argument).unwrap().into())
                            .collect::<Vec<_>>()
                            .as_slice(),
                        false,
                    ),
                    None => context.void_type().fn_type(
                        arguments
                            .iter()
                            .map(|argument| into_llvm_value_type(context, argument).unwrap().into())
                            .collect::<Vec<_>>()
                            .as_slice(),
                        false,
                    ),
                };

                let function = module.add_function(name.value, function_type, None);

                let entry_block = context.append_basic_block(function, "entry");
                builder.position_at_end(entry_block);

                for (argument, value) in function_define
                    .arguments
                    .iter()
                    .zip(function.get_params().into_iter())
                {
                    variable_map.insert(EntityID::from(&argument.name), value);
                }

                function_map.insert(EntityID::from(name), function);

                codegen_for_program(
                    &function_define.block.as_ref().unwrap().program,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    Some(function),
                    variable_map,
                    function_map,
                    class_map,
                );
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match statement {
            Statement::LetStatement(let_statement) => {
                let name = let_statement.name.as_ref().unwrap();
                let ty = type_map.get(&EntityID::from(name)).unwrap();
                let ty = into_llvm_value_type(context, ty).unwrap();
                let ptr = builder.build_alloca(ty, name.value).unwrap();

                variable_map.insert(EntityID::from(name), ptr.as_basic_value_enum());

                if let Some(expression) = &let_statement.expression {
                    let value = codegen_for_expression(
                        expression,
                        context,
                        builder,
                        module,
                        resolved_map,
                        type_map,
                        function_value,
                        variable_map,
                        function_map,
                        class_map,
                    );

                    builder.build_store(ptr, value).unwrap();
                }
            }
            Statement::Assignment(assignment) => {
                let ptr = codegen_for_primary(
                    &assignment.left,
                    false,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                let right = assignment.right.as_ref().clone().unwrap();

                let value = codegen_for_expression(
                    right,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                builder
                    .build_store(ptr.into_pointer_value(), value)
                    .unwrap();
            }
            Statement::Expression(expression) => {
                codegen_for_expression(
                    expression,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );
            }
            Statement::FunctionDefine(_) => {}
            Statement::ClassDefine(class_define) => {
                let name = class_define.name.as_ref().unwrap();

                let ty = type_map.get(&EntityID::from(name)).unwrap();
                let Type::Class {
                    entity_id: _,
                    name: _,
                    fields,
                    span: _,
                } = ty
                else {
                    unreachable!()
                };
                let fields = fields.lock().unwrap();

                let struct_type = context.struct_type(
                    fields
                        .iter()
                        .map(|(_, ty)| into_llvm_value_type(context, ty).unwrap().into())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                );

                class_map.insert(EntityID::from(name), struct_type);
            }
            Statement::IfStatement(if_statement) => {
                let first_condition = codegen_for_expression(
                    if_statement.first.condition.as_ref().unwrap(),
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                let then_block = context.append_basic_block(function_value.unwrap(), "then");
                let mut else_block = context.append_basic_block(function_value.unwrap(), "else");
                let finnaly_block = context.append_basic_block(function_value.unwrap(), "finnaly");

                builder
                    .build_conditional_branch(
                        first_condition.into_int_value(),
                        then_block,
                        else_block,
                    )
                    .unwrap();

                builder.position_at_end(then_block);
                codegen_for_program(
                    &if_statement.first.block.as_ref().unwrap().program,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                for chain in if_statement.chain.iter() {
                    match chain {
                        ElseOrElseIf::Else { block, span: _ } => {
                            builder.position_at_end(else_block);
                            codegen_for_program(
                                &block.as_ref().unwrap().program,
                                context,
                                builder,
                                module,
                                resolved_map,
                                type_map,
                                function_value,
                                variable_map,
                                function_map,
                                class_map,
                            );
                        }
                        ElseOrElseIf::ElseIf {
                            condition,
                            block,
                            span: _,
                        } => {
                            builder.position_at_end(else_block);

                            let condition = codegen_for_expression(
                                condition.as_ref().unwrap(),
                                context,
                                builder,
                                module,
                                resolved_map,
                                type_map,
                                function_value,
                                variable_map,
                                function_map,
                                class_map,
                            );

                            let then_block =
                                context.append_basic_block(function_value.unwrap(), "then");
                            else_block =
                                context.append_basic_block(function_value.unwrap(), "else");

                            builder
                                .build_conditional_branch(
                                    condition.into_int_value(),
                                    then_block,
                                    else_block,
                                )
                                .unwrap();

                            builder.position_at_end(then_block);
                            codegen_for_program(
                                &block.as_ref().unwrap().program,
                                context,
                                builder,
                                module,
                                resolved_map,
                                type_map,
                                function_value,
                                variable_map,
                                function_map,
                                class_map,
                            );
                        }
                    }
                }

                if if_statement.chain.is_empty() {
                    builder.build_unconditional_branch(finnaly_block).unwrap();

                    builder.position_at_end(else_block);
                    builder.build_unconditional_branch(finnaly_block).unwrap();

                    builder.position_at_end(finnaly_block);
                } else {
                    builder.build_unconditional_branch(finnaly_block).unwrap();

                    builder.position_at_end(then_block);
                    builder.build_unconditional_branch(finnaly_block).unwrap();

                    builder.position_at_end(finnaly_block);
                }
            }
        }
    }
}

fn codegen_for_expression<'ctx>(
    ast: &Expression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match ast {
        Expression::OrExpression(or_expression) => codegen_for_or_expression(
            or_expression,
            context,
            builder,
            module,
            resolved_map,
            type_map,
            function_value,
            variable_map,
            function_map,
            class_map,
        ),
        Expression::NewExpression(new_expression) => codegen_for_new_expression(
            new_expression,
            context,
            builder,
            module,
            resolved_map,
            type_map,
            function_value,
            variable_map,
            function_map,
            class_map,
        ),
        Expression::ReturnExpression(return_expression) => codegen_for_return_expression(
            return_expression,
            context,
            builder,
            module,
            resolved_map,
            type_map,
            function_value,
            variable_map,
            function_map,
            class_map,
        ),
    }
}

fn codegen_for_new_expression<'ctx>(
    ast: &NewExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let name = ast.name.as_ref().unwrap();
    let resolved = resolved_map.get(&EntityID::from(name)).unwrap();
    let struct_type = class_map.get(&resolved.entity_id).unwrap().clone();

    let Type::Class {
        entity_id: _,
        name: _,
        fields,
        span: _,
    } = type_map.get(&resolved.entity_id).unwrap()
    else {
        unreachable!()
    };
    let fields = fields.lock().unwrap();

    let malloc = builder.build_malloc(struct_type, "malloc").unwrap();

    for field_assign in ast.field_assign.iter() {
        let value = codegen_for_expression(
            &field_assign.expression,
            context,
            builder,
            module,
            resolved_map,
            type_map,
            function_value,
            variable_map,
            function_map,
            class_map,
        );

        let ptr = builder
            .build_struct_gep(
                struct_type,
                malloc,
                fields
                    .iter()
                    .position(|(name, _)| &name.value == field_assign.name.value)
                    .unwrap() as _,
                "gep",
            )
            .unwrap();

        builder.build_store(ptr, value).unwrap();
    }

    malloc.as_basic_value_enum()
}

fn codegen_for_return_expression<'ctx>(
    ast: &ReturnExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match &ast.expression {
        Some(expression) => {
            let value = codegen_for_expression(
                &expression,
                context,
                builder,
                module,
                resolved_map,
                type_map,
                function_value,
                variable_map,
                function_map,
                class_map,
            );

            builder.build_return(Some(&value)).unwrap();
        }
        None => {
            builder.build_return(None).unwrap();
        }
    }

    context.i8_type().const_zero().as_basic_value_enum()
}

fn codegen_for_or_expression<'ctx>(
    ast: &OrExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let first = codegen_for_and_expression(
        &ast.first,
        context,
        builder,
        module,
        resolved_map,
        type_map,
        function_value,
        variable_map,
        function_map,
        class_map,
    );

    match ast.chain.len() {
        0 => first,
        _ => {
            let mut value = first;

            for (_, chain) in ast.chain.iter() {
                let chain_value = codegen_for_and_expression(
                    chain,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                value = builder
                    .build_or(value.into_int_value(), chain_value.into_int_value(), "or")
                    .unwrap()
                    .into();
            }

            value
        }
    }
}

fn codegen_for_and_expression<'ctx>(
    ast: &AndExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let first = codegen_for_equals_expression(
        &ast.first,
        context,
        builder,
        module,
        resolved_map,
        type_map,
        function_value,
        variable_map,
        function_map,
        class_map,
    );

    match ast.chain.len() {
        0 => first,
        _ => {
            let mut value = first;

            for (_, chain) in ast.chain.iter() {
                let chain_value = codegen_for_equals_expression(
                    chain,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                value = builder
                    .build_and(value.into_int_value(), chain_value.into_int_value(), "or")
                    .unwrap()
                    .into();
            }

            value
        }
    }
}

fn codegen_for_equals_expression<'ctx>(
    ast: &EqualsOrNotEqualsExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match &ast.chain {
        Some((op, chain)) => {
            let value = codegen_for_less_expression(
                &ast.first,
                context,
                builder,
                module,
                resolved_map,
                type_map,
                function_value,
                variable_map,
                function_map,
                class_map,
            );

            let chain_value = codegen_for_less_expression(
                &chain,
                context,
                builder,
                module,
                resolved_map,
                type_map,
                function_value,
                variable_map,
                function_map,
                class_map,
            );

            match type_map.get(&EntityID::from(&ast.first)).unwrap() {
                Type::Int | Type::Bool => {
                    let op = match op.value {
                        EqualsOrNotEqusls::Equals => IntPredicate::EQ,
                        EqualsOrNotEqusls::NotEquals => IntPredicate::NE,
                    };
                    builder
                        .build_int_compare(
                            op,
                            value.into_int_value(),
                            chain_value.into_int_value(),
                            "compare",
                        )
                        .unwrap()
                        .into()
                }
                Type::Float => {
                    let op = match op.value {
                        EqualsOrNotEqusls::Equals => FloatPredicate::OEQ,
                        EqualsOrNotEqusls::NotEquals => FloatPredicate::ONE,
                    };
                    builder
                        .build_float_compare(
                            op,
                            value.into_float_value(),
                            chain_value.into_float_value(),
                            "compare",
                        )
                        .unwrap()
                        .into()
                }
                Type::String
                | Type::Class {
                    entity_id: _,
                    name: _,
                    fields: _,
                    span: _,
                } => todo!(),
                _ => unreachable!(),
            }
        }
        None => codegen_for_less_expression(
            &ast.first,
            context,
            builder,
            module,
            resolved_map,
            type_map,
            function_value,
            variable_map,
            function_map,
            class_map,
        ),
    }
}

fn codegen_for_less_expression<'ctx>(
    ast: &LessOrGreaterExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    match &ast.chain {
        Some((op, chain)) => {
            let value = codegen_for_add_expression(
                &ast.first,
                context,
                builder,
                module,
                resolved_map,
                type_map,
                function_value,
                variable_map,
                function_map,
                class_map,
            );

            let chain_value = codegen_for_add_expression(
                &chain,
                context,
                builder,
                module,
                resolved_map,
                type_map,
                function_value,
                variable_map,
                function_map,
                class_map,
            );

            match type_map.get(&EntityID::from(&ast.first)).unwrap() {
                Type::Int | Type::Bool => {
                    let op = match op.value {
                        LessOrGreaterThan::LessThan => IntPredicate::SLT,
                        LessOrGreaterThan::LessThanOrEquals => IntPredicate::SLE,
                        LessOrGreaterThan::GreaterThan => IntPredicate::SGT,
                        LessOrGreaterThan::GreaterThanOrEquals => IntPredicate::SGE,
                    };
                    builder
                        .build_int_compare(
                            op,
                            value.into_int_value(),
                            chain_value.into_int_value(),
                            "compare",
                        )
                        .unwrap()
                        .into()
                }
                Type::Float => {
                    let op = match op.value {
                        LessOrGreaterThan::LessThan => FloatPredicate::OLT,
                        LessOrGreaterThan::LessThanOrEquals => FloatPredicate::OLE,
                        LessOrGreaterThan::GreaterThan => FloatPredicate::OGT,
                        LessOrGreaterThan::GreaterThanOrEquals => FloatPredicate::OGE,
                    };
                    builder
                        .build_float_compare(
                            op,
                            value.into_float_value(),
                            chain_value.into_float_value(),
                            "compare",
                        )
                        .unwrap()
                        .into()
                }
                Type::String
                | Type::Class {
                    entity_id: _,
                    name: _,
                    fields: _,
                    span: _,
                } => todo!(),
                _ => unreachable!(),
            }
        }
        None => codegen_for_add_expression(
            &ast.first,
            context,
            builder,
            module,
            resolved_map,
            type_map,
            function_value,
            variable_map,
            function_map,
            class_map,
        ),
    }
}

fn codegen_for_add_expression<'ctx>(
    ast: &AddOrSubExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let first = codegen_for_mul_expression(
        &ast.first,
        context,
        builder,
        module,
        resolved_map,
        type_map,
        function_value,
        variable_map,
        function_map,
        class_map,
    );

    match ast.chain.len() {
        0 => first,
        _ => {
            let mut value = first;

            for (op, chain) in ast.chain.iter() {
                let chain_value = codegen_for_mul_expression(
                    chain,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                value = match type_map.get(&EntityID::from(&ast.first)).unwrap() {
                    Type::Int | Type::Bool => match op.value {
                        AddOrSub::Add => builder
                            .build_int_add(
                                value.into_int_value(),
                                chain_value.into_int_value(),
                                "add",
                            )
                            .unwrap()
                            .into(),
                        AddOrSub::Sub => builder
                            .build_int_sub(
                                value.into_int_value(),
                                chain_value.into_int_value(),
                                "sub",
                            )
                            .unwrap()
                            .into(),
                    },
                    Type::Float => match op.value {
                        AddOrSub::Add => builder
                            .build_float_add(
                                value.into_float_value(),
                                chain_value.into_float_value(),
                                "add",
                            )
                            .unwrap()
                            .into(),
                        AddOrSub::Sub => builder
                            .build_float_sub(
                                value.into_float_value(),
                                chain_value.into_float_value(),
                                "sub",
                            )
                            .unwrap()
                            .into(),
                    },
                    Type::String
                    | Type::Class {
                        entity_id: _,
                        name: _,
                        fields: _,
                        span: _,
                    } => todo!(),
                    _ => unreachable!(),
                };
            }

            value
        }
    }
}

fn codegen_for_mul_expression<'ctx>(
    ast: &MulOrDivExpression,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let first = codegen_for_factor(
        &ast.first,
        context,
        builder,
        module,
        resolved_map,
        type_map,
        function_value,
        variable_map,
        function_map,
        class_map,
    );

    match ast.chain.len() {
        0 => first,
        _ => {
            let mut value = first;

            for (op, chain) in ast.chain.iter() {
                let chain_value = codegen_for_factor(
                    chain,
                    context,
                    builder,
                    module,
                    resolved_map,
                    type_map,
                    function_value,
                    variable_map,
                    function_map,
                    class_map,
                );

                value = match type_map.get(&EntityID::from(&ast.first)).unwrap() {
                    Type::Int | Type::Bool => match op.value {
                        MulOrDiv::Mul => builder
                            .build_int_mul(
                                value.into_int_value(),
                                chain_value.into_int_value(),
                                "add",
                            )
                            .unwrap()
                            .into(),
                        MulOrDiv::Div => builder
                            .build_int_signed_div(
                                value.into_int_value(),
                                chain_value.into_int_value(),
                                "sub",
                            )
                            .unwrap()
                            .into(),
                    },
                    Type::Float => match op.value {
                        MulOrDiv::Mul => builder
                            .build_float_mul(
                                value.into_float_value(),
                                chain_value.into_float_value(),
                                "add",
                            )
                            .unwrap()
                            .into(),
                        MulOrDiv::Div => builder
                            .build_float_div(
                                value.into_float_value(),
                                chain_value.into_float_value(),
                                "sub",
                            )
                            .unwrap()
                            .into(),
                    },
                    Type::String
                    | Type::Class {
                        entity_id: _,
                        name: _,
                        fields: _,
                        span: _,
                    } => todo!(),
                    _ => unreachable!(),
                };
            }

            value
        }
    }
}

fn codegen_for_factor<'ctx>(
    ast: &Factor,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    let primary = ast.primary.as_ref().unwrap();

    let value = codegen_for_primary(
        primary,
        true,
        context,
        builder,
        module,
        resolved_map,
        type_map,
        function_value,
        variable_map,
        function_map,
        class_map,
    );

    match &ast.minus {
        Some(_) => match type_map.get(&EntityID::from(primary)).unwrap() {
            Type::Int | Type::Bool => builder
                .build_int_neg(value.into_int_value(), "neg")
                .unwrap()
                .into(),
            Type::Float => builder
                .build_float_neg(value.into_float_value(), "neg")
                .unwrap()
                .into(),
            _ => unreachable!(),
        },
        None => value,
    }
}

fn codegen_for_primary<'ctx>(
    ast: &Primary,
    load: bool,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    module: &Module<'ctx>,
    resolved_map: &HashMap<EntityID, Define>,
    type_map: &HashMap<EntityID, Type>,
    function_value: Option<FunctionValue<'ctx>>,
    variable_map: &mut HashMap<EntityID, BasicValueEnum<'ctx>>,
    function_map: &mut HashMap<EntityID, FunctionValue<'ctx>>,
    class_map: &mut HashMap<EntityID, StructType<'ctx>>,
) -> BasicValueEnum<'ctx> {
    fn deref_value<'ctx>(
        value: BasicValueEnum<'ctx>,
        builder: &Builder<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match <BasicValueEnum<'_> as TryInto<PointerValue<'_>>>::try_into(value) {
            Ok(ptr) => builder.build_load(ty, ptr, "load").unwrap(),
            Err(_) => value,
        }
    }

    let mut value = match &ast.first {
        PrimaryLeft::Literal {
            literal,
            function_call,
        } => match function_call {
            Some(function_call) => {
                let function = match literal.value {
                    "print" => module.get_function("print").unwrap(),
                    "print_int" => module.get_function("print_int").unwrap(),
                    _ => {
                        let resolved = resolved_map.get(&EntityID::from(literal)).unwrap();
                        function_map.get(&resolved.entity_id).unwrap().clone()
                    }
                };

                let result = builder
                    .build_direct_call(
                        function,
                        function_call
                            .arguments
                            .iter()
                            .map(|argument| {
                                codegen_for_expression(
                                    argument,
                                    context,
                                    builder,
                                    module,
                                    resolved_map,
                                    type_map,
                                    function_value,
                                    variable_map,
                                    function_map,
                                    class_map,
                                )
                                .into()
                            })
                            .collect::<Vec<_>>()
                            .as_slice(),
                        "call",
                    )
                    .unwrap();

                result
                    .try_as_basic_value()
                    .basic()
                    .unwrap_or(context.i8_type().const_zero().as_basic_value_enum())
            }
            None => {
                let resolved = resolved_map.get(&EntityID::from(literal)).unwrap();
                let value = variable_map
                    .get(&resolved.entity_id)
                    .unwrap()
                    .as_basic_value_enum();

                match load {
                    true => match function_value.unwrap().get_params().contains(&value) {
                        true => value,
                        false => deref_value(
                            value,
                            builder,
                            into_llvm_value_type(
                                context,
                                type_map.get(&EntityID::from(literal)).unwrap(),
                            )
                            .unwrap(),
                        ),
                    },
                    false => value,
                }
            }
        },
        PrimaryLeft::NumericLiteral { literal } => match literal.value.contains(".") {
            true => context
                .f64_type()
                .const_float(literal.value.parse::<f64>().unwrap())
                .as_basic_value_enum(),
            false => context
                .i64_type()
                .const_int(literal.value.parse::<i64>().unwrap() as _, true)
                .as_basic_value_enum(),
        },
        PrimaryLeft::StringLiteral { literal } => builder
            .build_global_string_ptr(&literal.value[1..(literal.value.len() - 1)], "string")
            .unwrap()
            .as_basic_value_enum(),
    };

    let mut last_entity_id = EntityID::from(&ast.first);

    for (index, chain) in ast.chain.iter().enumerate() {
        let Type::Class {
            entity_id,
            name: _,
            fields,
            span: _,
        } = type_map.get(&last_entity_id).unwrap()
        else {
            unreachable!()
        };
        let fields = fields.lock().unwrap();

        let field_ptr = builder
            .build_struct_gep(
                class_map.get(entity_id).unwrap().clone(),
                value.into_pointer_value(),
                fields
                    .iter()
                    .position(|field| &field.0.value == chain.literal.value)
                    .unwrap() as _,
                "gep",
            )
            .unwrap();

        let is_last = index == ast.chain.len() - 1;

        value = match is_last || load {
            true => field_ptr.as_basic_value_enum(),
            false => deref_value(
                field_ptr.as_basic_value_enum(),
                builder,
                into_llvm_value_type(context, type_map.get(&EntityID::from(chain)).unwrap())
                    .unwrap(),
            ),
        };
        last_entity_id = EntityID::from(chain);
    }

    value
}

#[cfg(test)]
mod test {
    use std::{
        collections::HashMap,
        ffi::{self},
    };

    use inkwell::{AddressSpace, OptimizationLevel, context::Context};

    use crate::{
        ast::Spanned,
        codegen::codegen_for_program,
        lexer::Lexer,
        name::{NameResolverContainer, resolve_name_for_program},
        parser::parse_program,
        types::{Type, TypeEnvironment, infer_type_for_program},
    };

    #[test]
    fn llvm() {
        let context = Context::create();
        let module = context.create_module("module");

        let int_type = context.i64_type();
        let function_type = int_type.fn_type(&[int_type.into(), int_type.into()], false);

        let function = module.add_function("calc", function_type, None);
        let builder = context.create_builder();

        let block = context.append_basic_block(function, "block");
        builder.position_at_end(block);

        let param0 = function.get_nth_param(0).unwrap();
        let param1 = function.get_nth_param(1).unwrap();

        let add = builder
            .build_int_add(param0.into_int_value(), param1.into_int_value(), "sum")
            .unwrap();

        builder.build_return(Some(&add)).unwrap();

        let jit_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        let calc = unsafe {
            jit_engine
                .get_function::<unsafe extern "C" fn(i64, i64) -> i64>("calc")
                .unwrap()
        };

        let result = unsafe { calc.into_raw()(100, 200) };

        dbg!(result);

        assert_eq!(result, 100 + 200);
    }

    #[test]
    fn codegen() {
        let source = r#"
function main() {
    print("Hello, world!");
    return;
}
        "#;

        let mut lexer = Lexer::new(source);
        let mut errors = Vec::new();

        let ast = parse_program(&mut lexer, &mut errors, &[]);

        for token in Lexer::new(source) {
            dbg!(token.kind);
        }

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

        let type_map = env.dump();

        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("module");

        let print_str_function = module.add_function(
            "print",
            context
                .void_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            None,
        );
        let print_int_function = module.add_function(
            "print_int",
            context
                .void_type()
                .fn_type(&[context.i64_type().into()], false),
            None,
        );

        codegen_for_program(
            &ast,
            &context,
            &builder,
            &module,
            &resolved_map,
            &type_map,
            None,
            &mut HashMap::new(),
            &mut HashMap::new(),
            &mut HashMap::new(),
        );

        unsafe extern "C" fn print_str(char: *const i8) {
            println!("{}", unsafe { ffi::CStr::from_ptr(char).to_str().unwrap() });
        }

        unsafe extern "C" fn print_int(int: i64) {
            println!("{}", int);
        }

        println!("{}", module.to_string());

        let jit_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        jit_engine.add_global_mapping(&print_str_function, print_str as usize);
        jit_engine.add_global_mapping(&print_int_function, print_int as usize);

        let main = unsafe {
            jit_engine
                .get_function::<unsafe extern "C" fn()>("main")
                .unwrap()
        };

        unsafe { main.into_raw()() };
    }
}
