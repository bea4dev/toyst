use std::{any::TypeId, mem::transmute, ops::Range};

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        Self { value, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityID((TypeId, usize));

impl<T: Sized> From<&T> for EntityID {
    fn from(value: &T) -> Self {
        Self((typeid::of::<T>(), unsafe { transmute(value) }))
    }
}

pub type Literal<'input> = Spanned<&'input str>;

#[derive(Debug)]
pub struct Program<'input> {
    pub statements: Vec<Statement<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Statement<'input> {
    LetStatement(LetStatement<'input>),
    Assignment(Assignment<'input>),
    Expression(Expression<'input>),
    FunctionDefine(FunctionDefine<'input>),
    ClassDefine(ClassDefine<'input>),
    IfStatement(IfStatement<'input>),
}

#[derive(Debug)]
pub struct LetStatement<'input> {
    pub name: Option<Literal<'input>>,
    pub expression: Option<Expression<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Assignment<'input> {
    pub left: Primary<'input>,
    pub right: Option<Expression<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionDefine<'input> {
    pub name: Option<Literal<'input>>,
    pub arguments: Vec<FunctionArgument<'input>>,
    pub return_type: Option<TypeInfo<'input>>,
    pub block: Option<Block<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionArgument<'input> {
    pub name: Literal<'input>,
    pub type_info: TypeInfo<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ClassDefine<'input> {
    pub name: Option<Literal<'input>>,
    pub fields: Vec<ClassField<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ClassField<'input> {
    pub name: Literal<'input>,
    pub type_info: TypeInfo<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct IfStatement<'input> {
    pub first: IfBranch<'input>,
    pub chain: Vec<ElseOrElseIf<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct IfBranch<'input> {
    pub condition: Expression<'input>,
    pub block: Block<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum ElseOrElseIf<'input> {
    Else {
        block: Block<'input>,
        span: Range<usize>,
    },
    ElseIf {
        condition: Expression<'input>,
        block: Block<'input>,
        span: Range<usize>,
    },
}

#[derive(Debug)]
pub struct Block<'input> {
    pub program: Box<Program<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct TypeInfo<'input> {
    pub name: Literal<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum Expression<'input> {
    OrExpression(OrExpression<'input>),
    NewExpression(NewExpression<'input>),
    ReturnExpression(ReturnExpression<'input>),
}

impl Expression<'_> {
    pub fn span(&self) -> Range<usize> {
        match self {
            Expression::OrExpression(or_expression) => or_expression.span.clone(),
            Expression::NewExpression(new_expression) => new_expression.span.clone(),
            Expression::ReturnExpression(return_expression) => return_expression.span.clone(),
        }
    }
}

#[derive(Debug)]
pub struct OrExpression<'input> {
    pub first: AndExpression<'input>,
    pub chain: Vec<(Range<usize>, AndExpression<'input>)>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct AndExpression<'input> {
    pub first: EqualsOrNotEqualsExpression<'input>,
    pub chain: Vec<(Range<usize>, EqualsOrNotEqualsExpression<'input>)>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct EqualsOrNotEqualsExpression<'input> {
    pub first: LessOrGreaterExpression<'input>,
    pub chain: Option<(Spanned<EqualsOrNotEqusls>, LessOrGreaterExpression<'input>)>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualsOrNotEqusls {
    Equals,
    NotEquals,
}

#[derive(Debug)]
pub struct LessOrGreaterExpression<'input> {
    pub first: AddOrSubExpression<'input>,
    pub chain: Option<(Spanned<LessOrGreaterThan>, AddOrSubExpression<'input>)>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LessOrGreaterThan {
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
}

#[derive(Debug)]
pub struct AddOrSubExpression<'input> {
    pub first: MulOrDivExpression<'input>,
    pub chain: Vec<(Spanned<AddOrSub>, MulOrDivExpression<'input>)>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddOrSub {
    Add,
    Sub,
}

#[derive(Debug)]
pub struct MulOrDivExpression<'input> {
    pub first: Factor<'input>,
    pub chain: Vec<(Spanned<MulOrDiv>, Factor<'input>)>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MulOrDiv {
    Mul,
    Div,
}

#[derive(Debug)]
pub struct Factor<'input> {
    pub minus: Option<Range<usize>>,
    pub primary: Option<Primary<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct Primary<'input> {
    pub first: PrimaryLeft<'input>,
    pub chain: Vec<PrimaryRight<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum PrimaryLeft<'input> {
    Literal {
        literal: Literal<'input>,
        function_call: Option<FunctionCall<'input>>,
    },
    NumericLiteral {
        literal: Literal<'input>,
    },
    StringLiteral {
        literal: Literal<'input>,
    },
}

impl PrimaryLeft<'_> {
    pub fn span(&self) -> Range<usize> {
        match self {
            PrimaryLeft::Literal {
                literal,
                function_call,
            } => match function_call {
                Some(function_call) => literal.span.start..function_call.span.end,
                None => literal.span.clone(),
            },
            PrimaryLeft::NumericLiteral { literal } | PrimaryLeft::StringLiteral { literal } => {
                literal.span.clone()
            }
        }
    }
}

#[derive(Debug)]
pub struct PrimaryRight<'input> {
    pub literal: Literal<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionCall<'input> {
    pub arguments: Vec<Expression<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct NewExpression<'input> {
    pub name: Option<Literal<'input>>,
    pub field_assign: Vec<FieldAssign<'input>>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct FieldAssign<'input> {
    pub name: Literal<'input>,
    pub expression: Expression<'input>,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ReturnExpression<'input> {
    pub expression: Option<Box<Expression<'input>>>,
    pub span: Range<usize>,
}
