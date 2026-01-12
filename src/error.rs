use std::ops::Range;

use crate::{ast::Spanned, types::Type};

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    MissingLetName,
    MissingLetEqual,
    MissingSemiColon,
    MissingRightExpressionInAssign,
    MissingNameInFunction,
    MissingFunctionArgument,
    InvalidFunctionArgument,
    UnclosedBrace,
    MissingBlock,
    MissingClassName,
    MissingClassField,
    InvalidClassField,
    MissingPrimary,
    UnclosedParen,
    MissingNewName,
    MissingNewFieldAssign,
}

#[derive(Debug)]
pub struct SemanticsError {
    pub kind: SemanticsErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticsErrorKind {
    InvalidAssignLeft,
}

#[derive(Debug)]
pub struct NameResolveError {
    pub kind: NameResolveErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum NameResolveErrorKind {
    UnknownName,
}

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    TypeMismatch {
        left: Spanned<Type>,
        right: Spanned<Type>,
    },
    InvalidTypeByFunction {
        function: Range<usize>,
    },
    InvalidTypeByVariable {
        variable: Range<usize>,
    },
    UnexpectedType {
        expected: Spanned<Type>,
        found: Spanned<Type>,
    },
    NotNumericType {
        expected: Range<usize>,
        found: Spanned<Type>,
    },
    UnknownTypeAtThisPoint,
    InvalidVariableByUserType {
        user_type: Range<usize>,
    },
    NotFunctionType {
        found: Spanned<Type>,
    },
    InvalidArgumentCount {
        expected: Spanned<usize>,
        found: Spanned<usize>,
    },
    NotClassType {
        found: Spanned<Type>,
    },
    NoClassFieldFound {
        not_found: String,
        class: Spanned<Type>,
    },
    InvalidClassTypeByFunction {
        function: Range<usize>,
    },
    InvalidClassTypeByVariable {
        variable: Range<usize>,
    },
    UnknownField {
        field: Spanned<String>,
        class: Spanned<Type>,
    },
    MissingField {
        missing: Spanned<String>,
    },
}
