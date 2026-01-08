use std::ops::Range;

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
