use std::ops::Range;

use ariadne::{Color, Fmt, Label, Report, ReportKind};

use crate::error::{
    NameResolveError, NameResolveErrorKind, ParseError, ParseErrorKind, SemanticsError,
    SemanticsErrorKind, TypeError, TypeErrorKind,
};

pub trait TakeReport {
    fn take_report(&self, source_name: &str)
    -> Result<(), Vec<Report<'_, (String, Range<usize>)>>>;
}

impl<E> TakeReport for Vec<E>
where
    E: ToReport,
{
    fn take_report(
        &self,
        source_name: &str,
    ) -> Result<(), Vec<Report<'_, (String, Range<usize>)>>> {
        if self.is_empty() {
            return Ok(());
        }

        Err(self
            .iter()
            .map(|error| error.to_report(source_name))
            .collect())
    }
}

pub trait ToReport {
    fn to_report(&self, source_name: &str) -> Report<'_, (String, Range<usize>)>;
}

impl ToReport for ParseError {
    fn to_report(&self, source_name: &str) -> Report<'_, (String, Range<usize>)> {
        let mut builder = Report::build(
            ReportKind::Error,
            (source_name.to_string(), self.span.clone()),
        );

        match self.kind {
            ParseErrorKind::MissingLetName => {
                builder.set_message("let文の名称部分が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'let'のあとには名前が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingLetEqual => {
                builder.set_message("let文の'='が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'='が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingSemiColon => {
                builder.set_message("';'が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("文は';'で仕切る必要があります")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingRightExpressionInAssign => {
                builder.set_message("挿入文の右の式が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'='のあとには式が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingNameInFunction => {
                builder.set_message("関数の名前が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'function'のあとには名前が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingFunctionArgument => {
                builder.set_message("関数の引数が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("関数の引数がありません")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::InvalidFunctionArgument => {
                builder.set_message("関数の引数の記述形式に誤りがあります");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("この記述または周辺に問題があります")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::UnclosedBrace => {
                builder.set_message("'{'が閉じられていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'}'で閉じられていないか中身の記述に問題があります")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingBlock => {
                builder.set_message("ブロックが記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'{'と'}'で囲われたブロックが必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingClassName => {
                builder.set_message("クラスの名前が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'class'のあとには名前が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingClassField => {
                builder.set_message("クラスのフィールドが記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("フィールドを記述する必要があります")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::InvalidClassField => {
                builder.set_message("クラスのフィールドの記述形式が正しくありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("フィールドは'name: type'の形式です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingPrimary => {
                builder.set_message("let文の名称部分が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("letのあとには名前が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::UnclosedParen => {
                builder.set_message("'('が閉じられていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("')'で閉じる必要があります")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingNewName => {
                builder.set_message("new式の作成対象の型が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("'new'のあとには型名が必要です")
                        .with_color(Color::Red),
                );
            }
            ParseErrorKind::MissingNewFieldAssign => {
                builder.set_message("new式のフィールド挿入が記述されていません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("フィールド挿入が必要です")
                        .with_color(Color::Red),
                );
            }
        }

        builder.finish()
    }
}

impl ToReport for SemanticsError {
    fn to_report(&self, source_name: &str) -> Report<'_, (String, Range<usize>)> {
        let mut builder = Report::build(
            ReportKind::Error,
            (source_name.to_string(), self.span.clone()),
        );

        match self.kind {
            SemanticsErrorKind::InvalidAssignLeft => {
                builder.set_message("挿入文の左辺として正しい形式ではありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("値の挿入対象として正しくありません")
                        .with_color(Color::Red),
                );
            }
        }

        builder.finish()
    }
}

impl ToReport for NameResolveError {
    fn to_report(&self, source_name: &str) -> Report<'_, (String, Range<usize>)> {
        let mut builder = Report::build(
            ReportKind::Error,
            (source_name.to_string(), self.span.clone()),
        );

        match self.kind {
            NameResolveErrorKind::UnknownName => {
                builder.set_message("名前解決に失敗しました");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("同じ名前の定義が見つかりませんでした")
                        .with_color(Color::Red),
                );
            }
        }

        builder.finish()
    }
}

impl ToReport for TypeError {
    fn to_report(&self, source_name: &str) -> Report<'_, (String, Range<usize>)> {
        let mut builder = Report::build(
            ReportKind::Error,
            (source_name.to_string(), self.span.clone()),
        );

        match &self.kind {
            TypeErrorKind::TypeMismatch { left, right } => {
                builder.set_message("型が一致しません");
                builder.add_label(
                    Label::new((source_name.to_string(), left.span.clone()))
                        .with_message(format!(
                            "この型は'{}'です",
                            left.value.to_display_string().fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), right.span.clone()))
                        .with_message(format!(
                            "この型は'{}'です",
                            right.value.to_display_string().fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::InvalidTypeByFunction { function } => {
                builder.set_message("型の指定として正しくありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("型の名前ではなく関数の名前が指定されています")
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), function.clone()))
                        .with_message("これは関数として定義されています")
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::InvalidTypeByVariable { variable } => {
                builder.set_message("型の指定として正しくありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("型の名前ではなく変数の名前が指定されています")
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), variable.clone()))
                        .with_message("これは変数として定義されています")
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::UnexpectedType { expected, found } => {
                builder.set_message(format!(
                    "型'{}'を期待しましたが型'{}'が指定されています",
                    expected.value.to_display_string().fg(Color::Yellow),
                    found.value.to_display_string().fg(Color::Red)
                ));
                builder.add_label(
                    Label::new((source_name.to_string(), expected.span.clone()))
                        .with_message(format!(
                            "型'{}'はこれによって期待されました",
                            expected.value.to_display_string().fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), found.span.clone()))
                        .with_message(format!(
                            "期待された型に反して型'{}'が指定されました",
                            found.value.to_display_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::NotNumericType { expected, found } => {
                builder.set_message(format!(
                    "数値型を期待しましたが型'{}'が指定されています",
                    found.value.to_display_string().fg(Color::Red)
                ));
                builder.add_label(
                    Label::new((source_name.to_string(), expected.clone()))
                        .with_message("数値型はこれによって期待されました")
                        .with_color(Color::Yellow),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), found.span.clone()))
                        .with_message(format!(
                            "期待された型に反して型'{}'が指定されました",
                            found.value.to_display_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::UnknownTypeAtThisPoint => {
                builder.set_message("型が確定していません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("型はこの時点で確定している必要があります")
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::InvalidVariableByUserType { user_type } => {
                builder.set_message("変数の指定として正しくありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("変数の名前ではなく型名が指定されています")
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), user_type.clone()))
                        .with_message("これは型として定義されています")
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::NotFunctionType { found } => {
                builder.set_message("関数型ではありません");
                builder.add_label(
                    Label::new((source_name.to_string(), found.span.clone()))
                        .with_message(format!(
                            "この型'{}'は関数型ではありません",
                            found.value.to_display_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::InvalidArgumentCount { expected, found } => {
                builder.set_message("引数の数が一致しません");
                builder.add_label(
                    Label::new((source_name.to_string(), expected.span.clone()))
                        .with_message(format!(
                            "定義されている引数は'{}'個です",
                            expected.value.to_string().fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), found.span.clone()))
                        .with_message(format!(
                            "指定されている引数は'{}'個です",
                            found.value.to_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::NotClassType { found } => {
                builder.set_message("クラス型ではありません");
                builder.add_label(
                    Label::new((source_name.to_string(), found.span.clone()))
                        .with_message(format!(
                            "クラスではない型'{}'が指定されています",
                            found.value.to_display_string().fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
            }
            TypeErrorKind::NoClassFieldFound { not_found, class } => {
                builder.set_message(format!(
                    "'{}'というフィールドが型'{}'の中に見つかりません",
                    not_found.clone().fg(Color::Red),
                    class.value.to_display_string().fg(Color::Yellow),
                ));
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message(format!(
                            "フィールド'{}'は見つかりません",
                            not_found.clone().fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), class.span.clone()))
                        .with_message(format!(
                            "クラス'{}'の中からは見つかりませんでした",
                            class.value.to_display_string().fg(Color::Red)
                        ))
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::InvalidClassTypeByFunction { function } => {
                builder.set_message("クラス名の指定として正しくありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("クラス名ではなく関数名が指定されています")
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), function.clone()))
                        .with_message("これは関数として定義されています")
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::InvalidClassTypeByVariable { variable } => {
                builder.set_message("変数の指定として正しくありません");
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message("変数の名前ではなく型名が指定されています")
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), variable.clone()))
                        .with_message("これは型として定義されています")
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::UnknownField { field, class } => {
                builder.set_message(format!(
                    "'{}'というフィールドが型'{}'の中に見つかりません",
                    field.value.clone().fg(Color::Red),
                    class.value.to_display_string().fg(Color::Yellow),
                ));
                builder.add_label(
                    Label::new((source_name.to_string(), field.span.clone()))
                        .with_message(format!(
                            "フィールド'{}'は見つかりません",
                            field.value.clone().fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), class.span.clone()))
                        .with_message(format!(
                            "クラス'{}'の中からは見つかりませんでした",
                            class.value.to_display_string().fg(Color::Red)
                        ))
                        .with_color(Color::Yellow),
                );
            }
            TypeErrorKind::MissingField { missing } => {
                builder.set_message(format!(
                    "フィールド'{}'の初期値が指定されていません",
                    missing.value.clone().fg(Color::Yellow),
                ));
                builder.add_label(
                    Label::new((source_name.to_string(), self.span.clone()))
                        .with_message(format!(
                            "フィールド'{}'の値が指定されていません",
                            missing.value.clone().fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
                builder.add_label(
                    Label::new((source_name.to_string(), missing.span.clone()))
                        .with_message(format!(
                            "フィールド'{}'はここで定義されています",
                            missing.value.clone().fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                );
            }
        }

        builder.finish()
    }
}
