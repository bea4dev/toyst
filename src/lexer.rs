use std::ops::Range;

use extension_fn::extension_fn;
use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// class
    Class,
    /// or
    Or,
    /// and
    And,
    /// let
    Let,
    /// function
    Function,
    /// new
    New,
    /// if
    If,
    /// else
    Else,
    /// =
    Equal,
    /// ==
    DoubleEqual,
    /// !=
    NotEqual,
    /// return
    Return,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// ,
    Comma,
    /// :
    Colon,
    /// ;
    SemiColon,
    /// .
    Dot,
    /// ->
    ThinArrow,
    /// <
    LessThan,
    /// >
    GreaterThan,
    /// <=
    LessThanOrEqual,
    /// >=
    GreaterThanOrEqual,
    /// (
    ParenthesesLeft,
    /// )
    ParenthesesRight,
    /// {
    BraceLeft,
    /// }
    BraceRight,
    /// e.g. normal_literal
    Literal,
    /// e.g. "string literal"
    StringLiteral,
    /// e.g. 100
    NumericLiteral,
    /// e.g. ' '
    Whitespace,
    /// e.g. // comment
    Comment,
    UnexpectedCharacter,
    None,
}

static TOKENIZERS: &[Tokenizer] = &[
    Tokenizer::Keyword(TokenKind::Class, "class"),
    Tokenizer::Keyword(TokenKind::Or, "or"),
    Tokenizer::Keyword(TokenKind::And, "and"),
    Tokenizer::Keyword(TokenKind::Let, "let"),
    Tokenizer::Keyword(TokenKind::Function, "function"),
    Tokenizer::Keyword(TokenKind::New, "new"),
    Tokenizer::Keyword(TokenKind::If, "if"),
    Tokenizer::Keyword(TokenKind::Else, "else"),
    Tokenizer::Keyword(TokenKind::Equal, "="),
    Tokenizer::Keyword(TokenKind::DoubleEqual, "=="),
    Tokenizer::Keyword(TokenKind::NotEqual, "!="),
    Tokenizer::Keyword(TokenKind::Return, "return"),
    Tokenizer::Keyword(TokenKind::Plus, "+"),
    Tokenizer::Keyword(TokenKind::Minus, "-"),
    Tokenizer::Keyword(TokenKind::Asterisk, "*"),
    Tokenizer::Keyword(TokenKind::Slash, "/"),
    Tokenizer::Keyword(TokenKind::Comma, ","),
    Tokenizer::Keyword(TokenKind::Colon, ":"),
    Tokenizer::Keyword(TokenKind::SemiColon, ";"),
    Tokenizer::Keyword(TokenKind::Dot, "."),
    Tokenizer::Keyword(TokenKind::ThinArrow, "->"),
    Tokenizer::Keyword(TokenKind::LessThan, "<"),
    Tokenizer::Keyword(TokenKind::GreaterThan, ">"),
    Tokenizer::Keyword(TokenKind::LessThanOrEqual, "<="),
    Tokenizer::Keyword(TokenKind::GreaterThanOrEqual, ">="),
    Tokenizer::Keyword(TokenKind::ParenthesesLeft, "("),
    Tokenizer::Keyword(TokenKind::ParenthesesRight, ")"),
    Tokenizer::Keyword(TokenKind::BraceLeft, "{"),
    Tokenizer::Keyword(TokenKind::BraceRight, "}"),
    Tokenizer::Regex(TokenKind::NumericLiteral, r"\d+(\.\d+)?"),
    Tokenizer::Regex(TokenKind::Literal, r"\w+"),
    Tokenizer::Regex(TokenKind::StringLiteral, r#"".*""#),
    Tokenizer::Regex(TokenKind::Whitespace, r"[ 　\t\n\r]+"),
    Tokenizer::Regex(TokenKind::Comment, r"//[^\n\r]*"),
    Tokenizer::Regex(TokenKind::Comment, r"/\*.*\*/"),
];

enum Tokenizer {
    Keyword(TokenKind, &'static str),
    Regex(TokenKind, &'static str),
}

impl Tokenizer {
    fn tokenize(
        &self,
        current_input: &str,
        index: usize,
        regex_cache: &mut [Option<Regex>],
    ) -> (TokenKind, usize) {
        return match self {
            Tokenizer::Keyword(kind, keyword) => {
                let mut input_chars = current_input.chars();
                let mut keyword_chars = keyword.chars();
                let mut current_byte_length = 0;
                loop {
                    let keyword_char = match keyword_chars.next() {
                        Some(c) => c,
                        _ => break,
                    };
                    let current_char = match input_chars.next() {
                        Some(c) => c,
                        _ => return (kind.clone(), 0), // reject
                    };
                    if current_char != keyword_char {
                        return (kind.clone(), 0); // reject
                    }

                    current_byte_length += current_char.len_utf8();
                }
                (kind.clone(), current_byte_length) // accept
            }
            Tokenizer::Regex(kind, regex) => {
                let regex = (&mut regex_cache[index])
                    .get_or_insert_with(|| Regex::new(format!("^({})", regex).as_str()).unwrap());

                let length = match regex.find(current_input) {
                    Some(matched) => matched.end(),
                    None => 0,
                };

                (kind.clone(), length)
            }
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token<'input> {
    pub kind: TokenKind,
    pub text: &'input str,
    pub span: Range<usize>,
}

#[extension_fn(Option<Token<'_>>)]
pub fn get_kind(&self) -> TokenKind {
    self.as_ref()
        .map(|token| token.kind)
        .unwrap_or(TokenKind::None)
}

pub struct Lexer<'input> {
    source: &'input str,
    current_byte_position: usize,
    regex_cache: Box<[Option<Regex>]>,
    current_token_cache: Option<Token<'input>>,
    pub comments: Vec<Range<usize>>,
    pub ignore_whitespace: bool,
    pub ignore_comment: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            source,
            current_byte_position: 0,
            regex_cache: vec![None; TOKENIZERS.len()].into_boxed_slice(),
            current_token_cache: None,
            comments: Vec::new(),
            ignore_whitespace: true,
            ignore_comment: true,
        }
    }

    pub fn current(&mut self) -> Option<Token<'input>> {
        let anchor = self.cast_anchor();

        // move to next temporarily
        self.current_token_cache = self.next();

        // back to anchor position
        self.current_byte_position = anchor.byte_position;

        self.current_token_cache.clone()
    }

    pub fn cast_anchor(&self) -> Anchor {
        Anchor {
            byte_position: self.current_byte_position,
        }
    }

    pub fn back_to_anchor(&mut self, anchor: Anchor) {
        self.current_byte_position = anchor.byte_position;
        self.current_token_cache = None;
    }

    pub fn enable_comment_token(mut self) -> Self {
        self.ignore_comment = false;
        self
    }

    pub fn current_span(&mut self) -> Range<usize> {
        match self.current() {
            Some(token) => token.span,
            None => self.source.len()..self.source.len(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        // take cache
        if let Some(token) = self.current_token_cache.take() {
            self.current_byte_position = token.span.end;
            return Some(token);
        }

        loop {
            if self.current_byte_position == self.source.len() {
                return None;
            }

            let current_input = &self.source[self.current_byte_position..self.source.len()];

            let mut current_max_length = 0;
            let mut current_token_kind = TokenKind::Whitespace;

            for (index, tokenizer) in TOKENIZERS.iter().enumerate() {
                let result = tokenizer.tokenize(current_input, index, &mut self.regex_cache);
                let token_kind = result.0;
                let byte_length = result.1;

                if byte_length > current_max_length {
                    current_max_length = byte_length;
                    current_token_kind = token_kind;
                }
            }

            let start_position = self.current_byte_position;

            let token = if current_max_length == 0 {
                let char_length = self.source[start_position..]
                    .chars()
                    .next()
                    .unwrap()
                    .len_utf8();

                self.current_byte_position += char_length;
                let end_position = start_position + char_length;

                Token {
                    kind: TokenKind::UnexpectedCharacter,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            } else {
                self.current_byte_position += current_max_length;

                if current_token_kind == TokenKind::Whitespace && self.ignore_whitespace {
                    continue;
                }

                if current_token_kind == TokenKind::Comment && self.ignore_comment {
                    self.comments
                        .push(start_position..self.current_byte_position);
                    continue;
                }

                let end_position = self.current_byte_position;

                Token {
                    kind: current_token_kind,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            };

            return Some(token);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Anchor {
    byte_position: usize,
}

impl Anchor {
    pub fn elapsed(&self, lexer: &Lexer) -> Range<usize> {
        // skip until not whitespace
        let floor = lexer.source[self.byte_position..]
            .chars()
            .take_while(|char| char.is_whitespace())
            .map(|char| char.len_utf8())
            .sum::<usize>();

        let start = self.byte_position + floor;
        let end = lexer.current_byte_position.max(start);

        start..end
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;

    #[test]
    fn lexer() {
        let source = "
class Test<T> {
    field: T
}

let object = new Test { field: 100 }
";

        let lexer = Lexer::new(source);

        for token in lexer {
            println!("{:?}: {}", token.kind, token.text.replace("\n", "\\n"));
        }
    }
}
