#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Illegal,
    Eof,
    Ident(String),
    NumLiteral(i64),
    FloatLiteral(f64),
    StrLiteral(String),

    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,

    And,
    Or,
    BitAnd,
    BitOr,

    Add,
    Subtract,
    Divide,
    Multiply,

    Assign,

    Comma,
    Semicolon,

    LParen,
    RParen,

    LSquirly,
    RSquirly,

    Function,
    Let,
    If,
    Else,
}

pub struct Token {
    pub t: TokenKind,
    /// Offset into the source string, where this token begins.
    pub location: usize,
}

impl Token {
    pub fn new(t: TokenKind, loc: usize) -> Self {
        Token {
            t,
            location: loc,
        }
    }
}
