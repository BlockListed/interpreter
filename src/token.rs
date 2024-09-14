#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    NumLiteral(i64),
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
}
