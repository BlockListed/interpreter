use std::borrow::Cow;

use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, tag, take_while1};
use nom::character::complete::multispace0;
use nom::combinator::{cut, value};
use nom::error::ParseError;
use nom::sequence::{preceded, terminated};
use nom::{IResult, Parser};

use crate::token::Token;

pub struct Lexer<'a> {
    raw: Cow<'a, str>,
    closed: bool,
    consumed: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(r: impl Into<Cow<'a, str>>) -> Lexer<'a> {
        Lexer {
            raw: r.into(),
            closed: false,
            consumed: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = crate::token::Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.closed {
            return None;
        }

        let orig_raw = &self.raw.as_ref()[self.consumed..];

        let (consumed, token) = {
            let raw = multispace0::<_, nom::error::Error<_>>(orig_raw)
                .expect("shouldn't fail")
                .0;

            if raw.is_empty() {
                self.consumed += orig_raw.len() - raw.len();
                self.closed = true;
                return Some(Token::Eof);
            }

            let Ok((raw, token)) = tokenize::<nom::error::Error<_>>(raw) else {
                panic!("big oof");
            };

            let consumed = orig_raw.len() - raw.len();

            (consumed, token)
        };

        self.consumed += consumed;

        Some(token)
    }
}

fn tokenize<'a, E: ParseError<&'a str>>(raw: &'a str) -> IResult<&'a str, Token, E> {
    let comparisons = alt((
        value(Token::GreaterThanOrEqual, tag(">=")),
        value(Token::LessThanOrEqual, tag("<=")),
        value(Token::Equal, tag("==")),
        value(Token::GreaterThan, tag(">")),
        value(Token::LessThan, tag("<")),
    ));

    let logic = alt((
        value(Token::And, tag("&&")),
        value(Token::Or, tag("||")),
        value(Token::BitAnd, tag("&")),
        value(Token::BitOr, tag("|")),
    ));

    let numbers = nom::character::complete::i64.map(Token::NumLiteral);
    let floats = nom::number::complete::double.map(Token::FloatLiteral);

    let arithmetic = alt((
        value(Token::Add, tag("+")),
        value(Token::Subtract, tag("-")),
        value(Token::Multiply, tag("*")),
        value(Token::Divide, tag("/")),
    ));

    let assign = value(Token::Assign, tag("="));

    let parens = alt((
        value(Token::LParen, tag("(")),
        value(Token::RParen, tag(")")),
        value(Token::LSquirly, tag("{")),
        value(Token::RSquirly, tag("}")),
    ));

    let punctuation = alt((
        value(Token::Comma, tag(",")),
        value(Token::Semicolon, tag(";")),
    ));

    let ident = identifier.map(|ident| match ident {
        "fn" => Token::Function,
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        i => Token::Ident(i.to_string()),
    });

    let (raw, token) = alt::<_, _, nom::error::Error<_>, _>((
        comparisons,
        logic,
        numbers,
        floats,
        arithmetic,
        assign,
        parens,
        punctuation,
        ident,
        string,
        value(Token::Illegal, take_one),
    ))(raw)
    .expect("we're fucked");

    Ok((raw, token))
}

fn is_valid_for_ident(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn string<'a, E: ParseError<&'a str>>(s: &'a str) -> IResult<&'a str, Token, E> {
    let string_parser = escaped_transform(
        take_while1(|c| c != '\\' && c != '"'),
        '\\',
        alt((
            value("\\", tag("\\")),
            value("\"", tag("\"")),
            value("\n", tag("n")),
            value("\r", tag("r")),
            value("\t", tag("t")),
            value("\0", tag("0")),
        )),
    );

    let (s, string) = preceded(tag("\""), cut(terminated(string_parser, tag("\""))))(s)?;

    Ok((s, Token::StrLiteral(string)))
}

fn identifier<'a, E: ParseError<&'a str>>(s: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while1(is_valid_for_ident)(s)
}

fn take_one<'a, E: ParseError<&'a str>>(s: &'a str) -> IResult<&'a str, &'a str, E> {
    if s.is_empty() {
        return Err(nom::Err::Error(E::from_error_kind(
            s,
            nom::error::ErrorKind::Eof,
        )));
    }

    Ok((&s[1..], &s[0..1]))
}

#[cfg(test)]
mod test {
    use crate::token::Token;

    use super::{string, Lexer};

    const TEST_VECTOR: &str = r#"
        let a = 5;
        let b = "Hello,\namogus.";
        if a >= 5 {
            amogus(b);
        }

        if a == 5 {
            amogus(a);
        }
        "#;

    #[test]
    fn lex_string() {
        let s = r#""Amogin\n,--y++++**\\Time""#;

        let (_, parsed) = string::<nom::error::VerboseError<_>>(s).unwrap();

        assert!(matches!(parsed, Token::StrLiteral(_)))
    }

    #[test]
    fn text_lexing() {
        let lexer: Vec<Token> = Lexer::new(TEST_VECTOR).collect();

        assert_eq!(
            lexer.as_slice(),
            &[
                Token::Let,
                Token::Ident("a".to_string()),
                Token::Assign,
                Token::NumLiteral(5),
                Token::Semicolon,
                Token::Let,
                Token::Ident("b".to_string()),
                Token::Assign,
                Token::StrLiteral("Hello,\namogus.".to_string()),
                Token::Semicolon,
                Token::If,
                Token::Ident("a".to_string()),
                Token::GreaterThanOrEqual,
                Token::NumLiteral(5),
                Token::LSquirly,
                Token::Ident("amogus".to_string()),
                Token::LParen,
                Token::Ident("b".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::RSquirly,
                Token::If,
                Token::Ident("a".to_string()),
                Token::Equal,
                Token::NumLiteral(5),
                Token::LSquirly,
                Token::Ident("amogus".to_string()),
                Token::LParen,
                Token::Ident("a".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::RSquirly,
                Token::Eof
            ]
        );
    }
}
