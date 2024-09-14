use std::borrow::Cow;

use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, tag, take_while1};
use nom::character::complete::multispace0;
use nom::combinator::{cut, value};
use nom::error::ParseError;
use nom::sequence::{preceded, terminated};
use nom::{IResult, Parser};

use crate::token::{Token, TokenKind};

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

        let loc = self.consumed;

        let (consumed, token) = {
            let raw = multispace0::<_, nom::error::Error<_>>(orig_raw)
                .expect("shouldn't fail")
                .0;

            if raw.is_empty() {
                self.consumed += orig_raw.len() - raw.len();
                self.closed = true;
                // Eof token should be at the end of the file.
                return Some(Token::new(TokenKind::Eof, self.consumed));
            }

            let Ok((raw, token)) = tokenize::<nom::error::Error<_>>(raw) else {
                panic!("big oof");
            };

            let consumed = orig_raw.len() - raw.len();

            (consumed, token)
        };

        self.consumed += consumed;

        Some(Token::new(token, loc))
    }
}

fn tokenize<'a, E: ParseError<&'a str>>(raw: &'a str) -> IResult<&'a str, TokenKind, E> {
    let comparisons = alt((
        value(TokenKind::GreaterThanOrEqual, tag(">=")),
        value(TokenKind::LessThanOrEqual, tag("<=")),
        value(TokenKind::Equal, tag("==")),
        value(TokenKind::GreaterThan, tag(">")),
        value(TokenKind::LessThan, tag("<")),
    ));

    let logic = alt((
        value(TokenKind::And, tag("&&")),
        value(TokenKind::Or, tag("||")),
        value(TokenKind::BitAnd, tag("&")),
        value(TokenKind::BitOr, tag("|")),
    ));

    let numbers = nom::character::complete::i64.map(TokenKind::NumLiteral);
    let floats = nom::number::complete::double.map(TokenKind::FloatLiteral);

    let arithmetic = alt((
        value(TokenKind::Add, tag("+")),
        value(TokenKind::Subtract, tag("-")),
        value(TokenKind::Multiply, tag("*")),
        value(TokenKind::Divide, tag("/")),
    ));

    let assign = value(TokenKind::Assign, tag("="));

    let parens = alt((
        value(TokenKind::LParen, tag("(")),
        value(TokenKind::RParen, tag(")")),
        value(TokenKind::LSquirly, tag("{")),
        value(TokenKind::RSquirly, tag("}")),
    ));

    let punctuation = alt((
        value(TokenKind::Comma, tag(",")),
        value(TokenKind::Semicolon, tag(";")),
    ));

    let ident = identifier.map(|ident| match ident {
        "fn" => TokenKind::Function,
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        i => TokenKind::Ident(i.to_string()),
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
        value(TokenKind::Illegal, take_one),
    ))(raw)
    .expect("we're fucked");

    Ok((raw, token))
}

fn is_valid_for_ident(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn string<'a, E: ParseError<&'a str>>(s: &'a str) -> IResult<&'a str, TokenKind, E> {
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

    Ok((s, TokenKind::StrLiteral(string)))
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
    use crate::token::TokenKind;

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
        let s = r#""Amogin\n\",--y++++**\\Time""#;

        let (_, parsed) = string::<nom::error::VerboseError<_>>(s).unwrap();

        assert_eq!(parsed, TokenKind::StrLiteral("Amogin\n\",--y++++**\\Time".to_string()));
    }

    #[test]
    fn text_lexing() {
        let lexer: Vec<TokenKind> = Lexer::new(TEST_VECTOR).map(|t| t.t).collect();

        assert_eq!(
            lexer.as_slice(),
            &[
                TokenKind::Let,
                TokenKind::Ident("a".to_string()),
                TokenKind::Assign,
                TokenKind::NumLiteral(5),
                TokenKind::Semicolon,
                TokenKind::Let,
                TokenKind::Ident("b".to_string()),
                TokenKind::Assign,
                TokenKind::StrLiteral("Hello,\namogus.".to_string()),
                TokenKind::Semicolon,
                TokenKind::If,
                TokenKind::Ident("a".to_string()),
                TokenKind::GreaterThanOrEqual,
                TokenKind::NumLiteral(5),
                TokenKind::LSquirly,
                TokenKind::Ident("amogus".to_string()),
                TokenKind::LParen,
                TokenKind::Ident("b".to_string()),
                TokenKind::RParen,
                TokenKind::Semicolon,
                TokenKind::RSquirly,
                TokenKind::If,
                TokenKind::Ident("a".to_string()),
                TokenKind::Equal,
                TokenKind::NumLiteral(5),
                TokenKind::LSquirly,
                TokenKind::Ident("amogus".to_string()),
                TokenKind::LParen,
                TokenKind::Ident("a".to_string()),
                TokenKind::RParen,
                TokenKind::Semicolon,
                TokenKind::RSquirly,
                TokenKind::Eof
            ]
        );
    }
}
