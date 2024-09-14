use std::iter::{Fuse, Peekable};

use crate::token::Token;

pub struct Lexer<I: Iterator<Item = char>> {
    iter: Peekable<Fuse<I>>,
    closed: bool,
}


impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(i: impl IntoIterator<Item = char, IntoIter = I>) -> Lexer<I> {
        Lexer {
            iter: i.into_iter().fuse().peekable(),
            closed: false,
        }
    }

    pub fn new_from_iter(i: I) -> Lexer<I> {
        Lexer {
            iter: i.fuse().peekable(),
            closed: false,
        }
    }

    fn next_matches(&mut self, mut p: impl FnMut(char) -> bool) -> bool {
        match self.iter.peek().copied() {
            Some(c) if p(c) => {
                let _ = self.iter.next();
                true
            },
            _ => false,
        }
    }

    fn next_is(&mut self, c: char) -> bool {
        self.next_matches(|ch| ch == c)
    }

    fn next_non_whitespace(&mut self) -> Option<char> {
        loop {
            match self.iter.next() {
                Some(c) if c.is_whitespace() => continue,
                Some(c) => return Some(c),
                None => return None,
            }
        }
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut full = String::with_capacity(32);
        full.push(first);

        loop {
            match self.iter.peek().copied() {
                Some(c) if is_valid_for_ident(c) => {
                    // throw away the peeked value
                    let _ = self.iter.next();
                    full.push(c);
                }
                _ => return full,
            }
        }
    }

    fn read_number(&mut self, first: char, negative: bool) -> Token {
        assert!(first.is_ascii_digit());
        let mut digits = String::with_capacity(32);
        digits.push(first);

        loop {
            match self.iter.peek() {
                Some(c) if c.is_ascii_digit() => {
                    digits.push(*c);
                }
                _ => break,
            }
        }

        let len = digits.len();

        assert!(len <= u32::MAX as usize);

        let n: i64 = digits.parse().unwrap();

        Token::NumLiteral(if negative { -n } else { n })
    }

    fn read_string(&mut self) -> Token {
        let mut output = String::with_capacity(64);

        loop {
            if let Some(c) = self.iter.next() {
                match c {
                    '\\' => match self.iter.next() {
                        Some(c) => {
                            if let Some(e) = unescape(c) {
                                output.push(e);
                            } else {
                                return Token::Illegal;
                            }
                        }
                        None => return Token::Illegal,
                    },
                    '"' => return Token::StrLiteral(output),
                    c => {
                        output.push(c);
                    }
                }
            } else {
                return Token::Illegal;
            }
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = crate::token::Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(c) = self.next_non_whitespace() else {
            if self.closed {
                return None;
            }

            self.closed = true;
            return Some(Token::Eof);
        };

        Some(match c {
            '>' if self.next_is('=') => Token::GreaterThanOrEqual,
            '<' if self.next_is('=') => Token::LessThanOrEqual,
            '>' => Token::GreaterThan,
            '<' => Token::LessThan,
            '=' if self.next_is('=') => Token::Equal,

            '&' if self.next_is('&') => Token::And,
            '|' if self.next_is('|') => Token::Or,
            '&' => Token::BitAnd,
            '|' => Token::BitOr,

            c if c.is_ascii_digit() => self.read_number(c, false),

            '-' if self.next_matches(|c| c.is_ascii_digit()) => {
                // we're prepending a zero, cause I do be lazy
                self.read_number('0', true)
            }

            '+' => Token::Add,
            '-' => Token::Subtract,
            '/' => Token::Divide,
            '*' => Token::Multiply,

            '=' => Token::Assign,

            ',' => Token::Comma,
            ';' => Token::Semicolon,

            '(' => Token::LParen,
            ')' => Token::RParen,

            '{' => Token::LSquirly,
            '}' => Token::RSquirly,

            '"' => self.read_string(),

            c if is_valid_for_ident(c) => {
                let identifier = self.read_identifier(c);

                match identifier.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "if" => Token::If,
                    _ => Token::Ident(identifier),
                }
            }

            _ => Token::Illegal,
        })
    }
}

fn is_valid_for_ident(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn unescape(c: char) -> Option<char> {
    match c {
        '\\' => Some('\\'),
        'n' => Some('\n'),
        '"' => Some('"'),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;

    use super::Lexer;

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
    fn text_lexing() {
        let lexer: Vec<Token> = Lexer::new_from_iter(TEST_VECTOR.chars()).collect();

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
