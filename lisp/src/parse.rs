use logos::{Lexer, Logos};
use crate::lex::RawToken;
use serde_derive::Serialize;

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum SExp<'a> {
    Num(i64),
    Sym(&'a str),
    Lst(Vec<SExp<'a>>),
}

impl<'a> SExp<'a> {
    pub fn is_num(&self) -> bool {
        if let SExp::Num(_) = self {
            true
        } else {
            false
        }
    }
    
    pub fn is_sym(&self) -> bool {
        if let SExp::Sym(_) = self {
            true
        } else {
            false
        }
    }
    
    pub fn as_sym(&self) -> Option<&str> {
        if let SExp::Sym(s) = self {
            Some(s)
        } else {
            None
        }
    }
    
    pub fn is_lst(&self) -> bool {
        if let SExp::Lst(_) = self {
            true
        } else {
            false
        }
    }
}

pub struct SExpIterator<'a> {
    raw_tokens: Vec<(RawToken, &'a str)>,
    pos: usize
}

impl<'a> SExpIterator<'a> {
    pub fn new(input: &'a str) -> SExpIterator<'a> {
        let mut lexer: Lexer<_> = RawToken::lexer(input);
        let mut raw_tokens = Vec::new();
        while let Some(token) = lexer.next() {
            let raw_token = token;
            let src = lexer.slice();
            raw_tokens.push((raw_token, src));
        };
        Self{raw_tokens, pos: 0}
    }

    fn peek_next_raw(&self) -> Option<(RawToken, &'a str)> {
        if self.pos < self.raw_tokens.len() {
            Some(self.raw_tokens[self.pos])
        } else {
            None
        }
    }

    fn next_raw(&mut self) -> Option<(RawToken, &'a str)> {
        if self.pos < self.raw_tokens.len() {
            let token = self.raw_tokens[self.pos];
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn parse_lst(&mut self) -> Vec<SExp<'a>> {
        let mut tokens = Vec::new();
        loop {
            match self.peek_next_raw() {
                Some((RawToken::RParen, _)) => {
                    self.next_raw();
                    break;
                },
                _ => {
                    if let Some(token) = self.next() {
                        tokens.push(token);
                    }else{
                        panic!("unexpected end of input");
                    }
                }
            }
        };
        tokens
    }
}

impl<'a> Iterator for SExpIterator<'a> {
    type Item = SExp<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_raw() {
            None => None,
            Some(tok) => {
                match tok.0 {
                    RawToken::Number => {
                        let num = tok.1.parse::<i64>().unwrap();
                        Some(SExp::Num(num))
                    },
                    RawToken::Symbol => {
                        let sym = tok.1;
                        Some(SExp::Sym(sym))
                    },
                    RawToken::LParen => {
                        Some(SExp::Lst(self.parse_lst()))
                    },
                    RawToken::RParen => {
                        panic!("Unexpected ')'")
                    },
                    RawToken::Error => {
                        panic!("Lexer Error: unexpected character")
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_parse() {
        let programs = [
            "(+ 1 2)",
            "12",
            "(+ 1 (+ 2 3))",
            "(+ 1 (+ 2 3)  4)",
        ];
        let expected = [
            SExp::Lst(vec![
                SExp::Sym("+"),
                SExp::Num(1),
                SExp::Num(2)
            ]),
            SExp::Num(12),
            SExp::Lst(vec![
                SExp::Sym("+"),
                SExp::Num(1),
                SExp::Lst(vec![
                    SExp::Sym("+"),
                    SExp::Num(2),
                    SExp::Num(3)
                ])
            ]),
            SExp::Lst(vec![
                SExp::Sym("+"),
                SExp::Num(1),
                SExp::Lst(vec![
                    SExp::Sym("+"),
                    SExp::Num(2),
                    SExp::Num(3)
                ]),
                SExp::Num(4)
            ]),
        ];

        programs.into_iter().zip(expected.into_iter()).for_each(|(program, expected)| {
            let mut token_iter = SExpIterator::new(program);
            let actual = token_iter.next();
            assert_eq!(actual, Some(expected));
        });
    }
}