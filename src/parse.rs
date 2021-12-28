use logos::{Lexer, Logos};
use crate::lex::RawToken;

#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
    Num(i64),
    Sym(&'a str),
    Lst(Vec<Token<'a>>),
}

pub struct TokenIterator<'a> {
    raw_tokens: Vec<(RawToken, &'a str)>,
    pos: usize
}

impl<'a> TokenIterator<'a> {
    pub fn new(input: &'a str) -> TokenIterator<'a> {
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

    fn parse_lst(&mut self) -> Vec<Token<'a>> {
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

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_raw() {
            None => None,
            Some(tok) => {
                match tok.0 {
                    RawToken::Number => {
                        let num = tok.1.parse::<i64>().unwrap();
                        Some(Token::Num(num))
                    },
                    RawToken::Symbol => {
                        let sym = tok.1;
                        Some(Token::Sym(sym))
                    },
                    RawToken::LParen => {
                        Some(Token::Lst(self.parse_lst()))
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
            Token::Lst(vec![
                Token::Sym("+"),
                Token::Num(1),
                Token::Num(2)
            ]),
            Token::Num(12),
            Token::Lst(vec![
                Token::Sym("+"),
                Token::Num(1),
                Token::Lst(vec![
                    Token::Sym("+"),
                    Token::Num(2),
                    Token::Num(3)
                ])
            ]),
            Token::Lst(vec![
                Token::Sym("+"),
                Token::Num(1),
                Token::Lst(vec![
                    Token::Sym("+"),
                    Token::Num(2),
                    Token::Num(3)
                ]),
                Token::Num(4)
            ]),
        ];

        programs.into_iter().zip(expected.into_iter()).for_each(|(program, expected)| {
            let mut token_iter = TokenIterator::new(program);
            let actual = token_iter.next();
            assert_eq!(actual, Some(expected));
        });
    }
}