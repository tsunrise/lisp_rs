use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
pub enum RawToken {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[regex(r"-?[0-9]+", priority = 2)]
    Number,
    #[regex(r"[a-zA-Z+\-*/<=/>?]+[a-zA-Z+\-*/<=/>?0-9]*", priority = 1)]
    Symbol,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::{Lexer, Logos};
    #[test]
    fn test_lexer() {
        let mut lexer: Lexer<_> = RawToken::lexer("(+ 1 -2)");
        assert_eq!(lexer.next(), Some(RawToken::LParen));
        assert_eq!(lexer.slice(), "(");
        assert_eq!(lexer.next(), Some(RawToken::Symbol));
        assert_eq!(lexer.slice(), "+");
        assert_eq!(lexer.next(), Some(RawToken::Number));
        assert_eq!(lexer.slice(), "1");
        assert_eq!(lexer.next(), Some(RawToken::Number));
        assert_eq!(lexer.slice(), "-2");
        assert_eq!(lexer.next(), Some(RawToken::RParen));
        assert_eq!(lexer.slice(), ")");
    }
}
