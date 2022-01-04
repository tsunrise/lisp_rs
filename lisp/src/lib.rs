mod lex;
pub mod s_exp;
pub mod ast;
pub mod symtab;
pub mod interpret;
// pub mod closure;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
