use crate::{
    ast::Expr::Closure,
    parse::SExp,
    symtab::{SymGen, Symbol, ToSymbol},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim0 {
    /// read-num
    ReadNum,
    /// newline
    NewLine,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim1 {
    /// add1
    Add1,
    /// sub1
    Sub1,
    /// zero?
    ZeroP,
    /// num?
    NumP,
    /// not
    Not,
    /// left
    Left,
    /// right
    Right,
    /// print
    Print,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim2 {
    /// +
    Plus,
    /// -
    Minus,
    /// =
    Eq,
    /// <
    Lt,
    /// pair
    Pair,
}

pub fn prim0_of_str(s: &str) -> Option<Prim0> {
    match s {
        "read-num" => Some(Prim0::ReadNum),
        "newline" => Some(Prim0::NewLine),
        _ => None,
    }
}

pub fn prim1_of_str(s: &str) -> Option<Prim1> {
    match s {
        "add1" => Some(Prim1::Add1),
        "sub1" => Some(Prim1::Sub1),
        "zero?" => Some(Prim1::ZeroP),
        "num?" => Some(Prim1::NumP),
        "not" => Some(Prim1::Not),
        "left" => Some(Prim1::Left),
        "right" => Some(Prim1::Right),
        "print" => Some(Prim1::Print),
        _ => None,
    }
}

pub fn prim2_of_str(s: &str) -> Option<Prim2> {
    match s {
        "+" => Some(Prim2::Plus),
        "-" => Some(Prim2::Minus),
        "=" => Some(Prim2::Eq),
        "<" => Some(Prim2::Lt),
        "pair" => Some(Prim2::Pair),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Prim0(Prim0),
    Prim1(Prim1, Box<Expr<'a>>),
    Prim2(Prim2, Box<Expr<'a>>, Box<Expr<'a>>),
    Let(Symbol<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    If(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    Do(Vec<Expr<'a>>),
    Num(i64),
    Var(Symbol<'a>),
    Closure(Symbol<'a>),
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// An intermediate AST before lambda desugaring.
pub enum ExprLambda<'a> {
    Prim0(Prim0),
    Prim1(Prim1, Box<ExprLambda<'a>>),
    Prim2(Prim2, Box<ExprLambda<'a>>, Box<ExprLambda<'a>>),
    Let(Symbol<'a>, Box<ExprLambda<'a>>, Box<ExprLambda<'a>>),
    If(
        Box<ExprLambda<'a>>,
        Box<ExprLambda<'a>>,
        Box<ExprLambda<'a>>,
    ),
    Do(Vec<ExprLambda<'a>>),
    Num(i64),
    Var(Symbol<'a>),
    /// (f x)
    Call(Box<ExprLambda<'a>>, Vec<ExprLambda<'a>>),
    Bool(bool),
    /// (lambda (arg1 arg2 ...) body)
    Lambda(Vec<Symbol<'a>>, Box<ExprLambda<'a>>),
}

/// definition of a function
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<'a> {
    pub name: Symbol<'a>,
    pub args: Vec<Symbol<'a>>,
    pub body: Expr<'a>,
}

impl<'a> Definition<'a> {
    pub fn new(name: Symbol<'a>, args: Vec<Symbol<'a>>, body: Expr<'a>) -> Self {
        Definition { name, args, body }
    }
}

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub defs: Vec<Definition<'a>>,
    pub body: Expr<'a>,
}

impl<'a> SExp<'a> {
    pub fn to_expr_lam(&self) -> ExprLambda<'a> {
        use ExprLambda::*;
        use SExp::{Lst, Num, Sym};
        match self {
            Num(x) => ExprLambda::Num(*x),
            Sym("true") => Bool(true),
            Sym("false") => Bool(false),
            Sym(var) => Var(var.to_symbol()),
            Lst(lst) => {
                match lst.as_slice() {
                    // (let ((var expr)) body)
                    [Sym("let"), Lst(name), body] => match name.as_slice() {

                        [Sym(var_name), exp] => Let(
                            var_name.to_symbol(),
                            Box::new(exp.to_expr_lam()),
                            Box::new(body.to_expr_lam()),
                        ),
                        bad => panic!("invalid `let` pattern: {:?}", bad),
                    },
                    // (do exprs ...)
                    [Sym("do"), exps @ ..] if exps.len() > 0 => {
                        Do(exps.iter().map(|e| e.to_expr_lam()).collect())
                    },
                    // (if cond then else)
                    [Sym("if"), test_s, then_s, else_s] => If(
                        Box::new(test_s.to_expr_lam()),
                        Box::new(then_s.to_expr_lam()),
                        Box::new(else_s.to_expr_lam()),
                    ),
                    // (lambda (args ...) body)
                    [Sym("lambda"), Lst(args), body] if args.iter().all(|x| x.is_sym()) => Lambda(
                        args.iter().map(|x| x.as_sym().unwrap()).collect(),
                        Box::new(body.to_expr_lam()),
                    ),
                    // (prim0)
                    [Sym(prim)] if prim0_of_str(prim).is_some() => {
                        Prim0(prim0_of_str(prim).unwrap())
                    },
                    // (prim1 arg)
                    [Sym(prim), arg] if prim1_of_str(prim).is_some() => {
                        Prim1(prim1_of_str(prim).unwrap(), Box::new(arg.to_expr_lam()))
                    },
                    // (prim2 arg1 arg2)
                    [Sym(prim), arg1, arg2] if prim2_of_str(prim).is_some() => Prim2(
                        prim2_of_str(prim).unwrap(),
                        Box::new(arg1.to_expr_lam()),
                        Box::new(arg2.to_expr_lam()),
                    ),
                    // (f args ...)
                    [f, args @ ..] => Call(
                        Box::new(f.to_expr_lam()),
                        args.iter().map(|x| x.to_expr_lam()).collect(),
                    ),
                    _ => panic!("cannot parse s-exp to defined expression: {:?}", self),
                }
            },
        }
    }
}

impl<'a> ExprLambda<'a> {
    pub fn to_expr(&self, defns: &mut Vec<Definition<'a>>, sym: &mut SymGen) -> Expr<'a> {
        use ExprLambda::*;
        match self {
            Num(x) => Expr::Num(*x),
            Var(x) => Expr::Var(*x),
            Bool(b) => Expr::Bool(*b),
            If(cond, then, else_) => Expr::If(
                Box::new(cond.to_expr(defns, sym)),
                Box::new(then.to_expr(defns, sym)),
                Box::new(else_.to_expr(defns, sym)),
            ),
            Let(var, expr, body) => Expr::Let(
                *var,
                Box::new(expr.to_expr(defns, sym)),
                Box::new(body.to_expr(defns, sym)),
            ),
            Prim0(p) => Expr::Prim0(*p),
            Prim1(p, arg) => Expr::Prim1(*p, Box::new(arg.to_expr(defns, sym))),
            Prim2(p, arg1, arg2) => Expr::Prim2(
                *p,
                Box::new(arg1.to_expr(defns, sym)),
                Box::new(arg2.to_expr(defns, sym)),
            ),
            Do(exps) => Expr::Do(exps.iter().map(|e| e.to_expr(defns, sym)).collect()),
            Call(f, args) => Expr::Call(
                Box::new(f.to_expr(defns, sym)),
                args.iter().map(|x| x.to_expr(defns, sym)).collect(),
            ),
            Lambda(args, body) => {
                let name = sym.gensym("_lambda");
                let body = body.to_expr(defns, sym);
                defns.push(Definition::new(name, args.to_vec(), body));
                Closure(name)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::SExpIterator;

    #[test]
    fn test_parse_expr_lam() {
        let test_cases = [
            (
                "(+ 1 2)",
                ExprLambda::Prim2(
                    Prim2::Plus,
                    ExprLambda::Num(1).into(),
                    ExprLambda::Num(2).into(),
                ),
            ),
            (
                "(+ 1 (+ 2 3))",
                ExprLambda::Prim2(
                    Prim2::Plus,
                    ExprLambda::Num(1).into(),
                    ExprLambda::Prim2(
                        Prim2::Plus,
                        ExprLambda::Num(2).into(),
                        ExprLambda::Num(3).into(),
                    )
                    .into(),
                ),
            ),
            (
                "(lambda (x y) (+ x y))",
                ExprLambda::Lambda(
                    vec!["x".to_symbol(), "y".to_symbol()],
                    ExprLambda::Prim2(
                        Prim2::Plus,
                        ExprLambda::Var("x".to_symbol()).into(),
                        ExprLambda::Var("y".to_symbol()).into(),
                    )
                    .into(),
                ),
            ),
            (
                "(print (+ ((lambda (x) (+ x 1)) 2) 3))",
                ExprLambda::Prim1(
                    Prim1::Print,
                    ExprLambda::Prim2(
                        Prim2::Plus,
                        ExprLambda::Call(
                            ExprLambda::Lambda(
                                vec!["x".to_symbol()],
                                ExprLambda::Prim2(
                                    Prim2::Plus,
                                    ExprLambda::Var("x".to_symbol()).into(),
                                    ExprLambda::Num(1).into(),
                                )
                                .into(),
                            )
                            .into(),
                            vec![ExprLambda::Num(2)],
                        )
                        .into(),
                        ExprLambda::Num(3).into(),
                    )
                    .into(),
                ),
            ),
        ];

        for (s, expected) in test_cases {
            let parsed = SExpIterator::new(s).next().unwrap();
            let parsed = parsed.to_expr_lam();
            assert_eq!(parsed, expected);
        }
    }

    #[test]
    fn test_parse_expr() {
        let test_cases = [
            (
                "(+ 1 2)",
                (
                    Expr::Prim2(Prim2::Plus, Expr::Num(1).into(), Expr::Num(2).into()),
                    vec![],
                ),
            ),
            (
                "(+ 1 (+ 2 3))",
                (
                    Expr::Prim2(
                        Prim2::Plus,
                        Expr::Num(1).into(),
                        Expr::Prim2(Prim2::Plus, Expr::Num(2).into(), Expr::Num(3).into()).into(),
                    ),
                    vec![],
                ),
            ),
            ("(let (x (lambda (x y) (+ x y))) (x 1 2))", {
                let mut sym = SymGen::new();
                let lmbda_name = sym.gensym("_lambda");
                (
                    Expr::Let(
                        "x".to_symbol(),
                        Closure(lmbda_name).into(),
                        Expr::Call(
                            Expr::Var("x".to_symbol()).into(),
                            vec![Expr::Num(1), Expr::Num(2)],
                        )
                        .into(),
                    ),
                    vec![Definition::new(
                        lmbda_name,
                        vec!["x".to_symbol(), "y".to_symbol()],
                        Expr::Prim2(
                            Prim2::Plus,
                            Expr::Var("x".to_symbol()).into(),
                            Expr::Var("y".to_symbol()).into(),
                        )
                        .into(),
                    )],
                )
            }),
            ("(print (+ ((lambda (x) (+ x 1)) 2) 3))", {
                let mut sym = SymGen::new();
                let lmbda_name = sym.gensym("_lambda");
                (
                    Expr::Prim1(
                        Prim1::Print,
                        Expr::Prim2(
                            Prim2::Plus,
                            Expr::Call(Expr::Closure(lmbda_name).into(), vec![Expr::Num(2)]).into(),
                            Expr::Num(3).into(),
                        )
                        .into(),
                    ),
                    vec![Definition::new(
                        lmbda_name,
                        vec!["x".to_symbol()],
                        Expr::Prim2(
                            Prim2::Plus,
                            Expr::Var("x".to_symbol()).into(),
                            Expr::Num(1).into(),
                        )
                        .into(),
                    )],
                )
            }),
        ];

        for (s, (expected_body, expected_definitions)) in test_cases {
            let parsed = SExpIterator::new(s).next().unwrap();
            let mut defns = Vec::new();
            let mut sym = SymGen::new();
            let parsed = parsed.to_expr_lam().to_expr(&mut defns, &mut sym);
            assert_eq!(parsed, expected_body);
            assert_eq!(defns, expected_definitions);
        }
    }
}
