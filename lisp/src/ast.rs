use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use crate::{
    ast::Expr::Closure,
    s_exp::SExp,
    symtab::{SymGen, Symbol, ToSymbol},
};
use derive_more::{Display};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Prim0 {
    /// read-num
    #[display(fmt = "read-num")]
    ReadNum,
    /// newline
    #[display(fmt = "newline")]
    NewLine,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Prim1 {
    /// add1
    #[display(fmt = "add1")]
    Add1,
    /// sub1
    #[display(fmt = "sub1")]
    Sub1,
    /// zero?
    #[display(fmt = "zero?")]
    ZeroP,
    /// num?
    #[display(fmt = "num?")]
    NumP,
    /// not
    #[display(fmt = "not")]
    Not,
    /// left
    #[display(fmt = "left")]
    Left,
    /// right
    #[display(fmt = "right")]
    Right,
    /// print
    #[display(fmt = "print")]
    Print,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Prim2 {
    /// +
    #[display(fmt = "+")]
    Plus,
    /// -
    #[display(fmt = "-")]
    Minus,
    /// *
    #[display(fmt = "*")]
    Times,
    /// /
    #[display(fmt = "/")]
    Divide,
    /// =
    #[display(fmt = "=")]
    Eq,
    /// <
    #[display(fmt = "<")]
    Lt,
    /// pair
    #[display(fmt = "pair")]
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
        "*" => Some(Prim2::Times),
        "/" => Some(Prim2::Divide),
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
    Call(Symbol<'a>, Vec<Expr<'a>>),
    DynCall(Box<Expr<'a>>, Vec<Expr<'a>>),
    Bool(bool),
}

impl<'a> Display for Expr<'a>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prim0(p) => write!(f, "({})", *p),
            Self::Prim1(p, a) => write!(f, "({} {})", *p, a),
            Self::Prim2(p ,a, b) => write!(f, "({} {} {})", *p, a, b),
            Self::Let(x, a, b) => write!(f, "(let ({} {}) {})", x, a, b),
            Self::If(a, b, c) => write!(f, "(if {} {} {})", a, b, c),
            Self::Do(es) => {
                write!(f, "(do")?;
                for e in es {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")
            },
            Self::Num(n) => write!(f, "{}", n),
            Self::Var(x) => write!(f, "{}", x),
            Self::Closure(x) => write!(f, "closure#{}", x),
            Self::Call(x, es) => {
                write!(f, "({}", x)?;
                for e in es {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")?;
                Ok(())
            },
            Self::DynCall(a, es) => {
                write!(f, "([dyn]{}", a)?;
                for e in es {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")?;
                Ok(())
            },
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
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
    /// (f x) where f is statically determined
    Call(Symbol<'a>, Vec<ExprLambda<'a>>),
    /// (f x) where f is a pointer dynamically determined
    DynCall(Box<ExprLambda<'a>>, Vec<ExprLambda<'a>>),
    Bool(bool),
    /// (lambda (arg1 arg2 ...) body)
    Lambda(Vec<Symbol<'a>>, Box<ExprLambda<'a>>),
}

/// definition of a function with no body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prototype<'a> {
    pub name: Symbol<'a>,
    pub args: Vec<Symbol<'a>>,
}

impl<'a> Prototype<'a> {
    pub fn new(name: Symbol<'a>, args: Vec<Symbol<'a>>) -> Self {
        Prototype { name, args }
    }
}

/// definition of a function
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<'a> {
    pub proto: Prototype<'a>,
    pub body: Expr<'a>,
}

impl<'a> Definition<'a> {
    pub fn new(name: Symbol<'a>, args: Vec<Symbol<'a>>, body: Expr<'a>) -> Self {
        Definition {
            proto: Prototype::new(name, args),
            body,
        }
    }
    pub fn name(&self) -> &Symbol<'a> {
        &self.proto.name
    }
    pub fn args(&self) -> &Vec<Symbol<'a>> {
        &self.proto.args
    }
}

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub defs: BTreeMap<Symbol<'a>, Definition<'a>>,
    pub body: Expr<'a>,
}

impl<'a> SExp<'a> {
    pub fn to_expr_lam(&self, available_defns: &[&'a str]) -> ExprLambda<'a> {
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
                        [Sym("define"), ..] => panic!("is a definition: {:?}", self),
                        [Sym(var_name), exp] => Let(
                            var_name.to_symbol(),
                            Box::new(exp.to_expr_lam(available_defns)),
                            Box::new(body.to_expr_lam(available_defns)),
                        ),
                        _ => panic!("invalid `let` pattern: {}", self),
                    },
                    // (do exprs ...)
                    [Sym("do"), exps @ ..] if exps.len() > 0 => Do(exps
                        .iter()
                        .map(|e| e.to_expr_lam(available_defns))
                        .collect()),
                    // (if cond then else)
                    [Sym("if"), test_s, then_s, else_s] => If(
                        Box::new(test_s.to_expr_lam(available_defns)),
                        Box::new(then_s.to_expr_lam(available_defns)),
                        Box::new(else_s.to_expr_lam(available_defns)),
                    ),
                    // (lambda (args ...) body)
                    [Sym("lambda"), Lst(args), body] if args.iter().all(|x| x.is_sym()) => Lambda(
                        args.iter().map(|x| x.as_sym().unwrap()).collect(),
                        Box::new(body.to_expr_lam(available_defns)),
                    ),
                    // (prim0)
                    [Sym(prim)] if prim0_of_str(prim).is_some() => {
                        Prim0(prim0_of_str(prim).unwrap())
                    },
                    // (prim1 arg)
                    [Sym(prim), arg] if prim1_of_str(prim).is_some() => Prim1(
                        prim1_of_str(prim).unwrap(),
                        Box::new(arg.to_expr_lam(available_defns)),
                    ),
                    // (prim2 arg1 arg2)
                    [Sym(prim), arg1, arg2] if prim2_of_str(prim).is_some() => Prim2(
                        prim2_of_str(prim).unwrap(),
                        Box::new(arg1.to_expr_lam(available_defns)),
                        Box::new(arg2.to_expr_lam(available_defns)),
                    ),
                    // (f args ...)
                    [Sym(s), args @ ..] if available_defns.contains(s) => Call(
                        s.to_symbol(),
                        args.iter()
                            .map(|x| x.to_expr_lam(available_defns))
                            .collect(),
                    ),
                    // (f args ...)
                    [f, args @ ..] => DynCall(
                        Box::new(f.to_expr_lam(available_defns)),
                        args.iter()
                            .map(|x| x.to_expr_lam(available_defns))
                            .collect(),
                    ),
                    _ => panic!("cannot parse s-exp to defined expression: {:?}", self),
                }
            },
        }
    }

    pub fn is_defn(&self) -> bool {
        use SExp::{Lst, Sym};
        match self {
            Lst(lst) => match lst.as_slice() {
                [Sym("define"), Lst(lst), _] if lst.len() > 0 => true,
                [Sym("define"), ..] => panic!("invalid definition: {:?}", self),
                _ => false,
            },
            _ => false,
        }
    }

    /// If `self` is a definition, return the name of the definition, its
    /// arguments, and its body.
    pub fn to_name_args_body_of_defns(&self) -> Option<(&'a str, Vec<Symbol<'a>>, SExp<'a>)> {
        use SExp::{Lst, Sym};
        match self {
            Lst(lst) => match lst.as_slice() {
                [Sym("define"), Lst(name_args), body] => match name_args.as_slice() {
                    [Sym(name), args @ ..] => Some((
                        name,
                        args.iter().map(|x| x.as_sym().unwrap()).collect(),
                        body.clone(),
                    )),
                    _ => panic!("invalid definition: {:?}", self),
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl<'a> ExprLambda<'a> {
    pub fn to_expr(
        &self,
        lambda_defns_dest: &mut Vec<Definition<'a>>,
        sym: &mut SymGen,
    ) -> Expr<'a> {
        use ExprLambda::*;
        match self {
            Num(x) => Expr::Num(*x),
            Var(x) => Expr::Var(*x),
            Bool(b) => Expr::Bool(*b),
            If(cond, then, else_) => Expr::If(
                Box::new(cond.to_expr(lambda_defns_dest, sym)),
                Box::new(then.to_expr(lambda_defns_dest, sym)),
                Box::new(else_.to_expr(lambda_defns_dest, sym)),
            ),
            Let(var, expr, body) => Expr::Let(
                *var,
                Box::new(expr.to_expr(lambda_defns_dest, sym)),
                Box::new(body.to_expr(lambda_defns_dest, sym)),
            ),
            Prim0(p) => Expr::Prim0(*p),
            Prim1(p, arg) => Expr::Prim1(*p, Box::new(arg.to_expr(lambda_defns_dest, sym))),
            Prim2(p, arg1, arg2) => Expr::Prim2(
                *p,
                Box::new(arg1.to_expr(lambda_defns_dest, sym)),
                Box::new(arg2.to_expr(lambda_defns_dest, sym)),
            ),
            Do(exps) => Expr::Do(
                exps.iter()
                    .map(|e| e.to_expr(lambda_defns_dest, sym))
                    .collect(),
            ),
            DynCall(f, args) => Expr::DynCall(
                Box::new(f.to_expr(lambda_defns_dest, sym)),
                args.iter()
                    .map(|x| x.to_expr(lambda_defns_dest, sym))
                    .collect(),
            ),
            Call(f, args) => Expr::Call(
                *f,
                args.iter()
                    .map(|x| x.to_expr(lambda_defns_dest, sym))
                    .collect(),
            ),
            Lambda(args, body) => {
                let name = sym.gensym("_lambda");
                let body = body.to_expr(lambda_defns_dest, sym);
                lambda_defns_dest.push(Definition::new(name, args.to_vec(), body));
                Closure(name)
            },
        }
    }
}

impl<'a> FromIterator<SExp<'a>> for Program<'a> {
    fn from_iter<T: IntoIterator<Item = SExp<'a>>>(iter: T) -> Self {
        let lexed = iter.into_iter().collect::<Vec<SExp<'a>>>();
        let mut defs = Vec::new();
        let mut body = None;
        lexed.into_iter().for_each(|s_exp| {
            if s_exp.is_defn() {
                defs.push(s_exp.to_name_args_body_of_defns().unwrap());
            } else {
                if body.is_some() {
                    panic!("multiple bodies in program");
                } else {
                    body = Some(s_exp);
                }
            }
        });
        let available_defns = defs.iter().map(|(name, ..)| *name).collect::<Vec<_>>();

        let mut sym = SymGen::new();
        let mut lambda_defs = Vec::new();

        let defs = defs
            .into_iter()
            .map(|(name, args, body)| {
                let body = body
                    .to_expr_lam(&available_defns)
                    .to_expr(&mut lambda_defs, &mut sym);
                Definition::new(name.to_symbol(), args, body)
            })
            .collect::<Vec<_>>();
        let body = body
            .expect("no body in program")
            .to_expr_lam(&available_defns)
            .to_expr(&mut lambda_defs, &mut sym);

        let defs = lambda_defs
            .into_iter()
            .chain(defs)
            .map(|d| (*d.name(), d))
            .collect::<BTreeMap<_, _>>();

        Program { defs, body }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::s_exp::SExpIterator;

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
                        ExprLambda::DynCall(
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
            let parsed = parsed.to_expr_lam(&[]);
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
            (
                "(+ 1 (some-func 2 3))",
                (
                    Expr::Prim2(
                        Prim2::Plus,
                        Expr::Num(1).into(),
                        Expr::Call("some-func".to_symbol(), vec![Expr::Num(2), Expr::Num(3)])
                            .into(),
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
                        Expr::DynCall(
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
                            Expr::DynCall(Expr::Closure(lmbda_name).into(), vec![Expr::Num(2)])
                                .into(),
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
            let parsed = parsed
                .to_expr_lam(&["some-func"])
                .to_expr(&mut defns, &mut sym);
            assert_eq!(parsed, expected_body);
            assert_eq!(defns, expected_definitions);
        }
    }
}
