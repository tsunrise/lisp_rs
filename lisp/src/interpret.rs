//! Evaluation of ASTs

use crate::{
    ast::{Definition, Expr, Program},
    symtab::{Symbol, Symtab},
};
use std::collections::BTreeMap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LispValue {
    Number(i64),
    Bool(bool),
    Pair(Box<LispValue>, Box<LispValue>),
}

pub trait IO {
    fn input_num(&mut self) -> i64;
    fn newline(&mut self);
    fn output_val(&mut self, val: &LispValue);
}

impl<'a> Expr<'a> {
    pub fn interpret<T: IO>(
        &self,
        defs: &BTreeMap<Symbol<'a>, Definition<'a>>,
        env: &Symtab<LispValue>,
        io: &mut T,
    ) -> LispValue {
        use crate::ast::{Prim0, Prim1, Prim2};
        match self {
            Self::Call(f_name, args) => {
                let f = defs
                    .get(&f_name)
                    .expect(&format!("function not found: {}", f_name));
                assert_eq!(
                    f.args().len(),
                    args.len(),
                    "wrong number of arguments when calling {}",
                    f_name
                );
                let mut local_env = env.fork();
                f.args().iter().zip(args.iter()).for_each(|(f, a)| {
                    local_env.insert(f.clone(), a.interpret(defs, &env, io));
                });
                f.body.interpret(defs, &local_env, io)
            },
            Self::DynCall(..) => unimplemented!("dynamic calls not supported yet"),
            Self::Prim0(Prim0::ReadNum) => LispValue::Number(io.input_num()),
            Self::Prim0(Prim0::NewLine) => {
                io.newline();
                LispValue::Bool(true)
            },
            Self::Prim1(Prim1::Print, expr) => {
                let val = expr.interpret(defs, env, io);
                io.output_val(&val);
                LispValue::Bool(true)
            },
            Self::Do(exprs) => {
                let mut last_val = LispValue::Bool(true);
                for expr in exprs {
                    last_val = expr.interpret(defs, env, io);
                }
                last_val
            },
            Self::Prim2(Prim2::Pair, a, bv) => LispValue::Pair(
                Box::new(a.interpret(defs, env, io)),
                Box::new(bv.interpret(defs, env, io)),
            ),
            Self::Prim1(Prim1::Left, expr) => {
                let pair = expr.interpret(defs, env, io);
                match pair {
                    LispValue::Pair(a, _) => *a,
                    _ => panic!("left called on non-pair value"),
                }
            },
            Self::Prim1(Prim1::Right, expr) => {
                let pair = expr.interpret(defs, env, io);
                match pair {
                    LispValue::Pair(_, b) => *b,
                    _ => panic!("right called on non-pair value"),
                }
            },
            Self::Var(name) => {
                if env.contains_key(*name) {
                    env.get(*name).unwrap().clone()
                } else {
                    panic!("unbound variable {}", name)
                }
            },
            Self::Let(var, e, body) => {
                let e = e.interpret(defs, env, io);
                let mut local_env = env.fork();
                local_env.insert(*var, e);
                body.interpret(defs, &local_env, io)
            },
            Self::Num(n) => LispValue::Number(*n),
            Self::Bool(b) => LispValue::Bool(*b),
            Self::Prim1(Prim1::Not, expr) => {
                let val = expr.interpret(defs, env, io);
                match val {
                    LispValue::Bool(b) => LispValue::Bool(!b),
                    _ => panic!("not called on non-bool value"),
                }
            },
            Self::Prim1(Prim1::ZeroP, expr) => {
                let val = expr.interpret(defs, env, io);
                match val {
                    LispValue::Number(n) => LispValue::Bool(n == 0),
                    _ => LispValue::Bool(false),
                }
            },
            Self::Prim1(Prim1::NumP, expr) => {
                let val = expr.interpret(defs, env, io);
                match val {
                    LispValue::Number(_) => LispValue::Bool(true),
                    _ => LispValue::Bool(false),
                }
            },
            Self::Prim1(Prim1::Add1, expr) => {
                let val = expr.interpret(defs, env, io);
                match val {
                    LispValue::Number(n) => LispValue::Number(n + 1),
                    _ => panic!("add1 called on non-number value"),
                }
            },
            Self::Prim1(Prim1::Sub1, expr) => {
                let val = expr.interpret(defs, env, io);
                match val {
                    LispValue::Number(n) => LispValue::Number(n - 1),
                    _ => panic!("sub1 called on non-number value"),
                }
            },
            Self::Prim2(Prim2::Plus, a, b) => {
                let a = a.interpret(defs, env, io);
                let b = b.interpret(defs, env, io);
                match (a, b) {
                    (LispValue::Number(a), LispValue::Number(b)) => LispValue::Number(a + b),
                    _ => panic!("+ called on non-number values"),
                }
            },
            Self::Prim2(Prim2::Minus, a, b) => {
                let a = a.interpret(defs, env, io);
                let b = b.interpret(defs, env, io);
                match (a, b) {
                    (LispValue::Number(a), LispValue::Number(b)) => LispValue::Number(a - b),
                    _ => panic!("- called on non-number values"),
                }
            },
            Self::Prim2(Prim2::Eq, a, b) => {
                let a = a.interpret(defs, env, io);
                let b = b.interpret(defs, env, io);
                LispValue::Bool(a == b)
            },
            Self::Prim2(Prim2::Lt, a, b) => {
                let a = a.interpret(defs, env, io);
                let b = b.interpret(defs, env, io);
                match (a, b) {
                    (LispValue::Number(a), LispValue::Number(b)) => LispValue::Bool(a < b),
                    _ => panic!("< called on non-number values"),
                }
            },
            Self::If(cond, then, else_) => {
                let cond = cond.interpret(defs, env, io);
                match cond {
                    LispValue::Bool(true) => then.interpret(defs, env, io),
                    LispValue::Bool(false) => else_.interpret(defs, env, io),
                    _ => panic!("if called on non-bool value"),
                }
            },
            Self::Closure(_) => todo!(),
        }
    }
}

impl<'a> Program<'a> {
    pub fn interpret<T: IO>(&self, io: &mut T) -> LispValue {
        let env = Symtab::new();
        self.body.interpret(&self.defs, &env, io)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Program, s_exp::SExpIterator};

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum Output {
        NewLine,
        Value(LispValue),
    }

    struct FakeIO {
        inputs: Vec<i64>,
        num_read: usize,
        outputs: Vec<Output>,
    }

    impl FakeIO {
        pub fn new(inputs: Vec<i64>) -> Self {
            FakeIO {
                inputs,
                num_read: 0,
                outputs: vec![],
            }
        }
    }

    impl IO for FakeIO {
        fn input_num(&mut self) -> i64 {
            if self.num_read >= self.inputs.len() {
                panic!("ran out of inputs");
            } else {
                self.num_read += 1;
                self.inputs[self.num_read - 1]
            }
        }

        fn newline(&mut self) {
            self.outputs.push(Output::NewLine);
        }

        fn output_val(&mut self, val: &LispValue) {
            self.outputs.push(Output::Value(val.clone()));
        }
    }

    struct TestCase {
        io: FakeIO,
        program: &'static str,
        expected_outputs: Vec<Output>,
        expected_value: LispValue,
    }

    impl TestCase {
        fn new(
            program: &'static str,
            inputs: Vec<i64>,
            expected_outputs: Vec<Output>,
            expected_value: LispValue,
        ) -> Self {
            TestCase {
                io: FakeIO::new(inputs),
                program,
                expected_outputs,
                expected_value,
            }
        }

        fn new_no_io(program: &'static str, expected_value: LispValue) -> Self {
            TestCase {
                io: FakeIO::new(vec![]),
                program,
                expected_outputs: vec![],
                expected_value,
            }
        }

        fn run_and_check(&mut self) {
            let program = SExpIterator::new(self.program).collect::<Program>();
            let output = program.interpret(&mut self.io);
            assert_eq!(
                self.io.outputs, self.expected_outputs,
                "IO outputs don't match on program: {}", self.program
            );
            assert_eq!(output, self.expected_value, "value doesn't match on program: {}", self.program);
        }
    }

    #[test]
    fn test_interpret() {
        let cases = [
            TestCase::new_no_io("(add1 2)", LispValue::Number(3)),
            TestCase::new_no_io("(add1 (add1 2))", LispValue::Number(4)),
            TestCase::new_no_io("(+ (+ 1 2) (+ 3 4))", LispValue::Number(10)),
            TestCase::new("(+ (read-num) 2)", vec![15], vec![], LispValue::Number(17)),
            TestCase::new("(if (< 15 12) (read-num) 15)", vec![], vec![], LispValue::Number(15)),
            TestCase::new("\
             (define (pair-add p) (+ (left p) (right p)))\
                 (do \
                 (print (pair-add (pair 15 17)))\
                 (newline)\
                 (print (pair-add (pair 10 20))))",
                          vec![],
                          vec![Output::Value(LispValue::Number(32)), Output::NewLine, Output::Value(LispValue::Number(30))], LispValue::Bool(true)),
        ];

        for mut case in cases {
            case.run_and_check();
        }
    }
}
