use crate::{
    ast::{Definition, Expr, Prim2, Program, Prototype},
    symtab::{Symbol, Symtab},
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue},
};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub fpm: inkwell::passes::PassManager<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("demo");
        let fpm = PassManager::create(&module);

        // optimizations options
        fpm.add_instruction_combining_pass(); // peephole optimizations
        fpm.add_reassociate_pass(); // reassociate expressions
        fpm.add_gvn_pass(); // perform CSE
        fpm.add_cfg_simplification_pass(); // simplify control flow graph

        fpm.initialize();

        Compiler {
            context,
            builder,
            module,
            fpm,
        }
    }

    pub fn run_passes(&self, f: FunctionValue) {
        self.fpm.run_on(&f);
    }
}

impl<'a> Expr<'a> {
    pub fn codegen<'ctx>(
        &self,
        compiler: &Compiler<'ctx>,
        env: &Symtab<BasicValueEnum<'ctx>>,
    ) -> IntValue<'ctx> {
        match self {
            Expr::Num(n) => compiler.context.i64_type().const_int(*n as u64, true),
            Expr::Prim2(prim, a, b) => compile_prim2(compiler, env, *prim, a, b),
            Expr::Call(f, args) => {
                // check if function is defined
                let f = compiler
                    .module
                    .get_function(f.to_string().as_str())
                    .expect(format!("undefined function: {}", f).as_str());
                let compiled_args = args
                    .iter()
                    .map(|e| e.codegen(compiler, env).into())
                    .collect::<Vec<_>>();
                compiler
                    .builder
                    .build_call(f, &compiled_args, "calltmp")
                    .try_as_basic_value()
                    .left()
                    .expect("invalid function return value")
                    .into_int_value()
            },
            Expr::Var(name) => {
                let var = env.get(*name);
                match var {
                    Some(v) => v.into_int_value(),
                    None => panic!("Unbound variable: {}", name),
                }
            },
            Expr::If(cond, then, els) => {
                let cond_val = cond.codegen(compiler, env);
                // cond_val is an i64, we need to convert it to i1
                // rule: if cond_val is 0, it's false, otherwise it's true
                let cond_bool = compiler.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond_val,
                    compiler.context.i64_type().const_zero(),
                    "if_cond",
                );

                let curr_func = compiler
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .expect("no parent");

                let mut then_bb = compiler.context.append_basic_block(curr_func, "if_then");
                let mut else_bb = compiler.context.append_basic_block(curr_func, "if_else");
                let merge_bb = compiler
                    .context
                    .append_basic_block(curr_func, "if_continue");

                compiler
                    .builder
                    .build_conditional_branch(cond_bool, then_bb, else_bb);

                // codegen for THEN branch
                compiler.builder.position_at_end(then_bb);
                let then_val = then.codegen(compiler, env);
                compiler.builder.build_unconditional_branch(merge_bb);
                // get updated then block because codegen for THEN branch may have added new
                // blocks
                then_bb = compiler.builder.get_insert_block().unwrap();

                // codegen for ELSE branch
                compiler.builder.position_at_end(else_bb);
                let else_val = els.codegen(compiler, env);
                compiler.builder.build_unconditional_branch(merge_bb);
                // get updated else block because codegen for ELSE branch may have added new
                // blocks
                else_bb = compiler.builder.get_insert_block().unwrap();

                // codegen for merge block
                compiler.builder.position_at_end(merge_bb);
                let phi = compiler
                    .builder
                    .build_phi(compiler.context.i64_type(), "if_merge_phi");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                phi.as_basic_value().into_int_value()
            },
            _ => todo!("unsupported expression: {}", self),
        }
    }
}

fn compile_prim2<'ctx>(
    compiler: &Compiler<'ctx>,
    env: &Symtab<BasicValueEnum<'ctx>>,
    prim2: Prim2,
    a: &Expr,
    b: &Expr,
) -> IntValue<'ctx> {
    let a = a.codegen(compiler, env);
    let b = b.codegen(compiler, env);
    match prim2 {
        Prim2::Plus => compiler.builder.build_int_add(a, b, "addtmp"),
        Prim2::Minus => compiler.builder.build_int_sub(a, b, "subtmp"),
        Prim2::Times => compiler.builder.build_int_mul(a, b, "multmp"),
        Prim2::Divide => compiler.builder.build_int_signed_div(a, b, "divtmp"),
        Prim2::Eq => {
            let cmp =
                compiler
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, a, b, "eqtmp_i1");
            // cmp is i1, convert to i64
            compiler
                .builder
                .build_int_z_extend(cmp, compiler.context.i64_type(), "eqtmp")
        },
        Prim2::Lt => {
            let cmp =
                compiler
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, a, b, "lttmp_i1");
            // cmp is i1, convert to i64
            compiler
                .builder
                .build_int_z_extend(cmp, compiler.context.i64_type(), "lttmp")
        },

        Prim2::Pair => {
            todo!()
        },
    }
}

impl<'a> Prototype<'a> {
    pub fn codegen<'ctx>(&self, compiler: &Compiler<'ctx>) -> FunctionValue<'ctx> {
        let ret_type = compiler.context.i64_type();
        let args_types = (0..self.args.len())
            .map(|_| ret_type.into())
            .collect::<Vec<_>>();

        let fn_type = ret_type.fn_type(&args_types, false);
        let fn_val = compiler
            .module
            .add_function(&self.name.to_string(), fn_type, None);

        // set argument names
        fn_val.get_param_iter().enumerate().for_each(|(i, arg)| {
            arg.into_int_value()
                .set_name(self.args[i].to_string().as_str())
        });

        fn_val
    }
}

impl<'a> Definition<'a> {
    pub fn main(body: Expr<'a>) -> Self {
        Definition::new(Symbol::non_unique("main"), vec![], body)
    }

    /// generate declaration of function
    pub fn codegen_declaration<'ctx>(&self, compiler: &Compiler<'ctx>) -> FunctionValue<'ctx> {
        self.proto.codegen(compiler)
    }

    /// generate body for the function definition
    pub fn codegen_body<'ctx>(&self, compiler: &Compiler<'ctx>) -> FunctionValue<'ctx> {
        let function = compiler
            .module
            .get_function(self.proto.name.to_string().as_str())
            .unwrap();
        let entry = compiler.context.append_basic_block(function, "entry");

        compiler.builder.position_at_end(entry);

        // bind arguments
        let mut env = Symtab::new();
        self.proto
            .args
            .iter()
            .zip(function.get_param_iter())
            .for_each(|(name, arg)| {
                env.insert(*name, arg);
            });

        // generate code for the body
        let ret_val = self.body.codegen(compiler, &env);

        // return the result
        compiler.builder.build_return(Some(&ret_val));

        assert!(function.verify(true));

        function
    }
}

impl<'a> Program<'a> {
    pub fn codegen(self, compiler: &Compiler) {
        // first, compiler all function declarations so they can call each other
        self.defs.values().for_each(|def| {
            def.codegen_declaration(compiler);
        });
        // then, compile all body of all functions
        self.defs.values().for_each(|def| {
            let f = def.codegen_body(compiler);
            // optimize the function
            compiler.run_passes(f);
        });

        let main = Definition::main(self.body);
        main.codegen_declaration(compiler);
        let main_f = main.codegen_body(compiler);
        // optimize the main function
        compiler.run_passes(main_f);
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Program, compiler::Compiler, s_exp::SExpIterator};
    use inkwell::{context::Context, OptimizationLevel};

    fn evaluate_body_using_jit(compiler: &Compiler) -> i64 {
        let jit_engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let main_func = unsafe { jit_engine.get_function::<unsafe extern "C" fn() -> i64>("main") }
            .expect("unable to get main function: ");
        let value = unsafe { main_func.call() };

        // free up space
        jit_engine.remove_module(&compiler.module).unwrap();

        value
    }

    #[test]
    fn control_flow_test() {
        const PROGRAM: &str = include_str!("../../lisp_examples/fib.lisp");

        let program = SExpIterator::new(&PROGRAM).collect::<Program>();
        let context = Context::create();
        let compiler = Compiler::new(&context);

        program.codegen(&compiler);

        compiler.module.print_to_stderr();

        // evaluate the compiled program
        let value = evaluate_body_using_jit(&compiler);

        println!();
        println!("Return Value: {}", value);
        assert_eq!(value, 55);
    }


}
