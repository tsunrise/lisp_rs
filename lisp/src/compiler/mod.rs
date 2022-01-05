use crate::{
    ast::{Definition, Expr, Prim2, Program, Prototype},
    symtab::{Symbol, Symtab},
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue},
};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, builder: Builder<'ctx>, module: Module<'ctx>) -> Self {
        Compiler {
            context,
            builder,
            module,
        }
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
            Expr::Prim2(Prim2::Plus, a, b) => {
                let a = a.codegen(compiler, env);
                let b = b.codegen(compiler, env);
                compiler.builder.build_int_add(a, b, "addtmp")
            },
            Expr::Prim2(Prim2::Minus, a, b) => {
                let a = a.codegen(compiler, env);
                let b = b.codegen(compiler, env);
                compiler.builder.build_int_sub(a, b, "subtmp")
            },
            Expr::Var(name) => {
                let var = env.get(*name);
                match var {
                    Some(v) => v.into_int_value(),
                    None => panic!("Unbound variable: {}", name),
                }
            },
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
            _ => todo!()
        }
    }
}

impl<'a> Prototype<'a> {
    pub fn codegen<'ctx>(&self, compiler: &Compiler<'ctx>) -> FunctionValue<'ctx> {
        let ret_type = compiler.context.i64_type();
        let args_types = (0..self.args.len())
            .map(|_| ret_type.into())
            .collect::<Vec<_>>();

        let fn_type = ret_type.fn_type(&args_types, false);
        let fn_name = format!("{}", self.name);
        let fn_val = compiler.module.add_function(&fn_name, fn_type, None);

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

    /// generate code for the function definition
    pub fn codegen<'ctx>(&self, compiler: &Compiler<'ctx>) -> FunctionValue<'ctx> {
        let function = self.proto.codegen(compiler);

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
        // first, compile all definitions
        self.defs.values().for_each(|def| {
            def.codegen(compiler);
        });

        let main = Definition::main(self.body);
        main.codegen(compiler);
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Program, compiler::Compiler, s_exp::SExpIterator};
    use inkwell::context::Context;

    #[test]
    fn demo() {
        let program_str = std::fs::read_to_string("./lisp_examples/demo_program.lisp").unwrap();

        let program = SExpIterator::new(&program_str).collect::<Program>();
        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("demo");
        let compiler = Compiler::new(&context, builder, module);

        program.codegen(&compiler);

        compiler.module.print_to_stderr();
    }
}
