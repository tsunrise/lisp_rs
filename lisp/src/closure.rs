use std::collections::{BTreeMap, BTreeSet};
use crate::ast::{Definition, Expr};
use crate::symtab::{Symbol, Symtab};

/// Definition with captured free variables.
/// `T` is Lisp Value if we are using interpreter.
/// `T` is memory location to heap if we are using compiler.
pub struct CapturedDefinition<'a, T>{
    pub definition: Definition<'a>,
    pub captured_free_variables: BTreeMap<Symbol<'a>, T>
}

impl<'a> Definition<'a>{
    pub fn to_captured<T>(&self, definitions: &[Definition<'a>], environment: Symtab<T>) -> CapturedDefinition<'a, T>{
        let me = self.clone();
        todo!()
    }
}

impl<'a> Expr<'a> {
    pub fn free_variables<'b>(&self, definitions: &[Definition<'a>], bound_variables: &'b Symtab<()>, free_variables_dest: &mut Symtab<'a, '_, ()>)  {
        use Expr::*;
        match self{
            Var(s) if !bound_variables.contains_key(*s) => {
                free_variables_dest.insert(*s, ());
            }
            Let(v, e, body) => {
                e.free_variables(definitions, bound_variables, free_variables_dest);
                let mut updated_bound = bound_variables.fork();
                updated_bound.insert(*v, ());
                body.free_variables(definitions, &updated_bound, free_variables_dest);
            }
            _ => todo!()
        }
    }
}