//! Evaluation of ASTs

use crate::ast::Program;

pub enum LispValue {
    Bool(bool),
    Pair(Box<LispValue>, Box<LispValue>),
}

impl<'a> Program<'a> {
    pub fn interpret(&self) {

    }
}