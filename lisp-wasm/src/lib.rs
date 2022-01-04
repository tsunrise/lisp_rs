#![feature(extern_types)]

mod utils;

use std::panic;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use lisp::s_exp::SExpIterator;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(typescript_custom_section)]
const SEXP: &str = r#"
interface SExp {
Num?: number;
Sym?: string;
Lst?: SExp[];
}
"#;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type=SExp)]
    pub type SExp;
}

#[wasm_bindgen]
pub fn parse_s_exp(input: &str) -> SExp {
    let result = panic::catch_unwind(|| {
        let mut parser = SExpIterator::new(input);
        let s_exp = parser.next().unwrap();
        JsValue::from_serde(&s_exp).unwrap().unchecked_into()
    });
    match result {
        Ok(s_exp) => s_exp,
        Err(_) => JsValue::from_str("code panicked").unchecked_into(),
    }
}