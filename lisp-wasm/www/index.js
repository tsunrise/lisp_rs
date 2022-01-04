import * as wasm from "lisp-wasm"

window.parse_s_exp = (s) => {
    let parsed = wasm.parse_s_exp(s);
    if (parsed.Lst) {
        return parsed.Lst
    } else if (parsed.Sym) {
        return parsed.Sym
    } else if (parsed.Num) {
        return parsed.Num
    }
}