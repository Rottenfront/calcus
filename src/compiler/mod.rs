use std::collections::HashMap;

use crate::{cir::*, parser::FunctionExpression};

pub struct Compiler {
    functions: Vec<Function>,
}

impl Compiler {
    pub fn compile_functions<'code>(functions: Vec<FunctionExpression<'code>>) {}

    pub fn compile_function<'code>(function: &HashMap<&'code str, usize>) {}
}
