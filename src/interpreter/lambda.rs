use std::collections::HashMap;

use super::types::Value;

#[derive(Debug, Clone)]
pub struct LambdaState {
    pub params: Vec<String>,
    pub provided_args: Vec<Value>,
    pub local_values: HashMap<String, Value>,
    pub body: Expressions,
}

impl LambdaState {
    pub fn push(&mut self, arg: Value) {
        self.provided_args.push(arg);
    }
}
