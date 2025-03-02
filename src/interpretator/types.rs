use super::lambda::LambdaState;

pub enum Value {
    Number(f64),
    Bool(bool),
    Lambda(LambdaState),
    Array(Vec<Value>),
}
