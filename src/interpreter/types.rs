use super::lambda::LambdaState;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Lambda(LambdaState),
    Tuple(Vec<Value>),
}
