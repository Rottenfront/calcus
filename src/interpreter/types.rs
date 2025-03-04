use super::lambda::LambdaState;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Lambda(LambdaState),
    Tuple(Vec<Value>),
}
