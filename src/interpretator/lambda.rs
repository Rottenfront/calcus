use super::ast::Expressions;

#[derive(Debug, Clone)]
pub struct LambdaState {
    pub params: Vec<String>,
    pub provided_args: Vec<Expressions>,
    pub body: Expressions,
}
