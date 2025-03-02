#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Expressions,
    pub local_functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Case {
    pub arg: Expressions,
    /// keys and values
    pub body: Vec<(Expressions, Expressions)>,
    pub default: Option<Expressions>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub body: Expressions,
}

#[derive(Debug, Clone)]
pub struct Expressions {
    pub values: Vec<XorExpression>,
}

#[derive(Debug, Clone)]
pub struct XorExpression {
    pub values: Vec<OrExpression>,
}

#[derive(Debug, Clone)]
pub struct OrExpression {
    pub values: Vec<AndExpression>,
}

#[derive(Debug, Clone)]
pub struct AndExpression {
    pub values: Vec<EqualityExpression>,
}

#[derive(Debug, Clone)]
pub struct EqualityExpression {
    pub lvalue: SumExpression,
    pub rvalue: Option<(EqualityOperator, SumExpression)>,
}

#[derive(Debug, Clone)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
}

#[derive(Debug, Clone)]
pub struct SumExpression {
    pub lvalue: MultExpression,
    pub rvalues: Vec<(SumOperator, MultExpression)>,
}

#[derive(Debug, Clone, Copy)]
pub enum SumOperator {
    Sum,
    Sub,
}

#[derive(Debug, Clone)]
pub struct MultExpression {
    pub lvalue: PipeExpression,
    pub rvalues: Vec<(MultOperator, PipeExpression)>,
}

#[derive(Debug, Clone, Copy)]
pub enum MultOperator {
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct PipeExpression {
    pub values: Vec<FunctionCall>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub args: Vec<UnaryExpression>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: Option<UnaryOperator>,
    pub value: BasicExpression,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Negative,
    Not,
}

#[derive(Debug, Clone)]
pub enum BasicExpression {
    Number(f64),
    Boolean(bool),
    Ident(String),
    Parenthesized(Expressions),
    Lambda(Lambda),
    Case(Case),
}
