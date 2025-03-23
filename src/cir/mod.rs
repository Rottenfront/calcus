pub mod error;
pub mod interpreter;
pub mod std_funcs;
pub mod type_checker;

pub use error::*;

use std::fmt::Debug;

use lady_deirdre::lexis::PositionSpan;

#[derive(Debug, Clone, Default)]
pub struct FunctionDescription {
    pub name: Option<String>,
    pub position: PositionSpan,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params_count: usize,
    /// Last value of stack is returned
    pub stack: Vec<StackValue>,
    pub body: Vec<Action>,
}

#[derive(Debug, Clone, Copy)]
pub enum StackValue {
    Bool(bool),
    Integer(i64),
    Float(f64),
    Function(FunctionIdentifier),
    /// Index of parameter in argument array
    Parameter(usize),
    None,
}

#[derive(Debug, Clone)]
pub enum Action {
    UnaryOperation {
        src: usize,
        position: PositionSpan,
        operator: UnaryOperator,
    },

    BinaryOperation {
        lhs: usize,
        rhs: usize,
        operator: BinaryOperator,

        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
    },

    Copy {
        src: usize,
        dist: usize,
    },

    ConditionalJump {
        src: usize,
        position: PositionSpan,
        label: usize,
    },

    Goto(usize),
}

#[derive(Clone, Debug)]
pub struct UnaryOperator {
    pub position: PositionSpan,
    pub variant: UnaryOperatorVariant,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperatorVariant {
    /// !
    Inversion,
    /// ~
    Negation,
}

#[derive(Clone, Debug)]
pub struct BinaryOperator {
    pub position: PositionSpan,
    pub variant: BinaryOperatorVariant,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperatorVariant {
    Xor,
    Or,
    And,
    Equals,
    NotEquals,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Addition,
    Substraction,
    Multiplication,
    Division,
    /// Function call (|>)
    PipeOperator,
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    #[default]
    None,
    Integer(i64),
    Float(f64),
    Bool(bool),
    LambdaState(LambdaState),
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionIdentifier {
    Defined(usize),
    BuiltIn(std_funcs::BuiltInFunction),
}

#[derive(Debug, Clone)]
pub struct LambdaState {
    pub function: FunctionIdentifier,
    pub provided_args: Vec<Value>,
}
