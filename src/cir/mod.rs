use std::rc::Rc;

use lady_deirdre::lexis::PositionSpan;

pub struct Function {
    pub params_count: usize,
    /// Last value of stack is returned
    pub stack: Vec<StackValue>,
    pub body: Body,
}

pub enum StackValue {
    Constant(Value),
    /// Index of parameter in argument array
    Parameter(usize),
    None,
}

pub struct Body(Vec<Action>);

pub enum Action {
    /// This variant cannot return any error, so we do not need to track its
    /// position
    MakeTuple {
        dest: usize,
        src: Vec<usize>,
    },

    UnaryOperation {
        src: usize,
        operator: UnaryOperator,
        /// Position of operator in document
        position: PositionSpan,
    },

    BinaryOperation {
        lhs: usize,
        rhs: usize,
        operator: BinaryOperator,
        /// Position of operator in document
        /// Is None if this is function call without pipe operator
        op_position: Option<PositionSpan>,

        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
    },

    Move {
        src: usize,
        dist: usize,
    },

    ConditionalJump {
        src: Vec<usize>,
        template: Vec<Option<usize>>,
        label: usize,
    },

    Goto(usize),
}

pub enum UnaryOperator {
    /// !
    Inversion,
    /// ~
    Negation,
}

pub enum BinaryOperator {
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

pub enum Value {
    Integer(i64),
    Float(f64),
    Lambda(LambdaState),
    Tuple(Vec<Value>),
}

pub struct LambdaState {
    function: Rc<Function>,
    provided_args: Vec<Value>,
}
