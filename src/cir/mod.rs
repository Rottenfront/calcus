use lady_deirdre::lexis::PositionSpan;

pub struct Function {
    pub params_count: usize,
    /// Last value of stack is returned
    pub stack: Vec<StackValue>,
    pub body: Body,
}

pub enum StackValue {
    Integer(i64),
    Float(f64),
    Function {
        id: usize,
    },
    Lambda {
        id: usize,
    },
    /// Index of parameter in argument array
    Parameter(usize),
    None,
}

pub struct Body(pub Vec<Action>);

pub enum Action {
    /// This variant cannot return any error, so we do not need to track its
    /// position
    MakeTuple {
        dest: usize,
        src: Vec<usize>,
    },

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
        src: Vec<usize>,
        template: Vec<Option<usize>>,
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

pub enum Value {
    Integer(i64),
    Float(f64),
    Lambda { function_id: usize },
}

pub struct LambdaState {
    pub function: usize,
    pub provided_args: Vec<Value>,
}
