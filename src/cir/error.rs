use lady_deirdre::{
    format::AnnotationPriority,
    lexis::{PositionSpan, SourceCode},
};

use crate::error_displayer::DisplayError;

use super::{BinaryOperator, FunctionDescription};

#[derive(Debug, Clone)]
pub enum InterpretationError {
    MismatchedType {
        position: PositionSpan,
        provided: ValueType,
        expected: ValueType,
    },
    CannotUseLambdaInExpression(PositionSpan),
    MismatchedTypes {
        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
        operator: BinaryOperator,
        type_left: ValueType,
        type_right: ValueType,
    },
    NotEnoughArguments {
        function: FunctionDescription,
        provided: usize,
        expected: usize,
    },
    TooMuchArguments {
        function: FunctionDescription,
        provided: usize,
        expected: usize,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    None,
    Number,
    Boolean,
    Lambda,
}

impl InterpretationError {
    pub fn display<'a, C: SourceCode>(self, source: &'a C) -> DisplayError<'a, C> {
        let message;
        let annotations;
        match self {
            InterpretationError::MismatchedType {
                position,
                provided,
                expected,
            } => {
                let expected = match expected {
                    ValueType::None => "none",
                    ValueType::Number => "number",
                    ValueType::Boolean => "bool",
                    ValueType::Lambda => "lambda",
                };
                let provided = match provided {
                    ValueType::None => "none",
                    ValueType::Number => "number",
                    ValueType::Boolean => "bool",
                    ValueType::Lambda => "lambda",
                };
                message = format!("Type mismatch, expected: {expected}, provided: {provided}");
                annotations = vec![(
                    position,
                    AnnotationPriority::Default,
                    format!("Expected: {expected}, provided: {provided}"),
                )];
            }
            InterpretationError::CannotUseLambdaInExpression(range) => {
                message = "Cannot use lambda in expression with binary operator".to_string();
                annotations = vec![(
                    range,
                    AnnotationPriority::Default,
                    "This expression".to_string(),
                )];
            }
            InterpretationError::MismatchedTypes {
                lhs_position,
                rhs_position,
                type_left,
                type_right,
                ..
            } => {
                let type_left = match type_left {
                    ValueType::None => "none",
                    ValueType::Number => "number",
                    ValueType::Boolean => "bool",
                    ValueType::Lambda => "lambda",
                };
                let type_right = match type_right {
                    ValueType::None => "none",
                    ValueType::Number => "number",
                    ValueType::Boolean => "bool",
                    ValueType::Lambda => "lambda",
                };
                message = format!("Type mismatch, left: {type_left}, right: {type_right}");
                annotations = vec![
                    (
                        lhs_position,
                        AnnotationPriority::Default,
                        format!("Type: {type_left}"),
                    ),
                    (
                        rhs_position,
                        AnnotationPriority::Default,
                        format!("Type: {type_right}"),
                    ),
                ];
            }
            InterpretationError::NotEnoughArguments {
                function,
                provided,
                expected,
            } => {
                message = format!("Not enough arguments to the function\nExpected: {expected}, provided: {provided}");
                annotations = vec![(
                    function.position,
                    AnnotationPriority::Default,
                    "This function".to_string(),
                )];
            }
            InterpretationError::TooMuchArguments {
                function,
                provided,
                expected,
            } => {
                message = format!("Too much arguments to the function\nExpected: {expected}, provided: {provided}");
                annotations = vec![(
                    function.position,
                    AnnotationPriority::Default,
                    "This function".to_string(),
                )];
            }
        }
        DisplayError {
            source,
            message,
            annotations,
        }
    }
}
