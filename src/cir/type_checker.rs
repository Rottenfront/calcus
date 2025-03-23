use lady_deirdre::lexis::PositionSpan;

use crate::parser::FunctionExpression;

use super::BinaryOperator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    None,
    Number,
    Boolean,
    Lambda,
}

#[derive(PartialEq, Eq, Clone)]
pub enum TypeState {
    Solved(ValueType),
    Variable(usize),
    Lambda(Vec<TypeState>),
}

#[derive(Clone, Copy)]
enum SalvationState {
    None,
    WIP,
    Done,
}

pub enum TypeError {
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
}

pub struct TypeSolver<'a, 'code> {
    functions: Vec<(&'a FunctionExpression<'code>, TypeState, SalvationState)>,
}

impl<'a, 'code> TypeSolver<'a, 'code> {
    pub fn new(functions: Vec<&'a FunctionExpression<'code>>) -> Self {
        let mut counter = 0;
        Self {
            functions: functions
                .iter()
                .map(|function| {
                    let mut types = vec![TypeState::Variable(0); function.params.len() + 1];
                    for type_ in &mut types {
                        *type_ = TypeState::Variable(counter);
                        counter += 1;
                    }
                    (*function, TypeState::Lambda(types), SalvationState::None)
                })
                .collect(),
        }
    }

    pub fn solve(&mut self) -> Result<(), TypeError> {
        Ok(())
    }
}
