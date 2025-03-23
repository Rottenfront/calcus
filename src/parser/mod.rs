use std::collections::HashMap;

use lady_deirdre::{
    lexis::{PositionSpan, ToSpan},
    syntax::{NodeRef, PolyRef},
    units::Document,
};
use lexer::BasicToken;
use syntax::BasicNode;

use crate::{
    cir::{
        std_funcs, BinaryOperator, BinaryOperatorVariant, FunctionIdentifier, UnaryOperator,
        UnaryOperatorVariant,
    },
    compiler::CompilationError,
};

pub mod lexer;
pub mod syntax;

pub struct Parser<'a, 'b, 'code> {
    doc: &'a Document<BasicNode>,
    functions_names: &'b HashMap<&'code str, usize>,
    current_params: HashMap<&'code str, usize>,
}

impl<'a: 'code, 'b: 'code, 'code> Parser<'a, 'b, 'code> {
    fn get_node_position(&self, node: &NodeRef) -> PositionSpan {
        node.span(self.doc)
            .unwrap()
            .to_position_span(self.doc)
            .unwrap()
    }

    pub fn parse_function(
        doc: &'a Document<BasicNode>,
        functions_names: &'b HashMap<&'code str, usize>,
        node: NodeRef,
    ) -> Result<FunctionExpression<'code>, CompilationError> {
        let Some(BasicNode::Function {
            name, params, body, ..
        }) = node.deref(doc)
        else {
            unreachable!();
        };
        let name = (
            name.string(doc).unwrap(),
            name.chunk(doc).unwrap().to_position_span(doc).unwrap(),
        );
        let params = params
            .iter()
            .map(|param| {
                (
                    param.string(doc).unwrap(),
                    param.chunk(doc).unwrap().to_position_span(doc).unwrap(),
                )
            })
            .collect::<Vec<(&'code str, PositionSpan)>>();
        let mut current_params = HashMap::new();
        for (index, (param, _)) in params.iter().enumerate() {
            current_params.insert(*param, index);
        }
        let parser = Parser {
            doc,
            functions_names,
            current_params,
        };
        let position = parser.get_node_position(&node);
        let body = parser.parse_xor_expression(*body)?;

        Ok(FunctionExpression {
            name,
            position,
            params,
            body,
        })
    }

    pub fn parse_xor_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::XorExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_or_expression(*value)?);
        }
        let operators = operators
            .iter()
            .map(|val| BinaryOperator {
                position: val
                    .chunk(self.doc)
                    .unwrap()
                    .to_position_span(self.doc)
                    .unwrap(),
                variant: BinaryOperatorVariant::Xor,
            })
            .collect::<Vec<BinaryOperator>>();
        Ok(if operators.is_empty() {
            new_values[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in new_values.into_iter().enumerate() {
                result = Some(if let Some(lhs) = result {
                    let position = lhs.position.start..rhs.position.end;
                    let operator = operators[idx - 1].clone();
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            operator,
                        })),
                    }
                } else {
                    rhs
                });
            }

            result.unwrap()
        })
    }

    pub fn parse_or_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::OrExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_and_expression(*value)?);
        }
        let operators = operators
            .iter()
            .map(|val| BinaryOperator {
                position: val
                    .chunk(self.doc)
                    .unwrap()
                    .to_position_span(self.doc)
                    .unwrap(),
                variant: BinaryOperatorVariant::Or,
            })
            .collect::<Vec<BinaryOperator>>();
        Ok(if operators.is_empty() {
            new_values[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in new_values.into_iter().enumerate() {
                result = Some(if let Some(lhs) = result {
                    let position = lhs.position.start..rhs.position.end;
                    let operator = operators[idx - 1].clone();
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            operator,
                        })),
                    }
                } else {
                    rhs
                });
            }

            result.unwrap()
        })
    }

    pub fn parse_and_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::AndExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_eq_expression(*value)?);
        }
        let operators = operators
            .iter()
            .map(|val| BinaryOperator {
                position: val
                    .chunk(self.doc)
                    .unwrap()
                    .to_position_span(self.doc)
                    .unwrap(),
                variant: BinaryOperatorVariant::And,
            })
            .collect::<Vec<BinaryOperator>>();
        Ok(if operators.is_empty() {
            new_values[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in new_values.into_iter().enumerate() {
                result = Some(if let Some(lhs) = result {
                    let position = lhs.position.start..rhs.position.end;
                    let operator = operators[idx - 1].clone();
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            operator,
                        })),
                    }
                } else {
                    rhs
                });
            }

            result.unwrap()
        })
    }

    pub fn parse_eq_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let position = self.get_node_position(&node);
        let Some(BasicNode::EqualityExpression {
            lvalue,
            rvalue,
            operator,
            ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let lhs = self.parse_sum_expression(*lvalue)?;
        Ok(if let Some(operator_variant) = operator.deref(self.doc) {
            let operator = BinaryOperator {
                position: operator
                    .chunk(self.doc)
                    .unwrap()
                    .to_position_span(self.doc)
                    .unwrap(),
                variant: match operator_variant {
                    BasicToken::Equal => BinaryOperatorVariant::Equals,
                    BasicToken::NotEqual => BinaryOperatorVariant::NotEquals,
                    BasicToken::Greater => BinaryOperatorVariant::Greater,
                    BasicToken::Less => BinaryOperatorVariant::Less,
                    BasicToken::GreaterEqual => BinaryOperatorVariant::GreaterEqual,
                    BasicToken::LessEqual => BinaryOperatorVariant::LessEqual,
                    _ => unreachable!(),
                },
            };
            let rhs = self.parse_sum_expression(*rvalue)?;
            ValueExpression {
                position,
                inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                    lhs,
                    rhs,
                    operator,
                })),
            }
        } else {
            lhs
        })
    }

    pub fn parse_sum_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::SumExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_mult_expression(*value)?);
        }
        let operators = operators
            .iter()
            .map(|val| {
                val.deref(self.doc)
                    .map(|token| BinaryOperator {
                        position: val
                            .chunk(self.doc)
                            .unwrap()
                            .to_position_span(self.doc)
                            .unwrap(),
                        variant: match token {
                            BasicToken::Plus => BinaryOperatorVariant::Addition,
                            BasicToken::Minus => BinaryOperatorVariant::Substraction,
                            _ => unreachable!(),
                        },
                    })
                    .unwrap()
            })
            .collect::<Vec<BinaryOperator>>();
        Ok(if operators.is_empty() {
            new_values[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in new_values.into_iter().enumerate() {
                result = Some(if let Some(lhs) = result {
                    let position = lhs.position.start..rhs.position.end;
                    let operator = operators[idx - 1].clone();
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            operator,
                        })),
                    }
                } else {
                    rhs
                });
            }

            result.unwrap()
        })
    }

    pub fn parse_mult_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::MultExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_pipe_expression(*value)?);
        }
        let operators = operators
            .iter()
            .map(|val| {
                val.deref(self.doc)
                    .map(|token| BinaryOperator {
                        position: val
                            .chunk(self.doc)
                            .unwrap()
                            .to_position_span(self.doc)
                            .unwrap(),
                        variant: match token {
                            BasicToken::Star => BinaryOperatorVariant::Multiplication,
                            BasicToken::Slash => BinaryOperatorVariant::Division,
                            _ => unreachable!(),
                        },
                    })
                    .unwrap()
            })
            .collect::<Vec<BinaryOperator>>();
        Ok(if operators.is_empty() {
            new_values[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in new_values.into_iter().enumerate() {
                result = Some(if let Some(lhs) = result {
                    let position = lhs.position.start..rhs.position.end;
                    let operator = operators[idx - 1].clone();
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            operator,
                        })),
                    }
                } else {
                    rhs
                });
            }

            result.unwrap()
        })
    }

    pub fn parse_pipe_expression(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::PipeExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_func_call(*value)?);
        }
        let operators = operators
            .iter()
            .map(|val| BinaryOperator {
                position: val
                    .chunk(self.doc)
                    .unwrap()
                    .to_position_span(self.doc)
                    .unwrap(),
                variant: BinaryOperatorVariant::PipeOperator,
            })
            .collect::<Vec<BinaryOperator>>();
        Ok(if operators.is_empty() {
            new_values[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in new_values.into_iter().enumerate() {
                result = Some(if let Some(lhs) = result {
                    let position = lhs.position.start..rhs.position.end;
                    let operator = operators[idx - 1].clone();
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Binary(Box::new(BinaryExpression {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            operator,
                        })),
                    }
                } else {
                    rhs
                });
            }

            result.unwrap()
        })
    }

    pub fn parse_func_call(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let Some(BasicNode::FunctionCall { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let mut new_values = Vec::with_capacity(values.len());
        for value in values {
            new_values.push(self.parse_unary_expr(*value)?);
        }
        Ok(new_values
            .into_iter()
            .reduce(|func, arg| {
                let position = func.position.start..arg.position.end;
                ValueExpression {
                    position,
                    inner: ValueExpressionInner::FuncCall(Box::new(FunctionCall { func, arg })),
                }
            })
            .unwrap())
    }

    pub fn parse_unary_expr(
        &self,
        node: NodeRef,
    ) -> Result<ValueExpression<'code>, CompilationError> {
        let position = self.get_node_position(&node);
        let Some(BasicNode::UnaryExpression { op, value, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let value = match value.deref(self.doc).unwrap() {
            BasicNode::Literal { node, .. } => self.parse_literal(*node),
            BasicNode::ParenthesesExpression { value, .. } => self.parse_xor_expression(*value),
            BasicNode::LambdaExpression { node, .. } => self.parse_lambda(*node),
            BasicNode::IfExpression { node, .. } => self.parse_if_expr(*node),
            _ => unreachable!(),
        }?;
        Ok(match op.deref(self.doc) {
            Some(operator) => {
                let operator = UnaryOperator {
                    position: op
                        .chunk(self.doc)
                        .unwrap()
                        .to_position_span(self.doc)
                        .unwrap(),
                    variant: match operator {
                        BasicToken::Exclamation => UnaryOperatorVariant::Inversion,
                        BasicToken::Tilde => UnaryOperatorVariant::Negation,
                        _ => unreachable!(),
                    },
                };
                ValueExpression {
                    position,
                    inner: ValueExpressionInner::Unary(Box::new(UnaryExpression {
                        operand: value,
                        operator,
                    })),
                }
            }
            None => value,
        })
    }

    pub fn parse_literal(&self, node: NodeRef) -> Result<ValueExpression<'code>, CompilationError> {
        let position = self.get_node_position(&node);
        let Some(BasicNode::Literal { value, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        Ok(ValueExpression {
            position: position.clone(),
            inner: match value.deref(self.doc).unwrap() {
                BasicToken::KwNone => ValueExpressionInner::None,
                BasicToken::KwTrue => ValueExpressionInner::Bool(true),
                BasicToken::KwFalse => ValueExpressionInner::Bool(false),
                BasicToken::Number => match value.string(self.doc).unwrap().parse() {
                    Ok(integer) => ValueExpressionInner::Integer(integer),
                    Err(_) => ValueExpressionInner::Float(
                        value.string(self.doc).unwrap().parse().unwrap(),
                    ),
                },
                BasicToken::Ident => {
                    let name = value.string(self.doc).unwrap();
                    if let Some(index) = self.current_params.get(&name) {
                        ValueExpressionInner::Parameter(*index)
                    } else {
                        if let Some(index) = self.functions_names.get(&name) {
                            ValueExpressionInner::Function(FunctionIdentifier::Defined(*index))
                        } else {
                            if let Some(function) = std_funcs::find_name(name) {
                                ValueExpressionInner::Function(FunctionIdentifier::BuiltIn(
                                    function,
                                ))
                            } else {
                                return Err(CompilationError::NameNotFound(position));
                            }
                        }
                    }
                }
                _ => unreachable!(),
            },
        })
    }

    pub fn parse_if_expr(&self, node: NodeRef) -> Result<ValueExpression<'code>, CompilationError> {
        let position = self.get_node_position(&node);
        let Some(&BasicNode::IfExpression {
            cond,
            if_true,
            if_false,
            ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        Ok(ValueExpression {
            position,
            inner: ValueExpressionInner::If(Box::new(IfExpression {
                cond: self.parse_xor_expression(cond)?,
                if_true: self.parse_xor_expression(if_true)?,
                if_false: self.parse_xor_expression(if_false)?,
            })),
        })
    }

    pub fn parse_lambda(&self, node: NodeRef) -> Result<ValueExpression<'code>, CompilationError> {
        let position = self.get_node_position(&node);
        let Some(BasicNode::LambdaExpression { params, body, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let params = params
            .iter()
            .map(|param| {
                (
                    param.string(self.doc).unwrap(),
                    param
                        .chunk(self.doc)
                        .unwrap()
                        .to_position_span(self.doc)
                        .unwrap(),
                )
            })
            .collect::<Vec<(&'code str, PositionSpan)>>();
        let mut current_params = HashMap::new();
        for (index, (param, _)) in params.iter().enumerate() {
            current_params.insert(*param, index);
        }
        let parser = Parser {
            doc: self.doc,
            functions_names: self.functions_names,
            current_params,
        };

        Ok(ValueExpression {
            position,
            inner: ValueExpressionInner::Lambda(Box::new(LambdaBody {
                params,
                body: parser.parse_xor_expression(*body)?,
            })),
        })
    }
}

#[derive(Clone, Debug)]
pub struct FunctionExpression<'code> {
    pub name: (&'code str, PositionSpan),
    pub position: PositionSpan,
    pub params: Vec<(&'code str, PositionSpan)>,
    pub body: ValueExpression<'code>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpression<'code> {
    pub lhs: ValueExpression<'code>,
    pub rhs: ValueExpression<'code>,
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug)]
pub struct UnaryExpression<'code> {
    pub operand: ValueExpression<'code>,
    pub operator: UnaryOperator,
}

#[derive(Clone, Debug)]
pub struct FunctionCall<'code> {
    pub func: ValueExpression<'code>,
    pub arg: ValueExpression<'code>,
}

#[derive(Clone, Debug)]
pub struct ValueExpression<'code> {
    pub position: PositionSpan,
    pub inner: ValueExpressionInner<'code>,
}

#[derive(Clone, Debug)]
pub enum ValueExpressionInner<'code> {
    None,
    Binary(Box<BinaryExpression<'code>>),
    Unary(Box<UnaryExpression<'code>>),
    FuncCall(Box<FunctionCall<'code>>),
    Function(FunctionIdentifier),
    Parameter(usize),
    Bool(bool),
    Integer(i64),
    Float(f64),
    Lambda(Box<LambdaBody<'code>>),
    If(Box<IfExpression<'code>>),
}

#[derive(Clone, Debug)]
pub struct LambdaBody<'code> {
    pub params: Vec<(&'code str, PositionSpan)>,
    pub body: ValueExpression<'code>,
}

#[derive(Clone, Debug)]
pub struct IfExpression<'code> {
    pub cond: ValueExpression<'code>,
    pub if_true: ValueExpression<'code>,
    pub if_false: ValueExpression<'code>,
}
