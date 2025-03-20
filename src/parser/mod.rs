use lady_deirdre::{
    lexis::{PositionSpan, ToSpan},
    syntax::{NodeRef, PolyRef},
    units::Document,
};
use lexer::BasicToken;
use syntax::BasicNode;

use crate::cir::{BinaryOperator, BinaryOperatorVariant, UnaryOperator, UnaryOperatorVariant};

pub mod lexer;
pub mod syntax;

pub struct Parser<'a> {
    doc: &'a Document<BasicNode>,
}

impl<'a> Parser<'a> {
    pub fn new(doc: &'a Document<BasicNode>) -> Self {
        Self { doc }
    }

    fn get_node_position(&self, node: &NodeRef) -> PositionSpan {
        node.span(self.doc)
            .unwrap()
            .to_position_span(self.doc)
            .unwrap()
    }

    pub fn parse_function(&self, node: NodeRef) -> FunctionExpression {
        let position = self.get_node_position(&node);
        let Some(BasicNode::Function {
            name, params, body, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let name = name.string(self.doc).unwrap();
        let params = params
            .iter()
            .map(|param| param.string(self.doc).unwrap())
            .collect();
        let body = self.parse_xor_expression(*body);

        FunctionExpression {
            name,
            position,
            params,
            body,
        }
    }

    pub fn parse_xor_expression(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::XorExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let values = values.iter().map(|val| self.parse_or_expression(*val));
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
        if operators.is_empty() {
            values.collect::<Vec<_>>()[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in values.enumerate() {
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
        }
    }

    pub fn parse_or_expression(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::OrExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let values = values.iter().map(|val| self.parse_and_expression(*val));
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
        if operators.is_empty() {
            values.collect::<Vec<_>>()[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in values.enumerate() {
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
        }
    }

    pub fn parse_and_expression(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::AndExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let values = values.iter().map(|val| self.parse_eq_expression(*val));
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
        if operators.is_empty() {
            values.collect::<Vec<_>>()[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in values.enumerate() {
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
        }
    }

    pub fn parse_eq_expression(&self, node: NodeRef) -> ValueExpression {
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
        let lhs = self.parse_sum_expression(*lvalue);
        if let Some(operator_variant) = operator.deref(self.doc) {
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
            let rhs = self.parse_sum_expression(*rvalue);
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
        }
    }

    pub fn parse_sum_expression(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::SumExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let values = values.iter().map(|val| self.parse_mult_expression(*val));
        let operator_spans = operators
            .iter()
            .map(|val| {
                val.chunk(self.doc)
                    .unwrap()
                    .to_position_span(self.doc)
                    .unwrap()
            })
            .collect::<Vec<PositionSpan>>();
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
        if operator_spans.is_empty() {
            values.collect::<Vec<_>>()[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in values.enumerate() {
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
        }
    }

    pub fn parse_mult_expression(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::MultExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let values = values.iter().map(|val| self.parse_pipe_expression(*val));
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
        if operators.is_empty() {
            values.collect::<Vec<_>>()[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in values.enumerate() {
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
        }
    }

    pub fn parse_pipe_expression(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::PipeExpression {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let values = values.iter().map(|val| self.parse_func_call(*val));
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
        if operators.is_empty() {
            values.collect::<Vec<_>>()[0].clone()
        } else {
            let mut result: Option<ValueExpression> = None;
            for (idx, rhs) in values.enumerate() {
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
        }
    }

    pub fn parse_func_call(&self, node: NodeRef) -> ValueExpression {
        let Some(BasicNode::FunctionCall { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        values
            .iter()
            .map(|val| self.parse_unary_expr(*val))
            .reduce(|func, arg| {
                let position = func.position.start..arg.position.end;
                ValueExpression {
                    position,
                    inner: ValueExpressionInner::FuncCall(Box::new(FunctionCall { func, arg })),
                }
            })
            .unwrap()
    }

    pub fn parse_unary_expr(&self, node: NodeRef) -> ValueExpression {
        let position = self.get_node_position(&node);
        let Some(BasicNode::UnaryExpression { op, value, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let value = {
            match value.deref(self.doc).unwrap() {
                BasicNode::Literal { node, .. } => self.parse_literal(*node),
                BasicNode::ParenthesesExpression { value, .. } => self.parse_xor_expression(*value),
                BasicNode::LambdaExpression { node, .. } => self.parse_lambda(*node),
                BasicNode::IfExpression { node, .. } => self.parse_if_expr(*node),
                _ => unreachable!(),
            }
        };
        match op.deref(self.doc) {
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
        }
    }

    pub fn parse_literal(&self, node: NodeRef) -> ValueExpression {
        let position = self.get_node_position(&node);
        let Some(BasicNode::Literal { value, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        ValueExpression {
            position,
            inner: match value.deref(self.doc).unwrap() {
                BasicToken::KwTrue => ValueExpressionInner::Bool(true),
                BasicToken::KwFalse => ValueExpressionInner::Bool(false),
                BasicToken::Number => match value.string(self.doc).unwrap().parse() {
                    Ok(integer) => ValueExpressionInner::Integer(integer),
                    Err(_) => ValueExpressionInner::Float(
                        value.string(self.doc).unwrap().parse().unwrap(),
                    ),
                },
                BasicToken::Ident => ValueExpressionInner::Ident(value.string(self.doc).unwrap()),
                _ => unreachable!(),
            },
        }
    }

    pub fn parse_if_expr(&self, node: NodeRef) -> ValueExpression {
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
        ValueExpression {
            position,
            inner: ValueExpressionInner::If(Box::new(IfExpression {
                cond: self.parse_xor_expression(cond),
                if_true: self.parse_xor_expression(if_true),
                if_false: self.parse_xor_expression(if_false),
            })),
        }
    }

    pub fn parse_lambda(&self, node: NodeRef) -> ValueExpression {
        let position = self.get_node_position(&node);
        let Some(BasicNode::LambdaExpression { params, body, .. }) = node.deref(self.doc) else {
            unreachable!();
        };

        ValueExpression {
            position,
            inner: ValueExpressionInner::Lambda(Box::new(LambdaBody {
                params: params
                    .iter()
                    .map(|param| param.string(self.doc).unwrap())
                    .collect(),
                body: self.parse_xor_expression(*body),
            })),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionExpression<'code> {
    pub name: &'code str,
    pub position: PositionSpan,
    pub params: Vec<&'code str>,
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
    Binary(Box<BinaryExpression<'code>>),
    Unary(Box<UnaryExpression<'code>>),
    FuncCall(Box<FunctionCall<'code>>),
    Ident(&'code str),
    Bool(bool),
    Integer(i64),
    Float(f64),
    Lambda(Box<LambdaBody<'code>>),
    If(Box<IfExpression<'code>>),
}

#[derive(Clone, Debug)]
pub struct LambdaBody<'code> {
    pub params: Vec<&'code str>,
    pub body: ValueExpression<'code>,
}

#[derive(Clone, Debug)]
pub struct IfExpression<'code> {
    pub cond: ValueExpression<'code>,
    pub if_true: ValueExpression<'code>,
    pub if_false: ValueExpression<'code>,
}
