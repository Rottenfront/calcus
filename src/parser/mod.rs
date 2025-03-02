pub mod lexer;
pub mod syntax;

use lady_deirdre::{syntax::NodeRef, units::Document};
use lexer::BasicToken;
use syntax::BasicNode;

use crate::interpretator::ast::*;

pub struct Parser<'a> {
    doc: &'a Document<BasicNode>,
}

impl<'a> Parser<'a> {
    pub fn new(doc: &'a Document<BasicNode>) -> Self {
        Self { doc }
    }

    pub fn parse_function(&self, node: NodeRef) -> Function {
        let Some(BasicNode::Function {
            name,
            parameters,
            body,
            local_functions,
            ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let name = name.string(self.doc).unwrap().to_owned();
        let parameters = parameters
            .iter()
            .map(|param| param.string(self.doc).unwrap().to_owned())
            .collect();
        let body = self.parse_expressions(*body);
        let local_functions = local_functions
            .iter()
            .map(|func| self.parse_function(*func))
            .collect();

        Function {
            name,
            parameters,
            body,
            local_functions,
        }
    }

    pub fn parse_expressions(&self, node: NodeRef) -> Expressions {
        let Some(BasicNode::Expr { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let values = values
            .iter()
            .map(|val| self.parse_xor_expression(*val))
            .collect();
        Expressions { values }
    }

    pub fn parse_xor_expression(&self, node: NodeRef) -> XorExpression {
        let Some(BasicNode::XorExpr { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let values = values
            .iter()
            .map(|val| self.parse_or_expression(*val))
            .collect();
        XorExpression { values }
    }

    pub fn parse_or_expression(&self, node: NodeRef) -> OrExpression {
        let Some(BasicNode::OrExpr { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let values = values
            .iter()
            .map(|val| self.parse_and_expression(*val))
            .collect();
        OrExpression { values }
    }

    pub fn parse_and_expression(&self, node: NodeRef) -> AndExpression {
        let Some(BasicNode::AndExpr { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let values = values
            .iter()
            .map(|val| self.parse_eq_expression(*val))
            .collect();
        AndExpression { values }
    }

    pub fn parse_eq_expression(&self, node: NodeRef) -> EqualityExpression {
        let Some(BasicNode::EqualityExpr {
            lvalue,
            rvalue,
            operator,
            ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let lvalue = self.parse_sum_expression(*lvalue);
        let rvalue = if let Some(operator) = operator.deref(self.doc) {
            let operator = match operator {
                BasicToken::Equal => EqualityOperator::Equal,
                BasicToken::NotEqual => EqualityOperator::NotEqual,
                BasicToken::Greater => EqualityOperator::Greater,
                BasicToken::Less => EqualityOperator::Less,
                BasicToken::GreaterEqual => EqualityOperator::GreaterEqual,
                BasicToken::LessEqual => EqualityOperator::LessEqual,
                _ => unreachable!(),
            };
            let rvalue = self.parse_sum_expression(*rvalue);
            Some((operator, rvalue))
        } else {
            None
        };

        EqualityExpression { lvalue, rvalue }
    }

    pub fn parse_sum_expression(&self, node: NodeRef) -> SumExpression {
        let Some(BasicNode::SumExpr {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut values = values
            .iter()
            .map(|val| self.parse_mult_expression(*val))
            .collect::<Vec<MultExpression>>();
        let mut operators = operators
            .iter()
            .map(|operator| match operator.deref(self.doc) {
                Some(BasicToken::Plus) => SumOperator::Sum,
                Some(BasicToken::Minus) => SumOperator::Sub,
                _ => unreachable!(),
            })
            .collect::<Vec<SumOperator>>();
        let mut rvalues = vec![];
        while let Some(operator) = operators.pop() {
            let rvalue = values.pop().unwrap();
            rvalues.push((operator, rvalue));
        }
        rvalues.reverse();

        SumExpression {
            lvalue: values[0].clone(),
            rvalues,
        }
    }

    pub fn parse_mult_expression(&self, node: NodeRef) -> MultExpression {
        let Some(BasicNode::MultExpr {
            values, operators, ..
        }) = node.deref(self.doc)
        else {
            unreachable!();
        };
        let mut values = values
            .iter()
            .map(|val| self.parse_pipe_expression(*val))
            .collect::<Vec<PipeExpression>>();
        let mut operators = operators
            .iter()
            .map(|operator| match operator.deref(self.doc) {
                Some(BasicToken::Star) => MultOperator::Multiply,
                Some(BasicToken::Slash) => MultOperator::Divide,
                _ => unreachable!(),
            })
            .collect::<Vec<MultOperator>>();
        let mut rvalues = vec![];
        while let Some(operator) = operators.pop() {
            let rvalue = values.pop().unwrap();
            rvalues.push((operator, rvalue));
        }
        rvalues.reverse();

        MultExpression {
            lvalue: values[0].clone(),
            rvalues,
        }
    }

    pub fn parse_pipe_expression(&self, node: NodeRef) -> PipeExpression {
        let Some(BasicNode::PipeExpr { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let values = values
            .iter()
            .map(|val| self.parse_func_call(*val))
            .collect();
        PipeExpression { values }
    }

    pub fn parse_func_call(&self, node: NodeRef) -> FunctionCall {
        let Some(BasicNode::FuncCall { values, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let args = values
            .iter()
            .map(|val| self.parse_unary_expr(*val))
            .collect();
        FunctionCall { args }
    }

    pub fn parse_unary_expr(&self, node: NodeRef) -> UnaryExpression {
        let Some(BasicNode::UnaryExpr { op, value, .. }) = node.deref(self.doc) else {
            unreachable!();
        };
        let value = {
            match value.deref(self.doc) {
                Some(BasicNode::Literal { value, .. }) => match value.deref(self.doc) {
                    Some(BasicToken::Ident) => {
                        BasicExpression::Ident(value.string(self.doc).unwrap().to_string())
                    }
                    Some(BasicToken::Number) => {
                        BasicExpression::Number(value.string(self.doc).unwrap().parse().unwrap())
                    }
                    Some(BasicToken::KwTrue) => BasicExpression::Boolean(true),
                    Some(BasicToken::KwFalse) => BasicExpression::Boolean(false),
                    _ => unreachable!(),
                },
                Some(BasicNode::ParenthesesExpr { value, .. }) => {
                    BasicExpression::Parenthesized(self.parse_expressions(*value))
                }
                Some(BasicNode::LambdaExpr { params, body, .. }) => {
                    BasicExpression::Lambda(Lambda {
                        params: params
                            .iter()
                            .map(|param| param.string(self.doc).unwrap().to_string())
                            .collect(),
                        body: self.parse_expressions(*body),
                    })
                }
                Some(BasicNode::CaseExpr {
                    expr,
                    cases,
                    results,
                    default_expr,
                    ..
                }) => {
                    let arg = self.parse_expressions(*expr);
                    let cases = cases.iter().map(|expr| self.parse_expressions(*expr));
                    let results = results.iter().map(|expr| self.parse_expressions(*expr));
                    let default = if default_expr.is_valid_ref(self.doc) {
                        Some(self.parse_expressions(*default_expr))
                    } else {
                        None
                    };

                    BasicExpression::Case(Case {
                        arg,
                        body: cases
                            .zip(results)
                            .collect::<Vec<(Expressions, Expressions)>>(),
                        default,
                    })
                }
                _ => unreachable!(),
            }
        };
        let operator = op.deref(self.doc).map(|op| match op {
            BasicToken::Exclamation => UnaryOperator::Not,
            BasicToken::Minus => UnaryOperator::Negative,
            _ => unreachable!(),
        });
        UnaryExpression { operator, value }
    }
}
