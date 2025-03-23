use lady_deirdre::{
    format::AnnotationPriority,
    lexis::{PositionSpan, SourceCode},
};

use crate::{
    cir::*,
    error_displayer::DisplayError,
    parser::{
        BinaryExpression, FunctionCall, FunctionExpression, IfExpression, LambdaBody,
        UnaryExpression, ValueExpression, ValueExpressionInner,
    },
};

#[derive(Debug)]
pub struct Compiler {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    NameNotFound(PositionSpan),
}

impl CompilationError {
    pub fn display<'a, C: SourceCode>(self, source: &'a C) -> DisplayError<'a, C> {
        let message;
        let annotations;
        match self {
            CompilationError::NameNotFound(position) => {
                message = format!("Cannot find symbol in this scope");
                annotations = vec![(
                    position,
                    AnnotationPriority::Default,
                    "Cannot find symbol in this scope".to_string(),
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

impl<'code> Compiler {
    pub fn new(functions: Vec<FunctionExpression<'code>>) -> Result<Self, CompilationError> {
        let compiler = Self {
            functions: vec![
                Function {
                    params_count: 0,
                    stack: vec![],
                    body: vec![]
                };
                functions.len()
            ],
        };
        compiler.compile_functions(functions)
    }

    fn compile_functions(
        mut self,
        functions: Vec<FunctionExpression<'code>>,
    ) -> Result<Self, CompilationError> {
        for (index, func) in functions.into_iter().enumerate() {
            self.functions[index] = self.compile_function(func.params.len(), &func.body)?;
        }
        Ok(self)
    }

    fn compile_function(
        &mut self,
        params_count: usize,
        body: &ValueExpression<'code>,
    ) -> Result<Function, CompilationError> {
        let (stack, body) = self.compile_expr(0, 0, body)?;

        Ok(Function {
            params_count,
            stack,
            body,
        })
    }

    fn compile_expr(
        &mut self,
        starting_stack_index: usize,
        starting_label_index: usize,
        expr: &ValueExpression<'code>,
    ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
        match &expr.inner {
            ValueExpressionInner::Binary(binary_expression) => self.compile_binary_expr(
                starting_stack_index,
                starting_label_index,
                binary_expression,
            ),
            ValueExpressionInner::Unary(unary_expression) => self.compile_unary_expr(
                starting_stack_index,
                starting_label_index,
                unary_expression,
            ),
            ValueExpressionInner::FuncCall(function_call) => {
                self.compile_func_call(starting_stack_index, starting_label_index, function_call)
            }
            ValueExpressionInner::Parameter(index) => {
                Ok((vec![StackValue::Parameter(*index)], vec![]))
            }
            ValueExpressionInner::Function(ident) => {
                Ok((vec![StackValue::Function(*ident)], vec![]))
            }
            ValueExpressionInner::Bool(bool) => Ok((vec![StackValue::Bool(*bool)], vec![])),
            ValueExpressionInner::None => Ok((vec![StackValue::None], vec![])),
            ValueExpressionInner::Integer(integer) => {
                Ok((vec![StackValue::Integer(*integer)], vec![]))
            }
            ValueExpressionInner::Float(float) => Ok((vec![StackValue::Float(*float)], vec![])),
            ValueExpressionInner::Lambda(lambda_body) => self.compile_lambda(lambda_body),
            ValueExpressionInner::If(if_expression) => {
                self.compile_if_expr(starting_stack_index, starting_label_index, if_expression)
            }
        }
    }

    fn compile_binary_expr(
        &mut self,
        starting_stack_index: usize,
        starting_label_index: usize,
        expr: &BinaryExpression<'code>,
    ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
        let mut stack = vec![];
        let mut actions = vec![];

        let lhs_index = starting_stack_index;

        let (mut lhs_stack, mut lhs_actions) =
            self.compile_expr(starting_stack_index, starting_label_index, &expr.lhs)?;
        let rhs_index = starting_stack_index + lhs_stack.len();
        let (mut rhs_stack, mut rhs_actions) = self.compile_expr(
            rhs_index,
            starting_label_index + lhs_actions.len(),
            &expr.rhs,
        )?;
        stack.append(&mut lhs_stack);
        stack.append(&mut rhs_stack);
        actions.append(&mut lhs_actions);
        actions.append(&mut rhs_actions);
        actions.push(Action::BinaryOperation {
            lhs: lhs_index,
            rhs: rhs_index,
            operator: expr.operator.clone(),
            lhs_position: expr.lhs.position.clone(),
            rhs_position: expr.rhs.position.clone(),
        });

        Ok((stack, actions))
    }

    fn compile_unary_expr(
        &mut self,
        starting_stack_index: usize,
        starting_label_index: usize,
        expr: &UnaryExpression<'code>,
    ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
        let index = starting_stack_index;

        let (stack, mut actions) =
            self.compile_expr(starting_stack_index, starting_label_index, &expr.operand)?;
        actions.push(Action::UnaryOperation {
            src: index,
            position: expr.operand.position.clone(),
            operator: expr.operator.clone(),
        });

        Ok((stack, actions))
    }

    fn compile_func_call(
        &mut self,
        starting_stack_index: usize,
        starting_label_index: usize,
        expr: &FunctionCall<'code>,
    ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
        let mut stack = vec![];
        let mut actions = vec![];

        let arg_index = starting_stack_index;

        let (mut arg_stack, mut arg_actions) =
            self.compile_expr(arg_index, starting_label_index, &expr.arg)?;
        let func_index = starting_stack_index + arg_stack.len();
        let (mut func_stack, mut func_actions) = self.compile_expr(
            starting_stack_index + arg_stack.len(),
            starting_label_index + arg_actions.len(),
            &expr.func,
        )?;
        stack.append(&mut arg_stack);
        actions.append(&mut arg_actions);
        stack.append(&mut func_stack);
        actions.append(&mut func_actions);
        actions.push(Action::BinaryOperation {
            lhs: arg_index,
            rhs: func_index,
            operator: BinaryOperator {
                position: Default::default(),
                variant: BinaryOperatorVariant::PipeOperator,
            },
            lhs_position: expr.arg.position.clone(),
            rhs_position: expr.func.position.clone(),
        });

        Ok((stack, actions))
    }

    fn compile_if_expr(
        &mut self,
        starting_stack_index: usize,
        starting_label_index: usize,
        expr: &IfExpression<'code>,
    ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
        // compile condition
        let (mut stack, mut actions) =
            self.compile_expr(starting_stack_index, starting_label_index, &expr.cond)?;
        // compile if_true with offset 1 (conditional jump)
        let (mut then_stack, mut then_actions) = self.compile_expr(
            starting_stack_index + stack.len(),
            starting_label_index + actions.len() + 1,
            &expr.if_true,
        )?;
        then_actions.push(Action::Copy {
            src: starting_stack_index + stack.len(),
            dist: starting_stack_index,
        });
        actions.push(Action::ConditionalJump {
            src: starting_stack_index,
            position: expr.cond.position.clone(),
            label: starting_label_index + actions.len() + then_actions.len() + 2,
        });
        stack.append(&mut then_stack);
        actions.append(&mut then_actions);
        drop(then_stack);
        drop(then_actions);
        // compile if_true with offset 1 (conditional jump)
        let (mut else_stack, mut else_actions) = self.compile_expr(
            starting_stack_index + stack.len(),
            starting_label_index + actions.len() + 1,
            &expr.if_false,
        )?;
        else_actions.push(Action::Copy {
            src: starting_stack_index + stack.len(),
            dist: starting_stack_index,
        });
        actions.push(Action::Goto(
            starting_label_index + actions.len() + 1 + else_actions.len(),
        ));
        stack.append(&mut else_stack);
        actions.append(&mut else_actions);

        Ok((stack, actions))
    }

    fn compile_lambda(
        &mut self,
        body: &LambdaBody<'code>,
    ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
        let index = self.functions.len();
        self.functions.push(Function {
            params_count: 0,
            stack: vec![],
            body: vec![],
        });
        self.functions[index] = self.compile_function(body.params.len(), &body.body)?;
        Ok((
            vec![StackValue::Function(FunctionIdentifier::Defined(index))],
            vec![],
        ))
    }
}
