use super::*;

pub struct Interpreter<'a> {
    pub functions: &'a Vec<Function>,
}

impl<'a> Interpreter<'a> {
    pub fn interpret_function(
        &self,
        function: usize,
        args: Vec<Value>,
    ) -> Result<Value, InterpretationError> {
        let function = &self.functions[function];
        if args.len() < function.params_count {
            return Err(InterpretationError::NotEnoughArguments {
                function: function.description.clone(),
                provided: args.len(),
                expected: function.params_count,
            });
        }
        let mut stack = function
            .stack
            .iter()
            .map(|start_value| match start_value {
                StackValue::Bool(bool) => Value::Bool(*bool),
                StackValue::Integer(int) => Value::Integer(*int),
                StackValue::Float(float) => Value::Float(*float),
                StackValue::Function(function_identifier) => Value::LambdaState(LambdaState {
                    function: *function_identifier,
                    provided_args: vec![],
                }),
                StackValue::Parameter(index) => args[*index].clone(),
                StackValue::None => Value::None,
            })
            .collect::<Vec<Value>>();
        let mut current_action = 0;
        while current_action < function.body.len() {
            current_action = match &function.body[current_action] {
                Action::UnaryOperation {
                    src,
                    position,
                    operator,
                } => self.run_unary_op(
                    current_action,
                    &mut stack,
                    *src,
                    position.clone(),
                    operator.clone(),
                )?,
                Action::BinaryOperation {
                    lhs,
                    rhs,
                    operator,
                    lhs_position,
                    rhs_position,
                } => self.run_binary_op(
                    current_action,
                    &mut stack,
                    *lhs,
                    *rhs,
                    operator.clone(),
                    lhs_position.clone(),
                    rhs_position.clone(),
                )?,
                Action::Copy { src, dist } => {
                    stack[*dist] = stack[*src].clone();
                    current_action + 1
                }
                Action::ConditionalJump {
                    src,
                    position,
                    label,
                } => self.run_conditional_jump(
                    current_action,
                    &mut stack,
                    *src,
                    position.clone(),
                    *label,
                )?,
                Action::Goto(new_action) => *new_action,
            }
        }
        Ok(stack[0].clone())
    }

    fn run_unary_op(
        &self,
        action_index: usize,
        stack: &mut Vec<Value>,
        src: usize,
        position: PositionSpan,
        operator: UnaryOperator,
    ) -> Result<usize, InterpretationError> {
        let mut value = std::mem::take(&mut stack[src]);

        macro_rules! mismatch_type {
            ($provided:ident, $expected:ident) => {
                return Err(InterpretationError::MismatchedType {
                    position,
                    provided: ValueType::$provided,
                    expected: ValueType::$expected,
                })
            };
        }

        match operator.variant {
            UnaryOperatorVariant::Inversion => match value {
                Value::None => {
                    mismatch_type!(None, Boolean);
                }
                Value::Integer(_) => {
                    mismatch_type!(Number, Boolean);
                }
                Value::Float(_) => {
                    mismatch_type!(Number, Boolean);
                }
                Value::Bool(bool) => {
                    value = Value::Bool(!bool);
                }
                Value::LambdaState(lambda_state) => match self.eval_lambda(lambda_state)? {
                    Value::None => {
                        mismatch_type!(None, Boolean);
                    }
                    Value::Integer(_) => {
                        mismatch_type!(Number, Boolean);
                    }
                    Value::Float(_) => {
                        mismatch_type!(Number, Boolean);
                    }
                    Value::Bool(bool) => {
                        value = Value::Bool(!bool);
                    }
                    Value::LambdaState(_) => mismatch_type!(Lambda, Boolean),
                },
            },
            UnaryOperatorVariant::Negation => match value {
                Value::None => {
                    mismatch_type!(None, Number);
                }
                Value::Integer(int) => {
                    value = Value::Integer(-int);
                }
                Value::Float(float) => {
                    value = Value::Float(-float);
                }
                Value::Bool(_) => {
                    mismatch_type!(Boolean, Number);
                }
                Value::LambdaState(lambda_state) => match self.eval_lambda(lambda_state)? {
                    Value::None => {
                        mismatch_type!(None, Number);
                    }
                    Value::Integer(int) => {
                        value = Value::Integer(-int);
                    }
                    Value::Float(float) => {
                        value = Value::Float(-float);
                    }
                    Value::Bool(_) => {
                        mismatch_type!(Boolean, Number);
                    }
                    Value::LambdaState(_) => mismatch_type!(Lambda, Number),
                },
            },
        }

        stack[src] = value;
        Ok(action_index + 1)
    }

    fn run_binary_op(
        &self,
        action_index: usize,
        stack: &mut Vec<Value>,
        lhs: usize,
        rhs: usize,
        operator: BinaryOperator,

        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
    ) -> Result<usize, InterpretationError> {
        let value = match operator.variant {
            BinaryOperatorVariant::Xor | BinaryOperatorVariant::Or | BinaryOperatorVariant::And => {
                self.bool_operator(
                    stack,
                    lhs,
                    rhs,
                    operator.clone(),
                    lhs_position,
                    rhs_position,
                )
            }
            BinaryOperatorVariant::Equals
            | BinaryOperatorVariant::NotEquals
            | BinaryOperatorVariant::Less
            | BinaryOperatorVariant::LessEqual
            | BinaryOperatorVariant::Greater
            | BinaryOperatorVariant::GreaterEqual => self.equality_operator(
                stack,
                lhs,
                rhs,
                operator.clone(),
                lhs_position,
                rhs_position,
            ),
            BinaryOperatorVariant::Addition
            | BinaryOperatorVariant::Substraction
            | BinaryOperatorVariant::Multiplication
            | BinaryOperatorVariant::Division => self.number_operator(
                stack,
                lhs,
                rhs,
                operator.clone(),
                lhs_position,
                rhs_position,
            ),
            BinaryOperatorVariant::PipeOperator => {
                self.pipe_operator(stack, lhs, rhs, rhs_position)
            }
        }?;

        stack[lhs] = value;

        Ok(action_index + 1)
    }

    fn bool_operator(
        &self,
        stack: &mut Vec<Value>,
        lhs: usize,
        rhs: usize,
        operator: BinaryOperator,
        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
    ) -> Result<Value, InterpretationError> {
        let lhs_value = match stack[lhs].clone() {
            Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
            value => value,
        };
        let rhs_value = match stack[rhs].clone() {
            Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
            value => value,
        };
        if let Value::None = rhs_value {
            return Ok(lhs_value);
        }
        let lhs_value = match lhs_value {
            Value::None => return Ok(rhs_value),
            Value::Integer(_) | Value::Float(_) => {
                return Err(InterpretationError::MismatchedType {
                    position: lhs_position,
                    provided: ValueType::Number,
                    expected: ValueType::Boolean,
                })
            }
            Value::Bool(bool) => bool,
            Value::LambdaState(_) => {
                return Err(InterpretationError::MismatchedType {
                    position: lhs_position,
                    provided: ValueType::Lambda,
                    expected: ValueType::Boolean,
                })
            }
        };
        let rhs_value = match rhs_value {
            Value::Integer(_) | Value::Float(_) => {
                return Err(InterpretationError::MismatchedType {
                    position: rhs_position,
                    provided: ValueType::Number,
                    expected: ValueType::Boolean,
                })
            }
            Value::Bool(bool) => bool,
            _ => {
                return Err(InterpretationError::MismatchedType {
                    position: rhs_position,
                    provided: ValueType::Lambda,
                    expected: ValueType::Boolean,
                })
            }
        };
        Ok(Value::Bool(match operator.variant {
            BinaryOperatorVariant::Xor => lhs_value ^ rhs_value,
            BinaryOperatorVariant::Or => lhs_value || rhs_value,
            BinaryOperatorVariant::And => lhs_value && rhs_value,
            _ => unreachable!(),
        }))
    }

    fn equality_operator(
        &self,
        stack: &mut Vec<Value>,
        lhs: usize,
        rhs: usize,
        operator: BinaryOperator,
        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
    ) -> Result<Value, InterpretationError> {
        let lhs_value = match stack[lhs].clone() {
            Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
            value => value,
        };
        let rhs_value = match stack[rhs].clone() {
            Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
            value => value,
        };

        #[derive(PartialEq, Clone, Copy)]
        enum ExprValue {
            None,
            Bool(bool),
            Float(f64),
        }

        let lhs_value = match lhs_value {
            Value::None => ExprValue::None,
            Value::Integer(int) => ExprValue::Float(int as f64),
            Value::Float(float) => ExprValue::Float(float),
            Value::Bool(bool) => ExprValue::Bool(bool),
            Value::LambdaState(_) => {
                return Err(InterpretationError::CannotUseLambdaInExpression(
                    lhs_position,
                ))
            }
        };
        let rhs_value = match rhs_value {
            Value::None => ExprValue::None,
            Value::Integer(int) => ExprValue::Float(int as f64),
            Value::Float(float) => ExprValue::Float(float),
            Value::Bool(bool) => ExprValue::Bool(bool),
            Value::LambdaState(_) => {
                return Err(InterpretationError::CannotUseLambdaInExpression(
                    rhs_position,
                ))
            }
        };
        Ok(Value::Bool(match operator.variant {
            BinaryOperatorVariant::Equals => lhs_value == rhs_value,
            BinaryOperatorVariant::NotEquals => lhs_value != rhs_value,
            BinaryOperatorVariant::Less
            | BinaryOperatorVariant::LessEqual
            | BinaryOperatorVariant::Greater
            | BinaryOperatorVariant::GreaterEqual => {
                let lhs_value = match lhs_value {
                    ExprValue::None => {
                        return Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::None,
                            expected: ValueType::Number,
                        });
                    }
                    ExprValue::Bool(_) => {
                        return Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::Boolean,
                            expected: ValueType::Number,
                        });
                    }
                    ExprValue::Float(float) => float,
                };
                let rhs_value = match rhs_value {
                    ExprValue::None => {
                        return Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::None,
                            expected: ValueType::Number,
                        });
                    }
                    ExprValue::Bool(_) => {
                        return Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::Boolean,
                            expected: ValueType::Number,
                        });
                    }
                    ExprValue::Float(float) => float,
                };
                match operator.variant {
                    BinaryOperatorVariant::Less => lhs_value < rhs_value,
                    BinaryOperatorVariant::LessEqual => lhs_value <= rhs_value,
                    BinaryOperatorVariant::Greater => lhs_value > rhs_value,
                    BinaryOperatorVariant::GreaterEqual => lhs_value >= rhs_value,
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }))
    }

    fn number_operator(
        &self,
        stack: &mut Vec<Value>,
        lhs: usize,
        rhs: usize,
        operator: BinaryOperator,
        lhs_position: PositionSpan,
        rhs_position: PositionSpan,
    ) -> Result<Value, InterpretationError> {
        let lhs_value = match stack[lhs].clone() {
            Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
            value => value,
        };
        let rhs_value = match stack[rhs].clone() {
            Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
            value => value,
        };

        macro_rules! matcher {
            ($lhs:expr, $rhs:expr) => {
                match operator.variant {
                    BinaryOperatorVariant::Addition => $lhs + $rhs,
                    BinaryOperatorVariant::Substraction => $lhs - $rhs,
                    BinaryOperatorVariant::Multiplication => $lhs * $rhs,
                    BinaryOperatorVariant::Division => $lhs / $rhs,
                    _ => unreachable!(),
                }
            };
        }

        match lhs_value {
            Value::None => Err(InterpretationError::MismatchedType {
                position: rhs_position,
                provided: ValueType::None,
                expected: ValueType::Number,
            }),
            Value::Bool(_) => Err(InterpretationError::MismatchedType {
                position: rhs_position,
                provided: ValueType::None,
                expected: ValueType::Number,
            }),
            Value::LambdaState(_) => Err(InterpretationError::CannotUseLambdaInExpression(
                lhs_position,
            )),
            Value::Integer(lhs) => match rhs_value {
                Value::None => Err(InterpretationError::MismatchedType {
                    position: rhs_position,
                    provided: ValueType::None,
                    expected: ValueType::Number,
                }),
                Value::Bool(_) => Err(InterpretationError::MismatchedType {
                    position: rhs_position,
                    provided: ValueType::Boolean,
                    expected: ValueType::Number,
                }),
                Value::LambdaState(_) => Err(InterpretationError::CannotUseLambdaInExpression(
                    rhs_position,
                )),
                Value::Integer(rhs) => Ok(Value::Integer(matcher!(lhs, rhs))),
                Value::Float(rhs) => Ok(Value::Float(matcher!(lhs as f64, rhs))),
            },
            Value::Float(lhs) => match rhs_value {
                Value::None => Err(InterpretationError::MismatchedType {
                    position: rhs_position,
                    provided: ValueType::None,
                    expected: ValueType::Number,
                }),
                Value::Bool(_) => Err(InterpretationError::MismatchedType {
                    position: rhs_position,
                    provided: ValueType::Boolean,
                    expected: ValueType::Number,
                }),
                Value::LambdaState(_) => Err(InterpretationError::CannotUseLambdaInExpression(
                    rhs_position,
                )),
                Value::Integer(rhs) => Ok(Value::Float(matcher!(lhs, rhs as f64))),
                Value::Float(rhs) => Ok(Value::Float(matcher!(lhs, rhs))),
            },
        }
    }

    fn pipe_operator(
        &self,
        stack: &mut Vec<Value>,
        lhs: usize,
        rhs: usize,
        rhs_position: PositionSpan,
    ) -> Result<Value, InterpretationError> {
        let lhs_value = std::mem::take(&mut stack[lhs]);
        let rhs_value = std::mem::take(&mut stack[rhs]);
        match rhs_value {
            Value::LambdaState(mut lambda) => {
                lambda.provided_args.push(lhs_value);
                Ok(Value::LambdaState(lambda))
            }
            value => Err(InterpretationError::MismatchedType {
                position: rhs_position,
                provided: match value {
                    Value::Bool(_) => ValueType::Boolean,
                    Value::Integer(_) | Value::Float(_) => ValueType::Number,
                    Value::None => ValueType::None,
                    _ => unreachable!(),
                },
                expected: ValueType::Lambda,
            }),
        }
    }

    fn run_conditional_jump(
        &self,
        action_index: usize,
        stack: &mut Vec<Value>,
        src: usize,
        position: PositionSpan,
        label: usize,
    ) -> Result<usize, InterpretationError> {
        let value = std::mem::take(&mut stack[src]);

        macro_rules! mismatch_type {
            ($provided:ident, $expected:ident) => {
                Err(InterpretationError::MismatchedType {
                    position,
                    provided: ValueType::$provided,
                    expected: ValueType::$expected,
                })
            };
        }
        match value {
            Value::None => {
                mismatch_type!(None, Boolean)
            }
            Value::Integer(_) | Value::Float(_) => {
                mismatch_type!(Number, Boolean)
            }
            Value::Bool(bool) => {
                if bool {
                    Ok(action_index + 1)
                } else {
                    Ok(label)
                }
            }
            Value::LambdaState(lambda_state) => match self.eval_lambda(lambda_state)? {
                Value::None => {
                    mismatch_type!(None, Boolean)
                }
                Value::Integer(_) | Value::Float(_) => {
                    mismatch_type!(Number, Boolean)
                }
                Value::Bool(bool) => {
                    if bool {
                        Ok(action_index + 1)
                    } else {
                        Ok(label)
                    }
                }
                Value::LambdaState(_) => mismatch_type!(Lambda, Boolean),
            },
        }
    }

    pub fn eval_lambda(&self, lambda: LambdaState) -> Result<Value, InterpretationError> {
        match lambda.function {
            FunctionIdentifier::Defined(index) => {
                let arg_count = self.functions[index].params_count;
                if arg_count > lambda.provided_args.len() {
                    return Ok(Value::LambdaState(lambda));
                }
                let mut args = Vec::with_capacity(arg_count);
                let mut remains = Vec::with_capacity(lambda.provided_args.len() - arg_count);
                for i in 0..arg_count {
                    args.push(lambda.provided_args[i].clone());
                }
                for i in arg_count..lambda.provided_args.len() {
                    remains.push(lambda.provided_args[i].clone());
                }
                let value = self.interpret_function(index, args)?;
                match value {
                    Value::LambdaState(lambda) => self.eval_lambda(LambdaState {
                        function: lambda.function,
                        provided_args: {
                            let mut args = lambda.provided_args;
                            args.append(&mut remains);
                            args
                        },
                    }),
                    value => {
                        if remains.is_empty() {
                            Ok(value)
                        } else {
                            Err(InterpretationError::TooMuchArguments {
                                function: self.functions[index].description.clone(),
                                expected: arg_count,
                                provided: lambda.provided_args.len(),
                            })
                        }
                    }
                }
            }
            FunctionIdentifier::BuiltIn(built_in_function) => {
                self.built_in_function(built_in_function, lambda.provided_args)
            }
        }
    }

    fn built_in_function(
        &self,
        function: BuiltInFunction,
        params: Vec<Value>,
    ) -> Result<Value, InterpretationError> {
        match function {
            BuiltInFunction::Print => self.print(params),
        }
    }

    fn print(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
        for param in params {
            let value = match param {
                Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state)?,
                value => value,
            };
            println!("{:#?}", value);
        }
        Ok(Value::None)
    }
}
