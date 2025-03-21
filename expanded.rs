#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use std::{path::PathBuf, time::Instant};
use cir::interpreter::Interpreter;
use compiler::Compiler;
use parser::FunctionExpression;
pub mod cir {
    pub mod error {
        use lady_deirdre::{
            format::AnnotationPriority, lexis::{PositionSpan, SourceCode},
        };
        use crate::error_displayer::DisplayError;
        use super::{
            std_funcs, type_checker::ValueType, BinaryOperator, FunctionDescription,
        };
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
            InvalidArgumentCountBuiltInFunction {
                function: std_funcs::BuiltInFunction,
                provided: usize,
                expected: usize,
            },
            InvalidArgumentTypeBuiltInFunction {
                function: std_funcs::BuiltInFunction,
                provided: ValueType,
                expected: ValueType,
            },
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for InterpretationError {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match self {
                    InterpretationError::MismatchedType {
                        position: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field3_finish(
                            f,
                            "MismatchedType",
                            "position",
                            __self_0,
                            "provided",
                            __self_1,
                            "expected",
                            &__self_2,
                        )
                    }
                    InterpretationError::CannotUseLambdaInExpression(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "CannotUseLambdaInExpression",
                            &__self_0,
                        )
                    }
                    InterpretationError::MismatchedTypes {
                        lhs_position: __self_0,
                        rhs_position: __self_1,
                        operator: __self_2,
                        type_left: __self_3,
                        type_right: __self_4,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field5_finish(
                            f,
                            "MismatchedTypes",
                            "lhs_position",
                            __self_0,
                            "rhs_position",
                            __self_1,
                            "operator",
                            __self_2,
                            "type_left",
                            __self_3,
                            "type_right",
                            &__self_4,
                        )
                    }
                    InterpretationError::NotEnoughArguments {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field3_finish(
                            f,
                            "NotEnoughArguments",
                            "function",
                            __self_0,
                            "provided",
                            __self_1,
                            "expected",
                            &__self_2,
                        )
                    }
                    InterpretationError::TooMuchArguments {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field3_finish(
                            f,
                            "TooMuchArguments",
                            "function",
                            __self_0,
                            "provided",
                            __self_1,
                            "expected",
                            &__self_2,
                        )
                    }
                    InterpretationError::InvalidArgumentCountBuiltInFunction {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field3_finish(
                            f,
                            "InvalidArgumentCountBuiltInFunction",
                            "function",
                            __self_0,
                            "provided",
                            __self_1,
                            "expected",
                            &__self_2,
                        )
                    }
                    InterpretationError::InvalidArgumentTypeBuiltInFunction {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field3_finish(
                            f,
                            "InvalidArgumentTypeBuiltInFunction",
                            "function",
                            __self_0,
                            "provided",
                            __self_1,
                            "expected",
                            &__self_2,
                        )
                    }
                }
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for InterpretationError {
            #[inline]
            fn clone(&self) -> InterpretationError {
                match self {
                    InterpretationError::MismatchedType {
                        position: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        InterpretationError::MismatchedType {
                            position: ::core::clone::Clone::clone(__self_0),
                            provided: ::core::clone::Clone::clone(__self_1),
                            expected: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                    InterpretationError::CannotUseLambdaInExpression(__self_0) => {
                        InterpretationError::CannotUseLambdaInExpression(
                            ::core::clone::Clone::clone(__self_0),
                        )
                    }
                    InterpretationError::MismatchedTypes {
                        lhs_position: __self_0,
                        rhs_position: __self_1,
                        operator: __self_2,
                        type_left: __self_3,
                        type_right: __self_4,
                    } => {
                        InterpretationError::MismatchedTypes {
                            lhs_position: ::core::clone::Clone::clone(__self_0),
                            rhs_position: ::core::clone::Clone::clone(__self_1),
                            operator: ::core::clone::Clone::clone(__self_2),
                            type_left: ::core::clone::Clone::clone(__self_3),
                            type_right: ::core::clone::Clone::clone(__self_4),
                        }
                    }
                    InterpretationError::NotEnoughArguments {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        InterpretationError::NotEnoughArguments {
                            function: ::core::clone::Clone::clone(__self_0),
                            provided: ::core::clone::Clone::clone(__self_1),
                            expected: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                    InterpretationError::TooMuchArguments {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        InterpretationError::TooMuchArguments {
                            function: ::core::clone::Clone::clone(__self_0),
                            provided: ::core::clone::Clone::clone(__self_1),
                            expected: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                    InterpretationError::InvalidArgumentCountBuiltInFunction {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        InterpretationError::InvalidArgumentCountBuiltInFunction {
                            function: ::core::clone::Clone::clone(__self_0),
                            provided: ::core::clone::Clone::clone(__self_1),
                            expected: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                    InterpretationError::InvalidArgumentTypeBuiltInFunction {
                        function: __self_0,
                        provided: __self_1,
                        expected: __self_2,
                    } => {
                        InterpretationError::InvalidArgumentTypeBuiltInFunction {
                            function: ::core::clone::Clone::clone(__self_0),
                            provided: ::core::clone::Clone::clone(__self_1),
                            expected: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                }
            }
        }
        impl InterpretationError {
            pub fn display<'a, C: SourceCode>(
                self,
                source: &'a C,
            ) -> DisplayError<'a, C> {
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
                        message = ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "Type mismatch, expected: {0}, provided: {1}",
                                    expected,
                                    provided,
                                ),
                            );
                            res
                        });
                        annotations = <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([
                                (
                                    position,
                                    AnnotationPriority::Default,
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!(
                                                "Expected: {0}, provided: {1}",
                                                expected,
                                                provided,
                                            ),
                                        );
                                        res
                                    }),
                                ),
                            ]),
                        );
                    }
                    InterpretationError::CannotUseLambdaInExpression(range) => {
                        message = "Cannot use lambda in expression with binary operator"
                            .to_string();
                        annotations = <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([
                                (
                                    range,
                                    AnnotationPriority::Default,
                                    "This expression".to_string(),
                                ),
                            ]),
                        );
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
                        message = ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "Type mismatch, left: {0}, right: {1}",
                                    type_left,
                                    type_right,
                                ),
                            );
                            res
                        });
                        annotations = <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([
                                (
                                    lhs_position,
                                    AnnotationPriority::Default,
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!("Type: {0}", type_left),
                                        );
                                        res
                                    }),
                                ),
                                (
                                    rhs_position,
                                    AnnotationPriority::Default,
                                    ::alloc::__export::must_use({
                                        let res = ::alloc::fmt::format(
                                            format_args!("Type: {0}", type_right),
                                        );
                                        res
                                    }),
                                ),
                            ]),
                        );
                    }
                    InterpretationError::NotEnoughArguments {
                        function,
                        provided,
                        expected,
                    } => {
                        message = ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "Not enough arguments to the function\nExpected: {0}, provided: {1}",
                                    expected,
                                    provided,
                                ),
                            );
                            res
                        });
                        annotations = <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([
                                (
                                    function.position,
                                    AnnotationPriority::Default,
                                    "This function".to_string(),
                                ),
                            ]),
                        );
                    }
                    InterpretationError::TooMuchArguments {
                        function,
                        provided,
                        expected,
                    } => {
                        message = ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "Too much arguments to the function\nExpected: {0}, provided: {1}",
                                    expected,
                                    provided,
                                ),
                            );
                            res
                        });
                        annotations = <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([
                                (
                                    function.position,
                                    AnnotationPriority::Default,
                                    "This function".to_string(),
                                ),
                            ]),
                        );
                    }
                    InterpretationError::InvalidArgumentCountBuiltInFunction {
                        function,
                        provided,
                        expected,
                    } => {
                        message = ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "Invalid function count for function {0}\nExpected: {1}, provided: {2}",
                                    std_funcs::get_name(function),
                                    expected,
                                    provided,
                                ),
                            );
                            res
                        });
                        annotations = ::alloc::vec::Vec::new();
                    }
                    InterpretationError::InvalidArgumentTypeBuiltInFunction {
                        function,
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
                        message = ::alloc::__export::must_use({
                            let res = ::alloc::fmt::format(
                                format_args!(
                                    "Invalid function argument for function {0}\nExpected: {1}, provided: {2}",
                                    std_funcs::get_name(function),
                                    expected,
                                    provided,
                                ),
                            );
                            res
                        });
                        annotations = ::alloc::vec::Vec::new();
                    }
                }
                DisplayError {
                    source,
                    message,
                    annotations,
                }
            }
        }
    }
    pub mod interpreter {
        use std_funcs::{get_arg_count, BuiltInFunction};
        use type_checker::ValueType;
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
                        StackValue::Function(function_identifier) => {
                            Value::LambdaState(LambdaState {
                                function: *function_identifier,
                                provided_args: ::alloc::vec::Vec::new(),
                            })
                        }
                        StackValue::Parameter(index) => args[*index].clone(),
                        StackValue::None => Value::None,
                    })
                    .collect::<Vec<Value>>();
                let mut current_action = 0;
                while current_action < function.body.len() {
                    current_action = match &function.body[current_action] {
                        Action::UnaryOperation { src, position, operator } => {
                            self.run_unary_op(
                                current_action,
                                &mut stack,
                                *src,
                                position.clone(),
                                operator.clone(),
                            )?
                        }
                        Action::BinaryOperation {
                            lhs,
                            rhs,
                            operator,
                            lhs_position,
                            rhs_position,
                        } => {
                            self.run_binary_op(
                                current_action,
                                &mut stack,
                                *lhs,
                                *rhs,
                                operator.clone(),
                                lhs_position.clone(),
                                rhs_position.clone(),
                            )?
                        }
                        Action::Copy { src, dist } => {
                            stack[*dist] = stack[*src].clone();
                            current_action + 1
                        }
                        Action::ConditionalJump { src, position, label } => {
                            self.run_conditional_jump(
                                current_action,
                                &mut stack,
                                *src,
                                position.clone(),
                                *label,
                            )?
                        }
                        Action::Goto(new_action) => *new_action,
                    };
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
                match operator.variant {
                    UnaryOperatorVariant::Inversion => {
                        match value {
                            Value::None => {
                                return Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::None,
                                    expected: ValueType::Boolean,
                                });
                            }
                            Value::Integer(_) => {
                                return Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::Number,
                                    expected: ValueType::Boolean,
                                });
                            }
                            Value::Float(_) => {
                                return Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::Number,
                                    expected: ValueType::Boolean,
                                });
                            }
                            Value::Bool(bool) => {
                                value = Value::Bool(!bool);
                            }
                            Value::LambdaState(lambda_state) => {
                                match self.eval_lambda(lambda_state)? {
                                    Value::None => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::None,
                                            expected: ValueType::Boolean,
                                        });
                                    }
                                    Value::Integer(_) => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::Number,
                                            expected: ValueType::Boolean,
                                        });
                                    }
                                    Value::Float(_) => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::Number,
                                            expected: ValueType::Boolean,
                                        });
                                    }
                                    Value::Bool(bool) => {
                                        value = Value::Bool(!bool);
                                    }
                                    Value::LambdaState(_) => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::Lambda,
                                            expected: ValueType::Boolean,
                                        });
                                    }
                                }
                            }
                        }
                    }
                    UnaryOperatorVariant::Negation => {
                        match value {
                            Value::None => {
                                return Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::None,
                                    expected: ValueType::Number,
                                });
                            }
                            Value::Integer(int) => {
                                value = Value::Integer(-int);
                            }
                            Value::Float(float) => {
                                value = Value::Float(-float);
                            }
                            Value::Bool(_) => {
                                return Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::Boolean,
                                    expected: ValueType::Number,
                                });
                            }
                            Value::LambdaState(lambda_state) => {
                                match self.eval_lambda(lambda_state)? {
                                    Value::None => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::None,
                                            expected: ValueType::Number,
                                        });
                                    }
                                    Value::Integer(int) => {
                                        value = Value::Integer(-int);
                                    }
                                    Value::Float(float) => {
                                        value = Value::Float(-float);
                                    }
                                    Value::Bool(_) => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::Boolean,
                                            expected: ValueType::Number,
                                        });
                                    }
                                    Value::LambdaState(_) => {
                                        return Err(InterpretationError::MismatchedType {
                                            position,
                                            provided: ValueType::Lambda,
                                            expected: ValueType::Number,
                                        });
                                    }
                                }
                            }
                        }
                    }
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
                    BinaryOperatorVariant::Xor
                    | BinaryOperatorVariant::Or
                    | BinaryOperatorVariant::And => {
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
                    | BinaryOperatorVariant::GreaterEqual => {
                        self.equality_operator(
                            stack,
                            lhs,
                            rhs,
                            operator.clone(),
                            lhs_position,
                            rhs_position,
                        )
                    }
                    BinaryOperatorVariant::Addition
                    | BinaryOperatorVariant::Substraction
                    | BinaryOperatorVariant::Multiplication
                    | BinaryOperatorVariant::Division => {
                        self.number_operator(
                            stack,
                            lhs,
                            rhs,
                            operator.clone(),
                            lhs_position,
                            rhs_position,
                        )
                    }
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
                let lhs_value = self.evaluate(stack[lhs].clone())?;
                let rhs_value = self.evaluate(stack[rhs].clone())?;
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
                        });
                    }
                    Value::Bool(bool) => bool,
                    Value::LambdaState(_) => {
                        return Err(InterpretationError::MismatchedType {
                            position: lhs_position,
                            provided: ValueType::Lambda,
                            expected: ValueType::Boolean,
                        });
                    }
                };
                let rhs_value = match rhs_value {
                    Value::Integer(_) | Value::Float(_) => {
                        return Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::Number,
                            expected: ValueType::Boolean,
                        });
                    }
                    Value::Bool(bool) => bool,
                    _ => {
                        return Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::Lambda,
                            expected: ValueType::Boolean,
                        });
                    }
                };
                Ok(
                    Value::Bool(
                        match operator.variant {
                            BinaryOperatorVariant::Xor => lhs_value ^ rhs_value,
                            BinaryOperatorVariant::Or => lhs_value || rhs_value,
                            BinaryOperatorVariant::And => lhs_value && rhs_value,
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        },
                    ),
                )
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
                let lhs_value = self.evaluate(stack[lhs].clone())?;
                let rhs_value = self.evaluate(stack[rhs].clone())?;
                enum ExprValue {
                    None,
                    Bool(bool),
                    Float(f64),
                }
                #[automatically_derived]
                impl ::core::marker::StructuralPartialEq for ExprValue {}
                #[automatically_derived]
                impl ::core::cmp::PartialEq for ExprValue {
                    #[inline]
                    fn eq(&self, other: &ExprValue) -> bool {
                        let __self_discr = ::core::intrinsics::discriminant_value(self);
                        let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                        __self_discr == __arg1_discr
                            && match (self, other) {
                                (ExprValue::Bool(__self_0), ExprValue::Bool(__arg1_0)) => {
                                    __self_0 == __arg1_0
                                }
                                (ExprValue::Float(__self_0), ExprValue::Float(__arg1_0)) => {
                                    __self_0 == __arg1_0
                                }
                                _ => true,
                            }
                    }
                }
                #[automatically_derived]
                impl ::core::clone::Clone for ExprValue {
                    #[inline]
                    fn clone(&self) -> ExprValue {
                        let _: ::core::clone::AssertParamIsClone<bool>;
                        let _: ::core::clone::AssertParamIsClone<f64>;
                        *self
                    }
                }
                #[automatically_derived]
                impl ::core::marker::Copy for ExprValue {}
                let lhs_value = match lhs_value {
                    Value::None => ExprValue::None,
                    Value::Integer(int) => ExprValue::Float(int as f64),
                    Value::Float(float) => ExprValue::Float(float),
                    Value::Bool(bool) => ExprValue::Bool(bool),
                    Value::LambdaState(_) => {
                        return Err(
                            InterpretationError::CannotUseLambdaInExpression(
                                lhs_position,
                            ),
                        );
                    }
                };
                let rhs_value = match rhs_value {
                    Value::None => ExprValue::None,
                    Value::Integer(int) => ExprValue::Float(int as f64),
                    Value::Float(float) => ExprValue::Float(float),
                    Value::Bool(bool) => ExprValue::Bool(bool),
                    Value::LambdaState(_) => {
                        return Err(
                            InterpretationError::CannotUseLambdaInExpression(
                                rhs_position,
                            ),
                        );
                    }
                };
                Ok(
                    Value::Bool(
                        match operator.variant {
                            BinaryOperatorVariant::Equals => lhs_value == rhs_value,
                            BinaryOperatorVariant::NotEquals => lhs_value != rhs_value,
                            BinaryOperatorVariant::Less
                            | BinaryOperatorVariant::LessEqual
                            | BinaryOperatorVariant::Greater
                            | BinaryOperatorVariant::GreaterEqual => {
                                let lhs_value = match lhs_value {
                                    ExprValue::None => {
                                        return Err(InterpretationError::MismatchedType {
                                            position: lhs_position,
                                            provided: ValueType::None,
                                            expected: ValueType::Number,
                                        });
                                    }
                                    ExprValue::Bool(_) => {
                                        return Err(InterpretationError::MismatchedType {
                                            position: lhs_position,
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
                                    BinaryOperatorVariant::GreaterEqual => {
                                        lhs_value >= rhs_value
                                    }
                                    _ => {
                                        ::core::panicking::panic(
                                            "internal error: entered unreachable code",
                                        )
                                    }
                                }
                            }
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        },
                    ),
                )
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
                let lhs_value = self.evaluate(stack[lhs].clone())?;
                let rhs_value = self.evaluate(stack[rhs].clone())?;
                match lhs_value {
                    Value::None => {
                        Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::None,
                            expected: ValueType::Number,
                        })
                    }
                    Value::Bool(_) => {
                        Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: ValueType::None,
                            expected: ValueType::Number,
                        })
                    }
                    Value::LambdaState(_) => {
                        Err(
                            InterpretationError::CannotUseLambdaInExpression(
                                lhs_position,
                            ),
                        )
                    }
                    Value::Integer(lhs) => {
                        match rhs_value {
                            Value::None => {
                                Err(InterpretationError::MismatchedType {
                                    position: rhs_position,
                                    provided: ValueType::None,
                                    expected: ValueType::Number,
                                })
                            }
                            Value::Bool(_) => {
                                Err(InterpretationError::MismatchedType {
                                    position: rhs_position,
                                    provided: ValueType::Boolean,
                                    expected: ValueType::Number,
                                })
                            }
                            Value::LambdaState(_) => {
                                Err(
                                    InterpretationError::CannotUseLambdaInExpression(
                                        rhs_position,
                                    ),
                                )
                            }
                            Value::Integer(rhs) => {
                                Ok(
                                    Value::Integer(
                                        match operator.variant {
                                            BinaryOperatorVariant::Addition => lhs + rhs,
                                            BinaryOperatorVariant::Substraction => lhs - rhs,
                                            BinaryOperatorVariant::Multiplication => lhs * rhs,
                                            BinaryOperatorVariant::Division => lhs / rhs,
                                            _ => {
                                                ::core::panicking::panic(
                                                    "internal error: entered unreachable code",
                                                )
                                            }
                                        },
                                    ),
                                )
                            }
                            Value::Float(rhs) => {
                                Ok(
                                    Value::Float(
                                        match operator.variant {
                                            BinaryOperatorVariant::Addition => lhs as f64 + rhs,
                                            BinaryOperatorVariant::Substraction => lhs as f64 - rhs,
                                            BinaryOperatorVariant::Multiplication => lhs as f64 * rhs,
                                            BinaryOperatorVariant::Division => lhs as f64 / rhs,
                                            _ => {
                                                ::core::panicking::panic(
                                                    "internal error: entered unreachable code",
                                                )
                                            }
                                        },
                                    ),
                                )
                            }
                        }
                    }
                    Value::Float(lhs) => {
                        match rhs_value {
                            Value::None => {
                                Err(InterpretationError::MismatchedType {
                                    position: rhs_position,
                                    provided: ValueType::None,
                                    expected: ValueType::Number,
                                })
                            }
                            Value::Bool(_) => {
                                Err(InterpretationError::MismatchedType {
                                    position: rhs_position,
                                    provided: ValueType::Boolean,
                                    expected: ValueType::Number,
                                })
                            }
                            Value::LambdaState(_) => {
                                Err(
                                    InterpretationError::CannotUseLambdaInExpression(
                                        rhs_position,
                                    ),
                                )
                            }
                            Value::Integer(rhs) => {
                                Ok(
                                    Value::Float(
                                        match operator.variant {
                                            BinaryOperatorVariant::Addition => lhs + rhs as f64,
                                            BinaryOperatorVariant::Substraction => lhs - rhs as f64,
                                            BinaryOperatorVariant::Multiplication => lhs * rhs as f64,
                                            BinaryOperatorVariant::Division => lhs / rhs as f64,
                                            _ => {
                                                ::core::panicking::panic(
                                                    "internal error: entered unreachable code",
                                                )
                                            }
                                        },
                                    ),
                                )
                            }
                            Value::Float(rhs) => {
                                Ok(
                                    Value::Float(
                                        match operator.variant {
                                            BinaryOperatorVariant::Addition => lhs + rhs,
                                            BinaryOperatorVariant::Substraction => lhs - rhs,
                                            BinaryOperatorVariant::Multiplication => lhs * rhs,
                                            BinaryOperatorVariant::Division => lhs / rhs,
                                            _ => {
                                                ::core::panicking::panic(
                                                    "internal error: entered unreachable code",
                                                )
                                            }
                                        },
                                    ),
                                )
                            }
                        }
                    }
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
                    value => {
                        Err(InterpretationError::MismatchedType {
                            position: rhs_position,
                            provided: match value {
                                Value::Bool(_) => ValueType::Boolean,
                                Value::Integer(_) | Value::Float(_) => ValueType::Number,
                                Value::None => ValueType::None,
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            },
                            expected: ValueType::Lambda,
                        })
                    }
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
                match value {
                    Value::None => {
                        Err(InterpretationError::MismatchedType {
                            position,
                            provided: ValueType::None,
                            expected: ValueType::Boolean,
                        })
                    }
                    Value::Integer(_) | Value::Float(_) => {
                        Err(InterpretationError::MismatchedType {
                            position,
                            provided: ValueType::Number,
                            expected: ValueType::Boolean,
                        })
                    }
                    Value::Bool(bool) => {
                        if bool { Ok(action_index + 1) } else { Ok(label) }
                    }
                    Value::LambdaState(lambda_state) => {
                        match self.eval_lambda(lambda_state)? {
                            Value::None => {
                                Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::None,
                                    expected: ValueType::Boolean,
                                })
                            }
                            Value::Integer(_) | Value::Float(_) => {
                                Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::Number,
                                    expected: ValueType::Boolean,
                                })
                            }
                            Value::Bool(bool) => {
                                if bool { Ok(action_index + 1) } else { Ok(label) }
                            }
                            Value::LambdaState(_) => {
                                Err(InterpretationError::MismatchedType {
                                    position,
                                    provided: ValueType::Lambda,
                                    expected: ValueType::Boolean,
                                })
                            }
                        }
                    }
                }
            }
            pub fn eval_lambda(
                &self,
                lambda: LambdaState,
            ) -> Result<Value, InterpretationError> {
                match lambda.function {
                    FunctionIdentifier::Defined(index) => {
                        let arg_count = self.functions[index].params_count;
                        if arg_count > lambda.provided_args.len() {
                            return Ok(Value::LambdaState(lambda));
                        }
                        let mut args = Vec::with_capacity(arg_count);
                        let mut remains = Vec::with_capacity(
                            lambda.provided_args.len() - arg_count,
                        );
                        for i in 0..arg_count {
                            args.push(lambda.provided_args[i].clone());
                        }
                        for i in arg_count..lambda.provided_args.len() {
                            remains.push(lambda.provided_args[i].clone());
                        }
                        let value = self.interpret_function(index, args)?;
                        match value {
                            Value::LambdaState(lambda) => {
                                self.eval_lambda(LambdaState {
                                    function: lambda.function,
                                    provided_args: {
                                        let mut args = lambda.provided_args;
                                        args.append(&mut remains);
                                        args
                                    },
                                })
                            }
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
                if let Some(arg_count) = get_arg_count(function) {
                    if params.len() != arg_count {
                        return Err(InterpretationError::InvalidArgumentCountBuiltInFunction {
                            function,
                            provided: params.len(),
                            expected: arg_count,
                        });
                    }
                }
                match function {
                    BuiltInFunction::Print => self.print(params),
                    BuiltInFunction::Sin => self.sin(params),
                    BuiltInFunction::Cos => self.cos(params),
                    BuiltInFunction::Tan => self.tan(params),
                    BuiltInFunction::Log2 => self.log2(params),
                    BuiltInFunction::Ln => self.ln(params),
                    BuiltInFunction::Log => self.log(params),
                    BuiltInFunction::Asin => self.asin(params),
                    BuiltInFunction::Acos => self.acos(params),
                    BuiltInFunction::Atan => self.atan(params),
                }
            }
            fn print(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                for param in params {
                    let value = self.evaluate(param)?;
                    match value {
                        Value::None => {
                            ::std::io::_print(format_args!("none "));
                        }
                        Value::Integer(int) => {
                            ::std::io::_print(format_args!("{0} ", int));
                        }
                        Value::Float(float) => {
                            ::std::io::_print(format_args!("{0} ", float));
                        }
                        Value::Bool(bool) => {
                            ::std::io::_print(format_args!("{0} ", bool));
                        }
                        Value::LambdaState(lambda_state) => {
                            ::std::io::_print(format_args!("\n {0:?}\n", lambda_state));
                        }
                    }
                }
                {
                    ::std::io::_print(format_args!("\n"));
                };
                Ok(Value::None)
            }
            fn get_float(
                function: BuiltInFunction,
                value: &Value,
            ) -> Result<f64, InterpretationError> {
                match value {
                    Value::None => {
                        Err(InterpretationError::InvalidArgumentTypeBuiltInFunction {
                            function,
                            provided: ValueType::None,
                            expected: ValueType::Number,
                        })
                    }
                    Value::Integer(int) => Ok(*int as f64),
                    Value::Float(float) => Ok(*float),
                    Value::Bool(_) => {
                        Err(InterpretationError::InvalidArgumentTypeBuiltInFunction {
                            function,
                            provided: ValueType::Boolean,
                            expected: ValueType::Number,
                        })
                    }
                    Value::LambdaState(_) => {
                        Err(InterpretationError::InvalidArgumentTypeBuiltInFunction {
                            function,
                            provided: ValueType::Lambda,
                            expected: ValueType::Number,
                        })
                    }
                }
            }
            fn sin(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Sin, &params[0])?;
                Ok(Value::Float(float.sin()))
            }
            fn cos(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Cos, &params[0])?;
                Ok(Value::Float(float.cos()))
            }
            fn tan(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Tan, &params[0])?;
                Ok(Value::Float(float.tan()))
            }
            fn asin(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Asin, &params[0])?;
                Ok(Value::Float(float.asin()))
            }
            fn acos(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Acos, &params[0])?;
                Ok(Value::Float(float.acos()))
            }
            fn atan(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Atan, &params[0])?;
                Ok(Value::Float(float.atan()))
            }
            fn log2(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Log2, &params[0])?;
                Ok(Value::Float(float.log2()))
            }
            fn ln(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let float = Self::get_float(BuiltInFunction::Ln, &params[0])?;
                Ok(Value::Float(float.ln()))
            }
            fn log(&self, params: Vec<Value>) -> Result<Value, InterpretationError> {
                let base = Self::get_float(BuiltInFunction::Log2, &params[0])?;
                let arg = Self::get_float(BuiltInFunction::Log2, &params[0])?;
                Ok(Value::Float(arg.log(base)))
            }
            fn evaluate(&self, value: Value) -> Result<Value, InterpretationError> {
                match value {
                    Value::LambdaState(lambda_state) => self.eval_lambda(lambda_state),
                    value => Ok(value),
                }
            }
        }
    }
    pub mod std_funcs {
        pub enum BuiltInFunction {
            Print,
            Sin,
            Cos,
            Tan,
            Log2,
            Ln,
            Log,
            Asin,
            Acos,
            Atan,
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for BuiltInFunction {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(
                    f,
                    match self {
                        BuiltInFunction::Print => "Print",
                        BuiltInFunction::Sin => "Sin",
                        BuiltInFunction::Cos => "Cos",
                        BuiltInFunction::Tan => "Tan",
                        BuiltInFunction::Log2 => "Log2",
                        BuiltInFunction::Ln => "Ln",
                        BuiltInFunction::Log => "Log",
                        BuiltInFunction::Asin => "Asin",
                        BuiltInFunction::Acos => "Acos",
                        BuiltInFunction::Atan => "Atan",
                    },
                )
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for BuiltInFunction {
            #[inline]
            fn clone(&self) -> BuiltInFunction {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for BuiltInFunction {}
        pub fn find_name<'code>(name: &'code str) -> Option<BuiltInFunction> {
            match name {
                "print" => Some(BuiltInFunction::Print),
                "sin" => Some(BuiltInFunction::Sin),
                "cos" => Some(BuiltInFunction::Cos),
                "tan" => Some(BuiltInFunction::Tan),
                "log2" => Some(BuiltInFunction::Log2),
                "ln" => Some(BuiltInFunction::Ln),
                "log" => Some(BuiltInFunction::Log),
                "asin" => Some(BuiltInFunction::Asin),
                "acos" => Some(BuiltInFunction::Acos),
                "atan" => Some(BuiltInFunction::Atan),
                _ => None,
            }
        }
        pub fn get_arg_count(function: BuiltInFunction) -> Option<usize> {
            match function {
                BuiltInFunction::Print => None,
                BuiltInFunction::Sin => Some(1),
                BuiltInFunction::Cos => Some(1),
                BuiltInFunction::Tan => Some(1),
                BuiltInFunction::Log2 => Some(1),
                BuiltInFunction::Ln => Some(1),
                BuiltInFunction::Log => Some(2),
                BuiltInFunction::Asin => Some(1),
                BuiltInFunction::Acos => Some(1),
                BuiltInFunction::Atan => Some(1),
            }
        }
        pub fn get_name(function: BuiltInFunction) -> &'static str {
            match function {
                BuiltInFunction::Print => "print",
                BuiltInFunction::Sin => "sin",
                BuiltInFunction::Cos => "cos",
                BuiltInFunction::Tan => "tan",
                BuiltInFunction::Log2 => "log2",
                BuiltInFunction::Ln => "ln",
                BuiltInFunction::Log => "log",
                BuiltInFunction::Asin => "asin",
                BuiltInFunction::Acos => "acos",
                BuiltInFunction::Atan => "atan",
            }
        }
    }
    pub mod type_checker {
        pub enum ValueType {
            None,
            Number,
            Boolean,
            Lambda,
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for ValueType {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(
                    f,
                    match self {
                        ValueType::None => "None",
                        ValueType::Number => "Number",
                        ValueType::Boolean => "Boolean",
                        ValueType::Lambda => "Lambda",
                    },
                )
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for ValueType {
            #[inline]
            fn clone(&self) -> ValueType {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for ValueType {}
    }
    pub use error::*;
    use std::fmt::Debug;
    use lady_deirdre::lexis::PositionSpan;
    pub struct FunctionDescription {
        pub name: Option<String>,
        pub position: PositionSpan,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for FunctionDescription {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "FunctionDescription",
                "name",
                &self.name,
                "position",
                &&self.position,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for FunctionDescription {
        #[inline]
        fn clone(&self) -> FunctionDescription {
            FunctionDescription {
                name: ::core::clone::Clone::clone(&self.name),
                position: ::core::clone::Clone::clone(&self.position),
            }
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for FunctionDescription {
        #[inline]
        fn default() -> FunctionDescription {
            FunctionDescription {
                name: ::core::default::Default::default(),
                position: ::core::default::Default::default(),
            }
        }
    }
    pub struct Function {
        pub description: FunctionDescription,
        pub params_count: usize,
        /// Last value of stack is returned
        pub stack: Vec<StackValue>,
        pub body: Vec<Action>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Function {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "Function",
                "description",
                &self.description,
                "params_count",
                &self.params_count,
                "stack",
                &self.stack,
                "body",
                &&self.body,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Function {
        #[inline]
        fn clone(&self) -> Function {
            Function {
                description: ::core::clone::Clone::clone(&self.description),
                params_count: ::core::clone::Clone::clone(&self.params_count),
                stack: ::core::clone::Clone::clone(&self.stack),
                body: ::core::clone::Clone::clone(&self.body),
            }
        }
    }
    pub enum StackValue {
        Bool(bool),
        Integer(i64),
        Float(f64),
        Function(FunctionIdentifier),
        /// Index of parameter in argument array
        Parameter(usize),
        None,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for StackValue {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                StackValue::Bool(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Bool",
                        &__self_0,
                    )
                }
                StackValue::Integer(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Integer",
                        &__self_0,
                    )
                }
                StackValue::Float(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Float",
                        &__self_0,
                    )
                }
                StackValue::Function(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Function",
                        &__self_0,
                    )
                }
                StackValue::Parameter(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Parameter",
                        &__self_0,
                    )
                }
                StackValue::None => ::core::fmt::Formatter::write_str(f, "None"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for StackValue {
        #[inline]
        fn clone(&self) -> StackValue {
            let _: ::core::clone::AssertParamIsClone<bool>;
            let _: ::core::clone::AssertParamIsClone<i64>;
            let _: ::core::clone::AssertParamIsClone<f64>;
            let _: ::core::clone::AssertParamIsClone<FunctionIdentifier>;
            let _: ::core::clone::AssertParamIsClone<usize>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for StackValue {}
    pub enum Action {
        UnaryOperation { src: usize, position: PositionSpan, operator: UnaryOperator },
        BinaryOperation {
            lhs: usize,
            rhs: usize,
            operator: BinaryOperator,
            lhs_position: PositionSpan,
            rhs_position: PositionSpan,
        },
        Copy { src: usize, dist: usize },
        ConditionalJump { src: usize, position: PositionSpan, label: usize },
        Goto(usize),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Action {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Action::UnaryOperation {
                    src: __self_0,
                    position: __self_1,
                    operator: __self_2,
                } => {
                    ::core::fmt::Formatter::debug_struct_field3_finish(
                        f,
                        "UnaryOperation",
                        "src",
                        __self_0,
                        "position",
                        __self_1,
                        "operator",
                        &__self_2,
                    )
                }
                Action::BinaryOperation {
                    lhs: __self_0,
                    rhs: __self_1,
                    operator: __self_2,
                    lhs_position: __self_3,
                    rhs_position: __self_4,
                } => {
                    ::core::fmt::Formatter::debug_struct_field5_finish(
                        f,
                        "BinaryOperation",
                        "lhs",
                        __self_0,
                        "rhs",
                        __self_1,
                        "operator",
                        __self_2,
                        "lhs_position",
                        __self_3,
                        "rhs_position",
                        &__self_4,
                    )
                }
                Action::Copy { src: __self_0, dist: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Copy",
                        "src",
                        __self_0,
                        "dist",
                        &__self_1,
                    )
                }
                Action::ConditionalJump {
                    src: __self_0,
                    position: __self_1,
                    label: __self_2,
                } => {
                    ::core::fmt::Formatter::debug_struct_field3_finish(
                        f,
                        "ConditionalJump",
                        "src",
                        __self_0,
                        "position",
                        __self_1,
                        "label",
                        &__self_2,
                    )
                }
                Action::Goto(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Goto",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Action {
        #[inline]
        fn clone(&self) -> Action {
            match self {
                Action::UnaryOperation {
                    src: __self_0,
                    position: __self_1,
                    operator: __self_2,
                } => {
                    Action::UnaryOperation {
                        src: ::core::clone::Clone::clone(__self_0),
                        position: ::core::clone::Clone::clone(__self_1),
                        operator: ::core::clone::Clone::clone(__self_2),
                    }
                }
                Action::BinaryOperation {
                    lhs: __self_0,
                    rhs: __self_1,
                    operator: __self_2,
                    lhs_position: __self_3,
                    rhs_position: __self_4,
                } => {
                    Action::BinaryOperation {
                        lhs: ::core::clone::Clone::clone(__self_0),
                        rhs: ::core::clone::Clone::clone(__self_1),
                        operator: ::core::clone::Clone::clone(__self_2),
                        lhs_position: ::core::clone::Clone::clone(__self_3),
                        rhs_position: ::core::clone::Clone::clone(__self_4),
                    }
                }
                Action::Copy { src: __self_0, dist: __self_1 } => {
                    Action::Copy {
                        src: ::core::clone::Clone::clone(__self_0),
                        dist: ::core::clone::Clone::clone(__self_1),
                    }
                }
                Action::ConditionalJump {
                    src: __self_0,
                    position: __self_1,
                    label: __self_2,
                } => {
                    Action::ConditionalJump {
                        src: ::core::clone::Clone::clone(__self_0),
                        position: ::core::clone::Clone::clone(__self_1),
                        label: ::core::clone::Clone::clone(__self_2),
                    }
                }
                Action::Goto(__self_0) => {
                    Action::Goto(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct UnaryOperator {
        pub position: PositionSpan,
        pub variant: UnaryOperatorVariant,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UnaryOperator {
        #[inline]
        fn clone(&self) -> UnaryOperator {
            UnaryOperator {
                position: ::core::clone::Clone::clone(&self.position),
                variant: ::core::clone::Clone::clone(&self.variant),
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryOperator {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "UnaryOperator",
                "position",
                &self.position,
                "variant",
                &&self.variant,
            )
        }
    }
    pub enum UnaryOperatorVariant {
        /// !
        Inversion,
        /// ~
        Negation,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UnaryOperatorVariant {
        #[inline]
        fn clone(&self) -> UnaryOperatorVariant {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for UnaryOperatorVariant {}
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryOperatorVariant {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    UnaryOperatorVariant::Inversion => "Inversion",
                    UnaryOperatorVariant::Negation => "Negation",
                },
            )
        }
    }
    pub struct BinaryOperator {
        pub position: PositionSpan,
        pub variant: BinaryOperatorVariant,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for BinaryOperator {
        #[inline]
        fn clone(&self) -> BinaryOperator {
            BinaryOperator {
                position: ::core::clone::Clone::clone(&self.position),
                variant: ::core::clone::Clone::clone(&self.variant),
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryOperator {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "BinaryOperator",
                "position",
                &self.position,
                "variant",
                &&self.variant,
            )
        }
    }
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
    #[automatically_derived]
    impl ::core::clone::Clone for BinaryOperatorVariant {
        #[inline]
        fn clone(&self) -> BinaryOperatorVariant {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for BinaryOperatorVariant {}
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryOperatorVariant {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    BinaryOperatorVariant::Xor => "Xor",
                    BinaryOperatorVariant::Or => "Or",
                    BinaryOperatorVariant::And => "And",
                    BinaryOperatorVariant::Equals => "Equals",
                    BinaryOperatorVariant::NotEquals => "NotEquals",
                    BinaryOperatorVariant::Less => "Less",
                    BinaryOperatorVariant::LessEqual => "LessEqual",
                    BinaryOperatorVariant::Greater => "Greater",
                    BinaryOperatorVariant::GreaterEqual => "GreaterEqual",
                    BinaryOperatorVariant::Addition => "Addition",
                    BinaryOperatorVariant::Substraction => "Substraction",
                    BinaryOperatorVariant::Multiplication => "Multiplication",
                    BinaryOperatorVariant::Division => "Division",
                    BinaryOperatorVariant::PipeOperator => "PipeOperator",
                },
            )
        }
    }
    pub enum Value {
        #[default]
        None,
        Integer(i64),
        Float(f64),
        Bool(bool),
        LambdaState(LambdaState),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Value {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Value::None => ::core::fmt::Formatter::write_str(f, "None"),
                Value::Integer(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Integer",
                        &__self_0,
                    )
                }
                Value::Float(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Float",
                        &__self_0,
                    )
                }
                Value::Bool(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Bool",
                        &__self_0,
                    )
                }
                Value::LambdaState(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LambdaState",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Value {
        #[inline]
        fn clone(&self) -> Value {
            match self {
                Value::None => Value::None,
                Value::Integer(__self_0) => {
                    Value::Integer(::core::clone::Clone::clone(__self_0))
                }
                Value::Float(__self_0) => {
                    Value::Float(::core::clone::Clone::clone(__self_0))
                }
                Value::Bool(__self_0) => {
                    Value::Bool(::core::clone::Clone::clone(__self_0))
                }
                Value::LambdaState(__self_0) => {
                    Value::LambdaState(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::default::Default for Value {
        #[inline]
        fn default() -> Value {
            Self::None
        }
    }
    pub enum FunctionIdentifier {
        Defined(usize),
        BuiltIn(std_funcs::BuiltInFunction),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for FunctionIdentifier {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                FunctionIdentifier::Defined(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Defined",
                        &__self_0,
                    )
                }
                FunctionIdentifier::BuiltIn(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "BuiltIn",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for FunctionIdentifier {
        #[inline]
        fn clone(&self) -> FunctionIdentifier {
            let _: ::core::clone::AssertParamIsClone<usize>;
            let _: ::core::clone::AssertParamIsClone<std_funcs::BuiltInFunction>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for FunctionIdentifier {}
    pub struct LambdaState {
        pub function: FunctionIdentifier,
        pub provided_args: Vec<Value>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for LambdaState {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "LambdaState",
                "function",
                &self.function,
                "provided_args",
                &&self.provided_args,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LambdaState {
        #[inline]
        fn clone(&self) -> LambdaState {
            LambdaState {
                function: ::core::clone::Clone::clone(&self.function),
                provided_args: ::core::clone::Clone::clone(&self.provided_args),
            }
        }
    }
}
pub mod compiler {
    use std::collections::HashMap;
    use lady_deirdre::{format::AnnotationPriority, lexis::{PositionSpan, SourceCode}};
    use crate::{
        cir::*, error_displayer::DisplayError,
        parser::{
            BinaryExpression, FunctionCall, FunctionExpression, IfExpression, LambdaBody,
            UnaryExpression, ValueExpression, ValueExpressionInner,
        },
    };
    pub struct Compiler<'code> {
        pub functions: Vec<Function>,
        pub func_names: HashMap<&'code str, usize>,
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for Compiler<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Compiler",
                "functions",
                &self.functions,
                "func_names",
                &&self.func_names,
            )
        }
    }
    pub enum CompilationError {
        NameNotFound(PositionSpan),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CompilationError {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CompilationError::NameNotFound(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NameNotFound",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CompilationError {
        #[inline]
        fn clone(&self) -> CompilationError {
            match self {
                CompilationError::NameNotFound(__self_0) => {
                    CompilationError::NameNotFound(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl CompilationError {
        pub fn display<'a, C: SourceCode>(self, source: &'a C) -> DisplayError<'a, C> {
            let message;
            let annotations;
            match self {
                CompilationError::NameNotFound(position) => {
                    message = ::alloc::__export::must_use({
                        let res = ::alloc::fmt::format(
                            format_args!("Cannot find symbol in this scope"),
                        );
                        res
                    });
                    annotations = <[_]>::into_vec(
                        #[rustc_box]
                        ::alloc::boxed::Box::new([
                            (
                                position,
                                AnnotationPriority::Default,
                                "Cannot find symbol in this scope".to_string(),
                            ),
                        ]),
                    );
                }
            }
            DisplayError {
                source,
                message,
                annotations,
            }
        }
    }
    impl<'code> Compiler<'code> {
        pub fn new(
            functions: Vec<FunctionExpression<'code>>,
        ) -> Result<Self, CompilationError> {
            let compiler = Self {
                functions: ::alloc::vec::from_elem(
                    Function {
                        description: Default::default(),
                        params_count: 0,
                        stack: ::alloc::vec::Vec::new(),
                        body: ::alloc::vec::Vec::new(),
                    },
                    functions.len(),
                ),
                func_names: {
                    let mut names = HashMap::new();
                    for (index, func) in functions.iter().enumerate() {
                        names.insert(func.name, index);
                    }
                    names
                },
            };
            compiler.compile_functions(functions)
        }
        fn compile_functions(
            mut self,
            functions: Vec<FunctionExpression<'code>>,
        ) -> Result<Self, CompilationError> {
            for (index, func) in functions.into_iter().enumerate() {
                self.functions[index] = self
                    .compile_function(
                        Some(func.name.to_string()),
                        func.position.clone(),
                        &func.params,
                        &func.body,
                    )?;
            }
            Ok(self)
        }
        fn compile_function(
            &mut self,
            name: Option<String>,
            position: PositionSpan,
            params: &Vec<&'code str>,
            body: &ValueExpression<'code>,
        ) -> Result<Function, CompilationError> {
            let mut params_table = HashMap::new();
            for (index, param) in params.iter().enumerate() {
                params_table.insert(*param, index);
            }
            let (stack, body) = self.compile_expr(0, 0, &params_table, body)?;
            Ok(Function {
                description: FunctionDescription {
                    name,
                    position,
                },
                params_count: params.len(),
                stack,
                body,
            })
        }
        fn compile_expr(
            &mut self,
            starting_stack_index: usize,
            starting_label_index: usize,
            params: &HashMap<&'code str, usize>,
            expr: &ValueExpression<'code>,
        ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
            match &expr.inner {
                ValueExpressionInner::Binary(binary_expression) => {
                    self.compile_binary_expr(
                        starting_stack_index,
                        starting_label_index,
                        params,
                        binary_expression,
                    )
                }
                ValueExpressionInner::Unary(unary_expression) => {
                    self.compile_unary_expr(
                        starting_stack_index,
                        starting_label_index,
                        params,
                        unary_expression,
                    )
                }
                ValueExpressionInner::FuncCall(function_call) => {
                    self.compile_func_call(
                        starting_stack_index,
                        starting_label_index,
                        params,
                        function_call,
                    )
                }
                ValueExpressionInner::Ident(name) => {
                    let value = Self::find_name(
                        name,
                        expr.position.clone(),
                        &[
                            (params, RepoType::Parameter),
                            (&mut self.func_names, RepoType::Function),
                        ],
                    )?;
                    Ok((
                        <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([value])),
                        ::alloc::vec::Vec::new(),
                    ))
                }
                ValueExpressionInner::Bool(bool) => {
                    Ok((
                        <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([StackValue::Bool(*bool)]),
                        ),
                        ::alloc::vec::Vec::new(),
                    ))
                }
                ValueExpressionInner::None => {
                    Ok((
                        <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([StackValue::None]),
                        ),
                        ::alloc::vec::Vec::new(),
                    ))
                }
                ValueExpressionInner::Integer(integer) => {
                    Ok((
                        <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([StackValue::Integer(*integer)]),
                        ),
                        ::alloc::vec::Vec::new(),
                    ))
                }
                ValueExpressionInner::Float(float) => {
                    Ok((
                        <[_]>::into_vec(
                            #[rustc_box]
                            ::alloc::boxed::Box::new([StackValue::Float(*float)]),
                        ),
                        ::alloc::vec::Vec::new(),
                    ))
                }
                ValueExpressionInner::Lambda(lambda_body) => {
                    self.compile_lambda(expr.position.clone(), lambda_body)
                }
                ValueExpressionInner::If(if_expression) => {
                    self.compile_if_expr(
                        starting_stack_index,
                        starting_label_index,
                        params,
                        if_expression,
                    )
                }
            }
        }
        fn compile_binary_expr(
            &mut self,
            starting_stack_index: usize,
            starting_label_index: usize,
            params: &HashMap<&'code str, usize>,
            expr: &BinaryExpression<'code>,
        ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
            let mut stack = ::alloc::vec::Vec::new();
            let mut actions = ::alloc::vec::Vec::new();
            let lhs_index = starting_stack_index;
            let (mut lhs_stack, mut lhs_actions) = self
                .compile_expr(
                    starting_stack_index,
                    starting_label_index,
                    params,
                    &expr.lhs,
                )?;
            let rhs_index = starting_stack_index + lhs_stack.len();
            let (mut rhs_stack, mut rhs_actions) = self
                .compile_expr(
                    rhs_index,
                    starting_label_index + lhs_actions.len(),
                    params,
                    &expr.rhs,
                )?;
            stack.append(&mut lhs_stack);
            stack.append(&mut rhs_stack);
            actions.append(&mut lhs_actions);
            actions.append(&mut rhs_actions);
            actions
                .push(Action::BinaryOperation {
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
            params: &HashMap<&'code str, usize>,
            expr: &UnaryExpression<'code>,
        ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
            let index = starting_stack_index;
            let (stack, mut actions) = self
                .compile_expr(
                    starting_stack_index,
                    starting_label_index,
                    params,
                    &expr.operand,
                )?;
            actions
                .push(Action::UnaryOperation {
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
            params: &HashMap<&'code str, usize>,
            expr: &FunctionCall<'code>,
        ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
            let mut stack = ::alloc::vec::Vec::new();
            let mut actions = ::alloc::vec::Vec::new();
            let arg_index = starting_stack_index;
            let (mut arg_stack, mut arg_actions) = self
                .compile_expr(arg_index, starting_label_index, params, &expr.arg)?;
            let func_index = starting_stack_index + arg_stack.len();
            let (mut func_stack, mut func_actions) = self
                .compile_expr(
                    starting_stack_index + arg_stack.len(),
                    starting_label_index + arg_actions.len(),
                    params,
                    &expr.func,
                )?;
            stack.append(&mut arg_stack);
            actions.append(&mut arg_actions);
            stack.append(&mut func_stack);
            actions.append(&mut func_actions);
            actions
                .push(Action::BinaryOperation {
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
            params: &HashMap<&'code str, usize>,
            expr: &IfExpression<'code>,
        ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
            let (mut stack, mut actions) = self
                .compile_expr(
                    starting_stack_index,
                    starting_label_index,
                    params,
                    &expr.cond,
                )?;
            let (mut then_stack, mut then_actions) = self
                .compile_expr(
                    starting_stack_index + stack.len(),
                    starting_label_index + actions.len() + 1,
                    params,
                    &expr.if_true,
                )?;
            then_actions
                .push(Action::Copy {
                    src: starting_stack_index + stack.len(),
                    dist: starting_stack_index,
                });
            actions
                .push(Action::ConditionalJump {
                    src: starting_stack_index,
                    position: expr.cond.position.clone(),
                    label: starting_label_index + actions.len() + then_actions.len() + 2,
                });
            stack.append(&mut then_stack);
            actions.append(&mut then_actions);
            drop(then_stack);
            drop(then_actions);
            let (mut else_stack, mut else_actions) = self
                .compile_expr(
                    starting_stack_index + stack.len(),
                    starting_label_index + actions.len() + 1,
                    params,
                    &expr.if_false,
                )?;
            else_actions
                .push(Action::Copy {
                    src: starting_stack_index + stack.len(),
                    dist: starting_stack_index,
                });
            actions
                .push(
                    Action::Goto(
                        starting_label_index + actions.len() + 1 + else_actions.len(),
                    ),
                );
            stack.append(&mut else_stack);
            actions.append(&mut else_actions);
            Ok((stack, actions))
        }
        fn compile_lambda(
            &mut self,
            position: PositionSpan,
            body: &LambdaBody<'code>,
        ) -> Result<(Vec<StackValue>, Vec<Action>), CompilationError> {
            let index = self.functions.len();
            self.functions
                .push(Function {
                    description: Default::default(),
                    params_count: 0,
                    stack: ::alloc::vec::Vec::new(),
                    body: ::alloc::vec::Vec::new(),
                });
            self.functions[index] = self
                .compile_function(None, position, &body.params, &body.body)?;
            Ok((
                <[_]>::into_vec(
                    #[rustc_box]
                    ::alloc::boxed::Box::new([
                        StackValue::Function(FunctionIdentifier::Defined(index)),
                    ]),
                ),
                ::alloc::vec::Vec::new(),
            ))
        }
        fn find_name(
            name: &str,
            position: PositionSpan,
            repos: &[(&HashMap<&str, usize>, RepoType)],
        ) -> Result<StackValue, CompilationError> {
            for (repo, repo_type) in repos {
                if let Some(index) = repo.get(&name) {
                    return Ok(
                        match repo_type {
                            RepoType::Parameter => StackValue::Parameter(*index),
                            RepoType::Function => {
                                StackValue::Function(FunctionIdentifier::Defined(*index))
                            }
                        },
                    );
                }
            }
            match std_funcs::find_name(name) {
                Some(func) => Ok(StackValue::Function(FunctionIdentifier::BuiltIn(func))),
                None => Err(CompilationError::NameNotFound(position)),
            }
        }
    }
    enum RepoType {
        Parameter,
        Function,
    }
}
pub mod error_displayer {
    use std::fmt::{Debug, Display, Formatter};
    use lady_deirdre::{
        format::{AnnotationPriority, SnippetConfig, SnippetFormatter},
        lexis::{PositionSpan, SourceCode},
    };
    pub struct DisplayError<'a, C: SourceCode> {
        pub source: &'a C,
        pub message: String,
        pub annotations: Vec<(PositionSpan, AnnotationPriority, String)>,
    }
    impl<'a, C: SourceCode> Debug for DisplayError<'a, C> {
        #[inline(always)]
        fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
            Display::fmt(self, formatter)
        }
    }
    impl<'a, C: SourceCode> Display for DisplayError<'a, C> {
        fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
            let mut snippet = formatter.snippet(self.source);
            snippet.set_config(&const { SnippetConfig::verbose() });
            for (span, priority, text) in self.annotations.iter().rev() {
                snippet.annotate(span, *priority, text);
            }
            if !self.message.is_empty() {
                snippet.set_summary(&self.message);
            }
            snippet.finish()
        }
    }
}
pub mod parser {
    use lady_deirdre::{
        lexis::{PositionSpan, ToSpan},
        syntax::{NodeRef, PolyRef},
        units::Document,
    };
    use lexer::BasicToken;
    use syntax::BasicNode;
    use crate::cir::{
        BinaryOperator, BinaryOperatorVariant, UnaryOperator, UnaryOperatorVariant,
    };
    pub mod lexer {
        use lady_deirdre::lexis::Token;
        #[define(HEX = ['0'..'9', 'A'..'F', 'a'..'f'])]
        #[repr(u8)]
        pub enum BasicToken {
            EOI = 0,
            Mismatch = 1,
            #[rule("if")]
            #[describe("KW_if")]
            #[priority(2)]
            KwIf,
            #[rule("then")]
            #[describe("KW_then")]
            #[priority(2)]
            KwThen,
            #[rule("else")]
            #[describe("KW_else")]
            #[priority(2)]
            KwElse,
            #[rule("where")]
            #[describe("KW_where")]
            #[priority(2)]
            KwWhere,
            #[rule("lambda")]
            #[describe("KW_lambda")]
            #[priority(2)]
            KwLambda,
            #[rule("none")]
            #[describe("KW_none")]
            #[priority(2)]
            KwNone,
            #[rule("true")]
            #[describe("KW_true")]
            #[priority(2)]
            KwTrue,
            #[rule("false")]
            #[describe("KW_false")]
            #[priority(2)]
            KwFalse,
            #[rule("and")]
            #[describe("KW_and")]
            #[priority(2)]
            KwAnd,
            #[rule("or")]
            #[describe("KW_or")]
            #[priority(2)]
            KwOr,
            #[rule("xor")]
            #[describe("KW_xor")]
            #[priority(2)]
            KwXor,
            #[rule(['0'..'9']+('.'['0'..'9']*('e' '-'?['0'..'9']+)?)?)]
            #[describe("number")]
            Number,
            #[rule("==")]
            #[describe("==")]
            #[priority(2)]
            Equal,
            #[rule("!=")]
            #[describe("!=")]
            #[priority(2)]
            NotEqual,
            #[rule("<")]
            #[describe("<")]
            Less,
            #[rule(">")]
            #[describe(">")]
            Greater,
            #[rule("<=")]
            #[describe("<=")]
            #[priority(2)]
            LessEqual,
            #[rule(">=")]
            #[describe(">=")]
            #[priority(2)]
            GreaterEqual,
            #[rule("+")]
            #[describe("+")]
            Plus,
            #[rule("-")]
            #[describe("-")]
            Minus,
            #[rule("*")]
            #[describe("*")]
            Star,
            #[rule("/")]
            #[describe("/")]
            Slash,
            #[rule("!")]
            #[describe("!")]
            Exclamation,
            #[rule("~")]
            #[describe("~")]
            Tilde,
            #[rule("=")]
            #[describe("=")]
            Let,
            #[rule("|>")]
            #[describe("|>")]
            #[priority(2)]
            PipeOperator,
            #[rule("->")]
            #[describe("->")]
            #[priority(2)]
            Arrow,
            #[rule("(")]
            #[describe("(")]
            ParenthesesOpen,
            #[rule(")")]
            #[describe(")")]
            ParenthesesClose,
            #[rule("[")]
            #[describe("[")]
            BracketOpen,
            #[rule("]")]
            #[describe("]")]
            BracketClose,
            #[rule(",")]
            #[describe(",")]
            Comma,
            #[rule(":")]
            #[describe(":")]
            Colon,
            #[rule("::")]
            #[describe("::")]
            #[priority(2)]
            DoubleColon,
            #[rule(";")]
            #[describe(";")]
            Semicolon,
            #[priority(5)]
            #[rule("--"^['\n']*'\n'?)]
            SingleComment,
            #[priority(6)]
            #[rule("--{{"(^['}']|('}'^['}']))*"}}")]
            MultilineComment,
            #[rule([' ', '\t', '\n', '\x0c', '\r']+)]
            Whitespace,
            #[rule($xid_start&$xid_continue*)]
            Ident,
        }
        impl ::lady_deirdre::lexis::Token for BasicToken {
            const LOOKBACK: ::lady_deirdre::lexis::Length = 1;
            fn scan(session: &mut impl ::lady_deirdre::lexis::LexisSession) -> Self {
                #[allow(unused_mut)]
                let mut state = 1usize;
                #[allow(unused_mut)]
                let mut token = Self::Mismatch;
                loop {
                    let byte = ::lady_deirdre::lexis::LexisSession::advance(session);
                    if byte == 0xFF {
                        break;
                    }
                    match state {
                        1usize => {
                            match byte {
                                65u8..=90u8
                                | 98u8..=100u8
                                | 103u8..=104u8
                                | 106u8..=107u8
                                | 109u8
                                | 112u8..=115u8
                                | 117u8..=118u8
                                | 121u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                120u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 10usize;
                                }
                                97u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 14usize;
                                }
                                91u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::BracketOpen;
                                    break;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 30usize;
                                }
                                9u8..=10u8 | 12u8..=13u8 | 32u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Whitespace;
                                    state = 31usize;
                                }
                                116u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 35usize;
                                }
                                60u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Less;
                                    state = 38usize;
                                }
                                105u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 47usize;
                                }
                                41u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::ParenthesesClose;
                                    break;
                                }
                                126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Tilde;
                                    break;
                                }
                                62u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Greater;
                                    state = 50usize;
                                }
                                119u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 51usize;
                                }
                                93u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::BracketClose;
                                    break;
                                }
                                48u8..=57u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                    state = 53usize;
                                }
                                42u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Star;
                                    break;
                                }
                                43u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Plus;
                                    break;
                                }
                                33u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Exclamation;
                                    state = 56usize;
                                }
                                111u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 57usize;
                                }
                                102u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 58usize;
                                }
                                124u8 => state = 59usize,
                                108u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 60usize;
                                }
                                58u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Colon;
                                    state = 61usize;
                                }
                                59u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Semicolon;
                                    break;
                                }
                                47u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Slash;
                                    break;
                                }
                                45u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Minus;
                                    state = 64usize;
                                }
                                40u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::ParenthesesOpen;
                                    break;
                                }
                                44u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Comma;
                                    break;
                                }
                                110u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 67usize;
                                }
                                61u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Let;
                                    state = 68usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_start(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        3usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        10usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=110u8
                                | 112u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                111u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 11usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        11usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=113u8
                                | 115u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                114u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwXor;
                                    state = 25usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        14usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=109u8
                                | 111u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                110u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 15usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        15usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=99u8
                                | 101u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                100u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwAnd;
                                    state = 45usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        25usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        30usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=107u8
                                | 109u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                108u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 23usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        23usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=114u8
                                | 116u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                115u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 24usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        24usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwElse;
                                    state = 83usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        31usize => {
                            match byte {
                                9u8..=10u8 | 12u8..=13u8 | 32u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Whitespace;
                                }
                                _ => break,
                            }
                        }
                        35usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=103u8
                                | 105u8..=113u8
                                | 115u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                114u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 36usize;
                                }
                                104u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 37usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        36usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=116u8
                                | 118u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                117u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 87usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        37usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 8usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        8usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=109u8
                                | 111u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                110u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwThen;
                                    state = 9usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        9usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        38usize => {
                            match byte {
                                61u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::LessEqual;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        45usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        47usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=101u8
                                | 103u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                102u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwIf;
                                    state = 79usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        50usize => {
                            match byte {
                                61u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::GreaterEqual;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        51usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=103u8
                                | 105u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                104u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 96usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        53usize => {
                            match byte {
                                48u8..=57u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                }
                                46u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                    state = 109usize;
                                }
                                _ => break,
                            }
                        }
                        56usize => {
                            match byte {
                                61u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::NotEqual;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        57usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=113u8
                                | 115u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                114u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwOr;
                                    state = 78usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        58usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 98u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                97u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 94usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        59usize => {
                            match byte {
                                62u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::PipeOperator;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        60usize => {
                            match byte {
                                97u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 2usize;
                                }
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 98u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        2usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=108u8
                                | 110u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                109u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 5usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        5usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8 | 99u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                98u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 12usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        12usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=99u8
                                | 101u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                100u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 13usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        13usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 98u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                97u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwLambda;
                                    state = 33usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        33usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        61usize => {
                            match byte {
                                58u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::DoubleColon;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        64usize => {
                            match byte {
                                62u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Arrow;
                                    break;
                                }
                                45u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 118usize;
                                }
                                _ => break,
                            }
                        }
                        67usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=110u8
                                | 112u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                111u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 19usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        19usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=109u8
                                | 111u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                110u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 20usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        20usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwNone;
                                    state = 76usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        68usize => {
                            match byte {
                                61u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Equal;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        76usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        78usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        79usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        83usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        87usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwTrue;
                                    state = 6usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        6usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        94usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=107u8
                                | 109u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                108u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 21usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        21usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=114u8
                                | 116u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                115u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 22usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        22usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwFalse;
                                    state = 95usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        95usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        96usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 26usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        26usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=113u8
                                | 115u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                114u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 27usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        27usize => {
                            match byte {
                                48u8..=57u8
                                | 65u8..=90u8
                                | 95u8
                                | 97u8..=100u8
                                | 102u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                101u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::KwWhere;
                                    state = 100usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        100usize => {
                            match byte {
                                48u8..=57u8 | 65u8..=90u8 | 95u8 | 97u8..=122u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Ident;
                                    state = 3usize;
                                }
                                _ => {
                                    let ch = unsafe {
                                        ::lady_deirdre::lexis::LexisSession::read(session)
                                    };
                                    if ::lady_deirdre::lexis::Char::is_xid_continue(ch) {
                                        unsafe {
                                            ::lady_deirdre::lexis::LexisSession::submit(session)
                                        };
                                        token = Self::Ident;
                                        state = 3usize;
                                        continue;
                                    }
                                    break;
                                }
                            }
                        }
                        109usize => {
                            match byte {
                                101u8 => state = 91usize,
                                48u8..=57u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                }
                                _ => break,
                            }
                        }
                        91usize => {
                            match byte {
                                48u8..=57u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                    state = 92usize;
                                }
                                45u8 => state = 93usize,
                                _ => break,
                            }
                        }
                        92usize => {
                            match byte {
                                48u8..=57u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                }
                                _ => break,
                            }
                        }
                        93usize => {
                            match byte {
                                48u8..=57u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::Number;
                                    state = 92usize;
                                }
                                _ => break,
                            }
                        }
                        118usize => {
                            match byte {
                                10u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    break;
                                }
                                9u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 124u8..=126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 98usize;
                                }
                                123u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 119usize;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 98usize;
                                }
                            }
                        }
                        98usize => {
                            match byte {
                                10u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    break;
                                }
                                9u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                }
                            }
                        }
                        119usize => {
                            match byte {
                                10u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    break;
                                }
                                123u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 42usize;
                                }
                                9u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 124u8..=126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 98usize;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 98usize;
                                }
                            }
                        }
                        42usize => {
                            match byte {
                                125u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 41usize;
                                }
                                9u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=124u8
                                | 126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                }
                                10u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 43usize;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                }
                            }
                        }
                        41usize => {
                            match byte {
                                9u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=124u8
                                | 126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 42usize;
                                }
                                10u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 43usize;
                                }
                                125u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::MultilineComment;
                                    state = 44usize;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 42usize;
                                }
                            }
                        }
                        43usize => {
                            match byte {
                                125u8 => state = 102usize,
                                9u8..=10u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=124u8
                                | 126u8 => state = 103usize,
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    state = 103usize;
                                }
                            }
                        }
                        44usize => {
                            match byte {
                                10u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    break;
                                }
                                9u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=126u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 98usize;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::SingleComment;
                                    state = 98usize;
                                }
                            }
                        }
                        102usize => {
                            match byte {
                                9u8..=10u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=124u8
                                | 126u8 => state = 103usize,
                                125u8 => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::submit(session)
                                    };
                                    token = Self::MultilineComment;
                                    break;
                                }
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    };
                                    state = 103usize;
                                }
                            }
                        }
                        103usize => {
                            match byte {
                                125u8 => state = 102usize,
                                9u8..=10u8
                                | 12u8..=13u8
                                | 32u8..=33u8
                                | 40u8..=62u8
                                | 91u8
                                | 93u8
                                | 97u8..=98u8
                                | 100u8..=102u8
                                | 104u8..=105u8
                                | 108u8..=111u8
                                | 114u8..=117u8
                                | 119u8..=120u8
                                | 123u8..=124u8
                                | 126u8 => {}
                                _ => {
                                    unsafe {
                                        ::lady_deirdre::lexis::LexisSession::consume(session)
                                    }
                                }
                            }
                        }
                        #[cfg(debug_assertions)]
                        state => {
                            ::core::panicking::panic_fmt(
                                format_args!("Invalid state {0}.", state),
                            );
                        }
                    }
                }
                token
            }
            #[inline(always)]
            fn eoi() -> Self {
                Self::EOI
            }
            #[inline(always)]
            fn mismatch() -> Self {
                Self::Mismatch
            }
            #[inline(always)]
            fn rule(self) -> ::lady_deirdre::lexis::TokenRule {
                self as u8
            }
            fn rule_name(
                rule: ::lady_deirdre::lexis::TokenRule,
            ) -> ::std::option::Option<&'static str> {
                if Self::EOI as u8 == rule {
                    return ::std::option::Option::Some("EOI");
                }
                if Self::Mismatch as u8 == rule {
                    return ::std::option::Option::Some("Mismatch");
                }
                if Self::KwIf as u8 == rule {
                    return ::std::option::Option::Some("KwIf");
                }
                if Self::KwThen as u8 == rule {
                    return ::std::option::Option::Some("KwThen");
                }
                if Self::KwElse as u8 == rule {
                    return ::std::option::Option::Some("KwElse");
                }
                if Self::KwWhere as u8 == rule {
                    return ::std::option::Option::Some("KwWhere");
                }
                if Self::KwLambda as u8 == rule {
                    return ::std::option::Option::Some("KwLambda");
                }
                if Self::KwNone as u8 == rule {
                    return ::std::option::Option::Some("KwNone");
                }
                if Self::KwTrue as u8 == rule {
                    return ::std::option::Option::Some("KwTrue");
                }
                if Self::KwFalse as u8 == rule {
                    return ::std::option::Option::Some("KwFalse");
                }
                if Self::KwAnd as u8 == rule {
                    return ::std::option::Option::Some("KwAnd");
                }
                if Self::KwOr as u8 == rule {
                    return ::std::option::Option::Some("KwOr");
                }
                if Self::KwXor as u8 == rule {
                    return ::std::option::Option::Some("KwXor");
                }
                if Self::Number as u8 == rule {
                    return ::std::option::Option::Some("Number");
                }
                if Self::Equal as u8 == rule {
                    return ::std::option::Option::Some("Equal");
                }
                if Self::NotEqual as u8 == rule {
                    return ::std::option::Option::Some("NotEqual");
                }
                if Self::Less as u8 == rule {
                    return ::std::option::Option::Some("Less");
                }
                if Self::Greater as u8 == rule {
                    return ::std::option::Option::Some("Greater");
                }
                if Self::LessEqual as u8 == rule {
                    return ::std::option::Option::Some("LessEqual");
                }
                if Self::GreaterEqual as u8 == rule {
                    return ::std::option::Option::Some("GreaterEqual");
                }
                if Self::Plus as u8 == rule {
                    return ::std::option::Option::Some("Plus");
                }
                if Self::Minus as u8 == rule {
                    return ::std::option::Option::Some("Minus");
                }
                if Self::Star as u8 == rule {
                    return ::std::option::Option::Some("Star");
                }
                if Self::Slash as u8 == rule {
                    return ::std::option::Option::Some("Slash");
                }
                if Self::Exclamation as u8 == rule {
                    return ::std::option::Option::Some("Exclamation");
                }
                if Self::Tilde as u8 == rule {
                    return ::std::option::Option::Some("Tilde");
                }
                if Self::Let as u8 == rule {
                    return ::std::option::Option::Some("Let");
                }
                if Self::PipeOperator as u8 == rule {
                    return ::std::option::Option::Some("PipeOperator");
                }
                if Self::Arrow as u8 == rule {
                    return ::std::option::Option::Some("Arrow");
                }
                if Self::ParenthesesOpen as u8 == rule {
                    return ::std::option::Option::Some("ParenthesesOpen");
                }
                if Self::ParenthesesClose as u8 == rule {
                    return ::std::option::Option::Some("ParenthesesClose");
                }
                if Self::BracketOpen as u8 == rule {
                    return ::std::option::Option::Some("BracketOpen");
                }
                if Self::BracketClose as u8 == rule {
                    return ::std::option::Option::Some("BracketClose");
                }
                if Self::Comma as u8 == rule {
                    return ::std::option::Option::Some("Comma");
                }
                if Self::Colon as u8 == rule {
                    return ::std::option::Option::Some("Colon");
                }
                if Self::DoubleColon as u8 == rule {
                    return ::std::option::Option::Some("DoubleColon");
                }
                if Self::Semicolon as u8 == rule {
                    return ::std::option::Option::Some("Semicolon");
                }
                if Self::SingleComment as u8 == rule {
                    return ::std::option::Option::Some("SingleComment");
                }
                if Self::MultilineComment as u8 == rule {
                    return ::std::option::Option::Some("MultilineComment");
                }
                if Self::Whitespace as u8 == rule {
                    return ::std::option::Option::Some("Whitespace");
                }
                if Self::Ident as u8 == rule {
                    return ::std::option::Option::Some("Ident");
                }
                None
            }
            #[allow(unused_variables)]
            fn rule_description(
                rule: ::lady_deirdre::lexis::TokenRule,
                verbose: bool,
            ) -> ::std::option::Option<&'static str> {
                if Self::EOI as u8 == rule {
                    return ::std::option::Option::Some("<eoi>");
                }
                if Self::Mismatch as u8 == rule {
                    return ::std::option::Option::Some("<mismatch>");
                }
                if Self::KwIf as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_if"),
                        true => ::std::option::Option::Some("if"),
                    };
                }
                if Self::KwThen as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_then"),
                        true => ::std::option::Option::Some("then"),
                    };
                }
                if Self::KwElse as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_else"),
                        true => ::std::option::Option::Some("else"),
                    };
                }
                if Self::KwWhere as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_where"),
                        true => ::std::option::Option::Some("where"),
                    };
                }
                if Self::KwLambda as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_lambda"),
                        true => ::std::option::Option::Some("lambda"),
                    };
                }
                if Self::KwNone as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_none"),
                        true => ::std::option::Option::Some("none"),
                    };
                }
                if Self::KwTrue as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_true"),
                        true => ::std::option::Option::Some("true"),
                    };
                }
                if Self::KwFalse as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_false"),
                        true => ::std::option::Option::Some("false"),
                    };
                }
                if Self::KwAnd as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_and"),
                        true => ::std::option::Option::Some("and"),
                    };
                }
                if Self::KwOr as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_or"),
                        true => ::std::option::Option::Some("or"),
                    };
                }
                if Self::KwXor as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("KW_xor"),
                        true => ::std::option::Option::Some("xor"),
                    };
                }
                if Self::Number as u8 == rule {
                    return match verbose {
                        false => ::std::option::Option::Some("number"),
                        true => ::std::option::Option::Some("<number>"),
                    };
                }
                if Self::Equal as u8 == rule {
                    return ::std::option::Option::Some("==");
                }
                if Self::NotEqual as u8 == rule {
                    return ::std::option::Option::Some("!=");
                }
                if Self::Less as u8 == rule {
                    return ::std::option::Option::Some("<");
                }
                if Self::Greater as u8 == rule {
                    return ::std::option::Option::Some(">");
                }
                if Self::LessEqual as u8 == rule {
                    return ::std::option::Option::Some("<=");
                }
                if Self::GreaterEqual as u8 == rule {
                    return ::std::option::Option::Some(">=");
                }
                if Self::Plus as u8 == rule {
                    return ::std::option::Option::Some("+");
                }
                if Self::Minus as u8 == rule {
                    return ::std::option::Option::Some("-");
                }
                if Self::Star as u8 == rule {
                    return ::std::option::Option::Some("*");
                }
                if Self::Slash as u8 == rule {
                    return ::std::option::Option::Some("/");
                }
                if Self::Exclamation as u8 == rule {
                    return ::std::option::Option::Some("!");
                }
                if Self::Tilde as u8 == rule {
                    return ::std::option::Option::Some("~");
                }
                if Self::Let as u8 == rule {
                    return ::std::option::Option::Some("=");
                }
                if Self::PipeOperator as u8 == rule {
                    return ::std::option::Option::Some("|>");
                }
                if Self::Arrow as u8 == rule {
                    return ::std::option::Option::Some("->");
                }
                if Self::ParenthesesOpen as u8 == rule {
                    return ::std::option::Option::Some("(");
                }
                if Self::ParenthesesClose as u8 == rule {
                    return ::std::option::Option::Some(")");
                }
                if Self::BracketOpen as u8 == rule {
                    return ::std::option::Option::Some("[");
                }
                if Self::BracketClose as u8 == rule {
                    return ::std::option::Option::Some("]");
                }
                if Self::Comma as u8 == rule {
                    return ::std::option::Option::Some(",");
                }
                if Self::Colon as u8 == rule {
                    return ::std::option::Option::Some(":");
                }
                if Self::DoubleColon as u8 == rule {
                    return ::std::option::Option::Some("::");
                }
                if Self::Semicolon as u8 == rule {
                    return ::std::option::Option::Some(";");
                }
                if Self::SingleComment as u8 == rule {
                    return ::std::option::Option::Some("<single comment>");
                }
                if Self::MultilineComment as u8 == rule {
                    return ::std::option::Option::Some("<multiline comment>");
                }
                if Self::Whitespace as u8 == rule {
                    return ::std::option::Option::Some("<whitespace>");
                }
                if Self::Ident as u8 == rule {
                    return ::std::option::Option::Some("<ident>");
                }
                None
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for BasicToken {
            #[inline]
            fn clone(&self) -> BasicToken {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for BasicToken {}
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for BasicToken {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for BasicToken {
            #[inline]
            fn eq(&self, other: &BasicToken) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for BasicToken {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {}
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for BasicToken {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(
                    f,
                    match self {
                        BasicToken::EOI => "EOI",
                        BasicToken::Mismatch => "Mismatch",
                        BasicToken::KwIf => "KwIf",
                        BasicToken::KwThen => "KwThen",
                        BasicToken::KwElse => "KwElse",
                        BasicToken::KwWhere => "KwWhere",
                        BasicToken::KwLambda => "KwLambda",
                        BasicToken::KwNone => "KwNone",
                        BasicToken::KwTrue => "KwTrue",
                        BasicToken::KwFalse => "KwFalse",
                        BasicToken::KwAnd => "KwAnd",
                        BasicToken::KwOr => "KwOr",
                        BasicToken::KwXor => "KwXor",
                        BasicToken::Number => "Number",
                        BasicToken::Equal => "Equal",
                        BasicToken::NotEqual => "NotEqual",
                        BasicToken::Less => "Less",
                        BasicToken::Greater => "Greater",
                        BasicToken::LessEqual => "LessEqual",
                        BasicToken::GreaterEqual => "GreaterEqual",
                        BasicToken::Plus => "Plus",
                        BasicToken::Minus => "Minus",
                        BasicToken::Star => "Star",
                        BasicToken::Slash => "Slash",
                        BasicToken::Exclamation => "Exclamation",
                        BasicToken::Tilde => "Tilde",
                        BasicToken::Let => "Let",
                        BasicToken::PipeOperator => "PipeOperator",
                        BasicToken::Arrow => "Arrow",
                        BasicToken::ParenthesesOpen => "ParenthesesOpen",
                        BasicToken::ParenthesesClose => "ParenthesesClose",
                        BasicToken::BracketOpen => "BracketOpen",
                        BasicToken::BracketClose => "BracketClose",
                        BasicToken::Comma => "Comma",
                        BasicToken::Colon => "Colon",
                        BasicToken::DoubleColon => "DoubleColon",
                        BasicToken::Semicolon => "Semicolon",
                        BasicToken::SingleComment => "SingleComment",
                        BasicToken::MultilineComment => "MultilineComment",
                        BasicToken::Whitespace => "Whitespace",
                        BasicToken::Ident => "Ident",
                    },
                )
            }
        }
    }
    pub mod syntax {
        use lady_deirdre::{lexis::TokenRef, syntax::{Node, NodeRef}};
        use super::lexer::BasicToken;
        #[token(BasicToken)]
        #[trivia($Whitespace|$SingleComment|$MultilineComment)]
        #[recovery(
            $ParenthesesClose,
            [$ParenthesesOpen..$ParenthesesClose],
            $BracketClose,
            [$BracketOpen..$BracketClose],
        )]
        pub enum BasicNode {
            #[root]
            #[rule((functions:Function)*)]
            Root {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                functions: Vec<NodeRef>,
            },
            #[rule(name:$Ident(params:$Ident)*$Let(body:XorExpression)$Semicolon)]
            Function {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                name: TokenRef,
                #[child]
                params: Vec<TokenRef>,
                #[child]
                body: NodeRef,
            },
            #[rule((values:OrExpression)+{operators:$KwXor})]
            XorExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
                #[child]
                operators: Vec<TokenRef>,
            },
            #[rule((values:AndExpression)+{operators:$KwOr})]
            OrExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
                #[child]
                operators: Vec<TokenRef>,
            },
            #[rule((values:EqualityExpression)+{operators:$KwAnd})]
            AndExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
                #[child]
                operators: Vec<TokenRef>,
            },
            #[rule(
                (
                    lvalue:SumExpression
                )(
                    (
                        operator:(
                            $Equal|$NotEqual|$Less|$Greater|$GreaterEqual|$LessEqual
                        )
                    )rvalue:SumExpression
                )?
            )]
            EqualityExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                lvalue: NodeRef,
                #[child]
                rvalue: NodeRef,
                #[child]
                operator: TokenRef,
            },
            #[rule((values:MultExpression)+{operators:($Plus|$Minus)})]
            SumExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
                #[child]
                operators: Vec<TokenRef>,
            },
            #[rule((values:PipeExpression)+{operators:($Star|$Slash)})]
            MultExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
                #[child]
                operators: Vec<TokenRef>,
            },
            #[rule((values:FunctionCall)+{operators:$PipeOperator})]
            PipeExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
                #[child]
                operators: Vec<TokenRef>,
            },
            #[rule((values:UnaryExpression)+)]
            FunctionCall {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                values: Vec<NodeRef>,
            },
            #[rule(
                (
                    op:$Exclamation
                )?value:(Literal|ParenthesesExpression|IfExpression|LambdaExpression)
            )]
            UnaryExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                value: NodeRef,
                #[child]
                op: TokenRef,
            },
            #[rule($ParenthesesOpen(value:XorExpression)$ParenthesesClose)]
            ParenthesesExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                value: NodeRef,
            },
            #[rule(value:($Number|$KwTrue|$KwFalse|$Ident|$KwNone))]
            Literal {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                value: TokenRef,
            },
            #[rule($KwLambda(params:$Ident)*$Arrow(body:XorExpression))]
            LambdaExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                params: Vec<TokenRef>,
                #[child]
                body: NodeRef,
            },
            #[rule(
                $KwIf(
                    cond:XorExpression
                )$KwThen(if_true:XorExpression)$KwElse(if_false:XorExpression)
            )]
            IfExpression {
                #[node]
                node: NodeRef,
                #[parent]
                parent: NodeRef,
                #[child]
                cond: NodeRef,
                #[child]
                if_true: NodeRef,
                #[child]
                if_false: NodeRef,
            },
        }
        impl ::lady_deirdre::analysis::AbstractFeature for BasicNode {
            fn attr_ref(&self) -> &::lady_deirdre::analysis::AttrRef {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => &::lady_deirdre::analysis::NIL_ATTR_REF,
                }
            }
            fn slot_ref(&self) -> &::lady_deirdre::analysis::SlotRef {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => &::lady_deirdre::analysis::NIL_SLOT_REF,
                }
            }
            #[allow(unused_variables)]
            fn feature(
                &self,
                key: ::lady_deirdre::syntax::Key,
            ) -> ::lady_deirdre::analysis::AnalysisResult<
                &dyn ::lady_deirdre::analysis::AbstractFeature,
            > {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => {
                        ::std::result::Result::Err(
                            ::lady_deirdre::analysis::AnalysisError::MissingFeature,
                        )
                    }
                }
            }
            #[allow(unused_variables)]
            fn feature_keys(&self) -> &'static [&'static ::lady_deirdre::syntax::Key] {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => &[],
                }
            }
        }
        impl ::lady_deirdre::analysis::Grammar for BasicNode {
            type Classifier = ::lady_deirdre::analysis::VoidClassifier<Self>;
            type CommonSemantics = ::lady_deirdre::analysis::VoidFeature<Self>;
            #[allow(unused_variables)]
            fn init<
                H: ::lady_deirdre::analysis::TaskHandle,
                S: ::lady_deirdre::sync::SyncBuildHasher,
            >(
                &mut self,
                #[allow(unused)]
                initializer: &mut ::lady_deirdre::analysis::Initializer<Self, H, S>,
            ) {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => {}
                }
            }
            #[allow(unused_variables)]
            fn invalidate<
                H: ::lady_deirdre::analysis::TaskHandle,
                S: ::lady_deirdre::sync::SyncBuildHasher,
            >(
                &self,
                invalidator: &mut ::lady_deirdre::analysis::Invalidator<Self, H, S>,
            ) {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => {}
                }
            }
            fn scope_attr(
                &self,
            ) -> ::lady_deirdre::analysis::AnalysisResult<
                &::lady_deirdre::analysis::ScopeAttr<Self>,
            > {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => {
                        ::std::result::Result::Err(
                            ::lady_deirdre::analysis::AnalysisError::MissingSemantics,
                        )
                    }
                }
            }
            #[inline(always)]
            fn is_scope(&self) -> bool {
                match self {
                    #[allow(unreachable_patterns)]
                    _ => false,
                }
            }
        }
        impl ::lady_deirdre::syntax::AbstractNode for BasicNode {
            fn rule(&self) -> ::lady_deirdre::syntax::NodeRule {
                match self {
                    Self::Root { .. } => 0u16,
                    Self::Function { .. } => 1u16,
                    Self::XorExpression { .. } => 2u16,
                    Self::OrExpression { .. } => 3u16,
                    Self::AndExpression { .. } => 4u16,
                    Self::EqualityExpression { .. } => 5u16,
                    Self::SumExpression { .. } => 6u16,
                    Self::MultExpression { .. } => 7u16,
                    Self::PipeExpression { .. } => 8u16,
                    Self::FunctionCall { .. } => 9u16,
                    Self::UnaryExpression { .. } => 10u16,
                    Self::IfExpression { .. } => 11u16,
                    Self::Literal { .. } => 12u16,
                    Self::LambdaExpression { .. } => 13u16,
                    Self::ParenthesesExpression { .. } => 14u16,
                    #[allow(unreachable_patterns)]
                    _ => ::lady_deirdre::syntax::NON_RULE,
                }
            }
            #[inline(always)]
            fn name(&self) -> ::std::option::Option<&'static str> {
                Self::rule_name(self.rule())
            }
            #[inline(always)]
            fn describe(&self, verbose: bool) -> ::std::option::Option<&'static str> {
                Self::rule_description(self.rule(), verbose)
            }
            fn node_ref(&self) -> ::lady_deirdre::syntax::NodeRef {
                match self {
                    Self::ParenthesesExpression { node, .. } => *node,
                    Self::FunctionCall { node, .. } => *node,
                    Self::Literal { node, .. } => *node,
                    Self::IfExpression { node, .. } => *node,
                    Self::MultExpression { node, .. } => *node,
                    Self::EqualityExpression { node, .. } => *node,
                    Self::AndExpression { node, .. } => *node,
                    Self::PipeExpression { node, .. } => *node,
                    Self::XorExpression { node, .. } => *node,
                    Self::Function { node, .. } => *node,
                    Self::OrExpression { node, .. } => *node,
                    Self::LambdaExpression { node, .. } => *node,
                    Self::SumExpression { node, .. } => *node,
                    Self::UnaryExpression { node, .. } => *node,
                    Self::Root { node, .. } => *node,
                    #[allow(unreachable_patterns)]
                    _ => ::lady_deirdre::syntax::NodeRef::nil(),
                }
            }
            fn parent_ref(&self) -> ::lady_deirdre::syntax::NodeRef {
                match self {
                    Self::ParenthesesExpression { parent, .. } => *parent,
                    Self::FunctionCall { parent, .. } => *parent,
                    Self::Literal { parent, .. } => *parent,
                    Self::IfExpression { parent, .. } => *parent,
                    Self::MultExpression { parent, .. } => *parent,
                    Self::EqualityExpression { parent, .. } => *parent,
                    Self::AndExpression { parent, .. } => *parent,
                    Self::PipeExpression { parent, .. } => *parent,
                    Self::XorExpression { parent, .. } => *parent,
                    Self::Function { parent, .. } => *parent,
                    Self::OrExpression { parent, .. } => *parent,
                    Self::LambdaExpression { parent, .. } => *parent,
                    Self::SumExpression { parent, .. } => *parent,
                    Self::UnaryExpression { parent, .. } => *parent,
                    Self::Root { parent, .. } => *parent,
                    #[allow(unreachable_patterns)]
                    _ => ::lady_deirdre::syntax::NodeRef::nil(),
                }
            }
            #[allow(unused_variables)]
            fn set_parent_ref(&mut self, parent_ref: ::lady_deirdre::syntax::NodeRef) {
                match self {
                    Self::ParenthesesExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::FunctionCall { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::Literal { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::IfExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::MultExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::EqualityExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::AndExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::PipeExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::XorExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::Function { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::OrExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::LambdaExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::SumExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::UnaryExpression { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    Self::Root { parent: target, .. } => {
                        *target = parent_ref;
                    }
                    #[allow(unreachable_patterns)]
                    _ => {}
                }
            }
            #[allow(unused_variables)]
            fn capture(
                &self,
                key: ::lady_deirdre::syntax::Key,
            ) -> ::std::option::Option<::lady_deirdre::syntax::Capture> {
                match self {
                    Self::ParenthesesExpression { value: _0, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("value") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::FunctionCall { values: _0, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::Literal { value: _0, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("value") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::IfExpression { cond: _0, if_true: _1, if_false: _2, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("cond") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("if_true") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(2usize)
                            | ::lady_deirdre::syntax::Key::Name("if_false") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_2),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::MultExpression { values: _0, operators: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("operators") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::EqualityExpression {
                        lvalue: _0,
                        rvalue: _1,
                        operator: _2,
                        ..
                    } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("lvalue") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("rvalue") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(2usize)
                            | ::lady_deirdre::syntax::Key::Name("operator") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_2),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::AndExpression { values: _0, operators: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("operators") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::PipeExpression { values: _0, operators: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("operators") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::XorExpression { values: _0, operators: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("operators") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::Function { name: _0, params: _1, body: _2, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("name") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("params") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(2usize)
                            | ::lady_deirdre::syntax::Key::Name("body") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_2),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::OrExpression { values: _0, operators: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("operators") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::LambdaExpression { params: _0, body: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("params") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("body") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::SumExpression { values: _0, operators: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("values") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("operators") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::UnaryExpression { value: _0, op: _1, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("value") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            ::lady_deirdre::syntax::Key::Index(1usize)
                            | ::lady_deirdre::syntax::Key::Name("op") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_1),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    Self::Root { functions: _0, .. } => {
                        match key {
                            ::lady_deirdre::syntax::Key::Index(0usize)
                            | ::lady_deirdre::syntax::Key::Name("functions") => {
                                ::std::option::Option::Some(
                                    <::lady_deirdre::syntax::Capture as ::std::convert::From<
                                        _,
                                    >>::from(_0),
                                )
                            }
                            _ => ::std::option::Option::None,
                        }
                    }
                    #[allow(unreachable_patterns)]
                    _ => ::std::option::Option::None,
                }
            }
            #[allow(unused_variables)]
            fn capture_keys(&self) -> &'static [::lady_deirdre::syntax::Key<'static>] {
                match self {
                    Self::ParenthesesExpression { .. } => {
                        &[::lady_deirdre::syntax::Key::Name("value")]
                    }
                    Self::FunctionCall { .. } => {
                        &[::lady_deirdre::syntax::Key::Name("values")]
                    }
                    Self::Literal { .. } => &[::lady_deirdre::syntax::Key::Name("value")],
                    Self::IfExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("cond"),
                            ::lady_deirdre::syntax::Key::Name("if_true"),
                            ::lady_deirdre::syntax::Key::Name("if_false"),
                        ]
                    }
                    Self::MultExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("values"),
                            ::lady_deirdre::syntax::Key::Name("operators"),
                        ]
                    }
                    Self::EqualityExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("lvalue"),
                            ::lady_deirdre::syntax::Key::Name("rvalue"),
                            ::lady_deirdre::syntax::Key::Name("operator"),
                        ]
                    }
                    Self::AndExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("values"),
                            ::lady_deirdre::syntax::Key::Name("operators"),
                        ]
                    }
                    Self::PipeExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("values"),
                            ::lady_deirdre::syntax::Key::Name("operators"),
                        ]
                    }
                    Self::XorExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("values"),
                            ::lady_deirdre::syntax::Key::Name("operators"),
                        ]
                    }
                    Self::Function { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("name"),
                            ::lady_deirdre::syntax::Key::Name("params"),
                            ::lady_deirdre::syntax::Key::Name("body"),
                        ]
                    }
                    Self::OrExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("values"),
                            ::lady_deirdre::syntax::Key::Name("operators"),
                        ]
                    }
                    Self::LambdaExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("params"),
                            ::lady_deirdre::syntax::Key::Name("body"),
                        ]
                    }
                    Self::SumExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("values"),
                            ::lady_deirdre::syntax::Key::Name("operators"),
                        ]
                    }
                    Self::UnaryExpression { .. } => {
                        &[
                            ::lady_deirdre::syntax::Key::Name("value"),
                            ::lady_deirdre::syntax::Key::Name("op"),
                        ]
                    }
                    Self::Root { .. } => {
                        &[::lady_deirdre::syntax::Key::Name("functions")]
                    }
                    #[allow(unreachable_patterns)]
                    _ => &[],
                }
            }
            #[allow(unused_variables)]
            fn rule_name(
                rule: ::lady_deirdre::syntax::NodeRule,
            ) -> ::std::option::Option<&'static str> {
                match rule {
                    0u16 => ::std::option::Option::Some("Root"),
                    1u16 => ::std::option::Option::Some("Function"),
                    2u16 => ::std::option::Option::Some("XorExpression"),
                    3u16 => ::std::option::Option::Some("OrExpression"),
                    4u16 => ::std::option::Option::Some("AndExpression"),
                    5u16 => ::std::option::Option::Some("EqualityExpression"),
                    6u16 => ::std::option::Option::Some("SumExpression"),
                    7u16 => ::std::option::Option::Some("MultExpression"),
                    8u16 => ::std::option::Option::Some("PipeExpression"),
                    9u16 => ::std::option::Option::Some("FunctionCall"),
                    10u16 => ::std::option::Option::Some("UnaryExpression"),
                    11u16 => ::std::option::Option::Some("IfExpression"),
                    12u16 => ::std::option::Option::Some("Literal"),
                    13u16 => ::std::option::Option::Some("LambdaExpression"),
                    14u16 => ::std::option::Option::Some("ParenthesesExpression"),
                    #[allow(unreachable_patterns)]
                    _ => None,
                }
            }
            #[allow(unused_variables)]
            fn rule_description(
                rule: ::lady_deirdre::syntax::NodeRule,
                verbose: bool,
            ) -> ::std::option::Option<&'static str> {
                match rule {
                    0u16 => ::std::option::Option::Some("Root"),
                    1u16 => ::std::option::Option::Some("Function"),
                    2u16 => ::std::option::Option::Some("Xor Expression"),
                    3u16 => ::std::option::Option::Some("Or Expression"),
                    4u16 => ::std::option::Option::Some("And Expression"),
                    5u16 => ::std::option::Option::Some("Equality Expression"),
                    6u16 => ::std::option::Option::Some("Sum Expression"),
                    7u16 => ::std::option::Option::Some("Mult Expression"),
                    8u16 => ::std::option::Option::Some("Pipe Expression"),
                    9u16 => ::std::option::Option::Some("Function Call"),
                    10u16 => ::std::option::Option::Some("Unary Expression"),
                    11u16 => ::std::option::Option::Some("If Expression"),
                    12u16 => ::std::option::Option::Some("Literal"),
                    13u16 => ::std::option::Option::Some("Lambda Expression"),
                    14u16 => ::std::option::Option::Some("Parentheses Expression"),
                    #[allow(unreachable_patterns)]
                    _ => None,
                }
            }
        }
        impl ::lady_deirdre::syntax::Node for BasicNode {
            type Token = BasicToken;
            #[inline(always)]
            fn parse<'code>(
                session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                    'code,
                    Node = Self,
                >,
                rule: ::lady_deirdre::syntax::NodeRule,
            ) -> Self {
                static RECOVERY_1: ::lady_deirdre::syntax::Recovery = ::lady_deirdre::syntax::Recovery::unlimited()
                    .unexpected_set(
                        ::lady_deirdre::lexis::TokenSet::inclusive(
                            &[
                                BasicToken::BracketClose as u8,
                                BasicToken::ParenthesesClose as u8,
                            ],
                        ),
                    )
                    .group(BasicToken::BracketOpen as u8, BasicToken::BracketClose as u8)
                    .group(
                        BasicToken::ParenthesesOpen as u8,
                        BasicToken::ParenthesesClose as u8,
                    );
                static RULES_1: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[2u16],
                );
                static RULES_2: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[10u16],
                );
                static RULES_3: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[8u16],
                );
                static RULES_4: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[6u16],
                );
                static RULES_5: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[5u16],
                );
                static RULES_6: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[9u16],
                );
                static RULES_7: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[3u16],
                );
                static RULES_8: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[4u16],
                );
                static RULES_9: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[7u16],
                );
                static RULES_10: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[11u16, 12u16, 13u16, 14u16],
                );
                static RULES_11: ::lady_deirdre::syntax::NodeSet = ::lady_deirdre::syntax::NodeSet::new(
                    &[1u16],
                );
                static TOKENS_1: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::ParenthesesOpen as u8],
                );
                static TOKENS_2: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::ParenthesesClose as u8,
                        BasicToken::ParenthesesOpen as u8,
                    ],
                );
                static TOKENS_3: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Exclamation as u8,
                        BasicToken::Ident as u8,
                        BasicToken::KwFalse as u8,
                        BasicToken::KwIf as u8,
                        BasicToken::KwLambda as u8,
                        BasicToken::KwNone as u8,
                        BasicToken::KwTrue as u8,
                        BasicToken::Number as u8,
                        BasicToken::ParenthesesOpen as u8,
                    ],
                );
                static TOKENS_4: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Exclamation as u8,
                        BasicToken::Ident as u8,
                        BasicToken::KwFalse as u8,
                        BasicToken::KwIf as u8,
                        BasicToken::KwLambda as u8,
                        BasicToken::KwNone as u8,
                        BasicToken::KwTrue as u8,
                        BasicToken::Number as u8,
                        BasicToken::ParenthesesClose as u8,
                        BasicToken::ParenthesesOpen as u8,
                    ],
                );
                static TOKENS_5: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::ParenthesesClose as u8],
                );
                static TOKENS_6: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Ident as u8,
                        BasicToken::KwFalse as u8,
                        BasicToken::KwNone as u8,
                        BasicToken::KwTrue as u8,
                        BasicToken::Number as u8,
                    ],
                );
                static TOKENS_7: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwIf as u8],
                );
                static TOKENS_8: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwElse as u8],
                );
                static TOKENS_9: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwThen as u8],
                );
                static TOKENS_10: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Slash as u8, BasicToken::Star as u8],
                );
                static TOKENS_11: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Equal as u8,
                        BasicToken::Greater as u8,
                        BasicToken::GreaterEqual as u8,
                        BasicToken::Less as u8,
                        BasicToken::LessEqual as u8,
                        BasicToken::NotEqual as u8,
                    ],
                );
                static TOKENS_12: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwAnd as u8],
                );
                static TOKENS_13: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::PipeOperator as u8],
                );
                static TOKENS_14: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwXor as u8],
                );
                static TOKENS_15: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Ident as u8],
                );
                static TOKENS_16: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Ident as u8, BasicToken::Semicolon as u8],
                );
                static TOKENS_17: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Exclamation as u8,
                        BasicToken::Ident as u8,
                        BasicToken::KwFalse as u8,
                        BasicToken::KwIf as u8,
                        BasicToken::KwLambda as u8,
                        BasicToken::KwNone as u8,
                        BasicToken::KwTrue as u8,
                        BasicToken::Number as u8,
                        BasicToken::ParenthesesOpen as u8,
                        BasicToken::Semicolon as u8,
                    ],
                );
                static TOKENS_18: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Semicolon as u8],
                );
                static TOKENS_19: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Ident as u8, BasicToken::Let as u8],
                );
                static TOKENS_20: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Ident as u8,
                        BasicToken::Let as u8,
                        BasicToken::Semicolon as u8,
                    ],
                );
                static TOKENS_21: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwOr as u8],
                );
                static TOKENS_22: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::KwLambda as u8],
                );
                static TOKENS_23: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Arrow as u8, BasicToken::Ident as u8],
                );
                static TOKENS_24: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Minus as u8, BasicToken::Plus as u8],
                );
                static TOKENS_25: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Exclamation as u8],
                );
                static TOKENS_26: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::Ident as u8,
                        BasicToken::KwFalse as u8,
                        BasicToken::KwIf as u8,
                        BasicToken::KwLambda as u8,
                        BasicToken::KwNone as u8,
                        BasicToken::KwTrue as u8,
                        BasicToken::Number as u8,
                        BasicToken::ParenthesesOpen as u8,
                    ],
                );
                static TOKENS_27: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[BasicToken::Ident as u8, ::lady_deirdre::lexis::EOI],
                );
                static TOKENS_28: ::lady_deirdre::lexis::TokenSet = ::lady_deirdre::lexis::TokenSet::inclusive(
                    &[
                        BasicToken::MultilineComment as u8,
                        BasicToken::SingleComment as u8,
                        BasicToken::Whitespace as u8,
                    ],
                );
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn skip_trivia<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) {
                    let mut state = 1usize;
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_28,
                                    token as u8,
                                ) {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                break;
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                }
                #[cfg(debug_assertions)]
                #[allow(dead_code)]
                const CHECK_EOI: () = {
                    if BasicToken::KwXor as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Star as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Slash as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::ParenthesesClose as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwElse as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::ParenthesesOpen as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::LessEqual as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::MultilineComment as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Ident as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwTrue as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Number as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Less as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Exclamation as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::SingleComment as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Equal as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::NotEqual as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Greater as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::PipeOperator as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwIf as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwFalse as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Semicolon as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwAnd as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Plus as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwNone as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Minus as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Let as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwOr as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwThen as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::KwLambda as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::GreaterEqual as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Arrow as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    if BasicToken::Whitespace as u8 == ::lady_deirdre::lexis::EOI {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("EOI token cannot be used directly."),
                            );
                        };
                    }
                    ()
                };
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_ParenthesesExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_value = ::lady_deirdre::syntax::NodeRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::ParenthesesOpen {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_2,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 14u16,
                                        recovery,
                                        expected_tokens: &TOKENS_1,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if recovered {
                                    if ::lady_deirdre::lexis::TokenCursor::token(session, 0)
                                        == BasicToken::ParenthesesClose
                                    {
                                        ::lady_deirdre::lexis::TokenCursor::advance(session);
                                        recovered = false;
                                    }
                                }
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                if token == BasicToken::ParenthesesClose {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 14u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_1,
                                        },
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_4,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 14u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_1,
                                    },
                                );
                                if recovered {
                                    if ::lady_deirdre::lexis::TokenCursor::token(session, 0)
                                        == BasicToken::ParenthesesClose
                                    {
                                        ::lady_deirdre::lexis::TokenCursor::advance(session);
                                        recovered = false;
                                    }
                                }
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::ParenthesesClose {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_5,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 14u16,
                                        recovery,
                                        expected_tokens: &TOKENS_5,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::ParenthesesExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        value: capture_value,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_FunctionCall<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            10u16,
                                        ),
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 9u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_2,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            10u16,
                                        ),
                                    );
                                    continue;
                                }
                                break;
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::FunctionCall {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_Literal<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_value = ::lady_deirdre::lexis::TokenRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwFalse {
                                    capture_value = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                if token == BasicToken::KwNone {
                                    capture_value = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                if token == BasicToken::KwTrue {
                                    capture_value = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                if token == BasicToken::Number {
                                    capture_value = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                if token == BasicToken::Ident {
                                    capture_value = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_6,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 12u16,
                                        recovery,
                                        expected_tokens: &TOKENS_6,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::Literal {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        value: capture_value,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_IfExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_if_false = ::lady_deirdre::syntax::NodeRef::nil();
                    let mut capture_if_true = ::lady_deirdre::syntax::NodeRef::nil();
                    let mut capture_cond = ::lady_deirdre::syntax::NodeRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwIf {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 6usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_7,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 11u16,
                                        recovery,
                                        expected_tokens: &TOKENS_7,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwThen {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 11u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_9,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    capture_if_true = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    state = 7usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_9,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 11u16,
                                        recovery,
                                        expected_tokens: &TOKENS_9,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwElse {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 11u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_1,
                                        },
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 4usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_if_true = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    state = 7usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 11u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_1,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            4usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_if_false = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 11u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_1,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            6usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_cond = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                if token == BasicToken::KwThen {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 11u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_1,
                                        },
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 11u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_1,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            7usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwElse {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 4usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 11u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_8,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    capture_if_false = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_8,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 11u16,
                                        recovery,
                                        expected_tokens: &TOKENS_8,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::IfExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        cond: capture_cond,
                        if_true: capture_if_true,
                        if_false: capture_if_false,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_MultExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_operators = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            8u16,
                                        ),
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_10,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 7u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_3,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 7u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_3,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_10,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                break;
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            8u16,
                                        ),
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_10,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 7u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_3,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 7u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_3,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::MultExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                        operators: capture_operators,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_EqualityExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_operator = ::lady_deirdre::lexis::TokenRef::nil();
                    let mut capture_rvalue = ::lady_deirdre::syntax::NodeRef::nil();
                    let mut capture_lvalue = ::lady_deirdre::syntax::NodeRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_lvalue = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        6u16,
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_11,
                                    token as u8,
                                ) {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 5u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_4,
                                        },
                                    );
                                    capture_operator = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 5u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_4,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_11,
                                    token as u8,
                                ) {
                                    capture_operator = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                break;
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_rvalue = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        6u16,
                                    );
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 5u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_4,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::EqualityExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        lvalue: capture_lvalue,
                        rvalue: capture_rvalue,
                        operator: capture_operator,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_AndExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    let mut capture_operators = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwAnd {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 4u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_5,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            5u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 4u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_5,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwAnd {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 4u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_5,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            5u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 4u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_5,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if token == BasicToken::KwAnd {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 4u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_12,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            5u16,
                                        ),
                                    );
                                    continue;
                                }
                                break;
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::AndExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                        operators: capture_operators,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_PipeExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    let mut capture_operators = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            9u16,
                                        ),
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                if token == BasicToken::PipeOperator {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 8u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_6,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 8u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_6,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 8u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_13,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            9u16,
                                        ),
                                    );
                                    continue;
                                }
                                if token == BasicToken::PipeOperator {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                break;
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            9u16,
                                        ),
                                    );
                                    state = 2usize;
                                    continue;
                                }
                                if token == BasicToken::PipeOperator {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 8u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_6,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 8u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_6,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::PipeExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                        operators: capture_operators,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_XorExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    let mut capture_operators = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwXor {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 2u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_7,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            3u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 2u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_7,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwXor {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 2u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_7,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            3u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 2u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_7,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if token == BasicToken::KwXor {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 2u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_14,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            3u16,
                                        ),
                                    );
                                    continue;
                                }
                                break;
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::XorExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                        operators: capture_operators,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_Function<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_name = ::lady_deirdre::lexis::TokenRef::nil();
                    let mut capture_params = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    let mut capture_body = ::lady_deirdre::syntax::NodeRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::Ident {
                                    capture_name = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_16,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 1u16,
                                        recovery,
                                        expected_tokens: &TOKENS_15,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if recovered {
                                    if ::lady_deirdre::lexis::TokenCursor::token(session, 0)
                                        == BasicToken::Semicolon
                                    {
                                        ::lady_deirdre::lexis::TokenCursor::advance(session);
                                        recovered = false;
                                    }
                                }
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::Ident {
                                    ::std::vec::Vec::push(
                                        &mut capture_params,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                if token == BasicToken::Let {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_20,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 1u16,
                                        recovery,
                                        expected_tokens: &TOKENS_19,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if recovered {
                                    if ::lady_deirdre::lexis::TokenCursor::token(session, 0)
                                        == BasicToken::Semicolon
                                    {
                                        ::lady_deirdre::lexis::TokenCursor::advance(session);
                                        recovered = false;
                                    }
                                }
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_body = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    state = 4usize;
                                    continue;
                                }
                                if token == BasicToken::Semicolon {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 1u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_1,
                                        },
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_17,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 1u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_1,
                                    },
                                );
                                if recovered {
                                    if ::lady_deirdre::lexis::TokenCursor::token(session, 0)
                                        == BasicToken::Semicolon
                                    {
                                        ::lady_deirdre::lexis::TokenCursor::advance(session);
                                        recovered = false;
                                    }
                                }
                                if !recovered {
                                    break;
                                }
                            }
                            4usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::Semicolon {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_18,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 1u16,
                                        recovery,
                                        expected_tokens: &TOKENS_18,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::Function {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        name: capture_name,
                        params: capture_params,
                        body: capture_body,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_OrExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    let mut capture_operators = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwOr {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 3u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_8,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            4u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 3u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_8,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwOr {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 3u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_8,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            4u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 3u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_8,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if token == BasicToken::KwOr {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 3u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_21,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            4u16,
                                        ),
                                    );
                                    continue;
                                }
                                break;
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::OrExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                        operators: capture_operators,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_LambdaExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_params = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    let mut capture_body = ::lady_deirdre::syntax::NodeRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwLambda {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if token == BasicToken::Ident {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 13u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_22,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_params,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if token == BasicToken::Arrow {
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 13u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &TOKENS_22,
                                            expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                        },
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_22,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 13u16,
                                        recovery,
                                        expected_tokens: &TOKENS_22,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::Ident {
                                    ::std::vec::Vec::push(
                                        &mut capture_params,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                if token == BasicToken::Arrow {
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_23,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 13u16,
                                        recovery,
                                        expected_tokens: &TOKENS_23,
                                        expected_nodes: &::lady_deirdre::syntax::EMPTY_NODE_SET,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    capture_body = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        2u16,
                                    );
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 13u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_1,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::LambdaExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        params: capture_params,
                        body: capture_body,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_SumExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_values = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    let mut capture_operators = ::std::vec::Vec::<
                        ::lady_deirdre::lexis::TokenRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_24,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 6u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_9,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            7u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 6u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_9,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            2usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_24,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::NodeRef::nil(),
                                    );
                                    let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::syntax::SyntaxSession::failure(
                                        session,
                                        ::lady_deirdre::syntax::SyntaxError {
                                            span: step_start_ref..step_end_ref,
                                            context: 6u16,
                                            recovery: ::lady_deirdre::syntax::RecoveryResult::InsertRecover,
                                            expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                            expected_nodes: &RULES_9,
                                        },
                                    );
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    continue;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_3,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_values,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            7u16,
                                        ),
                                    );
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 6u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_9,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_24,
                                    token as u8,
                                ) {
                                    ::std::vec::Vec::push(
                                        &mut capture_operators,
                                        ::lady_deirdre::lexis::TokenCursor::token_ref(session, 0),
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 2usize;
                                    continue;
                                }
                                break;
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::SumExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        values: capture_values,
                        operators: capture_operators,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_UnaryExpression<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut first = true;
                    let mut capture_value = ::lady_deirdre::syntax::NodeRef::nil();
                    let mut capture_op = ::lady_deirdre::lexis::TokenRef::nil();
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        match first {
                            true => first = false,
                            false => skip_trivia(session),
                        }
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwIf {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        11u16,
                                    );
                                    break;
                                }
                                if token == BasicToken::KwLambda {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        13u16,
                                    );
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_6,
                                    token as u8,
                                ) {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        12u16,
                                    );
                                    break;
                                }
                                if token == BasicToken::ParenthesesOpen {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        14u16,
                                    );
                                    break;
                                }
                                if token == BasicToken::Exclamation {
                                    capture_op = ::lady_deirdre::lexis::TokenCursor::token_ref(
                                        session,
                                        0,
                                    );
                                    ::lady_deirdre::lexis::TokenCursor::advance(session);
                                    state = 3usize;
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_3,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 10u16,
                                        recovery,
                                        expected_tokens: &TOKENS_25,
                                        expected_nodes: &RULES_10,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            3usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token == BasicToken::KwIf {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        11u16,
                                    );
                                    break;
                                }
                                if token == BasicToken::KwLambda {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        13u16,
                                    );
                                    break;
                                }
                                if ::lady_deirdre::lexis::TokenSet::contains(
                                    &TOKENS_6,
                                    token as u8,
                                ) {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        12u16,
                                    );
                                    break;
                                }
                                if token == BasicToken::ParenthesesOpen {
                                    capture_value = ::lady_deirdre::syntax::SyntaxSession::descend(
                                        session,
                                        14u16,
                                    );
                                    break;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_26,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 10u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_10,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::UnaryExpression {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        value: capture_value,
                        op: capture_op,
                    }
                }
                #[allow(unused)]
                #[allow(unused_mut)]
                #[allow(unused_assignments)]
                #[allow(unused_variables)]
                #[allow(non_snake_case)]
                fn parse_Root<'code>(
                    session: &mut impl ::lady_deirdre::syntax::SyntaxSession<
                        'code,
                        Node = BasicNode,
                    >,
                ) -> BasicNode {
                    let mut state = 1usize;
                    let mut capture_functions = ::std::vec::Vec::<
                        ::lady_deirdre::syntax::NodeRef,
                    >::with_capacity(1);
                    loop {
                        let step_start_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                            session,
                            0,
                        );
                        skip_trivia(session);
                        match state {
                            1usize => {
                                let token = ::lady_deirdre::lexis::TokenCursor::token(
                                    session,
                                    0,
                                );
                                if token
                                    == <BasicToken as ::lady_deirdre::lexis::Token>::eoi()
                                {
                                    break;
                                }
                                if token == BasicToken::Ident {
                                    ::std::vec::Vec::push(
                                        &mut capture_functions,
                                        ::lady_deirdre::syntax::SyntaxSession::descend(
                                            session,
                                            1u16,
                                        ),
                                    );
                                    continue;
                                }
                                let recovery = ::lady_deirdre::syntax::Recovery::recover(
                                    &RECOVERY_1,
                                    session,
                                    &TOKENS_27,
                                );
                                let mut recovered = ::lady_deirdre::syntax::RecoveryResult::recovered(
                                    &recovery,
                                );
                                let step_end_ref = ::lady_deirdre::lexis::TokenCursor::site_ref(
                                    session,
                                    0,
                                );
                                ::lady_deirdre::syntax::SyntaxSession::failure(
                                    session,
                                    ::lady_deirdre::syntax::SyntaxError {
                                        span: step_start_ref..step_end_ref,
                                        context: 0u16,
                                        recovery,
                                        expected_tokens: &::lady_deirdre::lexis::EMPTY_TOKEN_SET,
                                        expected_nodes: &RULES_11,
                                    },
                                );
                                if !recovered {
                                    break;
                                }
                            }
                            other => {
                                ::core::panicking::panic_fmt(
                                    format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("Unknown state {0}.", other),
                                    ),
                                );
                            }
                        }
                    }
                    BasicNode::Root {
                        node: ::lady_deirdre::syntax::SyntaxSession::node_ref(session),
                        parent: ::lady_deirdre::syntax::SyntaxSession::parent_ref(
                            session,
                        ),
                        functions: capture_functions,
                    }
                }
                match rule {
                    14u16 => parse_ParenthesesExpression(session),
                    9u16 => parse_FunctionCall(session),
                    12u16 => parse_Literal(session),
                    11u16 => parse_IfExpression(session),
                    7u16 => parse_MultExpression(session),
                    5u16 => parse_EqualityExpression(session),
                    4u16 => parse_AndExpression(session),
                    8u16 => parse_PipeExpression(session),
                    2u16 => parse_XorExpression(session),
                    1u16 => parse_Function(session),
                    3u16 => parse_OrExpression(session),
                    13u16 => parse_LambdaExpression(session),
                    6u16 => parse_SumExpression(session),
                    10u16 => parse_UnaryExpression(session),
                    0u16 => parse_Root(session),
                    #[allow(unreachable_patterns)]
                    other => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "not implemented: {0}",
                                format_args!("Unsupported rule {0}.", other),
                            ),
                        );
                    }
                }
            }
        }
    }
    pub struct Parser<'a> {
        doc: &'a Document<BasicNode>,
    }
    impl<'a> Parser<'a> {
        pub fn new(doc: &'a Document<BasicNode>) -> Self {
            Self { doc }
        }
        fn get_node_position(&self, node: &NodeRef) -> PositionSpan {
            node.span(self.doc).unwrap().to_position_span(self.doc).unwrap()
        }
        pub fn parse_function(&self, node: NodeRef) -> FunctionExpression {
            let Some(BasicNode::Function { name, params, body, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            let position = name
                .chunk(self.doc)
                .unwrap()
                .to_position_span(self.doc)
                .unwrap();
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
            let Some(BasicNode::XorExpression { values, operators, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
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
                    result = Some(
                        if let Some(lhs) = result {
                            let position = lhs.position.start..rhs.position.end;
                            let operator = operators[idx - 1].clone();
                            ValueExpression {
                                position,
                                inner: ValueExpressionInner::Binary(
                                    Box::new(BinaryExpression {
                                        lhs: lhs.clone(),
                                        rhs: rhs.clone(),
                                        operator,
                                    }),
                                ),
                            }
                        } else {
                            rhs
                        },
                    );
                }
                result.unwrap()
            }
        }
        pub fn parse_or_expression(&self, node: NodeRef) -> ValueExpression {
            let Some(BasicNode::OrExpression { values, operators, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
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
                    result = Some(
                        if let Some(lhs) = result {
                            let position = lhs.position.start..rhs.position.end;
                            let operator = operators[idx - 1].clone();
                            ValueExpression {
                                position,
                                inner: ValueExpressionInner::Binary(
                                    Box::new(BinaryExpression {
                                        lhs: lhs.clone(),
                                        rhs: rhs.clone(),
                                        operator,
                                    }),
                                ),
                            }
                        } else {
                            rhs
                        },
                    );
                }
                result.unwrap()
            }
        }
        pub fn parse_and_expression(&self, node: NodeRef) -> ValueExpression {
            let Some(BasicNode::AndExpression { values, operators, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
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
                    result = Some(
                        if let Some(lhs) = result {
                            let position = lhs.position.start..rhs.position.end;
                            let operator = operators[idx - 1].clone();
                            ValueExpression {
                                position,
                                inner: ValueExpressionInner::Binary(
                                    Box::new(BinaryExpression {
                                        lhs: lhs.clone(),
                                        rhs: rhs.clone(),
                                        operator,
                                    }),
                                ),
                            }
                        } else {
                            rhs
                        },
                    );
                }
                result.unwrap()
            }
        }
        pub fn parse_eq_expression(&self, node: NodeRef) -> ValueExpression {
            let position = self.get_node_position(&node);
            let Some(BasicNode::EqualityExpression { lvalue, rvalue, operator, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
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
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    },
                };
                let rhs = self.parse_sum_expression(*rvalue);
                ValueExpression {
                    position,
                    inner: ValueExpressionInner::Binary(
                        Box::new(BinaryExpression {
                            lhs,
                            rhs,
                            operator,
                        }),
                    ),
                }
            } else {
                lhs
            }
        }
        pub fn parse_sum_expression(&self, node: NodeRef) -> ValueExpression {
            let Some(BasicNode::SumExpression { values, operators, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            let values = values.iter().map(|val| self.parse_mult_expression(*val));
            let operator_spans = operators
                .iter()
                .map(|val| {
                    val.chunk(self.doc).unwrap().to_position_span(self.doc).unwrap()
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
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
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
                    result = Some(
                        if let Some(lhs) = result {
                            let position = lhs.position.start..rhs.position.end;
                            let operator = operators[idx - 1].clone();
                            ValueExpression {
                                position,
                                inner: ValueExpressionInner::Binary(
                                    Box::new(BinaryExpression {
                                        lhs: lhs.clone(),
                                        rhs: rhs.clone(),
                                        operator,
                                    }),
                                ),
                            }
                        } else {
                            rhs
                        },
                    );
                }
                result.unwrap()
            }
        }
        pub fn parse_mult_expression(&self, node: NodeRef) -> ValueExpression {
            let Some(BasicNode::MultExpression { values, operators, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
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
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
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
                    result = Some(
                        if let Some(lhs) = result {
                            let position = lhs.position.start..rhs.position.end;
                            let operator = operators[idx - 1].clone();
                            ValueExpression {
                                position,
                                inner: ValueExpressionInner::Binary(
                                    Box::new(BinaryExpression {
                                        lhs: lhs.clone(),
                                        rhs: rhs.clone(),
                                        operator,
                                    }),
                                ),
                            }
                        } else {
                            rhs
                        },
                    );
                }
                result.unwrap()
            }
        }
        pub fn parse_pipe_expression(&self, node: NodeRef) -> ValueExpression {
            let Some(BasicNode::PipeExpression { values, operators, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
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
                    result = Some(
                        if let Some(lhs) = result {
                            let position = lhs.position.start..rhs.position.end;
                            let operator = operators[idx - 1].clone();
                            ValueExpression {
                                position,
                                inner: ValueExpressionInner::Binary(
                                    Box::new(BinaryExpression {
                                        lhs: lhs.clone(),
                                        rhs: rhs.clone(),
                                        operator,
                                    }),
                                ),
                            }
                        } else {
                            rhs
                        },
                    );
                }
                result.unwrap()
            }
        }
        pub fn parse_func_call(&self, node: NodeRef) -> ValueExpression {
            let Some(BasicNode::FunctionCall { values, .. }) = node.deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            values
                .iter()
                .map(|val| self.parse_unary_expr(*val))
                .reduce(|func, arg| {
                    let position = func.position.start..arg.position.end;
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::FuncCall(
                            Box::new(FunctionCall { func, arg }),
                        ),
                    }
                })
                .unwrap()
        }
        pub fn parse_unary_expr(&self, node: NodeRef) -> ValueExpression {
            let position = self.get_node_position(&node);
            let Some(BasicNode::UnaryExpression { op, value, .. }) = node.deref(self.doc)
            else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            let value = {
                match value.deref(self.doc).unwrap() {
                    BasicNode::Literal { node, .. } => self.parse_literal(*node),
                    BasicNode::ParenthesesExpression { value, .. } => {
                        self.parse_xor_expression(*value)
                    }
                    BasicNode::LambdaExpression { node, .. } => self.parse_lambda(*node),
                    BasicNode::IfExpression { node, .. } => self.parse_if_expr(*node),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
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
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        },
                    };
                    ValueExpression {
                        position,
                        inner: ValueExpressionInner::Unary(
                            Box::new(UnaryExpression {
                                operand: value,
                                operator,
                            }),
                        ),
                    }
                }
                None => value,
            }
        }
        pub fn parse_literal(&self, node: NodeRef) -> ValueExpression {
            let position = self.get_node_position(&node);
            let Some(BasicNode::Literal { value, .. }) = node.deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            ValueExpression {
                position,
                inner: match value.deref(self.doc).unwrap() {
                    BasicToken::KwNone => ValueExpressionInner::None,
                    BasicToken::KwTrue => ValueExpressionInner::Bool(true),
                    BasicToken::KwFalse => ValueExpressionInner::Bool(false),
                    BasicToken::Number => {
                        match value.string(self.doc).unwrap().parse() {
                            Ok(integer) => ValueExpressionInner::Integer(integer),
                            Err(_) => {
                                ValueExpressionInner::Float(
                                    value.string(self.doc).unwrap().parse().unwrap(),
                                )
                            }
                        }
                    }
                    BasicToken::Ident => {
                        ValueExpressionInner::Ident(value.string(self.doc).unwrap())
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                },
            }
        }
        pub fn parse_if_expr(&self, node: NodeRef) -> ValueExpression {
            let position = self.get_node_position(&node);
            let Some(&BasicNode::IfExpression { cond, if_true, if_false, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            ValueExpression {
                position,
                inner: ValueExpressionInner::If(
                    Box::new(IfExpression {
                        cond: self.parse_xor_expression(cond),
                        if_true: self.parse_xor_expression(if_true),
                        if_false: self.parse_xor_expression(if_false),
                    }),
                ),
            }
        }
        pub fn parse_lambda(&self, node: NodeRef) -> ValueExpression {
            let position = self.get_node_position(&node);
            let Some(BasicNode::LambdaExpression { params, body, .. }) = node
                .deref(self.doc) else {
                ::core::panicking::panic("internal error: entered unreachable code");
            };
            ValueExpression {
                position,
                inner: ValueExpressionInner::Lambda(
                    Box::new(LambdaBody {
                        params: params
                            .iter()
                            .map(|param| param.string(self.doc).unwrap())
                            .collect(),
                        body: self.parse_xor_expression(*body),
                    }),
                ),
            }
        }
    }
    pub struct FunctionExpression<'code> {
        pub name: &'code str,
        pub position: PositionSpan,
        pub params: Vec<&'code str>,
        pub body: ValueExpression<'code>,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for FunctionExpression<'code> {
        #[inline]
        fn clone(&self) -> FunctionExpression<'code> {
            FunctionExpression {
                name: ::core::clone::Clone::clone(&self.name),
                position: ::core::clone::Clone::clone(&self.position),
                params: ::core::clone::Clone::clone(&self.params),
                body: ::core::clone::Clone::clone(&self.body),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for FunctionExpression<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "FunctionExpression",
                "name",
                &self.name,
                "position",
                &self.position,
                "params",
                &self.params,
                "body",
                &&self.body,
            )
        }
    }
    pub struct BinaryExpression<'code> {
        pub lhs: ValueExpression<'code>,
        pub rhs: ValueExpression<'code>,
        pub operator: BinaryOperator,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for BinaryExpression<'code> {
        #[inline]
        fn clone(&self) -> BinaryExpression<'code> {
            BinaryExpression {
                lhs: ::core::clone::Clone::clone(&self.lhs),
                rhs: ::core::clone::Clone::clone(&self.rhs),
                operator: ::core::clone::Clone::clone(&self.operator),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for BinaryExpression<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "BinaryExpression",
                "lhs",
                &self.lhs,
                "rhs",
                &self.rhs,
                "operator",
                &&self.operator,
            )
        }
    }
    pub struct UnaryExpression<'code> {
        pub operand: ValueExpression<'code>,
        pub operator: UnaryOperator,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for UnaryExpression<'code> {
        #[inline]
        fn clone(&self) -> UnaryExpression<'code> {
            UnaryExpression {
                operand: ::core::clone::Clone::clone(&self.operand),
                operator: ::core::clone::Clone::clone(&self.operator),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for UnaryExpression<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "UnaryExpression",
                "operand",
                &self.operand,
                "operator",
                &&self.operator,
            )
        }
    }
    pub struct FunctionCall<'code> {
        pub func: ValueExpression<'code>,
        pub arg: ValueExpression<'code>,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for FunctionCall<'code> {
        #[inline]
        fn clone(&self) -> FunctionCall<'code> {
            FunctionCall {
                func: ::core::clone::Clone::clone(&self.func),
                arg: ::core::clone::Clone::clone(&self.arg),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for FunctionCall<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "FunctionCall",
                "func",
                &self.func,
                "arg",
                &&self.arg,
            )
        }
    }
    pub struct ValueExpression<'code> {
        pub position: PositionSpan,
        pub inner: ValueExpressionInner<'code>,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for ValueExpression<'code> {
        #[inline]
        fn clone(&self) -> ValueExpression<'code> {
            ValueExpression {
                position: ::core::clone::Clone::clone(&self.position),
                inner: ::core::clone::Clone::clone(&self.inner),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for ValueExpression<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "ValueExpression",
                "position",
                &self.position,
                "inner",
                &&self.inner,
            )
        }
    }
    pub enum ValueExpressionInner<'code> {
        None,
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
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for ValueExpressionInner<'code> {
        #[inline]
        fn clone(&self) -> ValueExpressionInner<'code> {
            match self {
                ValueExpressionInner::None => ValueExpressionInner::None,
                ValueExpressionInner::Binary(__self_0) => {
                    ValueExpressionInner::Binary(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::Unary(__self_0) => {
                    ValueExpressionInner::Unary(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::FuncCall(__self_0) => {
                    ValueExpressionInner::FuncCall(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::Ident(__self_0) => {
                    ValueExpressionInner::Ident(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::Bool(__self_0) => {
                    ValueExpressionInner::Bool(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::Integer(__self_0) => {
                    ValueExpressionInner::Integer(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::Float(__self_0) => {
                    ValueExpressionInner::Float(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::Lambda(__self_0) => {
                    ValueExpressionInner::Lambda(::core::clone::Clone::clone(__self_0))
                }
                ValueExpressionInner::If(__self_0) => {
                    ValueExpressionInner::If(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for ValueExpressionInner<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ValueExpressionInner::None => {
                    ::core::fmt::Formatter::write_str(f, "None")
                }
                ValueExpressionInner::Binary(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Binary",
                        &__self_0,
                    )
                }
                ValueExpressionInner::Unary(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Unary",
                        &__self_0,
                    )
                }
                ValueExpressionInner::FuncCall(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FuncCall",
                        &__self_0,
                    )
                }
                ValueExpressionInner::Ident(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Ident",
                        &__self_0,
                    )
                }
                ValueExpressionInner::Bool(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Bool",
                        &__self_0,
                    )
                }
                ValueExpressionInner::Integer(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Integer",
                        &__self_0,
                    )
                }
                ValueExpressionInner::Float(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Float",
                        &__self_0,
                    )
                }
                ValueExpressionInner::Lambda(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Lambda",
                        &__self_0,
                    )
                }
                ValueExpressionInner::If(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "If", &__self_0)
                }
            }
        }
    }
    pub struct LambdaBody<'code> {
        pub params: Vec<&'code str>,
        pub body: ValueExpression<'code>,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for LambdaBody<'code> {
        #[inline]
        fn clone(&self) -> LambdaBody<'code> {
            LambdaBody {
                params: ::core::clone::Clone::clone(&self.params),
                body: ::core::clone::Clone::clone(&self.body),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for LambdaBody<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "LambdaBody",
                "params",
                &self.params,
                "body",
                &&self.body,
            )
        }
    }
    pub struct IfExpression<'code> {
        pub cond: ValueExpression<'code>,
        pub if_true: ValueExpression<'code>,
        pub if_false: ValueExpression<'code>,
    }
    #[automatically_derived]
    impl<'code> ::core::clone::Clone for IfExpression<'code> {
        #[inline]
        fn clone(&self) -> IfExpression<'code> {
            IfExpression {
                cond: ::core::clone::Clone::clone(&self.cond),
                if_true: ::core::clone::Clone::clone(&self.if_true),
                if_false: ::core::clone::Clone::clone(&self.if_false),
            }
        }
    }
    #[automatically_derived]
    impl<'code> ::core::fmt::Debug for IfExpression<'code> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "IfExpression",
                "cond",
                &self.cond,
                "if_true",
                &self.if_true,
                "if_false",
                &&self.if_false,
            )
        }
    }
}
fn main() {
    use lady_deirdre::{syntax::SyntaxTree, units::Document};
    use parser::syntax::BasicNode;
    let args = std::env::args();
    let args = args.collect::<Vec<String>>();
    if args.len() < 2 {
        {
            ::std::io::_eprint(format_args!("No file was provided.\n"));
        };
        return;
    }
    let file = PathBuf::from(args[1].clone());
    if !file.exists() || !file.is_file() {
        {
            ::std::io::_eprint(format_args!("Provided incorrect file\n"));
        };
        return;
    }
    let input = std::fs::read_to_string(file);
    let input = match input {
        Ok(input) => input.replace("\r", ""),
        Err(error) => {
            {
                ::std::io::_eprint(format_args!("Error while reading file.\n"));
            };
            {
                ::std::io::_eprint(format_args!("Error: {0}\n", error));
            };
            return;
        }
    };
    let doc = Document::<BasicNode>::new_immutable(input);
    for error in doc.errors() {
        {
            ::std::io::_print(format_args!("{0:#?}\n", error.display(&doc)));
        };
    }
    if doc.errors().count() != 0 {
        {
            ::std::io::_eprint(format_args!("Parsing errors found, stopping\n"));
        };
        return;
    }
    let tree = doc.root_node_ref();
    let Some(BasicNode::Root { functions, .. }) = tree.deref(&doc) else {
        ::core::panicking::panic("internal error: entered unreachable code")
    };
    let parser = parser::Parser::new(&doc);
    let functions = functions
        .iter()
        .map(|func| parser.parse_function(*func))
        .collect::<Vec<FunctionExpression>>();
    let compiler = Compiler::new(functions);
    let Compiler { functions, func_names } = match compiler {
        Ok(compiler) => compiler,
        Err(err) => {
            {
                ::std::io::_eprint(format_args!("{0:#?}\n", err.display(&doc)));
            };
            return;
        }
    };
    let interpreter = Interpreter {
        functions: &functions,
    };
    let before = Instant::now();
    match interpreter
        .eval_lambda(cir::LambdaState {
            function: cir::FunctionIdentifier::Defined(func_names["main"]),
            provided_args: ::alloc::vec::Vec::new(),
        })
    {
        Ok(value) => {
            ::std::io::_print(format_args!("{0:#?}\n", value));
        }
        Err(error) => {
            ::std::io::_eprint(format_args!("{0:#?}\n", error.display(&doc)));
        }
    }
    let after = Instant::now();
    {
        ::std::io::_print(format_args!("Time elapsed: {0:?}\n", after - before));
    };
}
