use std::collections::HashMap;

use ast::*;
use lambda::LambdaState;
use types::Value;

pub mod ast;
pub mod lambda;
pub mod types;

pub struct Interpreter {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub enum InterpretationError {
    TypeMismatch,
    CaseMismatch,
    WrongFunctionCall,
    SymbolNotFound(String),
    NotEnoughArguments,
}

impl Interpreter {
    pub fn print_func(&mut self, value: Value) -> Value {
        match value {
            Value::Number(num) => println!("{}", num),
            Value::Boolean(bool) => println!("{}", bool),
            Value::Lambda(lambda_state) => println!("{:?}", lambda_state),
            Value::Tuple(values) => println!("tuple: {:?}", values),
        }
        Value::Boolean(false)
    }

    pub fn interpret(
        &mut self,
        func: usize,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpretationError> {
        let func = self.functions[func].clone();

        let Function {
            local_functions,
            body,
            parameters,
            ..
        } = func;
        if arguments.len() != parameters.len() {
            return Err(InterpretationError::NotEnoughArguments);
        }
        let mut evaluator = Evaluator {
            functions: &self.functions,
            local_funcs: local_functions.clone(),
            local_values: {
                let mut values = HashMap::new();
                for (name, arg) in parameters.iter().zip(arguments.iter()) {
                    values.insert(name.clone(), arg.clone());
                }
                values
            },
        };
        evaluator.eval_expressions(body)
    }
}

struct Evaluator<'a> {
    functions: &'a Vec<Function>,
    local_funcs: Vec<Function>,
    local_values: HashMap<String, Value>,
}

impl<'a> Evaluator<'a> {
    fn eval_expressions(&mut self, exprs: Expressions) -> Result<Value, InterpretationError> {
        let mut values = vec![];

        for expr in exprs.values {
            let value = self.eval_xor_expression(expr)?;
            values.push(value);
        }
        if values.len() == 1 {
            Ok(values[0].clone())
        } else {
            Ok(Value::Tuple(values))
        }
    }

    fn eval_xor_expression(&mut self, exprs: XorExpression) -> Result<Value, InterpretationError> {
        let mut result = None;

        for expr in exprs.values {
            let rvalue = self.eval_or_expression(expr)?;
            match result {
                None => result = Some(rvalue),
                Some(lvalue) => {
                    if let Value::Boolean(lvalue) = lvalue {
                        if let Value::Boolean(rvalue) = rvalue {
                            result = Some(Value::Boolean(lvalue ^ rvalue));
                        } else {
                            return Err(InterpretationError::TypeMismatch);
                        }
                    } else {
                        return Err(InterpretationError::TypeMismatch);
                    }
                }
            }
        }

        Ok(result.unwrap())
    }

    fn eval_or_expression(&mut self, exprs: OrExpression) -> Result<Value, InterpretationError> {
        let mut result = None;

        for expr in exprs.values {
            let rvalue = self.eval_and_expression(expr)?;
            match result {
                None => result = Some(rvalue),
                Some(lvalue) => {
                    if let Value::Boolean(lvalue) = lvalue {
                        if let Value::Boolean(rvalue) = rvalue {
                            result = Some(Value::Boolean(lvalue || rvalue));
                        } else {
                            return Err(InterpretationError::TypeMismatch);
                        }
                    } else {
                        return Err(InterpretationError::TypeMismatch);
                    }
                }
            }
        }

        Ok(result.unwrap())
    }

    fn eval_and_expression(&mut self, exprs: AndExpression) -> Result<Value, InterpretationError> {
        let mut result = None;

        for expr in exprs.values {
            let rvalue = self.eval_equality_expression(expr)?;
            match result {
                None => result = Some(rvalue),
                Some(lvalue) => {
                    if let Value::Boolean(lvalue) = lvalue {
                        if let Value::Boolean(rvalue) = rvalue {
                            result = Some(Value::Boolean(lvalue && rvalue));
                        } else {
                            return Err(InterpretationError::TypeMismatch);
                        }
                    } else {
                        return Err(InterpretationError::TypeMismatch);
                    }
                }
            }
        }

        Ok(result.unwrap())
    }

    fn eval_equality_expression(
        &mut self,
        expr: EqualityExpression,
    ) -> Result<Value, InterpretationError> {
        let lvalue = self.eval_sum_expression(expr.lvalue)?;
        if let Some((operator, rvalue)) = expr.rvalue {
            let rvalue = self.eval_sum_expression(rvalue)?;
            if let Value::Number(lvalue) = lvalue {
                if let Value::Number(rvalue) = rvalue {
                    Ok(Value::Boolean(match operator {
                        EqualityOperator::Equal => lvalue == rvalue,
                        EqualityOperator::NotEqual => lvalue != rvalue,
                        EqualityOperator::Greater => lvalue > rvalue,
                        EqualityOperator::Less => lvalue < rvalue,
                        EqualityOperator::GreaterEqual => lvalue >= rvalue,
                        EqualityOperator::LessEqual => lvalue <= rvalue,
                    }))
                } else {
                    Err(InterpretationError::TypeMismatch)
                }
            } else {
                Err(InterpretationError::TypeMismatch)
            }
        } else {
            Ok(lvalue)
        }
    }

    fn eval_sum_expression(&mut self, expr: SumExpression) -> Result<Value, InterpretationError> {
        let lvalue = self.eval_mult_expression(expr.lvalue)?;
        if expr.rvalues.is_empty() {
            Ok(lvalue)
        } else {
            let Value::Number(mut lvalue) = lvalue else {
                return Err(InterpretationError::TypeMismatch);
            };

            for (operator, rvalue) in expr.rvalues {
                let Value::Number(rvalue) = self.eval_mult_expression(rvalue)? else {
                    return Err(InterpretationError::TypeMismatch);
                };
                match operator {
                    SumOperator::Sum => lvalue += rvalue,
                    SumOperator::Sub => lvalue -= rvalue,
                }
            }

            Ok(Value::Number(lvalue))
        }
    }

    fn eval_mult_expression(&mut self, expr: MultExpression) -> Result<Value, InterpretationError> {
        let lvalue = self.eval_function_call(expr.lvalue)?;
        if expr.rvalues.is_empty() {
            Ok(lvalue)
        } else {
            let Value::Number(mut lvalue) = lvalue else {
                return Err(InterpretationError::TypeMismatch);
            };

            for (operator, rvalue) in expr.rvalues {
                let Value::Number(rvalue) = self.eval_function_call(rvalue)? else {
                    return Err(InterpretationError::TypeMismatch);
                };
                match operator {
                    MultOperator::Multiply => lvalue *= rvalue,
                    MultOperator::Divide => lvalue /= rvalue,
                }
            }

            Ok(Value::Number(lvalue))
        }
    }

    fn eval_function_call(&mut self, expr: FunctionCall) -> Result<Value, InterpretationError> {
        let mut primary = self.eval_unary_expression(expr.primary)?;
        if expr.args.is_empty() {
            Ok(primary)
        } else {
            let mut args = expr.args;
            args.reverse();
            while let Some(arg) = args.pop() {
                let Value::Lambda(lambda) = &mut primary else {
                    return Err(InterpretationError::WrongFunctionCall);
                };
                let arg = self.eval_unary_expression(arg)?;
                lambda.push(arg);
                if lambda.params.len() == lambda.provided_args.len() {
                    primary = self.run_lambda(lambda.clone())?;
                }
            }

            Ok(primary)
        }
    }

    fn eval_unary_expression(
        &mut self,
        expr: UnaryExpression,
    ) -> Result<Value, InterpretationError> {
        let value = self.eval_basic_expression(expr.value)?;
        if let Some(operator) = expr.operator {
            match operator {
                UnaryOperator::Negative => {
                    let Value::Number(value) = value else {
                        return Err(InterpretationError::TypeMismatch);
                    };
                    Ok(Value::Number(-value))
                }
                UnaryOperator::Not => {
                    let Value::Boolean(value) = value else {
                        return Err(InterpretationError::TypeMismatch);
                    };
                    Ok(Value::Boolean(!value))
                }
            }
        } else {
            Ok(value)
        }
    }

    fn eval_basic_expression(
        &mut self,
        expr: BasicExpression,
    ) -> Result<Value, InterpretationError> {
        match expr {
            BasicExpression::Number(value) => Ok(Value::Number(value)),
            BasicExpression::Boolean(value) => Ok(Value::Boolean(value)),
            BasicExpression::Ident(ident) => self.eval_ident(ident),
            BasicExpression::Parenthesized(expressions) => self.eval_expressions(expressions),
            BasicExpression::Lambda(lambda) => self.eval_lambda(lambda),
            BasicExpression::Case(case) => self.eval_case(case),
        }
    }

    fn eval_ident(&mut self, ident: String) -> Result<Value, InterpretationError> {
        // search values
        if let Some(value) = self.local_values.get(&ident) {
            return Ok(value.clone());
        }

        let mut local_funcs = self.local_funcs.clone();
        // search local functions
        for func in &mut self.local_funcs {
            if ident == func.name {
                return Ok(Value::Lambda(LambdaState {
                    params: func.parameters.clone(),
                    provided_args: vec![],
                    local_functions: {
                        let mut funcs = func.local_functions.clone();
                        funcs.append(&mut local_funcs);
                        funcs
                    },
                    local_values: self.local_values.clone(),
                    body: func.body.clone(),
                }));
            }
        }

        for func in self.functions {
            if ident == func.name {
                return Ok(Value::Lambda(LambdaState {
                    params: func.parameters.clone(),
                    provided_args: vec![],
                    local_functions: func.local_functions.clone(),
                    local_values: Default::default(),
                    body: func.body.clone(),
                }));
            }
        }

        Err(InterpretationError::SymbolNotFound(ident))
    }

    fn eval_case(&mut self, expr: Case) -> Result<Value, InterpretationError> {
        fn match_values(lhs: &Value, rhs: &Value) -> bool {
            match lhs {
                Value::Number(lhs) => {
                    if let Value::Number(rhs) = rhs {
                        lhs == rhs
                    } else {
                        false
                    }
                }
                Value::Boolean(lhs) => {
                    if let Value::Boolean(rhs) = rhs {
                        lhs == rhs
                    } else {
                        false
                    }
                }
                Value::Lambda(_) => false,
                Value::Tuple(lhs) => {
                    if let Value::Tuple(rhs) = rhs {
                        if lhs.len() != rhs.len() {
                            return false;
                        }
                        lhs.iter()
                            .zip(rhs.iter())
                            .all(|(lhs, rhs)| match_values(lhs, rhs))
                    } else {
                        false
                    }
                }
            }
        }

        fn check_correspondence(
            arg: &Value,
            template: CaseTemplate,
        ) -> Option<HashMap<String, Value>> {
            match arg {
                Value::Tuple(tuple) => {
                    if template.templates.len() != tuple.len() {
                        return None;
                    }
                    let mut map = HashMap::new();

                    for (arg, template) in tuple.iter().zip(template.templates.iter()) {
                        match template {
                            ValueTemplate::Value(value) => {
                                if !match_values(arg, value) {
                                    return None;
                                }
                            }
                            ValueTemplate::Alias(name) => {
                                map.insert(name.clone(), arg.clone());
                            }
                        }
                    }

                    Some(map)
                }
                val => {
                    if template.templates.len() != 1 {
                        return None;
                    }
                    match &template.templates[0] {
                        ValueTemplate::Value(value) => {
                            if match_values(val, value) {
                                Some(Default::default())
                            } else {
                                None
                            }
                        }
                        ValueTemplate::Alias(name) => Some({
                            let mut map = HashMap::new();
                            map.insert(name.clone(), val.clone());
                            map
                        }),
                    }
                }
            }
        }
        let arg = self.eval_expressions(expr.arg)?;

        for (template, expr) in expr.body {
            let template = self.eval_template(template)?;
            match check_correspondence(&arg, template) {
                Some(new_values) => {
                    for (k, v) in new_values {
                        self.local_values.insert(k, v);
                    }
                    return self.eval_expressions(expr);
                }
                None => {}
            }
        }

        if let Some(default) = expr.default {
            return self.eval_expressions(default);
        }
        return Err(InterpretationError::CaseMismatch);
    }

    fn eval_lambda(&mut self, expr: Lambda) -> Result<Value, InterpretationError> {
        Ok(Value::Lambda(LambdaState {
            params: expr.params,
            provided_args: vec![],
            body: expr.body,
            local_functions: self.local_funcs.clone(),
            local_values: self.local_values.clone(),
        }))
    }

    fn run_lambda(&mut self, lambda: LambdaState) -> Result<Value, InterpretationError> {
        let LambdaState {
            params,
            provided_args,
            local_functions,
            local_values,
            body,
        } = lambda;
        let mut evaluator = Evaluator {
            functions: &self.functions,
            local_funcs: local_functions,
            local_values: {
                let mut values = local_values;
                for (ident, value) in params.iter().zip(provided_args.iter()) {
                    values.insert(ident.clone(), value.clone());
                }
                values
            },
        };
        evaluator.eval_expressions(body)
    }

    fn eval_template(&mut self, expr: Expressions) -> Result<CaseTemplate, InterpretationError> {
        fn check_if_only_ident(expr: &XorExpression) -> Option<String> {
            let expr = if expr.values.len() == 1 {
                expr.values[0].clone()
            } else {
                return None;
            };
            let expr = if expr.values.len() == 1 {
                expr.values[0].clone()
            } else {
                return None;
            };
            let expr = if expr.values.len() == 1 {
                expr.values[0].clone()
            } else {
                return None;
            };
            let expr = if expr.rvalue.is_none() {
                expr.lvalue
            } else {
                return None;
            };
            let expr = if expr.rvalues.is_empty() {
                expr.lvalue
            } else {
                return None;
            };
            let expr = if expr.rvalues.is_empty() {
                expr.lvalue
            } else {
                return None;
            };
            let expr = if expr.args.is_empty() {
                expr.primary
            } else {
                return None;
            };
            let expr = if expr.operator.is_none() {
                expr.value
            } else {
                return None;
            };
            if let BasicExpression::Ident(ident) = expr {
                Some(ident)
            } else {
                None
            }
        }
        let mut templates = vec![];
        for expr in expr.values {
            templates.push(if let Some(ident) = check_if_only_ident(&expr) {
                match self.eval_ident(ident.clone()) {
                    Ok(value) => ValueTemplate::Value(value),
                    Err(_) => ValueTemplate::Alias(ident),
                }
            } else {
                ValueTemplate::Value(self.eval_xor_expression(expr)?)
            })
        }
        Ok(CaseTemplate { templates })
    }
}

enum ValueTemplate {
    Value(Value),
    Alias(String),
}

struct CaseTemplate {
    templates: Vec<ValueTemplate>,
}
