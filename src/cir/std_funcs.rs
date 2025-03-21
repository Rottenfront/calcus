#[derive(Debug, Clone, Copy)]
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
