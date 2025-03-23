use std::{collections::HashMap, path::PathBuf, time::Instant};

use cir::interpreter::Interpreter;
use compiler::Compiler;
use parser::Parser;

pub mod cir;
pub mod compiler;
pub mod error_displayer;
pub mod parser;

fn main() {
    use lady_deirdre::{syntax::SyntaxTree, units::Document};

    use parser::syntax::BasicNode;

    let args = std::env::args();
    let args = args.collect::<Vec<String>>();
    if args.len() < 2 {
        eprintln!("No file was provided.");
        return;
    }
    let file = PathBuf::from(args[1].clone());
    if !file.exists() || !file.is_file() {
        eprintln!("Provided incorrect file");
        return;
    }
    let input = std::fs::read_to_string(file);
    let input = match input {
        Ok(input) => input.replace("\r", ""),
        Err(error) => {
            eprintln!("Error while reading file.");
            eprintln!("Error: {}", error);
            return;
        }
    };

    let doc = Document::<BasicNode>::new_immutable(input);
    for error in doc.errors() {
        println!("{:#?}", error.display(&doc));
    }
    if doc.errors().count() != 0 {
        eprintln!("Parsing errors found, stopping");
        return;
    }
    let tree = doc.root_node_ref();
    let Some(BasicNode::Root { functions, .. }) = tree.deref(&doc) else {
        unreachable!()
    };
    let mut functions_names = HashMap::new();
    for (index, function) in functions.iter().enumerate() {
        let BasicNode::Function { name, .. } = function.deref(&doc).unwrap() else {
            unreachable!()
        };
        let name = name.string(&doc).unwrap();
        functions_names.insert(name, index);
    }
    let mut parsed_functions = Vec::with_capacity(functions.len());
    for function in functions {
        match Parser::parse_function(&doc, &functions_names, *function) {
            Ok(function) => parsed_functions.push(function),
            Err(err) => {
                eprintln!("{:#?}", err.display(&doc));
                return;
            }
        }
    }

    let compiler = Compiler::new(parsed_functions);
    let Compiler { functions } = match compiler {
        Ok(compiler) => compiler,
        Err(err) => {
            eprintln!("{:#?}", err.display(&doc));
            return;
        }
    };

    let interpreter = Interpreter {
        functions: &functions,
    };
    let before = Instant::now();
    match interpreter.eval_lambda(cir::LambdaState {
        function: cir::FunctionIdentifier::Defined(functions_names["main"]),
        provided_args: vec![],
    }) {
        Ok(value) => println!("{:#?}", value),
        Err(error) => eprintln!("{:#?}", error.display(&doc)),
    }

    let after = Instant::now();
    println!("Time elapsed: {:?}", after - before);
}
