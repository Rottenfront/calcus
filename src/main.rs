use std::{path::PathBuf, time::Instant};

use cir::interpreter::Interpreter;
use compiler::Compiler;
use parser::FunctionExpression;

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
    let parser = parser::Parser::new(&doc);
    let functions = functions
        .iter()
        .map(|func| parser.parse_function(*func))
        .collect::<Vec<FunctionExpression>>();

    let compiler = Compiler::new(functions);
    let Compiler {
        functions,
        func_names,
    } = match compiler {
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
        function: cir::FunctionIdentifier::Defined(func_names["main"]),
        provided_args: vec![],
    }) {
        Ok(value) => println!("{:#?}", value),
        Err(error) => eprintln!("{:#?}", error.display(&doc)),
    }

    let after = Instant::now();
    println!("Time elapsed: {:?}", after - before);
}
