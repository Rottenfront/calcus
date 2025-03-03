use std::{error, path::PathBuf};

use interpreter::{types::Value, Interpreter};

pub mod interpreter;
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
        println!("Error: {:#?}", error.display(&doc));
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
        .collect();
    let mut interpreter = Interpreter { functions };
    println!("{:?}", interpreter.interpret(0, vec![Value::Number(3.0)]));
}
