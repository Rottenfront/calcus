pub mod interpretator;
pub mod parser;

fn main() {
    use lady_deirdre::{syntax::SyntaxTree, units::Document};

    use parser::syntax::BasicNode;
    static INPUT: &'static str = r#"fib n = case n of
            0 -> 0;
            1 -> 1;
            n -> res1 + res2;
        end
        where [
            res1 = fib (n - 1);
            res2 = fib (n - 2);
        ];"#;

    let doc = Document::<BasicNode>::new_immutable(INPUT);
    for error in doc.errors() {
        println!("Error: {:?}", error);
    }
    if doc.errors().count() != 0 {
        panic!("Parsing errors found, stopping");
    }
    let tree = doc.root_node_ref();
    let Some(BasicNode::Root { functions, .. }) = tree.deref(&doc) else {
        unreachable!()
    };
    let parser = parser::Parser::new(&doc);
    let functions = functions.iter().map(|func| parser.parse_function(*func));
}
