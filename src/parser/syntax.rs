use lady_deirdre::{
    lexis::TokenRef,
    syntax::{Node, NodeRef},
};

use super::lexer::BasicToken;

#[derive(Node)]
#[token(BasicToken)]
#[trivia($Whitespace | $SingleComment | $MultilineComment)]
#[recovery(
    $ParenthesesClose,
    [$ParenthesesOpen..$ParenthesesClose],
    $BracketClose,
    [$BracketOpen..$BracketClose],
)]
pub enum BasicNode {
    #[root]
    #[rule((functions: Function)*)]
    Root {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        functions: Vec<NodeRef>,
    },

    #[rule(name: $Ident (parameters: $Ident)* $Let (body: Expression)
        ($KwWhere $BracketOpen (local_functions: Function)* $BracketClose)?
        $Semicolon)]
    Function {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        name: TokenRef,
        #[child]
        parameters: Vec<TokenRef>,
        #[child]
        body: NodeRef,
        #[child]
        local_functions: Vec<NodeRef>,
    },

    #[rule((values: XorExpression)+{$Comma})]
    Expression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((values: OrExpression)+{$KwXor})]
    XorExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((values: AndExpression)+{$KwOr})]
    OrExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((values: EqualityExpression)+{$KwAnd})]
    AndExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((lvalue: SumExpression)
        ((operator: ($Equal | $NotEqual | $Less | $Greater | $GreaterEqual | $LessEqual))
        rvalue: SumExpression)?)]
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

    #[rule((values: MultExpression)+{operators: ($Plus | $Minus)})]
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

    #[rule((values: FunctionCall)+{operators: ($Star | $Slash)})]
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

    #[rule((values: UnaryExpression)+)]
    FunctionCall {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((op: $Exclamation)? value: (Literal | ParenthesesExpression | CaseExpression | LambdaExpression))]
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

    #[rule($ParenthesesOpen (value: Expression)? $ParenthesesClose)]
    ParenthesesExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: NodeRef,
    },

    #[rule(value: ($Number | $KwTrue | $KwFalse | $Ident))]
    Literal {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: TokenRef,
    },

    #[rule($KwLambda (params: $Ident)* $Arrow (body: Expression))]
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

    #[rule($KwCase (arg: Expression) $KwOf
        ((cases: Expression) $Arrow (results: Expression) $Semicolon)*
        ($KwOr $Arrow (default_expr: Expression) $Semicolon)?
    $KwEnd)]
    CaseExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        arg: NodeRef,
        #[child]
        cases: Vec<NodeRef>,
        #[child]
        results: Vec<NodeRef>,
        #[child]
        default_expr: NodeRef,
    },
}
