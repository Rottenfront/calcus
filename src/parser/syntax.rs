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

    #[rule(name: $Ident (params: $Ident)* $Let (body: XorExpression)
        $Semicolon)]
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

    #[rule((values: OrExpression)+{operators: $KwXor})]
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

    #[rule((values: AndExpression)+{operators: $KwOr})]
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

    #[rule((values: EqualityExpression)+{operators: $KwAnd})]
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

    #[rule((values: PipeExpression)+{operators: ($Star | $Slash)})]
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

    #[rule((values: FunctionCall)+{operators: $PipeOperator})]
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

    #[rule((values: UnaryExpression)+)]
    FunctionCall {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((op: $Exclamation)? value: (Literal | ParenthesesExpression | IfExpression | LambdaExpression))]
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

    #[rule($ParenthesesOpen (value: XorExpression) $ParenthesesClose)]
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

    #[rule($KwLambda (params: $Ident)* $Arrow (body: XorExpression))]
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

    #[rule($KwIf (cond: XorExpression) $KwThen (if_true: XorExpression) $KwElse (if_false: XorExpression))]
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
