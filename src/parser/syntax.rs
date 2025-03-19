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

    #[rule(name: $Ident (params: $Ident)* $Let (body: TupleExpression)
        ($KwWhere $BracketOpen (local_funcs: Function)* $BracketClose)?
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
        #[child]
        local_funcs: Vec<NodeRef>,
    },

    #[rule((values: XorExpression)+{$Comma})]
    TupleExpression {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
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

    #[rule($ParenthesesOpen (value: TupleExpression)? $ParenthesesClose)]
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

    #[rule($KwLambda (params: $Ident)* $Arrow (body: TupleExpression))]
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

    #[rule($KwLet (name: $Ident))]
    CaseDefinition {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        name: TokenRef,
    },

    #[rule($KwCase (arg: TupleExpression) $KwOf
        ((cases: CaseTemplate)
            $Arrow (results: TupleExpression) $Semicolon)*
        ($KwOr $Arrow (default_expr: TupleExpression) $Semicolon)?
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

    #[rule((values: (XorExpression | CaseDefinition))+{$Comma})]
    CaseTemplate {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },
}
