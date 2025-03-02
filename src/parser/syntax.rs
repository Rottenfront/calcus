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

    #[rule(name: $Ident (parameters: $Ident)* $Let (body: Expr)
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

    #[rule((values: XorExpr)+{$Comma})]
    Expr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((values: OrExpr)+{$KwXor})]
    XorExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((values: AndExpr)+{$KwOr})]
    OrExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((values: EqualityExpr)+{$KwAnd})]
    AndExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((lvalue: SumExpr)
        ((operator: ($Equal | $NotEqual | $Less | $Greater | $GreaterEqual | $LessEqual))
        rvalue: SumExpr)?)]
    EqualityExpr {
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

    #[rule((values: MultExpr)+{operators: $Plus | $Minus})]
    SumExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
        #[child]
        operators: Vec<TokenRef>,
    },

    #[rule((values: PipeExpr)+{operators: $Star | $Slash})]
    MultExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
        #[child]
        operators: Vec<TokenRef>,
    },

    #[rule((values: FuncCall)+{operators: $PipeOperator})]
    PipeExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
        #[child]
        operators: Vec<TokenRef>,
    },

    #[rule((values: UnaryExpr)+)]
    FuncCall {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        values: Vec<NodeRef>,
    },

    #[rule((op: $Minus | $Exclamation)? value: (Literal | ParenthesesExpr | CaseExpr | LambdaExpr))]
    UnaryExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: NodeRef,
        #[child]
        op: TokenRef,
    },

    #[rule($ParenthesesOpen (value: Expr)? $ParenthesesClose)]
    ParenthesesExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: NodeRef,
    },

    #[rule((value: $Number | $KwTrue | $KwFalse | $Ident))]
    Literal {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: TokenRef,
    },

    #[rule($KwLambda (params: $Ident)* $Arrow (body: Expr))]
    LambdaExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        params: Vec<TokenRef>,
        #[child]
        body: NodeRef,
    },

    #[rule($KwCase (expr: Expr) $KwOf
        ((cases: Expr) $Arrow (results: Expr) $Semicolon)*
        ($KwOr $Arrow (default_expr: Expr) $Semicolon)?
    $KwEnd)]
    CaseExpr {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        expr: NodeRef,
        #[child]
        cases: Vec<NodeRef>,
        #[child]
        results: Vec<NodeRef>,
        #[child]
        default_expr: NodeRef,
    },
}
