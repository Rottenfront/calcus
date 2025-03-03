use lady_deirdre::lexis::Token;

#[derive(Token, Clone, Copy, PartialEq, Eq, Debug)]
#[define(HEX = ['0'..'9', 'A'..'F', 'a'..'f'])]
#[repr(u8)]
pub enum BasicToken {
    EOI = 0,

    Mismatch = 1,

    #[rule("case")]
    #[describe("KW_case")]
    #[priority(2)]
    KwCase,

    #[rule("of")]
    #[describe("KW_of")]
    #[priority(2)]
    KwOf,

    #[rule("end")]
    #[describe("KW_end")]
    #[priority(2)]
    KwEnd,

    #[rule("where")]
    #[describe("KW_where")]
    #[priority(2)]
    KwWhere,

    #[rule("lambda")]
    #[describe("KW_lambda")]
    #[priority(2)]
    KwLambda,

    #[rule("true")]
    #[describe("KW_true")]
    #[priority(2)]
    KwTrue,

    #[rule("false")]
    #[describe("KW_false")]
    #[priority(2)]
    KwFalse,

    // #[rule("if")]
    // #[describe("KW_if")]
    // #[priority(2)]
    // KwIf,

    // #[rule("then")]
    // #[describe("KW_then")]
    // #[priority(2)]
    // KwThen,

    // #[rule("else")]
    // #[describe("KW_else")]
    // #[priority(2)]
    // KwElse,
    #[rule("and")]
    #[describe("KW_and")]
    #[priority(2)]
    KwAnd,

    #[rule("or")]
    #[describe("KW_or")]
    #[priority(2)]
    KwOr,

    #[rule("xor")]
    #[describe("KW_xor")]
    #[priority(2)]
    KwXor,

    #[rule(['0'..'9']+ ('.' ['0'..'9']*)?)]
    #[describe("number")]
    Number,

    #[rule("==")]
    #[describe("==")]
    #[priority(2)]
    Equal,

    #[rule("!=")]
    #[describe("!=")]
    #[priority(2)]
    NotEqual,

    #[rule("<")]
    #[describe("<")]
    Less,

    #[rule(">")]
    #[describe(">")]
    Greater,

    #[rule("<=")]
    #[describe("<=")]
    #[priority(2)]
    LessEqual,

    #[rule(">=")]
    #[describe(">=")]
    #[priority(2)]
    GreaterEqual,

    #[rule("+")]
    #[describe("+")]
    Plus,

    #[rule("-")]
    #[describe("-")]
    Minus,

    #[rule("*")]
    #[describe("*")]
    Star,

    #[rule("/")]
    #[describe("/")]
    Slash,

    #[rule("!")]
    #[describe("!")]
    Exclamation,

    #[rule("=")]
    #[describe("=")]
    Let,

    #[rule("|>")]
    #[describe("|>")]
    #[priority(2)]
    PipeOperator,

    #[rule("->")]
    #[describe("->")]
    #[priority(2)]
    Arrow,

    #[rule("(")]
    #[describe("(")]
    ParenthesesOpen,

    #[rule(")")]
    #[describe(")")]
    ParenthesesClose,

    #[rule("[")]
    #[describe("[")]
    BracketOpen,

    #[rule("]")]
    #[describe("]")]
    BracketClose,

    #[rule(",")]
    #[describe(",")]
    Comma,

    #[rule(":")]
    #[describe(":")]
    Colon,

    #[rule("::")]
    #[describe("::")]
    #[priority(2)]
    DoubleColon,

    #[rule(";")]
    #[describe(";")]
    Semicolon,

    // COMMENTS
    #[priority(5)]
    #[rule("--" ^['\n']* '\n'?)]
    SingleComment,
    #[priority(6)]
    #[rule("--{{" (^['}'] | ('}' ^['}']))* "}}")]
    MultilineComment,

    #[rule([' ', '\t', '\n', '\x0c', '\r']+)]
    Whitespace,

    #[rule($xid_start & $xid_continue*)]
    Ident,
}
