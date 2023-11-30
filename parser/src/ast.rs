use lexer::token;

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    pub children: Vec<TopLevel<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum TopLevel<'a> {
    // FnDecl(FnDecl),
    FnDef(FnDef<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    /// an empty statement `;`
    Nothing,
    Return(ReturnStmt<'a>),
    Decl(DeclarationStmt<'a>),
    If(Box<IfStmt<'a>>),
    Expr(Expr<'a>),
    Block(Box<Block<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Unary(Box<UnaryExpr<'a>>),
    Infix(Box<InfixExpr<'a>>),
    Conditional(Box<ConditionalExpr<'a>>),
    Assign(Box<Assignment<'a>>),
    IntLit(IntLiteral),
    Ident(Identifier<'a>),
}

#[derive(Debug, PartialEq)]
pub struct FnDef<'a> {
    pub return_type: Identifier<'a>,
    pub name: Identifier<'a>,
    pub args: Vec<FnArg<'a>>,
    pub body: Block<'a>,
}

#[derive(Debug, PartialEq)]
pub struct FnArg<'a> {
    pub type_: Identifier<'a>,
    pub name: Identifier<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'a> {
    pub name: &'a token::Identifier<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Block<'a> {
    pub body: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct DeclarationStmt<'a> {
    pub type_: Identifier<'a>,
    pub name: Identifier<'a>,
    pub initializer: Option<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt<'a> {
    pub cond: Expr<'a>,
    pub body: Stmt<'a>,
    pub alternative: Option<Stmt<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnarySign {
    Negate,
    BitComplement,
    LogicNegate,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr<'a> {
    pub symbol: UnarySign,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixSymbol {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Less,
    More,
    LessEq,
    MoreEq,
    Equality,
    NotEq,
    LogicalAnd,
    LogicalOr,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpr<'a> {
    pub symbol: InfixSymbol,
    pub left: Expr<'a>,
    pub right: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct ConditionalExpr<'a> {
    pub cond: Expr<'a>,
    pub succ: Expr<'a>,
    pub fail: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignSymbol {
    Equal,
    PlusEq,
    MinusEq,
    DivideEq,
    TimesEq,
    ModuloEq,
    ShiftLeftEq,
    ShiftRightEq,
    AndEq,
    OrEq,
    XorEq,
}

#[derive(Debug, PartialEq)]
pub struct Assignment<'a> {
    pub symbol: AssignSymbol,
    pub var: Identifier<'a>,
    pub value: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct IntLiteral {
    pub value: i32,
}
