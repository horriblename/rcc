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
    Return(ReturnStmt<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Unary(Box<UnaryExpr<'a>>),
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
pub enum UnarySign {
    Negate,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr<'a> {
    pub symbol: UnarySign,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct IntLiteral {
    pub value: i32,
}
