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
pub enum Stmt {
    Return(ReturnStmt),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    IntLit(IntLiteral),
}

#[derive(Debug, PartialEq)]
pub struct FnDef<'a> {
    pub return_type: Identifier<'a>,
    pub name: Identifier<'a>,
    pub args: Vec<FnArg<'a>>,
    pub body: Block,
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
pub struct Block {
    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq)]
pub struct IntLiteral {
    pub value: i32,
}
