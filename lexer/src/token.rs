#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    LParen(&'a str),
    RParen(&'a str),
    LBracket(&'a str),
    RBracket(&'a str),
    LBrace(&'a str),
    RBrace(&'a str),
    Semicolon(&'a str),
    Ident(Identifier<'a>),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'a> {
    pub name: &'a str,
}
