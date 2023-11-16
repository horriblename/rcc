#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    LParen(&'a str),
    RParen(&'a str),
    LBracket(&'a str),
    RBracket(&'a str),
    LBrace(&'a str),
    RBrace(&'a str),
    Semicolon(&'a str),
    Comma,
    Asterisk,
    Minus,
    Ident(Identifier<'a>),
    Integer(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier<'a> {
    pub name: &'a str,
}
