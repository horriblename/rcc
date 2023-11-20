use nom_locate::LocatedSpan;

#[derive(Debug)]
pub struct Token<'a> {
    pub position: LocatedSpan<&'a str>,
    pub type_: TokenType<'a>,
}

impl Token<'_> {
    pub fn new<'a>(position: LocatedSpan<&'a str>, type_: TokenType<'a>) -> Token<'a> {
        Token { position, type_ }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'a> {
    // Symbols
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
    Asterisk,
    Slash,
    Percent,
    Plus,
    Minus,
    Tilde,
    Exclamation,
    LogicalAnd,
    BitAnd,
    BitOr,
    Caret,
    BitShiftLeft,
    BitShiftRight,
    LogicalOr,
    EqEqual,
    BangEqual,
    Less,
    LessEq,
    More,
    MoreEq,

    Equal,
    PlusEq,
    MinusEq,
    SlashEq,
    AsteriskEq,
    PercentEq,
    LessLessEq,
    MoreMoreEq,
    AmpersandEq,
    PipeEq,
    CaretEq,

    // Keywords
    Return,
    If,
    Else,

    //
    Ident(Identifier<'a>),
    Integer(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier<'a> {
    pub name: &'a str,
}
