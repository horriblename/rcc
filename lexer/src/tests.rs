use std::error::Error;

use crate::token::Identifier;
use crate::token::TokenType;

use super::lex_program;
use nom_locate::LocatedSpan;

#[test]
fn test_main() -> Result<(), impl Error> {
    let source = r#"
            int main(int argc, char* argv) {
                return ~100;
                return !0;
                return -2;
            }
            "#;

    let span = LocatedSpan::new(source);
    let (_, tokens) = match lex_program(span) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    let expect = [
        TokenType::Ident(Identifier { name: "int" }),
        TokenType::Ident(Identifier { name: "main" }),
        TokenType::LParen,
        TokenType::Ident(Identifier { name: "int" }),
        TokenType::Ident(Identifier { name: "argc" }),
        TokenType::Comma,
        TokenType::Ident(Identifier { name: "char" }),
        TokenType::Asterisk,
        TokenType::Ident(Identifier { name: "argv" }),
        TokenType::RParen,
        TokenType::LBrace,
        TokenType::Ident(Identifier { name: "return" }),
        TokenType::Tilde,
        TokenType::Integer(100),
        TokenType::Semicolon,
        TokenType::Ident(Identifier { name: "return" }),
        TokenType::Exclamation,
        TokenType::Integer(0),
        TokenType::Semicolon,
        TokenType::Ident(Identifier { name: "return" }),
        TokenType::Minus,
        TokenType::Integer(2),
        TokenType::Semicolon,
        TokenType::RBrace,
    ];

    assert_eq!(expect.len(), tokens.len());

    for (got, exp) in tokens.iter().zip(expect.iter()) {
        assert_eq!(&got.type_, exp);
    }

    Ok(())
}
