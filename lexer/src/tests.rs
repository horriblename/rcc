use crate::token::Identifier;
use crate::token::TokenType;

use super::lex_program;
use nom_locate::LocatedSpan;

fn check_tokens(source: &str, expect: &[TokenType]) {
    let span = LocatedSpan::new(source);
    let (_, tokens) = lex_program(span).unwrap();

    assert_eq!(expect.len(), tokens.len());

    for (got, exp) in tokens.iter().zip(expect.iter()) {
        assert_eq!(&got.type_, exp);
    }
}

#[test]
fn test_main() {
    let source = r#"
            int main(int argc, char* argv) {
                return ~100;
                return !0;
                return -2;
            }
            "#;

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

    check_tokens(source, &expect)
}

#[test]
fn test_all() {
    let source = "() [ ] { } ; , * / % + - ~ ! && || == != < <= > >=";

    let expect = [
        TokenType::LParen,
        TokenType::RParen,
        TokenType::LBracket,
        TokenType::RBracket,
        TokenType::LBrace,
        TokenType::RBrace,
        TokenType::Semicolon,
        TokenType::Comma,
        TokenType::Asterisk,
        TokenType::Slash,
        TokenType::Percent,
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Tilde,
        TokenType::Exclamation,
        TokenType::LogicalAnd,
        TokenType::LogicalOr,
        TokenType::EqEqual,
        TokenType::BangEqual,
        TokenType::Less,
        TokenType::LessEq,
        TokenType::More,
        TokenType::MoreEq,
    ];

    check_tokens(source, &expect)
}