use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{multispace0, satisfy},
    combinator::{all_consuming, map, peek, value},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

pub mod token;

use token::Token;

pub fn lex_program<'a>(program: &'a str) -> IResult<&'a str, Vec<Token<'a>>> {
    all_consuming(many0(delimited(
        multispace0,
        alt((
            map(tag("("), Token::LParen),
            map(tag(")"), Token::RParen),
            map(tag("["), Token::LBracket),
            map(tag("]"), Token::RBracket),
            map(tag("{"), Token::LBrace),
            map(tag("}"), Token::RBrace),
            map(tag(";"), Token::Semicolon),
            value(Token::Comma, tag(",")),
            value(Token::Asterisk, tag("*")),
            value(Token::Minus, tag("-")),
            map(nom::character::complete::i32, Token::Integer),
            lex_identifier,
        )),
        multispace0,
    )))(program)
}

fn lex_identifier<'a>(source: &'a str) -> IResult<&'a str, Token> {
    map(
        tuple((
            peek(satisfy(|c| c.is_alphabetic() || c == '_')),
            take_while(|c: char| c.is_alphanumeric() || c == '_'),
        )),
        |(_, ident)| Token::Ident(token::Identifier { name: ident }),
    )(source)
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use crate::token::Identifier;
    use crate::token::Token;

    use super::lex_program;

    #[test]
    fn test_main() -> Result<(), impl Error> {
        let source = r#"
            int main(int argc, char* argv) {
                return -2;
            }
            "#;

        let (rest, tokens) = match lex_program(source) {
            Ok(x) => x,
            Err(e) => return Err(e),
        };
        assert_eq!(rest, "");

        let expect = [
            Token::Ident(Identifier { name: "int" }),
            Token::Ident(Identifier { name: "main" }),
            Token::LParen("("),
            Token::Ident(Identifier { name: "int" }),
            Token::Ident(Identifier { name: "argc" }),
            Token::Comma,
            Token::Ident(Identifier { name: "char" }),
            Token::Asterisk,
            Token::Ident(Identifier { name: "argv" }),
            Token::RParen(")"),
            Token::LBrace("{"),
            Token::Ident(Identifier { name: "return" }),
            Token::Minus,
            Token::Integer(2),
            Token::Semicolon(";"),
            Token::RBrace("}"),
        ];

        assert_eq!(expect.len(), tokens.len());

        for (got, exp) in tokens.iter().zip(expect.iter()) {
            assert_eq!(got, exp);
        }

        Ok(())
    }
}
