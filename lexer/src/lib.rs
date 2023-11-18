use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{multispace0, satisfy},
    combinator::{all_consuming, map, peek},
    multi::many0,
    sequence::{delimited, terminated, tuple},
    IResult,
};

pub mod token;

use nom_locate::{position, LocatedSpan};
use token::{Token, TokenType};

type Span<'a> = LocatedSpan<&'a str>;

pub fn lex_program<'a>(program: Span<'a>) -> IResult<Span<'a>, Vec<Token<'a>>> {
    fn tag_tok<'a>(
        tag_: &'a str,
        token: TokenType<'a>,
    ) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Token<'a>> {
        map(terminated(position, tag(tag_)), move |pos| {
            Token::new(pos, token.clone())
        })
    }

    all_consuming(many0(delimited(
        multispace0,
        alt((
            tag_tok("(", TokenType::LParen),
            tag_tok(")", TokenType::RParen),
            tag_tok("[", TokenType::LBracket),
            tag_tok("]", TokenType::RBracket),
            tag_tok("{", TokenType::LBrace),
            tag_tok("}", TokenType::RBrace),
            tag_tok(";", TokenType::Semicolon),
            tag_tok(",", TokenType::Comma),
            tag_tok("*", TokenType::Asterisk),
            tag_tok("/", TokenType::Slash),
            tag_tok("%", TokenType::Percent),
            tag_tok("+", TokenType::Plus),
            tag_tok("-", TokenType::Minus),
            tag_tok("~", TokenType::Tilde),
            tag_tok("!", TokenType::Exclamation),
            map(
                tuple((position, nom::character::complete::i32)),
                |(pos, n)| Token::new(pos, TokenType::Integer(n)),
            ),
            lex_identifier,
        )),
        multispace0,
    )))(program)
}

fn lex_identifier<'a>(source: Span<'a>) -> IResult<Span<'a>, Token> {
    let (s, _) = peek(satisfy(|c| c.is_alphabetic() || c == '_'))(source)?;
    let (s, pos) = position(s)?;
    let (s, ident) = take_while(|c: char| c.is_alphanumeric() || c == '_')(s)?;
    Ok((
        s,
        Token::new(pos, TokenType::Ident(token::Identifier { name: &ident })),
    ))
}

#[cfg(test)]
mod test {
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
}
