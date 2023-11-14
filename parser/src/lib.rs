use lexer::token::Token;
use nom::branch::alt;
use nom::combinator::{all_consuming, map};
use nom::error;
use nom::multi::many1;
use nom::sequence::tuple;
use nom::{
    multi::{many0, separated_list0},
    IResult,
};

pub mod ast;

#[cfg(test)]
mod tests;

type ParserResult<'a, T> = IResult<&'a [Token<'a>], T>;
type ParserIn<'a> = &'a [Token<'a>];

pub fn parse_program<'a>(program: &'a [Token]) -> ParserResult<'a, ast::Program<'a>> {
    let (rest, out) = all_consuming(many0(parse_top_level))(program)?;
    Ok((rest, ast::Program { children: out }))
}

fn parse_top_level<'a>(source: &'a [Token]) -> ParserResult<'a, ast::TopLevel<'a>> {
    alt((map(parse_function, ast::TopLevel::FnDef),))(source)
}

fn parse_function<'a>(source: &'a [Token]) -> ParserResult<'a, ast::FnDef<'a>> {
    map(
        tuple((
            parse_identifier,
            parse_identifier,
            parse_arg_list,
            parse_block,
        )),
        |(return_type, name, args, body)| ast::FnDef {
            return_type,
            name,
            args,
            body,
        },
    )(source)
}

fn parse_arg_list<'a>(source: &'a [Token]) -> ParserResult<'a, Vec<ast::FnArg<'a>>> {
    separated_list0(one(Token::Comma), parse_arg)(source)
}

// there's gotta be a better way right?

fn one<'a>(token: Token<'a>) -> impl Fn(ParserIn<'a>) -> ParserResult<'a, Token<'a>> {
    move |source| match source.first() {
        Some(tok) if tok == &token => Ok((&source[1..], Token::Comma)),
        _ => temp_error(source),
    }
}

fn parse_arg<'a>(source: &'a [Token]) -> ParserResult<'a, ast::FnArg<'a>> {
    map(
        tuple((parse_identifier, parse_identifier)),
        |(type_, name)| ast::FnArg { type_, name },
    )(source)
}

fn parse_block<'a>(source: &'a [Token]) -> ParserResult<'a, ast::Block> {
    map(
        tuple((
            one(Token::LBrace("{")),
            parse_stmts,
            one(Token::RBrace("}")),
        )),
        |(_, body, _)| ast::Block { body },
    )(source)
}

fn parse_stmts<'a>(source: ParserIn<'a>) -> ParserResult<'a, Vec<ast::Stmt>> {
    many1(parse_stmt)(source)
}

fn parse_stmt<'a>(source: ParserIn<'a>) -> ParserResult<'a, ast::Stmt> {
    alt((map(parse_return, ast::Stmt::Return),))(source)
}

fn parse_return<'a>(source: ParserIn<'a>) -> ParserResult<'a, ast::ReturnStmt> {
    map(parse_expr, |expr| ast::ReturnStmt { expr })(source)
}

fn parse_expr<'a>(source: ParserIn<'a>) -> ParserResult<'a, ast::Expr> {
    // TODO:
    map(parse_int_literal, ast::Expr::IntLit)(source)
}

fn parse_int_literal<'a>(source: ParserIn<'a>) -> ParserResult<'a, ast::IntLiteral> {
    map(eat_int_literal, |value| ast::IntLiteral { value })(source)
}

fn eat_int_literal<'a>(source: ParserIn<'a>) -> ParserResult<'a, i32> {
    match source.first() {
        Some(Token::Integer(n)) => Ok((&source[1..], *n)),
        _ => temp_error(source),
    }
}

fn parse_identifier<'a>(source: &'a [Token]) -> ParserResult<'a, ast::Identifier<'a>> {
    match source.first() {
        Some(Token::Ident(name)) => Ok((&source[1..], ast::Identifier { name: dbg!(name) })),
        _ => temp_error(source),
    }
}

fn temp_error<'a, O>(source: ParserIn<'a>) -> ParserResult<'a, O> {
    Err(nom::Err::Error(error::make_error(
        source,
        error::ErrorKind::IsNot,
    )))
}
