use lexer::token::{self, Token};
use nom::branch::alt;
use nom::combinator::{all_consuming, map, value};
use nom::error::{self, ParseError};
use nom::multi::many1;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{
    multi::{many0, separated_list0},
    IResult,
};

pub mod ast;

#[cfg(test)]
mod tests;

type ParserResult<'a, T, E> = IResult<ParserIn<'a>, T, E>;

type ParserIn<'a> = &'a [Token<'a>];

macro_rules! mark {
    ($e: expr, $($f: expr),+) => {{
        println!($($f),+);
        $e
    }};
}

pub fn parse_program<'a, E: ParseError<ParserIn<'a>>>(
    program: &'a [Token],
) -> ParserResult<'a, ast::Program<'a>, E> {
    let (rest, out) = all_consuming(many0(parse_top_level))(program)?;
    Ok((rest, ast::Program { children: out }))
}

fn parse_top_level<'a, E: ParseError<ParserIn<'a>>>(
    source: &'a [Token],
) -> ParserResult<'a, ast::TopLevel<'a>, E> {
    alt((map(parse_function, ast::TopLevel::FnDef),))(source)
}

fn parse_function<'a, E: ParseError<ParserIn<'a>>>(
    source: &'a [Token],
) -> ParserResult<'a, ast::FnDef<'a>, E> {
    map(
        tuple((
            parse_identifier,
            parse_identifier,
            delimited(
                one(Token::LParen("(")),
                parse_arg_list,
                one(Token::RParen(")")),
            ),
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

fn parse_arg_list<'a, E: ParseError<ParserIn<'a>>>(
    source: &'a [Token],
) -> ParserResult<'a, Vec<ast::FnArg<'a>>, E> {
    separated_list0(one(Token::Comma), parse_arg)(source)
}

fn parse_arg<'a, E: ParseError<ParserIn<'a>>>(
    source: &'a [Token],
) -> ParserResult<'a, ast::FnArg<'a>, E> {
    map(
        tuple((parse_identifier, parse_identifier)),
        |(type_, name)| ast::FnArg { type_, name },
    )(source)
}

fn parse_block<'a, E: ParseError<ParserIn<'a>>>(
    source: &'a [Token],
) -> ParserResult<'a, ast::Block<'a>, E> {
    map(
        delimited(
            one(Token::LBrace("{")),
            parse_stmts,
            one(Token::RBrace("}")),
        ),
        |body| ast::Block { body },
    )(source)
}

fn parse_stmts<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, Vec<ast::Stmt>, E> {
    many1(parse_stmt)(source)
}

fn parse_stmt<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Stmt, E> {
    terminated(
        alt((map(parse_return, ast::Stmt::Return),)),
        one(Token::Semicolon(";")),
    )(source)
}

fn parse_return<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::ReturnStmt, E> {
    map(
        preceded(
            one(Token::Ident(token::Identifier { name: "return" })),
            parse_expr,
        ),
        |expr| ast::ReturnStmt { expr },
    )(source)
}

fn parse_expr<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    // TODO:
    alt((
        map(parse_unary, |expr| ast::Expr::Unary(Box::new(expr))),
        map(parse_int_literal, ast::Expr::IntLit),
    ))(source)
}

fn parse_unary<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::UnaryExpr, E> {
    alt((map(
        tuple((one_unary_symbol, parse_expr)),
        |(symbol, expr)| ast::UnaryExpr { symbol, expr },
    ),))(source)
    .map_err(|x| mark!(x, "unary failed"))
}

fn parse_int_literal<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::IntLiteral, E> {
    map(eat_int_literal, |value| ast::IntLiteral { value })(source)
}

// there's gotta be a better way right?
fn one<'a, E: ParseError<ParserIn<'a>>>(
    token: Token<'a>,
) -> impl Fn(ParserIn<'a>) -> ParserResult<'a, Token<'a>, E> {
    move |source| match source.first() {
        Some(tok) if tok == &token => {
            mark!(Ok((&source[1..], Token::Comma)), "match! one {:?}", token)
        }
        _ => mark!(
            temp_error(source),
            "not one({:?}): {:?}",
            token,
            source.first()
        ),
    }
}

fn one_unary_symbol<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::UnarySign, E> {
    alt((
        value(ast::UnarySign::Negate, one(Token::Minus)),
        value(ast::UnarySign::BitComplement, one(Token::Tilde)),
        value(ast::UnarySign::LogicNegate, one(Token::Exclamation)),
    ))(source)
}

fn eat_int_literal<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, i32, E> {
    match source.first() {
        Some(Token::Integer(n)) => mark!(Ok((&source[1..], *n)), "match! {:?}", n),
        _ => mark!(temp_error(source), "not int: {:?}", source.first()),
    }
}

fn parse_identifier<'a, E: ParseError<ParserIn<'a>>>(
    source: &'a [Token],
) -> ParserResult<'a, ast::Identifier<'a>, E> {
    match source.first() {
        Some(Token::Ident(name)) => mark!(
            Ok((&source[1..], ast::Identifier { name })),
            "match! ident {:?}",
            name
        ),
        _ => mark!(temp_error(source), "not ident: {:?}", source.first()),
    }
}

fn temp_error<'a, O, E: ParseError<ParserIn<'a>>>(source: ParserIn<'a>) -> ParserResult<'a, O, E> {
    Err(nom::Err::Error(error::make_error(
        source,
        error::ErrorKind::IsNot,
    )))
}
