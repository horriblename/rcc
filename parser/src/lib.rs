use ast::{AssignSymbol, InfixSymbol};
use lexer::token::{Token, TokenType};
use nom::branch::alt;
use nom::combinator::{all_consuming, map, opt, value};
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

fn log_trying(_msg: &str) {
    #[cfg(any(debug, test))]
    eprintln!("\x1b[32mtrying: {}\x1b[0m", _msg)
}

// Creates a left-associative infix parser, e.g. '+', '-' etc.
macro_rules! infix_parser {
    (fn $fn_name: ident($child_parser: expr, $($symbol_parsers: expr),+)) => {
fn $fn_name<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
    left: ast::Expr<'a>,
) -> ParserResult<'a, ast::Expr<'a>, E> {
    let res = tuple((
        alt((
            $($symbol_parsers),+,
        )),
        $child_parser,
    ))(source);
    match res {
        Err(_) => Ok((source, left)),
        Ok((source, (symbol, right))) => $fn_name(
            source,
            ast::Expr::Infix(Box::new(ast::InfixExpr {
                symbol,
                left,
                right,
            })),
        ),
    }
}
    };
}

pub fn parse_program<'a, E: ParseError<ParserIn<'a>>>(
    program: ParserIn<'a>,
) -> ParserResult<'a, ast::Program<'a>, E> {
    let (rest, out) = all_consuming(many0(parse_top_level))(program)?;
    Ok((rest, ast::Program { children: out }))
}

fn parse_top_level<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::TopLevel<'a>, E> {
    alt((
        map(parse_function_def, ast::TopLevel::FnDef),
        map(parse_function_decl, ast::TopLevel::FnDecl),
    ))(source)
}

fn parse_function_def<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::FnDef<'a>, E> {
    map(
        tuple((
            parse_identifier,
            parse_identifier,
            delimited(
                one(TokenType::LParen),
                parse_arg_list,
                one(TokenType::RParen),
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

fn parse_function_decl<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::FnDecl<'a>, E> {
    map(
        tuple((
            parse_identifier,
            parse_identifier,
            delimited(
                one(TokenType::LParen),
                parse_arg_list,
                one(TokenType::RParen),
            ),
            one(TokenType::Semicolon),
        )),
        |(return_type, name, args, _)| ast::FnDecl {
            return_type,
            name,
            args,
        },
    )(source)
}

fn parse_arg_list<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, Vec<ast::FnArg<'a>>, E> {
    separated_list0(one(TokenType::Comma), parse_arg)(source)
}

fn parse_arg<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::FnArg<'a>, E> {
    map(
        tuple((parse_identifier, parse_identifier)),
        |(type_, name)| ast::FnArg { type_, name },
    )(source)
}

fn parse_block<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Block<'a>, E> {
    map(
        delimited(one(TokenType::LBrace), parse_stmts, one(TokenType::RBrace)),
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
    alt((
        map(
            terminated(parse_return, one(TokenType::Semicolon)),
            ast::Stmt::Return,
        ),
        parse_declaration_stmt,
        map(parse_if_stmt, |stmt| ast::Stmt::If(Box::new(stmt))),
        parse_expr_stmt,
        map(parse_block, |block| ast::Stmt::Block(Box::new(block))),
        parse_null_stmt,
        map(parse_for_loop, |stmt| ast::Stmt::For(Box::new(stmt))),
        map(parse_while_loop, |stmt| ast::Stmt::While(Box::new(stmt))),
        map(parse_do_while_loop, ast::Stmt::DoWhile),
        map(
            terminated(one(TokenType::Break), one(TokenType::Semicolon)),
            |_| ast::Stmt::Break,
        ),
        map(
            terminated(one(TokenType::Continue), one(TokenType::Semicolon)),
            |_| ast::Stmt::Continue,
        ),
    ))(source)
}

fn parse_expr_stmt<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Stmt, E> {
    map(
        terminated(parse_expr, one(TokenType::Semicolon)),
        ast::Stmt::Expr,
    )(source)
}

fn parse_declaration_stmt<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Stmt, E> {
    map(
        terminated(parse_declaration, one(TokenType::Semicolon)),
        ast::Stmt::Decl,
    )(source)
}

fn parse_null_stmt<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Stmt, E> {
    map(one(TokenType::Semicolon), |_| ast::Stmt::Nothing)(source)
}

fn parse_return<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::ReturnStmt, E> {
    map(preceded(one(TokenType::Return), parse_expr), |expr| {
        ast::ReturnStmt { expr }
    })(source)
}

fn parse_declaration<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::DeclarationStmt, E> {
    map(
        tuple((
            parse_identifier,
            parse_identifier,
            opt(preceded(one(TokenType::Equal), parse_expr)),
        )),
        |(type_, name, initializer)| ast::DeclarationStmt {
            type_,
            name,
            initializer,
        },
    )(source)
}

fn parse_if_stmt<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::IfStmt, E> {
    preceded(
        one(TokenType::If),
        map(
            tuple((
                delimited(one(TokenType::LParen), parse_expr, one(TokenType::RParen)),
                parse_stmt,
                opt(preceded(one(TokenType::Else), parse_stmt)),
            )),
            |(cond, body, alternative)| ast::IfStmt {
                cond,
                body,
                alternative,
            },
        ),
    )(source)
}

fn parse_for_loop<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::For, E> {
    log_trying("for loop");
    let (source, res) = tuple((
        one(TokenType::For),
        delimited(
            one(TokenType::LParen),
            tuple((
                alt((parse_declaration_stmt, parse_expr_stmt, parse_null_stmt)),
                alt((parse_expr_stmt, parse_null_stmt)),
                opt(parse_expr),
            )),
            one(TokenType::RParen),
        ),
        parse_stmt,
    ))(source)?;

    Ok((
        source,
        ast::For {
            init: res.1 .0,
            control: if let ast::Stmt::Expr(expr) = res.1 .1 {
                expr
            } else {
                ast::Expr::IntLit(ast::IntLiteral { value: 1 })
            },
            post: res.1 .2,
            body: res.2,
        },
    ))
}

fn parse_while_loop<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::While, E> {
    map(
        tuple((
            one(TokenType::While),
            delimited(one(TokenType::LParen), parse_expr, one(TokenType::RParen)),
            parse_stmt,
        )),
        |(_, cond, body)| ast::While { cond, body },
    )(source)
}

fn parse_do_while_loop<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::DoWhile, E> {
    map(
        tuple((
            one(TokenType::Do),
            parse_block,
            one(TokenType::While),
            delimited(one(TokenType::LParen), parse_expr, one(TokenType::RParen)),
            one(TokenType::Semicolon),
        )),
        |(_, body, _, cond, _)| ast::DoWhile { cond, body },
    )(source)
}

fn parse_expr<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    // TODO:
    alt((
        parse_assignment,
        // map(parse_int_literal, ast::Expr::IntLit),
    ))(source)
}

fn parse_primary_expr<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    alt((
        map(parse_identifier, ast::Expr::Ident),
        map(parse_int_literal, ast::Expr::IntLit),
    ))(source)
}

fn parse_postfix_expr<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_primary_expr(source)?;

    match alt((parse_call_arg_list,))(source) {
        Ok((source, right)) => Ok((
            source,
            ast::Expr::Postfix(Box::new(ast::PostfixExpr { left, right })),
        )),
        Err(nom::Err::Error(_)) => Ok((source, left)),
        Err(failure) => Err(failure),
    }
}

fn parse_call_arg_list<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::PostfixOp, E> {
    map(
        delimited(
            one(TokenType::LParen),
            separated_list0(one(TokenType::Comma), parse_assignment),
            one(TokenType::RParen),
        ),
        ast::PostfixOp::Call,
    )(source)
}

fn parse_unary<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    alt((
        parse_postfix_expr,
        map(tuple((one_unary_symbol, parse_unary)), |(symbol, expr)| {
            ast::Expr::Unary(Box::new(ast::UnaryExpr { symbol, expr }))
        }),
    ))(source)
}

fn parse_int_literal<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::IntLiteral, E> {
    map(eat_int_literal, |value| ast::IntLiteral { value })(source)
}

fn parse_multiplicative<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    // TODO:
    let (source, left) = parse_unary(source)?;

    infix_parser!(fn parse_multiplicative_repeat(
        parse_unary::<E>,
        value(InfixSymbol::Times, one(TokenType::Asterisk)),
        value(InfixSymbol::Divide, one(TokenType::Slash)),
        value(InfixSymbol::Modulo, one(TokenType::Percent))
    ));

    parse_multiplicative_repeat(source, left)
}

// additive -> multiplicative ( ('+'|'-') multiplicative )*
fn parse_additive<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_multiplicative(source)?;

    infix_parser!(fn parse_additive_repeat(
        parse_multiplicative::<E>,
        value(InfixSymbol::Plus, one(TokenType::Plus)),
        value(InfixSymbol::Minus, one(TokenType::Minus))
    ));

    parse_additive_repeat(source, left)
}

fn parse_bit_shift<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_additive(source)?;

    infix_parser!(fn parse_bit_shift_repeat(
        parse_additive::<E>,
        value(InfixSymbol::BitShiftLeft, one(TokenType::BitShiftLeft)),
        value(InfixSymbol::BitShiftRight, one(TokenType::BitShiftRight))
    ));

    parse_bit_shift_repeat(source, left)
}

fn parse_comparator<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_bit_shift(source)?;

    infix_parser!(fn parse_comparator_repeat(
        parse_additive::<E>,
        value(InfixSymbol::Less, one(TokenType::Less)),
        value(InfixSymbol::LessEq, one(TokenType::LessEq)),
        value(InfixSymbol::More, one(TokenType::More)),
        value(InfixSymbol::MoreEq, one(TokenType::MoreEq))
    ));

    parse_comparator_repeat(source, left)
}

fn parse_equality<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_comparator(source)?;

    infix_parser!(fn parse_equality_repeat(
        parse_comparator::<E>,
        value(InfixSymbol::Equality, one(TokenType::EqEqual)),
        value(InfixSymbol::NotEq, one(TokenType::BangEqual))
    ));

    parse_equality_repeat(source, left)
}

fn parse_bitwise_and<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_equality(source)?;

    infix_parser!(fn parse_bitwise_and_repeat(
       parse_equality::<E>,
       value(InfixSymbol::BitAnd, one(TokenType::BitAnd))
    ));

    parse_bitwise_and_repeat(source, left)
}

fn parse_bitwise_xor<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_bitwise_and(source)?;

    infix_parser!(fn parse_bitwise_xor_repeat(
       parse_bitwise_and::<E>,
       value(InfixSymbol::BitXor, one(TokenType::Caret))
    ));

    parse_bitwise_xor_repeat(source, left)
}

fn parse_bitwise_or<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_bitwise_xor(source)?;

    infix_parser!(fn parse_bitwise_or_repeat(
       parse_bitwise_and::<E>,
       value(InfixSymbol::BitOr, one(TokenType::BitOr))
    ));

    parse_bitwise_or_repeat(source, left)
}

fn parse_logical_and<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_bitwise_or(source)?;

    infix_parser!(fn parse_logical_and_repeat(
        parse_bitwise_or::<E>,
        value(InfixSymbol::LogicalAnd, one(TokenType::LogicalAnd))
    ));

    parse_logical_and_repeat(source, left)
}

fn parse_logical_or<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_logical_and(source)?;

    infix_parser!(fn parse_logical_or_repeat(
        parse_logical_and::<E>,
        value(InfixSymbol::LogicalOr, one(TokenType::LogicalOr))
    ));

    parse_logical_or_repeat(source, left)
}

fn parse_conditional<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    let (source, left) = parse_logical_or(source)?;
    let res = tuple((
        one(TokenType::Question),
        parse_expr::<E>,
        one(TokenType::Colon),
        parse_conditional::<E>,
    ))(source);

    match res {
        Ok((source, (_, succ, _, fail))) => Ok((
            source,
            ast::Expr::Conditional(Box::new(ast::ConditionalExpr {
                cond: left,
                succ,
                fail,
            })),
        )),
        _ => Ok((source, left)),
    }
}

fn parse_assignment<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Expr, E> {
    alt((
        map(
            tuple((
                parse_identifier,
                alt((
                    value(AssignSymbol::Equal, one(TokenType::Equal)),
                    value(AssignSymbol::PlusEq, one(TokenType::PlusEq)),
                    value(AssignSymbol::MinusEq, one(TokenType::MinusEq)),
                    value(AssignSymbol::TimesEq, one(TokenType::AsteriskEq)),
                    value(AssignSymbol::DivideEq, one(TokenType::SlashEq)),
                    value(AssignSymbol::ModuloEq, one(TokenType::PercentEq)),
                    value(AssignSymbol::ShiftLeftEq, one(TokenType::LessLessEq)),
                    value(AssignSymbol::ShiftRightEq, one(TokenType::MoreMoreEq)),
                    value(AssignSymbol::AndEq, one(TokenType::AmpersandEq)),
                    value(AssignSymbol::OrEq, one(TokenType::PipeEq)),
                    value(AssignSymbol::XorEq, one(TokenType::CaretEq)),
                )),
                parse_assignment,
            )),
            |(var, symbol, value)| {
                ast::Expr::Assign(Box::new(ast::Assignment { symbol, var, value }))
            },
        ),
        parse_conditional,
    ))(source)
}

// there's gotta be a better way right?
fn one<'a, E: ParseError<ParserIn<'a>>>(
    token: TokenType<'a>,
) -> impl Fn(ParserIn<'a>) -> ParserResult<'a, (), E> {
    move |source| match source.first() {
        Some(tok) if tok.type_ == token => {
            mark!(Ok((&source[1..], ())), "match! one {:?}", token)
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
        value(ast::UnarySign::Negate, one(TokenType::Minus)),
        value(ast::UnarySign::BitComplement, one(TokenType::Tilde)),
        value(ast::UnarySign::LogicNegate, one(TokenType::Exclamation)),
    ))(source)
}

fn eat_int_literal<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, i32, E> {
    match source.first() {
        Some(Token {
            type_: TokenType::Integer(n),
            ..
        }) => mark!(Ok((&source[1..], *n)), "match! {:?}", n),
        _ => mark!(temp_error(source), "not int: {:?}", source.first()),
    }
}

fn parse_identifier<'a, E: ParseError<ParserIn<'a>>>(
    source: ParserIn<'a>,
) -> ParserResult<'a, ast::Identifier<'a>, E> {
    match source.first() {
        Some(Token {
            type_: TokenType::Ident(name),
            ..
        }) => mark!(
            Ok((&source[1..], ast::Identifier { name: &name })),
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
