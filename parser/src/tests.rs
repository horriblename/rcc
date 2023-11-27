use lexer::{
    lex_program,
    token::{self},
};
use nom::error::VerboseError;

use crate::{
    ast::{
        self, AssignSymbol, Assignment, Block, DeclarationStmt, Expr, FnArg, FnDef, IfStmt,
        InfixExpr, InfixSymbol, IntLiteral, Program, ReturnStmt, Stmt, TopLevel, UnaryExpr,
        UnarySign,
    },
    parse_expr, parse_program, ParserIn,
};

macro_rules! ident_ast {
    ($name: expr) => {{
        ast::Identifier {
            name: &token::Identifier { name: $name },
        }
    }};
}

fn assign_expr<'a>(symbol: AssignSymbol, var: ast::Identifier<'a>, value: Expr<'a>) -> Expr<'a> {
    Expr::Assign(Box::new(Assignment { symbol, var, value }))
}

fn block<'a>(expr: Vec<Stmt<'a>>) -> Block<'a> {
    Block { body: expr }
}

macro_rules! block_stmt {
    [$($stmts: expr),*] => {{
        Stmt::Block(Box::new(Block {body: vec![$($stmts),*]}))
    }};
}

macro_rules! assign {
    ($symbol: expr, $var: expr, $value: expr) => {{
        Stmt::Expr(Expr::Assign(Box::new(Assignment {
            symbol: $symbol,
            var: ident_ast!($var),
            value: $value,
        })))
    }};
}

macro_rules! op {
    ($symbol:expr, $left:expr, $right:expr) => {{
        ast::Expr::Infix(Box::new(InfixExpr {
            symbol: $symbol,
            left: $left,
            right: $right,
        }))
    }};
    ($symbol:expr, $operand:expr) => {{
        ast::Expr::Unary(Box::new(UnaryExpr {
            symbol: $symbol,
            expr: $operand,
        }))
    }};
}

#[test]
fn test_parse_program() {
    use AssignSymbol::*;

    let prog = r#"
        int main(int argc, char argv) {
            int a;
            int b = ~!-100;
            c = -1;
            a += 1;
            a -= 1;
            a /= 1;
            a *= 1;
            a %= 1;
            a <<= 1;
            a >>= 1;
            {
                a &= 1;
                a |= 1;
                a ^= 1;
            }
            return a;
        }
        "#;

    let (_, tokens) = lex_program(prog.into()).unwrap();

    fn int<'a>(n: i32) -> Expr<'a> {
        Expr::IntLit(IntLiteral { value: n })
    }

    let expect = Program {
        children: {
            vec![TopLevel::FnDef(FnDef {
                return_type: ident_ast!("int"),
                name: ident_ast!("main"),
                args: vec![
                    FnArg {
                        type_: ident_ast!("int"),
                        name: ident_ast!("argc"),
                    },
                    FnArg {
                        type_: ident_ast!("char"),
                        name: ident_ast!("argv"),
                    },
                ],
                body: Block {
                    body: vec![
                        Stmt::Decl(DeclarationStmt {
                            type_: ident_ast!("int"),
                            name: ident_ast!("a"),
                            initializer: None,
                        }),
                        Stmt::Decl(DeclarationStmt {
                            type_: ident_ast!("int"),
                            name: ident_ast!("b"),
                            initializer: Some(op!(
                                UnarySign::BitComplement,
                                op!(UnarySign::LogicNegate, op!(UnarySign::Negate, int(100)))
                            )),
                        }),
                        Stmt::Expr(assign_expr(
                            AssignSymbol::Equal,
                            ident_ast!("c"),
                            op!(UnarySign::Negate, int(1)),
                        )),
                        assign!(PlusEq, "a", int(1)),
                        assign!(MinusEq, "a", int(1)),
                        assign!(DivideEq, "a", int(1)),
                        assign!(TimesEq, "a", int(1)),
                        assign!(ModuloEq, "a", int(1)),
                        assign!(ShiftLeftEq, "a", int(1)),
                        assign!(ShiftRightEq, "a", int(1)),
                        block_stmt![
                            assign!(AndEq, "a", int(1)),
                            assign!(OrEq, "a", int(1)),
                            assign!(XorEq, "a", int(1))
                        ],
                        Stmt::Return(ReturnStmt {
                            expr: Expr::Ident(ident_ast!("a")),
                        }),
                    ],
                },
            })]
        },
    };

    let (_, got) = parse_program::<VerboseError<ParserIn>>(&tokens).unwrap();

    assert_eq!(got, expect);
}

#[test]
fn test_parse_if() {
    let prog = r#"
        int main() {
            if (a == 3)
                return 2;
            else if (1)
                return 4;
            else
                2;
        }
        "#;

    let (_, tokens) = lex_program(prog.into()).unwrap();

    fn int<'a>(n: i32) -> Expr<'a> {
        Expr::IntLit(IntLiteral { value: n })
    }

    let expect = Program {
        children: {
            vec![TopLevel::FnDef(FnDef {
                return_type: ident_ast!("int"),
                name: ident_ast!("main"),
                args: vec![],
                body: Block {
                    body: vec![Stmt::If(Box::new(IfStmt {
                        cond: op!(InfixSymbol::Equality, Expr::Ident(ident_ast!("a")), int(3)),
                        body: Stmt::Return(ReturnStmt { expr: int(2) }),
                        alternative: Some(Stmt::If(Box::new(IfStmt {
                            cond: int(1),
                            body: (Stmt::Return(ReturnStmt { expr: int(4) })),
                            alternative: Some(Stmt::Expr(int(2))),
                        }))),
                    }))],
                },
            })]
        },
    };

    let (_, got) = parse_program::<VerboseError<ParserIn>>(&tokens).unwrap();

    assert_eq!(got, expect);
}

#[test]
fn test_op_precedence() {
    let x = "a = b = 1<8 | 11 || 4^3 > 2 ? a == 4 + 4*3 & 10 : 9 << 1 + 2 && 3 >= 4";

    let (_, tokens) = lexer::lex_program_source(x).unwrap();
    let (rest, tree) = parse_expr::<VerboseError<ParserIn>>(&tokens).unwrap();

    let ans = show_operator_precedence(&tree);
    let expected ="( a = ( b = ( ( ( ( 1 < 8 ) | 11 ) || ( 4 ^ ( 3 > 2 ) ) ) ? ( ( a == ( 4 + ( 4 * 3 ) ) ) & 10 ) : ( ( 9 << ( 1 + 2 ) ) && ( 3 >= 4 ) ) ) ) )";
    dbg!(&ans);

    assert_eq!(rest.len(), 0);
    assert_eq!(ans, expected)
}

fn show_operator_precedence(expr: &ast::Expr) -> String {
    match expr {
        Expr::Unary(expr) => {
            format!(
                "( {}{} )",
                match expr.symbol {
                    UnarySign::Negate => '-',
                    UnarySign::BitComplement => '~',
                    UnarySign::LogicNegate => '!',
                },
                &show_operator_precedence(&expr.expr)
            )
        }
        Expr::Infix(expr) => {
            format!(
                "( {} {} {} )",
                &show_operator_precedence(&expr.left),
                match expr.symbol {
                    InfixSymbol::Plus => "+",
                    InfixSymbol::Minus => "-",
                    InfixSymbol::Times => "*",
                    InfixSymbol::Divide => "/",
                    InfixSymbol::Modulo => "%",
                    InfixSymbol::Less => "<",
                    InfixSymbol::More => ">",
                    InfixSymbol::LessEq => "<=",
                    InfixSymbol::MoreEq => ">=",
                    InfixSymbol::Equality => "==",
                    InfixSymbol::NotEq => "!=",
                    InfixSymbol::LogicalAnd => "&&",
                    InfixSymbol::LogicalOr => "||",
                    InfixSymbol::BitAnd => "&",
                    InfixSymbol::BitOr => "|",
                    InfixSymbol::BitXor => "^",
                    InfixSymbol::BitShiftLeft => "<<",
                    InfixSymbol::BitShiftRight => ">>",
                },
                &show_operator_precedence(&expr.right)
            )
        }
        Expr::Assign(expr) => {
            format!(
                "( {} {} {} )",
                expr.var.name.name,
                match expr.symbol {
                    AssignSymbol::Equal => "=",
                    AssignSymbol::PlusEq => todo!(),
                    AssignSymbol::MinusEq => todo!(),
                    AssignSymbol::DivideEq => todo!(),
                    AssignSymbol::TimesEq => todo!(),
                    AssignSymbol::ModuloEq => todo!(),
                    AssignSymbol::ShiftLeftEq => todo!(),
                    AssignSymbol::ShiftRightEq => todo!(),
                    AssignSymbol::AndEq => todo!(),
                    AssignSymbol::OrEq => todo!(),
                    AssignSymbol::XorEq => todo!(),
                },
                &show_operator_precedence(&expr.value),
            )
        }
        Expr::IntLit(n) => format!("{}", n.value),
        Expr::Ident(ident) => format!("{}", ident.name.name),
        Expr::Conditional(cond) => format!(
            "( {} ? {} : {} )",
            &show_operator_precedence(&cond.cond),
            show_operator_precedence(&cond.succ),
            show_operator_precedence(&cond.fail),
        ),
    }
}
