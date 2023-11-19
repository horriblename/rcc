use lexer::{
    lex_program,
    token::{self},
};
use nom::error::VerboseError;

use crate::{
    ast::{
        self, Block, Expr, FnArg, FnDef, InfixExpr, InfixSymbol, IntLiteral, Program, ReturnStmt,
        Stmt, TopLevel, UnaryExpr, UnarySign,
    },
    parse_program, ParserIn,
};

macro_rules! ident_ast {
    ($name: expr) => {{
        ast::Identifier {
            name: &token::Identifier { name: $name },
        }
    }};
}

#[test]
fn test_parse_program() {
    let prog = r#"
        int main(int argc, char argv) {
            return ~100;
            return !0;
            return -1;
            return 1 * -2 + 3;
            return 1 + 2 + 3*9 + 4 + 5 - 1;
            return 1 < 8 || 4 > 2 == 4 + 4*3 && 3 >= 4;
        }
        "#;

    let (_, tokens) = lex_program(prog.into()).unwrap();

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

    fn int_<'a>(n: i32) -> Expr<'a> {
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
                        Stmt::Return(ReturnStmt {
                            expr: Expr::Unary(Box::new(UnaryExpr {
                                symbol: UnarySign::BitComplement,
                                expr: Expr::IntLit(IntLiteral { value: 100 }),
                            })),
                        }),
                        Stmt::Return(ReturnStmt {
                            expr: Expr::Unary(Box::new(UnaryExpr {
                                symbol: UnarySign::LogicNegate,
                                expr: Expr::IntLit(IntLiteral { value: 0 }),
                            })),
                        }),
                        Stmt::Return(ReturnStmt {
                            expr: Expr::Unary(Box::new(UnaryExpr {
                                symbol: UnarySign::Negate,
                                expr: Expr::IntLit(IntLiteral { value: 1 }),
                            })),
                        }),
                        Stmt::Return(ReturnStmt {
                            expr: op!(
                                InfixSymbol::Plus,
                                op!(InfixSymbol::Times, int_(1), op!(UnarySign::Negate, int_(2))),
                                int_(3)
                            ),
                        }),
                        // left associativity
                        Stmt::Return(ReturnStmt {
                            expr: op!(
                                InfixSymbol::Minus,
                                op!(
                                    InfixSymbol::Plus,
                                    op!(
                                        InfixSymbol::Plus,
                                        op!(
                                            InfixSymbol::Plus,
                                            op!(InfixSymbol::Plus, int_(1), int_(2)),
                                            op!(InfixSymbol::Times, int_(3), int_(9))
                                        ),
                                        int_(4)
                                    ),
                                    int_(5)
                                ),
                                Expr::IntLit(IntLiteral { value: 1 })
                            ),
                        }),
                        Stmt::Return(ReturnStmt {
                            expr: op!(
                                InfixSymbol::LogicalOr,
                                op!(InfixSymbol::Less, int_(1), int_(8)),
                                op!(
                                    InfixSymbol::LogicalAnd,
                                    op!(
                                        InfixSymbol::Equality,
                                        op!(InfixSymbol::More, int_(4), int_(2)),
                                        op!(
                                            InfixSymbol::Plus,
                                            int_(4),
                                            op!(InfixSymbol::Times, int_(4), int_(3))
                                        )
                                    ),
                                    op!(InfixSymbol::MoreEq, int_(3), int_(4))
                                )
                            ),
                        }),
                    ],
                },
            })]
        },
    };

    let (_, got) = parse_program::<VerboseError<ParserIn>>(&tokens).unwrap();

    assert_eq!(got, expect);
}
