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
            return 1 + 3;
        }
        "#;

    let (_, tokens) = lex_program(prog.into()).unwrap();

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
                            expr: Expr::Infix(Box::new(InfixExpr {
                                symbol: InfixSymbol::Plus,
                                left: Expr::IntLit(IntLiteral { value: 1 }),
                                right: Expr::IntLit(IntLiteral { value: 3 }),
                            })),
                        }),
                        // Stmt::Return(ReturnStmt {
                        //     expr: Expr::Infix(Box::new(InfixExpr {
                        //         symbol: InfixSymbol::Plus,
                        //         left: Expr::Infix(Box::new(InfixExpr {
                        //             symbol: InfixSymbol::Times,
                        //             left: Expr::IntLit(IntLiteral { value: 1 }),
                        //             right: Expr::Unary(Box::new(UnaryExpr {
                        //                 symbol: UnarySign::Negate,
                        //                 expr: Expr::IntLit(IntLiteral { value: 2 }),
                        //             })),
                        //         })),
                        //         right: Expr::IntLit(IntLiteral { value: 3 }),
                        //     })),
                        // }),
                    ],
                },
            })]
        },
    };

    let (_, got) = parse_program::<VerboseError<ParserIn>>(&tokens).unwrap();

    assert_eq!(got, expect);
}
