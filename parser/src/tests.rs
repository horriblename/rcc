use lexer::{
    lex_program,
    token::{self},
};
use nom::error::VerboseError;

use crate::{
    ast::{
        self, AssignSymbol, Assignment, Block, DeclarationStmt, Expr, FnArg, FnDef, InfixSymbol,
        IntLiteral, Program, ReturnStmt, Stmt, TopLevel, UnaryExpr, UnarySign,
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

#[test]
fn test_parse_program() {
    let prog = r#"
        int main(int argc, char argv) {
            int a;
            int b = ~!-100;
            c = -1;
            return a;
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
fn test_op_precedence() {
    let x = "a = 1<8 | 11 || 4^3 > 2 == 4 + 4*3 & 9 << 1 + 2 && 3 >= 4";

    let (_, tokens) = lexer::lex_program_source(x).unwrap();
    let (rest, tree) = parse_expr::<VerboseError<ParserIn>>(&tokens).unwrap();

    let ans = show_operator_precedence(&tree);
    let expected ="( a = ( ( ( 1 < 8 ) | 11 ) || ( ( 4 ^ ( ( ( 3 > 2 ) == ( 4 + ( 4 * 3 ) ) ) & ( 9 << ( 1 + 2 ) ) ) ) && ( 3 >= 4 ) ) ) )";
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
                },
                &show_operator_precedence(&expr.value),
            )
        }
        Expr::IntLit(n) => format!("{}", n.value),
        Expr::Ident(ident) => format!("{}", ident.name.name),
    }
}
