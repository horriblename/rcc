use lexer::{
    lex_program,
    token::{self},
};

use crate::{
    ast::{self, Block, FnArg, FnDef, IntLiteral, Program, ReturnStmt, Stmt, TopLevel},
    parse_program,
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
            return 1;
        }
        "#;

    let (_, tokens) = lex_program(prog).unwrap();

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
                    body: vec![Stmt::Return(ReturnStmt {
                        expr: ast::Expr::IntLit(IntLiteral { value: 1 }),
                    })],
                },
            })]
        },
    };

    let (_, got) = parse_program(&tokens).unwrap();

    assert_eq!(got, expect);
}
