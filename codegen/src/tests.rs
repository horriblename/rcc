use std::{
    io,
    process::{self},
};

use crate::codegen;

#[test]
fn test_code_gen() {
    let program = r#"
        int main() {
            return 2;
        }
        "#;
    let asm_name = "out.s";
    let outname = "out";

    gen_code(program, asm_name);

    let out = compile(asm_name, outname).unwrap();
    println!("gcc stderr: {}", String::from_utf8(out.stderr).unwrap());
    assert_eq!(out.status.code().unwrap(), 0);

    assert_eq!(
        std::process::Command::new(format!("./{}", outname))
            .output()
            .unwrap()
            .status
            .code(),
        Some(2)
    );
}

fn gen_code(program: &str, asm_name: &str) {
    let (_, tokens) = lexer::lex_program(program).unwrap();
    let (_, ast) = parser::parse_program::<nom::error::Error<_>>(&tokens).unwrap();

    let fname = asm_name;
    let mut outfile = std::fs::File::create(fname).unwrap();

    codegen(&ast, &mut outfile);
}

fn compile(fname: &str, outname: &str) -> io::Result<process::Output> {
    std::process::Command::new("gcc")
        .args(["-m64", fname, "-o", outname])
        .output()
}
