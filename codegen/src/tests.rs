use std::process::{self};

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
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(2));
}

fn gen_code(program: &str, asm_name: &str) {
    let (_, tokens) = lexer::lex_program(program).unwrap();
    let (_, ast) = parser::parse_program::<nom::error::Error<_>>(&tokens).unwrap();

    let fname = asm_name;
    let mut outfile = std::fs::File::create(fname).unwrap();

    codegen(&ast, &mut outfile);
}

fn compile(fname: &str, outname: &str) {
    let out = std::process::Command::new("gcc")
        .args(["-m64", fname, "-o", outname])
        .output()
        .unwrap();

    println!("gcc stderr: {}", String::from_utf8(out.stderr).unwrap());
    assert_eq!(out.status.code().unwrap(), 0);
}

fn exec_file(fname: &str, args: &[&str]) -> process::Output {
    std::process::Command::new(format!("./{}", fname))
        .args(args)
        .output()
        .unwrap()
}
