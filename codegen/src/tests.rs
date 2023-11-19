use crate::codegen;
use nom_locate::LocatedSpan;
use std::process::{self};

#[test]
fn test_code_gen() {
    let program = r#"
        int main() {
            return 1 + 2 + 3*9 + 4 + 7/3 - 1;
        }
        "#;
    let asm_name = "out.s";
    let outname = "out";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(35));
}

#[test]
fn test_equality() {
    let program = r#"
        int main() {
            return 3 == 3;
        }
        "#;
    let asm_name = "out_eq.s";
    let outname = "out_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

#[test]
fn test_inequality() {
    let program = r#"
        int main() {
            return 1 != 3;
        }
        "#;
    let asm_name = "out_neq.s";
    let outname = "out_neq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

#[test]
fn test_less() {
    let program = r#"
        int main() {
            return 1 < 3;
        }
        "#;
    let asm_name = "out_less.s";
    let outname = "out_less";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

#[test]
fn test_less_eq() {
    let program = r#"
        int main() {
            return 5 <= 8;
        }
        "#;
    let asm_name = "out_less_eq.s";
    let outname = "out_less_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

fn gen_code(program: LocatedSpan<&str>, asm_name: &str) {
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
    assert_eq!(out.status.code().unwrap(), 0, "compilation failed!");
}

fn exec_file(fname: &str, args: &[&str]) -> process::Output {
    std::process::Command::new(format!("./{}", fname))
        .args(args)
        .output()
        .unwrap()
}
