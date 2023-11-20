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

#[test]
fn test_logical_or() {
    let program = r#"
        int main() {
            return 0 || 1 || 3;
        }
        "#;
    let asm_name = "out_logical_or_eq.s";
    let outname = "out_logical_or_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

#[test]
fn test_logical_and() {
    let program = r#"
        int main() {
            return 3 && 2;
        }
        "#;
    let asm_name = "out_logical_and_eq.s";
    let outname = "out_logical_and_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

#[test]
fn test_bitwise_and() {
    let program = r#"
        int main() {
            return 5 & 3;
        }
        "#;
    let asm_name = "out_bitwise_and_eq.s";
    let outname = "out_bitwise_and_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(1));
}

#[test]
fn test_bitwise_or() {
    let program = r#"
        int main() {
            return 5 | 3;
        }
        "#;
    let asm_name = "out_bitwise_or_eq.s";
    let outname = "out_bitwise_or_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(7));
}

#[test]
fn test_bitwise_xor() {
    let program = r#"
        int main() {
            return 5 ^ 3;
        }
        "#;
    let asm_name = "out_bitwise_xor_eq.s";
    let outname = "out_bitwise_xor_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(6));
}

#[test]
fn test_bitwise_shiftl() {
    let program = r#"
        int main() {
            return 5 << 3;
        }
        "#;
    let asm_name = "out_bitwise_shiftl_eq.s";
    let outname = "out_bitwise_shiftl_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(40));
}

#[test]
fn test_bitwise_shiftr() {
    let program = r#"
        int main() {
            return 10 >> 2;
        }
        "#;
    let asm_name = "out_bitwise_shiftr_eq.s";
    let outname = "out_bitwise_shiftr_eq";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(2));
}

#[test]
fn test_declaration() {
    let program = r#"
        int main() {
            int a = 2;
            return a;
        }
        "#;
    let asm_name = "out_declaration.s";
    let outname = "out_declaration";

    gen_code(program.into(), asm_name);
    compile(asm_name, outname);
    assert_eq!(exec_file(outname, &[]).status.code(), Some(2));
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
