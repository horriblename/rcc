use crate::codegen;
use nom_locate::LocatedSpan;
use std::process::{self};

macro_rules! test_c_file {
    ($name: ident, $file: expr) => {
        #[test]
        fn $name() {
            test_file(stringify!($name), include_str!($file));
        }
    };
}

fn test_file(id: &str, program: &str) {
    let asm_name = format!("out_{}.s", id);
    let out_name = format!("out_{}", id);

    // the test file should have a line stating the expected exit code on top
    // like: // exit code: 0
    let line = program.lines().next().expect("Empty source code");
    let header = "// exit code: ";
    assert!(line.starts_with(header), "First line of source code should contain expected exit code in the first line, example:\n  // exit code: 0");
    let expected_code: i32 = (line[header.len()..])
        .parse()
        .expect(&format!("Invalid exit code in first line: {}", line));

    gen_code(program.into(), &asm_name);
    compile(&asm_name, &out_name);
    assert_eq!(exec_file(&out_name, &[]).status.code(), Some(expected_code));
}

test_c_file!(arithmetic, "tests/arithmetic.c");
test_c_file!(equality, "tests/equality.c");
test_c_file!(inequality, "tests/inequality.c");
test_c_file!(less, "tests/less.c");
test_c_file!(less_eq, "tests/less_eq.c");
test_c_file!(logical_or, "tests/logical_or.c");
test_c_file!(logical_and, "tests/logical_and.c");
test_c_file!(bitwise_and, "tests/bitwise_and.c");
test_c_file!(bitwise_or, "tests/bitwise_or.c");
test_c_file!(bitwise_xor, "tests/bitwise_xor.c");
test_c_file!(bitwise_shiftl, "tests/bitwise_shiftl.c");
test_c_file!(bitwise_shiftr, "tests/bitwise_shiftr.c");
test_c_file!(declaration, "tests/declaration.c");
test_c_file!(assignment, "tests/assignment.c");

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
