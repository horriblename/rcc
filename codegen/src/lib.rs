use parser::ast;

#[cfg(test)]
mod tests;

// like writeln! but always unwrap
macro_rules! writeln_ {
    ($out: expr, $($f: expr),+) => {{
        writeln!($out, $($f),+).unwrap()
    }};
}

macro_rules! write_op {
    ($out: expr, $($f: expr),+) => {{
        write!($out, "\t").unwrap();
        writeln!($out, $($f),+).unwrap()
    }};
}

pub fn codegen(program: &ast::Program, out: &mut impl std::io::Write) {
    write_op!(out, ".section .rodata");
    writeln_!(out, ".LCO:");
    write_op!(out, ".text");
    write_op!(out, ".globl main");
    write_op!(out, ".type main, @function");

    for child in &program.children {
        match child {
            ast::TopLevel::FnDef(func) => gen_fn_def(func, out),
        };
    }
}

fn gen_fn_def(fndef: &ast::FnDef, out: &mut impl std::io::Write) {
    writeln_!(out, "{}:", fndef.name.name.name);
    // push old bp
    write_op!(out, "pushq %rbp");
    // update bp to old sp
    write_op!(out, "movq %rsp, %rbp");
    // update sp
    write_op!(out, "subq $8, %rsp");
    // align the stack pointer to a 16-byte boundary.
    // apparently common for performance
    write_op!(out, "andq $-16, %rsp");
    for stmt in &fndef.body.body {
        gen_stmt(&stmt, out);
    }
}

fn gen_stmt(stmt: &ast::Stmt, out: &mut impl std::io::Write) {
    // writeln_!()
    match stmt {
        ast::Stmt::Return(ret) => gen_return_stmt(&ret, out),
    };
}

fn gen_return_stmt(stmt: &ast::ReturnStmt, out: &mut impl std::io::Write) {
    gen_expr(&stmt.expr, out);
    write_op!(out, "leave");
    write_op!(out, "ret");
}

fn gen_expr(expr: &ast::Expr, out: &mut impl std::io::Write) {
    match expr {
        ast::Expr::Ident(_) => todo!(),
        ast::Expr::Infix(_) => todo!(),
        ast::Expr::Unary(expr) => gen_unary_expr(expr, out),
        ast::Expr::IntLit(ast::IntLiteral { value }) => write_op!(out, "movl ${}, %eax", value),
    }
}

fn gen_unary_expr(expr: &ast::UnaryExpr, out: &mut impl std::io::Write) {
    use ast::UnarySign;

    gen_expr(&expr.expr, out);

    match expr.symbol {
        UnarySign::Negate => {
            write_op!(out, "neg %eax");
        }
        UnarySign::BitComplement => {
            write_op!(out, "not %eax");
        }
        UnarySign::LogicNegate => {
            write_op!(out, "cmpl $0, %eax");

            // zero out EAX
            write_op!(out, "movl $0, %eax");

            // set AL register (the lower byte of EAX) to 1 iff ZF is on
            write_op!(out, "%al");
        }
    }
}
