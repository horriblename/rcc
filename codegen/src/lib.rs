use parser::ast::{self, DeclarationStmt};

use crate::scope::FnIndex;

mod scope;
#[cfg(test)]
mod tests;

const WORD_SIZE_BYTES: u64 = 8;

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

#[allow(dead_code)]
fn tag_dbg(_out: &mut impl std::io::Write, _tag: &str) {
    #[cfg(any(debug, test))]
    writeln!(_out, "// {}", _tag).unwrap();
}

fn write_label(out: &mut impl std::io::Write, label: &str) {
    writeln!(out, "{}:", label).unwrap();
}

struct ProgramState {
    unique_label_counter: u32,
    // global: Scope,
    scopes: Option<FnIndex>,
    continue_target: Vec<String>,
    break_target: Vec<String>,
}

impl ProgramState {
    /// generates a unique label. A `tag` can be provided to make the label more easily
    /// distinguishable. if "" was given as the tag, the default: "label" is used
    fn gen_unique_label(&mut self, tag: &str) -> String {
        self.unique_label_counter += 1;
        let tag = if tag == "" { "label" } else { tag };
        format!("__{}_{}", tag, self.unique_label_counter)
    }
}

pub fn codegen(program: &ast::Program, out: &mut impl std::io::Write) {
    let mut state = ProgramState {
        unique_label_counter: 0,
        scopes: None,
        continue_target: Vec::new(),
        break_target: Vec::new(),
    };
    codegen_(&mut state, program, out);
}

fn codegen_(state: &mut ProgramState, program: &ast::Program, out: &mut impl std::io::Write) {
    write_op!(out, ".section .rodata");
    writeln_!(out, ".LCO:");
    write_op!(out, ".text");
    write_op!(out, ".globl main");
    write_op!(out, ".type main, @function");

    for child in &program.children {
        match child {
            ast::TopLevel::FnDef(func) => gen_fn_def(state, func, out),
        };
    }
}

fn gen_fn_def(state: &mut ProgramState, fndef: &ast::FnDef, out: &mut impl std::io::Write) {
    state.scopes = Some(FnIndex::new());

    writeln_!(out, "{}:", fndef.name.name.name);
    // push old bp
    write_op!(out, "push %rbp");
    // update bp to old sp
    write_op!(out, "movq %rsp, %rbp");
    gen_scoped_block(state, &fndef.body, out);

    write_op!(out, "movq %rbp, %rsp");
    write_op!(out, "popq %rbp");
    write_op!(out, "ret");

    state.scopes.take();
}

fn gen_stmt(state: &mut ProgramState, stmt: &ast::Stmt, out: &mut impl std::io::Write) {
    match stmt {
        ast::Stmt::Return(ret) => gen_return_stmt(state, &ret, out),
        ast::Stmt::Decl(DeclarationStmt {
            name, initializer, ..
        }) => {
            tag_dbg(out, "declaration");
            match state
                .scopes
                .as_mut()
                .expect("TODO")
                .declare(name.name.name.to_string(), 1)
            {
                Err(scope::Error::EmptyScopeStack) => unreachable!("this is a bug"),
                // TODO: better error message
                Err(scope::Error::VariableRedeclared) => panic!("variable redeclared"),
                Ok(_) => {
                    if let Some(expr) = initializer {
                        gen_expr(state, expr, out);
                        write_op!(out, "push %rax")
                    } else {
                        write_op!(out, "push $0");
                    }
                }
            }
        }
        ast::Stmt::Expr(expr) => gen_expr(state, expr, out),
        ast::Stmt::If(stmt) => gen_if_stmt(state, &stmt, out),
        ast::Stmt::Block(block) => gen_scoped_block(state, block, out),
        ast::Stmt::Nothing => (),
        ast::Stmt::For(stmt) => gen_for_loop(state, stmt, out),
        ast::Stmt::While(stmt) => gen_while_loop(state, stmt, out),
        ast::Stmt::DoWhile(stmt) => gen_do_while_loop(state, stmt, out),
        ast::Stmt::Break => {
            let break_target = state
                .break_target
                .last()
                .expect("break statement is not in a for/while loop");
            write_op!(out, "jmp {}", break_target);
        }
        ast::Stmt::Continue => {
            let cont_target = state
                .continue_target
                .last()
                .expect("continue statement is not in a for/while loop");
            write_op!(out, "jmp {}", cont_target);
        }
    };
}

fn gen_if_stmt(state: &mut ProgramState, stmt: &ast::IfStmt, out: &mut impl std::io::Write) {
    tag_dbg(out, "if stmt");
    gen_expr(state, &stmt.cond, out);
    let alt_label = stmt
        .alternative
        .as_ref()
        .map(|_| state.gen_unique_label("alternative"));
    let post_label = state.gen_unique_label("post_if");

    write_op!(out, "cmpl $0, %eax");
    // jump if condition == 0 (condition evaluates to false)
    // jump location is `alt` if it exists, else post-if-statement
    write_op!(out, "je {}", alt_label.as_ref().unwrap_or(&post_label));

    // consequence of if statement
    gen_stmt(state, &stmt.body, out);
    write_op!(out, "jmp {}", &post_label);

    // alternative of if statement, if it exists
    if let (Some(label), Some(alt)) = (alt_label, &stmt.alternative) {
        write_label(out, &label);
        gen_stmt(state, &alt, out);
    }

    write_label(out, &post_label);
}

// the control flow of a for loop:
// 1. Evaluate init
// 2. Evaluate condition
// 3. If it's false, jump to step 7
// 4. Execute statement
// 5. Execute post-expression
// 6. Jump to step 2.
// 7. Finish
fn gen_for_loop(state: &mut ProgramState, stmt: &ast::For, out: &mut impl std::io::Write) {
    let begin_label = state.gen_unique_label("begin_for");
    let end_label = state.gen_unique_label("end_for");
    state.break_target.push(end_label.clone());
    state.continue_target.push(begin_label.clone());

    tag_dbg(out, "for loop");
    gen_stmt(state, &stmt.init, out);

    write_label(out, &begin_label);
    gen_expr(state, &stmt.control, out);
    write_op!(out, "cmpl $0, %eax");
    write_op!(out, "je {}", end_label);

    tag_dbg(out, "for loop body");
    gen_stmt(state, &stmt.body, out);

    if let Some(post) = &stmt.post {
        tag_dbg(out, "for loop post expression");
        gen_expr(state, post, out);
    }

    write_op!(out, "jmp {}", begin_label);

    write_label(out, &end_label);
    state.break_target.pop();
    state.continue_target.pop();
}

// the control flow of a while loop:
// 1. Evaluate condition
// 2. if its false, jump to step 5
// 3. execute body statement
// 4. jump to step 1
// 5. Finish
fn gen_while_loop(state: &mut ProgramState, stmt: &ast::While, out: &mut impl std::io::Write) {
    let begin_label = state.gen_unique_label("begin_while");
    let end_label = state.gen_unique_label("end_while");
    state.break_target.push(end_label.clone());
    state.continue_target.push(begin_label.clone());

    write_label(out, &begin_label);

    tag_dbg(out, "eval while condition");
    gen_expr(state, &stmt.cond, out);

    write_op!(out, "cmpl $0, %eax");
    write_op!(out, "je {}", end_label);

    tag_dbg(out, "execute loop body");
    gen_stmt(state, &stmt.body, out);

    tag_dbg(out, "jump to start of loop");
    write_op!(out, "jmp {}", begin_label);

    write_label(out, &end_label);
    state.break_target.pop();
    state.continue_target.pop();
}

fn gen_do_while_loop(state: &mut ProgramState, stmt: &ast::DoWhile, out: &mut impl std::io::Write) {
    let begin_label = state.gen_unique_label("begin_do_while");
    let end_label = state.gen_unique_label("end_do_while");
    state.break_target.push(end_label.clone());
    state.continue_target.push(begin_label.clone());

    write_label(out, &begin_label);

    tag_dbg(out, "execute loop body");
    gen_scoped_block(state, &stmt.body, out);

    tag_dbg(out, "eval while condition");
    gen_expr(state, &stmt.cond, out);

    tag_dbg(out, "jump to start of loop if true");
    write_op!(out, "cmpl $0, %eax");
    write_op!(out, "jne {}", begin_label);

    write_label(out, &end_label);
    state.break_target.pop();
    state.continue_target.pop();
}

fn gen_return_stmt(
    state: &mut ProgramState,
    stmt: &ast::ReturnStmt,
    out: &mut impl std::io::Write,
) {
    gen_expr(state, &stmt.expr, out);
    write_op!(out, "leave");
    write_op!(out, "ret");
}

fn gen_scoped_block(state: &mut ProgramState, block: &ast::Block, out: &mut impl std::io::Write) {
    state.scopes.as_mut().expect("TODO").add_scope();

    tag_dbg(out, "scope");

    for stmt in &block.body {
        gen_stmt(state, &stmt, out)
    }

    let bytes_to_deallocate = state.scopes.as_mut().expect("TODO").pop_scope();
    write_op!(
        out,
        "addq $0x{:x}, %rsp",
        bytes_to_deallocate * WORD_SIZE_BYTES
    );

    tag_dbg(out, "scope end")
}

/// generates code to evaluate expression and keep result in EAX
fn gen_expr(state: &mut ProgramState, expr: &ast::Expr, out: &mut impl std::io::Write) {
    match expr {
        ast::Expr::Ident(var) => {
            let info = state
                .scopes
                .as_ref()
                .expect("TODO")
                .find_any(&var.name.name)
                .expect("undeclared variable.");
            write_op!(
                out,
                "movl -0x{:x}(%rbp), %eax",
                info.offset * WORD_SIZE_BYTES
            );
        }
        ast::Expr::Infix(expr) => gen_infix_expr(state, expr, out),
        ast::Expr::Unary(expr) => gen_unary_expr(state, expr, out),
        ast::Expr::IntLit(ast::IntLiteral { value }) => write_op!(out, "movl ${}, %eax", value),
        ast::Expr::Assign(expr) => gen_assignment(state, expr, out),
        ast::Expr::Conditional(expr) => gen_conditional(state, expr, out),
    }
}

// e1 ? e2 : e3
fn gen_conditional(
    state: &mut ProgramState,
    expr: &ast::ConditionalExpr,
    out: &mut impl std::io::Write,
) {
    gen_expr(state, &expr.cond, out);
    let alt_label = state.gen_unique_label("alternative");
    let post_label = state.gen_unique_label("post_conditional");

    write_op!(out, "cmpl $0, %eax");
    // jump if e1 == 0
    write_op!(out, "je {}", alt_label);

    // e1 is true, evaluate e2
    gen_expr(state, &expr.succ, out);
    write_op!(out, "jmp {}", &post_label);

    // evaluate e3
    write_label(out, &alt_label);
    gen_expr(state, &expr.fail, out);

    write_label(out, &post_label);
}

fn gen_assignment(state: &mut ProgramState, expr: &ast::Assignment, out: &mut impl std::io::Write) {
    match expr.symbol {
        ast::AssignSymbol::Equal => {
            let offset = state
                .scopes
                .as_ref()
                .expect("TODO")
                .find_any(&expr.var.name.name)
                .expect("undeclared variable.")
                .offset;
            gen_expr(state, &expr.value, out);
            write_op!(out, "movl %eax, -0x{:x}(%rbp)", offset * WORD_SIZE_BYTES);
        }
        _ => todo!(),
    }
}

fn gen_unary_expr(state: &mut ProgramState, expr: &ast::UnaryExpr, out: &mut impl std::io::Write) {
    use ast::UnarySign;

    gen_expr(state, &expr.expr, out);

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

fn gen_infix_expr(state: &mut ProgramState, expr: &ast::InfixExpr, out: &mut impl std::io::Write) {
    gen_expr(state, &expr.left, out);

    // NOTE: (stack) allocations can only happen in declarations, so we can push/pop within an
    // expression without worrying about our internal stack index going out of date (any push must
    // be followed with a pop though)
    match expr.symbol {
        ast::InfixSymbol::Plus => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "addl %ecx, %eax")
        }
        ast::InfixSymbol::Minus => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movl %eax, %ecx");
            write_op!(out, "pop %rax");
            write_op!(out, "subl %ecx, %eax")
        }
        ast::InfixSymbol::Times => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "imul %ecx, %eax")
        }
        ast::InfixSymbol::Divide => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movl %eax, %ebx");
            write_op!(out, "pop %rax");
            // zero out edx, not exactly sure if I need this
            write_op!(out, "xorl %edx, %edx");

            // `idivl %ebx` divides the 64-bit int edx:eax (concatenated) by ebx
            // storing the quotient in eax and remainder in edx
            write_op!(out, "idivl %ebx");
        }
        ast::InfixSymbol::Modulo => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movl %eax, %ebx");
            write_op!(out, "pop %rax");
            write_op!(out, "xorl %edx, %edx");
            write_op!(out, "idivl %ebx");

            write_op!(out, "movl %edx, %eax");
        }
        ast::InfixSymbol::Less => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // rax = right, rcx = left
            write_op!(out, "cmpl %eax, %ecx"); // compare and set flags
            write_op!(out, "movl $0, %eax"); // zero out eax
            write_op!(out, "setl %al"); // set if less
        }
        ast::InfixSymbol::More => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // rax = right, rcx = left
            write_op!(out, "cmpl %eax, %ecx"); // compare and set flags
            write_op!(out, "movl $0, %eax"); // zero out eax
            write_op!(out, "setg %al"); // set if less
        }
        ast::InfixSymbol::LessEq => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpl %eax, %ecx");
            write_op!(out, "movl $0, %eax"); // zero out eax
            write_op!(out, "setle %al")
        }
        ast::InfixSymbol::MoreEq => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpl %eax, %ecx");
            write_op!(out, "movl $0, %eax"); // zero out eax
            write_op!(out, "setge %al")
        }
        ast::InfixSymbol::Equality => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpl %eax, %ecx");
            write_op!(out, "movl $0, %eax");
            write_op!(out, "sete %al");
        }
        ast::InfixSymbol::NotEq => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpl %eax, %ecx");
            write_op!(out, "movl $0, %eax");
            write_op!(out, "setne %al");
        }
        ast::InfixSymbol::LogicalAnd => {
            let next_clause_label = state.gen_unique_label("next_clause");
            let end_label = state.gen_unique_label("end");
            write_op!(out, "cmpl $0, %eax"); // check if e1 is true
            write_op!(out, "movl $0, %eax");
            write_op!(out, "jne {}", next_clause_label); // e1 != 0 => jump to next clause

            // write_op!(out, "movl $0, %eax");
            write_op!(out, "jmp {}", end_label);
            write_label(out, &next_clause_label);
            gen_expr(state, &expr.right, out);
            write_op!(out, "cmpl $0, %eax"); // check if e2 is true
            write_op!(out, "movl $0, %eax"); // zero out EAX, without changing ZF
            write_op!(out, "setne %al"); // set AL register to 1 iff e2 != 0
            write_label(out, &end_label);
        }
        ast::InfixSymbol::LogicalOr => {
            let next_clause_label = state.gen_unique_label("next_clause");
            let end_label = state.gen_unique_label("end");
            write_op!(out, "cmpl $0, %eax"); // check if e1 is true
            write_op!(out, "movl $0, %eax");
            write_op!(out, "je {}", next_clause_label); // e1 == 0 => jump to next clause
            write_op!(out, "movl $1, %eax");
            write_op!(out, "jmp {}", end_label);
            write_label(out, &next_clause_label);
            gen_expr(state, &expr.right, out);
            write_op!(out, "cmpl $0, %eax"); // check if e2 is true
            write_op!(out, "movl $0, %eax"); // zero out EAX, without changing ZF
            write_op!(out, "setne %al"); // set AL register to 1 iff e2 != 0
            write_label(out, &end_label);
        }
        ast::InfixSymbol::BitAnd => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // eax = right, ecx = left
            write_op!(out, "and %ecx, %eax");
        }
        ast::InfixSymbol::BitOr => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // eax = right, ecx = left
            write_op!(out, "or %ecx, %eax");
        }
        ast::InfixSymbol::BitXor => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // eax = right, ecx = left
            write_op!(out, "xor %ecx, %eax");
        }
        ast::InfixSymbol::BitShiftLeft => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movl %eax, %ecx");
            write_op!(out, "pop %rax"); // eax = left, ecx = right
            write_op!(out, "shl %ecx, %eax");
        }
        ast::InfixSymbol::BitShiftRight => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movl %eax, %ecx");
            write_op!(out, "pop %rax"); // eax = left, ecx = right
            write_op!(out, "shr %ecx, %eax");
        }
    }
}
