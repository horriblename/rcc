use std::collections::HashMap;

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

struct TypeSignature<'a> {
    return_type: &'a ast::Identifier<'a>,
    args: Vec<&'a ast::Identifier<'a>>,
    defined: bool,
}

struct ProgramState<'a> {
    unique_label_counter: u32,
    // global: Scope,
    scopes: Option<FnIndex>,
    top_level_functions: HashMap<String, TypeSignature<'a>>,
    continue_target: Vec<String>,
    break_target: Vec<String>,
}

impl<'a> ProgramState<'a> {
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
        top_level_functions: HashMap::new(),
        continue_target: Vec::new(),
        break_target: Vec::new(),
    };
    codegen_(&mut state, program, out);
}

fn codegen_<'a>(
    state: &mut ProgramState<'a>,
    program: &'a ast::Program<'a>,
    out: &mut impl std::io::Write,
) {
    write_op!(out, ".section .rodata");
    writeln_!(out, ".LCO:");
    write_op!(out, ".text");
    write_op!(out, ".globl main");
    write_op!(out, ".type main, @function");

    for child in &program.children {
        match child {
            ast::TopLevel::FnDef(func) => gen_fn_def(state, func, out),
            ast::TopLevel::FnDecl(decl) => {
                let func = TypeSignature {
                    return_type: &decl.return_type,
                    args: decl.args.iter().map(|arg| &arg.type_).collect(),
                    defined: false,
                };
                let prev_entry = state
                    .top_level_functions
                    .entry(decl.name.name.name.to_string());

                prev_entry
                    .and_modify(|_| panic!("function {} redeclared", decl.name.name.name))
                    .or_insert(func);
            }
        };
    }
}

fn gen_fn_def(state: &mut ProgramState, fndef: &ast::FnDef, out: &mut impl std::io::Write) {
    state.scopes = Some(FnIndex::new());
    writeln_!(out, "{}:", fndef.name.name.name);

    // TODO: arguments have separate scope than other vars, check if this is supposed to be the
    // case.
    state.scopes.as_mut().expect("TODO").add_scope();

    for arg in fndef.args.iter() {
        let result = state
            .scopes
            .as_mut()
            .expect("TODO")
            .declare_function_argument(arg.name.name.name.to_string(), 1);

        match result {
            Err(scope::Error::EmptyScopeStack) => unreachable!("this is a compiler bug"),
            Err(scope::Error::VariableRedeclared) => {
                panic!("variable redeclared: {}", arg.name.name.name)
            }
            Ok(_) => (),
        };
    }

    if fndef.name.name.name == "main" {
        gen_main(state, fndef, out);
    } else {
        gen_scoped_block(state, &fndef.body, out);
    }

    state.scopes.take();
}

// I'm still not sure how cdecl works, but this compiles so I'll leave it be
fn gen_main(state: &mut ProgramState, fndef: &ast::FnDef, out: &mut impl std::io::Write) {
    // FIXME: empty returns should be replaced with `return 0`

    // push old bp
    write_op!(out, "push %rbp");
    // update bp to old sp
    write_op!(out, "movq %rsp, %rbp");
    gen_scoped_block(state, &fndef.body, out);

    write_op!(out, "ret");
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
                Err(scope::Error::EmptyScopeStack) => unreachable!("this is a compiler bug"),
                Err(scope::Error::VariableRedeclared) => {
                    panic!("variable redeclared: {}", name.name.name)
                }
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

    write_op!(out, "cmpq $0, %rax");
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
    write_op!(out, "cmpq $0, %rax");
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

    write_op!(out, "cmpq $0, %rax");
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
    write_op!(out, "cmpq $0, %rax");
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
                .unwrap_or_else(|| panic!("undeclared variable: {}", &var.name.name));

            let (sign, offset) = match info.offset {
                scope::VarPosition::Arg(offset) => ("", offset),
                scope::VarPosition::Var(offset) => ("-", offset),
            };

            write_op!(
                out,
                "movq {}0x{:x}(%rbp), %rax",
                sign,
                offset * WORD_SIZE_BYTES
            );
        }
        ast::Expr::Infix(expr) => gen_infix_expr(state, expr, out),
        ast::Expr::Unary(expr) => gen_unary_expr(state, expr, out),
        ast::Expr::IntLit(ast::IntLiteral { value }) => write_op!(out, "movq ${}, %rax", value),
        ast::Expr::Assign(expr) => gen_assignment(state, expr, out),
        ast::Expr::Conditional(expr) => gen_conditional(state, expr, out),
        ast::Expr::Postfix(expr) => match &expr.right {
            ast::PostfixOp::Call(args) => gen_function_call(state, &expr.left, args, out),
        },
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

    write_op!(out, "cmpq $0, %rax");
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

fn gen_function_call(
    state: &mut ProgramState,
    func: &ast::Expr,
    args: &[ast::Expr],
    out: &mut impl std::io::Write,
) {
    tag_dbg(out, "call function");

    for arg in args.iter().rev() {
        gen_expr(state, arg, out);
        write_op!(out, "push %rax");
    }

    // write_op!(out, "push %rbp"); // save old call frame
    // write_op!(out, "movq %rsp, %rbp"); // init new call frame

    let func_name = match func {
        ast::Expr::Ident(ast::Identifier { name }) => name.name,
        _ => todo!("calling a non-identifier?"),
    };

    write_op!(out, "call {}", func_name);

    // TODO: assumes all vars are equal sized
    let bytes_to_remove = WORD_SIZE_BYTES * args.len() as u64;
    write_op!(out, "addq ${}, %rsp", bytes_to_remove);
}

fn gen_assignment(state: &mut ProgramState, expr: &ast::Assignment, out: &mut impl std::io::Write) {
    match expr.symbol {
        ast::AssignSymbol::Equal => {
            let info = state
                .scopes
                .as_ref()
                .expect("TODO")
                .find_any(&expr.var.name.name)
                .unwrap_or_else(|| panic!("undeclared variable: {}", &expr.var.name.name));

            let (sign, offset) = match info.offset {
                scope::VarPosition::Arg(offset) => ("", offset),
                scope::VarPosition::Var(offset) => ("-", offset),
            };

            gen_expr(state, &expr.value, out);
            write_op!(
                out,
                "movq %rax, {}0x{:x}(%rbp)",
                sign,
                offset * WORD_SIZE_BYTES
            );
        }
        _ => todo!(),
    }
}

fn gen_unary_expr(state: &mut ProgramState, expr: &ast::UnaryExpr, out: &mut impl std::io::Write) {
    use ast::UnarySign;

    gen_expr(state, &expr.expr, out);

    match expr.symbol {
        UnarySign::Negate => {
            write_op!(out, "neg %rax");
        }
        UnarySign::BitComplement => {
            write_op!(out, "not %rax");
        }
        UnarySign::LogicNegate => {
            write_op!(out, "cmpq $0, %rax");

            // zero out EAX
            write_op!(out, "movq $0, %rax");

            // set AL register (the lower byte of EAX) to 1 iff ZF is on
            write_op!(out, "%al");
        }
    }
}

fn gen_infix_expr(state: &mut ProgramState, expr: &ast::InfixExpr, out: &mut impl std::io::Write) {
    gen_expr(state, &expr.left, out);

    // FIXME: me dumb. the statement below is false, declarations are expressions so I (think) I
    // need to rewrite this function
    // NOTE: (stack) allocations can only happen in declarations, so we can push/pop within an
    // expression without worrying about our internal stack index going out of date (any push must
    // be followed with a pop though)
    match expr.symbol {
        ast::InfixSymbol::Plus => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "addq %rcx, %rax")
        }
        ast::InfixSymbol::Minus => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movq %rax, %rcx");
            write_op!(out, "pop %rax");
            write_op!(out, "subq %rcx, %rax")
        }
        ast::InfixSymbol::Times => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "imul %rcx, %rax")
        }
        ast::InfixSymbol::Divide => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movq %rax, %rbx");
            write_op!(out, "pop %rax");
            // zero out edx, not exactly sure if I need this
            write_op!(out, "xorq %rdx, %rdx");

            // `idivl %ebx` divides the 64-bit int edx:eax (concatenated) by ebx
            // storing the quotient in eax and remainder in edx
            write_op!(out, "idivq %rbx");
        }
        ast::InfixSymbol::Modulo => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movq %rax, %rbx");
            write_op!(out, "pop %rax");
            write_op!(out, "xorq %rdx, %rdx");
            write_op!(out, "idivq %rbx");

            write_op!(out, "movq %rdx, %rax");
        }
        ast::InfixSymbol::Less => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // rax = right, rcx = left
            write_op!(out, "cmpq %rax, %rcx"); // compare and set flags
            write_op!(out, "movq $0, %rax"); // zero out eax
            write_op!(out, "setl %al"); // set if less
        }
        ast::InfixSymbol::More => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // rax = right, rcx = left
            write_op!(out, "cmpq %rax, %rcx"); // compare and set flags
            write_op!(out, "movq $0, %rax"); // zero out eax
            write_op!(out, "setg %al"); // set if less
        }
        ast::InfixSymbol::LessEq => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpq %rax, %rcx");
            write_op!(out, "movq $0, %rax"); // zero out eax
            write_op!(out, "setle %al")
        }
        ast::InfixSymbol::MoreEq => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpq %rax, %rcx");
            write_op!(out, "movq $0, %rax"); // zero out eax
            write_op!(out, "setge %al")
        }
        ast::InfixSymbol::Equality => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpq %rax, %rcx");
            write_op!(out, "movq $0, %rax");
            write_op!(out, "sete %al");
        }
        ast::InfixSymbol::NotEq => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx");
            write_op!(out, "cmpq %rax, %rcx");
            write_op!(out, "movq $0, %rax");
            write_op!(out, "setne %al");
        }
        ast::InfixSymbol::LogicalAnd => {
            let next_clause_label = state.gen_unique_label("next_clause");
            let end_label = state.gen_unique_label("end");
            write_op!(out, "cmpq $0, %rax"); // check if e1 is true
            write_op!(out, "movq $0, %rax");
            write_op!(out, "jne {}", next_clause_label); // e1 != 0 => jump to next clause

            // write_op!(out, "movl $0, %eax");
            write_op!(out, "jmp {}", end_label);
            write_label(out, &next_clause_label);
            gen_expr(state, &expr.right, out);
            write_op!(out, "cmpq $0, %rax"); // check if e2 is true
            write_op!(out, "movq $0, %rax"); // zero out EAX, without changing ZF
            write_op!(out, "setne %al"); // set AL register to 1 iff e2 != 0
            write_label(out, &end_label);
        }
        ast::InfixSymbol::LogicalOr => {
            let next_clause_label = state.gen_unique_label("next_clause");
            let end_label = state.gen_unique_label("end");
            write_op!(out, "cmpq $0, %rax"); // check if e1 is true
            write_op!(out, "movq $0, %rax");
            write_op!(out, "je {}", next_clause_label); // e1 == 0 => jump to next clause
            write_op!(out, "movq $1, %rax");
            write_op!(out, "jmp {}", end_label);
            write_label(out, &next_clause_label);
            gen_expr(state, &expr.right, out);
            write_op!(out, "cmpq $0, %rax"); // check if e2 is true
            write_op!(out, "movq $0, %rax"); // zero out EAX, without changing ZF
            write_op!(out, "setne %al"); // set AL register to 1 iff e2 != 0
            write_label(out, &end_label);
        }
        ast::InfixSymbol::BitAnd => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // eax = right, ecx = left
            write_op!(out, "and %rcx, %rax");
        }
        ast::InfixSymbol::BitOr => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // eax = right, ecx = left
            write_op!(out, "or %rcx, %rax");
        }
        ast::InfixSymbol::BitXor => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "pop %rcx"); // eax = right, ecx = left
            write_op!(out, "xor %rcx, %rax");
        }
        ast::InfixSymbol::BitShiftLeft => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movq %rax, %rcx");
            write_op!(out, "pop %rax"); // eax = left, ecx = right
            write_op!(out, "shl %rcx, %rax");
        }
        ast::InfixSymbol::BitShiftRight => {
            write_op!(out, "push %rax");
            gen_expr(state, &expr.right, out);
            write_op!(out, "movq %rax, %rcx");
            write_op!(out, "pop %rax"); // eax = left, ecx = right
            write_op!(out, "shr %rcx, %rax");
        }
    }
}
