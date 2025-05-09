use std::collections::HashMap;

pub struct VarInfo {
    /// The offset from base pointer, in units of words.
    /// use `format!("moveq {}(%rbp), %rax", -offset * 8)` to load this var into RAX on a 64-bit
    /// machine (word size = 8 bytes)
    pub offset: VarPosition,
}

/// describes the position of a variable relative to the base pointer,
/// actual address of an argument var is `moveq 8(%rbp)`
/// whereas the address for normal var is `moveq -8(%rbp)`
/// where `8` should be replaced with the actual offset
#[derive(Clone, Copy)]
pub enum VarPosition {
    Arg(u64),
    Var(u64),
}

/// Stuff we need to keep track of when generating function code
pub struct FnIndex {
    /// in the unit of 'words'
    stack_index: u64,

    arg_offset_bytes: u64,
    vars: Vec<Scope>,
}

pub struct Scope {
    vars: HashMap<String, VarInfo>,
    entry_stack_index: u64,
}

#[derive(Debug)]
pub enum Error {
    EmptyScopeStack,
    VariableRedeclared,
}

impl FnIndex {
    pub fn new() -> FnIndex {
        FnIndex {
            stack_index: 1,
            arg_offset_bytes: 1,
            vars: vec![],
        }
    }

    pub fn find_any<'a>(&'a self, var: &str) -> Option<&'a VarInfo> {
        self.vars
            .iter()
            .rev()
            .find_map(move |scope| scope.vars.get(var))
    }

    /// declare a new variable in the top-most scope, if var already exists an error is returned.
    /// Returns the offset from the base pointer as number of words, which can be used as
    /// ```ignore
    /// offset = fn_index.declare("x", 1);
    /// format!("movq 0x{:x}($rbp), %rax", offset * 8 /* 8 is the word size in bytes */)
    /// ```
    pub fn declare(&mut self, var: String, size_words: u64) -> Result<VarPosition, Error> {
        if let Some(scope) = self.vars.last_mut() {
            if scope.vars.contains_key(&var) {
                return Err(Error::VariableRedeclared);
            }

            let offset = VarPosition::Var(self.stack_index);

            scope.vars.insert(var, VarInfo { offset });
            self.stack_index += size_words;
            return Ok(offset);
        }
        return Err(Error::EmptyScopeStack);
    }

    pub fn add_scope(&mut self) {
        self.vars.push(Scope {
            vars: HashMap::new(),
            entry_stack_index: self.stack_index,
        });
    }

    /// declares a new function argument, should be called in the order they are defined in the
    /// arg list
    pub fn declare_function_argument(
        &mut self,
        name: String,
        size_bytes: u64,
    ) -> Result<VarPosition, Error> {
        if let Some(scope) = self.vars.last_mut() {
            if scope.vars.contains_key(&name) {
                return Err(Error::VariableRedeclared);
            }

            let offset = VarPosition::Arg(self.arg_offset_bytes);

            scope.vars.insert(name, VarInfo { offset });
            self.arg_offset_bytes += size_bytes;
            return Ok(offset);
        }
        return Err(Error::EmptyScopeStack);
    }

    /// Pops a scope, returning the number of bytes to deallocate.
    /// The stack pointer _MUST_ be set to the returned offset like this:
    /// ```ignore
    /// const WORD_SIZE_BYTES: u64 = 8;
    /// format!("addq $0x{:x}, %rsp", fn_index.pop_scope() * WORK_SIZE_BYTES);
    /// ```
    // TODO: this returns number of WORDS to deallocate not bytes
    pub fn pop_scope(&mut self) -> u64 {
        let entry_stack_index = self.vars.pop().expect("TODO").entry_stack_index;
        let bytes_to_deallocate = self.stack_index - entry_stack_index;
        return bytes_to_deallocate;
    }
}
