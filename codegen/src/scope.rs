use std::collections::HashMap;

pub struct VarInfo {
    /// The offset from base pointer, in units of words.
    /// use `format!("moveq {}(%rbp), %rax", -offset * 8)` to load this var into RAX on a 64-bit
    /// machine (word size = 8 bytes)
    pub offset: u64,
}

/// Stuff we need to keep track of when generating function code
pub struct FnIndex {
    /// in the unit of 'words'
    stack_index: u64,

    // Guarantee there is always at least one scope
    vars: Vec<Scope>,
}

pub type Scope = HashMap<String, VarInfo>;

#[derive(Debug)]
pub enum Error {
    EmptyScopeStack,
    VariableRedeclared,
}

impl FnIndex {
    pub fn new() -> FnIndex {
        FnIndex {
            stack_index: 1,
            vars: vec![],
        }
    }

    pub fn find_any<'a>(&'a self, var: &str) -> Option<&'a VarInfo> {
        self.vars.iter().find_map(move |scope| scope.get(var))
    }

    /// declare a new variable in the top-most scope, if var already exists an error is returned.
    /// Returns the offset from the base pointer as number of words, which can be directly used in
    /// mov as `format!("movq %rax, {offset}(%ebp)")`
    pub fn declare(&mut self, var: String, size_words: u64) -> Result<u64, Error> {
        if let Some(scope) = self.vars.last_mut() {
            if scope.contains_key(&var) {
                return Err(Error::VariableRedeclared);
            }

            scope.insert(
                var,
                VarInfo {
                    offset: self.stack_index,
                },
            );
            let offset = self.stack_index;
            self.stack_index += size_words;
            return Ok(offset);
        }
        return Err(Error::EmptyScopeStack);
    }

    pub fn add_scope(&mut self) {
        self.vars.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }
}
