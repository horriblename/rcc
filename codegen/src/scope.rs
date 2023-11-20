use std::collections::HashMap;

pub struct VarInfo {
    pub offset: u64,
}

pub struct Scope {
    /// in the unit of 'words'
    stack_index: u64,
    vars: HashMap<String, VarInfo>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            stack_index: 1,
            vars: HashMap::new(),
        }
    }
}

pub type ScopeStack = Vec<Scope>;

pub fn find_any<'a>(scopes: &'a ScopeStack, var: &str) -> Option<&'a VarInfo> {
    for s in scopes {
        if let x @ Some(_) = s.vars.get(var) {
            return x;
        }
    }

    None
}

pub fn top_has(scopes: &ScopeStack, var: &str) -> bool {
    scopes.last().is_some_and(|x| x.vars.contains_key(var))
}

#[derive(Debug)]
pub enum Error {
    EmptyScopeStack,
    VariableRedeclared,
}

/// declare a new variable in the top-most scope, if var already exists an error is returned.
/// Returns the offset from the base pointer as number of words, which can be directly used in
/// mov as `format!("movq %rax, {offset}(%ebp)")`
pub fn declare(scopes: &mut ScopeStack, var: String, size_words: u64) -> Result<u64, Error> {
    if let Some(scope) = scopes.last_mut() {
        if scope.vars.contains_key(&var) {
            return Err(Error::VariableRedeclared);
        }

        scope.vars.insert(
            var,
            VarInfo {
                offset: scope.stack_index,
            },
        );
        let offset = scope.stack_index;
        scope.stack_index += size_words;
        return Ok(offset);
    }
    return Err(Error::EmptyScopeStack);
}
