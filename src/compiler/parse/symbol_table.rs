//! We have two tables: one for types and one for variables.
//! for type table, we have a struct SpirvTypeTable with a method insert and lookup
//! Whenever we encounter a type declaration (e.g. `OpTypeInt`, `OpTypeBool`, or complex type like `OpTypeStruct`), we add it to the type table
//! Currently supported types are `OpTypeBool`, `OpTypeInt, `OpTypeVector`, `OpTypeArray`, `OpTypeRuntimeArray`, `OpTypeStruct`, `OpTypePointer`
//! for variable table, we have a struct SymbolTable with a method insert and lookup
//! Whenever we encounter a variable declaration (e.g. `OpVariable`), we add it to the variable table
use super::syntax::TokenKind;
use crate::compiler::parse::syntax::SyntaxNode;
use eyre::Result;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Arc;

type Symbol = String;

#[derive(Debug, PartialEq)]
pub(crate) enum StorageClass {
    // Uniform, Input and Output in SPIR-V are all Global in our case
    Global,
    // Workgroup in SPIR-V is Shared in our case
    Shared,
    // Function in SPIR-V is Local in our case
    Local,
}

/// each time we encounter a type declaration, we add it to the type table
/// for high level type like array and struct, we store the symbol of the element type
/// for low level type like int and bool, we store the type directly
#[derive(Debug, PartialEq)]
pub(crate) enum SpirvType {
    Bool,
    Int {
        width: u32,
        signed: bool,
    },
    Vector {
        element: Arc<Symbol>,
        count: u32,
    },
    Array {
        element: Arc<Symbol>,
        count: u32,
    },
    RuntimeArray {
        element: Arc<Symbol>,
    },
    Struct {
        members: Vec<Symbol>,
    },
    Pointer {
        pointee: Arc<Symbol>,
        storage_class: StorageClass,
    },
}

// Type table, mapping result ID to SpirvType
pub struct SpirvTypeTable {
    types: HashMap<String, SpirvType>,
}

impl SpirvTypeTable {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }
    pub fn insert(&mut self, id: String, ty: SpirvType) {
        self.types.insert(id, ty);
    }

    pub fn lookup(&self, id: &str) -> Option<&SpirvType> {
        self.types.get(id)
    }
}

pub struct VariableInfo {
    pub ty: SpirvType,
    pub storage_class: StorageClass,
}

// Represents a scope level in the symbol table
type Scope = HashMap<String, VariableInfo>;

// Represents the entire symbol table with multiple scopes
pub struct SymbolTable {
    global: Scope,
    shared: Scope,
    local: Scope,
}

impl SymbolTable {
    // Create a new, empty symbol table
    pub fn new() -> Self {
        Self {
            global: HashMap::new(),
            shared: HashMap::new(),
            local: HashMap::new(),
        }
    }

    // Insert a new variable declaration into the current scope
    pub fn insert(&mut self, var_name: String, var_info: VariableInfo) {
        match var_info.storage_class {
            StorageClass::Global => {
                self.global.insert(var_name, var_info);
            }
            StorageClass::Shared => {
                self.shared.insert(var_name, var_info);
            }
            StorageClass::Local => {
                self.local.insert(var_name, var_info);
            }
        }
    }

    // Lookup a variable by name, searching from the innermost scope outward
    pub fn lookup(&self, name: &str) -> Option<&VariableInfo> {
        if let Some(var) = self.local.get(name) {
            return Some(var);
        }
        if let Some(var) = self.shared.get(name) {
            return Some(var);
        }
        if let Some(var) = self.global.get(name) {
            return Some(var);
        }
        None
    }
}

// lazy_static! {
//     pub static ref TYPE_TABLE: SpirvTypeTable = {
//         let mut table = SpirvTypeTable::new();
//         table
//     };
// }
