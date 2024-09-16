//! common is used to stored the common program information(e.g. number of blocks, subgroup size, thread numbers,...) in the codegen module.

use super::constant::Constant;
use crate::compiler::parse::symbol_table::{BuiltInVariable, StorageClass, VariableInfo};
use camino::Utf8Path;
use smallvec::SmallVec;
use std::fs::File;
use std::io::{BufRead, BufReader};

static LAYOUT_CONFIG_HINT: &str = "(* Layout Configuration *)";
static PROGRAM_HINT: &str = "(* Program *)";

#[derive(Debug)]
pub enum BinaryExpr {
    Add,
    Sub,
    Mul,
    Div,
    NotEqual,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InstructionName {
    Assignment,
    Return,
    Load,
    Store,
    Branch,
    BranchConditional,
    Label,
    SelectionMerge,
    LoopMerge,
    AtomicExchange,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Add,
    Sub,
    Mul,
}

#[derive(Debug, PartialEq)]
pub enum VariableScope {
    Intermediate,
    Local,
    Shared,
    Global,
    Literal,
}

impl VariableScope {
    pub fn cast(storage_class: &StorageClass) -> Self {
        match storage_class {
            StorageClass::Global => VariableScope::Global,
            StorageClass::Local => VariableScope::Local,
            StorageClass::Shared => VariableScope::Shared,
            StorageClass::Intermediate => VariableScope::Intermediate,
            StorageClass::Constant => VariableScope::Literal,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InstructionScope {
    // CrossDevice = 0,
    // Device = 1,
    WorkGroup = 2,
    SubGroup = 3,
    Invocation = 4,
    None,
}

#[derive(Debug, PartialEq)]
pub enum InstructionBuiltInVariable {
    NumWorkgroups,
    WorkgroupSize,
    WorkgroupId,
    LocalInvocationId,
    GlobalInvocationId,
    SubgroupSize,
    NumSubgroups,
    SubgroupId,
    SubgroupLocalInvocationId,
}

impl InstructionBuiltInVariable {
    pub(crate) fn cast(var: BuiltInVariable) -> Self {
        match var {
            BuiltInVariable::NumWorkgroups => InstructionBuiltInVariable::NumWorkgroups,
            BuiltInVariable::WorkgroupSize => InstructionBuiltInVariable::WorkgroupSize,
            BuiltInVariable::WorkgroupId => InstructionBuiltInVariable::WorkgroupId,
            BuiltInVariable::LocalInvocationId => InstructionBuiltInVariable::LocalInvocationId,
            BuiltInVariable::GlobalInvocationId => InstructionBuiltInVariable::GlobalInvocationId,
            BuiltInVariable::SubgroupSize => InstructionBuiltInVariable::SubgroupSize,
            BuiltInVariable::NumSubgroups => InstructionBuiltInVariable::NumSubgroups,
            BuiltInVariable::SubgroupId => InstructionBuiltInVariable::SubgroupId,
            BuiltInVariable::SubgroupLocalInvocationId => {
                InstructionBuiltInVariable::SubgroupLocalInvocationId
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum IndexKind {
    Literal(i32),
    Variable(String),
}
#[derive(Debug, PartialEq)]
pub enum InstructionValue {
    None,
    Pointer(String, VariableInfo),
    BuiltIn(InstructionBuiltInVariable),
    Bool(bool),
    // String(String),
    Int(i32),
    UInt(u32),
}

#[derive(Debug)]
pub enum Scheduler {
    OBE,
    HSA,
}

#[derive(Debug)]
pub struct InstructionArgument {
    pub name: String,
    pub scope: VariableScope,
    pub value: InstructionValue,
    pub index: IndexKind,
}

#[derive(Debug)]
pub struct InstructionArguments {
    pub name: InstructionName,
    pub num_args: u32,
    pub arguments: SmallVec<[InstructionArgument; 4]>,
}

#[derive(Debug)]
pub struct Instruction {
    pub position: u32,
    pub name: InstructionName,
    pub scope: InstructionScope,
    pub arguments: InstructionArguments,
}

#[derive(Debug)]
pub struct Thread {
    pub instructions: SmallVec<[Instruction; 10]>,
}

/// `Program` is a struct that holds the program information.
/// `subgroup_size: u32` is the size of the subgroup.
/// `work_group_size: u32` is the size of the work group.
/// `num_work_groups: u32` is the number of work groups.
/// `num_threads: u32` is the number of threads.
/// `scheduler: Scheduler` is the scheduler type.
/// `thread: Vec<Thread>` is a vector of threads.
/// `constants: Vec<Constant>` is a vector of constants.
#[derive(Debug)]
pub struct Program {
    pub global_vars: Vec<VariableInfo>,
    pub subgroup_size: u32,
    pub work_group_size: u32,
    pub num_work_groups: u32,
    pub num_threads: u32,
    pub scheduler: Scheduler,
    pub instructions: SmallVec<[Instruction; 10]>,
    pub constants: SmallVec<[Constant; 10]>,
}
impl Program {
    fn new_tla_variable(&self, var: &VariableInfo) -> String {
        let name = &var.id;
        let output = format!("Var({} : {})", name, var.data_type);
    }

    fn new_tla_constant(&self, var_name : String, value : String) -> String {
        let name = &constant.id;
        format!("{} = {}", name, constant.value)
    }
    pub fn write_to_file(&self, path: &Utf8Path) -> Result<(), std::io::Error> {
        // Open the file for reading
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        // Read all lines into a vector
        let mut lines: Vec<String> = reader.lines().collect::<Result<_, _>>()?;

        // Find the index of the line that matches the hint
        if let Some(index) = lines.iter().position(|line| line.trim() == LAYOUT_CONFIG_HINT) {
            // Insert the new content after the hint line
            lines.insert(index + 1, );

            // Open the file for writing (truncate existing content)
            let mut file = OpenOptions::new()
                .write(true)
                .truncate(true)
                .open(file_path)?;

            // Write all lines back to the file
            for line in lines {
                writeln!(file, "{}", line)?;
            }
        } else {
            eprintln!("Hint not found: {}", hint);
        }

        Ok(())
    }

    fn write_to_global_vars(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        // Write global variables to the writer
        for var in &self.global_vars {
            writeln!(writer, "{:?}", var)?;
        }
        Ok(())
    }

    fn write_to_constant(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        // Write constants to the writer
        for constant in &self.constants {
            writeln!(writer, "{:?}", constant)?;
        }
        Ok(())
    }

    fn write_built_in_variables(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        // Write built-in variables to the writer
        Ok(())
    }
}
