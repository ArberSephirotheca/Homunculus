//! common is used to stored the common program information(e.g. number of blocks, subgroup size, thread numbers,...) in the codegen module.

use super::constant::Constant;
use crate::compiler::parse::symbol_table::{
    BuiltInVariable, ConstantInfo, StorageClass, VariableInfo,
};
use camino::Utf8Path;
use eyre::{eyre, Result, WrapErr};
use smallvec::SmallVec;
use std::fmt::Display;
use std::fs::File;
use std::io::{BufRead, BufReader};

static LAYOUT_CONFIG_HINT: &str = "(* Layout Configuration *)";
static PROGRAM_HINT: &str = "(* Program *)";
static GLOBAL_VARIABLES_HINT: &str = "(* Global Variables *)";

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
    AtomicLoad,
    AtomicStore,
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

impl Display for InstructionName{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionName::Assignment => write!(f, "Assignment"),
            InstructionName::Return => write!(f, "Return"),
            InstructionName::Load => write!(f, "OpLoad"),
            InstructionName::Store => write!(f, "OpStore"),
            InstructionName::AtomicLoad => write!(f, "OpAtomicLoad"),
            InstructionName::AtomicStore => write!(f, "OpAtomicStore"),
            InstructionName::Branch => write!(f, "OpBranch"),
            InstructionName::BranchConditional => write!(f, "OpBranchConditional"),
            InstructionName::Label => write!(f, "OpLabel"),
            InstructionName::SelectionMerge => write!(f, "OpSelectionMerge"),
            InstructionName::LoopMerge => write!(f, "OpLoopMerge"),
            InstructionName::AtomicExchange => write!(f, "OpAtomicExchange"),
            InstructionName::Equal => write!(f, "OpEqual"),
            InstructionName::NotEqual => write!(f, "OpNotEqual"),
            InstructionName::LessThan => write!(f, "OpLess"),
            InstructionName::LessThanEqual => write!(f, "OpLessOrEqual"),
            InstructionName::GreaterThan => write!(f, "OpGreater"),
            InstructionName::GreaterThanEqual => write!(f, "OpGreaterOrEqual"),
            InstructionName::Add => write!(f, "OpAdd"),
            InstructionName::Sub => write!(f, "OpSub"),
            InstructionName::Mul => write!(f, "OpMul"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum VariableScope {
    Intermediate,
    Local,
    Shared,
    Global,
    Literal,
}

impl Display for VariableScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableScope::Intermediate => write!(f, "intermediate"),
            VariableScope::Local => write!(f, "local"),
            VariableScope::Shared => write!(f, "shared"),
            VariableScope::Global => write!(f, "global"),
            VariableScope::Literal => write!(f, "literal"),
        }
    }
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

impl Display for InstructionBuiltInVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionBuiltInVariable::NumWorkgroups => write!(f, "NumWorkGroups"),
            InstructionBuiltInVariable::WorkgroupSize => write!(f, "WorkGroupSize"),
            InstructionBuiltInVariable::SubgroupSize => write!(f, "SubgroupSize"),
            InstructionBuiltInVariable::NumSubgroups => write!(f, "NumSubgroups"),
            InstructionBuiltInVariable::WorkgroupId => write!(f, "WorkGroupId(t)"),
            InstructionBuiltInVariable::LocalInvocationId => write!(f, "LocalInvocationId(t)"),
            InstructionBuiltInVariable::GlobalInvocationId => write!(f, "GlobalInvocationId(t)"),
            InstructionBuiltInVariable::SubgroupId => write!(f, "SubgroupId(t)"),
            InstructionBuiltInVariable::SubgroupLocalInvocationId => {
                write!(f, "SubgroupInvocationId(t)")
            }
        }
    }
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

impl Display for IndexKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexKind::Literal(value) => write!(f, "Index({})", value),
            IndexKind::Variable(name) => write!(f, "{}", name),
        }
    }
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

impl Display for InstructionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionValue::None => write!(f, "\"\""),
            InstructionValue::Pointer(name, _) => write!(f, ""),
            InstructionValue::BuiltIn(var) => write!(f, "{}", var),
            InstructionValue::Bool(value) => {
                if *value {
                    write!(f, "TRUE")
                } else {
                    write!(f, "FALSE")
                }
            }
            InstructionValue::Int(value) => write!(f, "{}", value),
            InstructionValue::UInt(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug)]
pub enum Scheduler {
    OBE,
    HSA,
}
impl std::fmt::Display for Scheduler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Scheduler::OBE => write!(f, "OBE"),
            Scheduler::HSA => write!(f, "HSA"),
        }
    }
}
#[derive(Debug)]
pub struct InstructionArgument {
    pub name: String,
    pub scope: VariableScope,
    pub value: InstructionValue,
    pub index: IndexKind,
}

impl Display for InstructionArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Var(\"{}\", \"{}\", {}, {})", self.scope, self.name, self.value, self.index)
    }
}
#[derive(Debug)]
pub struct InstructionArguments {
    pub name: InstructionName,
    pub num_args: u32,
    pub arguments: SmallVec<[InstructionArgument; 4]>,
}

impl Display for InstructionArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<<")?;
        for idx in 1..=self.arguments.len() {
            write!(f, "{}", self.arguments[idx])?;
            if idx != self.arguments.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, ">>")
    }
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
    /// example: Var("local", "done", FALSE, Index(-1))
    // fn new_tla_variable(&self, var: &VariableInfo) -> String {
    //     let name = &var.id;
    //     var.Instruct
    //     let output = format!("Var(\"{}\", \"{}\", {}, Index(\"{}\"))", var.storage_class, name, var., var.index);
    // }

    fn write_layout(&self, lines: &mut Vec<String>, index: usize) -> Result<()> {
        // Write layout information to the lines
        lines.insert(index + 1, format!("SubgroupSize == {}", self.subgroup_size));
        lines.insert(
            index + 2,
            format!("WorkGroupSize == {}", self.work_group_size),
        );
        lines.insert(
            index + 3,
            format!("NumWorkGroups == {}", self.num_work_groups),
        );
        lines.insert(index + 4, format!("NumSubgroups == {}", self.num_work_groups * self.work_group_size / self.subgroup_size));
        lines.insert(index + 4, format!("NumThreads == {}", self.num_threads));
        lines.insert(index + 5, format!("Scheduler == {}", self.scheduler));
        Ok(())
    }
    fn write_global_variables(&self, lines: &mut Vec<String>, index: usize) -> Result<()> {
        lines.insert(index + 1, format!("InitGPU == "));
        lines.insert(index + 2, format!("globalVars = {{"));
        for (idx, global_var) in self.global_vars.iter().enumerate() {
            lines.insert(
                index + 3 + idx,
                format!(
                    "    Var(\"{}\", \"{}\", {}, Index({}))",
                    global_var.get_storage_class(),
                    global_var.get_var_name(),
                    global_var.initial_value().to_text(),
                    global_var.get_index(),
                ),
            );
        }
        lines.insert(index + 3 + self.global_vars.len(), format!("}}"));
        Ok(())
    }
    fn write_program(&self, lines: &mut Vec<String>, index: usize) -> Result<()> {
        // Write instructions to the lines
        let num_instructions = self.instructions.len();
        lines.insert(index + 1, format!(r"ThreadInstructions ==  [t \in 1..NumThreads |-> <<"));
        for (idx, inst) in self.instructions.iter().enumerate() {
            lines.insert(index + 2 + idx, format!("\"{}\"", inst.name ));
        }
        lines.insert(index + 2 + num_instructions, format!(">>]"));
        
        // Insert ThreadArguments
        lines.insert(index + 3 + num_instructions, format!(r"ThreadArguments == [t \in 1..NumThreads |-> <<"));
        for (idx, inst) in self.instructions.iter().enumerate() {
            lines.insert(index + 4 + num_instructions + idx, format!("{}", inst.arguments));
        }
        lines.insert(index + 4 + num_instructions * 2, format!(">>]"));
        Ok(())
    }

    pub fn write_to_file(&self, path: &Utf8Path) -> Result<()> {
        // Open the file for reading
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        // Read all lines into a vector
        let mut lines: Vec<String> = reader.lines().collect::<Result<_, _>>()?;

        // Find the index of the line that matches the hint
        if let Some(index) = lines
            .iter()
            .position(|line| line.trim() == LAYOUT_CONFIG_HINT)
        {
            // Insert the new content after the hint line
            self.write_layout(&mut lines, index)?;
        } else {
            return Err(eyre!("Layout configuration hint not found in the file."));
        }

        if let Some(index) = lines
            .iter()
            .position(|line| line.trim() == GLOBAL_VARIABLES_HINT)
        {
            // Insert the new content after the hint line
            self.write_global_variables(&mut lines, index)?;
        } else {
            return Err(eyre!("Global variables hint not found in the file."));
        }

        if let Some(index) = lines.iter().position(|line| line.trim() == PROGRAM_HINT) {
            // Insert the new content after the hint line
            self.write_program(&mut lines, index)?;
        } else {
            return Err(eyre!("Program hint not found in the file."));
        }

        Ok(())

    }
}
