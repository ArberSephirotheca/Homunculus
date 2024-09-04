//! common is used to stored the common program information(e.g. number of blocks, subgroup size, thread numbers,...) in the codegen module.

use super::constant::Constant;
use crate::compiler::{
    ast::ast::{Expr, Stmt},
    parse::symbol_table::{BuiltInVariable, SpirvType, StorageClass, VariableInfo},
};
use smallvec::SmallVec;

// #[derive(Debug)]
// pub enum BuiltInVariable {
//     NumWorkgroups,
//     WorkgroupSize,
//     WorkgroupId,
//     LocalInvocationId,
//     GlobalInvocationId,
//     SubgroupSize,
//     NumSubgroups,
//     SubgroupId,
//     SubgroupLocalInvocationId,
//     // There are more built-in variables, but currently we only support these
// }

#[derive(Debug)]
pub enum InstructionName {
    Assignment,
    Store,
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
        }
    }
}

#[derive(Debug)]
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
    String(String),
    Int(i32),
    UInt(u32),
}

#[derive(Debug)]
pub enum Scheduler {
    OBE,
    HSA,
}

#[derive(Debug)]
pub struct GlobalVar {
    pub name: String,
    pub value: InstructionValue,
    pub index: IndexKind,
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
    pub num_args: u32,
    pub scope: InstructionScope,
    pub arguments: SmallVec<[InstructionArgument; 4]>,
}

#[derive(Debug)]
pub struct Instruction {
    // pub position: u32,
    pub name: InstructionName,
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
    pub global_vars: Vec<GlobalVar>,
    pub subgroup_size: u32,
    pub work_group_size: u32,
    pub num_work_groups: u32,
    pub num_threads: u32,
    pub scheduler: Scheduler,
    pub instructions: SmallVec<[Instruction; 10]>,
    pub constants: SmallVec<[Constant; 10]>,
}
