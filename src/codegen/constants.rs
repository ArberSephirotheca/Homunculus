//! const is used to stored the constants value(e.g. number of blocks, subgroup size, thread numbers,...) in the codegen module.

use crate::compiler::ast::ast::{Expr, Stmt};
use smallvec::SmallVec;
#[derive(Debug)]
pub enum InstructionName{
    Expr(Expr),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum VariableScope{
    Literal,
    Local,
    Shared,
    Global,
}

#[derive(Debug)]
pub enum InstructionScope{
    // CrossDevice = 0,
    // Device = 1,
    WorkGroup = 2,
    SubGroup = 3,
    Invocation = 4,
    None,
}

#[derive(Debug)]
pub enum InstructionValueType{
    Bool(bool),
    String(String),
    Int(i32),
}

#[derive(Debug)]
pub enum Scheduler{
    OBE,
    HSA,
}

#[derive(Debug)]
pub struct GlobalVar{
    pub name: String,
    pub value: String,
    pub index: u32,
}

#[derive(Debug)]
pub struct InstructionArgument{
    pub name: String,
    pub scope: VariableScope,
    pub value: InstructionValueType,
    pub index: u32,
}

#[derive(Debug)]
pub struct InstructionArguments{
    pub num_args: u32,
    pub scope: InstructionScope,
    pub arguments: SmallVec<[InstructionArgument; 4]>,
}

#[derive(Debug)]
pub struct Instruction{
    pub position: u32,
    pub name: InstructionName,
    pub arguments: InstructionArguments,
}

#[derive(Debug)]
pub struct Thread{
    pub instructions: SmallVec<[Instruction; 10]>,
}

/// `Program` is a struct that holds the program information.
/// `subgroup_size: u32` is the size of the subgroup.
/// `work_group_size: u32` is the size of the work group.
/// `num_work_groups: u32` is the number of work groups.
/// `num_threads: u32` is the number of threads.
/// `scheduler: Scheduler` is the scheduler type.
/// `thread: Vec<Thread>` is a vector of threads.
#[derive(Debug)]
pub struct Program{
    pub global_vars: Vec<GlobalVar>,
    pub subgroup_size: u32,
    pub work_group_size: u32,
    pub num_work_groups: u32,
    pub num_threads: u32,
    pub scheduler: Scheduler,
    pub thread: SmallVec<[Thread; 8]>,
}
