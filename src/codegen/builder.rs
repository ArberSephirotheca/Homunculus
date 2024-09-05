use crate::compiler::parse::symbol_table::SpirvType;

use super::common::*;
use super::constant::*;
use eyre::{eyre, Result};
use smallvec::SmallVec;

#[derive(Default)]
pub struct ConstantBuilder {
    name: Option<String>,
    value: Option<ConstantType>,
}

impl ConstantBuilder {
    pub fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn value(mut self, value: ConstantType) -> Self {
        self.value = Some(value);
        self
    }

    pub fn build(self) -> Result<Constant> {
        Ok(Constant {
            name: self.name.ok_or_else(|| eyre!("Name is required"))?,
            value: self.value.ok_or_else(|| eyre!("Value is required"))?,
        })
    }
}

#[derive(Default)]
pub struct GlobalVarBuilder {
    name: Option<String>,
    value: Option<InstructionValue>,
    index: Option<IndexKind>,
}

impl GlobalVarBuilder {
    pub fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn value(mut self, value: InstructionValue) -> Self {
        self.value = Some(value);
        self
    }

    pub fn index(mut self, index: IndexKind) -> Self {
        self.index = Some(index);
        self
    }

    pub fn build(self) -> Result<GlobalVar> {
        Ok(GlobalVar {
            name: self.name.ok_or_else(|| eyre!("Name is required"))?,
            value: self.value.ok_or_else(|| eyre!("Value is required"))?,
            index: self.index.ok_or_else(|| eyre!("Index is required"))?,
        })
    }
}

impl InstructionArgument {
    pub fn builder() -> InstructionArgumentBuilder {
        InstructionArgumentBuilder::default()
    }
}

#[derive(Default)]
pub struct InstructionArgumentBuilder {
    name: Option<String>,
    scope: Option<VariableScope>,
    value: Option<InstructionValue>,
    index: Option<IndexKind>,
}

impl InstructionArgumentBuilder {
    pub fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn scope(mut self, scope: VariableScope) -> Self {
        self.scope = Some(scope);
        self
    }

    pub fn value(mut self, value: InstructionValue) -> Self {
        self.value = Some(value);
        self
    }

    pub fn index(mut self, index: IndexKind) -> Self {
        self.index = Some(index);
        self
    }

    pub fn build(self) -> Result<InstructionArgument> {
        Ok(InstructionArgument {
            name: self.name.ok_or_else(|| eyre!("Name is required"))?,
            scope: self.scope.ok_or_else(|| eyre!("Scope is required"))?,
            value: self.value.ok_or_else(|| eyre!("Value is required"))?,
            index: self.index.ok_or_else(|| eyre!("Index is required"))?,
        })
    }
}

impl InstructionArguments {
    pub fn builder() -> InstructionArgumentsBuilder {
        InstructionArgumentsBuilder::default()
    }
}

#[derive(Default)]
pub struct InstructionArgumentsBuilder {
    num_args: Option<u32>,
    scope: Option<InstructionScope>,
    arguments: SmallVec<[InstructionArgument; 4]>,
}

impl InstructionArgumentsBuilder {
    pub fn num_args(mut self, num_args: u32) -> Self {
        self.num_args = Some(num_args);
        self
    }

    pub fn scope(mut self, scope: InstructionScope) -> Self {
        self.scope = Some(scope);
        self
    }

    pub fn push_argument(mut self, argument: InstructionArgument) -> Self {
        self.arguments.push(argument);
        self
    }

    pub fn build(self) -> Result<InstructionArguments> {
        Ok(InstructionArguments {
            num_args: self
                .num_args
                .ok_or_else(|| eyre!("Number of arguments is required"))?,
            scope: self
                .scope
                .ok_or_else(|| eyre!("Instruction scope is required"))?,
            arguments: self.arguments,
        })
    }
}

impl Instruction {
    pub fn builder() -> InstructionBuilder {
        InstructionBuilder::default()
    }
}

#[derive(Default)]
pub struct InstructionBuilder {
    position: Option<u32>,
    name: Option<InstructionName>,
    arguments: Option<InstructionArguments>,
}

impl InstructionBuilder {
    pub fn position(mut self, position: u32) -> Self {
        self.position = Some(position);
        self
    }

    pub fn name(mut self, name: InstructionName) -> Self {
        self.name = Some(name);
        self
    }

    pub fn arguments(mut self, arguments: InstructionArguments) -> Self {
        self.arguments = Some(arguments);
        self
    }

    pub fn build(self) -> Result<Instruction> {
        Ok(Instruction {
            position: self.position.ok_or_else(|| eyre!("Position is required"))?,
            name: self.name.ok_or_else(|| eyre!("Name is required"))?,
            arguments: self
                .arguments
                .ok_or_else(|| eyre!("Arguments are required"))?,
        })
    }
}

impl Thread {
    pub fn builder() -> ThreadBuilder {
        ThreadBuilder::default()
    }
}

#[derive(Default)]
pub struct ThreadBuilder {
    instructions: SmallVec<[Instruction; 10]>,
}

impl ThreadBuilder {
    pub fn instruction(mut self, instruction: Instruction) -> Self {
        self.instructions.push(instruction);
        self
    }

    pub fn build(self) -> Thread {
        Thread {
            instructions: self.instructions,
        }
    }
}

impl Program {
    pub fn builder() -> ProgramBuilder {
        ProgramBuilder::default()
    }
}

#[derive(Default)]
pub struct ProgramBuilder {
    global_vars: Vec<GlobalVar>,
    subgroup_size: Option<u32>,
    work_group_size: Option<u32>,
    num_work_groups: Option<u32>,
    num_threads: Option<u32>,
    scheduler: Option<Scheduler>,
    // thread: SmallVec<[Thread; 8]>,
    instructions: SmallVec<[Instruction; 10]>,
    constants: SmallVec<[Constant; 10]>,
}

impl ProgramBuilder {
    pub fn global_var(mut self, global_var: GlobalVar) -> Self {
        self.global_vars.push(global_var);
        self
    }

    pub fn subgroup_size(mut self, subgroup_size: u32) -> Self {
        self.subgroup_size = Some(subgroup_size);
        self
    }

    pub fn work_group_size(mut self, work_group_size: u32) -> Self {
        self.work_group_size = Some(work_group_size);
        self
    }

    pub fn num_work_groups(mut self, num_work_groups: u32) -> Self {
        self.num_work_groups = Some(num_work_groups);
        self
    }

    pub fn num_threads(mut self, num_threads: u32) -> Self {
        self.num_threads = Some(num_threads);
        self
    }

    pub fn scheduler(mut self, scheduler: Scheduler) -> Self {
        self.scheduler = Some(scheduler);
        self
    }

    pub fn push_instruction(mut self, instruction: Instruction) -> Self {
        self.instructions.push(instruction);
        self
    }

    pub fn build(self) -> Result<Program> {
        Ok(Program {
            global_vars: self.global_vars,
            subgroup_size: self
                .subgroup_size
                .ok_or_else(|| eyre!("Subgroup size is required"))?,
            work_group_size: self
                .work_group_size
                .ok_or_else(|| eyre!("Work group size is required"))?,
            num_work_groups: self
                .num_work_groups
                .ok_or_else(|| eyre!("Number of work groups is required"))?,
            num_threads: self
                .num_threads
                .ok_or_else(|| eyre!("Number of threads is required"))?,
            scheduler: self
                .scheduler
                .ok_or_else(|| eyre!("Scheduler is required"))?,
            instructions: self.instructions,
            constants: self.constants,
        })
    }
}
