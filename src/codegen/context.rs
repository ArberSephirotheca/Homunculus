use crate::compiler::ast::ast::{Expr, Root, Stmt};
/// `CodegenCx` is a struct that holds the compilation unit of the codegen.
use crate::compiler::parse::symbol_table::*;
use crate::compiler::parse::syntax::SyntaxNode;

use super::builder::{InstructionArgumentsBuilder, InstructionBuilder, ProgramBuilder};
use super::common::{
    Instruction, InstructionArgument, InstructionArguments, InstructionBuiltInVariable,
    InstructionScope, InstructionValue, Program, Scheduler, VariableScope,
};
use crate::codegen::common::{IndexKind, InstructionName};
pub struct CodegenCx {
    type_table: SpirvTypeTable,
    variable_table: VariableSymbolTable,
    constant_table: ConstantSymbolTable,
    label_table: LabelTable,
    current_inst_position: u32,
    // also include built in variable
}

impl CodegenCx {
    pub fn new() -> Self {
        Self {
            type_table: SpirvTypeTable::new(),
            variable_table: VariableSymbolTable::new(),
            constant_table: ConstantSymbolTable::new(),
            label_table: LabelTable::new(),
            current_inst_position: 0,
        }
    }
    pub fn increment_inst_position(&mut self) -> u32 {
        self.current_inst_position += 1;
        self.current_inst_position
    }
    pub fn insert_type(&mut self, id: String, ty: SpirvType) {
        self.type_table.insert(id, ty);
    }

    pub fn lookup_type(&self, id: &str) -> Option<&SpirvType> {
        self.type_table.lookup(id)
    }

    pub fn insert_variable(&mut self, name: String, var_info: VariableInfo) {
        self.variable_table.insert(name, var_info);
    }

    pub fn lookup_variable(&self, id: &str) -> Option<VariableInfo> {
        self.variable_table.lookup(id)
    }

    pub fn insert_constant(&mut self, id: String, constant: ConstantInfo) {
        self.constant_table.insert(id, constant);
    }

    pub fn lookup_constant(&self, id: &str) -> Option<&ConstantInfo> {
        self.constant_table.lookup(id)
    }

    pub fn built_in_variable(&self, id: &str) -> Option<BuiltInVariable> {
        match self.lookup_variable(id) {
            Some(var_info) => var_info.get_builtin(),
            None => None,
        }
    }

    pub fn insert_label(&mut self, label: String, position: u32) {
        self.label_table.insert(label, position);
    }

    pub fn lookup_label(&self, label: &str) -> Option<&u32> {
        self.label_table.lookup(label)
    }

    /// get_from_spirv_type will return the InstructionValueType and index for the given SpirvType.
    pub fn get_from_spirv_type(&self, spirv_type: &SpirvType) -> (InstructionValue, IndexKind) {
        match spirv_type {
            // SpirvType::BuiltIn { built_in } => {
            //     let instr_value = match built_in {
            //         BuiltInVariable::NumWorkgroups => InstructionValueType::BuiltIn(InstructionBuiltInVariable::NumWorkgroups),
            //         BuiltInVariable::WorkgroupSize => InstructionValueType::BuiltIn(InstructionBuiltInVariable::WorkgroupSize),
            //         BuiltInVariable::WorkgroupId => InstructionValueType::BuiltIn(InstructionBuiltInVariable::WorkgroupId),
            //         BuiltInVariable::LocalInvocationId => InstructionValueType::BuiltIn(InstructionBuiltInVariable::LocalInvocationId),
            //         BuiltInVariable::GlobalInvocationId => InstructionValueType::BuiltIn(InstructionBuiltInVariable::GlobalInvocationId),
            //         BuiltInVariable::SubgroupSize => InstructionValueType::BuiltIn(InstructionBuiltInVariable::SubgroupSize),
            //         BuiltInVariable::NumSubgroups => InstructionValueType::BuiltIn(InstructionBuiltInVariable::NumSubgroups),
            //         BuiltInVariable::SubgroupId => InstructionValueType::BuiltIn(InstructionBuiltInVariable::SubgroupId),
            //         BuiltInVariable::SubgroupLocalInvocationId => InstructionValueType::BuiltIn(InstructionBuiltInVariable::SubgroupLocalInvocationId),
            //     };
            //     (instr_value, -1)
            // }
            SpirvType::Bool => (InstructionValue::Bool(true), IndexKind::Literal(-1)),
            SpirvType::Int { width, signed } => (InstructionValue::Int(0), IndexKind::Literal(-1)),
            SpirvType::Vector { element, count } => {
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (
                    self.get_from_spirv_type(real_type).0,
                    IndexKind::Literal(*count as i32),
                )
            }
            SpirvType::Array { element, count } => {
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (
                    self.get_from_spirv_type(real_type).0,
                    IndexKind::Literal(*count as i32),
                )
            }
            // fixme: what to do with runtime array? the index is unknown
            SpirvType::RuntimeArray { element } => {
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (
                    self.get_from_spirv_type(real_type).0,
                    IndexKind::Literal(-1),
                )
            }
            // fixme: we only accept one member for now
            // run the function recursively to get the actual type
            SpirvType::Struct { members } => {
                let real_type = self.lookup_type(members.as_str()).unwrap();
                self.get_from_spirv_type(real_type)
            }

            SpirvType::Pointer { ty, storage_class } => {
                let real_type = self.lookup_type(ty.as_str()).unwrap();
                self.get_from_spirv_type(real_type)
                // (innermost_type.0, innermost_type.1)
            } // SpirvType::AccessChain { ty, base, index } => {
              //     let base_info = self.lookup_variable(base.as_str()).unwrap();
              //     // index can be a constant or a SSA variable
              //     if let Some(constant_info) = self.lookup_constant(index.as_str()) {
              //         (
              //             InstructionValue::Pointer(base_info.get_var_name(), base_info),
              //             IndexKind::Literal(constant_info.get_value()),
              //         )
              //     } else if let Some(var_info) = self.lookup_variable(index.as_str()) {
              //         (
              //             InstructionValue::Pointer(base_info.get_var_name(), base_info),
              //             IndexKind::Variable(index.clone()),
              //         )
              //     } else {
              //         panic!("Index {} not found", index)
              //     }

              //     // (InstructionValue::Pointer(base_info.get_var_name(), base_info), index_info.get_value())
              // }
        }
    }

    /// generate_code_for_expr will generate the SPIR-V code for the given expression,
    /// and the generated code will be added to the instruction builder.
    fn generate_code_for_expr(
        &mut self,
        var_name: String,
        expr: &Expr,
    ) -> Option<InstructionArgumentsBuilder> {
        match expr {
            Expr::TypeExpr(type_expr) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg_builder = InstructionArgument::builder();
                let ty = type_expr.ty();
                self.insert_type(var_name.clone(), ty);
                println!("Type {} inserted", var_name);
                // for type expr, we just need to add it to type symbol table, no need to generate code
                None
            }

            Expr::VariableExpr(var_expr) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg_builder = InstructionArgument::builder();
                // fixme: error handling
                let ty_name = var_expr.ty_name().unwrap();
                let storage_class = var_expr.storage_class().unwrap();
                println!("ty_name {:#?}", ty_name);
                // get the actual type of the variable
                let built_in = self.built_in_variable(var_name.as_str());
                println!("Built-in variable {:#?}", built_in);

                let spirv_type = match self.lookup_type(ty_name.text()) {
                    Some(ty) => ty,
                    None => panic!("Type {} not found", ty_name),
                };

                let (instr_value, idx) = match &built_in {
                    Some(b) => {
                        let instr_value =
                            InstructionValue::BuiltIn(InstructionBuiltInVariable::cast(b.clone()));
                        (instr_value, IndexKind::Literal(-1))
                    }
                    None => self.get_from_spirv_type(spirv_type),
                };
                // variable expression would be a variable declaration, so its SSA form is the same as the variable name
                let var_info = VariableInfo {
                    id: var_name.clone(),
                    ty: spirv_type.clone(),
                    access_chain: vec![],
                    storage_class,
                    built_in,
                };
                self.insert_variable(var_name.clone(), var_info);
                // fixme: avoid using unwrap, use better error handling instead
                let arg = inst_arg_builder
                    .name(var_name)
                    .value(instr_value)
                    .index(idx)
                    .scope(VariableScope::cast(&var_expr.storage_class().unwrap()))
                    .build()
                    .unwrap();

                Some(
                    inst_args_builder
                        .num_args(1)
                        .push_argument(arg)
                        .scope(InstructionScope::None),
                )
            }
            // fixme: handle array type
            // LoadExpr will only load to a SSA result ID that has pointer type
            // it will never load to a real variable
            Expr::LoadExpr(load_expr) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg1_builder = InstructionArgument::builder();
                let inst_arg2_builder = InstructionArgument::builder();
                let ty = self.lookup_type(load_expr.ty().unwrap().text()).unwrap();

                // fixme: better error handling
                let pointer_ssa_id = load_expr.pointer().unwrap();
                let pointer_info = self.lookup_variable(pointer_ssa_id.text()).unwrap();
                println!("Pointer info {:#?}", pointer_info);

                self.insert_variable(var_name.clone(), pointer_info.clone());

                // first arg is the pointer to load into
                let arg1 = inst_arg1_builder
                    .name(var_name.clone())
                    // it is intializing a ssa, so the value is None
                    .value(InstructionValue::None)
                    .index(IndexKind::Literal(-1))
                    .scope(VariableScope::Intermediate)
                    .build()
                    .unwrap();

                // second arg is the pointer to load from
                let arg2 = inst_arg2_builder
                    .name(pointer_ssa_id.text().to_string() /* .get_var_name()*/)
                    .value(if pointer_info.is_builtin() {
                        InstructionValue::BuiltIn(InstructionBuiltInVariable::cast(
                            pointer_info.get_builtin().unwrap(),
                        ))
                    } else {
                        // as we are loading from a pointer, the value should be None
                        InstructionValue::None
                    })
                    .index(IndexKind::Literal(-1))
                    .scope(VariableScope::cast(&pointer_info.get_storage_class()))
                    .build()
                    .unwrap();

                Some(
                    inst_args_builder
                        .num_args(2)
                        .push_argument(arg1)
                        .push_argument(arg2)
                        .scope(InstructionScope::None),
                )
            }
            // example: OpAccessChain
            // fixme: need testing
            Expr::VariableRef(var_ref) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg_builder = InstructionArgument::builder();

                // Start with the base variable
                let base_var_name = var_ref.base_var_name().unwrap();
                let base_var_info = self
                    .lookup_variable(base_var_name.text())
                    .expect("OpAccessChain: Base variable not found in symbol table");

                println!("{:#?}", var_ref.ty().unwrap().text());
                let ty = self
                    .lookup_type(var_ref.ty().unwrap().text())
                    .expect("OpAccessChain: Type not found in symbol table");
                // Initialize access chain tracking
                let mut access_chain = base_var_info.access_chain.clone();
                let mut current_type = var_ref.ty().unwrap();

                let index_name = var_ref.index_name().unwrap();
                if let Some(constant_info) = self.lookup_constant(index_name.text()) {
                    // Record the access step
                    // since it is a constant, we can directly use its value
                    access_chain.push(AccessStep::ConstIndex(constant_info.get_int_value()));

                    // // Update the current type based on the access step
                    // current_type = self.get_element_type(&current_type)
                    //     .expect("Failed to get element type from SPIR-V type");
                } else if let Some(var_info) = self.lookup_variable(index_name.text()) {
                    // Record the access step
                    access_chain.push(AccessStep::VariableIndex(index_name.text().to_string()));

                    // // Update the current type based on the access step
                    // current_type = self.get_element_type(&current_type)
                    //     .expect("Failed to get element type from SPIR-V type");
                }

                // Build the final variable information after applying the access chain
                let var_info = VariableInfo {
                    id: base_var_name.text().to_string(), // This should be the new SSA name
                    ty: ty.clone(),
                    access_chain,
                    storage_class: base_var_info.storage_class, // Inherit from base variable
                    built_in: base_var_info.built_in.clone(), // Inherit from base variable if applicable
                };

                // Insert the new variable information into the symbol table
                self.insert_variable(var_name.clone(), var_info.clone());

                // Build the InstructionArgument
                let instr_value = InstructionValue::Pointer(var_name.clone(), var_info);

                // OpAccessChain instruction does not output instruction in TLA+ code
                None
            }
            Expr::ConstExpr(cost_expr) => {
                let ty = self.lookup_type(cost_expr.ty().unwrap().text()).unwrap();
                let value = cost_expr.value().unwrap().text().parse::<i32>().unwrap();
                let signed = match ty {
                    SpirvType::Int { signed, .. } => signed,
                    _ => panic!("ConstExpr: Type {:#?} is not an integer type", ty),
                };
                let constant_info = ConstantInfo::new_int(value, *signed);
                self.insert_constant(var_name.clone(), constant_info);
                println!("Constant {} inserted", var_name);
                // for const expr, we just need to add it to constant symbol table, no need to generate code
                None
            }
            Expr::ConstTrueExpr(const_true_expr) => {
                let constant_info = ConstantInfo::new_bool(true);
                self.insert_constant(var_name.clone(), constant_info);
                // for const expr, we just need to add it to constant symbol table, no need to generate code
                None
            }
            Expr::ConstFalseExpr(const_false_expr) => {
                let constant_info = ConstantInfo::new_bool(false);
                self.insert_constant(var_name.clone(), constant_info);
                // for const expr, we just need to add it to constant symbol table, no need to generate code
                None
            }
            Expr::LabelExpr(labe_expr) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg_builder = InstructionArgument::builder();

                self.insert_label(var_name.clone(), self.current_inst_position + 1);

                let arg1 = inst_arg_builder
                    .index(IndexKind::Literal(-1))
                    .name(var_name)
                    .scope(VariableScope::Global)
                    .value(InstructionValue::None)
                    .build()
                    .unwrap();

                let args = inst_args_builder
                    .num_args(1)
                    .push_argument(arg1)
                    .scope(InstructionScope::None);

                Some(args)
            }
            Expr::AtomicExchangeExpr(atomic_exch_expr) => {
                todo!()
            }
            Expr::AtomicCompareExchangeExpr(atomic_cmp_exch_expr) => {
                todo!()
            }
            // todo: implement the rest of the expressions
            _ => unimplemented!(),
        }
    }

    fn generate_code_for_stmt(&mut self, stmt: &Stmt) -> Option<Instruction> {
        match stmt {
            Stmt::VariableDef(var_def) => {
                let inst_builder = Instruction::builder();
                println!("Variable definition {:#?}", var_def);
                let var_name = var_def.name().unwrap().text().to_string();
                let expr = var_def.value().unwrap();
                let inst_args_builder = self.generate_code_for_expr(var_name, &expr);
                if inst_args_builder.is_none() {
                    None
                } else {
                    Some(
                        inst_builder
                            .arguments(inst_args_builder.unwrap().build().unwrap())
                            .name(InstructionName::Assignment)
                            .position(self.increment_inst_position())
                            .build()
                            .unwrap(),
                    )
                }
            }
            // decorate statement is used to attach built-in variables/metadata to a variable
            Stmt::DecorateStatement(decorate_stmt) => {
                let name = decorate_stmt.name().unwrap();
                let built_in = decorate_stmt.built_in_var().unwrap();
                // fixme: find a better way to represent built-in variables
                let var_info = VariableInfo {
                    // attached variable name is the same as its SSA name
                    id: name.text().to_string(),
                    ty: SpirvType::Bool,
                    access_chain: vec![],
                    storage_class: StorageClass::Global,
                    built_in: Some(BuiltInVariable::cast(built_in.kind())),
                };
                println!("name {:#?}", name);
                println!("Built-in variable {:#?}", var_info);

                self.insert_variable(name.text().to_string(), var_info);
                None
            }
            // fixme:: does not support OpAccesschain yet
            Stmt::StoreStatement(store_stmt) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg1_builder = InstructionArgument::builder();
                let inst_arg2_builder = InstructionArgument::builder();
                // fixme: better error handling
                let pointer_ssa_id = store_stmt.pointer().unwrap();
                let pointer_info = self.lookup_variable(pointer_ssa_id.text()).unwrap();
                println!("Pointer info {:#?}", pointer_info);

                let object_ssa_id = store_stmt.object().unwrap();

                self.insert_variable(pointer_ssa_id.text().to_string(), pointer_info.clone());

                // first arg is the pointer to load into
                let arg1 = inst_arg1_builder
                    .name(pointer_ssa_id.text().to_string())
                    // it is intializing a ssa, so the value is None
                    .value(InstructionValue::None)
                    .index(IndexKind::Literal(-1))
                    .scope(VariableScope::cast(&pointer_info.get_storage_class()))
                    .build()
                    .unwrap();

                // object can be constant or pointer
                let arg2 = if let Some(constant_info) = self.lookup_constant(object_ssa_id.text()) {
                    inst_arg2_builder
                        .name(object_ssa_id.text().to_string())
                        .value(InstructionValue::Int(constant_info.get_int_value()))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::Literal)
                        .build()
                        .unwrap()
                } else if let Some(var_info) = self.lookup_variable(object_ssa_id.text()) {
                    inst_arg2_builder
                        .name(object_ssa_id.text().to_string())
                        .value(InstructionValue::Pointer(
                            var_info.get_var_name(),
                            var_info.clone(),
                        ))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::cast(&var_info.get_storage_class()))
                        .build()
                        .unwrap()
                } else {
                    panic!("object {} not found", object_ssa_id)
                };

                let inst_args = inst_args_builder
                    .num_args(2)
                    .push_argument(arg1)
                    .push_argument(arg2)
                    .scope(InstructionScope::None)
                    .build()
                    .unwrap();
                Some(
                    Instruction::builder()
                        .arguments(inst_args)
                        .name(InstructionName::Store)
                        .position(self.increment_inst_position())
                        .build()
                        .unwrap(),
                )
            }
            Stmt::BranchStatement(branch_stmt) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg_builder = InstructionArgument::builder();
                let label = branch_stmt.label().unwrap();
                let label_position = self.lookup_label(label.text()).unwrap();
                let arg = inst_arg_builder
                    .name(label.text().to_string())
                    .value(InstructionValue::Int(label_position.clone() as i32))
                    .index(IndexKind::Literal(-1))
                    .scope(VariableScope::Literal)
                    .build()
                    .unwrap();
                let inst_args = inst_args_builder
                    .num_args(1)
                    .push_argument(arg)
                    .scope(InstructionScope::None)
                    .build()
                    .unwrap();
                Some(
                    Instruction::builder()
                        .arguments(inst_args)
                        .name(InstructionName::Branch)
                        .position(self.increment_inst_position())
                        .build()
                        .unwrap(),
                )
            }
            Stmt::BranchConditionalStatement(branch_conditional_stmt) => {
                let inst_args_builder = InstructionArguments::builder();
                let inst_arg1_builder = InstructionArgument::builder();
                let inst_arg2_builder = InstructionArgument::builder();
                let inst_arg3_builder = InstructionArgument::builder();
                let condition = branch_conditional_stmt.condition().unwrap();
                let true_label = branch_conditional_stmt.true_label().unwrap();
                let false_label = branch_conditional_stmt.false_label().unwrap();
                let true_label_position = self.lookup_label(true_label.text()).unwrap();
                let false_label_position = self.lookup_label(false_label.text()).unwrap();

                if let Some(constant_info) = self.lookup_constant(condition.text()) {
                    let arg1 = inst_arg1_builder
                        .name(condition.text().to_string())
                        .value(InstructionValue::Bool(constant_info.get_bool_value()))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::Literal)
                        .build()
                        .unwrap();

                    let arg2 = inst_arg2_builder
                        .name(true_label.text().to_string())
                        .value(InstructionValue::Int(*true_label_position as i32))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::Literal)
                        .build()
                        .unwrap();

                    let arg3 = inst_arg3_builder
                        .name(false_label.text().to_string())
                        .value(InstructionValue::Int(*false_label_position as i32))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::Literal)
                        .build()
                        .unwrap();

                    let inst_args = inst_args_builder
                        .num_args(3)
                        .push_argument(arg1)
                        .push_argument(arg2)
                        .push_argument(arg3)
                        .scope(InstructionScope::None)
                        .build()
                        .unwrap();

                    Some(
                        Instruction::builder()
                            .arguments(inst_args)
                            .name(InstructionName::BranchConditional)
                            .position(self.increment_inst_position())
                            .build()
                            .unwrap(),
                    )
                } else if let Some(condition_var_info) = self.lookup_variable(condition.text()) {
                    // condition_var_info = self.lookup_variable(condition.text()).unwrap();

                    let arg1 = inst_arg1_builder
                        .name(condition.text().to_string())
                        .value(InstructionValue::None)
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::cast(&condition_var_info.get_storage_class()))
                        .build()
                        .unwrap();

                    let arg2 = inst_arg2_builder
                        .name(true_label.text().to_string())
                        .value(InstructionValue::Int(*true_label_position as i32))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::Literal)
                        .build()
                        .unwrap();

                    let arg3 = inst_arg3_builder
                        .name(false_label.text().to_string())
                        .value(InstructionValue::Int(*false_label_position as i32))
                        .index(IndexKind::Literal(-1))
                        .scope(VariableScope::Literal)
                        .build()
                        .unwrap();

                    let inst_args = inst_args_builder
                        .num_args(3)
                        .push_argument(arg1)
                        .push_argument(arg2)
                        .push_argument(arg3)
                        .scope(InstructionScope::None)
                        .build()
                        .unwrap();
                    Some(
                        Instruction::builder()
                            .arguments(inst_args)
                            .name(InstructionName::BranchConditional)
                            .position(self.increment_inst_position())
                            .build()
                            .unwrap(),
                    )
                } else {
                    panic!("condition {} not found", condition.text())
                }
            }
            Stmt::LoopMergeStatement(loop_merge_stmt) => {
                todo!()
            }
            Stmt::SelectionMergeStatement(selection_merge_stmt) => {
                todo!()
            }
            _ => unimplemented!(),
        }
    }

    fn generate_code(&mut self, root: SyntaxNode) -> Program {
        let mut program_builder = Program::builder();
        let root = Root::cast(root).unwrap();
        for stmt in root.stmts() {
            let inst = self.generate_code_for_stmt(&stmt);
            match inst {
                Some(i) => program_builder = program_builder.push_instruction(i),
                None => { /* do nothing */ }
            }
        }
        // fixme: remove the hardcoded values
        program_builder
            .num_threads(1)
            .num_work_groups(1)
            .work_group_size(1)
            .subgroup_size(1)
            .scheduler(Scheduler::OBE)
            .build()
            .unwrap()
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr;
    use crate::ast::Root;
    use crate::codegen::common::IndexKind;
    use crate::codegen::common::Instruction;
    use crate::codegen::common::InstructionBuiltInVariable;
    use crate::codegen::common::VariableScope;
    use crate::codegen::context::AccessStep;
    use crate::codegen::context::InstructionBuiltInVariable::SubgroupLocalInvocationId;
    use crate::codegen::context::InstructionValue;
    use crate::codegen::context::VariableInfo;
    use crate::compiler::parse::symbol_table::SpirvType;
    use crate::compiler::parse::symbol_table::StorageClass;
    use crate::compiler::{
        ast::ast::TypeExpr,
        parse::{parser::parse, syntax::TokenKind},
    };

    use super::CodegenCx;
    fn check(input: &str, expected_tree: expect_test::Expect) {
        let parse = parse(input);
        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn check_basic_type_symbol_table() {
        CodegenCx::new();
        let input = "%uint = OpTypeInt 32 0
         %uint_0 = OpVariable %uint Function
        ";
        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        // let basic_type = program.instructions.get(0).unwrap();
        let variable_decl = program.instructions.get(0).unwrap();
        assert_eq!(variable_decl.arguments.num_args, 1);
        assert_eq!(variable_decl.arguments.arguments[0].name, "%uint_0");
        assert_eq!(
            variable_decl.arguments.arguments[0].value,
            InstructionValue::Int(0)
        );
        assert_eq!(
            variable_decl.arguments.arguments[0].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(
            variable_decl.arguments.arguments[0].scope,
            VariableScope::Local
        );
    }

    #[test]
    fn check_high_level_type_symbol_table() {
        CodegenCx::new();
        let input = "%uint = OpTypeInt 32 0
         %v3uint = OpTypeVector %uint 30
         %_ptr_Input_v3uint = OpTypePointer Input %v3uint
         %v3uint_0 = OpVariable %_ptr_Input_v3uint Function
        ";

        let syntax = parse(input).syntax();
        // let root = Root::cast(syntax).unwrap();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        // let basic_type = program.instructions.get(0).unwrap();
        let variable_decl = program.instructions.get(0).unwrap();
        assert_eq!(variable_decl.arguments.num_args, 1);
        assert_eq!(variable_decl.arguments.arguments[0].name, "%v3uint_0");
        assert_eq!(
            variable_decl.arguments.arguments[0].value,
            InstructionValue::Int(0)
        );
        assert_eq!(
            variable_decl.arguments.arguments[0].index,
            IndexKind::Literal(30)
        );
        assert_eq!(
            variable_decl.arguments.arguments[0].scope,
            VariableScope::Local
        );
    }

    #[test]
    fn check_constant() {
        let input = "%int = OpTypeInt 32 1
        %11 = OpConstant %int -5 
        ";

        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        assert_ne!(codegen_ctx.lookup_constant("%11"), None);
        let const_val = codegen_ctx.lookup_constant("%11").unwrap();
        assert_eq!(const_val.get_int_value(), -5);
        assert_eq!(const_val.is_signed(), true);
    }

    #[test]
    fn check_constant_true() {
        let input = "%bool = OpTypeBool
        %11 = OpConstantTrue %bool
        ";

        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        assert_ne!(codegen_ctx.lookup_constant("%11"), None);
        let const_val = codegen_ctx.lookup_constant("%11").unwrap();
        assert_eq!(const_val.get_bool_value(), true);
    }

    #[test]
    fn check_constant_false() {
        let input = "%bool = OpTypeBool
        %11 = OpConstantFalse %bool
        ";

        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        assert_ne!(codegen_ctx.lookup_constant("%11"), None);
        let const_val = codegen_ctx.lookup_constant("%11").unwrap();
        assert_eq!(const_val.get_bool_value(), false);
    }

    #[test]
    fn check_built_in_load() {
        CodegenCx::new();
        let input = "OpDecorate %gl_SubgroupInvocationID BuiltIn SubgroupLocalInvocationId
         %uint = OpTypeInt 32 0
         %_ptr_Input_uint = OpTypePointer Input %uint
         %gl_SubgroupInvocationID = OpVariable %_ptr_Input_uint Input
         %11 = OpLoad %uint %gl_SubgroupInvocationID
        ";

        let syntax = parse(input).syntax();
        // let root = Root::cast(syntax).unwrap();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let builtin_variable_decl = program.instructions.get(0).unwrap();

        assert_eq!(builtin_variable_decl.arguments.num_args, 1);
        assert_eq!(
            builtin_variable_decl.arguments.arguments[0].name,
            "%gl_SubgroupInvocationID"
        );
        assert_eq!(
            builtin_variable_decl.arguments.arguments[0].value,
            InstructionValue::BuiltIn(SubgroupLocalInvocationId)
        );
        assert_eq!(
            builtin_variable_decl.arguments.arguments[0].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(
            builtin_variable_decl.arguments.arguments[0].scope,
            VariableScope::Global
        );

        let var_load = program.instructions.get(1).unwrap();
        println!("{:#?}", var_load);
        assert_eq!(var_load.arguments.num_args, 2);
        assert_eq!(var_load.arguments.arguments[0].name, "%11");
        assert_eq!(
            var_load.arguments.arguments[0].value,
            InstructionValue::None
        );
        assert_eq!(
            var_load.arguments.arguments[0].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(
            var_load.arguments.arguments[0].scope,
            VariableScope::Intermediate
        );

        assert_eq!(
            var_load.arguments.arguments[1].name,
            "%gl_SubgroupInvocationID"
        );
        assert_eq!(
            var_load.arguments.arguments[1].value,
            InstructionValue::BuiltIn(SubgroupLocalInvocationId)
        );
        assert_eq!(
            var_load.arguments.arguments[1].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(var_load.arguments.arguments[1].scope, VariableScope::Global);
    }

    #[test]
    fn check_store_constant() {
        let input = "%int = OpTypeInt 32 1
        %11 = OpConstant %int -5
        %_ptr_Function_uint = OpTypePointer Function %int
        %idx = OpVariable %_ptr_Function_uint Function
        OpStore %idx %11
        ";
        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let store = program.instructions.get(1).unwrap();
        assert_eq!(store.arguments.num_args, 2);
        assert_eq!(store.arguments.arguments[0].name, "%idx");
        assert_eq!(store.arguments.arguments[0].value, InstructionValue::None);
        assert_eq!(store.arguments.arguments[0].index, IndexKind::Literal(-1));
        assert_eq!(store.arguments.arguments[0].scope, VariableScope::Local);

        assert_eq!(store.arguments.arguments[1].name, "%11");
        assert_eq!(
            store.arguments.arguments[1].value,
            InstructionValue::Int(-5)
        );
        assert_eq!(store.arguments.arguments[1].index, IndexKind::Literal(-1));
        assert_eq!(store.arguments.arguments[1].scope, VariableScope::Literal);
    }

    #[test]
    fn check_store_variable() {
        let input = "%uint = OpTypeInt 32 0
        %uint_0 = OpVariable %uint Function
        %_ptr_Function_uint = OpTypePointer Function %uint
        %idx = OpVariable %_ptr_Function_uint Function
        OpStore %idx %uint_0
        ";
        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let store = program.instructions.get(2).unwrap();
        assert_eq!(store.arguments.num_args, 2);
        assert_eq!(store.arguments.arguments[0].name, "%idx");
        assert_eq!(store.arguments.arguments[0].value, InstructionValue::None);
        assert_eq!(store.arguments.arguments[0].index, IndexKind::Literal(-1));
        assert_eq!(store.arguments.arguments[0].scope, VariableScope::Local);

        assert_eq!(store.arguments.arguments[1].name, "%uint_0");
        assert_eq!(
            store.arguments.arguments[1].value,
            InstructionValue::Pointer(
                "%uint_0".to_string(),
                VariableInfo {
                    id: "%uint_0".to_string(),
                    ty: SpirvType::Int {
                        width: 32,
                        signed: false
                    },
                    access_chain: vec![],
                    storage_class: StorageClass::Local,
                    built_in: None
                }
            )
        );
        assert_eq!(store.arguments.arguments[1].index, IndexKind::Literal(-1));
        assert_eq!(store.arguments.arguments[1].scope, VariableScope::Local);
    }

    #[test]
    fn check_access_chain() {
        let input = "%uint = OpTypeInt 32 0
         %v3uint = OpTypeVector %uint 30
         %_ptr_Input_v3uint = OpTypePointer Input %v3uint
         %_ptr_Input_uint = OpTypePointer Workgroup %uint
         %v3uint_0 = OpVariable %_ptr_Input_v3uint Function
         %10 = OpConstant %uint 2
         %11 = OpAccessChain %_ptr_Input_uint %v3uint_0 %10
        ";
        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let var_info = codegen_ctx.lookup_variable("%11");
        assert_ne!(var_info, None);
        let var_info = var_info.unwrap();
        assert_eq!(var_info.id, "%v3uint_0");
        assert_eq!(
            var_info.ty,
            SpirvType::Pointer {
                ty: "%uint".to_string(),
                storage_class: StorageClass::Shared
            }
        );
        assert_eq!(var_info.access_chain.len(), 1);
        assert_eq!(var_info.access_chain[0], AccessStep::ConstIndex(2));
    }

    /*
    layout(std140, binding = 0) uniform UBO {
    float data[10];
    int indices[10];
    };

    void main() {
    int index = int(gl_FragCoord.x);

    // First load: load an index from the 'indices' array
    int tempIndex = indices[index];  // Load index

    // Second load: use the loaded index to load from 'data'
    float value = data[tempIndex];   // Load value using tempIndex
    }
     */
    #[test]
    fn check_load_with_access_chain() {
        let input = "%ptr1 = OpAccessChain %_ptr_Uniform_int %UBO %index 
            %tempIndex = OpLoad %int %ptr1
            %ptr2 = OpAccessChain %_ptr_Uniform_float %UBO %tempIndex
            %value = OpLoad %float %ptr2
            ";
        todo!()
    }

    #[test]
    fn check_label() {
        let input = "%uint = OpTypeInt 32 0
        %v3uint = OpTypeVector %uint 30
        %_ptr_Input_v3uint = OpTypePointer Input %v3uint
        %_ptr_Input_uint = OpTypePointer Workgroup %uint
        %v3uint_0 = OpVariable %_ptr_Input_v3uint Function
        %1 = OpLabel
        ";

        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let label = program.instructions.get(1).unwrap();
        assert_eq!(label.arguments.num_args, 1);
        assert_eq!(label.arguments.arguments[0].name, "%1");
        assert_ne!(codegen_ctx.lookup_label("%1"), None);
        let label = codegen_ctx.lookup_label("%1").unwrap();
        assert_eq!(label, &2);
    }

    #[test]
    fn check_branch() {
        let input = "%1 = OpLabel
        %2 = OpLabel
        OpBranch %2
        ";

        let syntax = parse(input).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let branch = program.instructions.get(2).unwrap();
        assert_eq!(branch.arguments.num_args, 1);
        assert_eq!(branch.arguments.arguments[0].name, "%2");
        assert_eq!(
            branch.arguments.arguments[0].value,
            InstructionValue::Int(2)
        );
        assert_eq!(branch.arguments.arguments[0].index, IndexKind::Literal(-1));
        assert_eq!(branch.arguments.arguments[0].scope, VariableScope::Literal);
    }

    #[test]
    fn check_branch_conditional() {
        let intput = "%1 = OpLabel
        %2 = OpLabel
        %bool = OpTypeBool
        %true = OpConstantTrue %bool
        %v3uint = OpTypeVector %uint 30
        OpBranchConditional %true %1 %2
        ";

        let syntax = parse(intput).syntax();
        let mut codegen_ctx = CodegenCx::new();
        let program = codegen_ctx.generate_code(syntax);
        let branch_conditional = program.instructions.get(2).unwrap();
        assert_eq!(branch_conditional.arguments.num_args, 3);
        assert_eq!(branch_conditional.arguments.arguments[0].name, "%true");
        assert_eq!(
            branch_conditional.arguments.arguments[0].value,
            InstructionValue::Bool(true)
        );
        assert_eq!(
            branch_conditional.arguments.arguments[0].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(
            branch_conditional.arguments.arguments[0].scope,
            VariableScope::Literal
        );

        assert_eq!(branch_conditional.arguments.arguments[1].name, "%1");
        assert_eq!(
            branch_conditional.arguments.arguments[1].value,
            InstructionValue::Int(1)
        );
        assert_eq!(
            branch_conditional.arguments.arguments[1].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(
            branch_conditional.arguments.arguments[1].scope,
            VariableScope::Literal
        );

        assert_eq!(branch_conditional.arguments.arguments[2].name, "%2");
        assert_eq!(
            branch_conditional.arguments.arguments[2].value,
            InstructionValue::Int(2)
        );
        assert_eq!(
            branch_conditional.arguments.arguments[2].index,
            IndexKind::Literal(-1)
        );
        assert_eq!(
            branch_conditional.arguments.arguments[2].scope,
            VariableScope::Literal
        );
    }
}
