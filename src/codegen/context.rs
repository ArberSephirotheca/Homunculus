use crate::compiler::ast::ast::{Expr, Root, Stmt};
/// `CodegenCx` is a struct that holds the compilation unit of the codegen.
use crate::compiler::parse::symbol_table::*;
use crate::compiler::parse::syntax::SyntaxNode;

use super::builder::{InstructionArgumentsBuilder, InstructionBuilder, ProgramBuilder};
use super::common::{
    Instruction, InstructionArgument, InstructionArguments, InstructionScope, InstructionValueType,
    Program, Scheduler, VariableScope,
};
use crate::codegen::common::InstructionName;
pub struct CodegenCx {
    type_table: SpirvTypeTable,
    variable_table: SymbolTable,
}

impl CodegenCx {
    pub fn new() -> Self {
        Self {
            type_table: SpirvTypeTable::new(),
            variable_table: SymbolTable::new(),
        }
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

    pub fn lookup_variable(&self, id: &str) -> Option<&VariableInfo> {
        self.variable_table.lookup(id)
    }

    /// get_from_spirv_type will return the InstructionValueType and index for the given SpirvType.
    pub fn get_from_spirv_type(&self, spirv_type: &SpirvType) -> (InstructionValueType, i32) {
        match spirv_type {
            SpirvType::Bool => (InstructionValueType::Bool(true), -1),
            SpirvType::Int { width, signed } => (InstructionValueType::Int(0), -1),
            SpirvType::Vector { element, count } => {
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, *count as i32)
            }
            SpirvType::Array { element, count } => {
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, *count as i32)
            }
            // fixme: what to do with runtime array? the index is unknown
            SpirvType::RuntimeArray { element } => {
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, -1)
            }
            // fixme: we only accept one member for now
            // run the function recursively to get the actual type
            SpirvType::Struct { members } => {
                let real_type = self.lookup_type(members.as_str()).unwrap();
                self.get_from_spirv_type(real_type)
            }
            SpirvType::Pointer {
                pointee,
                storage_class,
            } => {
                let real_type = self.lookup_type(pointee.as_str()).unwrap();
                self.get_from_spirv_type(real_type)
                // (innermost_type.0, innermost_type.1)
            }
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
                let ty_name = var_expr.ty_name();
                // get the actual type of the variable
                let spirv_type = match self.lookup_type(ty_name.as_str()) {
                    Some(ty) => ty,
                    None => panic!("Type {} not found", ty_name),
                };
                let (instr_value_type, idx) = self.get_from_spirv_type(spirv_type);

                // fixme: avoid using unwrap, use better error handling instead
                let arg = inst_arg_builder
                    .name(var_name)
                    .value(instr_value_type.into())
                    .index(idx)
                    .scope(VariableScope::from_storage_class(&var_expr.storage_class()))
                    .build()
                    .unwrap();

                Some(
                    inst_args_builder
                        .num_args(1)
                        .push_argument(arg)
                        .scope(InstructionScope::None),
                )
            }

            // example: OpAccessChain
            Expr::VariableRef(var_ref) => {
                todo!()
            }
            // todo: implement the rest of the expressions
            _ => unimplemented!(),
        }
    }

    fn generate_code_for_stmt(&mut self, stmt: &Stmt) -> Option<Instruction> {
        let mut program = Program::builder();
        match stmt {
            Stmt::VariableDef(var_def) => {
                let inst_builder = Instruction::builder();
                let var_name: String = var_def.name().unwrap().text().to_string();
                let expr = var_def.value().unwrap();
                let inst_args_builder = self.generate_code_for_expr(var_name, &expr);
                if inst_args_builder.is_none() {
                    None
                } else {
                    Some(
                        inst_builder
                            .arguments(inst_args_builder.unwrap().build().unwrap())
                            .name(InstructionName::Assignment)
                            .build()
                            .unwrap(),
                    )
                }
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
    use crate::codegen::common::Instruction;
    use crate::codegen::common::VariableScope;
    use crate::codegen::context::InstructionValueType;
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
        assert_eq!(variable_decl.arguments.arguments[0].name, "uint_0");
        assert_eq!(
            variable_decl.arguments.arguments[0].value,
            InstructionValueType::Int(0)
        );
        assert_eq!(variable_decl.arguments.arguments[0].index, -1);
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
        assert_eq!(variable_decl.arguments.arguments[0].name, "v3uint_0");
        assert_eq!(
            variable_decl.arguments.arguments[0].value,
            InstructionValueType::Int(0)
        );
        assert_eq!(variable_decl.arguments.arguments[0].index, 30);
        assert_eq!(
            variable_decl.arguments.arguments[0].scope,
            VariableScope::Local
        );
    }
}
