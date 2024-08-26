use crate::compiler::ast::ast::{Expr, Root, Stmt};
/// `CodegenCx` is a struct that holds the compilation unit of the codegen.
use crate::compiler::parse::symbol_table::*;
use crate::compiler::parse::syntax::SyntaxNode;

use super::builder::{Instruction, InstructionArgumentsBuilder, InstructionBuilder, Program, ProgramBuilder};
use super::common::{InstructionArgument, InstructionArguments, InstructionScope, InstructionValueType, VariableScope};
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
    pub fn get_from_spirv_type(&self, spirv_type : &SpirvType) -> (InstructionValueType, i32){
        match spirv_type{
            SpirvType::Bool => {
                (InstructionValueType::Bool(true), -1)
            }
            SpirvType::Int { width, signed } =>{
                (InstructionValueType::Int(0), -1)
            }
            SpirvType::Vector { element, count } =>{
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, *count as i32)
            }
            SpirvType::Array { element, count } =>{
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, *count as i32)
            }
            // fixme: what to do with runtime array? the index is unknown
            SpirvType::RuntimeArray { element } =>{
                let real_type = self.lookup_type(element.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, -1)
            }
            // fixme: we only accept one member for now
            // run the function recursively to get the actual type
            SpirvType::Struct { members } =>{
                let real_type = self.lookup_type(members[0].as_str()).unwrap();
                self.get_from_spirv_type(real_type)
            }
            SpirvType::Pointer { pointee, storage_class } =>{
                let real_type = self.lookup_type(pointee.as_str()).unwrap();
                (self.get_from_spirv_type(real_type).0, -1)
            }
        }

    }

    /// generate_code_for_expr will generate the SPIR-V code for the given expression,
    /// and the generated code will be added to the instruction builder.
    fn generate_code_for_expr(&mut self, var_name : String, expr: &Expr) -> InstructionArgumentsBuilder{
        match expr {
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
                
                // fixme: better error handling
                let arg = inst_arg_builder
                .name(var_name)
                .value(instr_value_type.into())
                .index(idx)
                .scope(VariableScope::from_storage_class(&var_expr.storage_class()))
                .build()
                .unwrap();
                
                inst_args_builder
                .num_args(1)
                .push_argument(arg)
                .scope(InstructionScope::None)
                
            }

            // example: OpAccessChain
            Expr::VariableRef(var_ref) => {
                todo!()
            }
            // todo: implement the rest of the expressions
            _ => unimplemented!(),
        }
    }

    fn generate_code_for_stmt(&mut self, stmt: &Stmt) -> Instruction{
        let mut program = Program::builder();
        match stmt {
            Stmt::VariableDef(var_def) => {
                let mut inst_builder = Instruction::builder();
                let mut inst_args_builder = InstructionArguments::builder();
                // let var_name = var_def.name().unwrap().text().to_string();
                // let var_info = self.lookup_variable(&var_name);
                // we only create a new variable if it doesn't exist and if its memory is allocated
                // with OpVariable
                // Generate code for the variable definition
                // For now, we will just return the index of the variable
                // in the symbol table
                let var_name = var_def.name().unwrap().text().to_string();
                let expr = var_def.value().unwrap();
                inst_args_builder = self.generate_code_for_expr(var_name,&expr );

                inst_builder
                .arguments(inst_args_builder.build().unwrap())
                .name(InstructionName::Assignment)
                .build()
                .unwrap()
            }
            _ => unimplemented!(),
        }
    }

    fn generate_code(&mut self, root: SyntaxNode) -> Program{
        let mut program_builder = Program::builder();
        let root = Root::cast(root).unwrap();
        for stmt in root.stmts() {
            program_builder = program_builder.push_instruction(self.generate_code_for_stmt(&stmt));
        }
        program_builder.build().unwrap()
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr;
    use crate::ast::Root;
    use crate::compiler::parse::symbol_table::SpirvType;
    use crate::compiler::parse::symbol_table::StorageClass;
    use crate::compiler::{
        ast::ast::TypeExpr,
        parse::{parser::parse, syntax::TokenKind},
    };
    use expect_test::expect;
    fn check(input: &str, expected_tree: expect_test::Expect) {
        let parse = parse(input);
        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn check_type() {
        let parse = parse(
            "OpTypeInt 32 0
        ",
        );
        let syntax = parse.syntax();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();
        let type_expr = match stmt {
            crate::compiler::ast::ast::Stmt::Expr(Expr::TypeExpr(type_expr)) => type_expr,
            _ => panic!("Expected variable definition"),
        };
        assert_eq!(type_expr.name(), "OpTypeInt");
        assert_eq!(
            type_expr.ty(),
            SpirvType::Int {
                width: 32,
                signed: false
            }
        );
    }

    #[test]
    fn check_variable_expr() {
        let input = "OpVariable %_ptr_Uniform_Output Uniform";
        let expected_name = "_ptr_Uniform_Output";
        let parse = parse(input);
        let syntax = parse.syntax();
        let root = Root::cast(syntax).unwrap();
        let stmt = root.stmts().next().unwrap();
        let variable_expr = match stmt {
            crate::compiler::ast::ast::Stmt::Expr(Expr::VariableExpr(variable_expr)) => {
                variable_expr
            }
            _ => panic!("Expected variable definition"),
        };
        assert_eq!(variable_expr.ty_name(), expected_name);
        assert_eq!(variable_expr.storage_class(), StorageClass::Global);
    }
}
