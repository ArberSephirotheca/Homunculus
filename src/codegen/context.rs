use crate::compiler::ast::ast::{Expr, Root, Stmt};
/// `CodegenCx` is a struct that holds the compilation unit of the codegen.
use crate::compiler::parse::symbol_table::*;
use crate::compiler::parse::syntax::SyntaxNode;

use super::builder::{Instruction, InstructionBuilder, Program, ProgramBuilder};
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

    /// generate_code_for_expr will generate the SPIR-V code for the given expression,
    /// and the generated code will be added to the instruction builder.
    fn generate_code_for_expr(&mut self, expr: &Expr, inst_builder: &mut InstructionBuilder){
        match expr {
            Expr::VariableExpr(var_expr) => {
                let ty_name = var_expr.ty_name();
                // get the actual type of the variable
                let spirv_type = match self.lookup_type(ty_name.as_str()) {
                    Some(ty) => ty,
                    None => panic!("Type {} not found", ty_name),
                };
            }

            // example: OpAccessChain
            Expr::VariableRef(var_ref) => {
                let var_name = var_ref.name();
                let var_info = self.lookup_variable(&var_name);
                if let Some(var_info) = var_info {
                    // Generate code for the variable reference
                    // For now, we will just return the index of the variable
                    // in the symbol table
                    0
                } else {
                    panic!("Variable {} not found", var_name);
                }
            }
            Expr::LoadExpr(load_expr) => {
                let expr = load_expr.expr();
                self.generate_code_for_expr(&expr.unwrap())
            }
            _ => unimplemented!(),
        }
    }

    fn generate_code_for_stmt(&mut self, stmt: &Stmt) {
        let mut program = Program::builder();
        match stmt {
            Stmt::VariableDef(var_def) => {
                let inst = Instruction::builder();
                let var_name = var_def.name().unwrap().text().to_string();
                let var_info = self.lookup_variable(&var_name);
                // we only create a new variable if it doesn't exist and if its memory is allocated
                // with OpVariable
                if let Some(var_info) = var_info {
                    // Generate code for the variable definition
                    // For now, we will just return the index of the variable
                    // in the symbol table
                    let expr = var_def.value().unwrap();
                    if let Expr::VariableExpr(var_expr) = expr {
                        let ty = self
                            .lookup_type(&var_expr.ty_name())
                            .unwrap_or(panic!("Type {} not found", &var_expr.ty_name()));
                        
                    }
                    let value = self.generate_code_for_expr(&expr);
                } else {
                    panic!("Variable {} not found", &var_name);
                }
            }
            _ => unimplemented!(),
        }
    }

    fn generate_code(&mut self, root: SyntaxNode) {
        let root = Root::cast(root).unwrap();
        for stmt in root.stmts() {
            self.generate_code_for_stmt(&stmt);
        }
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
