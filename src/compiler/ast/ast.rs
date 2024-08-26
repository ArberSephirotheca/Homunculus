use rowan::TokenAtOffset;

use crate::compiler::parse::symbol_table::{SpirvType, StorageClass};
use crate::compiler::parse::syntax::{SyntaxElement, SyntaxNode, SyntaxToken, TokenKind};

#[derive(Debug)]
pub struct Root(SyntaxNode);
#[derive(Debug)]
pub struct VariableRef(SyntaxNode);
#[derive(Debug)]
pub struct VariableDef(SyntaxNode);
#[derive(Debug)]
pub struct FuncStatement(SyntaxNode);
#[derive(Debug)]
pub struct VariableExpr(SyntaxNode);
#[derive(Debug)]
pub struct TypeExpr(SyntaxNode);

#[derive(Debug)]
pub struct LabelExpr(SyntaxNode);
#[derive(Debug)]
pub struct LoadExpr(SyntaxNode);
#[derive(Debug)]
pub struct StoreExpr(SyntaxNode);
#[derive(Debug)]
pub struct ConstExpr(SyntaxNode);
#[derive(Debug)]
pub struct EqualExpr(SyntaxNode);
#[derive(Debug)]
pub struct NotEqualExpr(SyntaxNode);
#[derive(Debug)]
pub struct LessThanExpr(SyntaxNode);
#[derive(Debug)]
pub struct GreaterThanExpr(SyntaxNode);
#[derive(Debug)]
pub struct LessThanEqualExpr(SyntaxNode);
#[derive(Debug)]
pub struct GreaterThanEqualExpr(SyntaxNode);
#[derive(Debug)]
pub struct AtomicExchangeExpr(SyntaxNode);
#[derive(Debug)]
pub struct AtomicCompareExchangeExpr(SyntaxNode);

#[derive(Debug)]
pub struct ReturnStatement(SyntaxNode);
#[derive(Debug)]
pub struct BranchConditionalStatement(SyntaxNode);
#[derive(Debug)]
pub struct BranchStatement(SyntaxNode);
#[derive(Debug)]
pub struct SwitchStatement(SyntaxNode);
#[derive(Debug)]
pub struct LoopMergeStatement(SyntaxNode);
#[derive(Debug)]
pub struct SelectionMergeStatement(SyntaxNode);

#[derive(Debug)]
pub enum Expr {
    VariableExpr(VariableExpr),
    TypeExpr(TypeExpr),
    VariableRef(VariableRef),
    LabelExpr(LabelExpr),
    LoadExpr(LoadExpr),
    StoreExpr(StoreExpr),
    ConstExpr(ConstExpr),
    EqualExpr(EqualExpr),
    NotEqualExpr(NotEqualExpr),
    LessThanExpr(LessThanExpr),
    GreaterThanExpr(GreaterThanExpr),
    LessThanEqualExpr(LessThanEqualExpr),
    GreaterThanEqualExpr(GreaterThanEqualExpr),
    AtomicExchangeExpr(AtomicExchangeExpr),
    AtomicCompareExchangeExpr(AtomicCompareExchangeExpr),
}

#[derive(Debug)]
pub enum Stmt {
    VariableDef(VariableDef),
    FuncStatement(FuncStatement),
    ReturnStatement(ReturnStatement),
    BranchConditionalStatement(BranchConditionalStatement),
    BranchStatement(BranchStatement),
    SwitchStatement(SwitchStatement),
    LoopMergeStatement(LoopMergeStatement),
    SelectionMergeStatement(SelectionMergeStatement),
    Expr(Expr),
}

impl Expr {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            TokenKind::TypeBoolExpr
            | TokenKind::TypeIntExpr
            | TokenKind::TypeVectorExpr
            | TokenKind::TypeArrayExpr
            | TokenKind::TypeRuntimeArrayExpr
            | TokenKind::TypeStructExpr
            | TokenKind::TypePointerExpr => Some(Self::TypeExpr(TypeExpr(node))),
            TokenKind::VariableExpr => Some(Self::VariableExpr(VariableExpr(node))),
            TokenKind::OpAccessChain => Some(Self::VariableRef(VariableRef(node))),
            TokenKind::OpLabel => Some(Self::LabelExpr(LabelExpr(node))),
            TokenKind::OpLoad => Some(Self::LoadExpr(LoadExpr(node))),
            TokenKind::OpStore => Some(Self::StoreExpr(StoreExpr(node))),
            TokenKind::OpConstant => Some(Self::ConstExpr(ConstExpr(node))),
            TokenKind::OpIEqual => Some(Self::EqualExpr(EqualExpr(node))),
            TokenKind::OpINotEqual => Some(Self::NotEqualExpr(NotEqualExpr(node))),
            TokenKind::OpSGreaterThan => Some(Self::GreaterThanExpr(GreaterThanExpr(node))),
            TokenKind::OpSGreaterThanEqual => {
                Some(Self::GreaterThanEqualExpr(GreaterThanEqualExpr(node)))
            }
            TokenKind::OpSLessThan => Some(Self::LessThanExpr(LessThanExpr(node))),
            TokenKind::OpSLessThanEqual => Some(Self::LessThanEqualExpr(LessThanEqualExpr(node))),
            TokenKind::OpAtomicExchange => Some(Self::AtomicExchangeExpr(AtomicExchangeExpr(node))),
            TokenKind::OpAtomicCompareExchange => Some(Self::AtomicCompareExchangeExpr(
                AtomicCompareExchangeExpr(node),
            )),
            _ => None,
        }
    }
}

impl Stmt {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            TokenKind::VariableDef => Some(Self::VariableDef(VariableDef(node))),
            TokenKind::OpReturn | TokenKind::OpKill => {
                Some(Self::ReturnStatement(ReturnStatement(node)))
            }
            TokenKind::BranchConditionalStatement => Some(Self::BranchConditionalStatement(
                BranchConditionalStatement(node),
            )),
            TokenKind::BranchStatement => Some(Self::BranchStatement(BranchStatement(node))),
            //TokenKind::SwitchStatement => Some(Self::SwitchStatement(SwitchStatement(node))),
            TokenKind::LoopMergeStatement => {
                Some(Self::LoopMergeStatement(LoopMergeStatement(node)))
            }
            TokenKind::SelectionMergeStatement => {
                Some(Self::SelectionMergeStatement(SelectionMergeStatement(node)))
            }
            _ => Some(Self::Expr(Expr::cast(node)?)),
        }
    }
}

impl Root {
    pub(crate) fn stmts(&self) -> impl Iterator<Item = Stmt> {
        self.0.children().filter_map(Stmt::cast)
    }
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == TokenKind::Root {
            Some(Self(node))
        } else {
            None
        }
    }
}

impl TypeExpr {
    pub fn ty(&self) -> SpirvType {
        let tokens: Vec<SyntaxToken> = self
            .0
            .children_with_tokens()
            .filter_map(|child| {
                let token = child.into_token()?;
                // Filter out whitespace tokens and percent
                if token.kind() != TokenKind::Whitespace && token.kind() != TokenKind::Percent {
                    Some(token)
                } else {
                    None
                }
            })
            // fixme: make this more robust
            .take(10)
            .collect();

        match &tokens[0].kind() {
            TokenKind::OpTypeBool => SpirvType::Bool,
            TokenKind::OpTypeInt => {
                let width = &tokens[1];
                let signed = &tokens[2];
                match (&width.kind(), &signed.kind()) {
                    (TokenKind::Int, TokenKind::Int) => {
                        return SpirvType::Int {
                            width: width.text().parse().unwrap(),
                            signed: match signed.text().parse().unwrap() {
                                0 => false,
                                1 => true,
                                _ => panic!("Invalid signed value {:#?}", signed),
                            },
                        };
                    }
                    _ => panic!("Invalid width {:#?}, and signed value {:#?}", width, signed),
                }
            }
            TokenKind::OpTypeVector => {
                // fixme: error handling
                let inner_ty_symbol = &tokens[1];
                let count = &tokens[2];
                println!("{:#?}", tokens);
                println!("inner_ty_symbol: {:#?}", inner_ty_symbol);
                println!("count: {:#?}", count);
                SpirvType::Vector { element: inner_ty_symbol.text().to_string(), count: count.text().parse().unwrap() }
            },
            TokenKind::OpTypeArray => todo!(),
            TokenKind::OpTypeRuntimeArray => todo!(),
            TokenKind::OpTypeStruct => todo!(),
            TokenKind::OpTypePointer => {
                println!("{:#?}", tokens);
                let storage_class = &tokens[1];
                let pointee = &tokens[2];
                SpirvType::Pointer {
                    pointee: pointee.text().to_string(),
                    storage_class: match storage_class.text().to_string().as_str() {
                        "Uniform" | "Input" | "Output" => StorageClass::Global,
                        "Workgroup" => StorageClass::Shared,
                        "Function" => StorageClass::Local,
                        _ => panic!("Invalid storage class {:#?}", storage_class),
                    },
                }
            },
            _ => panic!("Invalid type {}", self.0.first_token().unwrap().text()),
        }
    }
    pub fn name(&self) -> String {
        self.0.first_token().unwrap().text().to_string()
    }
}

impl VariableExpr {
    pub(crate) fn ty_name(&self) -> String {
        self.0
            .children_with_tokens()
            .filter_map(|child| {
                let token = child.into_token()?;
                // Filter out whitespace and percent tokens
                if token.kind() != TokenKind::Whitespace && token.kind() != TokenKind::Percent {
                    Some(token)
                } else {
                    None
                }
            })
            .take(2)
            .nth(1)
            .unwrap()
            .text()
            .to_string()
    }

    pub(crate) fn storage_class(&self) -> StorageClass {
        let scope_str = self
            .0
            .children_with_tokens()
            .filter_map(|child| {
                let token = child.into_token()?;
                // Filter out whitespace and percent tokens
                if token.kind() != TokenKind::Whitespace && token.kind() != TokenKind::Percent {
                    Some(token)
                } else {
                    None
                }
            })
            .take(3)
            .nth(2)
            .unwrap()
            .text()
            .to_string();
        // fixme: error handling
        match scope_str.as_str() {
            "Uniform" | "Input" | "Output" => StorageClass::Global,
            "Workgroup" => StorageClass::Shared,
            "Function" => StorageClass::Local,
            _ => panic!("Invalid storage class {}", scope_str),
        }
    }

    // pub(crate) fn scope(&self) -> InstructionScope {
    //     let scope_str = self
    //         .0
    //         .children_with_tokens()
    //         .filter_map(|child| {
    //             let token = child.into_token()?;
    //             // Filter out whitespace and percent tokens
    //             if token.kind() != TokenKind::Whitespace && token.kind() != TokenKind::Percent {
    //                 Some(token)
    //             } else {
    //                 None
    //             }
    //         })
    //         .take(3)
    //         .nth(2)
    //         .unwrap()
    //         .text()
    //         .to_string();
    // }
}
impl VariableRef {
    pub(crate) fn name(&self) -> String {
        self.0.first_token().unwrap().text().to_string()
    }
}

impl VariableDef {
    // we want
    pub(crate) fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn value(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl LabelExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl LoadExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl StoreExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl ConstExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl EqualExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl NotEqualExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl LessThanExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl GreaterThanExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl LessThanEqualExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl GreaterThanEqualExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl AtomicExchangeExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl AtomicCompareExchangeExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl ReturnStatement {
    // todo: implement
}

impl BranchConditionalStatement {
    // todo: implement
}

impl BranchStatement {
    // todo: implement
}

impl SwitchStatement {
    // todo: implement
}

impl LoopMergeStatement {
    // todo: implement
}

impl SelectionMergeStatement {
    // todo: implement
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
