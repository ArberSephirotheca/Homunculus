use rowan::TokenAtOffset;
use smallvec::smallvec;

use crate::compiler::parse::symbol_table::{SpirvType, StorageClass};
use crate::compiler::parse::syntax::{SyntaxElement, SyntaxNode, SyntaxToken, TokenKind};

#[derive(Debug)]
pub struct Root(SyntaxNode);
#[derive(Debug)]
pub struct DecorateStatement(SyntaxNode);
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
pub struct StoreStatement(SyntaxNode);
#[derive(Debug)]
pub struct ConstExpr(SyntaxNode);
#[derive(Debug)]
pub struct ConstTrueExpr(SyntaxNode);
#[derive(Debug)]
pub struct ConstFalseExpr(SyntaxNode);
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
    ConstExpr(ConstExpr),
    ConstTrueExpr(ConstTrueExpr),
    ConstFalseExpr(ConstFalseExpr),
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
    DecorateStatement(DecorateStatement),
    VariableDef(VariableDef),
    StoreStatement(StoreStatement),
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
            TokenKind::AccessChainExpr => Some(Self::VariableRef(VariableRef(node))),
            TokenKind::LabelExpr => Some(Self::LabelExpr(LabelExpr(node))),
            TokenKind::LoadExpr => Some(Self::LoadExpr(LoadExpr(node))),
            TokenKind::ConstantExpr => Some(Self::ConstExpr(ConstExpr(node))),
            // TokenKind::ConstantCompositeExpr => Some(Self::ConstExpr(ConstExpr(node))),
            TokenKind::ConstantTrueExpr => Some(Self::ConstTrueExpr(ConstTrueExpr(node))),
            TokenKind::ConstantFalseExpr => Some(Self::ConstFalseExpr(ConstFalseExpr(node))),
            TokenKind::EqualExpr => Some(Self::EqualExpr(EqualExpr(node))),
            TokenKind::NotEqualExpr => Some(Self::NotEqualExpr(NotEqualExpr(node))),
            TokenKind::GreaterThanExpr => Some(Self::GreaterThanExpr(GreaterThanExpr(node))),
            TokenKind::GreaterEqualThanExpr => {
                Some(Self::GreaterThanEqualExpr(GreaterThanEqualExpr(node)))
            }
            TokenKind::LessThanExpr => Some(Self::LessThanExpr(LessThanExpr(node))),
            TokenKind::LessThanEqualExpr => Some(Self::LessThanEqualExpr(LessThanEqualExpr(node))),
            TokenKind::AtomicExchangeExpr => {
                Some(Self::AtomicExchangeExpr(AtomicExchangeExpr(node)))
            }
            TokenKind::AtomicCompareExchangeExpr => Some(Self::AtomicCompareExchangeExpr(
                AtomicCompareExchangeExpr(node),
            )),
            _ => None,
        }
    }
}

impl Stmt {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            TokenKind::DecorateStatement => Some(Self::DecorateStatement(DecorateStatement(node))),
            TokenKind::VariableDef => Some(Self::VariableDef(VariableDef(node))),
            TokenKind::OpReturn | TokenKind::OpKill => {
                Some(Self::ReturnStatement(ReturnStatement(node)))
            }
            TokenKind::StoreStatement => Some(Self::StoreStatement(StoreStatement(node))),

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
                if token.kind() != TokenKind::Whitespace {
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
                SpirvType::Vector {
                    element: inner_ty_symbol.text().to_string(),
                    count: count.text().parse().unwrap(),
                }
            }
            TokenKind::OpTypeArray => todo!(),
            TokenKind::OpTypeRuntimeArray => todo!(),
            TokenKind::OpTypeStruct => todo!(),
            TokenKind::OpTypePointer => {
                println!("{:#?}", tokens);
                let storage_class = &tokens[1];
                let ty = &tokens[2];
                SpirvType::Pointer {
                    ty: ty.text().to_string(),
                    storage_class: match storage_class.kind() {
                        TokenKind::Global => StorageClass::Global,
                        TokenKind::Shared => StorageClass::Shared,
                        TokenKind::Local => StorageClass::Local,
                        _ => panic!("Invalid storage class {:#?}", storage_class),
                    },
                }
            }
            // TokenKind::AccessChainExpr => {
            //     let ty = &tokens[1];
            //     let base = &tokens[2];
            //     let index = &tokens[3];
            //     SpirvType::AccessChain {
            //         ty: ty.text().to_string(),
            //         base: base.text().to_string(),
            //         index: index.text().to_string(),
            //     }
            // }
            _ => panic!("Invalid type {}", self.0.first_token().unwrap().text()),
        }
    }
    pub fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::OpTypeBool || x.kind() == TokenKind::OpTypeInt)
    }
}

impl VariableExpr {
    pub(crate) fn ty_name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|child| child.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn storage_class(&self) -> Option<StorageClass> {
        let token = self
            .0
            .children_with_tokens()
            .filter_map(|child| child.into_token())
            .find(|x| {
                x.kind() == TokenKind::Global
                    || x.kind() == TokenKind::Shared
                    || x.kind() == TokenKind::Local
            });
        match token {
            Some(token) => match token.kind() {
                TokenKind::Global => Some(StorageClass::Global),
                TokenKind::Shared => Some(StorageClass::Shared),
                TokenKind::Local => Some(StorageClass::Local),
                _ => None,
            },
            None => None,
        }
    }
}
impl VariableRef {
    pub(crate) fn ty(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn base_var_name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .filter(|x| x.kind() == TokenKind::Ident)
            .nth(1)
    }

    pub(crate) fn index_name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .filter(|x| x.kind() == TokenKind::Ident)
            .nth(2)
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

impl LoadExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub(crate) fn ty(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn pointer(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .filter(|token| token.kind() == TokenKind::Ident)
            .nth(1)
    }
}

impl StoreStatement {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
    pub(crate) fn pointer(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn object(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .filter(|token| token.kind() == TokenKind::Ident)
            .nth(1)
    }
}

impl ConstExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub(crate) fn ty(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn value(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Int)
    }
}

impl ConstFalseExpr {}
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

impl DecorateStatement {
    pub(crate) fn name(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn built_in_var(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| {
                x.kind() == TokenKind::NumWorkgroups
                    || x.kind() == TokenKind::WorkgroupSize
                    || x.kind() == TokenKind::WorkgroupId
                    || x.kind() == TokenKind::LocalInvocationId
                    || x.kind() == TokenKind::GlobalInvocationId
                    || x.kind() == TokenKind::SubgroupSize
                    || x.kind() == TokenKind::NumSubgroups
                    || x.kind() == TokenKind::SubgroupId
                    || x.kind() == TokenKind::SubgroupLocalInvocationId
            })
    }
}

impl ReturnStatement {
    // todo: implement
}

impl BranchConditionalStatement {
    pub(crate) fn condition(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }

    pub(crate) fn true_label(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .filter(|x| x.kind() == TokenKind::Ident)
            .nth(1)
    }

    pub(crate) fn false_label(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .filter(|x| x.kind() == TokenKind::Ident)
            .nth(2)
    }
}

impl BranchStatement {
    pub(crate) fn label(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|x| x.kind() == TokenKind::Ident)
    }
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
        assert_eq!(type_expr.name().unwrap().text(), "OpTypeInt");
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
        let expected_name = "%_ptr_Uniform_Output";
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
        assert_eq!(variable_expr.ty_name().unwrap().text(), expected_name);
        assert_eq!(variable_expr.storage_class().unwrap(), StorageClass::Global);
    }
}
