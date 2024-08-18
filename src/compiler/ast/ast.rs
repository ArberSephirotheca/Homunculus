use crate::compiler::parse::syntax::{SyntaxElement, SyntaxNode, SyntaxToken, TokenKind};

#[derive(Debug)]
pub struct Root(SyntaxNode);
#[derive(Debug)]
pub struct BinaryExpr(SyntaxNode);
#[derive(Debug)]
pub struct Literal(SyntaxNode);
#[derive(Debug)]
pub struct ParenExpr(SyntaxNode);
#[derive(Debug)]
pub struct UnaryExpr(SyntaxNode);
#[derive(Debug)]
pub struct VariableRef(SyntaxNode);
#[derive(Debug)]
pub struct VariableDef(SyntaxNode);
#[derive(Debug)]
pub struct IfStatement(SyntaxNode);
#[derive(Debug)]
pub struct BlockStatement(SyntaxNode);
#[derive(Debug)]
pub struct FuncStatement(SyntaxNode);

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
    BinaryExpr(BinaryExpr),
    Literal(Literal),
    ParenExpr(ParenExpr),
    UnaryExpr(UnaryExpr),
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
    IfStatement(IfStatement),
    BlockStatement(BlockStatement),
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
            TokenKind::InfixExpr => Some(Self::BinaryExpr(BinaryExpr(node))),
            TokenKind::Literal => Some(Self::Literal(Literal(node))),
            TokenKind::ParenExpr => Some(Self::ParenExpr(ParenExpr(node))),
            TokenKind::VariableRef => Some(Self::VariableRef(VariableRef(node))),
            TokenKind::OpLabel => Some(Self::LabelExpr(LabelExpr(node))),
            TokenKind::OpLoad => Some(Self::LoadExpr(LoadExpr(node))),
            TokenKind::OpStore => Some(Self::StoreExpr(StoreExpr(node))),
            TokenKind::OpConstant => Some(Self::ConstExpr(ConstExpr(node))),
            TokenKind::OpIEqual => Some(Self::EqualExpr(EqualExpr(node))),
            _ => None,
        }
    }
}

impl Stmt {
    pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            TokenKind::VariableDef => Some(Self::VariableDef(VariableDef(node))),
            TokenKind::IfStatement => todo!(),
            TokenKind::BlockStatement => todo!(),
            TokenKind::FuncStatement => todo!(),
            TokenKind::OpReturn | TokenKind::OpKill => Some(Self::ReturnStatement(ReturnStatement(node))),
            TokenKind::BranchConditionalStatement => Some(Self::BranchConditionalStatement(BranchConditionalStatement(node))),
            TokenKind::BranchStatement => Some(Self::BranchStatement(BranchStatement(node))),
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

impl BinaryExpr {
    pub(crate) fn lhs(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub(crate) fn rhs(&self) -> Option<Expr> {
        self.0.children().filter_map(Expr::cast).nth(1)
    }

    pub(crate) fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|t| {
                matches!(
                    t.kind(),
                    TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash,
                )
            })
    }
}

impl UnaryExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }

    pub(crate) fn op(&self) -> Option<SyntaxToken> {
        self.0
            .children_with_tokens()
            .filter_map(|x| x.into_token())
            .find(|t| t.kind() == TokenKind::Minus)
    }
}

impl ParenExpr {
    pub(crate) fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl Literal {
    pub(crate) fn parse(&self) -> u64 {
        self.0.first_token().unwrap().text().parse().unwrap()
    }
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
