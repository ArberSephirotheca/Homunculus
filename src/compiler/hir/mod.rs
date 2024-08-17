use crate::compiler::ast::ast;
use crate::compiler::parse::syntax::TokenKind;
#[derive(Debug)]
pub enum Stmt {
    VariableDef {
        name: String,
        value: Expr,
    },
    IfStatement {},
    BlockStatement {},
    FuncStatement {},
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Literal {
        n: u64,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Self>,
    },
    VariableRef {
        var: String,
    },
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
}

impl Stmt {
    fn lower(ast: ast::Stmt) -> Option<Self> {
        match ast {
            ast::Stmt::VariableDef(ast) => Some(Self::VariableDef {
                name: ast.name()?.text().to_string(),
                value: Expr::lower(ast.value()),
            }),
            ast::Stmt::Expr(ast) => Some(Self::Expr(Expr::lower(Some(ast)))),
            ast::Stmt::IfStatement(ast) => todo!(),
            ast::Stmt::BlockStatement(ast) => todo!(),
            ast::Stmt::FuncStatement(ast) => todo!(),
        }
    }
}

impl Expr {
    fn lower(ast: Option<ast::Expr>) -> Self {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::BinaryExpr(ast) => Self::lower_binary(ast),
                ast::Expr::Literal(ast) => Self::Literal { n: ast.parse() },
                ast::Expr::UnaryExpr(ast) => Self::lower_unary(ast),
                ast::Expr::ParenExpr(ast) => Self::lower(ast.expr()),
                ast::Expr::VariableRef(ast) => Self::VariableRef { var: ast.name() },
            }
        } else {
            Self::Missing
        }
    }

    fn lower_binary(ast: ast::BinaryExpr) -> Self {
        let op = match ast.op().unwrap().kind() {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            _ => unreachable!(),
        };
        Self::Binary {
            op,
            lhs: Box::new(Expr::lower(ast.lhs())),
            rhs: Box::new(Expr::lower(ast.rhs())),
        }
    }

    fn lower_unary(ast: ast::UnaryExpr) -> Self {
        let op = match ast.op().unwrap().kind() {
            TokenKind::Minus => UnaryOp::Neg,
            _ => unreachable!(),
        };

        Self::Unary {
            op,
            expr: Box::new(Expr::lower(ast.expr())),
        }
    }
}

pub(crate) fn lower(ast: ast::Root) -> impl Iterator<Item = Stmt>{
    ast.stmts().filter_map(Stmt::lower)
}