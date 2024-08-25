use crate::compiler::parse::parser::Parser;
use crate::compiler::parse::{marker::CompletedMarker, syntax::TokenKind};
use core::panic;
use std::ops::Add;

#[derive(Debug, PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Optional,
    LeftSquare,
}

impl Op {
    fn infix_binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
            _ => panic!("bad infixOp: {:?}", self),
        }
    }
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 5),
            _ => panic!("bad prefixOp: {:?}", self),
        }
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        match self {
            Self::Optional | Self::LeftSquare => Some((7, ())),
            _ => None,
        }
    }
}

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

// Pratt parsing
fn expr_binding_power(p: &mut Parser, min_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = lhs(p)?;

    loop {
        let op = if p.at(TokenKind::Plus) {
            Op::Add
        } else if p.at(TokenKind::Minus) {
            Op::Sub
        } else if p.at(TokenKind::Star) {
            Op::Mul
        } else if p.at(TokenKind::Slash) {
            Op::Div
        } else if p.at(TokenKind::Optional) {
            Op::Optional
        } else if p.at(TokenKind::LeftSquare) {
            Op::LeftSquare
        }
        // other operation, let caller to decide what to do
        else {
            break;
        };

        // check postfix expression first
        if let Some((left_binding_power, ())) = op.postfix_binding_power() {
            if left_binding_power < min_binding_power {
                break;
            }

            // consume postfix operator token
            p.bump();
            if op == Op::LeftSquare {
                let m = lhs.precede(p);
                let rhs = expr_binding_power(p, 0).is_some();
                p.expect(TokenKind::RightSquare);
                m.complete(p, TokenKind::PostfixExpr);
                if !rhs {
                    break;
                }
                // todo: do something wih postfix such as array indexing and query
            } else if op == Op::Optional {
                todo!()
            }
            continue;
        }

        // then check infx expression
        let (left_binding_power, right_binding_power) = op.infix_binding_power();

        if left_binding_power < min_binding_power {
            break;
        }
        // consume the infix operator token
        p.bump();

        let m = lhs.precede(p);
        let parsed = expr_binding_power(p, right_binding_power).is_some();
        lhs = m.complete(p, TokenKind::InfixExpr);
        if !parsed {
            break;
        }
    }
    Some(lhs)
}

// lhs is responsible for handling variable reference, literal, pefix expression, parenthesis expression
fn lhs(p: &mut Parser) -> Option<CompletedMarker> {
    let lhs = if p.at(TokenKind::Ident) {
        variable_ref(p)
    } else if p.at(TokenKind::Int) || p.at(TokenKind::Float) || p.at(TokenKind::String) {
        literal(p)
    } else if p.at(TokenKind::Minus) {
        prefix_expr(p)
    } else if p.at(TokenKind::LeftParen) {
        paren_expr(p)
    } else {
        p.error();
        return None;
    };

    Some(lhs)
}

fn rhs(p: &mut Parser) -> Option<CompletedMarker> {
    todo!()
}

// handle query execution
fn postfix_expr(p: &mut Parser) -> Option<CompletedMarker> {
    todo!()
}

fn variable_ref(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump();
    m.complete(p, TokenKind::VariableRef)
}

fn literal(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump();

    m.complete(p, TokenKind::Literal)
}

fn binary_expr(p: &mut Parser) -> CompletedMarker {
    todo!()
}

fn unary_expr(p: &mut Parser) -> CompletedMarker {
    todo!()
}

fn prefix_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    let op = Op::Neg;
    let (_, right_binding_power) = op.prefix_binding_power();
    // consume prefix operator
    p.bump();

    expr_binding_power(p, right_binding_power);
    m.complete(p, TokenKind::PrefixExpr)
}

fn infix_expr(p: &mut Parser) -> CompletedMarker {
    todo!()
}
fn paren_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump();
    expr_binding_power(p, 0);
    p.expect(TokenKind::RightParen);

    m.complete(p, TokenKind::ParenExpr)
}

mod test {
    use super::*;
    use crate::compiler::parse::parser::parse;
    use expect_test::{expect, Expect};
    fn check(input: &str, expected_tree: expect_test::Expect) {
        let parse = parse(input);
        expected_tree.assert_eq(&parse.debug_tree());
    }
    #[test]
    fn test_parse_nothing() {
        check("", expect![[r#"Root@0..0"#]])
    }

    // #[test]
    // fn parse_binding_usage() {
    //     check(
    //         "counter",
    //         expect![[r#"
    //         Root@0..7
    //           VariableRef@0..7
    //             Ident@0..7 "counter""#]],
    //     );
    // }
}
