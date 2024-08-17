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

    #[test]
    fn parse_binding_usage() {
        check(
            "counter",
            expect![[r#"
            Root@0..7
              VariableRef@0..7
                Ident@0..7 "counter""#]],
        );
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
            Root@0..3
              InfixExpr@0..3
                Literal@0..1
                  Int@0..1 "1"
                Plus@1..2 "+"
                Literal@2..3
                  Int@2..3 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
            Root@0..7
              InfixExpr@0..7
                InfixExpr@0..5
                  InfixExpr@0..3
                    Literal@0..1
                      Int@0..1 "1"
                    Plus@1..2 "+"
                    Literal@2..3
                      Int@2..3 "2"
                  Plus@3..4 "+"
                  Literal@4..5
                    Int@4..5 "3"
                Plus@5..6 "+"
                Literal@6..7
                  Int@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
            Root@0..7
              InfixExpr@0..7
                InfixExpr@0..5
                  Literal@0..1
                    Int@0..1 "1"
                  Plus@1..2 "+"
                  InfixExpr@2..5
                    Literal@2..3
                      Int@2..3 "2"
                    Star@3..4 "*"
                    Literal@4..5
                      Int@4..5 "3"
                Minus@5..6 "-"
                Literal@6..7
                  Int@6..7 "4""#]],
        );
    }

    //#[test]
    fn parse_query_expr() {
        check(
            "
        a?",
            expect![
                r##"
Root@0..1
  PostfixExpr@0..1
    Ident  "##
            ],
        )
    }

    #[test]
    fn parse_indexing_expr() {
        check(
            "
        a[1]",
            expect![
                r##"
Root@0..13
  Whitespace@0..9 "\n        "
  PostfixExpr@9..13
    VariableRef@9..10
      Ident@9..10 "a"
    LeftSquare@10..11 "["
    Literal@11..12
      Int@11..12 "1"
    RightSquare@12..13 "]""##
            ],
        )
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check(
            "
-- addition
1 + 1 + 10",
            expect![[r##"
            Root@0..23
              Whitespace@0..1 "\n"
              Comment@1..12 "-- addition"
              Whitespace@12..13 "\n"
              InfixExpr@13..23
                InfixExpr@13..19
                  Literal@13..15
                    Int@13..14 "1"
                    Whitespace@14..15 " "
                  Plus@15..16 "+"
                  Whitespace@16..17 " "
                  Literal@17..19
                    Int@17..18 "1"
                    Whitespace@18..19 " "
                Plus@19..20 "+"
                Whitespace@20..21 " "
                Literal@21..23
                  Int@21..23 "10""##]],
        );
    }
    #[test]
    fn recover_on_let_token() {
        check(
            "let a = 1+;",
            expect![[r#"
Root@0..11
  VariableDef@0..11
    Let@0..3 "let"
    Whitespace@3..4 " "
    Ident@4..5 "a"
    Whitespace@5..6 " "
    Equal@6..7 "="
    Whitespace@7..8 " "
    InfixExpr@8..10
      Literal@8..9
        Int@8..9 "1"
      Plus@9..10 "+"
    SemiColon@10..11 ";"
error at 10..11: expected identifier, int, float, string, ‘-’ or ‘(’, but found ‘;‘"#]],
        );
    }

    #[test]
    fn parse_multiple_statements() {
        check(
            "let a = 1;",
            expect![[r#"
            Root@0..10
              VariableDef@0..10
                Let@0..3 "let"
                Whitespace@3..4 " "
                Ident@4..5 "a"
                Whitespace@5..6 " "
                Equal@6..7 "="
                Whitespace@7..8 " "
                Literal@8..9
                  Int@8..9 "1"
                SemiColon@9..10 ";""#]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check(
            "(foo",
            expect![[r#"
Root@0..4
  ParenExpr@0..4
    LeftParen@0..1 "("
    VariableRef@1..4
      Ident@1..4 "foo"
error at 1..4: expected ‘+’, ‘-’, ‘*’, ‘/’, ‘?‘, ‘[‘ or ‘)’"#]],
        );
    }

    #[test]
    fn do_not_parse_operator_if_gettting_rhs_failed() {
        check(
            "(1+",
            expect![[r#"
Root@0..3
  ParenExpr@0..3
    LeftParen@0..1 "("
    InfixExpr@1..3
      Literal@1..2
        Int@1..2 "1"
      Plus@2..3 "+"
error at 2..3: expected identifier, int, float, string, ‘-’ or ‘(’
error at 2..3: expected ‘)’"#]],
        );
    }

    #[test]
    fn basic_block() {
        check(
            "
        {
            let a = 1;
            let b = 2;
        ",
            expect![
                r#"
Root@0..65
  Whitespace@0..9 "\n        "
  BlockStatement@9..65
    LeftBrace@9..10 "{"
    Whitespace@10..23 "\n            "
    VariableDef@23..46
      Let@23..26 "let"
      Whitespace@26..27 " "
      Ident@27..28 "a"
      Whitespace@28..29 " "
      Equal@29..30 "="
      Whitespace@30..31 " "
      Literal@31..32
        Int@31..32 "1"
      SemiColon@32..33 ";"
      Whitespace@33..46 "\n            "
    VariableDef@46..65
      Let@46..49 "let"
      Whitespace@49..50 " "
      Ident@50..51 "b"
      Whitespace@51..52 " "
      Equal@52..53 "="
      Whitespace@53..54 " "
      Literal@54..55
        Int@54..55 "2"
      SemiColon@55..56 ";"
      Whitespace@56..65 "\n        "
error at 56..65: expected ‘}’ or ‘}’
error at 56..65: expected ‘;‘"#
            ],
        )
    }

    #[test]
    fn nested_block() {
        check(
            "
        {
            let a = 1;
            {
                let b = 2;
            };
        };
        ",
            expect![],
        )
    }
    #[test]
    fn if_else_statement() {
      todo!()
    }
}
