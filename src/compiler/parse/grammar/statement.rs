use super::expr::expr;
use crate::compiler::parse::{
    marker::CompletedMarker,
    parser::{Parse, Parser},
    syntax::TokenKind,
};

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Percent) {
        Some(variable_def(p))
    } else if p.at(TokenKind::If) {
        Some(if_statement(p))
    } else if p.at(TokenKind::LeftBrace) {
        Some(block_statement(p))
    } else if p.at(TokenKind::Function) {
        Some(func_statement(p))
    } else if p.at(TokenKind::OpLabel){
        Some(op_label_expr(p))
    } else if p.at(TokenKind::OpConstant){
        Some(op_constant_expr(p))
    } else if p.at(TokenKind::OpReturn){
        Some(op_return_statement(p))
    } else if p.at(TokenKind::OpLoad){
        Some(op_load_expr(p))
    } else if p.at(TokenKind::OpStore){
        Some(op_store_expr(p))
    } else if p.at(TokenKind::OpIEqual){
        Some(op_equal_expr(p))
    } else if p.at(TokenKind::OpINotEqual){
        Some(op_not_equal_expr(p))
    } else if p.at(TokenKind::OpSGreaterThan){
        Some(op_greater_than_expr(p))
    } else if p.at(TokenKind::OpSGreaterThanEqual){
        Some(op_greater_than_equal_expr(p))
    } else if p.at(TokenKind::OpSLessThan){
        Some(op_less_than_expr(p))
    } else if p.at(TokenKind::OpSLessThanEqual){
        Some(op_less_than_equal_expr(p))
    } else if p.at(TokenKind::OpBranch){
        Some(op_branch_statement(p))
    } else if p.at(TokenKind::OpBranchConditional){
        Some(op_branch_conditional_statement(p))
    } else if p.at(TokenKind::OpLoopMerge){
        Some(op_loop_merge_statement(p))
    } else if p.at(TokenKind::OpSelectionMerge){
        Some(op_selection_merge_statement(p))
    } 
    // else if p.at(TokenKind::OpSwitch){
    //     Some(op_switch_statement(p))
    // } 
    else if p.at(TokenKind::OpAtomicExchange){
        Some(op_atomic_exchange_expr(p))
    } else if p.at(TokenKind::OpAtomicCompareExchange){
        Some(op_atomic_compare_exchange_expr(p))
    } else {
        // todo: add error handling
        expr(p)
    }
}

/// example: OpLabel
fn op_label_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpLabel token
    p.bump();
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LabelExpr)
}


/// example: OpConstant %uint 0
fn op_constant_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpConstant token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::ConstantExpr)
}

/// example: OpReturn
fn op_return_statement(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpReturn token
    p.bump();
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::ReturnStatement)
}

/// example: OpLoad %float %arrayidx Aligned 4
fn op_load_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpLoad token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Aligned);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LoadExpr)
    
}

/// example: OpStore %arrayidx2 %add Aligned 4
fn op_store_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpStore token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Aligned);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::StoreExpr)
}

/// example: %cmp = OpIEqual %bool %call %num_elements
fn op_equal_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpIEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::EqualExpr)
}

/// example: OpINotEqual %bool %call %num_elements
fn op_not_equal_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpINotEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::NotEqualExpr)
}

/// example: OpSGreaterThan %bool %call %num_elements
fn op_greater_than_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpSGreaterThan token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::GreaterThanExpr)
}

/// example: OpSGreaterThanEqual %bool %call %num_elements 
fn op_greater_than_equal_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpSGreaterThanEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::GreaterEqualThanExpr)
}

/// example: OpSLessThan %bool %call %num_elements
fn op_less_than_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpSLessThan token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LessThanExpr)
}
 
/// example: OpSLessThanEqual %bool %call %num_elements
fn op_less_than_equal_expr(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpSLessThanEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Type);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LessThanEqualExpr)
}

/// example: OpBranchConditional %cmp_not %if_end %if_then
fn op_branch_conditional_statement(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpBranchConditional token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::BranchConditionalStatement)
}

/// example: OpBranch %if_end
fn op_branch_statement(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpBranch token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::BranchStatement)
}


fn op_switch_statement(p: &mut Parser) -> CompletedMarker{
    todo!()
}

/// example: OpLoopMerge %51 %52 None
fn op_loop_merge_statement(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpLoopMerge token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LoopMergeStatement)
}

/// example: OpSelectionMerge %29 None
fn op_selection_merge_statement(p: &mut Parser) -> CompletedMarker{
    let m = p.start();
    // skip OpSelectionMerge token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::SelectionMergeStatement)
}

/// example: OpAtomicCompareExchange %uint %ptr %value %value %scope %scope
fn op_atomic_exchange_expr(p: &mut Parser) -> CompletedMarker{
    todo!()
}

/// example: OpAtomicCompareExchange %uint %ptr %value %value %value %scope %scope
fn op_atomic_compare_exchange_expr(p: &mut Parser) -> CompletedMarker{
    todo!()
}


fn variable_def(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip Percent token
    p.bump();

    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Equal);

    stmt(p);
    p.expect(TokenKind::Newline);

    m.complete(p, TokenKind::VariableDef)
}

fn if_statement(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // consume if keyword
    p.bump();
    let condition = expr(p);
    p.expect(TokenKind::LeftBrace);

    let then_branch = stmt(p);
    p.expect(TokenKind::RightBrace);
    if p.at(TokenKind::Else) {
        p.expect(TokenKind::LeftBrace);

        let else_branch = stmt(p);
        p.expect(TokenKind::RightBrace);
    }
    m.complete(p, TokenKind::IfStatement)
}

// block statement contain a list of statements within a block
fn block_statement(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // consume left brace
    p.bump();
    // stop until reach right brace or EOF
    while !p.at(TokenKind::RightBrace) && !p.at_end() {
        if stmt(p).is_none() {
            break;
        }
    }
    p.expect(TokenKind::RightBrace);
    p.expect(TokenKind::SemiColon);
    m.complete(p, TokenKind::BlockStatement)
}

fn func_statement(p: &mut Parser) -> CompletedMarker {
    todo!()
}
