use crate::compiler::parse::{
    marker::CompletedMarker,
    parser::{Parse, Parser},
    symbol_table::*,
    syntax::TokenKind,
};

pub(super) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Percent) {
        Some(variable_def(p))
    } else if p.at(TokenKind::OpFunction) {
        Some(op_function_expr(p))
    } else if p.at(TokenKind::OpFunctionEnd) {
        Some(op_function_end_statement(p))
    } else if p.at(TokenKind::OpTypeInt) {
        Some(op_type_int_expr(p))
    } else if p.at(TokenKind::OpTypeBool) {
        Some(op_type_bool_expr(p))
    } else if p.at(TokenKind::OpTypeVector) {
        Some(op_type_vector_expr(p))
    } else if p.at(TokenKind::OpTypeArray) {
        Some(op_type_array_expr(p))
    } else if p.at(TokenKind::OpTypeRuntimeArray) {
        Some(op_type_runtime_array_expr(p))
    } else if p.at(TokenKind::OpTypeStruct) {
        Some(op_type_struct_expr(p))
    } else if p.at(TokenKind::OpTypePointer) {
        Some(op_type_pointer_expr(p))
    } else if p.at(TokenKind::OpVariable) {
        Some(op_variable_expr(p))
    } else if p.at(TokenKind::OpAccessChain) {
        Some(op_access_chain_expr(p))
    } else if p.at(TokenKind::OpLabel) {
        Some(op_label_expr(p))
    } else if p.at(TokenKind::OpConstant) {
        Some(op_constant_expr(p))
    } else if p.at(TokenKind::OpConstantComposite) {
        Some(op_constant_composite_expr(p))
    } else if p.at(TokenKind::OpReturn) {
        Some(op_return_statement(p))
    } else if p.at(TokenKind::OpLoad) {
        Some(op_load_expr(p))
    } else if p.at(TokenKind::OpStore) {
        Some(op_store_expr(p))
    } else if p.at(TokenKind::OpIEqual) {
        Some(op_equal_expr(p))
    } else if p.at(TokenKind::OpINotEqual) {
        Some(op_not_equal_expr(p))
    } else if p.at(TokenKind::OpSGreaterThan) {
        Some(op_greater_than_expr(p))
    } else if p.at(TokenKind::OpSGreaterThanEqual) {
        Some(op_greater_than_equal_expr(p))
    } else if p.at(TokenKind::OpSLessThan) {
        Some(op_less_than_expr(p))
    } else if p.at(TokenKind::OpSLessThanEqual) {
        Some(op_less_than_equal_expr(p))
    } else if p.at(TokenKind::OpAtomicExchange) {
        Some(op_atomic_exchange_expr(p))
    } else if p.at(TokenKind::OpAtomicCompareExchange) {
        Some(op_atomic_compare_exchange_expr(p))
    } else if p.at(TokenKind::OpBranch) {
        Some(op_branch_statement(p))
    } else if p.at(TokenKind::OpBranchConditional) {
        Some(op_branch_conditional_statement(p))
    } else if p.at(TokenKind::OpLoopMerge) {
        Some(op_loop_merge_statement(p))
    } else if p.at(TokenKind::OpSelectionMerge) {
        Some(op_selection_merge_statement(p))
    }
    // else if p.at(TokenKind::OpSwitch){
    //     Some(op_switch_statement(p))
    // }
    else if p.at(TokenKind::OpAtomicExchange) {
        Some(op_atomic_exchange_expr(p))
    } else if p.at(TokenKind::OpAtomicCompareExchange) {
        Some(op_atomic_compare_exchange_expr(p))
    } else {
        // todo: add more info
        None
    }
}

/// example: OpFunction %void None %1
fn op_function_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpFunction token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::FunctionExpr)
}

/// example: OpFunctionEnd
fn op_function_end_statement(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpFunctionEnd token
    p.bump();
    m.complete(p, TokenKind::FunctionEndStatement)
}

/// example OpTypeInt 32 0
fn op_type_int_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypeInt token
    p.bump();
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypeIntExpr)
}

/// example OpTypeBool
fn op_type_bool_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypeBool token
    p.bump();
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypeBoolExpr)
}

/// example OpTypeVector %v3uint %uint 3
fn op_type_vector_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypeVector token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypeVectorExpr)
}

/// example OpTypeArray %arr_uint %uint 256
fn op_type_array_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypeArray token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypeArrayExpr)
}

/// example OpTypeRuntimeArray %arr_uint %uint
fn op_type_runtime_array_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypeRuntimeArray token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypeRuntimeArrayExpr)
}

/// example OpTypeStruct %sruntimearr_uint_0
/// fixme: currently we only support one member, implement multiple members
fn op_type_struct_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypeStruct token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypeStructExpr)
}

/// example OpTypePointer %_ptr_Function_uint Function
fn op_type_pointer_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpTypePointer token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::TypePointerExpr)
}
/// example: OpVariable %_ptr_Function_uint Function
fn op_variable_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpVariable token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    // it can be Uniform, Input, Output, Workgroup, Function
    if p.at(TokenKind::Global) || p.at(TokenKind::Shared) || p.at(TokenKind::Local) {
        p.bump();
    }
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::VariableExpr)
}

/// example: OpAccessChain %_ptr_Uniform_uint %_ %int_0
fn op_access_chain_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpAccessChain token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::AccessChainExpr)
}
/// example: OpLabel
fn op_label_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpLabel token
    p.bump();
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LabelExpr)
}

/// example: OpConstant %uint 0
fn op_constant_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpConstant token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::ConstantExpr)
}

/// example: OpConstantComposite %v3uint %uint_256 %uint_1 %uint_1
fn op_constant_composite_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpConstantComposite token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::ConstantCompositeExpr)
}

/// example: OpReturn
fn op_return_statement(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpReturn token
    p.bump();
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::ReturnStatement)
}

/// example: OpLoad %float %arrayidx Aligned 4
fn op_load_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpLoad token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Aligned);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LoadExpr)
}

/// example: OpStore %arrayidx2 %add Aligned 4
fn op_store_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpStore token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Aligned);
    p.expect(TokenKind::Int);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::StoreExpr)
}

/// example: %cmp = OpIEqual %bool %call %num_elements
fn op_equal_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpIEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::EqualExpr)
}

/// example: OpINotEqual %bool %call %num_elements
fn op_not_equal_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpINotEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::NotEqualExpr)
}

/// example: OpSGreaterThan %bool %call %num_elements
fn op_greater_than_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpSGreaterThan token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::GreaterThanExpr)
}

/// example: OpSGreaterThanEqual %bool %call %num_elements
fn op_greater_than_equal_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpSGreaterThanEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::GreaterEqualThanExpr)
}

/// example: OpSLessThan %bool %call %num_elements
fn op_less_than_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpSLessThan token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LessThanExpr)
}

/// example: OpSLessThanEqual %bool %call %num_elements
fn op_less_than_equal_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpSLessThanEqual token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::LessThanEqualExpr)
}

/// example: OpBranchConditional %cmp_not %if_end %if_then
fn op_branch_conditional_statement(p: &mut Parser) -> CompletedMarker {
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
fn op_branch_statement(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpBranch token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::BranchStatement)
}

// fixme: implement switch statement
fn op_switch_statement(p: &mut Parser) -> CompletedMarker {
    todo!()
}

/// example: OpLoopMerge %51 %52 None
fn op_loop_merge_statement(p: &mut Parser) -> CompletedMarker {
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
fn op_selection_merge_statement(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpSelectionMerge token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Newline);
    m.complete(p, TokenKind::SelectionMergeStatement)
}

/// example: OpAtomicCompareExchange %uint %result %result_ptr 1 0 $value
fn op_atomic_exchange_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpAtomicExchange token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Int);

    p.expect(TokenKind::Int);

    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    m.complete(p, TokenKind::AtomicExchangeExpr)
}

/// example: OpAtomicCompareExchange %uint %result %ptr 1 0 0 %value %comp
fn op_atomic_compare_exchange_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    // skip OpAtomicExchange token
    p.bump();
    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Int);

    p.expect(TokenKind::Int);

    p.expect(TokenKind::Percent);
    p.expect(TokenKind::Ident);

    m.complete(p, TokenKind::AtomicExchangeExpr)
}

fn variable_def(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(TokenKind::Percent);

    p.expect(TokenKind::Ident);

    p.expect(TokenKind::Equal);

    stmt(p);
    // p.expect(TokenKind::Newline);

    m.complete(p, TokenKind::VariableDef)
}
