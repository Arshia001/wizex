//! Type translator functions from `wasmparser` to `wasm_encoder`.

pub(crate) fn const_expr(expr: wasmparser::ConstExpr) -> wasm_encoder::ConstExpr {
    match expr.get_operators_reader().read().unwrap() {
        wasmparser::Operator::F32Const { value } => {
            wasm_encoder::ConstExpr::f32_const(f32::from_bits(value.bits()))
        }
        wasmparser::Operator::F64Const { value } => {
            wasm_encoder::ConstExpr::f64_const(f64::from_bits(value.bits()))
        }
        wasmparser::Operator::I32Const { value } => wasm_encoder::ConstExpr::i32_const(value),
        wasmparser::Operator::I64Const { value } => wasm_encoder::ConstExpr::i64_const(value),
        wasmparser::Operator::V128Const { value } => {
            wasm_encoder::ConstExpr::v128_const(value.i128())
        }

        _ => panic!("not supported"),
    }
}

pub(crate) fn val_type(ty: wasmparser::ValType) -> wasm_encoder::ValType {
    use wasm_encoder::ValType;
    use wasmparser::ValType::*;
    match ty {
        I32 => ValType::I32,
        I64 => ValType::I64,
        F32 => ValType::F32,
        F64 => ValType::F64,
        V128 => ValType::V128,
        wasmparser::ValType::FUNCREF => ValType::FUNCREF,
        Ref(_) => panic!("not supported"),
    }
}

pub(crate) fn global_type(ty: wasmparser::GlobalType) -> wasm_encoder::GlobalType {
    wasm_encoder::GlobalType {
        val_type: val_type(ty.content_type),
        mutable: ty.mutable,
    }
}

pub(crate) fn memory_type(ty: wasmparser::MemoryType) -> wasm_encoder::MemoryType {
    wasm_encoder::MemoryType {
        minimum: ty.initial,
        maximum: ty.maximum,
        memory64: ty.memory64,
        shared: ty.shared,
    }
}

pub(crate) fn export(kind: wasmparser::ExternalKind) -> wasm_encoder::ExportKind {
    match kind {
        wasmparser::ExternalKind::Func => wasm_encoder::ExportKind::Func,
        wasmparser::ExternalKind::Global => wasm_encoder::ExportKind::Global,
        wasmparser::ExternalKind::Table => wasm_encoder::ExportKind::Table,
        wasmparser::ExternalKind::Memory => wasm_encoder::ExportKind::Memory,
        wasmparser::ExternalKind::Tag => unreachable!(),
    }
}
