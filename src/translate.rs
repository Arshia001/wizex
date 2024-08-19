//! Type translator functions from `wasmparser` to `wasm_encoder`.

use wasm_encoder::{GlobalType, MemoryType, TableType, TagType};

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

pub(crate) fn ref_type(ty: wasmparser::RefType) -> wasm_encoder::RefType {
    wasm_encoder::RefType {
        nullable: ty.is_nullable(),
        heap_type: match ty.heap_type() {
            wasmparser::HeapType::Any => wasm_encoder::HeapType::Any,
            wasmparser::HeapType::Array => wasm_encoder::HeapType::Array,
            wasmparser::HeapType::Concrete(concrete) => {
                wasm_encoder::HeapType::Concrete(match concrete {
                    wasmparser::UnpackedIndex::Module(i) => i,
                    _ => panic!("not supported"),
                })
            }
            wasmparser::HeapType::Eq => wasm_encoder::HeapType::Eq,
            wasmparser::HeapType::Exn => wasm_encoder::HeapType::Exn,
            wasmparser::HeapType::Extern => wasm_encoder::HeapType::Extern,
            wasmparser::HeapType::Func => wasm_encoder::HeapType::Func,
            wasmparser::HeapType::I31 => wasm_encoder::HeapType::I31,
            wasmparser::HeapType::NoExtern => wasm_encoder::HeapType::NoExtern,
            wasmparser::HeapType::NoFunc => wasm_encoder::HeapType::NoFunc,
            wasmparser::HeapType::None => wasm_encoder::HeapType::None,
            wasmparser::HeapType::Struct => wasm_encoder::HeapType::Struct,
        },
    }
}

pub(crate) fn tag_kind(kind: wasmparser::TagKind) -> wasm_encoder::TagKind {
    match kind {
        wasmparser::TagKind::Exception => wasm_encoder::TagKind::Exception,
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

pub(crate) fn import(ty: wasmparser::TypeRef) -> wasm_encoder::EntityType {
    match ty {
        wasmparser::TypeRef::Func(func) => wasm_encoder::EntityType::Function(func),
        wasmparser::TypeRef::Global(global) => wasm_encoder::EntityType::Global(GlobalType {
            val_type: val_type(global.content_type),
            mutable: global.mutable,
        }),
        wasmparser::TypeRef::Memory(memory) => wasm_encoder::EntityType::Memory(MemoryType {
            minimum: memory.initial,
            maximum: memory.maximum,
            memory64: memory.memory64,
            shared: memory.shared,
        }),
        wasmparser::TypeRef::Table(table) => wasm_encoder::EntityType::Table(TableType {
            element_type: ref_type(table.element_type),
            minimum: table.initial,
            maximum: table.maximum,
        }),
        wasmparser::TypeRef::Tag(tag) => wasm_encoder::EntityType::Tag(TagType {
            kind: tag_kind(tag.kind),
            func_type_idx: tag.func_type_idx,
        }),
    }
}
