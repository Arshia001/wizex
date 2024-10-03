//! Final rewrite pass.

use std::convert::TryInto;

use crate::{
    info::ModuleContext, snapshot::Snapshot, translate, FuncRenames, Wizex, DEFAULT_KEEP_INIT_FUNC,
};
use wasm_encoder::{ConstExpr, SectionId};

impl Wizex {
    /// Given the initialized snapshot, rewrite the Wasm so that it is already
    /// initialized.
    ///
    pub(crate) fn rewrite(
        &self,
        cx: &mut ModuleContext<'_>,
        store: &crate::Store,
        snapshot: &Snapshot,
        renames: &FuncRenames,
        has_wasix_initialize: bool,
        has_wasix_init_memory: bool,
    ) -> anyhow::Result<Vec<u8>> {
        log::debug!("Rewriting input Wasm to pre-initialized state");

        let mut encoder = wasm_encoder::Module::new();
        let module = cx.root();

        let wasix_init_memory_function_idx = if has_wasix_init_memory {
            Some(
                module
                    .imports(cx)
                    .iter()
                    .filter(|i| matches!(i.ty, wasmparser::TypeRef::Func(_)))
                    .count()
                    + module.defined_functions(cx).count(),
            )
        } else {
            None
        };
        let wasix_init_memory_function_type = if has_wasix_init_memory {
            Some(module.insert_type(
                cx,
                wasmparser::CompositeType::Func(wasmparser::FuncType::new([], [])),
            ))
        } else {
            None
        };

        // Encode the initialized data segments from the snapshot rather
        // than the original, uninitialized data segments.
        let (data_count, mut data_section) = {
            let mut data_section = wasm_encoder::DataSection::new();
            for data in module.datas(cx) {
                match data.kind {
                    wasmparser::DataKind::Passive => {
                        data_section.passive(data.data.iter().cloned());
                    }
                    wasmparser::DataKind::Active {
                        memory_index,
                        offset_expr,
                    } => {
                        if has_wasix_init_memory {
                            anyhow::bail!("Modules with WASIX concurrent memory initialization logic can't have active data segments");
                        }
                        data_section.active(
                            memory_index,
                            &translate::const_expr(offset_expr),
                            data.data.iter().cloned(),
                        );
                    }
                }
            }

            for seg in &snapshot.data_segments {
                if has_wasix_init_memory {
                    data_section.passive(seg.data(store).iter().copied());
                } else {
                    data_section.active(
                        seg.memory_index,
                        &ConstExpr::i32_const(seg.offset as i32),
                        seg.data(store).iter().copied(),
                    );
                }
            }

            (
                data_section.len(),
                if data_section.is_empty() {
                    None
                } else {
                    Some(data_section)
                },
            )
        };

        // There are multiple places were we potentially need to check whether
        // we've added the data section already and if we haven't yet, then do
        // so. For example, the original Wasm might not have a data section at
        // all, and so we have to potentially add it at the end of iterating
        // over the original sections. This closure encapsulates all that
        // add-it-if-we-haven't-already logic in one place.
        let mut add_data_section = |module: &mut wasm_encoder::Module| {
            if let Some(data_section) = data_section.take() {
                module.section(&data_section);
            }
        };

        for section in module.raw_sections(cx) {
            match section {
                // Some tools expect the name custom section to come last, even
                // though custom sections are allowed in any order. Therefore,
                // make sure we've added our data section by now.
                s if is_name_section(s) => {
                    add_data_section(&mut encoder);
                    encoder.section(s);
                }

                // For the memory section, we update the minimum size of each
                // defined memory to the snapshot's initialized size for that
                // memory.
                s if s.id == u8::from(SectionId::Memory) => {
                    let mut memories = wasm_encoder::MemorySection::new();
                    assert_eq!(module.memories_len(cx), snapshot.memory_mins.len());
                    for ((_, mem), new_min) in module.defined_memories(cx).zip(
                        snapshot
                            .memory_mins
                            .iter()
                            .skip(module.first_defined_memory_index(cx))
                            .copied(),
                    ) {
                        let mut mem = translate::memory_type(mem);
                        mem.minimum = new_min as u64;
                        memories.memory(mem);
                    }
                    encoder.section(&memories);
                }

                // Do the same for imported memories as well.
                s if s.id == u8::from(SectionId::Import) => {
                    let mut imports = wasm_encoder::ImportSection::new();

                    assert_eq!(module.memories_len(cx), snapshot.memory_mins.len());
                    let mut memory_index = 0;

                    for import in module.imports(cx) {
                        if let wasmparser::TypeRef::Memory(memory_type) = import.ty {
                            let mut mem = translate::memory_type(memory_type);
                            let new_min = snapshot.memory_mins[memory_index];
                            memory_index += 1;
                            mem.minimum = new_min as u64;
                            imports.import(import.module, import.name, mem);
                        } else {
                            imports.import(
                                import.module,
                                import.name,
                                translate::import(import.ty),
                            );
                        }
                    }

                    encoder.section(&imports);
                }

                // Encode the initialized global values from the snapshot,
                // rather than the original values.
                s if s.id == u8::from(SectionId::Global) => {
                    let mut globals = wasm_encoder::GlobalSection::new();
                    for ((_, glob_ty), val) in
                        module.defined_globals(cx).zip(snapshot.globals.iter())
                    {
                        let glob_ty = translate::global_type(glob_ty);
                        globals.global(
                            glob_ty,
                            &match val {
                                wasmer::Value::I32(x) => ConstExpr::i32_const(*x),
                                wasmer::Value::I64(x) => ConstExpr::i64_const(*x),
                                wasmer::Value::F32(x) => ConstExpr::f32_const(*x),
                                wasmer::Value::F64(x) => ConstExpr::f64_const(*x),
                                wasmer::Value::V128(x) => ConstExpr::v128_const(*x as i128),
                                _ => unreachable!(),
                            },
                        );
                    }
                    encoder.section(&globals);
                }

                // Remove exports for the wizer initialization
                // function and WASI reactor _initialize function,
                // then perform any requested renames.
                s if s.id == u8::from(SectionId::Export) => {
                    let mut exports = wasm_encoder::ExportSection::new();
                    for export in module.exports(cx) {
                        if (export.name == self.init_func
                            && !self.keep_init_func.unwrap_or(DEFAULT_KEEP_INIT_FUNC))
                            || (has_wasix_initialize && export.name == "_initialize")
                        {
                            continue;
                        }

                        if !renames.rename_src_to_dst.contains_key(export.name)
                            && renames.rename_dsts.contains(export.name)
                        {
                            // A rename overwrites this export, and it is not
                            // renamed to another export, so skip it.
                            continue;
                        }

                        let field = renames
                            .rename_src_to_dst
                            .get(export.name)
                            .map_or(export.name, |f| f.as_str());

                        let kind = translate::export(export.kind);
                        exports.export(field, kind, export.index);
                    }
                    encoder.section(&exports);
                }

                // Transcribe functions from the original module, and optionally
                // add the new init_memory function.
                s if s.id == u8::from(SectionId::Function) => {
                    let mut function_section = wasm_encoder::FunctionSection::new();

                    for (_, function) in module.defined_functions(cx) {
                        function_section.function(function.index);
                    }

                    if has_wasix_init_memory {
                        function_section.function(
                            wasix_init_memory_function_type
                                .expect("Wasix init memory function type not initialized")
                                .index,
                        );
                    }

                    encoder.section(&function_section);
                }

                s if s.id == u8::from(SectionId::Code) => {
                    let mut code_section = wasm_encoder::CodeSection::new();

                    for code in module.defined_code(cx) {
                        code_section.function(code);
                    }

                    if has_wasix_init_memory {
                        code_section
                            .function(&generate_wasix_memory_init_function(cx, &module, snapshot));
                    }

                    encoder.section(&code_section);
                }

                s if s.id == u8::from(SectionId::Start) => {
                    // We need to generate a new memory_init function if the module is a multi-threaded WASIX module
                    if has_wasix_init_memory {
                        encoder.section(&wasm_encoder::StartSection {
                            function_index: wasix_init_memory_function_idx
                                .expect("Should have generated a WASIX memory_init function")
                                .try_into()
                                .unwrap(),
                        });
                    }

                    // otherwise, nothing to do as the start function has already run
                }

                s if s.id == u8::from(SectionId::DataCount) => {
                    encoder.section(&wasm_encoder::DataCountSection { count: data_count });
                }

                s if s.id == u8::from(SectionId::Data) => {
                    add_data_section(&mut encoder);
                }

                s => {
                    encoder.section(s);
                }
            }
        }

        // Make sure that we've added our data section to the module.
        add_data_section(&mut encoder);
        Ok(encoder.finish())
    }
}

fn is_name_section(s: &wasm_encoder::RawSection) -> bool {
    s.id == u8::from(SectionId::Custom) && {
        let mut reader = wasmparser::BinaryReader::new(s.data);
        matches!(reader.read_string(), Ok("name"))
    }
}

fn generate_wasix_memory_init_function(
    cx: &ModuleContext<'_>,
    module: &crate::info::Module,
    snapshot: &Snapshot,
) -> wasm_encoder::Function {
    use wasm_encoder::Instruction as I;

    let defined_datas_count = module.datas(cx).len();
    let data_segments = snapshot
        .data_segments
        .iter()
        .enumerate()
        .map(|(n, d)| (n + defined_datas_count, d))
        .collect::<Vec<_>>();

    // This is really just an arbitrary choice. As far as I know, LLVM (the only compiler we
    // really, actually use everywhere) leaves the first 1KB of memory untouched, so this
    // address should be unused. To guard against potential corruption in the future, we
    // also check if this address was written to while the module was initializing; i.e.
    // if this falls within any of the snapshot segments.
    let status_flag_address = 1020_i32;
    if data_segments.iter().any(|(_, d)| {
        d.offset < (status_flag_address + 4).try_into().unwrap()
            && d.offset + d.len > status_flag_address.try_into().unwrap()
    }) {
        panic!(
            "The address chosen for the memory initialization status flag was written to \
            during module initialization, a new address must be chosen"
        );
    }

    let mut function = wasm_encoder::Function::new([]);

    let mut i = |inst| {
        function.instruction(&inst);
    };

    i(I::Block(wasm_encoder::BlockType::Empty));
    {
        i(I::Block(wasm_encoder::BlockType::Empty));
        {
            i(I::Block(wasm_encoder::BlockType::Empty));
            {
                // Decide the current status:
                //  * 0 = not initialized, we should initialize
                //  * 1 = another thread is initializing the memory, we should wait
                //  * 2 = already initialized, nothing to do
                i(I::I32Const(status_flag_address));
                i(I::I32Const(0));
                i(I::I32Const(1));
                i(I::I32AtomicRmwCmpxchg(wasm_encoder::MemArg {
                    memory_index: 0,
                    align: 2,
                    offset: 0,
                }));
                i(I::BrTable([0, 1].as_ref().into(), 2));

                i(I::End);
            }

            for (n, d) in &data_segments {
                i(I::I32Const(d.offset.try_into().unwrap()));
                i(I::I32Const(0));
                i(I::I32Const(d.len.try_into().unwrap()));
                i(I::MemoryInit {
                    mem: d.memory_index,
                    data_index: (*n).try_into().unwrap(),
                });
            }

            // Write the status flag to indicate we're done
            i(I::I32Const(status_flag_address));
            i(I::I32Const(2));
            i(I::I32AtomicStore(wasm_encoder::MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));

            // Notify any threads that are waiting
            i(I::I32Const(status_flag_address));
            i(I::I32Const(-1));
            i(I::MemoryAtomicNotify(wasm_encoder::MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            i(I::Drop);

            // Skip the waiting logic
            i(I::Br(1));

            i(I::End);
        }

        i(I::I32Const(status_flag_address));
        i(I::I32Const(1));
        i(I::I64Const(-1));
        i(I::MemoryAtomicWait32(wasm_encoder::MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        i(I::Drop);

        i(I::End);
    }

    // Drop all the data segments as they're no longer needed
    for (n, _) in &data_segments {
        i(I::DataDrop((*n).try_into().unwrap()));
    }

    // End the function
    i(I::End);

    function
}

/*
X  block ;; label = @1
X    block ;; label = @2
X      block ;; label = @3
X        i32.const 8176
X        i32.const 0
X        i32.const 1
X        i32.atomic.rmw.cmpxchg
X        br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
X      end
X      i32.const 1024
X      i32.const 0
X      i32.const 112
X      memory.init $.tdata
X      i32.const 1136
X      i32.const 0
X      i32.const 2720
X      memory.init $.rodata
X      i32.const 3856
X      i32.const 0
X      i32.const 296
X      memory.init $.data
X      i32.const 8176
X      i32.const 2
X      i32.atomic.store
X      i32.const 8176
X      i32.const -1
X      memory.atomic.notify
X      drop
X      br 1 (;@1;)
X    end
X    i32.const 8176
X    i32.const 1
X    i64.const -1
X    memory.atomic.wait32
X    drop
X  end
X  data.drop $.rodata
X  data.drop $.data
*/
