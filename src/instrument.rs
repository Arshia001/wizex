//! The initial instrumentation pass.

use crate::info::{Module, ModuleContext};
use crate::stack_ext::StackExt;
use crate::translate;
use wasm_encoder::SectionId;

/// Instrument the input Wasm so that it exports its memories and globals,
/// allowing us to inspect their state after the module is instantiated and
/// initialized.
///
/// For example, given this input module:
///
/// ```wat
/// (module $A
///   (module $B
///     (memory $B_mem)
///     (global $B_glob (mut i32))
///   )
///
///   (instance $x (instantiate $B))
///   (instance $y (instantiate $B))
///
///   (memory $A_mem)
///   (global $A_glob (mut i32))
/// )
/// ```
///
/// this pass will produce the following instrumented module:
///
/// ```wat
/// (module $A
///   (module $B
///     (memory $B_mem)
///     (global $B_glob (mut i32))
///
///     ;; Export all state.
///     (export "__wizer_memory_0" (memory $B_mem))
///     (export "__wizer_global_0" (global $B_glob))
///   )
///
///   (instance $x (instantiate $B))
///   (instance $y (instantiate $B))
///
///   (memory $A_mem)
///   (global $A_glob (mut i32))
///
///   ;; Export of all state (including transitively re-exporting nested
///   ;; instantiations' state).
///   (export "__wizer_memory_0" (memory $A_mem))
///   (export "__wizer_global_0" (global $A_glob))
///   (export "__wizer_instance_0" (instance $x))
///   (export "__wizer_instance_1" (instance $y))
/// )
/// ```
///
/// NB: we re-export nested instantiations as a whole instance export because we
/// can do this without disturbing existing instances' indices. If we were to
/// export their memories and globals individually, that would disturb the
/// modules locally defined memoryies' and globals' indices, which would require
/// rewriting the code section, which would break debug info offsets.
pub(crate) fn instrument(cx: &ModuleContext<'_>) -> Vec<u8> {
    log::debug!("Instrumenting the input Wasm");

    struct StackEntry<'a> {
        /// This entry's module.
        module: Module,

        /// The work-in-progress encoding of the new, instrumented module.
        encoder: wasm_encoder::Module,

        /// Sections in this module info that we are iterating over.
        sections: std::slice::Iter<'a, wasm_encoder::RawSection<'a>>,

        /// The memories we should add for this entry. Kept here because
        /// there are a couple of places we need to check if we've added
        /// the memories if we haven't done it yet.
        memories: Option<wasm_encoder::MemorySection>,
    }

    let root = cx.root();
    let mut stack = vec![StackEntry {
        module: root,
        encoder: wasm_encoder::Module::new(),
        sections: root.raw_sections(cx).iter(),
        memories: {
            let mut memories = wasm_encoder::MemorySection::new();

            for import in root.imports(cx) {
                if let wasmparser::TypeRef::Memory(memory_type) = import.ty {
                    memories.memory(wasm_encoder::MemoryType {
                        minimum: memory_type.initial,
                        maximum: memory_type.maximum,
                        memory64: memory_type.memory64,
                        shared: memory_type.shared,
                    });
                }
            }

            for (_, memory) in root.defined_memories(cx) {
                memories.memory(wasm_encoder::MemoryType {
                    minimum: memory.initial,
                    maximum: memory.maximum,
                    memory64: memory.memory64,
                    shared: memory.shared,
                });
            }

            if memories.is_empty() {
                None
            } else {
                Some(memories)
            }
        },
    }];

    loop {
        assert!(!stack.is_empty());

        match stack.top_mut().sections.next() {
            // For the exports section, we need to transitively export internal
            // state so that we can read the initialized state after we call the
            // initialization function.
            Some(section) if section.id == u8::from(SectionId::Export) => {
                let entry = stack.top_mut();
                let mut exports = wasm_encoder::ExportSection::new();

                // First, copy over all the original exports.
                for export in entry.module.exports(cx) {
                    exports.export(
                        export.name,
                        match export.kind {
                            wasmparser::ExternalKind::Func => wasm_encoder::ExportKind::Func,
                            wasmparser::ExternalKind::Table => wasm_encoder::ExportKind::Table,
                            wasmparser::ExternalKind::Memory => wasm_encoder::ExportKind::Memory,
                            wasmparser::ExternalKind::Global => wasm_encoder::ExportKind::Global,
                            wasmparser::ExternalKind::Tag => {
                                unreachable!("should have been rejected in validation/parsing")
                            }
                        },
                        export.index,
                    );
                }

                // Now export all of this module's defined globals, memories,
                // and instantiations under well-known names so we can inspect
                // them after initialization.
                for (i, (j, _)) in entry.module.defined_globals(cx).enumerate() {
                    let name = format!("__wizer_global_{}", i);
                    exports.export(&name, wasm_encoder::ExportKind::Global, j);
                }

                let mut memory_idx = 0;

                // We turn imported memories into exported ones, and those always start at index 0
                for import in entry.module.imports(cx) {
                    if let wasmparser::TypeRef::Memory(_) = import.ty {
                        let name = format!("__wizer_memory_{}", memory_idx);
                        exports.export(&name, wasm_encoder::ExportKind::Memory, memory_idx);
                        memory_idx += 1;
                    }
                }

                for (j, _) in entry.module.defined_memories(cx) {
                    let name = format!("__wizer_memory_{}", memory_idx);
                    memory_idx += 1;
                    exports.export(&name, wasm_encoder::ExportKind::Memory, j);
                }

                // If the original module had no memory section but we added one,
                // add it in above the export section
                if let Some(memories) = entry.memories.take() {
                    entry.encoder.section(&memories);
                }

                entry.encoder.section(&exports);
            }

            // We turn imported memories into exported ones, so we need to modify the
            // memory section to add memories with the same definitions in
            Some(section) if section.id == u8::from(SectionId::Memory) => {
                let entry = stack.top_mut();

                if let Some(memories) = entry.memories.take() {
                    entry.encoder.section(&memories);
                }
            }

            // We also need to modify the imports section to skip any imported
            // memories.
            Some(section) if section.id == u8::from(SectionId::Import) => {
                let entry = stack.top_mut();
                let mut imports = wasm_encoder::ImportSection::new();

                for import in entry.module.imports(cx) {
                    if !matches!(import.ty, wasmparser::TypeRef::Memory(_)) {
                        imports.import(import.module, import.name, translate::import(import.ty));
                    }
                }

                entry.encoder.section(&imports);
            }

            // End of the current module: if this is the root, return the
            // instrumented module, otherwise add it as an entry in its parent's
            // module section.
            None => {
                let mut entry = stack.pop().unwrap();

                // If the original module had no memory section but we added one,
                // add it in before finalizing the module
                if let Some(memories) = entry.memories.take() {
                    entry.encoder.section(&memories);
                }

                if entry.module.is_root() {
                    assert!(stack.is_empty());
                    return entry.encoder.finish();
                }
            }

            // All other sections don't need instrumentation and can be copied
            // over directly.
            Some(section) => {
                if section.id == u8::from(SectionId::Global)
                    || section.id == u8::from(SectionId::Tag)
                {
                    let entry = stack.top_mut();
                    // If the original module had no memory section but we added one,
                    // it should live above global and tag.
                    if let Some(memories) = entry.memories.take() {
                        entry.encoder.section(&memories);
                    }
                }

                stack.top_mut().encoder.section(section);
            }
        }
    }
}
