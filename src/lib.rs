//! Wizer: the WebAssembly pre-initializer!
//!
//! See the [`Wizer`] struct for details.

#![deny(missing_docs)]

#[cfg(fuzzing)]
pub mod dummy;
#[cfg(not(fuzzing))]
mod dummy;

mod info;
mod instrument;
mod parse;
mod rewrite;
mod snapshot;
mod stack_ext;
mod translate;

/// Re-export wasmer so users can align with our version.
pub use wasmer;

use anyhow::Context;
use dummy::dummy_imports;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;
#[cfg(feature = "structopt")]
use structopt::StructOpt;
use wasmer::AsStoreMut;
use wasmer::{sys::Features, Cranelift, Engine, Extern, Imports, NativeEngineExt, Store, Target};
use wasmer_wasix::{
    runners::{wasi::WasiRunner, MappedDirectory},
    runtime::task_manager::tokio::{RuntimeOrHandle, TokioTaskManager},
    virtual_fs::NullFile,
    PluggableRuntime,
};
use webc::metadata::annotations::Wasi;

const DEFAULT_INHERIT_STDIO: bool = true;
const DEFAULT_INHERIT_ENV: bool = false;
const DEFAULT_KEEP_INIT_FUNC: bool = false;
const DEFAULT_WASM_MULTI_VALUE: bool = true;
const DEFAULT_WASM_MULTI_MEMORY: bool = true;
const DEFAULT_WASM_BULK_MEMORY: bool = false;
const DEFAULT_WASM_SIMD: bool = true;

#[cfg(feature = "structopt")]
fn parse_map_dirs(s: &str) -> anyhow::Result<(String, PathBuf)> {
    let parts: Vec<&str> = s.split("::").collect();
    if parts.len() != 2 {
        anyhow::bail!("must contain exactly one double colon ('::')");
    }
    Ok((parts[0].into(), parts[1].into()))
}

/// Wizer: the WebAssembly pre-initializer!
///
/// Don't wait for your Wasm module to initialize itself, pre-initialize it!
/// Wizer instantiates your WebAssembly module, executes its initialization
/// function, and then serializes the instance's initialized state out into a
/// new WebAssembly module. Now you can use this new, pre-initialized
/// WebAssembly module to hit the ground running, without making your users wait
/// for that first-time set up code to complete.
///
/// ## Caveats
///
/// * The initialization function may not call any imported functions. Doing so
///   will trigger a trap and `wizer` will exit.
///
/// * The Wasm module may not import globals, tables, or memories.
///
/// * Reference types are not supported yet. This is tricky because it would
///   allow the Wasm module to mutate tables, and we would need to be able to
///   snapshot the new table state, but funcrefs and externrefs don't have
///   identity and aren't comparable in the Wasm spec, which makes snapshotting
///   difficult.
#[cfg_attr(feature = "structopt", derive(StructOpt))]
#[derive(Clone)]
pub struct Wizex {
    /// The Wasm export name of the function that should be executed to
    /// initialize the Wasm module.
    #[cfg_attr(
        feature = "structopt",
        structopt(short = "f", long = "init-func", default_value = "wizex.initialize")
    )]
    init_func: String,

    /// Any function renamings to perform.
    ///
    /// A renaming specification `dst=src` renames a function export `src` to
    /// `dst`, overwriting any previous `dst` export.
    ///
    /// Multiple renamings can be specified. It is an error to specify more than
    /// one source to rename to a destination name, or to specify more than one
    /// renaming destination for one source.
    ///
    /// This option can be used, for example, to replace a `_start` entry point
    /// in an initialized module with an alternate entry point.
    ///
    /// When module linking is enabled, these renames are only applied to the
    /// outermost module.
    #[cfg_attr(
        feature = "structopt",
        structopt(
            short = "r",
            long = "rename-func",
            alias = "func-rename",
            value_name = "dst=src"
        )
    )]
    func_renames: Vec<String>,

    /// Allow WASIX imports to be called during initialization.
    ///
    /// This can introduce diverging semantics because the initialization can
    /// observe nondeterminism that might have gone a different way at runtime
    /// than it did at initialization time.
    ///
    /// If your Wasm module uses WASIX's `get_random` to add randomness to
    /// something as a security mitigation (e.g. something akin to ASLR or the
    /// way Rust's hash maps incorporate a random nonce) then note that, if the
    /// randomization is added during initialization time and you don't ever
    /// re-randomize at runtime, then that randomization will become per-module
    /// rather than per-instance.
    #[cfg_attr(feature = "structopt", structopt(long = "allow-wasix"))]
    allow_wasix: bool,

    /// Provide an additional preloaded module that is available to the
    /// main module.
    ///
    /// This allows running a module that depends on imports from
    /// another module. Note that the additional module's state is *not*
    /// snapshotted, nor is its code included in the Wasm snapshot;
    /// rather, it is assumed that the resulting snapshot Wasm will also
    /// be executed with the same imports available.
    ///
    /// The main purpose of this option is to allow "stubs" for certain
    /// intrinsics to be included, when these will be provided with
    /// different implementations when running or further processing the
    /// snapshot.
    ///
    /// The format of this option is `name=file.{wasm,wat}`; e.g.,
    /// `intrinsics=stubs.wat`. Multiple instances of the option may
    /// appear.
    #[cfg_attr(feature = "structopt", structopt(long = "preload"))]
    preload: Vec<String>,

    /// Like `preload` above, but with the module contents provided,
    /// rather than a filename. This is more useful for programmatic
    /// use-cases where the embedding tool may also embed a Wasm module.
    #[cfg_attr(feature = "structopt", structopt(skip))]
    preload_bytes: Vec<(String, Vec<u8>)>,

    /// When using WASIX during initialization, should `stdin`, `stderr`, and
    /// `stdout` be inherited?
    ///
    /// This is true by default.
    #[cfg_attr(
        feature = "structopt",
        structopt(long = "inherit-stdio", value_name = "true|false")
    )]
    inherit_stdio: Option<bool>,

    /// When using WASIX during initialization, should environment variables be
    /// inherited?
    ///
    /// This is false by default.
    #[cfg_attr(
        feature = "structopt",
        structopt(long = "inherit-env", value_name = "true|false")
    )]
    inherit_env: Option<bool>,

    /// After initialization, should the Wasm module still export the
    /// initialization function?
    ///
    /// This is `false` by default, meaning that the initialization function is
    /// no longer exported from the Wasm module.
    #[cfg_attr(
        feature = "structopt",
        structopt(long = "keep-init-func", value_name = "true|false")
    )]
    keep_init_func: Option<bool>,

    /// When using WASIX during initialization, which file system directories
    /// should be made available?
    ///
    /// None are available by default.
    #[cfg_attr(
        feature = "structopt",
        structopt(long = "dir", parse(from_os_str), value_name = "directory")
    )]
    dirs: Vec<PathBuf>,

    /// When using WASIX during initialization, which guest directories should be
    /// mapped to a host directory?
    ///
    /// The `--mapdir` option differs from `--dir` in that it allows giving a
    /// custom guest name to the directory that is different from its name in
    /// the host.
    ///
    /// None are mapped by default.
    #[cfg_attr(
        feature = "structopt",
        structopt(long = "mapdir", value_name = "GUEST_DIR::HOST_DIR", parse(try_from_str = parse_map_dirs))
    )]
    map_dirs: Vec<(String, PathBuf)>,

    /// Enable or disable Wasm multi-memory proposal.
    ///
    /// Enabled by default.
    #[cfg_attr(feature = "structopt", structopt(long, value_name = "true|false"))]
    wasm_multi_memory: Option<bool>,

    /// Enable or disable the Wasm multi-value proposal.
    ///
    /// Enabled by default.
    #[cfg_attr(feature = "structopt", structopt(long, value_name = "true|false"))]
    wasm_multi_value: Option<bool>,

    /// Enable or disable Wasm bulk memory operations.
    ///
    /// Note that only `memory.copy`, `memory.fill`, and `memory.init` operations
    /// are currently supported.  Modules which use other instructions, such as
    /// `table.copy` will be rejected.
    ///
    /// Disabled by default.
    #[cfg_attr(feature = "structopt", structopt(long, value_name = "true|false"))]
    wasm_bulk_memory: Option<bool>,

    /// Enable or disable the Wasm SIMD128 proposal.
    ///
    /// Enabled by default.
    #[cfg_attr(feature = "structopt", structopt(long, value_name = "true|false"))]
    wasm_simd: Option<bool>,
}

impl std::fmt::Debug for Wizex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Wizex {
            init_func,
            func_renames,
            allow_wasix,
            preload,
            preload_bytes,
            inherit_stdio,
            inherit_env,
            keep_init_func,
            dirs,
            map_dirs,
            wasm_multi_memory,
            wasm_multi_value,
            wasm_bulk_memory,
            wasm_simd,
        } = self;
        f.debug_struct("Wizer")
            .field("init_func", &init_func)
            .field("func_renames", &func_renames)
            .field("allow_wasix", &allow_wasix)
            .field("preload", &preload)
            .field("preload_bytes", &preload_bytes)
            .field("make_linker", &"..")
            .field("inherit_stdio", &inherit_stdio)
            .field("inherit_env", &inherit_env)
            .field("keep_init_func", &keep_init_func)
            .field("dirs", &dirs)
            .field("map_dirs", &map_dirs)
            .field("wasm_multi_memory", &wasm_multi_memory)
            .field("wasm_multi_value", &wasm_multi_value)
            .field("wasm_bulk_memory", &wasm_bulk_memory)
            .field("wasm_simd", &wasm_simd)
            .finish()
    }
}

struct FuncRenames {
    /// For a given export name that we encounter in the original module, a map
    /// to a new name, if any, to emit in the output module.
    rename_src_to_dst: HashMap<String, String>,
    /// A set of export names that we ignore in the original module (because
    /// they are overwritten by renamings).
    rename_dsts: HashSet<String>,
}

impl FuncRenames {
    fn parse(renames: &Vec<String>) -> anyhow::Result<FuncRenames> {
        let mut ret = FuncRenames {
            rename_src_to_dst: HashMap::new(),
            rename_dsts: HashSet::new(),
        };
        if renames.is_empty() {
            return Ok(ret);
        }

        for rename_spec in renames {
            let equal = rename_spec
                .trim()
                .find('=')
                .ok_or_else(|| anyhow::anyhow!("Invalid function rename part: {}", rename_spec))?;
            // TODO: use .split_off() when the API is stabilized.
            let dst = rename_spec[..equal].to_owned();
            let src = rename_spec[equal + 1..].to_owned();
            if ret.rename_dsts.contains(&dst) {
                anyhow::bail!("Duplicated function rename dst {}", dst);
            }
            if ret.rename_src_to_dst.contains_key(&src) {
                anyhow::bail!("Duplicated function rename src {}", src);
            }
            ret.rename_dsts.insert(dst.clone());
            ret.rename_src_to_dst.insert(src, dst);
        }

        Ok(ret)
    }
}

impl Default for Wizex {
    fn default() -> Self {
        Self::new()
    }
}

impl Wizex {
    /// Construct a new `Wizer` builder.
    pub fn new() -> Self {
        Wizex {
            init_func: "wizex.initialize".into(),
            func_renames: vec![],
            allow_wasix: false,
            preload: vec![],
            preload_bytes: vec![],
            inherit_stdio: None,
            inherit_env: None,
            keep_init_func: None,
            dirs: vec![],
            map_dirs: vec![],
            wasm_multi_memory: None,
            wasm_multi_value: None,
            wasm_bulk_memory: None,
            wasm_simd: None,
        }
    }

    /// The export name of the initializer function.
    ///
    /// Defaults to `"wizex.initialize"`.
    pub fn init_func(&mut self, init_func: impl Into<String>) -> &mut Self {
        self.init_func = init_func.into();
        self
    }

    /// Add a function rename to perform.
    pub fn func_rename(&mut self, new_name: impl Display, old_name: impl Display) -> &mut Self {
        self.func_renames.push(format!("{}={}", new_name, old_name));
        self
    }

    /// Allow WASIX imports to be called during initialization?
    ///
    /// This can introduce diverging semantics because the initialization can
    /// observe nondeterminism that might have gone a different way at runtime
    /// than it did at initialization time.
    ///
    /// If your Wasm module uses WASI's `get_random` to add randomness to
    /// something as a security mitigation (e.g. something akin to ASLR or the
    /// way Rust's hash maps incorporate a random nonce) then note that, if the
    /// randomization is added during initialization time and you don't ever
    /// re-randomize at runtime, then that randomization will become per-module
    /// rather than per-instance.
    ///
    /// Defaults to `false`.
    pub fn allow_wasix(&mut self, allow: bool) -> anyhow::Result<&mut Self> {
        self.allow_wasix = allow;
        Ok(self)
    }

    /// Provide an additional preloaded module that is available to the
    /// main module.
    ///
    /// This allows running a module that depends on imports from
    /// another module. Note that the additional module's state is *not*
    /// snapshotted, nor is its code included in the Wasm snapshot;
    /// rather, it is assumed that the resulting snapshot Wasm will also
    /// be executed with the same imports available.
    ///
    /// The main purpose of this option is to allow "stubs" for certain
    /// intrinsics to be included, when these will be provided with
    /// different implementations when running or further processing the
    /// snapshot.
    pub fn preload(&mut self, name: &str, filename: &str) -> anyhow::Result<&mut Self> {
        anyhow::ensure!(
            !name.contains("="),
            "Module name cannot contain an `=` character"
        );
        self.preload.push(format!("{}={}", name, filename));
        Ok(self)
    }

    /// Provide an additional preloaded module that is available to the
    /// main module. Unlike `preload()`, this method takes an owned
    /// vector of bytes as the module's actual content, rather than a
    /// filename. As with `preload()`, the module may be in Wasm binary
    /// format or in WAT text format.
    ///
    /// This allows running a module that depends on imports from
    /// another module. Note that the additional module's state is *not*
    /// snapshotted, nor is its code included in the Wasm snapshot;
    /// rather, it is assumed that the resulting snapshot Wasm will also
    /// be executed with the same imports available.
    ///
    /// The main purpose of this option is to allow "stubs" for certain
    /// intrinsics to be included, when these will be provided with
    /// different implementations when running or further processing the
    /// snapshot.
    pub fn preload_bytes(
        &mut self,
        name: &str,
        module_bytes: Vec<u8>,
    ) -> anyhow::Result<&mut Self> {
        self.preload_bytes.push((name.to_owned(), module_bytes));
        Ok(self)
    }

    /// When using WASIX during initialization, should `stdin`, `stdout`, and
    /// `stderr` be inherited?
    ///
    /// Defaults to `true`.
    pub fn inherit_stdio(&mut self, inherit: bool) -> &mut Self {
        self.inherit_stdio = Some(inherit);
        self
    }

    /// When using WASIX during initialization, should the environment variables
    /// be inherited?
    ///
    /// Defaults to `false`.
    pub fn inherit_env(&mut self, inherit: bool) -> &mut Self {
        self.inherit_env = Some(inherit);
        self
    }

    /// After initialization, should the Wasm module still export the
    /// initialization function?
    ///
    /// This is `false` by default, meaning that the initialization function is
    /// no longer exported from the Wasm module.
    pub fn keep_init_func(&mut self, keep: bool) -> &mut Self {
        self.keep_init_func = Some(keep);
        self
    }

    /// When using WASIX during initialization, which file system directories
    /// should be made available?
    ///
    /// None are available by default.
    pub fn dir(&mut self, directory: impl Into<PathBuf>) -> &mut Self {
        self.dirs.push(directory.into());
        self
    }

    /// When using WASIX during initialization, which guest directories should be
    /// mapped to a host directory?
    ///
    /// The `map_dir` method differs from `dir` in that it allows giving a custom
    /// guest name to the directory that is different from its name in the host.
    ///
    /// None are mapped by default.
    pub fn map_dir(
        &mut self,
        guest_dir: impl Into<String>,
        host_dir: impl Into<PathBuf>,
    ) -> &mut Self {
        self.map_dirs.push((guest_dir.into(), host_dir.into()));
        self
    }

    /// Enable or disable the Wasm multi-memory proposal.
    ///
    /// Defaults to `true`.
    pub fn wasm_multi_memory(&mut self, enable: bool) -> &mut Self {
        self.wasm_multi_memory = Some(enable);
        self
    }

    /// Enable or disable the Wasm multi-value proposal.
    ///
    /// Defaults to `true`.
    pub fn wasm_multi_value(&mut self, enable: bool) -> &mut Self {
        self.wasm_multi_value = Some(enable);
        self
    }

    /// Enable or disable Wasm bulk memory operations.
    ///
    /// Note that only `memory.copy`, `memory.fill`, and `memory.init`
    /// operations are currently supported.  Modules which use other
    /// instructions, such as `table.copy` will be rejected.
    ///
    /// Defaults to `false`.
    pub fn wasm_bulk_memory(&mut self, enable: bool) -> &mut Self {
        self.wasm_bulk_memory = Some(enable);
        self
    }

    /// Enable or disable the Wasm SIMD128 proposal.
    ///
    /// Defaults to `true`.
    pub fn wasm_simd(&mut self, enable: bool) -> &mut Self {
        self.wasm_simd = Some(enable);
        self
    }

    /// Initialize the given Wasm, snapshot it, and return the serialized
    /// snapshot as a new, pre-initialized Wasm module.
    pub fn run(&self, wasm: &[u8]) -> anyhow::Result<Vec<u8>> {
        // Parse rename spec.
        let renames = FuncRenames::parse(&self.func_renames)?;

        // Make sure we're given valid Wasm from the get go.
        self.wasm_validate(wasm)?;

        let mut cx = parse::parse(wasm)?;
        let instrumented_wasm = instrument::instrument(&cx);

        if cfg!(debug_assertions) {
            if let Err(error) = self.wasm_validate(&instrumented_wasm) {
                #[cfg(feature = "wasmprinter")]
                let wat = wasmprinter::print_bytes(&instrumented_wasm)
                    .unwrap_or_else(|e| format!("Disassembling to WAT failed: {}", e));
                #[cfg(not(feature = "wasmprinter"))]
                let wat = "`wasmprinter` cargo feature is not enabled".to_string();
                panic!(
                    "instrumented Wasm is not valid: {:?}\n\nWAT:\n{}",
                    error, wat
                );
            }
        }

        let (config, features) = self.wasmer_config();
        let engine = wasmer::Engine::new(config, Target::default(), features);

        let mut tokio_runtime = {
            if !self.allow_wasix {
                None
            } else {
                Some(
                    tokio::runtime::Builder::new_multi_thread()
                        .enable_all()
                        .build()?,
                )
            }
        };

        let tokio_runtime_handle = tokio_runtime.as_ref().map(|r| r.handle().clone());
        let _guard = tokio_runtime_handle.as_ref().map(|h| h.enter());

        let runtime = tokio_runtime.take().map(|r| {
            let tokio_task_manager = Arc::new(TokioTaskManager::new(RuntimeOrHandle::from(r)));
            let mut rt = PluggableRuntime::new(tokio_task_manager);
            rt.set_engine(Some(engine.clone()))
                .set_networking_implementation(virtual_net::host::LocalNetworking::default());
            rt
        });

        let runner = self.wasix_runner()?;
        let module = wasmer::Module::new(&engine, &instrumented_wasm)
            .context("failed to compile the Wasm module")?;
        let mut store = wasmer::Store::new(engine.clone());
        self.validate_init_func(&module)?;

        // If the module imports any WASI(X) thread-related functions, we want to give it
        // a memory init function instead of active data segments, since active data segments
        // and threads don't play nicely together. Basically, each thread is a new instance,
        // and will overwrite the (shared) memory with data from its active data segments,
        // which will corrupt the memory.
        let has_wasix_init_memory = module
            .imports()
            .any(|i| i.module().contains("wasi") && i.name().contains("thread"));

        let (instance, has_wasix_initialize, imported_memories) =
            self.initialize(&engine, &mut store, &module, runtime, runner)?;
        let snapshot = snapshot::snapshot(&mut store, &instance, &imported_memories);
        let rewritten_wasm = self.rewrite(
            &mut cx,
            &store,
            &snapshot,
            &renames,
            has_wasix_initialize,
            has_wasix_init_memory,
        )?;

        if cfg!(debug_assertions) {
            if let Err(error) = self.wasm_validate(&rewritten_wasm) {
                #[cfg(feature = "wasmprinter")]
                let wat = wasmprinter::print_bytes(&rewritten_wasm)
                    .unwrap_or_else(|e| format!("Disassembling to WAT failed: {}", e));
                #[cfg(not(feature = "wasmprinter"))]
                let wat = "`wasmprinter` cargo feature is not enabled".to_string();
                panic!("rewritten Wasm is not valid: {:?}\n\nWAT:\n{}", error, wat);
            }
        }

        Ok(rewritten_wasm)
    }

    // NB: keep this in sync with the wasmparser features.
    fn wasmer_config(&self) -> (Box<dyn wasmer::CompilerConfig>, Features) {
        let config = Cranelift::new();

        let mut features = Features::new();

        // Proposals we support.
        features.multi_memory(self.wasm_multi_memory.unwrap_or(DEFAULT_WASM_MULTI_MEMORY));
        features.multi_value(self.wasm_multi_value.unwrap_or(DEFAULT_WASM_MULTI_VALUE));
        // Note that we only support `memory.copy`, `memory.fill`, and
        // `memory.init` for the time being:
        features.bulk_memory(self.wasm_bulk_memory.unwrap_or(DEFAULT_WASM_BULK_MEMORY));

        features.simd(self.wasm_simd.unwrap_or(DEFAULT_WASM_SIMD));

        // Proposals that we should add support for.
        features.reference_types(false);

        (Box::new(config), features)
    }

    // NB: keep this in sync with the wasmer config.
    fn wasm_features(&self) -> wasmparser::WasmFeatures {
        wasmparser::WasmFeatures {
            mutable_global: true,
            saturating_float_to_int: true,
            sign_extension: true,
            floats: true,
            component_model_values: false,
            component_model_nested_names: false,

            // Proposals that we support.
            multi_memory: self.wasm_multi_memory.unwrap_or(DEFAULT_WASM_MULTI_MEMORY),
            multi_value: self.wasm_multi_value.unwrap_or(DEFAULT_WASM_MULTI_VALUE),
            threads: true,

            // Proposals that we should add support for.
            reference_types: false,
            simd: self.wasm_simd.unwrap_or(DEFAULT_WASM_SIMD),
            tail_call: false,
            memory64: false,
            exceptions: false,
            extended_const: false,
            relaxed_simd: false,
            component_model: false,
            memory_control: false,
            function_references: false,
            gc: false,

            // XXX: Though we don't fully support bulk memory yet, we
            // unconditionally turn it on.
            //
            // Many parsers, notably our own `wasmparser`, assume that which
            // Wasm features are enabled or disabled cannot affect parsing, only
            // validation. That assumption is incorrect when it comes to data
            // segments, the multi-memory proposal, and the bulk memory
            // proposal. A `0x01` prefix of a data segment can either mean "this
            // is a passive segment" if bulk memory is enabled or "this segment
            // is referring to memory index 1" if both bulk memory is disabled
            // and multi-memory is enabled. `wasmparser` fails to handle this
            // edge case, which means that everything built on top of it, like
            // Wasmtime, also fail to handle this edge case. However, because
            // bulk memory is merged into the spec proper and is no longer
            // technically a "proposal", and because a fix would require
            // significant refactoring and API changes to give a
            // `wasmparser::Parser` a `wasmparser::WasmFeatures`, we won't ever
            // resolve this discrepancy in `wasmparser`.
            //
            // So we enable bulk memory during parsing, validation, and
            // execution, but we add our own custom validation pass to ensure
            // that no table-mutating instructions exist in our input modules
            // until we *actually* support bulk memory.
            bulk_memory: true,
        }
    }

    fn wasm_validate(&self, wasm: &[u8]) -> anyhow::Result<()> {
        log::debug!("Validating input Wasm");

        let mut validator = wasmparser::Validator::new_with_features(self.wasm_features());
        validator.validate_all(wasm)?;

        // Reject bulk memory stuff that manipulates state we don't
        // snapshot. See the comment inside `wasm_features`.
        let mut wasm = wasm;
        let mut parsers = vec![wasmparser::Parser::new(0)];
        while !parsers.is_empty() {
            let payload = match parsers.last_mut().unwrap().parse(wasm, true).unwrap() {
                wasmparser::Chunk::NeedMoreData(_) => unreachable!(),
                wasmparser::Chunk::Parsed { consumed, payload } => {
                    wasm = &wasm[consumed..];
                    payload
                }
            };
            match payload {
                wasmparser::Payload::CodeSectionEntry(code) => {
                    let mut ops = code.get_operators_reader().unwrap();
                    while !ops.eof() {
                        match ops.read().unwrap() {
                            wasmparser::Operator::TableCopy { .. } => {
                                anyhow::bail!("unsupported `table.copy` instruction")
                            }
                            wasmparser::Operator::TableInit { .. } => {
                                anyhow::bail!("unsupported `table.init` instruction")
                            }
                            wasmparser::Operator::ElemDrop { .. } => {
                                anyhow::bail!("unsupported `elem.drop` instruction")
                            }
                            // TODO @wasmer: WASIX modules seem to like using data.drop at the end of
                            // __wasm_init_memory, which *should* be harmless here?
                            // Technically, we could leave the dropped data sections out. However, in
                            // practice, I have no idea how to detect if a data section was dropped or
                            // not over the lifetime of the initialization function.
                            // wasmparser::Operator::DataDrop { .. } => {
                            //     anyhow::bail!("unsupported `data.drop` instruction")
                            // }
                            wasmparser::Operator::TableSet { .. } => {
                                unreachable!("part of reference types")
                            }
                            _ => continue,
                        }
                    }
                }
                wasmparser::Payload::End(_) => {
                    parsers.pop();
                }
                _ => continue,
            }
        }

        Ok(())
    }

    /// Check that the module exports an initialization function, and that the
    /// function has the correct type.
    fn validate_init_func(&self, module: &wasmer::Module) -> anyhow::Result<()> {
        log::debug!("Validating the exported initialization function");
        match module.exports().find(|e| e.name() == self.init_func) {
            Some(export) => match export.ty() {
                wasmer::ExternType::Function(func_ty) => {
                    if !func_ty.params().is_empty() || !func_ty.results().is_empty() {
                        anyhow::bail!(
                            "the Wasm module's `{}` function export does not have type `[] -> []`",
                            &self.init_func
                        );
                    }
                }
                _ => anyhow::bail!(
                    "the Wasm module's `{}` export is not a function",
                    &self.init_func
                ),
            },
            None => anyhow::bail!(
                "the Wasm module does not have a `{}` export",
                &self.init_func
            ),
        }
        Ok(())
    }

    fn wasix_runner(&self) -> anyhow::Result<Option<WasiRunner>> {
        if !self.allow_wasix {
            return Ok(None);
        }

        let mut runner = WasiRunner::new();

        runner
            .with_forward_host_env(self.inherit_env.unwrap_or(DEFAULT_INHERIT_ENV))
            .with_mapped_directories(self.dirs.iter().map(|dir| {
                log::debug!("Preopening directory: {}", dir.display());
                MappedDirectory {
                    host: dir.clone(),
                    guest: dir
                        .to_str()
                        .expect("Failed to convert path to string")
                        .to_owned(),
                }
            }))
            .with_mapped_directories(self.map_dirs.iter().map(|(guest, host)| {
                log::debug!("Preopening directory: {}::{}", guest, host.display());
                MappedDirectory {
                    host: host.clone(),
                    guest: guest.clone(),
                }
            }));

        if !self.inherit_stdio.unwrap_or(DEFAULT_INHERIT_STDIO) {
            runner
                .with_stdin(Box::<NullFile>::default())
                .with_stdout(Box::<NullFile>::default())
                .with_stderr(Box::<NullFile>::default());
        }

        Ok(Some(runner))
    }

    /// Preload a module.
    fn instantiate_for_preload(
        engine: &Engine,
        store: &mut wasmer::StoreMut,
        content: &[u8],
    ) -> anyhow::Result<wasmer::Instance> {
        let module =
            wasmer::Module::new(engine, content).context("failed to parse preload module")?;
        let imports = wasmer::imports! {};
        wasmer::Instance::new(store, &module, &imports)
            .context("failed to instantiate preload module")
    }

    fn prepare_preloads(
        preload: &Vec<String>,
        preload_bytes: &Vec<(String, Vec<u8>)>,
        engine: &Engine,
        store: &mut wasmer::StoreMut,
    ) -> anyhow::Result<Vec<(String, wasmer::Instance)>> {
        let mut result = vec![];

        for preload in preload {
            if let Some((name, value)) = preload.split_once('=') {
                let content = std::fs::read(value).context("failed to read preload module")?;
                result.push((
                    name.to_owned(),
                    Self::instantiate_for_preload(engine, store, &content[..])?,
                ));
            } else {
                anyhow::bail!(
                    "Bad preload option: {} (must be of form `name=file`)",
                    preload
                );
            }
        }

        for (name, bytes) in preload_bytes {
            result.push((
                name.clone(),
                Self::instantiate_for_preload(engine, store, &bytes[..])?,
            ));
        }

        Ok(result)
    }

    /// Instantiate the module and call its initialization function.
    fn initialize(
        &self,
        engine: &Engine,
        store: &mut Store,
        module: &wasmer::Module,
        runtime: Option<PluggableRuntime>,
        wasi_runner: Option<WasiRunner>,
    ) -> anyhow::Result<(wasmer::Instance, bool, Vec<wasmer::Memory>)> {
        log::debug!("Calling the initialization function");

        let mut imported_memories = vec![];

        let instance = match (
            wasmer_wasix::is_wasi_module(module) || wasmer_wasix::is_wasix_module(module),
            runtime,
            wasi_runner,
        ) {
            (true, Some(mut runtime), Some(runner)) => {
                let engine = engine.clone();
                let module_imports = module.imports().collect::<Vec<_>>();
                let preload = self.preload.clone();
                let preload_bytes = self.preload_bytes.clone();
                runtime.with_additional_imports(move |store| {
                    let mut imports = Imports::new();
                    let preloads = Self::prepare_preloads(
                        &preload,
                        &preload_bytes,
                        &engine,
                        &mut store.as_store_mut(),
                    )?;
                    for (name, instance) in preloads {
                        imports.register_namespace(&name, instance.exports.clone());
                    }
                    dummy_imports(
                        &mut store.as_store_mut(),
                        module_imports.iter().cloned(),
                        &mut imports,
                    )?;
                    Ok(imports)
                });

                let wasi = Wasi::new("wasix-program");
                let env_builder = runner.prepare_webc_env(
                    "wasix-program",
                    &wasi,
                    None,
                    Arc::new(runtime),
                    None,
                )?;

                let (instance, env) = env_builder.instantiate(module.clone(), store)?;

                // If the module imports its memory, Wasmer will create one for it, and
                // we need to get it here so we can read the contents later.
                if module
                    .imports()
                    .any(|i| matches!(i.ty(), wasmer::ExternType::Memory(_)))
                {
                    imported_memories.push(
                        env.data(store)
                            .try_memory_clone()
                            .expect("Failed to get WASIX memory"),
                    );
                }

                instance
            }
            _ => {
                let mut imports = Imports::new();
                let preloads = Self::prepare_preloads(
                    &self.preload,
                    &self.preload_bytes,
                    engine,
                    &mut store.as_store_mut(),
                )?;
                for (name, instance) in preloads {
                    imports.register_namespace(&name, instance.exports.clone());
                }
                dummy_imports(&mut store.as_store_mut(), module.imports(), &mut imports)?;
                wasmer::Instance::new(store, module, &imports)?
            }
        };

        let mut has_wasix_initialize = false;

        if let Ok(Extern::Function(func)) = instance.exports.get("_initialize") {
            func.typed::<(), ()>(&store)
                .and_then(|f| {
                    has_wasix_initialize = true;
                    f.call(&mut *store).map_err(Into::into)
                })
                .context("calling the Reactor initialization function")?;

            if self.init_func == "_initialize" && has_wasix_initialize {
                // Don't run `_initialize` twice if the it was explicitly
                // requested as the init function.
                return Ok((instance, has_wasix_initialize, imported_memories));
            }
        }

        let init_func = instance
            .exports
            .get_typed_function::<(), ()>(store, &self.init_func)
            .expect("checked by `validate_init_func`");
        init_func
            .call(&mut *store)
            .with_context(|| format!("the `{}` function trapped", self.init_func))?;

        Ok((instance, has_wasix_initialize, imported_memories))
    }
}
