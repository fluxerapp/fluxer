// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, command_succeeds, env_bool, output_text, run_command};
use anyhow::{Context, Result, anyhow, ensure};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use clap::Args;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

const LIBFLUXCORE_WASM_BINDGEN_VERSION: &str = "0.2.128";
const LIBFLUXCORE_WASM_SIZE_BUDGET_BYTES: u64 = 300 * 1024;
const LIBFLUXCORE_WRAPPER_JS: &str = include_str!("../templates/libfluxcore_wrapper.js");
const LIBFLUXCORE_WRAPPER_DTS: &str = include_str!("../templates/libfluxcore_wrapper.d.ts");
const LIBFLUXWEBP_SIMD_WASM_SIZE_BUDGET_BYTES: u64 = 672 * 1024;
const LIBFLUXWEBP_SCALAR_WASM_SIZE_BUDGET_BYTES: u64 = 288 * 1024;
const LIBFLUXWEBP_WASM_IMPORT_MODULE: &str = "./libfluxwebp_bg.js";
const WASM_TARGET_RUSTFLAGS_ENV: &str = "CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUSTFLAGS";
const WASM_CFLAGS_ENV: &str = "CFLAGS_wasm32_unknown_unknown";

#[derive(Debug, Args, Clone)]
pub struct BuildAppWasmArgs {
    #[arg(long)]
    app_dir: Option<PathBuf>,
}

#[derive(Debug, Args, Clone)]
pub struct BuildMarkdownParserWasmArgs {
    #[arg(long)]
    app_dir: Option<PathBuf>,
}

pub fn run_build_app_wasm(args: BuildAppWasmArgs) -> Result<()> {
    let app_dir = args.app_dir.unwrap_or(resolve_app_dir()?);
    build_markdown_parser_wasm(&app_dir)?;
    build_libfluxcore_wasm(&app_dir)?;
    build_libfluxwebp_wasm(&app_dir)
}

pub fn run_build_markdown_parser_wasm(args: BuildMarkdownParserWasmArgs) -> Result<()> {
    let app_dir = args.app_dir.unwrap_or(resolve_app_dir()?);
    build_markdown_parser_wasm(&app_dir)
}

const WASM_CC_ENV: &str = "CC_wasm32_unknown_unknown";
const WASM_AR_ENV: &str = "AR_wasm32_unknown_unknown";

fn apply_wasm_c_toolchain(spec: CommandSpec) -> Result<CommandSpec> {
    if env::var_os(WASM_CC_ENV).is_some_and(|value| !value.is_empty()) {
        return Ok(spec);
    }

    let clang = discover_wasm_clang()?;
    println!("+ {WASM_CC_ENV}={}", clang.display());
    let mut spec = spec.env(WASM_CC_ENV, clang.as_os_str());
    if env::var_os(WASM_AR_ENV).is_none_or(|value| value.is_empty())
        && let Some(archiver) = discover_wasm_ar(&clang)
    {
        println!("+ {WASM_AR_ENV}={}", archiver.display());
        spec = spec.env(WASM_AR_ENV, archiver.as_os_str());
    }
    Ok(spec)
}

fn discover_wasm_clang() -> Result<PathBuf> {
    discover_wasm_clang_from(wasm_clang_candidates())
}

fn wasm_clang_candidates() -> Vec<PathBuf> {
    let mut candidates: Vec<PathBuf> = Vec::new();
    if let Some(explicit) = env::var_os("CC").filter(|value| !value.is_empty()) {
        candidates.push(PathBuf::from(explicit));
    }
    candidates.push(PathBuf::from("clang"));
    if let Some(prefix) = homebrew_llvm_prefix() {
        candidates.push(prefix.join("bin/clang"));
    }
    candidates.push(PathBuf::from("/opt/homebrew/opt/llvm/bin/clang"));
    candidates.push(PathBuf::from("/usr/local/opt/llvm/bin/clang"));
    candidates
}

fn discover_wasm_clang_from(candidates: Vec<PathBuf>) -> Result<PathBuf> {
    discover_wasm_clang_with(candidates, clang_targets_wasm32)
}

fn discover_wasm_clang_with(
    candidates: Vec<PathBuf>,
    targets_wasm32: impl Fn(&Path) -> bool,
) -> Result<PathBuf> {
    let mut seen: Vec<PathBuf> = Vec::new();
    for candidate in candidates {
        if seen.contains(&candidate) {
            continue;
        }
        seen.push(candidate.clone());
        if targets_wasm32(&candidate) {
            return Ok(candidate);
        }
    }

    Err(anyhow!(
        "No clang with a wasm32 backend was found, so cc-rs cannot compile the C dependencies \
         for wasm32-unknown-unknown.\nTried: {}\nOn macOS the Xcode clang has no wasm32 target; \
         install a full LLVM and retry:\n    brew install llvm\nOr point the build at one \
         explicitly:\n    export {WASM_CC_ENV}=/path/to/clang\n    export {WASM_AR_ENV}=/path/to/llvm-ar",
        seen.iter()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", "),
    ))
}

fn discover_wasm_ar(clang: &Path) -> Option<PathBuf> {
    if let Some(dir) = clang.parent().filter(|dir| !dir.as_os_str().is_empty()) {
        let sibling = dir.join("llvm-ar");
        if sibling.is_file() {
            return Some(sibling);
        }
    }
    let path_ar = PathBuf::from("llvm-ar");
    command_succeeds(CommandSpec::new(&path_ar).arg("--version")).then_some(path_ar)
}

fn homebrew_llvm_prefix() -> Option<PathBuf> {
    let output = std::process::Command::new("brew")
        .args(["--prefix", "llvm"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let prefix = String::from_utf8(output.stdout).ok()?;
    let prefix = prefix.trim();
    (!prefix.is_empty()).then(|| PathBuf::from(prefix))
}

fn clang_targets_wasm32(clang: &Path) -> bool {
    let Ok(output) = std::process::Command::new(clang)
        .arg("--print-targets")
        .output()
    else {
        return false;
    };
    if !output.status.success() {
        return false;
    }
    String::from_utf8_lossy(&output.stdout).contains("wasm32")
}

fn build_markdown_parser_wasm(app_dir: &Path) -> Result<()> {
    let rust_source_dir = app_dir.join("../packages/markdown_parser/rust");
    let bytes_path =
        app_dir.join("src/features/messaging/utils/markdown/parser/MarkdownParserWasmBytes.ts");
    let target_dir = rust_source_dir.join("target");

    run_command(apply_wasm_c_toolchain(
        CommandSpec::new("cargo")
            .args(["build", "--release", "--target", "wasm32-unknown-unknown"])
            .env("CARGO_TARGET_DIR", target_dir.to_string_lossy().as_ref())
            .current_dir(&rust_source_dir),
    )?)?;

    let wasm_path = target_dir.join("wasm32-unknown-unknown/release/fluxer_markdown_parser.wasm");
    let wasm =
        fs::read(&wasm_path).with_context(|| format!("Failed to read {}", wasm_path.display()))?;
    let content = markdown_wasm_bytes_content(&wasm);
    if let Some(parent) = bytes_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    fs::write(&bytes_path, content)
        .with_context(|| format!("Failed to write {}", bytes_path.display()))
}

fn build_libfluxcore_wasm(app_dir: &Path) -> Result<()> {
    let rust_package_dir = app_dir.join("rust/libfluxcore");
    let out_dir = app_dir.join("pkgs/libfluxcore");
    let wasm_path = out_dir.join("libfluxcore_bg.wasm");
    let previous_wasm_size = file_size(&wasm_path)?;

    fs::create_dir_all(&out_dir)
        .with_context(|| format!("Failed to create {}", out_dir.display()))?;

    let mut build = CommandSpec::new("cargo")
        .args([
            "build",
            "--release",
            "--target",
            "wasm32-unknown-unknown",
            "--manifest-path",
        ])
        .arg(rust_package_dir.join("Cargo.toml"))
        .current_dir(&rust_package_dir);

    if env_bool("FLUXCORE_WASM_SIMD") {
        let rustflags = match env::var("RUSTFLAGS") {
            Ok(value) if !value.trim().is_empty() => format!("{value} -C target-feature=+simd128"),
            _ => "-C target-feature=+simd128".to_string(),
        };
        build = build.env("RUSTFLAGS", rustflags);
    }

    run_command(apply_wasm_c_toolchain(build)?)?;
    let temp = TempDir::new().context("Failed to create libfluxcore wasm-bindgen temp dir")?;
    let bindgen_dir = temp.path().join("bindgen");
    run_wasm_bindgen(
        &rust_package_dir.join("target/wasm32-unknown-unknown/release/libfluxcore.wasm"),
        &bindgen_dir,
        "libfluxcore",
    )?;

    let bindgen_js_path = bindgen_dir.join("libfluxcore.js");
    let bindgen_dts_path = bindgen_dir.join("libfluxcore.d.ts");
    let bindgen_wasm_path = bindgen_dir.join("libfluxcore_bg.wasm");
    let bindgen_wasm_dts_path = bindgen_dir.join("libfluxcore_bg.wasm.d.ts");

    write_with_spdx(
        &out_dir.join("libfluxcore_bindgen.js"),
        &patch_libfluxcore_bindgen_js(
            &fs::read_to_string(&bindgen_js_path)
                .with_context(|| format!("Failed to read {}", bindgen_js_path.display()))?,
        )?,
    )?;
    write_with_spdx(
        &out_dir.join("libfluxcore_bindgen.d.ts"),
        &patch_libfluxcore_bindgen_dts(
            &fs::read_to_string(&bindgen_dts_path)
                .with_context(|| format!("Failed to read {}", bindgen_dts_path.display()))?,
        )?,
    )?;
    fs::copy(&bindgen_wasm_path, &wasm_path).with_context(|| {
        format!(
            "Failed to copy {} to {}",
            bindgen_wasm_path.display(),
            wasm_path.display()
        )
    })?;
    fs::copy(
        &bindgen_wasm_dts_path,
        out_dir.join("libfluxcore_bg.wasm.d.ts"),
    )
    .with_context(|| format!("Failed to copy {}", bindgen_wasm_dts_path.display()))?;

    fs::write(
        out_dir.join("libfluxcore.js"),
        libfluxcore_index_js_content(),
    )
    .with_context(|| {
        format!(
            "Failed to write {}",
            out_dir.join("libfluxcore.js").display()
        )
    })?;
    fs::write(
        out_dir.join("libfluxcore.d.ts"),
        libfluxcore_index_dts_content(),
    )
    .with_context(|| {
        format!(
            "Failed to write {}",
            out_dir.join("libfluxcore.d.ts").display()
        )
    })?;
    fs::write(
        out_dir.join("package.json"),
        libfluxcore_package_json_content(),
    )
    .with_context(|| format!("Failed to write {}", out_dir.join("package.json").display()))?;
    fs::write(out_dir.join("README.md"), libfluxcore_readme_content())
        .with_context(|| format!("Failed to write {}", out_dir.join("README.md").display()))?;

    let wasm_size = check_wasm_size_budget(&wasm_path, LIBFLUXCORE_WASM_SIZE_BUDGET_BYTES)?;

    let size_comparison = match previous_wasm_size {
        Some(previous) => format!("{} -> {}", format_bytes(previous), format_bytes(wasm_size)),
        None => "no previous artifact".to_string(),
    };
    println!(
        "libfluxcore_bg.wasm size: {size_comparison} (budget {})",
        format_bytes(LIBFLUXCORE_WASM_SIZE_BUDGET_BYTES)
    );

    Ok(())
}

fn run_wasm_bindgen(wasm: &Path, out_dir: &Path, out_name: &str) -> Result<()> {
    let wasm_bindgen = ensure_wasm_bindgen_cli()?;
    fs::create_dir_all(out_dir)
        .with_context(|| format!("Failed to create {}", out_dir.display()))?;
    run_command(
        CommandSpec::new(wasm_bindgen)
            .args(["--target", "web", "--out-dir"])
            .arg(out_dir)
            .args(["--out-name", out_name])
            .arg(wasm),
    )
}

fn check_wasm_size_budget(path: &Path, budget: u64) -> Result<u64> {
    let size =
        file_size(path)?.ok_or_else(|| anyhow!("wasm build did not emit {}", path.display()))?;
    ensure!(
        size <= budget,
        "{} is {}, over the {} budget",
        path.file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_default(),
        format_bytes(size),
        format_bytes(budget)
    );
    Ok(size)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LibfluxwebpVariant {
    Simd,
    Scalar,
}

impl LibfluxwebpVariant {
    fn name(self) -> &'static str {
        match self {
            Self::Simd => "simd",
            Self::Scalar => "scalar",
        }
    }

    fn installed_wasm_name(self) -> &'static str {
        match self {
            Self::Simd => "libfluxwebp_simd_bg.wasm",
            Self::Scalar => "libfluxwebp_bg.wasm",
        }
    }

    fn size_budget(self) -> u64 {
        match self {
            Self::Simd => LIBFLUXWEBP_SIMD_WASM_SIZE_BUDGET_BYTES,
            Self::Scalar => LIBFLUXWEBP_SCALAR_WASM_SIZE_BUDGET_BYTES,
        }
    }

    fn cflags(self, crate_dir: &Path) -> String {
        let shim = crate_dir.join("shim");
        match self {
            Self::Simd => format!(
                "-isystem {} -isystem {} -msimd128 -DEMSCRIPTEN -D__SSE__ -D__SSE2__ \
                 -DWEBP_HAVE_SSE2 -DNDEBUG -O3",
                shim.display(),
                crate_dir.join("simd").display()
            ),
            Self::Scalar => format!("-isystem {} -DNDEBUG -Oz", shim.display()),
        }
    }
}

fn build_libfluxwebp_wasm(app_dir: &Path) -> Result<()> {
    let crate_dir = app_dir.join("rust/libfluxwebp");
    let crate_dir = fs::canonicalize(&crate_dir)
        .with_context(|| format!("Failed to resolve {}", crate_dir.display()))?;
    let out_dir = app_dir.join("pkgs/libfluxwebp");
    let temp = TempDir::new().context("Failed to create libfluxwebp wasm-bindgen temp dir")?;

    let mut bindgen_dirs = Vec::new();
    for variant in [LibfluxwebpVariant::Simd, LibfluxwebpVariant::Scalar] {
        let target_dir = crate_dir.join("target").join(variant.name());
        let mut build = CommandSpec::new("cargo")
            .args([
                "build",
                "--release",
                "--locked",
                "--target",
                "wasm32-unknown-unknown",
            ])
            .env("CARGO_TARGET_DIR", target_dir.as_os_str())
            .env(WASM_CFLAGS_ENV, variant.cflags(&crate_dir))
            .current_dir(&crate_dir);
        build = match variant {
            LibfluxwebpVariant::Simd => {
                build.env(WASM_TARGET_RUSTFLAGS_ENV, "-C target-feature=+simd128")
            }
            LibfluxwebpVariant::Scalar => build.env_remove(WASM_TARGET_RUSTFLAGS_ENV),
        };
        run_command(apply_wasm_c_toolchain(build)?)?;

        let bindgen_dir = temp.path().join(variant.name());
        run_wasm_bindgen(
            &target_dir.join("wasm32-unknown-unknown/release/libfluxwebp.wasm"),
            &bindgen_dir,
            "libfluxwebp",
        )?;
        let wasm_path = bindgen_dir.join("libfluxwebp_bg.wasm");
        let wasm = fs::read(&wasm_path)
            .with_context(|| format!("Failed to read {}", wasm_path.display()))?;
        ensure_wasm_imports_only_from(&wasm, LIBFLUXWEBP_WASM_IMPORT_MODULE)
            .with_context(|| format!("libfluxwebp {} wasm import gate", variant.name()))?;
        bindgen_dirs.push((variant, bindgen_dir));
    }

    let (_, reference_dir) = &bindgen_dirs[0];
    for file in [
        "libfluxwebp.js",
        "libfluxwebp.d.ts",
        "libfluxwebp_bg.wasm.d.ts",
    ] {
        let reference =
            fs::read(reference_dir.join(file)).with_context(|| format!("Failed to read {file}"))?;
        for (variant, dir) in &bindgen_dirs[1..] {
            let other =
                fs::read(dir.join(file)).with_context(|| format!("Failed to read {file}"))?;
            ensure!(
                other == reference,
                "libfluxwebp {} glue {file} differs from the simd build",
                variant.name()
            );
        }
    }

    fs::create_dir_all(&out_dir)
        .with_context(|| format!("Failed to create {}", out_dir.display()))?;
    write_with_spdx(
        &out_dir.join("libfluxwebp.js"),
        &fs::read_to_string(reference_dir.join("libfluxwebp.js"))
            .context("Failed to read libfluxwebp.js")?,
    )?;
    write_with_spdx(
        &out_dir.join("libfluxwebp.d.ts"),
        &fs::read_to_string(reference_dir.join("libfluxwebp.d.ts"))
            .context("Failed to read libfluxwebp.d.ts")?,
    )?;
    for installed in ["libfluxwebp_bg.wasm.d.ts", "libfluxwebp_simd_bg.wasm.d.ts"] {
        fs::copy(
            reference_dir.join("libfluxwebp_bg.wasm.d.ts"),
            out_dir.join(installed),
        )
        .with_context(|| format!("Failed to install {installed}"))?;
    }

    for (variant, dir) in &bindgen_dirs {
        let installed = out_dir.join(variant.installed_wasm_name());
        let previous = file_size(&installed)?;
        fs::copy(dir.join("libfluxwebp_bg.wasm"), &installed)
            .with_context(|| format!("Failed to install {}", installed.display()))?;
        let size = check_wasm_size_budget(&installed, variant.size_budget())?;
        let size_comparison = match previous {
            Some(previous) => format!("{} -> {}", format_bytes(previous), format_bytes(size)),
            None => "no previous artifact".to_string(),
        };
        println!(
            "{} size: {size_comparison} (budget {})",
            variant.installed_wasm_name(),
            format_bytes(variant.size_budget())
        );
    }

    Ok(())
}

fn ensure_wasm_imports_only_from(wasm: &[u8], allowed: &str) -> Result<()> {
    let foreign: Vec<String> = wasm_import_modules(wasm)?
        .into_iter()
        .filter(|module| module != allowed)
        .collect();
    ensure!(
        foreign.is_empty(),
        "wasm imports from {} instead of only {allowed}; a C dependency probably references a \
         libc symbol the shim does not provide",
        foreign.join(", ")
    );
    Ok(())
}

fn wasm_import_modules(wasm: &[u8]) -> Result<Vec<String>> {
    ensure!(
        wasm.len() >= 8 && wasm[..4] == *b"\0asm",
        "not a wasm module"
    );
    let mut reader = WasmReader {
        bytes: wasm,
        pos: 8,
    };
    let mut modules = Vec::new();
    while reader.pos < wasm.len() {
        let id = reader.byte()?;
        let size = reader.leb()? as usize;
        let end = reader
            .pos
            .checked_add(size)
            .filter(|end| *end <= wasm.len())
            .ok_or_else(|| anyhow!("wasm section {id} overruns the module"))?;
        if id == 2 {
            let count = reader.leb()?;
            for _ in 0..count {
                let module = reader.name()?;
                reader.name()?;
                reader.import_desc()?;
                if !modules.contains(&module) {
                    modules.push(module);
                }
            }
        }
        reader.pos = end;
    }
    Ok(modules)
}

struct WasmReader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl WasmReader<'_> {
    fn byte(&mut self) -> Result<u8> {
        let value = *self
            .bytes
            .get(self.pos)
            .ok_or_else(|| anyhow!("wasm module ends early"))?;
        self.pos += 1;
        Ok(value)
    }

    fn leb(&mut self) -> Result<u64> {
        let mut value = 0u64;
        for shift in (0..64).step_by(7) {
            let byte = self.byte()?;
            value |= u64::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                return Ok(value);
            }
        }
        Err(anyhow!("wasm LEB128 value is too long"))
    }

    fn name(&mut self) -> Result<String> {
        let len = self.leb()? as usize;
        let end = self
            .pos
            .checked_add(len)
            .filter(|end| *end <= self.bytes.len())
            .ok_or_else(|| anyhow!("wasm name overruns the module"))?;
        let name = String::from_utf8_lossy(&self.bytes[self.pos..end]).into_owned();
        self.pos = end;
        Ok(name)
    }

    fn limits(&mut self) -> Result<()> {
        let flags = self.byte()?;
        self.leb()?;
        if flags & 1 != 0 {
            self.leb()?;
        }
        Ok(())
    }

    fn import_desc(&mut self) -> Result<()> {
        match self.byte()? {
            0 => {
                self.leb()?;
            }
            1 => {
                self.byte()?;
                self.limits()?;
            }
            2 => self.limits()?,
            3 => {
                self.byte()?;
                self.byte()?;
            }
            4 => {
                self.byte()?;
                self.leb()?;
            }
            kind => return Err(anyhow!("unknown wasm import kind {kind}")),
        }
        Ok(())
    }
}

fn patch_libfluxcore_bindgen_js(content: &str) -> Result<String> {
    const MARKER: &str = "\nasync function __wbg_load(module, imports) {";
    const RESET_EXPORT: &str = r#"
export function __resetLibfluxcoreWasmForMemoryPressure() {
    wasmModule = undefined;
    wasmInstance = undefined;
    wasm = undefined;
    cachedDataViewMemory0 = null;
    cachedUint8ArrayMemory0 = null;
    heap = new Array(1024).fill(undefined);
    heap.push(undefined, null, true, false);
    heap_next = heap.length;
    numBytesDecoded = 0;
}
"#;
    ensure!(
        content.contains(MARKER),
        "libfluxcore wasm-bindgen JS output did not contain reset insertion marker"
    );
    Ok(content.replacen(MARKER, &format!("{RESET_EXPORT}{MARKER}"), 1))
}

fn patch_libfluxcore_bindgen_dts(content: &str) -> Result<String> {
    const MARKER: &str = "\nexport type InitInput";
    const RESET_EXPORT: &str =
        "\nexport function __resetLibfluxcoreWasmForMemoryPressure(): void;\n";
    ensure!(
        content.contains(MARKER),
        "libfluxcore wasm-bindgen DTS output did not contain reset insertion marker"
    );
    Ok(content.replacen(MARKER, &format!("{RESET_EXPORT}{MARKER}"), 1))
}

pub(crate) fn resolve_app_dir() -> Result<PathBuf> {
    let cwd = env::current_dir().context("Failed to resolve current directory")?;
    if cwd.file_name().and_then(|value| value.to_str()) == Some("fluxer_app") {
        return Ok(cwd);
    }
    if cwd.join("fluxer_app").is_dir() {
        return Ok(cwd.join("fluxer_app"));
    }
    Err(anyhow!(
        "Could not resolve fluxer_app directory from {}",
        cwd.display()
    ))
}

fn ensure_wasm_bindgen_cli() -> Result<OsString> {
    let expected = format!("wasm-bindgen {LIBFLUXCORE_WASM_BINDGEN_VERSION}");
    if command_succeeds(CommandSpec::new("wasm-bindgen").arg("--version")) {
        let version = output_text(CommandSpec::new("wasm-bindgen").arg("--version"))?;
        if version.trim() == expected {
            return Ok("wasm-bindgen".into());
        }
    }

    run_command(CommandSpec::new("cargo").args([
        "install",
        "wasm-bindgen-cli",
        "--version",
        LIBFLUXCORE_WASM_BINDGEN_VERSION,
        "--locked",
        "--force",
    ]))?;
    Ok("wasm-bindgen".into())
}

fn file_size(path: &Path) -> Result<Option<u64>> {
    match fs::metadata(path) {
        Ok(metadata) => Ok(Some(metadata.len())),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(error).with_context(|| format!("Failed to stat {}", path.display())),
    }
}

fn format_bytes(bytes: u64) -> String {
    format!("{bytes} B")
}

fn write_with_spdx(path: &Path, content: &str) -> Result<()> {
    fs::write(
        path,
        format!("// SPDX-License-Identifier: AGPL-3.0-or-later\n{content}"),
    )
    .with_context(|| format!("Failed to write {}", path.display()))
}

fn libfluxcore_index_js_content() -> String {
    format!(
        "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
import {{crop_rotate_rgba_raw}} from './libfluxcore_bindgen.js';\n\
{LIBFLUXCORE_WRAPPER_JS}\n\
export * from './libfluxcore_bindgen.js';\n\
export {{default}} from './libfluxcore_bindgen.js';\n"
    )
}

fn libfluxcore_index_dts_content() -> String {
    format!(
        "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export * from './libfluxcore_bindgen.js';\n\
export {{default}} from './libfluxcore_bindgen.js';\n\n\
{LIBFLUXCORE_WRAPPER_DTS}"
    )
}

fn libfluxcore_package_json_content() -> String {
    let manifest = serde_json::json!({
        "name": "libfluxcore",
        "private": true,
        "type": "module",
        "version": "0.0.0",
        "license": "AGPL-3.0-or-later",
        "sideEffects": false,
        "files": [
            "libfluxcore.js",
            "libfluxcore.d.ts",
            "libfluxcore_bindgen.js",
            "libfluxcore_bindgen.d.ts",
            "libfluxcore_bg.wasm",
            "libfluxcore_bg.wasm.d.ts",
            "README.md"
        ],
        "main": "libfluxcore.js",
        "module": "libfluxcore.js",
        "types": "libfluxcore.d.ts",
        "exports": {
            ".": {
                "types": "./libfluxcore.d.ts",
                "default": "./libfluxcore.js"
            },
            "./libfluxcore_bg.wasm": "./libfluxcore_bg.wasm"
        }
    });
    format!("{manifest:#}\n")
}

fn libfluxcore_readme_content() -> &'static str {
    "<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->\n\
# libfluxcore\n\n\
Rust WebAssembly helpers and JavaScript codec wrappers for Fluxer media processing.\n"
}

fn markdown_wasm_bytes_content(wasm: &[u8]) -> String {
    format!(
        "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export const MARKDOWN_PARSER_WASM_BASE64 =\n\
\t'{}';\n",
        BASE64.encode(wasm)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn markdown_wasm_bytes_content_matches_legacy_node_output() {
        assert_eq!(
            markdown_wasm_bytes_content(b"hello"),
            "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export const MARKDOWN_PARSER_WASM_BASE64 =\n\
\t'aGVsbG8=';\n"
        );
    }

    #[test]
    fn libfluxcore_index_reexports_bindgen_module() {
        let content = libfluxcore_index_js_content();
        assert!(content.contains("import {crop_rotate_rgba_raw} from './libfluxcore_bindgen.js';"));
        assert!(content.contains("export * from './libfluxcore_bindgen.js';"));
        assert!(content.contains("export function crop_rotate_rgba("));
    }

    #[test]
    fn libfluxcore_bindgen_js_reset_hook_is_inserted() {
        let content =
            "function __wbg_finalize_init() {}\nasync function __wbg_load(module, imports) {}";
        let patched = patch_libfluxcore_bindgen_js(content).expect("patch should succeed");
        assert!(patched.contains("export function __resetLibfluxcoreWasmForMemoryPressure()"));
        assert!(patched.contains("wasm = undefined;"));
        assert!(patched.contains("async function __wbg_load(module, imports) {}"));
    }

    #[test]
    fn wasm_clang_discovery_reports_an_actionable_remedy_when_nothing_supports_wasm32() {
        let error = discover_wasm_clang_from(vec![
            PathBuf::from("/nonexistent/xcode/clang"),
            PathBuf::from("/nonexistent/xcode/clang"),
            PathBuf::from("/nonexistent/other/clang"),
        ])
        .expect_err("no candidate exists, so discovery must fail");
        let message = error.to_string();

        assert!(message.contains("brew install llvm"), "{message}");
        assert!(message.contains(WASM_CC_ENV), "{message}");
        assert!(message.contains(WASM_AR_ENV), "{message}");
        assert!(message.contains("/nonexistent/other/clang"), "{message}");
        assert_eq!(
            message.matches("/nonexistent/xcode/clang").count(),
            1,
            "duplicate candidates must be reported once: {message}"
        );
    }

    #[test]
    fn wasm_clang_discovery_prefers_a_wasm32_capable_candidate_over_an_earlier_one() {
        let capable = PathBuf::from("/opt/llvm/bin/clang");
        let found = discover_wasm_clang_with(
            vec![PathBuf::from("/nonexistent/clang"), capable.clone()],
            |candidate| candidate == capable,
        )
        .expect("the capable candidate advertises wasm32");
        assert_eq!(found, capable);
    }

    #[test]
    fn wasm_ar_is_taken_from_the_discovered_clang_toolchain() {
        let temp = TempDir::new().expect("temp dir");
        let clang = temp.path().join("clang");
        let archiver = temp.path().join("llvm-ar");
        fs::write(&clang, "").expect("write clang");
        fs::write(&archiver, "").expect("write llvm-ar");

        assert_eq!(discover_wasm_ar(&clang), Some(archiver));
    }

    fn wasm_with_imports(imports: &[(&str, &str)]) -> Vec<u8> {
        let mut section = vec![imports.len() as u8];
        for (module, field) in imports {
            section.push(module.len() as u8);
            section.extend_from_slice(module.as_bytes());
            section.push(field.len() as u8);
            section.extend_from_slice(field.as_bytes());
            section.extend_from_slice(&[0, 0]);
        }
        let mut wasm = b"\0asm\x01\0\0\0".to_vec();
        wasm.extend_from_slice(&[1, 4, 1, 0x60, 0, 0]);
        wasm.push(2);
        wasm.push(section.len() as u8);
        wasm.extend_from_slice(&section);
        wasm.extend_from_slice(&[5, 3, 1, 0, 17]);
        wasm
    }

    #[test]
    fn wasm_import_gate_accepts_a_bindgen_only_module() {
        let wasm = wasm_with_imports(&[
            ("./libfluxwebp_bg.js", "__wbg_new_1"),
            ("./libfluxwebp_bg.js", "__wbindgen_throw"),
        ]);
        assert_eq!(
            wasm_import_modules(&wasm).expect("parse"),
            vec!["./libfluxwebp_bg.js".to_string()]
        );
        ensure_wasm_imports_only_from(&wasm, LIBFLUXWEBP_WASM_IMPORT_MODULE)
            .expect("bindgen imports pass");
    }

    #[test]
    fn wasm_import_gate_rejects_a_libc_import() {
        let wasm = wasm_with_imports(&[
            ("./libfluxwebp_bg.js", "__wbindgen_throw"),
            ("env", "fopen"),
        ]);
        let message = ensure_wasm_imports_only_from(&wasm, LIBFLUXWEBP_WASM_IMPORT_MODULE)
            .expect_err("env import must fail")
            .to_string();
        assert!(message.contains("env"), "{message}");
    }

    #[test]
    fn wasm_import_parser_rejects_truncated_modules() {
        let mut wasm = wasm_with_imports(&[("env", "fopen")]);
        wasm.truncate(wasm.len() - 12);
        assert!(wasm_import_modules(&wasm).is_err());
        assert!(wasm_import_modules(b"\0asm").is_err());
    }

    #[test]
    fn libfluxwebp_cflags_select_sse2_for_simd_and_oz_for_scalar() {
        let crate_dir = Path::new("/repo/fluxer_app/rust/libfluxwebp");
        assert_eq!(
            LibfluxwebpVariant::Simd.cflags(crate_dir),
            "-isystem /repo/fluxer_app/rust/libfluxwebp/shim -isystem \
             /repo/fluxer_app/rust/libfluxwebp/simd -msimd128 -DEMSCRIPTEN -D__SSE__ -D__SSE2__ \
             -DWEBP_HAVE_SSE2 -DNDEBUG -O3"
        );
        assert_eq!(
            LibfluxwebpVariant::Scalar.cflags(crate_dir),
            "-isystem /repo/fluxer_app/rust/libfluxwebp/shim -DNDEBUG -Oz"
        );
    }

    #[test]
    fn libfluxcore_bindgen_dts_reset_hook_is_inserted() {
        let content = "export function is_animated_image(input: Uint8Array): boolean;\nexport type InitInput = RequestInfo;";
        let patched = patch_libfluxcore_bindgen_dts(content).expect("patch should succeed");
        assert!(
            patched.contains("export function __resetLibfluxcoreWasmForMemoryPressure(): void;")
        );
        assert!(patched.contains("export type InitInput = RequestInfo;"));
    }
}
