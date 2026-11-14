# libfluxcore licenses

`libfluxcore_bg.wasm` is built from this crate and bundled into the app. It
contains third-party code that keeps its upstream license.

| Component | Source | License |
| --- | --- | --- |
| Zstandard 1.5.7 | Vendored by the `zstd-sys` 2.1.0 crate | BSD-3-Clause, see `LICENSE-ZSTD.txt` (Zstandard is dual licensed, Fluxer uses it under BSD-3-Clause) |
| `zstd-sys` 2.1.0 | Rust bindings, build script and WebAssembly libc shim | BSD-3-Clause, see `LICENSE-ZSTD-SYS.txt` |
| `zstd` 0.14.0 | Rust wrapper | BSD-3-Clause, see `LICENSE-ZSTD-RS.txt` |

No Fluxer license notice grants rights to third-party trademarks or brand names.
