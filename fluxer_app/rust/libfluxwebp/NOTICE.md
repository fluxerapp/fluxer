# libfluxwebp licenses

`libfluxwebp_bg.wasm` and `libfluxwebp_simd_bg.wasm` are built from this crate
and bundled into the app. They contain third-party code that keeps its upstream
license.

| Component | Source | License |
| --- | --- | --- |
| libwebp 1.6.0 | Vendored by the `libwebp-sys` 0.14.4 crate | BSD-3-Clause, see `LICENSE-LIBWEBP.txt`, with the additional patent grant in `PATENTS-LIBWEBP.txt` |
| `libwebp-sys` 0.14.4 | Rust bindings and build script | MIT, see `LICENSE-LIBWEBP-SYS.txt` |
| `simd/xmmintrin.h`, `simd/emmintrin.h` | Emscripten 4.0.15 SSE compatibility headers | MIT or University of Illinois/NCSA, see `simd/LICENSE` |

The SIMD build compiles libwebp's SSE2 code paths through the Emscripten
headers, unmodified. The headers in `shim/` and `src/shim.rs` are Fluxer code.

No Fluxer license notice grants rights to third-party trademarks or brand names.
