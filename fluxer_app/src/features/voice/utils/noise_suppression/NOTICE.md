# Noise suppression worklet licenses

The noise suppression backends load WebAssembly binaries and AudioWorklet
processors that are bundled into the app from
`@sapphi-red/web-noise-suppressor`. They are third-party artifacts and keep
their upstream licenses.

| Artifact | Component | License |
| --- | --- | --- |
| `rnnoise.wasm`, `rnnoise_simd.wasm` | RNNoise (xiph) built by `@shiguredo/rnnoise-wasm` | BSD-3-Clause (RNNoise), Apache-2.0 (build wrapper) |
| `gtcrn.wasm` | GTCRN compiled with `onnx2c` by `@sapphi-red/gtcrn-wasm`, bundling `pffft` | MIT (GTCRN code and weights), FFTPACK/UCAR (pffft) |
| `speex.wasm` | SpeexDSP preprocessor built by `@sapphi-red/speex-preprocess-wasm` | BSD-3-Clause (Xiph.Org, Valin, Analog Devices, CSIRO, Rowe, EpicGames, Degener/Bormann) |
| `*.worklet.js` | AudioWorklet processors from `@sapphi-red/web-noise-suppressor` | MIT |

The worklet processors are modified at build time by
`fluxer_app/scripts/build/rspack/noise-suppressor-worklet-loader.cjs`, which
makes them pass audio through until their WebAssembly module is ready, start
their message port, and report readiness and initialisation failures.

The Xiph license text ships inside `speex.wasm`'s worklet bundle. No Fluxer
license notice grants rights to third-party trademarks or brand names.
