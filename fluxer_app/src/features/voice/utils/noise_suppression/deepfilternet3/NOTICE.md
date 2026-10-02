# DeepFilterNet3 licenses

The files in this directory are third-party DeepFilterNet3 WASM/model assets,
including derived Mezon noise suppression distribution artifacts. The app build
emits them as content-hashed assets.

They are distributed under MIT or Apache-2.0 terms. The copied license texts are
available in `LICENSE-MIT` and `LICENSE-APACHE`.

| File | Source | Changes |
| --- | --- | --- |
| `df_bg.wasm` | libDF WASM build shipped by `deepfilternet3-noise-filter` 1.3.0 | One constant changed: `max_db_df_thresh` 20 to 30. SHA-256 `c42dcfa05c64ff21d23fe3e3a35e934323e4fd85c303b6a9f8a21bed05f7fb74`. |
| `DeepFilterNet3_onnx.tar.gz` | DeepFilterNet3 ONNX model | Unmodified. SHA-256 `c94d91f70911001c946e0fabb4aa9adc37045f45a03b56008cb0c8244cb63616`. |
| `../deepFilterProcessor.worklet.js` | AudioWorklet processor and wasm-bindgen 0.2.126 glue from `deepfilternet3-noise-filter` 1.3.0 | Rewritten around the same bindings: mono input, input sanitising, high-pass filter, ready, error and health messages. |

No Fluxer license notice grants rights to third-party trademarks or brand names.
