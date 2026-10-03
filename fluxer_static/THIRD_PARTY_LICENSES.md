# Third-party licenses

This directory mirrors static assets for Fluxer. Fluxer-owned assets are
copyright Fluxer Platform AB and licensed under CC BY-SA 4.0, see `LICENSE`.
Third-party assets keep their upstream licenses and attribution requirements.

| Path | Component | License |
| --- | --- | --- |
| `desktop/spellcheck/dictionaries/` | Hunspell dictionaries packaged as exact `dictionary-*` npm package versions | Varies by language; each dictionary directory includes its own `LICENSE`, and the package summary is in `desktop/spellcheck/dictionaries/NOTICE.md`. |
| `emoji/` | Twemoji graphics from `jdecked/twemoji`. Copyright 2014-2021 Twitter, Inc and other contributors, 2022-present Jason Sofonia, Justine De Caires and other contributors | CC-BY-4.0. See `emoji/LICENSE` and `emoji/NOTICE.md`. |

`fluxer_app` also bundles this third-party artwork outside this directory:

| Path | Component | License |
| --- | --- | --- |
| `fluxer_app/src/media/images/i-like-food.svg` | "I Like Food" pattern from Hero Patterns (https://heropatterns.com) by Steve Schoger | CC-BY-4.0 (https://creativecommons.org/licenses/by/4.0/). |
| `fluxer_app/src/media/images/neko.png`, `fluxer_app/src/features/accessibility/components/NekoSprite.tsx` | Sprite sheet and sprite tables from `adryd325/oneko.js`. Copyright 2022 adryd | MIT (https://github.com/adryd325/oneko.js/blob/main/LICENSE). |
| `fluxer_app/src/features/ui/components/icons/InboxIcon.tsx` | Inbox icon path from Google Material Icons (`google/material-design-icons`). Copyright Google LLC | Apache-2.0 (https://www.apache.org/licenses/LICENSE-2.0). |

Noise suppression WebAssembly binaries and AudioWorklet processors are bundled
by `fluxer_app` from `@sapphi-red/web-noise-suppressor` rather than mirrored
here; their licenses and build-time modification disclosure live with the code in
`fluxer_app/src/features/voice/utils/noise_suppression/NOTICE.md`.

The `libfluxcore` and `libfluxwebp` WebAssembly modules are built by
`fluxer_app` from Zstandard, libwebp and Emscripten's SSE compatibility headers
rather than mirrored here. Their licenses live with the crates in
`fluxer_app/rust/libfluxcore/NOTICE.md` and `fluxer_app/rust/libfluxwebp/NOTICE.md`,
and the app build ships them next to the modules as `assets/libfluxcore-*.txt`
and `assets/libfluxwebp-*.txt`.

Fonts used to be mirrored here under `fonts/`. They are now bundled by each app
that uses them, and their OFL-1.1 licenses and modification disclosure live with
the binaries in `packages/fonts/` (`LICENSE-IBM-PLEX.txt`,
`NOTICE.md`).

No Fluxer license notice grants rights to third-party trademarks or brand names.
