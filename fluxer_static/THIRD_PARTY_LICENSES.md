# Third-party licenses

This repository mirrors static assets for Fluxer. Fluxer-owned assets are
covered by the root `LICENSE` notice. Third-party assets keep their upstream
licenses and attribution requirements.

| Path | Component | License |
| --- | --- | --- |
| `desktop/spellcheck/dictionaries/` | Hunspell dictionaries packaged as exact `dictionary-*` npm package versions | Varies by language; each dictionary directory includes its own `LICENSE`, and the package summary is in `desktop/spellcheck/dictionaries/NOTICE.md`. |
| `emoji/` | Twemoji graphics from `jdecked/twemoji` | CC-BY-4.0; see `emoji/LICENSE` and `emoji/NOTICE.md`. |
| `marketing/flags/` | Twemoji flag graphics from `jdecked/twemoji` | CC-BY-4.0; see `marketing/flags/LICENSE` and `marketing/flags/NOTICE.md`. |
| `libs/deepfilternet3/` | DeepFilterNet3 WASM/model assets | MIT or Apache-2.0; see the license files and `libs/deepfilternet3/NOTICE.md`. |
| `embeds/icons/hn.webp` | Hacker News brand icon | Third-party brand asset; see `embeds/icons/NOTICE.md`. |

Fonts used to be mirrored here under `fonts/`. They are now bundled by each app
that uses them, and their OFL-1.1 licenses and modification disclosure live with
the binaries in `packages/fonts/` (`LICENSE-IBM-PLEX.txt`,
`NOTICE.md`).

No Fluxer license notice grants rights to third-party trademarks or brand names.
