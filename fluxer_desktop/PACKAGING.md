# Packaging the Fluxer desktop app

This page is for people who build the Fluxer desktop app from source for a Linux distribution, such as nixpkgs, Flathub, the AUR or a Debian-style archive. It lists every dependency, the system libraries each native module links against, which modules the app can start without, and the one command that builds the app with no network access.

Our own release pipeline is in `tools/ci` and does more than a distribution needs. You don't have to run or read it. [What our CI does that you can skip](#what-our-ci-does-that-you-can-skip) lists the extra steps.

## What you build

The desktop app is an Electron app that bundles the Fluxer web app. A desktop build therefore also builds the web app in `fluxer_app`, including three small Rust crates compiled to WebAssembly. There's no way to build the desktop app without the web app.

Linux on x64 and arm64 is supported. We build the macOS and Windows apps ourselves.

## Source and version

Package the stable version our own Linux packages ship. It's `shell.latest_version` in [the stable feed](https://pkgs.fluxer.com/desktop/stable/linux/x64/modules.json), and you can read it with `curl -fsS https://pkgs.fluxer.com/desktop/stable/linux/x64/modules.json | jq -r .shell.latest_version`. Build that version from the tag `fluxer-desktop-stable@<version>`, which points at its source commit.

Don't take the newest `fluxer-desktop-stable@` tag instead. We also tag releases that only update the web app inside our own installs, and builds we hold back, and neither of them is a version we shipped as a desktop app. Please package stable, because canary comes out several times a day.

This page and the build steps on it apply from the first stable release whose tag contains this file. Older tags need our patched Electron and don't have the `package:dir` script.

The version isn't stored in the source tree, and you pass it to the build as `BUILD_VERSION`. The build doesn't look at git metadata, and a tarball of the tag works the same as a checkout.

## Toolchain

| Tool | Version | Used for |
| --- | --- | --- |
| Node.js | 26 | Everything in JavaScript |
| pnpm | 11, ideally the version in `packageManager` in the root `package.json` | Installing JavaScript dependencies |
| Rust | 1.98.1 in our CI, with the `wasm32-unknown-unknown` target | The native modules, `fluxer-ci` and the WebAssembly crates |
| wasm-bindgen-cli | The `wasm-bindgen` version in the root `Cargo.lock` (0.2.128 at the time of writing) | Generating JavaScript bindings for the WebAssembly crates |
| clang that can target wasm32, plus llvm-ar | Any recent LLVM | The C code inside the WebAssembly crates (zstd and libwebp) |
| libclang | Any recent LLVM | bindgen in the PipeWire, libfido2 and hunspell crates |
| C and C++ compiler | GCC or clang | Bundled SQLite and hunspell, and ring |
| pkg-config | | Finding PipeWire, libfido2 and libudev |

The build looks for a wasm32-capable clang on its own. If your default clang can't target wasm32, set `CC_wasm32_unknown_unknown` and `AR_wasm32_unknown_unknown` to a clang and llvm-ar that can.

If `wasm-bindgen` on `PATH` has a different version, the build tries `cargo install wasm-bindgen-cli`, which fails without network. Put the matching version on `PATH` first.

These are the development headers you need, with the Debian and Fedora packages that have them.

| Library | Debian | Fedora |
| --- | --- | --- |
| PipeWire 0.3.65 or newer | `libpipewire-0.3-dev` | `pipewire-devel` |
| libfido2 | `libfido2-dev` | `libfido2-devel` |
| libudev | `libudev-dev` | `systemd-devel` |

You don't need python3, readelf, fpm, rpm or the AppImage tools for the build described here. Our CI uses them for the patched Electron check and the release packages.

## Fetching dependencies

Everything below needs network once, and after that the build runs offline.

Two pnpm settings matter for an offline build. Set both in the environment for every pnpm command, including the ones the build runs for you.

| Setting | Why |
| --- | --- |
| `pnpm_config_pm_on_fail=ignore` | The root `package.json` has an exact pnpm version in `packageManager`. Without this, a pnpm of another version tries to download that one, which fails offline. |
| `pnpm_config_trust_lockfile=true` | `pnpm-workspace.yaml` sets `minimumReleaseAge` and `trustPolicy`. pnpm checks the lockfile against them with registry metadata, and a fresh offline install has none, so it stops with `ERR_PNPM_NO_OFFLINE_META`. The check already ran when you fetched the dependencies. |

nixpkgs' `pnpmConfigHook` sets both for pnpm 11.

JavaScript dependencies come from `pnpm-lock.yaml`. The desktop app doesn't depend on `fluxer_app` as a package, and you need both filters.

```sh
pnpm install --frozen-lockfile --filter 'fluxer_desktop...' --filter 'fluxer_app...'
```

With `ELECTRON_SKIP_BINARY_DOWNLOAD=1` the `electron` package doesn't download its own binary, because you provide Electron yourself.

Rust dependencies come from one lockfile, `Cargo.lock` in the repository root. It has no git dependencies, and every crate comes from crates.io. The root workspace holds `fluxer-ci`, the three WebAssembly crates, every native module and our server crates. The desktop build only compiles the crates it needs, so the server crates are in the lockfile but never built.

Every cargo build in the desktop build uses `--locked`. For an offline build, vendor the crates from that one lockfile or fill the cargo cache beforehand (for example with `cargo fetch --locked` in the repository root) and set `CARGO_NET_OFFLINE=true`.

In nixpkgs, write the cargo source replacement for the vendored crates yourself. `rustPlatform.cargoSetupHook` also sets `ZSTD_SYS_USE_PKG_CONFIG=1`, which makes the WebAssembly build link the system zstd and fail, and `LIBSQLITE3_SYS_USE_PKG_CONFIG=1`, which swaps the SQLite that the app-store module bundles for the system one.

## Building

```sh
export BUILD_CHANNEL=stable
export BUILD_VERSION=<version>
export NODE_ENV=production
export FLUXER_OFFLINE=1
export FLUXER_ALLOW_STOCK_ELECTRON=1
export FLUXER_LAUNCH_COMMAND=fluxer
export ELECTRON_SKIP_BINARY_DOWNLOAD=1
export CARGO_NET_OFFLINE=true
export pnpm_config_pm_on_fail=ignore
export pnpm_config_trust_lockfile=true

pnpm install --offline --frozen-lockfile --filter 'fluxer_desktop...' --filter 'fluxer_app...'
pnpm --filter fluxer_desktop package:dir -c.electronDist=/path/to/electron -c.electronVersion=44.10.0
```

The result is in `fluxer_desktop/dist-electron/linux-unpacked/`, or `linux-arm64-unpacked/` on arm64. The executable is `fluxer` for stable, `fluxer-canary` for canary.

`package:dir` runs `node scripts/build.mjs` and then `electron-builder --dir`. The build script runs these steps in order.

1. Builds `tools/ci` once without its S3 and HTTP features. Set `FLUXER_CI_BIN` to a prebuilt `fluxer-ci` binary to skip this step, for example when you build it as its own package.
2. Builds the 11 Linux native modules with cargo and copies each `.node` file into `node_modules`.
3. Builds the web app with `pnpm --filter fluxer_app build:desktop`. That compiles the three WebAssembly crates, generates some TypeScript, compiles the translation catalogs and runs rspack. None of these steps uses the network. The WebAssembly step runs `bash tools/ci/run.sh`, so `bash` has to be on `PATH`, and nothing in the build needs `/usr/bin/env`.
4. Copies the web app into `fluxer_desktop/dist/renderer` and bundles the main process, the preload scripts and the account engine with esbuild.

### Environment variables

| Variable | Default | Meaning |
| --- | --- | --- |
| `BUILD_CHANNEL` | `stable` | `stable`, `canary` or `development`. It picks the app name, app ID, icons, data directory and the instance the app opens. The build passes the same value to the web app. |
| `BUILD_VERSION` | empty | The release version. Without it the app reports a development version. |
| `NODE_ENV` | `development` | Set it to `production` for minified bundles and release native builds. |
| `FLUXER_OFFLINE` | off | `1` turns off the built-in updater and the app's own `.desktop` file, gives the app its own data directory (see [Desktop integration](#desktop-integration)) and turns the WebAssembly size budgets into warnings. Distribution builds should set it. |
| `FLUXER_LAUNCH_COMMAND` | empty | The command your package installs, such as `fluxer` or `/usr/bin/fluxer`. The app writes it into the autostart entry when the user turns on start at login. Without it the app writes the path of the running Electron binary, which is wrong when you start a system Electron with a wrapper, and goes stale when that path changes with each update. A plain command name is looked up on `PATH`. |
| `FLUXER_ALLOW_STOCK_ELECTRON` | off | `1` lets electron-builder package any Electron with the same major version as ours. See [Electron](#electron). |
| `FLUXER_SYSTEM_LIBS` | off | `1` stops the build from copying libfido2 and its dependencies next to the webauthn module, so it uses the system libraries. |
| `FLUXER_CI_BIN` | empty | Path to a prebuilt `fluxer-ci`. |
| `ELECTRON_ARCH` | host | `x64` or `arm64` for a cross build. |
| `CC_wasm32_unknown_unknown`, `AR_wasm32_unknown_unknown` | found automatically | The C compiler and archiver for the WebAssembly crates. |

The WebAssembly crates have size budgets of 300 KiB, 672 KiB and 288 KiB. Our CI stops when a crate goes over. With `FLUXER_OFFLINE=1` the build only prints a warning, so a different Rust or LLVM can't break your package this way.

To build the web app as a separate package, run `pnpm --filter fluxer_app build:desktop` there, copy `fluxer_app/dist` to `fluxer_desktop/dist/renderer` and call `node scripts/build.mjs --use-shared-renderer`, which skips the web app build.

## Electron

The desktop app uses Electron 44, and the exact version is in `fluxer_desktop/package.json`. Pass your Electron with `-c.electronDist=<directory>` and `-c.electronVersion=<version>`.

You can install the whole `linux-unpacked` directory, or only its `resources` directory and start a system Electron with a wrapper. With a wrapper, start Electron with the path to `resources/app.asar` as its first argument and set `ELECTRON_FORCE_IS_PACKAGED=1`. Electron otherwise treats the app as a development run, and the app then skips its native module check, `fluxer://` registration and other packaged-only behaviour. The app finds its icons and other resources next to `app.asar`, so they don't have to be inside your Electron directory.

Our own Linux packages use Electron with one Chromium patch, `electron/patches/fix_synchronize_vaapi_surfaces_before_output.patch`. When Chromium can't synchronize a VA-API video surface before showing it, the patch makes the hardware decoder report an error, and Chromium then falls back to software decoding. Without `FLUXER_ALLOW_STOCK_ELECTRON=1`, Linux packaging refuses to run without the patched Electron, which our CI downloads from our GitHub release.

With `FLUXER_ALLOW_STOCK_ELECTRON=1` you can use any Electron 44, such as nixpkgs' `electron_44`. The build checks the major version in the `version` file of your Electron directory and stops if it doesn't match. Setting both `FLUXER_ALLOW_STOCK_ELECTRON` and `FLUXER_ELECTRON_DIST` is an error. If you want the patch, `electron/build-linux.sh` rebuilds our Electron, but that needs about 300 GiB of disk and 32 to 48 GiB of memory.

## Native modules

Each module is a Rust crate in `fluxer_desktop/native/<module>`, built with napi-rs and cargo. The app checks the modules at startup. When a module marked "required" fails to load, the app stops at startup. Modules marked "optional" only turn off their feature.

| Module | Feature | Links against | Build needs | At startup |
| --- | --- | --- | --- | --- |
| `linux-audio-capture` | Screen share audio | `libpipewire-0.3.so.0` | PipeWire 0.3.65 or newer headers, libclang | Required |
| `linux-portals` | Screen capture and global shortcuts through xdg-desktop-portal | Nothing extra (pure Rust D-Bus and X11) | | Required |
| `linux-notifications` | Desktop notifications | Nothing extra (pure Rust D-Bus) | | Required |
| `linux-evdev` | Push-to-talk on input devices | `libudev.so.1` | libudev headers | Required |
| `linux-input-hook` | Push-to-talk keys on X11 | Nothing extra (pure Rust X11) | | Required |
| `system-hunspell` | Spellcheck with system dictionaries | `libstdc++.so.6` (hunspell is built from the crate's own source) | C++ compiler, libclang | Required |
| `platform-info` | GPU and system information | Nothing extra | | Required |
| `webauthn` | Security keys | `libfido2.so.1`, and through it `libcbor`, `libcrypto`, `libz` and `libudev` | libfido2 headers, libclang | Required |
| `app-store` | Local storage for app state | Nothing extra (bundled SQLite) | C compiler | Optional |
| `gateway-socket` | Native connection to the Fluxer gateway | Nothing extra (rustls and ring) | C compiler | Optional |
| `hardware-encoder` | NVIDIA video encoding | Nothing at link time. It opens `libcuda.so.1` and `libnvidia-encode.so.1` with dlopen when they're installed, so the driver's library directory has to be on the search path. On NixOS that's `/run/opengl-driver/lib`, which `addDriverRunpath` adds. | | Optional |

By default the build copies libfido2 and its dependencies next to `webauthn.linux-<arch>-gnu.node` and sets its runpath to `$ORIGIN`. Our own packages need that to run on older distributions. With `FLUXER_SYSTEM_LIBS=1` the build skips the copy, and the module uses the system libraries through the normal search path or your runpath.

`FLUXER_SKIP_NATIVE=true` skips all native modules. Use it only for debugging, because the result won't start on Linux.

At runtime the modules also need a D-Bus session bus, xdg-desktop-portal for screen capture and global shortcuts, and a running PipeWire for screen share audio. `gateway-socket` uses the system CA certificates, and `SSL_CERT_FILE` and `SSL_CERT_DIR` work as usual.

`system-hunspell` looks for dictionaries in `HUNSPELL_DICT_DIR`, `$XDG_DATA_HOME/hunspell`, `hunspell` under each `$XDG_DATA_DIRS` entry, `/usr/share/hunspell`, `/usr/share/myspell/dicts` and `/usr/share/myspell`. When none of them has a dictionary for the language, the app uses its own copy, which it either bundles or downloads.

## Libraries Electron opens at runtime

Electron and Chromium open these with dlopen. When one is missing, the app still starts and only that feature is off.

| Library | Feature |
| --- | --- |
| libnotify | Notifications through Chromium |
| libappindicator or libayatana-appindicator, with libdbusmenu-gtk3 | Tray icon |
| libsecret | Encrypting local app data with the system keyring |
| libva and a VA-API driver | Hardware video decoding |
| libpulse | Audio on systems without the PipeWire Pulse server |
| speech-dispatcher | Text to speech |

Our deb and rpm packages also depend on GTK 3, NSS, libXScrnSaver, libXtst, at-spi2, libuuid and xdg-utils, which every Electron app needs.

## Prebuilt files in the source tree

A few binary files are in the repository or come from npm, and the web app loads them as data.

| File | Where it comes from | Feature |
| --- | --- | --- |
| `fluxer_app/src/features/voice/utils/noise_suppression/deepfilternet3/df_bg.wasm` and `DeepFilterNet3_onnx.tar.gz` | DeepFilterNet 3 | Noise suppression |
| `fluxer_app/src/features/voice/utils/camera-effects/models/selfie_segmentation_256x256.onnx` | MediaPipe selfie segmentation | Camera background effects |
| `hunspell-asm` from npm | hunspell compiled with emscripten | Spellcheck fallback |

The two `.wasm` files are prebuilt bytecode that the app runs, so in nixpkgs terms the package is `fromSource` and `binaryBytecode`. The ONNX files are model weights.

## Desktop integration

A build with `FLUXER_OFFLINE=1` doesn't check for updates and doesn't write its own `.desktop` file to `~/.local/share/applications`. Install a desktop entry yourself with the ID the app looks for.

| Channel | Desktop entry | Icons |
| --- | --- | --- |
| stable | `app.fluxer.FluxerDesktop.desktop` | `fluxer_desktop/build_resources/icons-stable/<size>x<size>.png`, for example `128x128.png` |
| canary | `app.fluxer.FluxerDesktopCanary.desktop` | `fluxer_desktop/build_resources/icons-canary/<size>x<size>.png` |

The entry needs `MimeType=x-scheme-handler/fluxer;` so `fluxer://` links open the app, and the app registers itself as the handler only when that entry exists. The icons come in 16, 24, 32, 48, 64, 128, 256, 512 and 1024 px, which you can install into the hicolor theme without resizing. The icon name the app uses is `fluxer` for stable and `fluxer-canary` for canary.

`fluxer_desktop/packaging/linux/app.fluxer.Fluxer.desktop` and the matching metainfo files are for our Flatpak, which uses a different app ID. Use them as a template if you like, but install the entry under the desktop entry ID above.

When you repack one of our AppImage or tar.gz releases, which aren't offline builds, set `FLUXER_DISABLE_DESKTOP_FILE=1` at runtime to stop the app from writing its own `.desktop` file.

An offline build keeps its data in its own directory, `~/.config/fluxer-distro` for stable and `~/.config/fluxercanary-distro` for canary. Our own packages use `~/.config/fluxer` and update the web app inside it, so a distribution build that lags behind would otherwise open data written by a newer version. Users who switch from our packages to yours sign in again.

If a user also runs our AppImage or tar.gz, that copy writes `~/.local/share/applications/app.fluxer.FluxerDesktop.desktop`, which hides your system entry with the same ID, and `fluxer://` links then open our copy.

## What our CI does that you can skip

- Working out the CalVer version and source commit, and stamping `package.json`.
- Building PipeWire 0.3.65 headers and libfido2 1.16.0 from source, which only exists so our packages run on Ubuntu 22.04's glibc.
- Building the web app once and handing it to every platform job.
- Splitting the web app into downloadable modules, pruning the bundled copy and downloading spellcheck dictionaries for the module feed.
- Downloading our patched Electron.
- Building AppImage, deb, rpm and tar.gz packages, checking glibc compatibility, checksums, signing, notarization and publishing.
