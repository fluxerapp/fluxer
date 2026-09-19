---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Desktop builds
description: Paths for desktop installers, checksums and the update feeds each platform reads.
---

Every desktop path below is served from `https://pkgs.fluxer.com` and is built from four coordinates.

| Coordinate | Values |
| --- | --- |
| `channel` | `stable`, `canary` |
| `platform` | `darwin`, `linux`, `win32` |
| `arch` | `x64`, `arm64` |
| `format` | `dmg`, `zip`, `setup`, `portable`, `appimage`, `deb`, `rpm`, `tar_gz` |

A format is only published for the platform it belongs to. `dmg` and `zip` are macOS, `setup` and `portable` are Windows, and the other four are Linux.

## Release metadata

```
GET /desktop/{channel}/{platform}/{arch}/latest
```

`latest` returns a JSON document naming the current version, its publication date, and a per-format URL and SHA256.

## Artifacts

```
GET /desktop/{channel}/{platform}/{arch}/{version}/{format}
GET /desktop/{channel}/{platform}/{arch}/{version}/{format}.sha256
```

`version` is either a published version or the literal `latest`, which resolves to the current one. The `.sha256` sibling holds the checksum for the artifact beside it.

Linux AppImage builds also publish a zsync control file, so `AppImageUpdate`, `AppImageLauncher` and Gear Lever can fetch only the blocks that changed:

```
GET /desktop/{channel}/linux/{arch}/{version}/appimage.zsync
```

## Update feeds

Each platform's updater reads the feed its own framework expects, beside the artifacts:

| Platform | Feed |
| --- | --- |
| macOS | `RELEASES.json`, `releases.json` |
| Windows | `RELEASES`, `releases.win.json`, `assets.win.json` |
| All | `manifest.json`, `version.json`, `latest.json` |
