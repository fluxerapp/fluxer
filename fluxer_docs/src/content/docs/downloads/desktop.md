---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Desktop builds
description: Download paths for Fluxer desktop builds, their SHA-256 checksums and the files the app checks for updates.
---

The Fluxer desktop builds, their checksums and the files the app checks for updates are public, and you can download all of them without an account.

## Paths

Builds are on `https://pkgs.fluxer.com`. Each path is made of these parts.

| Part | Values |
| --- | --- |
| Channel | `stable` or `canary` |
| Platform | A path value from the table below |
| Architecture | `x64` or `arm64` |
| Format | For a file, a format from the table below |

| Platform | Path value | Formats |
| --- | --- | --- |
| macOS | `darwin` | `dmg`, `zip` |
| Windows | `win32` | `setup`, `portable` |
| Linux | `linux` | `appimage`, `deb`, `rpm`, `tar_gz` |

## Latest version

```
GET /desktop/{channel}/{platform}/{arch}/latest
```

Returns JSON with these fields.

- `version` is the current version.
- `pub_date` is when it was published.
- `files` gives each format's download `url` and `sha256` checksum.

## Downloads

```
GET /desktop/{channel}/{platform}/{arch}/{version}/{format}
GET /desktop/{channel}/{platform}/{arch}/{version}/{format}.sha256
GET /desktop/{channel}/linux/{arch}/{version}/appimage.zsync
```

`version` is a release version such as `2026.904.135113`, or `latest` for the newest one. Add `.sha256` to a download path to get the file's SHA-256 checksum.

This path downloads the latest Windows installer.

```
https://pkgs.fluxer.com/desktop/stable/win32/x64/latest/setup
```

AppImage builds also have an `appimage.zsync` file, which AppImage update tools use to download only the blocks that changed.

## Update files

To find updates, the macOS and Windows desktop apps check these files in `/desktop/{channel}/{platform}/{arch}/`.

| Platform | Files |
| --- | --- |
| macOS | `RELEASES.json`, `releases.json` |
| Windows | `RELEASES`, `releases.win.json`, `assets.win.json` |

On Linux, the AppImage updates itself from `latest` when it's in a folder you can write to. Packages update through the [Linux repositories](/downloads/linux-repositories/), and the tar.gz doesn't update itself.
