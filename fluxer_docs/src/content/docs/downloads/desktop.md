---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Desktop builds
description: Download paths for Fluxer desktop builds, their SHA-256 checksums and the update feeds.
---

Desktop builds are on `https://pkgs.fluxer.com`, and you can download them without signing in. Each path has a channel (`stable` or `canary`), a platform, an architecture (`x64` or `arm64`) and, for a file, a format from the table below.

| Platform | Path value | Formats |
| --- | --- | --- |
| macOS | `darwin` | `dmg`, `zip` |
| Windows | `win32` | `setup`, `portable` |
| Linux | `linux` | `appimage`, `deb`, `rpm`, `tar_gz` |

## Latest version

```
GET /desktop/{channel}/{platform}/{arch}/latest
```

Returns JSON with the current `version`, its `pub_date` and a `files` object that gives each format's `url` and `sha256`.

## Downloads

```
GET /desktop/{channel}/{platform}/{arch}/{version}/{format}
GET /desktop/{channel}/{platform}/{arch}/{version}/{format}.sha256
GET /desktop/{channel}/linux/{arch}/{version}/appimage.zsync
```

`version` is a release version such as `2026.904.135113`, or `latest` for the newest one. Add `.sha256` to a download path to get the file's SHA-256 checksum. For example, `https://pkgs.fluxer.com/desktop/stable/win32/x64/latest/setup` downloads the newest Windows installer.

AppImage builds also have an `appimage.zsync` file, which AppImage update tools use to download only the blocks that changed.

## Update feeds

The macOS and Windows desktop apps check these files in `/desktop/{channel}/{platform}/{arch}/` for updates.

| Platform | Feed |
| --- | --- |
| macOS | `RELEASES.json`, `releases.json` |
| Windows | `RELEASES`, `releases.win.json`, `assets.win.json` |

On Linux, the AppImage updates itself from `latest` when it's in a folder you can write to. Packages update through the [Linux repositories](/downloads/linux-repositories/), and the tar.gz doesn't update itself.
