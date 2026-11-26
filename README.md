<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./fluxer_static/marketing/branding/logo-white.svg">
    <img src="./fluxer_static/marketing/branding/logo-color.svg" alt="Fluxer logo" width="400">
  </picture>
</p>

<p align="center">
  <a href="https://fluxer.com/download">
    <img src="https://img.shields.io/badge/Download-fluxer.com-4641D9" alt="Download" /></a>
  <a href="https://docs.fluxer.com">
    <img src="https://img.shields.io/badge/Docs-docs.fluxer.com-blue" alt="Documentation" /></a>
  <a href="https://fluxer.com/donate">
    <img src="https://img.shields.io/badge/Donate-fluxer.com%2Fdonate-brightgreen" alt="Donate" /></a>
  <a href="https://fluxer.com/funding.json">
    <img src="https://img.shields.io/badge/funding.json-fluxer.com%2Ffunding.json-orange" alt="funding.json" /></a>
  <a href="./LICENSE">
    <img src="https://img.shields.io/badge/License-AGPLv3-purple" alt="AGPLv3 License" /></a>
</p>

<p align="center">
  <a href="https://flathub.org/apps/app.fluxer.Fluxer">
    <img src="https://dl.flathub.org/assets/badges/flathub-badge-en.svg" alt="Get it on Flathub" height="60" /></a>
</p>

# Fluxer

> [!IMPORTANT]
> Bug reports and feature requests have moved to [feedback.fluxer.com](https://feedback.fluxer.com). Sign in with your Fluxer account to post, vote and follow updates. GitHub Issues and Discussions are closed. Report security vulnerabilities privately through [fluxer.com/security](https://fluxer.com/security).

Fluxer is a free and open source instant messaging and VoIP chat app built for friends, groups, and communities.

<p align="center">
  <img src="https://fluxer.app/static/img/screenshots-desktop-readme-1920w.9f7c8fdb2760d49b.png" alt="Fluxer running side by side on a desktop monitor and a phone" width="640">
</p>

## Download

| Windows | macOS | Linux | Android | iOS |
| --- | --- | --- | --- | --- |
| [Installer (x64)][win-setup-x64] | [Disk image][mac-dmg] | [Flatpak][flatpak-ref] | [Google Play][android-play] | [Beta paused][ios-beta-paused] |
| [Installer (ARM64)][win-setup-arm64] | | [Flathub][flathub] | [APK][android-apk] | |
| [Portable (x64)][win-portable-x64] | | [deb (x64)][linux-deb-x64] | [Obtainium (most phones)][obtainium-fcm] | |
| [Portable (ARM64)][win-portable-arm64] | | [deb (ARM64)][linux-deb-arm64] | [Obtainium (degoogled phones)][obtainium-oss] | |
| | | [rpm (x64)][linux-rpm-x64] | | |
| | | [rpm (ARM64)][linux-rpm-arm64] | | |
| | | [AppImage (x64)][linux-appimage-x64] | | |
| | | [AppImage (ARM64)][linux-appimage-arm64] | | |
| | | [tar.gz (x64)][linux-targz-x64] | | |
| | | [tar.gz (ARM64)][linux-targz-arm64] | | |

The macOS disk image runs on both Apple silicon and Intel. Windows and Linux need the build matching your processor.

On Linux, add one of our [package repositories][docs-linux] for apt, dnf, pacman or Flatpak so Fluxer updates with the rest of your system. You can also [open Fluxer in a browser](https://fluxer.com/app) with nothing to install.

## Self-hosting

You can run Fluxer on your own server from this repository, as one container, one systemd service or one Kubernetes pod. The [self-hosting guide][docs-selfhost] covers each setup. [rexctl][docs-rexctl] is an optional helper that installs and looks after Fluxer on Docker, Podman or systemd.

## Building from source

[Building from source][docs-build] shows how to build the server's release tarball and container image yourself. To package the desktop app for a Linux distribution, follow [fluxer_desktop/PACKAGING.md](fluxer_desktop/PACKAGING.md). The mobile app has its own repository, [fluxerapp/flutter_client](https://github.com/fluxerapp/flutter_client).

## Contributing

Read the [contributing guide][docs-contributing] before you open a pull request. It explains how to get a change approved, how to use AI tools and what each commit needs. You can also [translate Fluxer on Weblate](https://weblate.fluxer.tools). The [code of conduct][docs-conduct] applies to everything in this repository.

## Roadmap

The [roadmap](https://fluxer.com/roadmap) shows what we're working on and what comes next.

## Governance

[fluxer.com/governance](https://fluxer.com/governance) explains who makes decisions about Fluxer and how maintainers work.

## Funding

Fluxer is funded by the people who use fluxer.com, through optional purchases
and donations, and it has no outside investors. Organizations can
[donate](https://fluxer.com/donate?type=business) or
[fund a piece of Fluxer's open source work](https://fluxer.com/help/fund-fluxer)
with a grant, and every way to fund Fluxer is in our
[funding.json](https://fluxer.com/funding.json).

## License

The source is licensed under the [AGPL-3.0-or-later](./LICENSE) license.

Fluxer artwork, such as the logo, icons, badges and default avatars, is
licensed under [CC BY-SA 4.0](./fluxer_static/LICENSE). Third-party material
keeps its own terms, listed in
[fluxer_static/THIRD_PARTY_LICENSES.md](./fluxer_static/THIRD_PARTY_LICENSES.md).

Use of the Fluxer name and logo is covered by the
[name and marks policy](https://fluxer.com/governance#name-and-marks).

[win-setup-x64]: https://pkgs.fluxer.com/desktop/stable/win32/x64/latest/setup
[win-setup-arm64]: https://pkgs.fluxer.com/desktop/stable/win32/arm64/latest/setup
[win-portable-x64]: https://pkgs.fluxer.com/desktop/stable/win32/x64/latest/portable
[win-portable-arm64]: https://pkgs.fluxer.com/desktop/stable/win32/arm64/latest/portable
[mac-dmg]: https://pkgs.fluxer.com/desktop/stable/darwin/arm64/latest/dmg
[linux-deb-x64]: https://pkgs.fluxer.com/desktop/stable/linux/x64/latest/deb
[linux-deb-arm64]: https://pkgs.fluxer.com/desktop/stable/linux/arm64/latest/deb
[linux-rpm-x64]: https://pkgs.fluxer.com/desktop/stable/linux/x64/latest/rpm
[linux-rpm-arm64]: https://pkgs.fluxer.com/desktop/stable/linux/arm64/latest/rpm
[linux-appimage-x64]: https://pkgs.fluxer.com/desktop/stable/linux/x64/latest/appimage
[linux-appimage-arm64]: https://pkgs.fluxer.com/desktop/stable/linux/arm64/latest/appimage
[linux-targz-x64]: https://pkgs.fluxer.com/desktop/stable/linux/x64/latest/tar_gz
[linux-targz-arm64]: https://pkgs.fluxer.com/desktop/stable/linux/arm64/latest/tar_gz
[flatpak-ref]: https://pkgs.fluxer.com/flatpak/fluxer.flatpakref
[flathub]: https://flathub.org/apps/app.fluxer.Fluxer
[android-play]: https://play.google.com/store/apps/details?id=com.fluxer
[android-apk]: https://fluxer.com/download/android
[obtainium-fcm]: https://apps.obtainium.imranr.dev/redirect?r=obtainium%3A%2F%2Fapp%2F%7B%22id%22%3A%22com.fluxer%22%2C%22url%22%3A%22https%3A%2F%2Fgithub.com%2Ffluxerapp%2Fflutter_client%22%2C%22author%22%3A%22fluxerapp%22%2C%22name%22%3A%22Fluxer%20%28FCM%29%22%2C%22additionalSettings%22%3A%22%7B%5C%22includePrereleases%5C%22%3Afalse%2C%5C%22fallbackToOlderReleases%5C%22%3Atrue%2C%5C%22apkFilterRegEx%5C%22%3A%5C%22stablefcm%5C%22%2C%5C%22autoApkFilterByArch%5C%22%3Atrue%7D%22%7D
[obtainium-oss]: https://apps.obtainium.imranr.dev/redirect?r=obtainium%3A%2F%2Fapp%2F%7B%22id%22%3A%22com.fluxer%22%2C%22url%22%3A%22https%3A%2F%2Fgithub.com%2Ffluxerapp%2Fflutter_client%22%2C%22author%22%3A%22fluxerapp%22%2C%22name%22%3A%22Fluxer%20%28OSS%29%22%2C%22additionalSettings%22%3A%22%7B%5C%22includePrereleases%5C%22%3Afalse%2C%5C%22fallbackToOlderReleases%5C%22%3Atrue%2C%5C%22apkFilterRegEx%5C%22%3A%5C%22stableoss%5C%22%2C%5C%22autoApkFilterByArch%5C%22%3Atrue%7D%22%7D
[ios-beta-paused]: https://fluxer.com/help/ios-testflight
[docs-linux]: https://docs.fluxer.com/downloads/linux-repositories/
[docs-selfhost]: https://docs.fluxer.com/operator/
[docs-rexctl]: https://docs.fluxer.com/operator/with-rexctl/
[docs-build]: https://docs.fluxer.com/operator/build-from-source/
[docs-contributing]: https://docs.fluxer.com/contributing/
[docs-conduct]: https://docs.fluxer.com/code-of-conduct/
