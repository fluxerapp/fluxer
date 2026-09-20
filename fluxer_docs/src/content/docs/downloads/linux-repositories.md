---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Linux repositories
description: The apt, dnf, pacman and Flatpak repositories Fluxer publishes, and how a client adds them.
---

Fluxer publishes four Linux repositories. In every one the package is `fluxer` for stable and `fluxer-canary` for canary. The apt and dnf entrypoints each subscribe to one channel, so the file you install decides which of the two you track. pacman and Flatpak work the other way. One repository serves both channels, and the package name or application id selects it.

## apt

One repository serves Debian and Ubuntu. It uses the standard `dists` and `pool` layout, publishes both SHA256 and SHA512 by-hash indexes, and is signed.

```
sudo install -d -m 0755 /etc/apt/keyrings
sudo curl -fsSL -o /etc/apt/keyrings/fluxer-archive-keyring.gpg \
  https://pkgs.fluxer.com/keys/fluxer-archive-keyring.gpg
sudo curl -fsSL -o /etc/apt/sources.list.d/fluxer.sources \
  https://pkgs.fluxer.com/deb/fluxer.sources
sudo apt update && sudo apt install fluxer
```

The `.sources` entry uses `Signed-By` rather than `Trusted: yes`, so `apt update` verifies the repository and prints nothing.

## dnf

One repository serves Fedora and the RHEL family, split by channel and architecture. It is signed, with both `gpgcheck` and `repo_gpgcheck` enabled.

```
sudo curl -fsSL -o /etc/yum.repos.d/fluxer.repo \
  https://pkgs.fluxer.com/rpm/fluxer.repo
sudo dnf install fluxer
```

Metadata expires after six hours, so a freshly published build becomes visible within that window, or immediately with `dnf --refresh upgrade`.

:::caution[RHEL, Rocky, Alma and CentOS Stream need EPEL]
The package depends on `libXScrnSaver`, which the EL base repositories do not ship. Run `sudo dnf install epel-release` first. Fedora does not need this.
:::

## pacman

One repository named `fluxer` holds both channels.

```
sudo tee -a /etc/pacman.conf >/dev/null <<'REPO'

[fluxer]
SigLevel = Never
Server = https://pkgs.fluxer.com/arch/$repo/os/$arch
REPO
sudo pacman -Syu --noconfirm fluxer
```

Install `fluxer-canary` instead for the canary channel. Both come from this one repository and install alongside each other.

Write `$repo` and `$arch` literally. Both are pacman variables, not shell ones, which is why the heredoc above is quoted. `$repo` expands to the section name, so the `Server` line needs no editing.

A pacman sync database records one version per package name, so only the current release is installable by name. An older build is still served, and `curl` followed by `pacman -U ./<file>` installs it.

## Flatpak

One remote named `fluxer` serves both application ids, `app.fluxer.Fluxer` and `app.fluxer.FluxerCanary`.

```
flatpak install https://pkgs.fluxer.com/flatpak/fluxer.flatpakref
```

Use `fluxer-canary.flatpakref` for the canary channel. The reference file names the remote and resolves the runtime the application builds against, so this works on a machine with no remotes configured.

Once the remote exists, either application installs by id:

```
flatpak install fluxer app.fluxer.FluxerCanary
```

No `--no-gpg-verify` flag is required. The repository is unsigned, and flatpak reads that from the repository metadata itself.
