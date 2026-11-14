---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Linux repositories
description: How to add the apt, dnf, pacman and Flatpak repositories Fluxer publishes.
---

Fluxer publishes apt, dnf, pacman and Flatpak repositories, all signed with the key whose fingerprint is `09D01339EE128925F75E675C855C5BDE34D205D2`. You pick the channel with the entry file on apt and dnf, by package name on pacman and by app ID on Flatpak.

| Package manager | Pick stable with | Pick canary with |
| --- | --- | --- |
| apt | `fluxer.sources` | `fluxer-canary.sources` |
| dnf | `fluxer.repo` | `fluxer-canary.repo` |
| pacman | `fluxer` | `fluxer-canary` |
| Flatpak | `app.fluxer.Fluxer` | `app.fluxer.FluxerCanary` |

## Debian and Ubuntu

```sh
sudo install -d -m 0755 /etc/apt/keyrings
sudo curl -fsSL -o /etc/apt/keyrings/fluxer-archive-keyring.gpg \
  https://pkgs.fluxer.com/keys/fluxer-archive-keyring.gpg
sudo curl -fsSL -o /etc/apt/sources.list.d/fluxer.sources \
  https://pkgs.fluxer.com/deb/fluxer.sources
sudo apt update && sudo apt install fluxer
```

A `.deb` you downloaded only gets updates after you add the entry for its channel. For canary, add the canary entry and install `fluxer-canary`.

```sh
sudo curl -fsSL -o /etc/apt/sources.list.d/fluxer-canary.sources \
  https://pkgs.fluxer.com/deb/fluxer-canary.sources
sudo apt update && sudo apt install fluxer-canary
```

## Fedora and RHEL

RHEL, Rocky, Alma and CentOS Stream need `sudo dnf install epel-release` first, because the package depends on `libXScrnSaver`.

```sh
sudo curl -fsSL -o /etc/yum.repos.d/fluxer.repo \
  https://pkgs.fluxer.com/rpm/fluxer.repo
sudo dnf install fluxer
```

dnf asks you to import the signing key on the first install, once for the repository and once for the package. Check that the fingerprint matches the one above both times.

For canary, add the canary entry and install `fluxer-canary`.

```sh
sudo curl -fsSL -o /etc/yum.repos.d/fluxer-canary.repo \
  https://pkgs.fluxer.com/rpm/fluxer-canary.repo
sudo dnf install fluxer-canary
```

dnf keeps the repository metadata for six hours, so a new build can take up to six hours to show up. Run `sudo dnf --refresh upgrade` to check sooner.

## Arch Linux

Add the signing key to the pacman keyring once, or pacman refuses the repository with `unknown trust`.

```sh
sudo pacman-key --init
curl -fsSL -o /tmp/fluxer-archive-keyring.asc \
  https://pkgs.fluxer.com/keys/fluxer-archive-keyring.asc
sudo pacman-key --add /tmp/fluxer-archive-keyring.asc
sudo pacman-key --lsign-key 09D01339EE128925F75E675C855C5BDE34D205D2
```

Then add the repository and install `fluxer` or `fluxer-canary`.

```sh
sudo tee -a /etc/pacman.conf >/dev/null <<'REPO'

[fluxer]
SigLevel = Required TrustedOnly
Server = https://pkgs.fluxer.com/arch/$repo/os/$arch
REPO
sudo pacman -Syu fluxer
```

## Flatpak

Stable is also on Flathub as `app.fluxer.Fluxer`, but canary is only in the Fluxer repository. The reference file adds a remote called `fluxer` and installs the app.

```sh
flatpak install https://pkgs.fluxer.com/flatpak/fluxer.flatpakref
```

For canary, use the canary reference file.

```sh
flatpak install https://pkgs.fluxer.com/flatpak/fluxer-canary.flatpakref
```
