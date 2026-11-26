---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Linux repositories
description: How to add the apt, dnf, pacman and Flatpak repositories Fluxer publishes.
---

Fluxer publishes apt, dnf, pacman and Flatpak repositories, signed by the key with fingerprint `09D01339EE128925F75E675C855C5BDE34D205D2`. Each channel has its own entry file on apt and dnf, package name on pacman and app ID on Flatpak.

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

A `.deb` you downloaded gets updates only once you add the entry for its channel. For canary, add that entry and install `fluxer-canary`.

```sh
sudo curl -fsSL -o /etc/apt/sources.list.d/fluxer-canary.sources \
  https://pkgs.fluxer.com/deb/fluxer-canary.sources
sudo apt update && sudo apt install fluxer-canary
```

## Fedora and RHEL

RHEL, Rocky, Alma and CentOS Stream need `sudo dnf install epel-release` first, because the package depends on `libXScrnSaver` and their base repositories lack it. Fedora has it already and doesn't need this step.

```sh
sudo curl -fsSL -o /etc/yum.repos.d/fluxer.repo \
  https://pkgs.fluxer.com/rpm/fluxer.repo
sudo dnf install fluxer
```

dnf asks you to import the signing key on the first install, once for the repository and once for the package. Check that the fingerprint matches the one above both times.

Canary needs its own entry and `fluxer-canary`.

```sh
sudo curl -fsSL -o /etc/yum.repos.d/fluxer-canary.repo \
  https://pkgs.fluxer.com/rpm/fluxer-canary.repo
sudo dnf install fluxer-canary
```

dnf keeps the repository metadata for six hours, so a new build can take that long to show up. Run `sudo dnf --refresh upgrade` to check sooner.

## Arch Linux

Add the signing key to the pacman keyring once or pacman fails with `unknown trust`.

```sh
sudo pacman-key --init
curl -fsSL -o /tmp/fluxer-archive-keyring.asc \
  https://pkgs.fluxer.com/keys/fluxer-archive-keyring.asc
sudo pacman-key --add /tmp/fluxer-archive-keyring.asc
sudo pacman-key --lsign-key 09D01339EE128925F75E675C855C5BDE34D205D2
```

Then add the repository and install `fluxer`, or `fluxer-canary` for canary.

```sh
sudo tee -a /etc/pacman.conf >/dev/null <<'REPO'

[fluxer]
SigLevel = Required TrustedOnly
Server = https://pkgs.fluxer.com/arch/$repo/os/$arch
REPO
sudo pacman -Syu fluxer
```

Type `$repo` and `$arch` exactly as shown. pacman fills them in itself, and the quoted `'REPO'` keeps your shell from expanding them.

## Flatpak

Fluxer's own Flatpak repository gets each update on release day. Open [the stable reference file](https://pkgs.fluxer.com/flatpak/fluxer.flatpakref) or [the canary one](https://pkgs.fluxer.com/flatpak/fluxer-canary.flatpakref) and your software manager takes over. In the address bar, some desktops also accept `flatpak+https://pkgs.fluxer.com/flatpak/fluxer.flatpakref`.

From a terminal, the reference file adds a remote called `fluxer` and installs the app.

```sh
flatpak install https://pkgs.fluxer.com/flatpak/fluxer.flatpakref
```

For canary, use the canary reference file.

```sh
flatpak install https://pkgs.fluxer.com/flatpak/fluxer-canary.flatpakref
```

Stable is also on [Flathub](https://flathub.org/apps/app.fluxer.Fluxer) as `app.fluxer.Fluxer`. Flathub can be a few weeks behind Fluxer's own repository and has no canary.

```sh
flatpak install flathub app.fluxer.Fluxer
```
