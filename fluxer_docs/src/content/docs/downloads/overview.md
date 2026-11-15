---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Downloads overview
description: Where Fluxer publishes desktop builds and Linux package repositories.
---

Fluxer publishes desktop builds and Linux package repositories at `https://pkgs.fluxer.com`. You don't need to sign in to download anything.

| Page | What it has |
| --- | --- |
| [Desktop builds](/downloads/desktop/) | Fetch a build by channel, platform, architecture and version |
| [Linux repositories](/downloads/linux-repositories/) | Commands to add the apt, dnf, pacman and Flatpak repositories |

## Channels

Every build is published to either the `stable` or the `canary` channel. The apt, dnf and pacman packages are called `fluxer` on stable and `fluxer-canary` on canary, and you can install both on one machine.

## Paths

| Prefix | Contents |
| --- | --- |
| `/desktop/` | [Desktop builds](/downloads/desktop/) |
| `/flatpak/` | [The Flatpak repository](/downloads/linux-repositories/#flatpak) |
| `/arch/` | [The pacman repository](/downloads/linux-repositories/#arch-linux) |
| `/deb/` | [The apt repository](/downloads/linux-repositories/#debian-and-ubuntu) |
| `/rpm/` | [The dnf repository](/downloads/linux-repositories/#fedora-and-rhel) |
| `/keys/` | [The public signing key](/downloads/linux-repositories/) for every repository |
