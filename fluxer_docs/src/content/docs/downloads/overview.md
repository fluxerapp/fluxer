---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Package origin overview
description: The public origin that serves Fluxer desktop builds and Linux package repositories.
---

Fluxer publishes every desktop build and every Linux package repository to one public origin, `https://pkgs.fluxer.com`. It is a static file origin. It serves bytes and nothing else, it needs no credential, and it declares no rate limit bucket.

## Channels

Every path names a channel, either `stable` or `canary`. The two are published independently and never share a file. The package name follows the channel: `fluxer` on stable, `fluxer-canary` on canary. A machine may install both at once.

## What the origin serves

| Prefix | Contents |
| --- | --- |
| `/desktop/` | Desktop installers, checksums and update feeds |
| `/flatpak/` | An ostree repository holding both application ids |
| `/arch/` | A pacman repository, one directory per architecture |
| `/deb/` | An apt repository in the `dists` and `pool` layout |
| `/rpm/` | A dnf repository, one directory per channel and architecture |
| `/keys/` | The public signing key for the apt and rpm repositories |

## What it guarantees

The origin answers `Range`, `If-Range` and `If-None-Match` on every artifact, so a client may resume an interrupted download and revalidate a cached one. It never lists a directory. A request for a path that does not exist returns 404 with no body.

Artifacts under a version directory never change once published, so they are cached for a year. The files that move when a release ships, the `latest` documents and every repository index, are cached for minutes.

## Signing

The apt and rpm repositories are signed. The public key lives at `/keys/fluxer-archive-keyring.asc`, and the entrypoint files reference it, so a client verifies every package it installs.

The flatpak and pacman repositories are unsigned and rely on HTTPS for transport integrity. pacman still verifies each package against the SHA256 its index records, and flatpak verifies each object against its content address.
