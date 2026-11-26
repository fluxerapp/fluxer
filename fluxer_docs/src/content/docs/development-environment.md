---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Set up a development environment
description: Open Fluxer's dev container, run the whole stack from source, run the checks CI runs and find your way around the repository.
---

The [repository](https://github.com/fluxerapp/fluxer) has a dev container with the tools and services you need to run Fluxer from source. Read [Contributing](/contributing/) before you start on a change.

## Before you start

- Docker Engine or Docker Desktop.
- An editor that supports dev containers, such as Visual Studio Code.
- At least 4 CPU cores, 8 GB of memory and 32 GB of disk for the container.
- Go 1.27 if you work on `fluxer_native` or `fluxer_rexctl`, or want to run the docs link check. The dev container doesn't include Go.

## Open the dev container

1. Clone the repository.

   ```sh
   git clone https://github.com/fluxerapp/fluxer.git
   ```

2. Open the folder in your editor. In Visual Studio Code, install the Dev Containers extension and run "Dev Containers: Reopen in Container" from the command palette.

The container has Node.js 26, pnpm, Rust, Erlang, Gleam and the libraries the media proxy needs. Next to it, Docker Compose starts PostgreSQL, Valkey, NATS, LiveKit, Meilisearch and Mailpit. The first build compiles FFmpeg and libheif from source and takes a while.

When the container is created, it runs `cargo run -p fluxer-dev -- bootstrap`. This installs the JavaScript dependencies, downloads the spellcheck dictionaries, writes the gateway's development config, waits for the services and starts SeaweedFS for file storage. Run `pnpm dev:bootstrap` to do it again, for example after the first start failed.

When `.devcontainer/Dockerfile` changes on main, rebuild the container with "Dev Containers: Rebuild Container". A container keeps the image it was built from until you rebuild it, and the Rust build can then fail with C errors from `fluxer_media_proxy` because a library it needs is missing or too old.

Open and rebuild the container from your editor. The Compose project is called `fluxer-dev`, and the `node_modules`, Cargo and database volumes belong to that project. If you start the stack by hand with `docker compose up`, your editor can't adopt the containers that command creates.

## Run Fluxer

Start every part of the stack from the repository root inside the container.

```sh
pnpm dev
```

The tasks start one after another, and the first start takes a while because it compiles the Rust services. Then open [http://localhost:8088](http://localhost:8088), the one port everything goes through.

| Path | What it opens |
| --- | --- |
| `/` | The web app |
| `/api` | The API |
| `/gateway` | The gateway |
| `/media` | The media proxy |
| `/admin` | The admin panel |
| `/devmail` | Mailpit, which catches every email the stack sends, such as sign-up verification |

By default `pnpm dev` runs the `proxy`, `services`, `media`, `push`, `admin`, `api`, `gateway-single`, `worker`, `app` and `app-proxy` tasks. To run only some of these tasks, give their names after the command.

```sh
pnpm dev proxy api worker gateway-single app app-proxy
```

The `gateway` task runs a local gateway cluster and `gateway-single` runs one gateway. Pick one of the two, because `pnpm dev` won't start both.

The development settings are in `config/env/development.env`, and your own overrides go in `config/env/local.env` or `.env.local`, which Git ignores.

These commands manage the services Docker Compose runs next to the container.

| Command | What it does |
| --- | --- |
| `pnpm dev:infra:status` | Lists the services and their state |
| `pnpm dev:infra:stop` | Stops them |
| `pnpm dev:infra:start` | Starts them again |
| `pnpm dev:infra:restart` | Stops and starts them |

If uploads fail because file storage is down, `cargo run -p fluxer-dev -- media-proxy doctor --repair` starts SeaweedFS again. Its log is `.fluxer/dev/logs/seaweedfs.log`.

## Run the checks

Pull requests run these checks in CI, among others. Run them inside the container, from the repository root. Before you change anything, run them once on an unchanged checkout of main to see which failures were already there.

| Check | Command |
| --- | --- |
| Formatting and lint for TypeScript | `pnpm lint` |
| Type checks | `pnpm typecheck` |
| TypeScript tests | `pnpm test` |
| Unused files, exports and dependencies | `pnpm knip` |
| Rust formatting | `cargo fmt --all -- --check` |
| Rust lint | `cargo clippy --all-targets --all-features --locked -- -D warnings` |
| Rust tests | `cargo test --all-features --locked` |
| Gateway formatting, lint, build and tests | `tools/ci/run.sh gateway --step all_checks` |

`pnpm typecheck` and `pnpm test` run the code generators they need first. `pnpm test` runs the tests of every workspace package and then the API tests, which need NATS. Inside the container they use `nats://nats:4222`, and you can point them elsewhere with `FLUXER_NATS_URL`. To run one package's tests, filter on it, as in `pnpm --filter fluxer_api test` for the API package.

If you change text that gets translated, run `pnpm i18n:compile` and commit the catalog files it changes. CI fails when they are out of date.

For changes to these docs, `pnpm docs:dev` starts a preview and `pnpm docs:build` builds the site the way CI does. CI also checks every docs link with `go run ./internal/lint/doclinks ..` from the `fluxer_rexctl` folder.

## Repository layout

| Folder | What's in it |
| --- | --- |
| `fluxer_api` | The API and its background worker, in TypeScript |
| `fluxer_app` | The web app, which the desktop app also uses |
| `fluxer_desktop` | The Electron desktop app and its native modules |
| `fluxer_gateway` | The real-time gateway, in Gleam on the Erlang VM, with Rust native code |
| `fluxer_media_proxy` | Serves and processes images and video, in Rust and C |
| `fluxer_admin` | The admin panel, in Rust |
| `fluxer_app_proxy` | Serves the web app's pages |
| `fluxer_push` | Push notification delivery |
| `fluxer_users`, `fluxer_messages`, `fluxer_snowflakes`, `fluxer_unfurl`, `fluxer_gifs` | Rust services for users, messages, IDs, link previews and GIF search |
| `fluxer_server` | One binary that runs the Rust services together |
| `fluxer_svc`, `fluxer_common` | Code the Rust services share |
| `fluxer_native` | `fluxerd`, the Go supervisor a self-hosted instance runs |
| `fluxer_rexctl` | `rexctl`, the Go command for installing and running an instance |
| `fluxer_static` | Static files such as emoji, avatars and email assets |
| `fluxer_docs` | This site |
| `packages` | TypeScript packages the API, the web app and the desktop app share |
| `tools` | `fluxer-dev` for local development, `fluxer-ci` for CI and the release build scripts |
| `deploy`, `packaging` | The Docker Compose file, the Helm chart, the container image and the systemd service |
| `config/env` | Development settings |
| `.devcontainer` | The dev container |

[Architecture](/operator/how-it-works/) shows how these parts run together on an instance, and [Building from source](/operator/build-from-source/) shows how to build a release tarball or image.
