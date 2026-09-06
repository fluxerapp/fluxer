---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Fluxer API
description: The four Fluxer protocol surfaces and the contracts they share.
---

Fluxer is a self-hostable chat platform. Its API has four surfaces, and all four share one identifier space.

- To build a client or a bot, start with the [HTTP API](/http-api/) and the [Gateway](/gateway/overview/).
- For voice or a screen share, read [Voice](/voice/).
- To run an instance, start with [Get started](/operator/get-started/).
- To look up one route, use the sidebar or the [Protocol surfaces](#protocol-surfaces) table.

## Protocol surfaces

| Surface | What it is | Reference |
| --- | --- | --- |
| HTTP API | Resource reads and mutations below `/v1` | [HTTP API](/http-api/) |
| Gateway | A persistent WebSocket for session state and real-time events | [Gateway](/gateway/overview/) |
| Media Proxy | Attachments, image assets, themes, entrance sound audio, and the upload relay | [Media Proxy](/media-proxy/overview/) |
| Admin API | The privileged namespace below `/v1/admin` | [Admin API](/admin-api/) |

A client mutates a resource over the HTTP API and receives the resulting update as a Gateway [Dispatch](/gateway/events/). Each operation states the Dispatches it fires, and [Events](/gateway/events/) defines each payload and its recipient scope.

A [snowflake](/snowflakes/) is the identifier all four surfaces share. Voice runs on LiveKit, and [Voice](/voice/) defines the placement protocol and the media transport.

## Shared contracts

| Read this | For |
| --- | --- |
| [Conventions](/conventions/) | Wire table notation, footnotes, omission and `null` |
| [Authentication](/authentication/) | The `Authorization` grammar and the four credential kinds |
| [Snowflakes](/snowflakes/) | Identifiers, ordering, and pagination cursors |
| [Errors](/http-api/errors/) | The error envelope and the code registries |
| [Rate limits](/topics/rate-limits/) | Buckets, the 429 body, and the `X-RateLimit-*` headers |
| [Locales](/topics/locales/) | The locale registry and `Accept-Language` negotiation |

## Endpoint discovery

A client that knows only a Fluxer origin reads endpoint discovery first. `GET /.well-known/fluxer` is unversioned, accepts no credential, and is readable from any origin.

```text
GET https://example.com/.well-known/fluxer
```

It returns the [instance discovery object](/http-api/instance/#instance-discovery-object). Every base URL a client uses comes from the [instance endpoints object](/http-api/instance/#instance-endpoints-object) inside it. A client MUST read every base URL from that response, and it MUST NOT derive one from the origin it was given or assume an official Fluxer domain.

Take `endpoints.api` from that response, then send a credential in the `Authorization` header.

```text
GET https://api.example.com/v1/users/@me
Authorization: flx_ZDb1GURItsMuYl1zvrgxv2qLBxyNmgNSEaWT
```

That credential is a user session token. [Log in with a password](/http-api/authentication/#log-in-with-a-password) issues one. A bot sends a bot token with the `Bot` prefix, issued by [Create application](/http-api/applications/#create-application). [Authentication](/authentication/) gives the exact form of all four kinds.
