---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Fluxer API
description: Start using the Fluxer API.
---

Fluxer is a self-hostable chat platform.

- To build a client or a bot, start with the [HTTP API](/http-api/) and the [Gateway](/gateway/overview/).
- For voice and screen sharing, read [Voice](/voice/).
- To run an instance, start with [Get started](/operator/get-started/).

## Protocol surfaces

Use the HTTP API to read and change resources, and Gateway [events](/gateway/events/) to receive updates. The [Media Proxy](/media-proxy/overview/) serves media and uploads. The [Admin API](/admin-api/) provides privileged instance management.

## Shared contracts

See [Authentication](/authentication/), [Errors](/http-api/errors/), [Rate limits](/topics/rate-limits/) and [Locales](/topics/locales/) for shared behaviour. Most resource identifiers are [snowflakes](/snowflakes/). Each resource page states where it differs from this shared behaviour.

## Field notation

In field tables, `?` after a field name means optional. Before a type, it means nullable.

| Field | Type | Meaning |
| --- | --- | --- |
| `name` | `string` | Required and cannot be `null` |
| `name?` | `string` | Optional, but cannot be `null` when present |
| `name` | `?string` | Required, but can be `null` |
| `name?` | `?string` | Nullish, so it can be omitted or set to `null` |

An omitted field is different from a field set to `null`. Each operation explains how these values affect the resource.

In the type column, `array[type]` is an array of the named type, and `map[key, value]` is a JSON object keyed by the first type with values of the second.

## Endpoint discovery

Start with `GET /.well-known/fluxer` on the instance origin. It requires no authentication and allows cross-origin requests.

```text
GET https://example.com/.well-known/fluxer
```

For the hosted instance, use `https://fluxer.app/.well-known/fluxer`.

Read base URLs from the response's [endpoints](/http-api/instance/#instance-endpoints-object). Do not derive them from the origin or hard-code Fluxer domains.

- Bots, libraries and third-party clients use `endpoints.api_public`.
- The first-party web application uses `endpoints.api_client`. `endpoints.api` is an alias for it.

Use `/v1` paths and send credentials in the `Authorization` header as described in [Authentication](/authentication/).
