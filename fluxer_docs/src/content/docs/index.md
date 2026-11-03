---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Fluxer API
description: Where to start with the Fluxer API and how to read the reference pages.
---

Fluxer is a self-hostable chat platform. You can build clients and bots with its HTTP API and Gateway, or run your own instance.

| Section | Description |
| --- | --- |
| [HTTP API](/http-api/) | Read and change users, guilds, channels and messages |
| [Gateway](/gateway/overview/) | Receive [events](/gateway/events/) over a WebSocket connection |
| [Voice](/voice/) | Join voice calls and share your screen |
| [Media Proxy](/media-proxy/overview/) | Load media and upload files |
| [Admin API](/admin-api/) | Manage an instance with admin access |
| [Self-hosting](/operator/) | Install and run an instance |

[Authentication](/authentication/), [Errors](/http-api/errors/), [Rate limits](/topics/rate-limits/) and [Locales](/topics/locales/) cover behavior the APIs share. Each page says where it differs. Most IDs are [snowflakes](/snowflakes/).

## Field notation

In field tables, `?` after a field name means optional. Before a type, it means nullable.

| Field | Type | Meaning |
| --- | --- | --- |
| `name` | `string` | Required and cannot be `null` |
| `name?` | `string` | Optional, but cannot be `null` when present |
| `name` | `?string` | Required, but can be `null` |
| `name?` | `?string` | Can be omitted or set to `null` |

In a request body, leaving a field out and sending `null` can do different things, and the endpoint says which.

| Type | Meaning |
| --- | --- |
| `array[type]` | An array of that type |
| `map[key, value]` | A JSON object with keys of the first type and values of the second |

## Endpoint discovery

Get `/.well-known/fluxer` on the instance origin to find its base URLs. It needs no authentication and allows cross-origin requests.

```text
GET https://example.com/.well-known/fluxer
```

Bots, libraries and third-party clients send requests to `endpoints.api_public` from the [endpoints object](/http-api/instance/#instance-endpoints-object) followed by a `/v1` path, such as `/v1/users/@me`. Connect to the Gateway at `endpoints.gateway`. Don't build base URLs from the origin or hard-code Fluxer domains. Send credentials in the `Authorization` header as described in [Authentication](/authentication/).
