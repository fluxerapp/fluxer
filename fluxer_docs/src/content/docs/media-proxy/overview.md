---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Media Proxy overview
description: How the Media Proxy serves files, and the deployment modes, access policies and headers that apply to every route.
---

The Media Proxy serves attachments, image assets, themes, entrance sounds and signed external media. It also accepts uploads to a signed upload URL.

## Route families

| Family | Paths | Reference |
| --- | --- | --- |
| Attachment | `/attachments/{path}` | [Get attachment](/media-proxy/routes/#get-attachment) |
| Signed external | `/external/{signature}/{target}` | [Get signed external media](/media-proxy/routes/#get-signed-external-media) |
| Theme CSS | `/themes/{path}.css` | [Get theme CSS](/media-proxy/routes/#get-theme-css) |
| Entrance sound | `/entrance-sounds/{user_id}/{filename}` | [Get entrance sound](/media-proxy/routes/#get-entrance-sound) |
| Image asset<sup>1</sup> | `/avatars`, `/icons`, `/branding`, `/banners`, `/splashes`, `/embed-splashes`, `/role-icons`, `/emojis`, `/stickers` | [Image asset contract](/media-proxy/routes/#image-asset-contract) |
| Static object<sup>2</sup> | `/{key}` | [Get static object](/media-proxy/routes/#get-static-object) |
| Upload relay | `/v1/relay/{key}` | [Upload relay](/media-proxy/upload-relay/) |
| Operator and internal | `/_health`, `/_metrics`, `/_metadata`, `/_sniff`, `/_thumbnail`, `/_frames`, `/_strip`, `/_audio_duration` | [Operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) |

<sup>1</sup> Guild member avatars and banners are image assets too, at these paths.

- `/guilds/{guild_id}/users/{user_id}/avatars`
- `/guilds/{guild_id}/users/{user_id}/banners`

<sup>2</sup> Only a process in [`static` mode](#deployment-modes) serves this route.

The attachment, signed external and image asset routes take [transformation parameters](/media-proxy/transformations/) from the query string of the URL.

## Base URLs

Get the read base URLs `endpoints.media` and `endpoints.static_cdn` from [instance discovery](/http-api/instance/#instance-endpoints-object). Read paths have no `/v1` prefix. For an upload, use the full URL from the HTTP API response.

<a id="authorization"></a>

## Authorisation

Read routes need no `Authorization` header.

- Anyone with a media URL can open the file without an account.
- Leaving a channel doesn't revoke a URL that was already issued.
- Only the internal `POST` endpoints use `Authorization`, and they need `Bearer {secret}` with the deployment secret.
- The Media Proxy has no rate limit.

## Deployment modes

Each Media Proxy process serves one mode, set with `FLUXER_MEDIA_PROXY_MODE` at startup, and the default is `mp`.

| Mode | Serves |
| --- | --- |
| `mp` | Attachments, signed external media, themes, entrance sounds and every image asset route |
| `static` | Every read route as a raw object from the static bucket, with no transformation or SVG rasterisation |
| `upload` | The [upload relay](/media-proxy/upload-relay/) and every `mp` read route, without SVG rasterisation on a non-transforming read |
| `relay` | The [upload relay](/media-proxy/upload-relay/) alone, and every read path returns 404 |

The [operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) work in every mode. You choose which mode serves each base URL, and the reference self-hosted deployment serves `endpoints.media` from an `upload` process.

## Cross-origin reads

`FLUXER_MEDIA_PROXY_CORS_MODE` limits which web origins can read media, and is `off` by default. Turning on `report` or `enforce` needs `FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS` as well.

| Value | Description |
| --- | --- |
| `off` | Media has `Access-Control-Allow-Origin: *` and `Origin` is not checked |
| `report` | Responses are the same as under `off`, and each read `enforce` would refuse is logged |
| `enforce` | A read from an origin outside the allowlist returns 403 |

### Allowed origins under enforce

- An `Origin` must match an entry in `FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS` exactly.
- The desktop app origin `fluxer-app://app` is always allowed.
- An allowed read gets its origin back in `Access-Control-Allow-Origin`.
- A read with no `Origin` is served without that header, and an `<img>` or `<video>` without `crossorigin` still loads.
- A preflight `OPTIONS` request from an allowed origin gets a 204 that allows `GET` and `HEAD` with the `Range` header. A refused origin gets the same 403 as a read.

### Where the policy applies

The policy applies to `mp` and `upload` processes, and not to the upload relay or the operator endpoints. If a shared cache is in front of the Media Proxy, purge `/attachments/` when you switch from `off` to `enforce`.

## Signed attachment URLs

`FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE` can require a signature on every attachment read. It is `off` by default, and turning it on needs `FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64` as well.

| Value | Description |
| --- | --- |
| `off` | No signature is checked |
| `report` | Every read is served, and each read `enforce` would refuse is logged |
| `enforce` | A read without a valid signature returns 404 |

The policy applies to `/attachments/` paths on `mp` and `upload` processes.

### Signature parameters

An instance with attachment URL secrets signs every attachment URL it returns, and [Refresh attachment URLs](/http-api/messages/#refresh-attachment-urls) signs one again.

| Parameter | Value |
| --- | --- |
| `ex` | When the URL stops working, 24 hours after `is`, as 8 hex digits of Unix time. `0` on a data package URL |
| `is` | When the URL was issued, as 8 hex digits of Unix time |
| `hm` | The signature, in lowercase hex |
| `uc` | `dp` on a [data package URL](/http-api/messages/#data-package-attachment-urls), which never expires, and absent otherwise |

```text
?ex={expires}&is={issued}&hm={signature}
?ex=0&is={issued}&hm={signature}&uc=dp
```

Keep `ex`, `is`, `hm` and `uc` as issued, and you can add or change any other parameter. One signed URL works for every [transformation](/media-proxy/transformations/) and `download` variant of its file.

### Refused requests

A request is refused in these cases.

- A signature parameter is missing or malformed.
- The signature doesn't match a configured secret.
- The URL has expired.

| Mode | Response | Log reason |
| --- | --- | --- |
| `enforce` | The same 404 as a missing file, with the body `This content is no longer available.` | `attachment_signature_refused` |
| `report` | The file, with `Cache-Control: no-store` | `attachment_signature_would_deny` |

The warnings are limited to 20 a second.

### Shared caches

- A shared cache must leave `ex`, `is`, `hm` and `uc` out of its cache key.
- Purge `/attachments/` after every `mp` and `upload` process runs `enforce`, because entries stored under `off` keep their year-long cache lifetime.

### What a signature limits

Any signed-in user or bot can re-sign an attachment URL with [Refresh attachment URLs](/http-api/messages/#refresh-attachment-urls). A signature limits how long a copied URL works and not who can open the file.

## Byte ranges

The Media Proxy serves one range with the exact lowercase unit `bytes`, and ignores a range with another unit, more than one range or bad syntax.

| Value | Description |
| --- | --- |
| `bytes={start}-{end}` | The inclusive interval, with an end past the representation clamped |
| `bytes={start}-` | From start through the final byte |
| `bytes=-{length}` | The final length bytes |

A satisfiable range returns 206 with `Accept-Ranges: bytes`, `Content-Range` and `Content-Length`. A reversed range, a zero-length suffix, a start outside the representation or any range on an empty representation returns 416 with `Content-Range: bytes */{size}`.

### Transformed representations

A response whose bytes come from a transformation ignores `Range`, returns the complete result with 200 and has no `Accept-Ranges` header. An SVG that an `mp` process rasterises counts as transformed.

## Representation headers

`HEAD` returns the same headers as `GET` with an empty body. On the signed external route, a `HEAD` with no transformation or range gets its headers from the origin `HEAD` response.

| Field | Type | Description |
| --- | --- | --- |
| Content-Type<sup>1</sup> | string | The detected or selected media type |
| Content-Length? | integer | The length of the selected body, including on HEAD |
| Content-Range? | string | `bytes {start}-{end}/{size}` on 206 and `bytes */{size}` on 416 |
| Content-Disposition?<sup>2</sup> | string | The inline or attachment disposition |
| Accept-Ranges? | string | The literal `bytes`, absent on a [transformed representation](#transformed-representations) |
| Access-Control-Allow-Origin? | string | `*`, or the request origin under `enforce` |
| Cache-Control<sup>3</sup> | string | The browser cache policy |
| CDN-Cache-Control<sup>3</sup> | string | The shared cache policy |
| Vary | string | `Accept-Encoding`, with `Origin` added under `enforce` |
| X-Content-Type-Options | string | The literal `nosniff` |
| X-Robots-Tag? | string | A no-indexing policy in `mp` and `upload` mode |
| X-Fluxer-Version | string | The Fluxer build, or `dev` when none is configured |

<sup>1</sup> Fixed at `text/css; charset=utf-8` on the theme route, and absent on a 416.

<sup>2</sup> Sent on the attachment, image asset and signed external routes, and never in `static` mode.

<sup>3</sup> `public, max-age=31536000` on a successful response, with [shorter ages for signed attachments and data packages](/media-proxy/responses-and-limits/#cache-policies)

## Content type and disposition

Use the response `Content-Type`. Outside `static` mode, the Media Proxy replaces a JavaScript type, a type that isn't visible ASCII, or a missing type on a 200 or 206 with `text/plain; charset=utf-8`.

An image other than SVG and a video are served inline. Every other type, including SVG and PDF, is an attachment, and `download` forces an attachment on every route that accepts it. When you save a file, use the filename from `Content-Disposition`, which has the right extension for a transformed file.

An `mp` process rasterises SVG to lossless WebP on a non-transforming read. An `upload` process returns the original SVG bytes as an attachment.
