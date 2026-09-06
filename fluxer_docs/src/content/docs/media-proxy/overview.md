---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Media Proxy overview
description: Media Proxy surfaces, methods, byte ranges, response headers, and cache behaviour.
---

The Media Proxy serves Fluxer attachments, image assets, themes, entrance sound audio, and signed external media. It also accepts uploads authorised by a capability, a signed grant in the upload URL.

## Route families

| Family | Paths | Reference |
| --- | --- | --- |
| Attachment | `/attachments/{path}` | [Get attachment](/media-proxy/routes/#get-attachment) |
| Signed external | `/external/{signature}/{target}` | [Get signed external media](/media-proxy/routes/#get-signed-external-media) |
| Theme CSS | `/themes/{path}.css` | [Get theme CSS](/media-proxy/routes/#get-theme-css) |
| Entrance sound | `/entrance-sounds/{user_id}/{filename}` | [Get entrance sound](/media-proxy/routes/#get-entrance-sound) |
| Image asset<sup>1</sup> | `/avatars`, `/icons`, `/branding`, `/banners`, `/splashes`, `/embed-splashes`, `/emojis`, `/stickers` | [Image asset contract](/media-proxy/routes/#image-asset-contract) |
| Static object<sup>2</sup> | `/{key}` | [Get static object](/media-proxy/routes/#get-static-object) |
| Upload relay | `/v1/relay/{key}` | [Upload relay](/media-proxy/upload-relay/) |
| Operator and internal | `/_health`, `/_metrics`, `/_metadata`, `/_thumbnail`, `/_frames` | [Operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) |

<sup>1</sup> Guild member avatars and banners are image assets too, at `/guilds/{guild_id}/users/{user_id}/avatars` and `/guilds/{guild_id}/users/{user_id}/banners`

<sup>2</sup> Served by a `static` endpoint alone

[Transformations](/media-proxy/transformations/) defines representation selection for every family that has one. [Responses and limits](/media-proxy/responses-and-limits/) consolidates statuses, size bounds, deadlines, and cache policies.

## Base URLs

Read paths have no `/v1` prefix. Take `endpoints.media` and `endpoints.static_cdn` from [instance discovery](/http-api/instance/#instance-endpoints-object).

The upload relay is the exception in both respects. It is served below `/v1/relay/`, and its base comes from a deployment setting. A client MUST take that base from the upload URL the HTTP API issued and MUST NOT rebuild one.

## Authorisation

No public Media Proxy route reads the HTTP API `Authorization` header. A stored object is authorised by its path, signed external media by its path signature, and an upload by the capability in its URL. Only the internal `POST` endpoints read `Authorization`, and they require the exact value `Bearer {secret}` built from the deployment secret.

:::caution[A media URL is a bearer capability]
An attachment path has no signature and no expiry, so anyone holding the URL reads the object for as long as it exists. Leaving the channel does not revoke a URL already issued.
:::

The Media Proxy has no request-count rate limit, so no response has the [rate limit headers](/topics/rate-limits/#rate-limit-headers) of the versioned HTTP API. A failure returns a short `text/plain` body, and [media error response](/media-proxy/responses-and-limits/#media-error-response) defines that shape.

## Deployment modes

One Media Proxy process serves exactly one mode. The mode is fixed at startup and defaults to `mp`.

| Mode | Serves |
| --- | --- |
| `mp` | Attachments, signed external media, themes, entrance sounds, and every image asset route |
| `static` | Every read path as a raw object read from the static bucket, with no transformation and no SVG rasterisation<sup>1</sup> |
| `upload` | The [upload relay](/media-proxy/upload-relay/), plus every `mp` read route from the same buckets |

<sup>1</sup> A `static` mode endpoint also strips `X-Robots-Tag` from every response and sends no `Content-Disposition`

The relay `PUT` is the only route with a mode gate, and it returns 404 outside `upload` mode. On a read that requests no transformation, only an `mp` endpoint rasterises SVG, so an `upload` endpoint returns the original SVG bytes. The [operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) behave the same in every mode.

Which published base URL serves which mode is a deployment choice. The reference self-hosted deployment serves `endpoints.media` from an `upload` mode process.

## Methods

Every read route accepts `GET` and `HEAD`. HEAD returns the same status and representation headers as GET with an empty body, and a `Range` on HEAD still selects 206 or 416. Any other method on a read path returns 405 with the body `Method Not Allowed`.

The relay path accepts `PUT`. Any other method there returns 405 with an `Allow` header. An unknown path returns 404 with the body `Not Found`.

The [signed external route](/media-proxy/routes/#get-signed-external-media) is the one place a HEAD can answer differently from the matching GET. A HEAD with no range and no transformation is served from an origin HEAD when that origin returns 200, declares a length within the [500 MiB media bound](/media-proxy/responses-and-limits/#request-and-media-limits), and names a non-SVG media type.

:::note[An origin HEAD resolves the type without bytes]
That answer uses the declared type and the filename alone, so an origin that mislabels its bytes produces a different `Content-Type` and `Content-Disposition` than the GET of the same URL.
:::

## Request headers

Only `Range` affects the representation a public read returns. `X-Forwarded-For` decides which address the [media access allowlist](#access-restrictions) evaluates, so it can turn a 200 into a 403 without changing the representation. Every other request header, including `Authorization`, `Cookie`, `Accept`, `If-Range`, `If-None-Match`, and `Origin`, is ignored on a public read route.

### Common request headers

| Field | Type | Description |
| --- | --- | --- |
| Range?<sup>1</sup> | string | One byte range under the [byte-range contract](#byte-ranges) |
| X-Forwarded-For?<sup>2</sup> | string | Client address chain read by the [media access allowlist](#access-restrictions) |

<sup>1</sup> A value whose unit is not the exact lowercase `bytes`, or that names more than one range, is ignored, and the response has the complete representation

<sup>2</sup> Read only when the allowlist gate is enabled and the peer address is a configured trusted proxy. A value from any other peer is ignored

The Media Proxy reads no `If-None-Match`, `If-Modified-Since`, `If-Range`, `If-Match`, or `If-Unmodified-Since`, and it sends no `ETag` or `Last-Modified` on a read route, so a cache revalidates a representation by fetching it again.

## Access restrictions

An operator MAY gate public reads on a CDN edge address allowlist. The gate is disabled by default. When it is enabled, the process fetches the Bunny edge address list at startup and refreshes it every 3600 seconds by default, and a process whose first fetch fails does not start.

A request from an address outside the list returns 403 with the body `origin not in bunny allowlist`. `/_health`, `/_metrics`, `/_metadata`, `/_thumbnail`, `/_frames`, and every path below `/v1/relay/` are exempt.

When the peer address is a configured trusted proxy, the client address is the rightmost `X-Forwarded-For` hop that is not itself a trusted proxy. Otherwise the Media Proxy uses the peer address and ignores `X-Forwarded-For`. It does the same when there is no `X-Forwarded-For`, or when every hop in it is unparseable or is itself a trusted proxy.

## Selector parsing

A read route names its representation in its query string. The attachment, signed external, and image asset routes accept selectors. The theme and entrance sound routes accept none and ignore any query string.

Query names and values use URL form decoding, so `+` decodes to a space and a percent escape decodes to its byte. When a name occurs more than once, the final value wins. An unknown name is ignored.

The Media Proxy canonicalises nothing and issues no redirect for a noncanonical target. Two spellings of the same selection are two separate cache entries.

A Boolean is true only for case-insensitive `true` or the exact value `1`. Every other value, including `false` and `0`, is false. [Transformations](/media-proxy/transformations/) defines dimensions, formats, quality values, and animation flags.

## Byte ranges

This contract governs every range the Media Proxy resolves itself. A range is recognised only when its unit is the exact lowercase `bytes`, so `bytes=0-99` selects an interval and `BYTES=0-99` is ignored. Surrounding spaces and tabs are trimmed. A range containing a comma names multiple ranges and is ignored. A malformed range is ignored and produces the complete representation. `HEAD` applies the same contract as `GET`.

| Value | Description |
| --- | --- |
| `bytes={start}-{end}` | Selects the inclusive interval and clamps an end beyond the representation |
| `bytes={start}-` | Selects from start through the final byte |
| `bytes=-{length}` | Selects the final length bytes and selects the complete representation when length is larger |

A reversed range, a zero-length suffix, a start outside the representation, or any range over an empty representation returns 416 with `Content-Range: bytes */{size}` and `Accept-Ranges: bytes`. A satisfiable range returns 206, `Accept-Ranges: bytes`, and exact `Content-Range` and `Content-Length` values.

### Ranges on the signed external route

The route forwards a range to the origin only when no transformation is requested. It sends the range verbatim when the value after `bytes=` is non-empty and every byte of it is an ASCII graphic character, so a multiple range reaches the origin and the origin decides how to answer it. A value with a space anywhere is dropped, and no range is sent. The route relays the origin partial response with the origin `Content-Range` unchanged.

A transforming request forwards no range to the origin and applies the client range to the transformed bytes, so it still returns 206 or 416. An origin 200 on a non-transforming request is relayed as that 200 when its declared type is trustworthy and the response is not SVG by declared type, filename, or leading bytes. A relayed 200 does not reapply the client range. An SVG response is rasterised and the client range is applied to the rasterised bytes.

A trustworthy type is a normalised `image/`, `video/`, or `audio/` type other than `application/octet-stream`. An absent or empty `Content-Type`, `text/plain`, `application/pdf`, and `application/zip` are all untrustworthy. Fluxer buffers the body of a 200 under an untrustworthy type and applies the client range to those bytes, so that read returns 206.

The route fetches twice in exactly one case. When an origin answers a forwarded range with 206 under a declared SVG media type, Fluxer discards that partial response, fetches the whole object again without a range, rasterises it, and applies the client range to the rasterised bytes.

:::caution[A mislabelled SVG reaches the client as bytes]
The re-fetch tests the declared media type alone. An origin that answers a forwarded range with SVG bytes under another type produces a 206 of raw SVG under that type.
:::

Disposition follows that declared type, so SVG mislabelled as an image or video media type is served inline.

## Representation headers

| Field | Type | Description |
| --- | --- | --- |
| Content-Type<sup>1</sup> | string | The detected or selected media type |
| Content-Length?<sup>2</sup> | integer | The selected body length, including on HEAD |
| Content-Range? | string | Present on 206 as `bytes {start}-{end}/{size}` and on 416 as `bytes */{size}` |
| Content-Disposition?<sup>3</sup> | string | The route-selected inline or attachment disposition |
| Accept-Ranges | string | The literal `bytes` on every media representation |
| Access-Control-Allow-Origin | string | The literal `*` on media responses |
| Cache-Control<sup>4</sup> | string | The browser cache policy for the representation |
| CDN-Cache-Control | string | The corresponding shared cache policy |
| Vary | string | The literal `Accept-Encoding` |
| X-Content-Type-Options | string | The literal `nosniff` |
| X-Robots-Tag<sup>5</sup> | string | The indexing policy |
| X-Fluxer-Version<sup>6</sup> | string | The serving Fluxer build identifier, or `dev` when none is configured |

<sup>1</sup> Fixed at `text/css; charset=utf-8` on the theme route. A 416 response has none

<sup>2</sup> Counts the selected bytes, so a 206 advertises the range length. Omitted from a streamed [signed external](/media-proxy/routes/#get-signed-external-media) response whose origin declared no length

<sup>3</sup> Present on the attachment, image asset, and signed external routes, absent on the theme and entrance sound routes, absent on every `static` mode read, and omitted when an image asset falls back to its original bytes after a failed transcode

<sup>4</sup> An audio or video representation appends `no-transform`. A route-produced error uses `no-store` instead

<sup>5</sup> The literal `noindex, nofollow, nosnippet, noimageindex, notranslate, max-snippet:0, max-image-preview:none, max-video-preview:0`. Present in `mp` and `upload` mode. A `static` mode endpoint removes it

<sup>6</sup> Absent from a media access allowlist rejection

Every successful media representation uses `Cache-Control: public, max-age=31536000` and `CDN-Cache-Control: public, max-age=31536000`. An audio or video representation adds `no-transform` to the browser-facing policy only. No response repeats its policy in an `Expires` header. [Cache policies](/media-proxy/responses-and-limits/#cache-policies) lists the responses that have no policy at all.

No read route sends an `ETag`. The upload relay is the only route that returns one, and it relays the object storage value for the stored object.

:::note[External media is cached for a year too]
The signed path is derived from the target URL, so the bytes behind one unchanged target are cached for a year at both layers.
:::

A 416 response has `Accept-Ranges`, `Content-Range`, `Access-Control-Allow-Origin`, `Vary`, and `X-Robots-Tag`, with no `Content-Type` and no cache policy. Its body is empty.

## Content detection

For a streamed object, Fluxer first reads stored media type metadata that names an image, audio, or video. Otherwise it takes the filename extension, then any other non-empty declared type except `application/octet-stream`, and finally `application/octet-stream`.

When the complete input is available, detected SVG takes precedence. Another trustworthy declared image, audio, or video type remains authoritative. Without one, Fluxer checks the leading 8,192 bytes, then the filename extension, then the declared type, and finally uses `application/octet-stream`.

A filename extension that identifies MP4 audio overrides a declared `video/mp4` with `audio/mp4`.

## Content disposition

Disposition follows the resolved media type. An image other than SVG and a video are served inline. Every other type, including SVG and PDF, is served as an attachment. An explicit `download` request forces attachment disposition on every route that accepts the parameter.

The disposition filename comes from the route. An attachment or signed external read uses the filename in the path or target URL. An image asset uses the path hash with any `a_` prefix stripped, followed by the canonical name of the path extension, so `/avatars/1/a_abcd1234.jpg` is offered as `abcd1234.jpeg`.

When `download` resolves to true and the served media type has a canonical extension the filename does not already use, the filename keeps its stem and takes that extension, so a PNG transformation of `holiday.jpg` is offered as `holiday.png`. When a filename is not safe as a quoted ASCII value, Fluxer sends a sanitised quoted fallback and an RFC 5987 `filename*` parameter.

:::caution[A scriptable document is never inline]
The attachment, image asset, and signed external routes rasterise SVG to WebP, so a browser does not execute the document in the Media Proxy origin.
:::

On a non-transforming attachment read, only an `mp` endpoint rasterises SVG, and that path always uses lossless WebP. A transforming request uses the `format` and `quality` it was given, and an image asset path defaults to `high`. A `static` mode endpoint serves the original bytes.
