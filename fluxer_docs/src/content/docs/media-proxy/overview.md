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
| Operator and internal | `/_health`, `/_metrics`, `/_metadata`, `/_sniff`, `/_thumbnail`, `/_frames` | [Operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) |

<sup>1</sup> Guild member avatars and banners are image assets too, at `/guilds/{guild_id}/users/{user_id}/avatars` and `/guilds/{guild_id}/users/{user_id}/banners`

<sup>2</sup> Served by a `static` endpoint alone

[Transformations](/media-proxy/transformations/) defines the query parameters that select a representation for the families that accept them. [Responses and limits](/media-proxy/responses-and-limits/) lists statuses, size bounds, deadlines, and cache policies.

## Base URLs

Read paths have no `/v1` prefix. Take `endpoints.media` and `endpoints.static_cdn` from [instance discovery](/http-api/instance/#instance-endpoints-object).

The upload relay is the exception in both respects. It is served below `/v1/relay/`, and its base comes from a deployment setting. A client MUST take that base from the upload URL the HTTP API issued and MUST NOT rebuild one.

## Authorisation

No public Media Proxy route reads the HTTP API `Authorization` header. An attachment is authorised by its path and, under the [signed attachment URL policy](#signed-attachment-urls), by the signature in its query string. Any other stored object is authorised by its path, signed external media by its path signature, and an upload by the capability in its URL. Only the internal `POST` endpoints read `Authorization`, and they require the exact value `Bearer {secret}` built from the deployment secret.

:::caution[A media URL is a bearer capability]
Anyone holding a URL reads the object without an account. Under `enforce`, an ordinary signed attachment URL works until its `ex` time, which is at most one day after it was signed, and a [data package URL](/http-api/messages/#data-package-attachment-urls) works until the secret that signed it is removed. Under `off` or `report`, an attachment path works for as long as the object exists. Leaving the channel does not revoke a URL already issued.
:::

The Media Proxy has no request-count rate limit, so no response has the [rate limit headers](/topics/rate-limits/#rate-limit-headers) of the versioned HTTP API. A failure returns a short `text/plain` body, and [media error response](/media-proxy/responses-and-limits/#media-error-response) defines that shape.

## Deployment modes

One Media Proxy process serves exactly one mode. The mode is fixed at startup and defaults to `mp`.

| Mode | Serves |
| --- | --- |
| `mp` | Attachments, signed external media, themes, entrance sounds, and every image asset route |
| `static` | Every read route as a raw object read from the static bucket, with no transformation and no SVG rasterisation<sup>1</sup> |
| `upload` | The [upload relay](/media-proxy/upload-relay/), plus every `mp` read route from the same buckets |
| `relay` | The [upload relay](/media-proxy/upload-relay/) alone. Every read path returns 404 |

<sup>1</sup> A `static` mode endpoint also strips `X-Robots-Tag` from every response and sends no `Content-Disposition`

The relay `PUT` returns 404 outside `upload` and `relay` mode. A `relay` endpoint returns 404 for a `GET` or `HEAD` of any read path and reads no object for it. On a read that requests no transformation, only an `mp` endpoint rasterises SVG, so an `upload` endpoint returns the original SVG bytes. The [operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) behave the same in every mode.

The operator chooses which mode serves each published base URL. The reference self-hosted deployment serves `endpoints.media` from an `upload` mode process.

## Methods

Every read route accepts `GET` and `HEAD`. HEAD returns the same status and representation headers as GET with an empty body, and a `Range` on HEAD still selects 206 or 416. Any other method on a read route returns 405.

The relay path accepts `PUT`. Any other method there returns 405 with an `Allow` header. An unknown path returns 404.

On the [signed external route](/media-proxy/routes/#get-signed-external-media), a HEAD request can be answered from the headers of an origin HEAD response, so its representation headers can differ from GET.

## Request headers

Only `Range` affects the representation a public read returns. The [cross-origin read policy](#cross-origin-reads) reads `Origin`, and under `enforce` that header decides `Access-Control-Allow-Origin` and can turn a 200 into a 403. Every other request header, including `Authorization`, `Cookie`, `Accept`, `If-Range`, `If-None-Match`, `If-Modified-Since`, `If-Match`, and `If-Unmodified-Since`, is ignored on a public read route.

### Common request headers

| Field | Type | Description |
| --- | --- | --- |
| Range?<sup>1</sup> | string | One byte range under the [byte-range contract](#byte-ranges) |
| Origin?<sup>2</sup> | string | Serialised request origin read by the [cross-origin read policy](#cross-origin-reads) |

<sup>1</sup> A value whose unit is not the exact lowercase `bytes`, or that names more than one range, is ignored, and the response has the complete representation

<sup>2</sup> Read only when the cross-origin read policy is `report` or `enforce`

## Cross-origin reads

An operator can limit which web origins read media through CORS. `FLUXER_MEDIA_PROXY_CORS_MODE` sets this cross-origin read policy, and it defaults to `off`.

| Value | Description |
| --- | --- |
| `off` | A media representation has `Access-Control-Allow-Origin: *`, and no `Origin` is read |
| `report` | Every response is the same as under `off`, and each read `enforce` would refuse is logged |
| `enforce` | A read from an origin outside the allowlist returns 403 |

Under `enforce`, an `Origin` is allowed when it equals the serialised form of an entry in `FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS` byte for byte. Anything else is refused, including `null`, a repeated `Origin` header, and a value that is not ASCII. A refused read returns 403 on every read path, and the Media Proxy reads no object and fetches no external URL for it. [Media Proxy settings](/operator/configuration/#media-proxy-settings) defines how an entry is parsed and serialised.

A read with no `Origin` is served without `Access-Control-Allow-Origin`. A browser sends no `Origin` for an `<img>` or a `<video>` without `crossorigin`, so those elements still load. An allowed read gets its own origin back in `Access-Control-Allow-Origin` on a media representation and on a 416. No error has CORS headers.

Under `enforce`, every response to a `GET` or `HEAD` on a read path has `Vary: Accept-Encoding, Origin`, the 403 for a refused `Origin` included.

`report` changes no response. The Media Proxy logs a warning with the reason `cors_origin_would_deny` for each read `enforce` would refuse.

The policy applies to a response the Media Proxy generates, and a shared cache in front of it answers a hit on its own. An entry stored while the policy was `off` holds `Access-Control-Allow-Origin: *` and a year of `Cache-Control`, and a foreign origin reading that URL gets both back until the entry goes. Purge `/attachments/` when you move the policy to `enforce`. [Caching in front of Fluxer](/operator/reverse-proxy/#caching-in-front-of-fluxer) states how.

A `static` or `relay` endpoint ignores the policy. The [upload relay](/media-proxy/upload-relay/) and the [operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) are exempt from it. A method other than `GET` or `HEAD` on a read path returns 405 whatever its `Origin`.

The reference self-hosted deployment leaves the policy `off`. An operator who turns it on gets the instance public origin as the only entry, and [Media Proxy settings](/operator/configuration/#media-proxy-settings) states how to allow `https://web.fluxer.app` as well.

## Signed attachment URLs

An operator can require a signature on every attachment read. `FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE` sets this signed attachment URL policy, and it defaults to `off`. The reference self-hosted deployment leaves it `off`, and turning it on takes a secret and the mode. The Media Proxy reads the mode once at start, and [Turning the signature policy on](/operator/reverse-proxy/#turning-the-signature-policy-on) gives the order to move it and a cache in front of it, and what each pairing of the two serves.

| Value | Description |
| --- | --- |
| `off` | No signature is read, and an attachment read is served whatever its query string |
| `report` | Every attachment read is served, and each read `enforce` would refuse is logged |
| `enforce` | An attachment read without a valid signature returns 404 |

An instance with attachment URL secrets signs every attachment URL it returns, and [Refresh attachment URLs](/http-api/messages/#refresh-attachment-urls) signs one again. An ordinary signed URL has three query parameters, and Fluxer places them first in this order:

```text
?ex={expires}&is={issued}&hm={signature}
```

`ex` and `is` are each exactly 8 lowercase hexadecimal digits, the Unix time in seconds at which the URL expires and at which it was issued. `hm` is exactly 64 lowercase hexadecimal digits. Any other parameter follows them after a single `&`. A client MUST keep them exactly as issued and MAY add or change any other parameter.

A [data package URL](/http-api/messages/#data-package-attachment-urls) has a fourth parameter, `uc`, and its `ex` is the single character `0`:

```text
?ex=0&is={issued}&hm={signature}&uc=dp
```

`uc` is exactly `dp` and nothing else. `is` and `hm` have the same shape as on an ordinary URL. The 8-digit rule for `ex`, and the rule that `is` is no later than `ex`, hold for an ordinary URL alone. A data package URL never expires.

The Media Proxy reads the raw query string. It splits it on `&`, skips empty fields, and splits each field at its first `=`. It form-decodes each name and takes each value exactly as sent. A read is refused when any of these holds:

- There is no query string, or none of `ex`, `is`, `hm`, and `uc` is present.
- One or two of `ex`, `is`, and `hm` are present, so `uc` alone is refused too.
- Any of the four occurs more than once.
- `uc` is present with a value other than `dp`.
- `ex` is not 8 lowercase hexadecimal digits on an ordinary URL, or not the single character `0` on a data package URL.
- `is` is not 8 lowercase hexadecimal digits, or `hm` is not 64 lowercase hexadecimal digits.
- `is` is later than `ex` on an ordinary URL.
- `hm` does not verify against any configured secret.
- The current time is at or after `ex` on an ordinary URL.

A shared cache in front of the Media Proxy must leave the signature parameters `ex`, `is`, `hm`, and `uc` out of its cache key, and must decode each parameter name before it compares it. A cache that keeps them stores a new entry every time the same object is signed again. [Caching in front of Fluxer](/operator/reverse-proxy/#caching-in-front-of-fluxer) states the rest of the rules.

Every other parameter is ignored, and so is the order of the parameters. The signature is over `ex`, `is`, `uc`, and the percent-decoded storage key, and never over the host, the scheme, the port, or another parameter. Because `uc` is signed, an ordinary URL relabelled `uc=dp` does not verify, and neither does a data package URL with `uc` removed. One signed URL therefore reads every [transformation](/media-proxy/transformations/) and `download` variant of its object. Every percent-encoded spelling of the same key verifies the same way, and a key whose decoded bytes are not valid UTF-8 is refused.

Under `enforce`, a refused read returns 404 with the body `This content is no longer available.`, `Cache-Control: no-store`, and no `Access-Control-Allow-Origin`. The Media Proxy reads no object for it. The response is the same for every reason, for `GET` and `HEAD`, with or without a `Range`, and whatever transformation or `download` the request names. A read whose object does not exist returns that same 404 under `enforce`, even with a valid signature, so a response never tells a missing object apart from a refused signature. `off` and `report` return the ordinary `Not found` body for a missing object.

The Media Proxy checks the method first, then the `relay` mode, then the [cross-origin read policy](#cross-origin-reads), then the storage key, and only then the signature. A refused `Origin` returns 403 and an invalid storage key returns 400, and neither reads a signature.

`report` refuses nothing. Every status and body under `report` is the one `off` returns, and so is every header apart from the cache policy. A read whose signature is valid gets the same capped `Cache-Control` and `CDN-Cache-Control` that `enforce` sets, so the entries a cache stores during the dry run do not outlive the URLs that made them. A read `report` would refuse keeps the one-year policy `off` returns. The Media Proxy logs a warning with the reason `attachment_signature_would_deny` for each read `enforce` would refuse. The warning has the verdict `missing`, `malformed`, `mismatch`, or `expired`, the request identifier, and the user agent. Under every mode, the request log writes the values of `ex`, `is`, `hm`, and `uc` as `[redacted]`.

A `static` or `relay` endpoint ignores the policy. The policy covers `/attachments/` paths alone, so signed external media, image assets, themes, and entrance sounds need no signature. The [operator and internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) are exempt, and they read an attachment URL of this instance without a signature.

:::caution[A signature is not access control]
Refresh attachment URLs signs any well-formed attachment URL of the instance for any signed-in user or bot, with no membership check. A signature limits how long a copied URL works without a refresh. It does not decide who reads the object.
:::

## Selector parsing

A read route names its representation in its query string. The attachment, signed external, and image asset routes accept selectors. The theme and entrance sound routes accept none and ignore any query string.

Query names and values use URL form decoding, so `+` decodes to a space and a percent escape decodes to its byte. When a name occurs more than once, the final value wins. An unknown name is ignored. The [signed attachment URL policy](#signed-attachment-urls) reads `ex`, `is`, `hm`, and `uc` under its own rules.

The Media Proxy does not redirect alternative spellings to a canonical URL.

A Boolean is true only for case-insensitive `true` or the exact value `1`. Every other value, including `false` and `0`, is false. [Transformations](/media-proxy/transformations/) defines dimensions, formats, quality values, and animation flags.

## Byte ranges

This contract controls every range the Media Proxy resolves itself. A range is recognised only when its unit is the exact lowercase `bytes`, so `bytes=0-99` selects an interval and `BYTES=0-99` is ignored. Surrounding spaces and tabs are trimmed. A range containing a comma names multiple ranges and is ignored. A malformed range is ignored and produces the complete representation. `HEAD` applies the same contract as `GET`.

| Value | Description |
| --- | --- |
| `bytes={start}-{end}` | Selects the inclusive interval and clamps an end beyond the representation |
| `bytes={start}-` | Selects from start through the final byte |
| `bytes=-{length}` | Selects the final length bytes and selects the complete representation when length is larger |

A reversed range, a zero-length suffix, a start outside the representation, or any range over an empty representation returns 416 with `Content-Range: bytes */{size}` and `Accept-Ranges: bytes`. A satisfiable range returns 206, `Accept-Ranges: bytes`, and exact `Content-Range` and `Content-Length` values.

### Ranges on the signed external route

The route forwards a range to the origin only when no transformation is requested. It sends the range verbatim when the value after `bytes=` is non-empty and every byte of it is an ASCII graphic character. A multiple range therefore reaches the origin, and the origin decides how to answer it. A value with a space anywhere is dropped, and no range is sent. The route relays the origin partial response with the origin `Content-Range` unchanged.

A range on a transformed response selects bytes from the result and returns 206 or 416. Without a transformation, an origin that ignores the range can produce a complete 200 response. Always check the response status and range headers.

:::caution[A mislabelled SVG reaches the client as bytes]
An origin that answers a forwarded range with SVG bytes under another media type can produce a 206 containing raw SVG.
:::

Content disposition follows the media type the origin declared, so SVG mislabelled as an image or video media type is served inline.

## Representation headers

| Field | Type | Description |
| --- | --- | --- |
| Content-Type<sup>1</sup> | string | The detected or selected media type, or `text/plain; charset=utf-8` for a JavaScript type outside `static` mode |
| Content-Length?<sup>2</sup> | integer | The selected body length, including on HEAD |
| Content-Range? | string | Present on 206 as `bytes {start}-{end}/{size}` and on 416 as `bytes */{size}` |
| Content-Disposition?<sup>3</sup> | string | The route-selected inline or attachment disposition |
| Accept-Ranges | string | The literal `bytes` on every media representation |
| Access-Control-Allow-Origin? | string | The literal `*` on media responses. Under `enforce` it is the request origin, or absent with no `Origin` |
| Cache-Control<sup>4</sup> | string | The browser cache policy for the representation |
| CDN-Cache-Control | string | The matching shared cache policy |
| Vary | string | The literal `Accept-Encoding`, or `Accept-Encoding, Origin` under `enforce` |
| X-Content-Type-Options | string | The literal `nosniff` |
| X-Robots-Tag<sup>5</sup> | string | The indexing policy |
| X-Fluxer-Version | string | The serving Fluxer build identifier, or `dev` when none is configured |

<sup>1</sup> Fixed at `text/css; charset=utf-8` on the theme route. A 416 response has none

<sup>2</sup> Counts the selected bytes, so a 206 advertises the range length. Omitted from a streamed [signed external](/media-proxy/routes/#get-signed-external-media) response whose origin declared no length

<sup>3</sup> Present on the attachment, image asset, and signed external routes, absent on the theme and entrance sound routes, absent on every `static` mode read, and omitted when an image asset falls back to its original bytes after a failed transcode

<sup>4</sup> An audio or video representation appends `no-transform`. A route-produced error uses `no-store` instead

<sup>5</sup> The literal `noindex, nofollow, nosnippet, noimageindex, notranslate, max-snippet:0, max-image-preview:none, max-video-preview:0`. Present in `mp` and `upload` mode, and a `static` mode endpoint removes it

Every successful media representation uses `Cache-Control: public, max-age=31536000` and `CDN-Cache-Control: public, max-age=31536000`. An audio or video representation adds `no-transform` to `Cache-Control` only. No response repeats its policy in an `Expires` header. [Cache policies](/media-proxy/responses-and-limits/#cache-policies) lists every cache policy this surface sets, including the responses that set none.

The upload relay is the only route that returns an `ETag`, and it relays the object storage value for the stored object. No read route sends an `ETag` or a `Last-Modified`, so a cache revalidates a representation by fetching it again.

:::note[External media is cached for a year too]
The signed path is derived from the target URL, so the bytes behind one unchanged target are cached for a year by browsers under `Cache-Control` and by shared caches under `CDN-Cache-Control`.
:::

A 416 response has `Accept-Ranges`, `Content-Range`, `Access-Control-Allow-Origin`, `Vary`, and `X-Robots-Tag`, with no `Content-Type` and no cache policy. Its body is empty. The [cross-origin read policy](#cross-origin-reads) sets its `Access-Control-Allow-Origin` and `Vary` the same way it does on a 200.

## Content detection

Use the response's `Content-Type`. It can differ from the filename extension or the origin's declared type.

An endpoint in any mode other than `static` replaces a `Content-Type` that names a JavaScript media type with `text/plain; charset=utf-8`, at any status. It splits the value at every comma and reads each part on its own. In each part it skips leading spaces and tabs, then reads the type up to the first space, tab, semicolon, or opening parenthesis. The header is replaced when that type in any part equals one of these, ignoring ASCII case: `application/ecmascript`, `application/javascript`, `application/x-ecmascript`, `application/x-javascript`, `text/ecmascript`, `text/javascript`, `text/javascript1.0` through `text/javascript1.5`, `text/jscript`, `text/livescript`, `text/x-ecmascript`, and `text/x-javascript`. So `text/javascript x`, `TEXT/JAVASCRIPT (x)`, and `video/mp4, text/javascript` are replaced, and `text/javascript1.6` and `text/javascriptx` are left as they are.

The endpoint also replaces a `Content-Type` that is not visible ASCII, at any status. A 200 or 206 with no `Content-Type`, or with one that is empty or only spaces and tabs, gets `text/plain; charset=utf-8` too. A response with any other status and no `Content-Type` keeps none, so a 416 and the empty-body method rejection of a registered path still have no `Content-Type`. `Content-Disposition` is unchanged, so it still names the original filename.

A `static` endpoint is the one endpoint that changes no `Content-Type`. An `upload` or `relay` endpoint applies the same replacement an `mp` endpoint does, which is what makes the reference self-hosted deployment safe, because it serves `endpoints.media` from an `upload` endpoint.

## Content disposition

Disposition follows the resolved media type. An image other than SVG and a video are served inline. Every other type, including SVG and PDF, is served as an attachment. An explicit `download` request forces attachment disposition on every route that accepts the parameter.

Use the filename from `Content-Disposition` when saving a response. Transformations can change its extension, and non-ASCII filenames can use the `filename*` parameter.

:::caution[A scriptable document is never inline]
The attachment, image asset, and signed external routes rasterise SVG to WebP, so a browser does not execute the document in the Media Proxy origin.
:::

On a non-transforming attachment read, an `mp` endpoint rasterises SVG to lossless WebP. A transforming request uses the `format` and `quality` it was given, and an image asset path uses `high` when the request gives no `quality`, except for animated WebP output, which uses `auto`. A `static` mode endpoint serves the original bytes.
