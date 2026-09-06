---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Responses and limits
description: Media Proxy statuses, error bodies, size bounds, work admission, deadlines, and cache policies.
---

Almost every failed Media Proxy request answers with a short `text/plain` reason phrase and nothing else. Every bound below is a resource bound, because the Media Proxy has no request-count rate limit.

## Media error response

An unsuccessful response body is an English reason phrase under the content type `text/plain; charset=utf-8`. The JSON [HTTP API error response](/http-api/#error-response) never appears here. `HEAD` returns the same status and headers with an empty body. The method rejection of a registered path<sup>2</sup> and a locally unsatisfiable range have neither a body nor a `Content-Type`.

| Status | Body | Condition |
| --- | --- | --- |
| 400 | Bad request | An invalid storage key, dimension, format, or external path, an unparsable relay `partNumber`, a failed attachment or external transformation, or a relay upload the endpoint could not take<sup>1</sup> |
| 401 | Unauthorized | An invalid signed external path signature, a missing, malformed, or expired relay capability, or a missing or invalid internal bearer token |
| 403 | Media access denied | The media access allowlist rejected the client address |
| 403 | Forbidden | A relay capability presented for another bucket, key, method, `uploadId`, or `partNumber`, or `/_metrics` requested from a non-loopback address |
| 404 | Not found | A `GET` or `HEAD` of an unrouted path, an object that does not exist, or a relay `PUT` outside `upload` mode |
| 405 | Method not allowed<sup>2</sup> | A method other than `GET` or `HEAD` on a read route or on an unrouted path |
| 413 | Payload too large | A stored object, external body, upload body, or internal request body beyond its bound |
| 500 | Transcode failed | An image asset transcode failed and the source is not directly displayable |
| 500 | Internal server error | A relay spool write to the endpoint's own disk failed |
| 502 | Bad gateway | An object store read or write failed, an external origin could not be used, or an outbound socket deadline or a relay object storage write deadline expired |
| 503 | Service unavailable | The upload relay spool budget is exhausted, an external buffer reservation or allocation failed, or an external origin answered `/_metadata` with 429<sup>3</sup> |
| 504 | Gateway timeout | A transformation admission slot was unavailable or the transformation deadline expired |

<sup>1</sup> The relay also answers 400 when the client connection fails part way through the body

<sup>2</sup> The six registered paths, `/_health`, `/_metrics`, `/_metadata`, `/_thumbnail`, `/_frames`, and `/v1/relay/{key}`, answer an unaccepted method with an empty body, no `Content-Type`, and an `Allow` header

<sup>3</sup> `/_metadata` is the only endpoint that remaps an origin 429. The signed external read route retains 429 as 429

A retained external origin status reaches the client as an upstream fetch failure on the signed external read route and as the canonical reason phrase of its status on `/_metadata`. An object store error that maps to no case above uses the canonical reason phrase of its status.

Every error a route produces uses `Cache-Control: no-store` and the standard [security headers](/media-proxy/overview/#representation-headers). Three failures have the security headers and set no cache policy, and [Cache policies](#cache-policies) names them. No plain-text error has CORS headers unless it came from the [upload relay](/media-proxy/upload-relay/), and no error has `Retry-After` or a request identifier.

### Handling contract

The set of reason phrases is open. A client MUST branch on the HTTP status and MUST NOT parse, compare, or pattern-match the body, because a phrase can be reworded and a new condition can introduce one without a version change. A client MUST NOT expect a JSON body on a failure, and MUST NOT expect a failure to name the key, parameter, or field that caused it.

## Status registry

Every `HEAD` response has an empty body, so the Body column describes `GET`, `PUT`, and `POST`.

| Status | Body | Condition |
| --- | --- | --- |
| 200<sup>1</sup> | Media bytes, or an empty `HEAD` or relay `PUT` body | A complete representation was served or a relay write succeeded |
| 206 | Selected media bytes for `GET` and empty for `HEAD` | One byte range is satisfiable |
| 400 | [Media error response](#media-error-response) | A path, query, target, transformation, or relay request is invalid |
| 401 | [Media error response](#media-error-response) | An external path signature, a relay capability, or an internal token is invalid |
| 403 | [Media error response](#media-error-response) | The media access policy denied the request, a relay capability does not match the request, or an external origin returned 403 |
| 404 | [Media error response](#media-error-response) | The route or object does not exist, the relay is not served here, or an external origin returned 404 |
| 405<sup>2</sup> | [Media error response](#media-error-response) | The route rejects the method, or an external origin returned 405 |
| 406, 408, 409, 410, 411, 412, 414, 415, 428, 429<sup>3</sup> | [Media error response](#media-error-response) | An external origin returned that status |
| 413 | [Media error response](#media-error-response) | A source, external response, or upload exceeds its applicable bound |
| 416<sup>4</sup> | empty locally, otherwise [Media error response](#media-error-response) | A local range is unsatisfiable, or an external origin returned 416 |
| 500 | [Media error response](#media-error-response) | An image asset transcode failed with no usable source, or a relay spool write failed |
| 502 | [Media error response](#media-error-response) | An object store, external origin, or upload destination returned an unusable response |
| 503 | [Media error response](#media-error-response) | Upload spool capacity is exhausted, an external buffer reservation or allocation failed, or an external origin answered `/_metadata` with 429<sup>5</sup> |
| 504 | [Media error response](#media-error-response) | Transformation capacity was unavailable or a transformation deadline expired |

<sup>1</sup> Four [internal endpoints](/media-proxy/routes/#operator-and-internal-endpoints) answer 200 with a non-media body. `/_metadata` and `/_frames` answer with JSON, `/_health` with plain text, and `/_metrics` with the Prometheus text exposition

<sup>2</sup> A path served by the read fallback answers with the [media error response](#media-error-response). Each registered path answers with an empty body, no `Content-Type`, and an `Allow` header

<sup>3</sup> A retained external origin status is the only source of these statuses

<sup>4</sup> A locally unsatisfiable range answers with an empty body, `Content-Range: bytes */{size}`, and `Accept-Ranges: bytes`, and has no `Content-Type` and no cache policy

<sup>5</sup> The remap produces a service unavailable response, which is indistinguishable from a budget exhaustion 503

An external origin status of 400, 401, 403, 404, 405, 406, 408, 409, 410, 411, 412, 413, 414, 415, 416, 428, or 429 reaches the client unchanged as an upstream fetch failure. The origin response body and headers are not forwarded. Any other unsuccessful origin status becomes 502.

The Media Proxy evaluates no conditional request header and never redirects a noncanonical target, so it returns no 304 and no 308.

:::note[A retained status describes the external URL]
The third-party origin chose it, and Fluxer passed the status through.
:::

## Request and media limits

Proxied or stored media is limited to 500 MiB, and exceeding that returns 413. The bound applies to a streamed object, a buffered object, an external response body, and any input selected for transformation. When a streamed external body passes the bound only after the response head is committed, the Media Proxy truncates it.

A decoded signed external target URL is limited to 8,192 bytes, and the route follows at most five redirects. A longer URL returns 400. A sixth redirect returns 502, and so does a redirect back to an already visited URL. A redirect target is subject to the same bound and the same address policy as the original URL. Content detection inspects the leading 8,192 bytes of a body.

Buffered external bodies share one endpoint budget of 500 MiB for every [work admission](#work-admission) slot plus 512 KiB. A body the budget cannot cover returns 503, and so does a failed buffer allocation.

Decoded images are limited to 16,384 pixels on either edge and 268,435,456 pixels in total. Animated input is limited to 20,000 frames and 1,073,741,824 decoded pixels across all frames. No configuration changes these bounds. [Transformations](/media-proxy/transformations/#transformation-limits) defines the resulting failure statuses.

The upload relay limits a body to the smaller of the capability's declared maximum and the endpoint's configured body limit, which defaults to the same 500 MiB ceiling and can be configured from 1 byte through 5 GiB. A request that declares no `Content-Length` is spooled to disk first, and spooled bodies share an 8 GiB endpoint budget by default.

An internal `/_metadata`, `/_thumbnail`, or `/_frames` request body is limited to the base64 expansion of the 500 MiB media bound plus 1 MiB. All three answer a larger body with 413.

## Work admission

A transformation first takes an admission slot without waiting. The pool holds one slot for every concurrent native transform plus one for every queued transform. Native transform concurrency defaults to the process parallelism clamped to 2 through 8 and can be configured from 1 through 128. The queue defaults to eight times that concurrency and can be configured from 1 through 8192. When no slot is free the request returns 504 immediately.

Once admitted, the transformation waits for a native transform permit until the transformation deadline. A wait that outlives the deadline also returns 504.

The image branch of `/_thumbnail` follows the same admission and deadline rules. Its video branch takes no admission slot and no permit at all.

A successful transformation stays in the memory cache for 120,000 ms by default, within a 256 MiB total budget and a 64 MiB per-entry budget<sup>1</sup>. External content-type hints are cached for the same interval across at most 4,096 targets.

<sup>1</sup> The per-entry budget is clamped to the total budget, and setting the interval, the total budget, or the per-entry budget to zero disables the transform cache

## Deadlines

| Deadline | Default | Configurable range |
| --- | --- | --- |
| Outbound socket connect and total timeout<sup>1</sup> | 30,000 ms | 0 through 300,000 ms |
| Transformation | 15,000 ms | 1,000 through 120,000 ms |
| Upload relay object storage write<sup>2</sup> | 900,000 ms | 1,000 through 3,600,000 ms |

<sup>1</sup> One value sets both the connect timeout and the total request timeout, and it applies to external origin fetches, to object storage requests, and to the response body streamed back to the client

<sup>2</sup> A relay write with a declared `Content-Length` extends this deadline by one second for every 16 KiB of declared length

The same socket timeout bounds every streamed response body. A streamed stored object and a streamed signed external response terminate when the gap between two body chunks exceeds that timeout. The whole transfer has a second deadline of that timeout plus one second for every 16 KiB of expected length, which is a floor of 16 KiB per second. A body that ends before, or runs past, the advertised `Content-Length` also terminates with an error.

Animated encoding stops adding frames 3,000 ms before the transformation deadline, so the encoder has time to flush what it already holds. The request then succeeds with a shorter animation. The separate 30,000 ms animation bound caps the playback length of the encoded animation, and [Transformations](/media-proxy/transformations/#transformation-limits) defines it.

:::caution[A deadline after the head truncates the body]
A status and its headers are chosen before the body is sent. A streamed object store or external response that fails afterwards terminates the body, so the observed body can be shorter than the advertised `Content-Length`.
:::

## Cache policies

Every successful media representation uses `Cache-Control: public, max-age=31536000` and `CDN-Cache-Control: public, max-age=31536000`. An audio or video representation appends `no-transform` to `Cache-Control` only. Signed external media uses the same policy as stored media.

No route sets `immutable`, `Expires`, `ETag`, or `Last-Modified` on a read response, and no route selects a shorter cache policy. A cache revalidates a representation by fetching it again.

Every route-produced error response uses `Cache-Control: no-store`, and a successful `/_metrics` read uses it too. A 416 response, a successful upload relay response, a media access allowlist rejection, and the empty-body method rejection of a registered path set no cache policy at all.

## Range response headers

A complete media response has `Accept-Ranges: bytes`, the representation `Content-Type`, and an exact `Content-Length`<sup>1</sup>. A 206 additionally sets `Content-Range` to the complete size and `Content-Length` to the selected byte count. A 416 has `Content-Range: bytes */{size}` and `Accept-Ranges: bytes` with an empty body.

<sup>1</sup> A streamed signed external response omits `Content-Length` when the origin declared none

`HEAD` applies the same range contract as `GET` and returns 206 or 416 with an empty body. A forwarded external range is answered with the origin `Content-Range` and `Content-Length` unchanged. [Byte ranges](/media-proxy/overview/#byte-ranges) defines which request ranges are recognised.
