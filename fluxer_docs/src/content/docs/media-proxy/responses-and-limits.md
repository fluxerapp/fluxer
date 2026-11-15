---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Responses and limits
description: Media Proxy error responses, size limits, deadlines, cache policies and range headers.
---

The Media Proxy has no request rate limit, only the size limits and deadlines below.

## Media error response

A failed request returns an English reason phrase as `text/plain; charset=utf-8`, not the JSON [error response](/http-api/#error-response). `HEAD` returns the same status and headers with an empty body. Branch on the status and don't parse the body, because the phrases can change.

| Status | Body | Condition |
| --- | --- | --- |
| 400 | Bad Request | A storage key, dimension, format, external path or relay `partNumber` is invalid, a transformation fails, or the relay couldn't read the whole upload body from the client |
| 401 | Unauthorized | An external path signature, relay capability or internal bearer token is missing, invalid or expired |
| 403 | Forbidden | A relay capability doesn't match the request, or `/_metrics` is requested from a non-loopback address |
| 403 | Forbidden | The [cross-origin read policy](/media-proxy/overview/#cross-origin-reads) refuses the `Origin` |
| 404 | Not Found | The path or object doesn't exist, or a read is sent to a `relay` endpoint |
| 404 | This content is no longer available. | The [signed attachment URL policy](/media-proxy/overview/#signed-attachment-urls) is `enforce` and refuses the read, or the object is missing |
| 405 | Method Not Allowed | A method other than `GET` or `HEAD` is sent to a read route or an unrouted path |
| 405 | Empty, with an `Allow` header | A wrong method is sent to a registered path such as `/_health` or `/v1/relay/{key}` |
| 413 | Payload Too Large | An object, external body, upload body or internal request body is over its limit |
| 500 | Transcode Failed | An image asset transcode failed and the source can't be shown directly |
| 500 | Internal Server Error | A relay spool write to the endpoint's own disk failed |
| 502 | Bad Gateway | An object store or external origin request failed, redirected more than five times or hit a deadline |
| 503 | Service Unavailable | The relay spool is full, an external buffer couldn't be allocated or `/_metadata` got a 429 from the origin |
| 504 | Gateway Timeout | No transformation slot was free or the transformation deadline passed |

For signed external media, the Media Proxy passes these origin statuses through with the body `Upstream fetch failed`, and it doesn't forward the origin's own body or headers.

| Origin status | Result |
| --- | --- |
| 400, 401, 403, 404, 405, 406, 408, 409, 410, 411, 412, 413, 414, 415, 416, 428, 429 | The same status |
| Any other unsuccessful status | 502 |

For `/_metadata`, the body is the reason phrase of the origin status.

## Request and media limits

| Limit | Value | Over the limit |
| --- | --- | --- |
| Proxied or stored media | 500 MiB | 413 |
| Decoded signed external target URL | 8,192 bytes | 400 |
| External redirects | 5 | 502 |
| Decoded image width or height | 16,384 pixels | See [transformation limits](/media-proxy/transformations/#transformation-limits) |
| Decoded image area | 268,435,456 pixels | See [transformation limits](/media-proxy/transformations/#transformation-limits) |
| Animated input frames | 20,000 | See [transformation limits](/media-proxy/transformations/#transformation-limits) |
| Animated input decoded pixels | 1,073,741,824 across all frames | See [transformation limits](/media-proxy/transformations/#transformation-limits) |
| Upload relay body | The smaller of the authorized size and the endpoint limit, which defaults to 500 MiB | 413 |
| Internal `/_metadata`, `/_sniff`, `/_thumbnail` and `/_frames` body | About 668 MiB | 413 |

Each redirect target goes through the same URL length limit and the public address check, which blocks non-public IP addresses and every port except 80 and 443.

## Deadlines

| Deadline | Default |
| --- | --- |
| Outbound request timeout | 30,000 ms |
| Transformation | 15,000 ms |
| Upload relay object storage write | 900,000 ms |

The outbound request timeout is the connect and total timeout for object storage requests and the total timeout for external fetches. An external fetch has a fixed 3,000 ms connect timeout. A streamed body also ends when the gap between two chunks passes the timeout. It also ends if the whole transfer takes longer than the timeout plus one second for every 16 KiB, or if it runs short of or past its `Content-Length`. When a streamed body fails after the headers are sent, the response ends early and can be shorter than its `Content-Length`.

## Cache policies

A successful media response, including signed external media, has `Cache-Control: public, max-age=31536000` and the same `CDN-Cache-Control`. Audio and video add `no-transform` to `Cache-Control` only.

No read route sends `ETag` or `Last-Modified`. A CDN or browser keeps serving a deleted or replaced object until its cache entry is purged or expires.

Under the `report` or `enforce` signed attachment URL policy, an attachment read with a valid signature gets `max-age` set to the seconds left on that signature in both headers. A data package URL never expires and gets one day (`max-age=86400`).

Error responses use `Cache-Control: no-store`, except a 416, which has no cache policy. A successful relay response has none either.

## Range response headers

| Response | Headers |
| --- | --- |
| 200 | `Accept-Ranges: bytes`, `Content-Type` and the exact `Content-Length` |
| 206 | `Content-Range` with the selected interval over the full size, and `Content-Length` with the selected byte count |
| 416 | `Content-Range: bytes */{size}`, `Accept-Ranges: bytes` and an empty body |
| [Transformed media](/media-proxy/overview/#transformed-representations) | No `Accept-Ranges`, and always the whole body |

Signed external media leaves out `Content-Length` when the origin sent none, and a forwarded range keeps the origin's `Content-Range` and `Content-Length`. `HEAD` follows the same rules with an empty body. See [byte ranges](/media-proxy/overview/#byte-ranges) for the `Range` values the Media Proxy accepts.
