---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Attachment uploads
description: The pre-upload plan, its two modes, and how a client claims the result.
---

Fluxer accepts an attachment inline as a `files[n]` part of a [multipart request](/http-api/#request-body-formats), or pre-uploaded before the message exists. The pre-upload operations and objects live on the [Messages resource](/http-api/messages/).

The flow has two modes and the server chooses between them from the declared byte count. A file of 10485760 bytes or less, the 10 MiB threshold, is planned as a singlepart upload and is finished as soon as its bytes are stored. A larger file is planned as a multipart upload and needs an explicit completion call. Both modes end the same way, by naming the resulting `upload_filename` in an ordinary message operation.

:::caution[Pre-uploads can be switched off]
When a deployment disables them, the plan request and the completion request both answer 403 `FEATURE_TEMPORARILY_DISABLED` ahead of the channel, permission, and size checks, and the [instance features object](/http-api/instance/#instance-features-object) reports `presigned_attachment_uploads` false. A client falls back to the inline multipart path.
:::

## Requesting an upload plan

[Request attachment upload URLs](/http-api/messages/#request-attachment-upload-urls) takes from 1 through 10 attachment declarations. Each is a client-chosen `id`, a `filename`, an exact `file_size` in bytes, and a `content_type`. A user session credential and a bot token are accepted, and an OAuth2 bearer credential is rejected with 403 `ACCESS_DENIED`.

The channel must support messages, and any other channel type returns 400 `CANNOT_SEND_MESSAGES_IN_NON_TEXT_CHANNEL`. A guild channel additionally requires [SEND_MESSAGES](/http-api/permissions/) and [ATTACH_FILES](/http-api/permissions/), returning 403 `MISSING_PERMISSIONS` otherwise, and a caller under a communication timeout receives 403 `COMMUNICATION_DISABLED`.

Fluxer then checks every declared size on its own against the `max_attachment_file_size` limit resolved for the caller and the guild. A size above it returns 400 `FILE_SIZE_TOO_LARGE` with the resolved ceiling before anything is planned. That limit defaults to 26214400 bytes, the 25 MiB non-premium allowance, and to 524288000 bytes, the 500 MiB premium allowance. A bot credential is clamped to 52428800 bytes, the 50 MiB bot ceiling, even when the resolved limit is higher.

The response returns one entry for each declaration, in request order, discriminated by `upload_mode`. Every entry has the `id` from the request, the `filename`, the opaque `upload_filename` to claim later, the `file_size`, and the `content_type` the server derived from the filename. The issued capability authorises that derived media type, and the stored object has it.

### Upload modes

| Value | Selected for | Added fields |
| --- | --- | --- |
| singlepart<sup>1</sup> | A declared `file_size` of at most 10485760 bytes | `upload_url` |
| multipart<sup>2</sup> | A declared `file_size` above 10485760 bytes | `upload_id`, `part_size`, and `parts` |

<sup>1</sup> Stored as soon as its `PUT` succeeds, so it is never sent to [Complete attachment upload](/http-api/messages/#complete-attachment-upload)

<sup>2</sup> Each `parts` entry has a one-based `part_number` and its own `upload_url`, and the array is ordered by ascending part number

## Part geometry

Fluxer derives multipart geometry from the file size. The part size is the declared size divided by 20, rounded up to a whole mebibyte, and never below 10485760 bytes. Every part is exactly `part_size` bytes except the last, which is the remainder.

A plan is bounded at 10,000 parts. One that would need more returns 400 `FILE_SIZE_TOO_LARGE` before the storage multipart upload is opened. The resolved file size limit is the only binding constraint.

## Transferring the bytes

Each `upload_url` is a `PUT` target with its own authorisation in its query string. A direct storage URL has the object store's own signature and a relay URL has the signed relay capability in its `t` parameter, so neither shape reads an `Authorization` header.

The direct storage capability signs the exact byte count, so a `PUT` of any other length is rejected. A relay capability bounds the length at the same value and answers 413 above it, and the relay applies its own body ceiling, 500 MiB by default, on top of that. A transfer that declares no `Content-Length` is spooled to the smaller of the two bounds and answers 413 past it. A relay request whose capability is missing, malformed, or expired answers 401. A client sends exactly the authorised byte count either way.

A singlepart transfer sends the whole file and must send the entry's `content_type` as its `Content-Type` header. A relay capability takes the media type from the capability and ignores the header. A multipart part transfer sends only that part's bytes and has no signed media type.

The instance decides per request whether to relay. It resolves the caller's country from the client IP address and issues a direct storage URL only when that country is on the deployment's direct-upload list. Every other caller, including one whose geolocation lookup fails, receives a URL on the [upload relay](/media-proxy/upload-relay/). A client treats both shapes the same way and MUST NOT parse, rewrite, or reorder the query string of either.

### Capability lifetimes

| Capability | Lifetime |
| --- | --- |
| Direct singlepart upload URL | 5 minutes |
| Direct multipart part URL | 1 hour |
| Relay URL of either kind<sup>1</sup> | The relay token lifetime the deployment configures, 900 seconds by default |

<sup>1</sup> A relay expiry is exclusive, so the capability is already rejected at its expiry second

:::caution[The complete URL is the credential]
An issued upload URL authorises writing one object or one part. A client treats the whole URL, query string included, as secret until it expires, and keeps it out of logs, referrers, and redirect targets.
:::

## Completing a multipart upload

[Complete attachment upload](/http-api/messages/#complete-attachment-upload) finalises from 1 through 10 multipart uploads. Each entry names the `upload_filename` and the `upload_id` from the plan. The caller sends no part list and no entity tags. The server lists the parts the storage backend has already accepted, sorts them by part number, and assembles them in that order.

The channel, permission, and communication checks of the plan request run again, and the operation answers 403 `FEATURE_TEMPORARILY_DISABLED` when pre-uploads are switched off.

Two failures are reported as [validation error object](/http-api/#validation-error-object) entries on a 400 `INVALID_FORM_BODY` response.

| Code | Path | Condition |
| --- | --- | --- |
| UPLOADED_ATTACHMENT_NOT_FOUND<sup>1</sup> | `uploads.{index}.upload_filename` | The key is not a pending multipart upload of the authenticated identity in this channel |
| NO_UPLOADED_PARTS_TO_FINALIZE<sup>2</sup> | `parts` | The storage backend holds no part for the upload |

<sup>1</sup> An upload another identity planned, one planned for another channel, one planned as singlepart, and one a message has already consumed are all reported this way, so a caller cannot probe another identity's upload state

<sup>2</sup> The multipart upload is aborted before the error is returned

Fluxer then sums the listed part sizes. A total above the resolved file size limit aborts the upload and returns 400 `FILE_SIZE_TOO_LARGE`. A storage failure during assembly aborts it as well. An aborted upload discards its parts, and its `upload_filename` can never be claimed. A client requests a new plan for the file.

A client never sends a singlepart upload to this operation.

## Claiming the upload

Nothing before this step creates a message, changes a channel, or emits a Gateway Dispatch. The attachment exists only once a [Create message](/http-api/messages/#create-message) or [Modify message](/http-api/messages/#modify-message) request has the `upload_filename` in a [pre-uploaded attachment](/http-api/messages/#pre-uploaded-attachment-object) entry.

An upload is bound to the identity and the channel that planned it. A key the authenticated identity does not own, a key planned for another channel, and a key an attachment has already consumed each return 400 `INVALID_FORM_BODY` with `UPLOADED_ATTACHMENT_NOT_FOUND` on `attachments.{index}.upload_filename`. A key whose object is absent from storage, which is what an untransferred plan leaves behind, returns the same status with `FILE_NOT_FOUND` on the same path.

A key is single use, and a second claim of the same key returns the same `UPLOADED_ATTACHMENT_NOT_FOUND` entry.

:::caution[There is no resume operation]
A capability that expires cannot be refreshed and a plan cannot be re-read. A client that loses its plan, or whose part capabilities expire mid transfer, requests a new plan for the file and starts again.
:::

## Stream previews

A stream preview is a still image attached to one voice connection. It does not use the attachment flow. A preview belongs to the voice connection that publishes the stream. The [stream key](/http-api/streams/#stream-key) names the scope, the channel, and that connection ID. Every preview operation is user-only.

Uploading the image, issuing an upload capability, and deleting the preview each require the caller to hold a voice state in that channel whose connection ID matches the key. In a guild channel they require the [STREAM](/http-api/permissions/) permission as well. Reading the preview requires only access to the channel, and [CONNECT](/http-api/permissions/) in a guild channel, so any member who can join can read it.

:::note[Treat a stored preview as publisher-asserted]
A voice state has no flag the check reads, so a caller can upload and read a preview for a connection that is publishing nothing.
:::

[Upload stream preview](/http-api/streams/#upload-stream-preview) posts the image inline. Its JSON body has the `channel_id` the connection is in, the base64-encoded image in `thumbnail` of 1 through 2000000 characters, and an optional `content_type` of 1 through 64 characters. A `thumbnail` that is not canonical base64 returns 400 `INVALID_STREAM_THUMBNAIL_PAYLOAD`. Canonical base64 here means the standard alphabet, a length that is a multiple of four, at most two trailing `=`, and decoded bytes that re-encode to the string the request sent. A `content_type` containing `jpeg` or `jpg` in any case is accepted without inspecting the bytes. Every other value, an absent field included, is accepted only when the decoded bytes begin with `FF D8` and end with `FF D9`. A failure returns 400 `PREVIEW_MUST_BE_JPEG`. Decoded bytes above 1000000 return 400 `FILE_SIZE_TOO_LARGE`.

The operation answers 204 once the image is accepted. Fluxer absorbs a transient storage failure, so a 204 confirms acceptance alone.

[Create stream preview upload URL](/http-api/streams/#create-stream-preview-upload-url) instead issues a reusable `PUT` capability for the same purpose. Its `content_type` must contain `jpeg` or `jpg` in any case, and every other value returns 400 `PREVIEW_MUST_BE_JPEG`. It answers with `upload_url`, `method` fixed to `PUT`, the `content_type` the client sends, `expires_at`, `expires_in`, and `max_bytes`, which is always 1000000. A direct storage capability lasts one day and a relay capability lasts the relay token lifetime, so `expires_in` differs between the two shapes. The capability writes the same object every time it is used, and a publisher refreshes the thumbnail without asking for a new URL.

Nothing inspects the bytes written through that capability. A relay capability still refuses a declared length above `max_bytes` with 413, and a direct storage capability enforces nothing beyond its signed media type. A publisher encodes a valid JPEG of at most 1000000 bytes itself.

[Get stream preview](/http-api/streams/#get-stream-preview) returns the current image bytes with `Cache-Control: no-store, private`, and [Delete stream preview](/http-api/streams/#delete-stream-preview) removes it. The stored preview record expires one day after the inline upload that wrote it, or one day after the call that issued the capability, and using a capability again does not extend it. A read past that point answers an empty 404 even when the object is still in storage.

## Failures

Every HTTP API operation in this flow returns the ordinary [error response](/http-api/#error-response) envelope. A relay request answers with the plain-text [media error response](/media-proxy/responses-and-limits/#media-error-response) that the [Media Proxy API](/media-proxy/overview/) defines. A direct storage URL returns whatever the storage backend returns, in that backend's own format.

The relay has no request-count rate limit. [Request attachment upload URLs](/http-api/messages/#request-attachment-upload-urls) and [Complete attachment upload](/http-api/messages/#complete-attachment-upload) share the `channel:attachment:upload::channel_id` bucket, which permits 10 requests per 10 seconds for each authenticated identity and channel ID. Each stream preview operation declares its own bucket. [Rate limits](/topics/rate-limits/) defines both.
