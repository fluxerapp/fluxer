---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Attachment uploads
description: Upload attachments and claim them in a message.
---

Send attachments as `files[n]` parts in a [multipart message](/http-api/#request-body-formats), or upload them before creating the message. Pre-uploading has four steps:

1. Request an upload plan.
2. Send the bytes to its upload URLs.
3. Complete the upload if it is multipart.
4. Include its `upload_filename` when creating or editing a message.

:::caution[Pre-uploads can be switched off]
When [instance features](/http-api/instance/#instance-features-object) reports `presigned_attachment_uploads` false, use inline multipart uploads. Plan and completion requests return 403 `FEATURE_TEMPORARILY_DISABLED`.
:::

## Requesting an upload plan

[Request attachment upload URLs](/http-api/messages/#request-attachment-upload-urls) accepts up to 10 files. Declare each file's ID, name, exact byte count and content type. The endpoint reference defines the fields, permissions and file size limits.

Keep the returned plan, including its `upload_filename`. Use the returned `content_type`, which Fluxer derives from the file name and which can therefore differ from the declared value. The `upload_mode` determines the next steps.

### Upload modes

| Value | Selected for | Added fields |
| --- | --- | --- |
| singlepart<sup>1</sup> | A declared `file_size` of at most 10485760 bytes | `upload_url` |
| multipart<sup>2</sup> | A declared `file_size` above 10485760 bytes | `upload_id`, `part_size`, and `parts` |

<sup>1</sup> Stored as soon as its `PUT` succeeds, so it is never sent to [Complete attachment upload](/http-api/messages/#complete-attachment-upload)

<sup>2</sup> Each `parts` entry has a one-based `part_number` and its own `upload_url`, and the array is ordered by ascending part number

## Part geometry

Use the `part_size` and `parts` returned in the upload plan. Every part must contain exactly `part_size` bytes except the last, which contains the remainder. Do not calculate a different part layout.

## Transferring the bytes

Send `PUT` requests to the returned `upload_url` values without an `Authorization` header. The URLs can target storage or the [upload relay](/media-proxy/upload-relay/). Do not rewrite their paths or query strings.

Send exactly the declared byte count. The relay returns 413 for an oversized body and 401 for a missing, invalid, or expired capability. It also returns 413 for a body above its own limit, which is 500 MiB by default.

A singlepart transfer sends the whole file with the entry's `content_type` as its `Content-Type` header. A multipart transfer sends each part separately.

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

[Complete attachment upload](/http-api/messages/#complete-attachment-upload) finishes from 1 through 10 multipart uploads. Send the `upload_filename` and `upload_id` from each plan after all its parts succeed. Do not send a part list or entity tags.

Completion checks permissions and file size limits again. See the endpoint's [response table](/http-api/messages/#complete-attachment-upload) for errors. If completion aborts the upload, request a new plan and upload the file again.

## Claiming the upload

Include the `upload_filename` in a [pre-uploaded attachment](/http-api/messages/#pre-uploaded-attachment-object) when calling [Create message](/http-api/messages/#create-message) or [Modify message](/http-api/messages/#modify-message). Uploading alone does not create a message or emit a Gateway event.

An upload is bound to the identity and the channel that planned it, and a key is single use. A key the authenticated identity does not own, a key planned for another channel, and a key an attachment has already consumed each return 400 `INVALID_FORM_BODY` with `UPLOADED_ATTACHMENT_NOT_FOUND` on `attachments.{index}.upload_filename`. Where the object is absent from storage, which is what an untransferred plan leaves behind, the claim returns the same status with `FILE_NOT_FOUND` on the same path.

:::caution[There is no resume operation]
Once a capability expires it cannot be refreshed, and a plan cannot be re-read. A client that loses its plan, or whose part capabilities expire mid transfer, requests a new plan for the file and starts again.
:::

## Stream previews

A stream preview is a JPEG attached to a voice connection. It uses the user-only [Streams API](/http-api/streams/), not the attachment flow. Follow its [access rules](/http-api/streams/#access-rules) to read or change a preview.

:::note[Treat a stored preview as publisher-asserted]
A preview does not prove that its connection is currently publishing a stream.
:::

Use [Upload stream preview](/http-api/streams/#upload-stream-preview) to send the image as base64 in JSON. Canonical base64 uses the standard alphabet, a length divisible by four and at most two trailing `=` characters. Decoding and re-encoding must produce the same string. Invalid encoding returns 400 `INVALID_STREAM_THUMBNAIL_PAYLOAD`.

Alternatively, [request an upload URL](/http-api/streams/#create-stream-preview-upload-url) and send the JPEG with `PUT`. Use the returned `content_type`, send at most `max_bytes` bytes and upload before `expires_at`. The URL can be reused until it expires.

Send a valid JPEG of at most 1000000 bytes. [Get stream preview](/http-api/streams/#get-stream-preview) reads it, and [Delete stream preview](/http-api/streams/#delete-stream-preview) removes it.

A preview expires one day after an inline upload or the request that issued its upload URL. Reusing the URL does not extend the preview's lifetime. An expired preview returns an empty 404.

## Failures

API requests return the standard [error response](/http-api/#error-response). Relay requests return [plain-text media errors](/media-proxy/responses-and-limits/#media-error-response). Direct storage errors use the storage provider's format.

Each API endpoint documents its [rate limit](/topics/rate-limits/). The relay has no request-count rate limit.
