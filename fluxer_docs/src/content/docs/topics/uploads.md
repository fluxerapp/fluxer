---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Attachment uploads
description: Upload message attachments and stream previews before you use them.
---

You can send attachments as `files[n]` parts in a [multipart message](/http-api/#request-body-formats), or upload them first and attach them by their upload key (`upload_filename`). Uploading first takes four steps.

1. Request an upload plan.
2. Send the bytes to its upload URLs.
3. Complete the upload if it is multipart.
4. Attach the upload by its upload key when you create or edit a message or start a thread.

Uploading first needs `presigned_attachment_uploads` set to true in [instance features](/http-api/instance/#instance-features-object). When it is off, the plan and completion requests return 403 `FEATURE_TEMPORARILY_DISABLED`.

## Request an upload plan

Call [Request attachment upload URLs](/http-api/messages/#request-attachment-upload-urls) with up to 10 files, each with its exact byte count. Each file in the plan has an `upload_mode` that decides which fields it gets.

| Value | When | Added fields |
| --- | --- | --- |
| singlepart | A `file_size` of at most 10485760 bytes (10 MiB) | `upload_url` |
| multipart | A `file_size` above 10485760 bytes | `upload_id`, `part_size` and `parts` |

Each `parts` entry has a `part_number` starting at 1 and its own `upload_url`.

## Upload the bytes

Send a `PUT` to each `upload_url` exactly as returned, with no `Authorization` header. Fluxer picks `content_type` from the file name, and a singlepart upload must send the whole file with it as `Content-Type`. A multipart upload sends each part to its own URL, with exactly `part_size` bytes in every part except the last.

An upload URL points to storage or to the [upload relay](/media-proxy/upload-relay/), and it expires after the time below.

| Upload URL | Lifetime |
| --- | --- |
| Storage, singlepart | 5 minutes |
| Storage, multipart part | 1 hour |
| Upload relay | 15 minutes by default, and the instance can change it |

Keep upload URLs out of logs, because anyone who has one can upload to it until it expires. When a URL expires, request a new plan and upload the file again.

## Complete a multipart upload

After every part has uploaded, call [Complete attachment upload](/http-api/messages/#complete-attachment-upload) with the `upload_filename` and `upload_id` of up to 10 uploads. Fluxer checks permissions and the file size limit again. If completion fails because no parts arrived, the parts are too large or storage can't finish the upload, Fluxer cancels the upload and you need a new plan.

## Attach the upload

Put the `upload_filename` in a [pre-uploaded attachment](/http-api/messages/#pre-uploaded-attachment-object) when you call [Create message](/http-api/messages/#create-message), [Modify message](/http-api/messages/#modify-message) or [Start thread](/http-api/threads/#start-thread) in a forum or media channel.

An upload key works once, for the user and channel that requested it. Using a key that belongs to someone else, was requested for another channel or was already used returns 400 `INVALID_FORM_BODY` with `UPLOADED_ATTACHMENT_NOT_FOUND`. A key whose bytes never uploaded returns the same error with `FILE_NOT_FOUND`.

## Stream previews

A stream preview is a JPEG of at most 1000000 bytes that shows for a voice stream. You can upload it with either of these [Streams API](/http-api/streams/) endpoints.

- [Upload stream preview](/http-api/streams/#upload-stream-preview) takes the image as canonical base64 in JSON, using the standard alphabet with padding. Other encodings return 400 `INVALID_STREAM_THUMBNAIL_PAYLOAD`.
- [Create stream preview upload URL](/http-api/streams/#create-stream-preview-upload-url) returns a URL. Send the JPEG to it with `PUT` and the returned `content_type` before `expires_at`, and keep it at or under `max_bytes`.

A preview expires one day after you upload it or request its upload URL, and after that [Get stream preview](/http-api/streams/#get-stream-preview) returns 404.
