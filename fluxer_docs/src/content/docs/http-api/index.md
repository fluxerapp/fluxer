---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: HTTP API
description: Request format, body representations, shared headers, cross-origin policy, and the error objects.
---

The Fluxer HTTP API is the set of routes a client calls to read and change data. Every route is published under `/v1`, and `/v1` is the only version. The base URL comes from `endpoints.api` in the [instance discovery document](/http-api/instance/#get-instance-discovery), which is served unversioned at `/.well-known/fluxer`.

Every route is also mounted at the root, so a path resolves with or without the prefix. A client MUST use the `/v1` form. [Download stored object](/http-api/downloads/#download-stored-object) is the one exception, and it resolves at the root alone.

Numeric limits such as attachment counts, expression counts, and profile field lengths are instance configuration. Each deployment publishes its current values in its [instance discovery](/http-api/instance/#limit-keys) document, so a client reads them at runtime.

[API conventions](/conventions/) defines the wire notation and the omission and `null` semantics.

## Request format

A JSON request body has `Content-Type: application/json`. A response body is UTF-8 JSON with `Content-Type: application/json` unless the operation states another representation. Message operations and webhook execution accept multipart bodies, and the OAuth2 token operations accept form-encoded bodies.

The API applies no generic byte limit to a request body. An operation that accepts an upload bounds that upload itself.

Every request counts against one in-flight request ceiling for the whole instance. A request that arrives while the instance is at that ceiling returns 503 `SERVICE_UNAVAILABLE` with `Retry-After: 1` before the operation runs. The `/_health`, `/_healthz`, and `/_metrics` probe paths are exempt.

A request to a path that matches no route returns 404 `NOT_FOUND`. A request whose path is registered but not for the request method is answered the same way, and the response has no `Allow` header. Routing is strict, so a trailing slash is significant.

The `GET` registered for a path also serves `HEAD`. A path that registers no `GET` serves no `HEAD` either. A `HEAD` response has the status and headers that `GET` returns, with no body. The request still reports `HEAD` as its method, so the [same-host origin check](#cross-origin-requests) can refuse a `HEAD` that has no `Origin` where the identical `GET` succeeds.

## Request body formats

An operation documents its body under `JSON body`, `Form body`, or `Multipart body` and states any content-type restriction it enforces.

A JSON body is parsed from the raw request text without inspecting `Content-Type`. The instance content filter scans a `POST`, `PUT`, or `PATCH` body that parses as JSON against the banned-phrase and banned-URL blocklists, whatever the header declares. It skips a body whose `Content-Type` contains `multipart/form-data` or `application/x-www-form-urlencoded`. A client MUST send the canonical media type.

Form bodies accept `application/x-www-form-urlencoded` and `multipart/form-data` interchangeably. The three [OAuth2](/http-api/oauth2/) token operations are the only ones that take one. A field that occurs once is a string or file. Repeating the same field name produces an array in occurrence order, and a name ending in `[]` also collects its values into an array.

[Create message](/http-api/messages/#create-message), [Modify message](/http-api/messages/#modify-message), and [Execute webhook](/http-api/webhooks/#execute-webhook) are the only operations that define a multipart body of their own. Each selects the multipart parser when the request `Content-Type` contains `multipart/form-data` and parses the body as JSON otherwise. The three OAuth2 token operations also accept `multipart/form-data`, but read it as an ordinary form body.

| Field | Type | Description |
| --- | --- | --- |
| payload_json?<sup>1</sup> | string | JSON object with the operation's complete message request payload |
| files[n]?<sup>2</sup> | file | Direct attachment file at zero-based index `n` |

<sup>1</sup> An absent `payload_json` is read as an empty object. A value that is not a string, or does not parse as JSON, is rejected with `INVALID_JSON_IN_PAYLOAD_JSON`

<sup>2</sup> `n` is a run of decimal digits, at most 10000 and below the deployment's [`max_attachments_per_message`](/http-api/instance/#limit-keys) limit, which defaults to 10

The legacy names `file` and `file` followed by an index are accepted as file fields as well, and a bare `file` takes the next free legacy index.

Five field-name failures are rejected with their own code. An index outside either bound returns `FILE_INDEX_EXCEEDS_MAXIMUM`. Any other name beginning with `files[` returns `INVALID_FILE_FIELD_NAME`. Two file fields resolving to the same index return `DUPLICATE_FILE_INDEX`. More than one file supplied for one index returns `MULTIPLE_FILES_FOR_INDEX_NOT_ALLOWED`. Where the resolved limit is 0, any file field at all returns `ATTACHMENTS_NOT_ALLOWED_FOR_MESSAGE`.

The `Content-Type` header supplies the multipart boundary. Each part's field name is in `Content-Disposition`. A body the multipart parser cannot read is rejected with `FAILED_TO_PARSE_MULTIPART_FORM_DATA`. A field name the operation does not recognise is ignored, and a `files[n]` part whose value is not a file is ignored once its index has been bounds-checked.

A multipart message body MAY also have `content`, `nonce`, `tts`, `flags`, `favorite_meme_id`, and `sticker_ids` as plain form fields. Each overrides the member of the same name in the parsed `payload_json`, and `sticker_ids` collects every value it is given.

The `attachments` array inside `payload_json` maps attachment metadata to files by matching each attachment `id` to `n` in `files[n]`. A metadata entry that has a `filename` but matches no supplied file is rejected with `NO_FILE_FOR_ATTACHMENT_METADATA`, and two metadata entries claiming the same file index are rejected with `DUPLICATE_ATTACHMENT_IDS_NOT_ALLOWED`. A supplied file that no metadata entry claims is still attached, taking its index as the attachment ID and its uploaded filename as the attachment filename. A metadata entry that has no `filename` and matches no supplied file is read as a pre-uploaded attachment reference.

[Messages](/http-api/messages/) defines the attachment metadata and pre-uploaded attachment form, and [Attachment uploads](/topics/uploads/) defines the separate relay upload flow.

:::caution[Multipart indices are attachment IDs]
Each direct file's `files[n]` index is also the `id` in its attachment metadata entry. Fluxer reads `n` from the field name, so part order sets no identity. The indices need not begin at zero and need not be contiguous.
:::

## Input normalisation

Field tables use the shared [wire table notation](/conventions/#wire-table-notation).

:::note[Empty values are normalised before validation]
An empty string becomes `null` at any depth, and an empty nested object becomes `null` below the root of the payload.
:::

The shared validator normalises the JSON body, a form body, the query string, path parameters, request headers, and cookies before their schemas run.

A nested object whose members have all become `null` becomes `null` in turn. The root object itself is never collapsed this way. An empty request body is read as an empty object, so the caller sees the operation's own required-field failures. A body that is present but does not parse as JSON returns 400 `INVALID_FORM_BODY` with one element at path `body` and code `INVALID_FORMAT`.

:::caution[Three operations bypass the shared validator]
[Create message](/http-api/messages/#create-message), [Modify message](/http-api/messages/#modify-message), and [Execute webhook](/http-api/webhooks/#execute-webhook) read their own body and apply none of that normalisation.
:::

An empty string stays an empty string and an empty nested object stays an empty object in those three. The first two reject a JSON body that does not parse. On a JSON body all three collapse every schema failure to one validation entry, and each operation names that entry on its own page.

## Authentication

[Authentication](/authentication/) defines the accepted `Authorization` schemes, their exact token forms, and the OAuth2 scope registry. An operation that requires a credential states the scheme on its resource page. The [sudo verification object](/http-api/users/mfa/#sudo-verification-object) defines the sudo mode credential that guards sensitive account operations.

An OAuth2 bearer access token is accepted only where a route opts in, and the resource page says so. Everywhere else a bearer credential is refused with 403 `ACCESS_DENIED`, and an account with a suspicious activity flag is refused with 403 `ACCOUNT_SUSPICIOUS_ACTIVITY`.

## Standard request headers

These headers are accepted across resources. An operation-specific header is documented in that operation's request header table.

| Field | Type | Description |
| --- | --- | --- |
| Authorization? | string | The single credential for an authenticated request, in one of the [accepted schemes](/authentication/#authorization-schemes) |
| Content-Type?<sup>1</sup> | string | The media type of the request body, which selects the multipart parser when it contains `multipart/form-data` |
| Accept-Language?<sup>2</sup> | string | Selects the locale used for an error `message` |
| X-Audit-Log-Reason?<sup>3</sup> | string | Free-text reason recorded on the resulting audit log entry |
| X-Fluxer-Client-Properties?<sup>4</sup> | string | Base64-encoded JSON with the native client's `os`, read when an authentication session is created |
| X-Fluxer-Sudo-Mode-JWT?<sup>5</sup> | string | A sudo mode proof previously issued to the authenticated user |
| X-Captcha-Token?<sup>6</sup> | string | The CAPTCHA solution issued by the selected provider |
| X-Captcha-Type?<sup>6</sup> | string | Either `hcaptcha` or `turnstile`, selecting the provider that issued the token |
| X-Request-ID?<sup>7</sup> | string | A correlation identifier the client chooses, echoed unchanged in the response |
| User-Agent? | string | The originating client description recorded on a new authentication session and on an Admin audit entry |
| Origin?<sup>8</sup> | string | The browser origin used for cross-origin negotiation and for the mutating same-host origin check |

<sup>1</sup> Only the multipart message operations read it to decide how the body is parsed. The instance content filter reads it separately, as described under [request body formats](#request-body-formats)

<sup>2</sup> The configured locale of the authenticated account takes precedence, so this header selects the locale only for an unauthenticated request or an account with no configured locale

<sup>3</sup> The value is read verbatim with no percent-decoding, then stripped of form feed and right-to-left override characters and trimmed. A blank or over-long value is treated as absent

<sup>4</sup> Read only for a native Fluxer `User-Agent`, at most 4096 characters, and only the `os` member is used

<sup>5</sup> A valid token replaces the sudo proof fields in the operation body and is echoed in the response header without extending its lifetime.

<sup>6</sup> Read only while the instance has a provider configured and the operation is gated. The handshake is defined in [CAPTCHA handling](/topics/captcha/)

<sup>7</sup> A supplied value is echoed back unchanged and unvalidated

<sup>8</sup> The exact use is defined in [cross-origin requests](#cross-origin-requests)

A client MUST NOT send an `X-Request-ID` it is not willing to see in logs, because Fluxer echoes and records the value without validating it.

An `X-Audit-Log-Reason` normalised to more than 512 characters is discarded, and the request still succeeds. Every request is normalised this way, but only an operation that supports an audit reason records the result.

## Standard response headers

| Field | Type | Description |
| --- | --- | --- |
| X-Fluxer-Version?<sup>1</sup> | string | The build version of the instance that served the request |
| X-Request-ID?<sup>2</sup> | string | The identifier assigned to the request |
| X-Fluxer-Sudo-Mode-JWT?<sup>3</sup> | string | The sudo mode token for the caller, present only on an operation that completes a sudo proof |
| Content-Type? | string | The media type of the representation, absent from a response with no body |
| Cache-Control?<sup>4</sup> | string | The literal value `no-cache` unless the operation sets its own directive |
| Access-Control-Allow-Origin?<sup>5</sup> | string | The request `Origin` when it is a configured application origin, and the literal `*` on routes that set their own wildcard |
| Access-Control-Expose-Headers?<sup>5</sup> | string | The literal value `X-Fluxer-Version` |
| Vary?<sup>5</sup> | string | The literal value `Origin`, sent whenever the allowed origin was echoed |
| Retry-After?<sup>6</sup> | string | Whole seconds to wait, sent on a rate limit denial, a slowmode denial, a resource lock, and the in-flight ceiling 503 |
| X-RateLimit-Limit?<sup>7</sup> | string | Present on a route denial and on a successful bot or webhook request |
| X-RateLimit-Remaining?<sup>7</sup> | string | Present on a route denial and on a successful bot or webhook request |
| X-RateLimit-Reset?<sup>7</sup> | string | Unix timestamp in seconds |
| X-RateLimit-Reset-After?<sup>7</sup> | string | Seconds until the bucket resets |
| X-RateLimit-Bucket?<sup>7</sup> | string | A stable 16-character hash of the bucket name |
| X-RateLimit-Scope?<sup>7</sup> | string | Present on a rate limit denial, taking the value `user`, `shared`, or `global` |
| X-RateLimit-Global?<sup>7</sup> | string | Present on a global rate limit denial, taking the literal value `true` |

<sup>1</sup> The value is the literal `dev` when the build has no version stamp

<sup>2</sup> A generated UUID unless the request supplied its own, in which case that value is echoed back unchanged and unvalidated

<sup>3</sup> A token newly issued where the caller proved sudo mode afresh, and otherwise the incoming proof echoed back with no extension of its lifetime

<sup>4</sup> Absent from a response with no body

<sup>5</sup> `Access-Control-Expose-Headers` is sent on every response the instance CORS policy handles. `Access-Control-Allow-Origin` and `Vary` are sent only for an allowed origin

<sup>6</sup> The literal 1 on the in-flight ceiling 503, and otherwise the [rate limit header](/topics/rate-limits/#rate-limit-headers) contract

<sup>7</sup> The complete contract is defined in [Rate limits](/topics/rate-limits/#rate-limit-headers)

An operation that sets its own `Cache-Control` keeps that value. A response whose `Content-Type` names a stylesheet, script, font, image, video, or audio representation receives `public, max-age=31536000`.

<a id="rate-limits"></a>

## Rate limits

Every route consumes its own rate limit bucket and is additionally evaluated against one global bucket unless that bucket is exempt. A denial returns 429 `RATE_LIMITED`. [Rate limits](/topics/rate-limits/) defines the bucket scoping rules, the global allowance, the 429 body, the scope registry, and the complete `X-RateLimit-*` header contract.

:::note[Two 429 responses have no `X-RateLimit-*` header]
A 429 `RESOURCE_LOCKED` response has `Retry-After: 1`, and a 429 `IP_AUTHORIZATION_RESEND_COOLDOWN` response has the remaining cooldown in whole seconds. A client that reads the bucket headers branches on `code`.
:::

A global denial has `Retry-After`, `X-RateLimit-Scope`, and `X-RateLimit-Global` alone.

## Hosted-only routes

A small set of routes exists only on the hosted Fluxer deployment. A self-hosted deployment answers one of them with 404 `NOT_FOUND`. [Deployment availability](/http-api/deployment-availability/) lists every hosted-only route and states how a client resolves the deployment kind before authenticating.

## Cross-origin requests

The CORS response policy is an allow-list of exactly two origins, the deployment's configured web application endpoint and its marketing endpoint. A request whose `Origin` matches one of them receives `Access-Control-Allow-Origin` set to that origin and `Vary: Origin`. Every other request, including one that sends no `Origin`, receives no `Access-Control-Allow-Origin` from that policy. Credentialed cross-origin requests are not enabled, so `Access-Control-Allow-Credentials` is never sent.

Five paths are readable from any origin. `/v1/webhooks/{webhook_id}/{token}` and `/v1/webhooks/{webhook_id}/{token}/messages/{message_id}` have a second cross-origin policy that allows any origin. Four of the methods registered on them refuse the first-party web client outright, and that refusal is defined by [Origin refusal](/http-api/webhooks/#origin-refusal).

[Get instance discovery](/http-api/instance/#get-instance-discovery) on `/.well-known/fluxer`, [Get OpenAPI document](/http-api/instance/#get-openapi-document) on `/v1/openapi.json`, and [Get client geolocation](/http-api/instance/#get-client-geolocation) on `/v1/ip` set `Access-Control-Allow-Origin: *` in the operation itself. The wildcard stands for any origin outside the allow-list, and for an allowed origin the policy replaces it with that exact origin and sends `Vary: Origin`.

`Access-Control-Expose-Headers` is the single value `X-Fluxer-Version`. Every other Fluxer response header, the rate limit headers and `X-Request-ID` included, is hidden from cross-origin script.

:::caution[Same-host mutations require a matching origin]
A production deployment rejects a non-`GET` request whose `Host` is `web.fluxer.app` or `web.canary.fluxer.app` unless its `Origin` is exactly `https://` followed by that same host, returning 403 `INVALID_API_ORIGIN`. A request sent to the API host is unaffected.
:::

<a id="error-response"></a>

## Error response object

A JSON error response identifies the stable error code and a human-readable message. An operation can add further top-level members with structured detail. [Errors](/http-api/errors/) documents the code registries, the HTTP status fallback mapping, and the localisation behaviour.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| code<sup>1</sup> | string | Stable machine-readable [API error code](/http-api/errors/#api-error-code-registry) |
| message<sup>2</sup> | string | Human-readable description of this failure |
| errors?<sup>3</sup> | array[[validation error](#validation-error-object) object] | Request fields that failed validation |

<sup>1</sup> Every code is an uppercase symbolic name matching `[A-Z][A-Z0-9_]*`

<sup>2</sup> The wording varies by locale and defaults to the code itself when the failure supplies no message

<sup>3</sup> Present on an `INVALID_FORM_BODY` response, and on any other failure that has field detail

### Example

```json
{
  "code": "MISSING_PERMISSIONS",
  "message": "Missing permissions"
}
```

The body has no request identifier. A client correlates a failure through the `X-Request-ID` response header.

:::note[Structured detail sits at the top level]
`IP_AUTHORIZATION_REQUIRED` has `ip_authorization_required`, `ticket`, `email`, and `resend_available_in`. `SUDO_MODE_REQUIRED` has `has_mfa` and `methods`.
:::

`MISSING_OAUTH_SCOPE` has `required_scope`, and `RATE_LIMITED` has `retry_after` and `global`, where `global` is `true` only on a global bucket denial. Each operation that produces one of those members documents it. A client MUST treat any member it does not recognise as absent. Two operations returning the same `code` can have different extra members, so a client MUST read the members its own operation documents.

## Validation error object

Each entry identifies one failed input field. A 400 response whose top-level code is `INVALID_FORM_BODY` has the array in `errors`. [Errors](/http-api/errors/#validation-failure-codes) states which failures produce it.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| path<sup>1</sup> | string | The location of the request field that failed validation |
| code?<sup>2</sup> | string | Stable machine-readable validation code |
| message<sup>3</sup> | string | The human-readable description of this validation failure |

<sup>1</sup> A nested field is a dot-joined path such as `embeds.0.title`, and a failure with no field position uses the literal path `root`

<sup>2</sup> Present whenever the failure has an enumerated code from the [validation error code registry](/http-api/errors/#validation-error-code-registry)

<sup>3</sup> The localised message registered for the `code`, falling back to the code itself, and a fixed English string on a failure that has no code

### Example

```json
{
  "code": "INVALID_FORM_BODY",
  "message": "Invalid form body",
  "errors": [
    {"path": "name", "code": "BASE_TYPE_REQUIRED", "message": "This field is required"}
  ]
}
```

Fluxer produces at most one entry for each distinct pair of `path` and `code`, so a field that fails several equivalent constraints appears once.

## Resource pages

| Page | Covers |
| --- | --- |
| [Errors](/http-api/errors/) | The error envelope, status fallback mapping, and the API and validation code registries |
| [Deployment availability](/http-api/deployment-availability/) | Routes that exist only on the hosted deployment |
| [Authentication operations](/http-api/authentication/) | Registration, login, MFA, WebAuthn, SSO, email verification, recovery, sessions, IP authorisation, desktop handoff |
| [Gateway](/http-api/gateway/) | Bot discovery of the main Gateway endpoint and session start budget |
| [Instance](/http-api/instance/) | Instance discovery, client geolocation, the limit key registry, the served OpenAPI document |
| [OAuth2](/http-api/oauth2/) | Authorisation, consent, token exchange, introspection, revocation, granted authorisations |
| [Applications](/http-api/applications/) | Application, bot account, client secret, and bot token management, plus public lookup |
| [Connections](/http-api/connections/) | External account connection initiation, verification, visibility, and deletion |
| [Users](/http-api/users/) | The user object and the shared user enumerations |
| [Current user](/http-api/users/current-user/) | Current account retrieval, profile mutation, lifecycle, policy acceptance, authorised IP state |
| [User settings](/http-api/users/settings/) | Account, notification, and privacy settings, guild folders, voice activity sharing |
| [User settings Protobuf](/http-api/users/settings-protobuf/) | Every structured client preference message and enumeration |
| [Email and password changes](/http-api/users/email-and-password/) | The ticketed credential replacement flows |
| [Multi-factor authentication](/http-api/users/mfa/) | TOTP, backup codes, WebAuthn credentials, sudo verification |
| [Phone verification](/http-api/users/phone-verification/) | Outbound and inbound phone verification |
| [Relationships](/http-api/users/relationships/) | Friend requests, friendships, blocks, relationship nicknames |
| [User notes](/http-api/users/notes/) | Private notes attached to user IDs |
| [Private channels](/http-api/users/private-channels/) | Direct message and group DM discovery, creation, preload, pin state |
| [User content collections](/http-api/users/content/) | Recent mentions, saved messages, asynchronous message deletion |
| [Gift inventory](/http-api/users/gifts/) | Premium gift codes created by the current account |
| [Data harvests](/http-api/users/data-harvest/) | Data harvest creation, status, and download |
| [Read states](/http-api/read-states/) | Message watermark and mention count acknowledgements |
| [Memes](/http-api/memes/) | The saved image, video, and audio collection and batch GIF URL resolution |
| [Themes](/http-api/themes/) | Shareable custom CSS theme creation |
| [Channels](/http-api/channels/) | Channel objects, private recipients, permission overwrites, slowmode, RTC regions |
| [Calls](/http-api/calls/) | Call eligibility, region selection, ringing, and termination |
| [Streams](/http-api/streams/) | Go Live stream keys, stream regions, preview image lifecycle |
| [Entrance sounds](/http-api/entrance-sounds/) | The entrance sound collection, its per-scope selections, and playback |
| [Messages](/http-api/messages/) | Messages, attachments, embeds, history, acknowledgements, pins, reactions, typing |
| [Guilds](/http-api/guilds/) | Guild objects and lifecycle operations |
| [Guild channels](/http-api/guild-channels/) | Listing, creation, hierarchy, permission inheritance, bulk positioning |
| [Guild members](/http-api/guild-members/) | Member objects, profiles, voice moderation, ownership, removal, role assignment |
| [Guild member search](/http-api/guild-member-search/) | Indexed search filters, sorting, pagination, supplemental join metadata |
| [Guild moderation](/http-api/guild-moderation/) | Bans, temporary bans, ban replacement, and the blocks a ban has |
| [Guild emojis](/http-api/guild-emojis/) | Guild emoji objects, uploads, metadata changes, deletion |
| [Guild stickers](/http-api/guild-stickers/) | Guild sticker objects, uploads, metadata changes, deletion |
| [Expressions](/http-api/expressions/) | Emoji and sticker metadata reads across both |
| [Guild audit logs](/http-api/guild-audit-logs/) | Audit entries, typed targets, contexts, changes, filters, the audit reason contract |
| [Roles and permissions](/http-api/permissions/) | Permission flags, computation order, role objects and lifecycle |
| [Discovery](/http-api/discovery/) | Public guild listings, categories, the listing application lifecycle, joining without an invite |
| [Invites](/http-api/invites/) | Invite lookup, creation, acceptance, deletion, code generation |
| [Webhooks](/http-api/webhooks/) | Webhook management, message execution, GitHub, Slack, and Instatus callbacks |
| [Search](/http-api/search/) | Authenticated global message search |
| [Unfurl](/http-api/unfurl/) | Authenticated external URL metadata resolution |
| [Billing](/http-api/billing/) | Stripe checkout, card preapproval, gift purchase, age verification, refunds, the Stripe webhook |
| [Premium](/http-api/premium/) | Premium pricing, entitlement state, subscription self-service, billing portal handoff |
| [Gifts](/http-api/gifts/) | Public gift code lookup and authenticated redemption |
| [Donations](/http-api/donations/) | Donation currencies and intervals, checkout sessions, the donor management link |
| [Reports](/http-api/reports/) | Authenticated safety reports and email-verified Digital Services Act notices |
