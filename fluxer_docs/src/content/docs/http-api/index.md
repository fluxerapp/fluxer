---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: HTTP API
description: Request format, shared headers, cross-origin policy, error objects and the list of resource pages.
---

Use the HTTP API to read and change resources.

## Base URL

- Every path starts with `/v1` under the base URL in [`/.well-known/fluxer`](/http-api/instance/#get-instance-discovery).
- Third-party clients use `endpoints.api_public`.
- Every instance serves its OpenAPI 3.1 document at [`/v1/openapi.json`](/http-api/instance/#get-openapi-document).

## Conventions

- Limits such as attachment counts differ between instances, and each instance returns its own in [instance discovery](/http-api/instance/#limit-keys).
- Field tables use the [field notation](/#field-notation) for optional and nullable fields.

## Resource pages

| Page | What it documents |
| --- | --- |
| [Errors](/http-api/errors/) | Error responses, status codes and every error code |
| [Deployment availability](/http-api/deployment-availability/) | Routes served only while premium or billing is on |
| [Authentication resource](/http-api/authentication/) | Registration, login, MFA, WebAuthn, SSO, recovery, sessions |
| [Gateway](/http-api/gateway/) | Gateway endpoint discovery and the session start budget |
| [Instance](/http-api/instance/) | Instance discovery, geolocation, limit keys, the OpenAPI document |
| [Experiments](/http-api/experiments/) | Experiment assignments and how to poll for them |
| [OAuth2](/http-api/oauth2/) | Authorisation, consent, tokens, introspection, revocation |
| [Applications](/http-api/applications/) | Applications, bot accounts, client secrets, bot tokens |
| [Connections](/http-api/connections/) | External account connections |
| [Users](/http-api/users/) | The user object and shared user enumerations |
| [Current user](/http-api/users/current-user/) | Your account, profile, lifecycle and policy acceptance |
| [User settings](/http-api/users/settings/) | Account, notification and privacy settings, guild folders |
| [User settings Protobuf](/http-api/users/settings-protobuf/) | Structured client preference messages |
| [Email and password changes](/http-api/users/email-and-password/) | The ticketed credential change flows |
| [Multi-factor authentication](/http-api/users/mfa/) | TOTP, backup codes, WebAuthn credentials, sudo verification |
| [Relationships](/http-api/users/relationships/) | Friends, friend requests, blocks |
| [User notes](/http-api/users/notes/) | Private notes on users |
| [Private channels](/http-api/users/private-channels/) | Direct message and group direct message channels |
| [Push notifications](/http-api/users/push-notifications/) | Registering a device for push delivery |
| [User content collections](/http-api/users/content/) | Recent mentions, saved messages, message deletion |
| [Gift inventory](/http-api/users/gifts/) | Premium gift codes you created |
| [Data exports](/http-api/users/data-harvest/) | Requesting and downloading a copy of your data |
| [Read states](/http-api/read-states/) | Read acknowledgements and mention counts |
| [Memes](/http-api/memes/) | Saved media and GIF URL resolution |
| [GIFs](/http-api/gifs/) | GIF search, featured and trending listings, share registration |
| [Themes](/http-api/themes/) | Shareable custom CSS themes |
| [Channels](/http-api/channels/) | Channels, recipients, permission overwrites, slowmode |
| [Threads](/http-api/threads/) | Starting, archiving and searching threads |
| [Thread members](/http-api/thread-members/) | Joining and leaving threads, notification settings |
| [Forums](/http-api/forums/) | Forum and media channels, tags, posts |
| [Calls](/http-api/calls/) | Call eligibility, regions, ringing, termination |
| [Peer-to-peer voice](/http-api/peer-to-peer-voice/) | Connection outcome reports for peer-to-peer calls |
| [Streams](/http-api/streams/) | Stream keys, regions, previews |
| [Entrance sounds](/http-api/entrance-sounds/) | The entrance sound collection and playback |
| [Messages](/http-api/messages/) | Messages, attachments, embeds, history, pins, reactions, typing |
| [Guilds](/http-api/guilds/) | Guild objects and lifecycle |
| [Guild channels](/http-api/guild-channels/) | Listing, creating and positioning guild channels |
| [Guild members](/http-api/guild-members/) | Members, profiles, voice moderation, roles |
| [Guild member search](/http-api/guild-member-search/) | Member search filters and sorting |
| [Guild moderation](/http-api/guild-moderation/) | Bans and temporary bans |
| [Guild emojis](/http-api/guild-emojis/) | Guild emoji management |
| [Guild stickers](/http-api/guild-stickers/) | Guild sticker management |
| [Expressions](/http-api/expressions/) | Emoji and sticker metadata |
| [Guild audit logs](/http-api/guild-audit-logs/) | Audit entries, filters and the audit reason |
| [Roles and permissions](/http-api/permissions/) | Permission flags and roles |
| [Discovery](/http-api/discovery/) | Public guild listings and listing applications |
| [Invites](/http-api/invites/) | Invite lookup, creation, acceptance, deletion |
| [Webhooks](/http-api/webhooks/) | Webhook management, execution and provider callbacks |
| [Search](/http-api/search/) | Global message search |
| [Unfurl](/http-api/unfurl/) | External URL metadata |
| [Billing](/http-api/billing/) | Stripe checkout, gift purchase, the Stripe webhook |
| [Premium](/http-api/premium/) | Pricing, entitlement state, subscription self-service |
| [Gifts](/http-api/gifts/) | Gift code lookup and redemption |
| [Reports](/http-api/reports/) | Safety reports and Digital Services Act notices |

## Request format

- Send JSON bodies with `Content-Type: application/json`.
- Responses are UTF-8 JSON unless an operation says otherwise.
- Every `GET` route also responds to `HEAD`.
- Unknown paths, including a known path with a trailing slash, return 404 `NOT_FOUND`.
- An overloaded instance returns 503 `SERVICE_UNAVAILABLE` with `Retry-After: 1`.

## Request body formats

[Create message](/http-api/messages/#create-message), [Modify message](/http-api/messages/#modify-message), [Execute webhook](/http-api/webhooks/#execute-webhook) and [Start thread](/http-api/threads/#start-thread) take `multipart/form-data` with these fields when you attach files.

| Field | Type | Description |
| --- | --- | --- |
| payload_json? | string | The JSON request payload of the operation |
| files[n]? | file | The attachment file at index `n` |

- The index `n` must be below the instance's [`max_attachments_per_message`](/http-api/instance/#limit-keys) limit.
- It matches the `id` of an entry in the `attachments` array of `payload_json`.
- Indexes don't need to start at zero or be consecutive.
- A malformed multipart body returns 400 `INVALID_FORM_BODY` with the validation code `FAILED_TO_PARSE_MULTIPART_FORM_DATA`.

To upload files before sending, use [attachment uploads](/topics/uploads/). The OAuth2 token operations take `application/x-www-form-urlencoded` or `multipart/form-data` bodies.

<a id="input-normalization"></a>

## Input normalisation

Before validation, Fluxer changes these values.

- Empty strings and empty nested objects become `null`.
- An empty body counts as `{}`.

Create message, Modify message, Execute webhook and Start thread skip this step. Malformed JSON returns 400 `INVALID_FORM_BODY` with the `path` `body` and the `code` `INVALID_FORMAT`.

## Authentication

Each operation says which [credentials](/authentication/) it accepts. An OAuth2 bearer token works only on routes marked OAuth2, and other routes return 403 `ACCESS_DENIED`.

## Standard request headers

| Field | Type | Description |
| --- | --- | --- |
| Authorization? | string | The credential for an authenticated request |
| Content-Type? | string | The body media type and, for multipart bodies, the boundary |
| Accept-Language? | string | The locale for an error `message`, used when the account has none set |
| X-Audit-Log-Reason? | string | A reason for the audit log entry (up to 512 characters) |
| X-Fluxer-Sudo-Mode-JWT? | string | The sudo token from [Multi-factor authentication](/http-api/users/mfa/#sudo-mode) |
| X-Captcha-Token? | string | The [solved ALTCHA challenge](/topics/captcha/) |
| X-Request-ID? | string | An identifier you choose, echoed in the response |

## Standard response headers

| Field | Type | Description |
| --- | --- | --- |
| X-Fluxer-Version? | string | The build version of the instance |
| X-Request-ID? | string | The request identifier, generated when you send none |
| X-Fluxer-Sudo-Mode-JWT? | string | The sudo mode token, sent when an operation completes a sudo proof |
| Cache-Control? | string | `no-cache` unless the operation sets its own value or the body is media |
| Retry-After? | string | Whole seconds to wait after a 429 or 503 |

A 429, and any response to a bot or webhook token, can have the [`X-RateLimit-*` headers](/topics/rate-limits/).

## Rate limits

Every route has its own bucket, and most routes also count against a global bucket. A denial returns 429 [`RATE_LIMITED`](/topics/rate-limits/).

## Conditional routes

A self-hosted instance serves [some routes](/http-api/deployment-availability/) only while its premium mode or billing is on, and returns 404 `NOT_FOUND` for the others.

## Cross-origin requests

Only the instance's web app and marketing origins are allowed. A request from one of them gets `Access-Control-Allow-Origin` set to that origin and `Vary: Origin`. The API doesn't use cookies. Send your token in `Authorization`.

### Routes open to any origin

- [Get instance discovery](/http-api/instance/#get-instance-discovery), [List guild badges](/http-api/instance/#list-guild-badges), [Get OpenAPI document](/http-api/instance/#get-openapi-document), [Get client geolocation](/http-api/instance/#get-client-geolocation) and [Look up the actor with WebFinger](/http-api/unfurl/#look-up-the-actor-with-webfinger) allow any origin with `*`.
- The webhook execution routes also allow any origin, with the exceptions under [Origin refusal](/http-api/webhooks/#origin-refusal).

### Headers

Browser scripts can see only the `X-Fluxer-Version` and `ETag` response headers. Cross-origin requests can send these headers.

- The headers in [Standard request headers](#standard-request-headers)
- `X-Requested-With`
- `If-None-Match`
- `X-Captcha-ID`
- The `X-Fluxer-*` client headers

<a id="error-response"></a>

## Error response object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| code | string | The stable [API error code](/http-api/errors/#api-error-codes) |
| message | string | A description in the request locale |
| errors? | array[[validation error](#validation-error-object) object] | The request fields that failed validation |

Some codes add [more top-level fields](/http-api/errors/#extra-fields). Ignore fields you don't recognise.

## Validation error object

A 400 `INVALID_FORM_BODY` response has one validation error object in `errors` for each field that failed, with its [validation code](/http-api/errors/#validation-errors).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| path | string | The failed field as a dotted path like `embeds.0.title`, `root` for the whole object or `body` for malformed JSON |
| code? | string | The [validation error code](/http-api/errors/#validation-error-codes) |
| message | string | A description of the failure |

### Example

```json
{
  "code": "INVALID_FORM_BODY",
  "message": "Invalid form body.",
  "errors": [
    {"path": "name", "code": "INVALID_FORMAT", "message": "The provided value is in an invalid format."}
  ]
}
```
