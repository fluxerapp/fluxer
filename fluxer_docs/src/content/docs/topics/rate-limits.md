---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Rate limits
description: How the HTTP API limits requests, the 429 response and the rate limit headers.
---

Too many requests return 429 with `code` set to `RATE_LIMITED`. Wait `retry_after` seconds from the [response body](#rate-limit-response-object) and send the request again.

## Buckets

Every HTTP API and Admin API operation declares a bucket, and its reference section says how many requests the bucket allows. A bucket name with a path parameter, such as `guild:emojis:list::guild_id`, has an allowance for each value of that parameter.

Each caller has its own allowance, and a user session, a bot token, an Admin API key and an OAuth2 application count as separate callers even on the same account.

A request with no token counts against the client IP address, or its `/64` for IPv6.

Each request also counts against a global bucket of 50 requests per second for each caller. An account with the [`HIGH_GLOBAL_RATE_LIMIT`](/admin-api/users/#account-flags) flag gets 1,200 requests per second, and an account with [`RATE_LIMIT_BYPASS`](/admin-api/users/#account-flags) skips the global bucket and every route bucket. The [limits enforced inside a handler](#limits-enforced-inside-a-handler) still apply to it.

Executing a webhook, including its GitHub, Slack, Instatus and Libredesk callbacks, getting, editing or deleting its messages through the webhook's token URL, and [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) don't count against the global bucket.

Buckets refill continuously. A request still counts when the API rejects it with an error.

## Rate limit response object

A 429 body adds two fields to the [error response](/http-api/#error-response).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| code | string | `RATE_LIMITED`, or a more specific code from one of the [operation limits](#limits-enforced-inside-a-handler) |
| message | string | The rate limit message in the request's [locale](/topics/locales/#negotiation) |
| global | boolean | Whether the global bucket denied the request |
| retry_after | number | The seconds to wait, as a decimal (at least 0.001) for a bucket denial and in whole seconds for most operation limits |

### Example

```json
{
  "code": "RATE_LIMITED",
  "message": "You're being rate limited.",
  "global": false,
  "retry_after": 0.428
}
```

## Rate limit headers

Every 429 has `Retry-After`. A 429 with the [rate limit response object](#rate-limit-response-object) also has `X-RateLimit-Scope`, and a route bucket denial has the other `X-RateLimit-*` headers too. Successful responses have the route headers only for a bot token or a webhook token.

| Field | Type | Description |
| --- | --- | --- |
| Retry-After? | string | The seconds to wait, rounded up to a whole number (at least 1) |
| X-RateLimit-Scope? | string | The [scope](#scopes) that denied the request |
| X-RateLimit-Global? | string | `true` when the global bucket denied the request, sent with no route headers |
| X-RateLimit-Limit? | string | The most requests the route bucket allows at once |
| X-RateLimit-Remaining? | string | The requests the route bucket allows right now, 0 on a denial |
| X-RateLimit-Reset? | string | The Unix time in seconds when the route bucket is full again |
| X-RateLimit-Reset-After? | string | The seconds until the route bucket is full again |
| X-RateLimit-Bucket? | string | A 16-character hex ID for the route bucket, left out on an operation limit |

The reset headers say when the bucket is full again, and `retry_after` says when you can send one more request. Browser clients can't read these headers across origins and should use `retry_after` from the body.

### Scopes

| Value | Description |
| --- | --- |
| user | A route bucket, counted for you alone |
| global | The global bucket |
| shared | An allowance shared with other members, such as the announcement channel limits |

## Handling a 429

- Use the body's `retry_after`, since `Retry-After` rounds up to whole seconds.
- When `global` is true, pause every request that uses that token.
- For a bot, read `X-RateLimit-Remaining` on every successful response, and slow down your requests as the value gets close to 0.

## Limits enforced inside a handler

Some operations have extra limits apart from the route and global buckets, and `RATE_LIMIT_BYPASS` doesn't skip them.

### Limits that return 429

These return 429 with the [rate limit response object](#rate-limit-response-object) and the [headers](#rate-limit-headers).

| Operation | Limit |
| --- | --- |
| [Log in with a password](/http-api/authentication/#log-in-with-a-password) | 5 per 15 minutes for one email address, and 10 failed logins per 30 minutes from one IP address |
| [Register an account](/http-api/authentication/#register-an-account) | 3 per 15 minutes for one email address, 3 per hour for one IP address and 15 per hour for one subnet |
| [Resend email verification](/http-api/authentication/#resend-email-verification) | 3 per 15 minutes for one account |
| [Request password recovery](/http-api/authentication/#request-password-recovery) | 5 per 30 minutes for one email address and 20 per 30 minutes for one IP address |
| [Recover an account](/http-api/authentication/#recover-an-account) | 10 per 30 minutes from one IP address, and 5 per 30 minutes for one username from one IP address |
| [Start email change](/http-api/users/email-and-password/#start-email-change) | 3 code sends to the current address and 5 to the new one per 15 minutes, and one every 30 seconds per ticket |
| [Start password change](/http-api/users/email-and-password/#start-password-change) | 3 code sends per 15 minutes, and one every 30 seconds per ticket |
| [Submit message report flow](/http-api/reports/#submit-message-report-flow) | 100 reports per hour for one reporter, 30 in one channel, 50 in one community and 20 for one message from all reporters. A report with a highest-priority reason doesn't count towards any of these limits |
| [Create DSA report](/http-api/reports/#create-dsa-report) | 100 reports per hour for one email address |

On an instance where people sign in with a username, a password login allows 5 attempts per 15 minutes for one username from one IP address and 100 failed logins per hour for one username, and the IP address limit still applies.

Registering an [instant account](/admin-api/instance/#auth-configuration-object) doesn't use the limits of [Register an account](/http-api/authentication/#register-an-account). It allows 5 per hour and 20 per day from one IP address and 40 per hour from one subnet.

[Resend IP authorisation](/http-api/authentication/#resend-ip-authorization) allows one resend per ticket, 30 seconds or more after the ticket was issued. An earlier resend returns 429 `IP_AUTHORIZATION_RESEND_COOLDOWN` with `retry_after` and `resend_available_in` in the body, a `Retry-After` header and no `X-RateLimit-*` headers. A second resend returns 400 `IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED` for that ticket.

### Announcement channel limits

These return `X-RateLimit-Scope` set to `shared`. Everyone who publishes in the channel or edits one of its published messages draws on the same allowance.

| Limit | Code |
| --- | --- |
| 10 [crossposts](/http-api/messages/#crosspost-message) from one channel, then one every 6 minutes | `MESSAGE_CROSSPOST_RATE_LIMITED` |
| 3 edits of one published message, then one every 20 minutes | `PUBLISHED_MESSAGE_EDIT_RATE_LIMITED` |

### Limits that return 400

These return 400 `INVALID_FORM_BODY` with a [validation error](/http-api/#validation-error-object) whose `code` says which limit you hit, and no `retry_after` or rate limit headers. The username limit applies to [Modify current user](/http-api/users/current-user/#modify-current-user) and [Update bot profile](/http-api/applications/#update-bot-profile). The other profile limits apply to [Modify current user](/http-api/users/current-user/#modify-current-user) and to [Modify current guild member](/http-api/guild-members/#modify-current-guild-member), which counts them per guild.

| Change | Limit | Validation code |
| --- | --- | --- |
| Username | 5 username or discriminator changes per 3 hours | `USERNAME_CHANGED_TOO_MANY_TIMES` |
| Biography | 25 changes per 30 minutes | `BIO_CHANGED_TOO_MANY_TIMES` |
| Pronouns | 25 changes per 30 minutes | `PRONOUNS_CHANGED_TOO_MANY_TIMES` |
| Accent colour | 25 changes per 30 minutes | `ACCENT_COLOR_CHANGED_TOO_MANY_TIMES` |
| Avatar | 25 changes per 30 minutes | `AVATAR_CHANGED_TOO_MANY_TIMES` |
| Banner | 25 changes per 30 minutes | `BANNER_CHANGED_TOO_MANY_TIMES` |
| [Voice activity sharing](/http-api/users/settings/#modify-voice-activity-sharing) | 1 change per 24 hours | `VOICE_ACTIVITY_SHARING_ON_COOLDOWN` |
| [TOTP login](/http-api/authentication/#complete-login-with-totp) | 10 attempts per 15 minutes, and 5 per 5 minutes on one MFA ticket | `INVALID_CODE` |
| [Sudo mode](/http-api/users/mfa/#sudo-mode) with TOTP | 10 attempts per 15 minutes | `INVALID_MFA_CODE` |

The TOTP login limits also apply to [Complete login with WebAuthn MFA](/http-api/authentication/#complete-login-with-webauthn-mfa).

When you hit the MFA ticket limit, the ticket is deleted and you have to log in again.

## Slowmode

When a channel's `rate_limit_per_user` is N, each member can send one message every N seconds. A send that comes too early returns 400 `SLOWMODE_RATE_LIMITED` with `retry_after` in the body and a `Retry-After` header.

Bots and members with [BYPASS_SLOWMODE](/http-api/permissions/) aren't limited. [Get channel slowmode state](/http-api/channels/#get-channel-slowmode-state) returns how long you have to wait before you send.

## Gateway and media

The Gateway has [its own limits](/gateway/limits-and-rate-limits/). The [Media Proxy](/media-proxy/overview/) doesn't count requests.
