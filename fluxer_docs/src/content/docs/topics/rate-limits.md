---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Rate limits
description: The route and global buckets, the denial body, and the rate limit headers.
---

Fluxer bounds HTTP API traffic with two allowances, a per-route bucket and one global bucket. A bucket is one allowance counted over one window. Fluxer denies an over-allowance request with 429, `code` set to `RATE_LIMITED`, a [rate limit response object](#rate-limit-response-object), and the [rate limit headers](#rate-limit-headers).

## Buckets and scope

Nearly every route declares its own bucket. A bucket name can have a path parameter placeholder written as a colon followed by the parameter name, as in `guild:emojis:list::guild_id`. Fluxer substitutes the request value before keying the bucket, so one route holds a separate allowance for each path resource.

Every bucket is also keyed by the caller's identity. An authenticated request is keyed by the account and its credential kind, and an OAuth2 bearer credential is keyed by the owning application as well. A session, a bot token, an Admin API key, and each bearer application therefore draw on separate allowances for the same account.

A request that resolves no account is keyed by the client IP address, exactly for IPv4 and by the `/64` for IPv6, so clients in the same `/64` share an allowance. Where the deployment is configured to read the address from a header the request does not have, it is refused with 403 `FORBIDDEN` before any bucket is evaluated.

Fluxer also evaluates a route bucket against the global bucket unless the route declares that bucket exempt. The global bucket is keyed by the same identity, so a request that resolves no account consumes the global allowance of its client IP address.

Seven buckets are exempt, and each is the only bucket its route declares. Those routes draw on no global allowance: `webhook:execute::webhook_id`, `webhook:message_get::webhook_id`, `webhook:message_edit::webhook_id`, `webhook:message_delete::webhook_id`, `webhook:github::webhook_id`, `webhook:instatus::webhook_id`, and `stripe:webhook`. The `user:group_dm:create` and `user:group_dm:recipient:add` buckets are exempt as well. Each sits on a route that already consumed a non-exempt bucket, so both routes still draw on the global allowance.

Every HTTP API and Admin API operation declares a bucket, apart from the seven [desktop download](/http-api/downloads/) routes, which declare none. A caller that sends no credential on a [Bluesky client document](/http-api/connections/#get-bluesky-client-metadata) is keyed by the client IP address.

The global window is one second. The default allowance is 50 requests per second, and an account holding the [`HIGH_GLOBAL_RATE_LIMIT`](/admin-api/users/#account-flags) flag receives 1,200 requests per second instead. An account holding the [`RATE_LIMIT_BYPASS`](/admin-api/users/#account-flags) flag is evaluated against neither the global bucket nor any route bucket, and receives no rate limit header on a successful response.

Some operations enforce a further limit inside the handler. `RATE_LIMIT_BYPASS` exempts an account from none of them. [Limits enforced inside a handler](#limits-enforced-inside-a-handler) has the complete set.

The global bucket is evaluated first. A route bucket is consumed only after the global check admits the request.

:::note[An allowance drains continuously]
Every bucket is a leaky bucket. It admits at most the declared limit at once and refills continuously at that limit for each declared window, so a client that exhausts an allowance can send again as soon as enough of it has drained.
:::

:::note[The headers and the body report different instants]
`X-RateLimit-Reset` and `X-RateLimit-Reset-After` report when the bucket has drained completely. `retry_after` in the response body reports the shorter delay after which one further request is admitted.
:::

:::note[A 429 can hide a 401 or 403]
A route's rate limit middleware normally runs before its authentication policy, so an over-allowance request returns 429 `RATE_LIMITED` where the same request inside its allowance would return 401 or 403. Fluxer resolves the credential before either check, and both buckets are keyed by the authenticated account whenever one resolves.
:::

Four routes charge a second bucket. [Create private channel](/http-api/users/private-channels/#create-private-channel) and [Add group direct message recipient](/http-api/channels/#add-group-direct-message-recipient) evaluate that second bucket after the authentication policy and the request validation, so an unauthenticated or malformed request is refused before it is consumed. Create private channel consumes `user:group_dm:create` only when the validated body supplies `recipients`. A one-to-one direct message create reaches no second bucket.

[Delete guild emoji](/http-api/guild-emojis/#delete-guild-emoji) and [Delete guild sticker](/http-api/guild-stickers/#delete-guild-sticker) declare both of their buckets ahead of the authentication policy and the request validation, so an unauthenticated or malformed request consumes the second bucket too. The `guild:emoji:delete:daily::guild_id` and `guild:sticker:delete:daily::guild_id` buckets draw on the global allowance, and one delete request evaluates it twice.

:::caution[A global denial revokes a user session]
When the global bucket denies a request authenticated by a user session token belonging to a non-bot account, Fluxer revokes that session token before writing the 429 and the client must authenticate again. A bot token, an OAuth2 access token, and an Admin API key are never revoked this way, and a route bucket denial never revokes a credential.
:::

A deployment can disable both buckets through instance configuration. While they are disabled, no response has an `X-RateLimit-*` header and no request is refused with 429 `RATE_LIMITED`. That switch also turns off the two login allowances. Every other [limit enforced inside a handler](#limits-enforced-inside-a-handler) stays in force.

## Rate limit response object

The denial body has the two members of the ordinary [error response](/http-api/#error-response) and two further members.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| code<sup>1</sup> | string | `RATE_LIMITED` on a bucket denial |
| message<sup>2</sup> | string | The localised rate limit message |
| global | boolean | Whether the global bucket produced the denial, present and false on a route denial |
| retry_after<sup>3</sup> | number | The delay in fractional seconds before another request is admitted |

<sup>1</sup> A limit enforced outside the route bucket middleware can reuse this body with its own code. [Send phone verification](/http-api/users/phone-verification/#send-phone-verification) is the only live one, reporting `PHONE_RATE_LIMIT_EXCEEDED`

<sup>2</sup> The locale is the one resolved for the request, which the account setting selects ahead of [Accept-Language](/http-api/#standard-request-headers)

<sup>3</sup> Never below 0.001, falling back to the whole-second `Retry-After` value when no fractional delay was computed

The [Resend IP authorisation](/http-api/authentication/#resend-ip-authorisation) cooldown answers 429 with its own code and a different body. [Allowances answering 429](#allowances-answering-429) states what that body has.

A [slowmode](#slowmode) denial answers 400 `SLOWMODE_RATE_LIMITED`.

### Example

```json
{
  "code": "RATE_LIMITED",
  "message": "You're being rate limited.",
  "global": false,
  "retry_after": 0.428
}
```

## Rate limit scopes

The `X-RateLimit-Scope` header is the scope that produced a denial.

| Value | Description |
| --- | --- |
| user | The denial came from an allowance keyed by the caller alone |
| global | The denial came from the global bucket |
| shared<sup>1</sup> | The denial came from an allowance that several accounts can exhaust for each other |

<sup>1</sup> No route bucket declares a scope of its own, so every route bucket denial reports `user`. [Send phone verification](/http-api/users/phone-verification/#send-phone-verification) is the only live source of `shared`

Phone verification reports `shared` when the per-number send allowance or a number-scoped provider cooldown produced the denial. Both are keyed by the submitted number, so two accounts sending to one number share the allowance.

## Rate limit headers

These headers describe a rate limit decision. An operation that answers 429 returns this set.

| Field | Type | Description |
| --- | --- | --- |
| Retry-After?<sup>1</sup> | string | The delay in whole seconds before another request is admitted, rounded up and never below 1 |
| X-RateLimit-Scope?<sup>1</sup> | string | The [rate limit scope](#rate-limit-scopes) that produced the denial |
| X-RateLimit-Global?<sup>2</sup> | string | The literal value `true` on a global HTTP 429 |
| X-RateLimit-Limit?<sup>3</sup> | string | The maximum requests the route allowance admits at once |
| X-RateLimit-Remaining?<sup>3</sup> | string | The whole requests the route allowance still admits, always 0 on a denial |
| X-RateLimit-Reset?<sup>3</sup> <sup>4</sup> | string | The Unix timestamp in seconds at which the whole route allowance is available again |
| X-RateLimit-Reset-After?<sup>3</sup> <sup>5</sup> | string | The seconds until the whole route allowance is available again |
| X-RateLimit-Bucket?<sup>3</sup> <sup>6</sup> | string | The stable 16-character hexadecimal identifier of the route bucket |

<sup>1</sup> Sent on every rate limit denial and on no successful response

<sup>2</sup> Sent only when the global bucket produced the denial, in which case none of the route bucket headers is sent

<sup>3</sup> Sent on a route HTTP 429, subject to footnote 6 for `X-RateLimit-Bucket`. On a successful response they are sent only when the credential resolves to a bot account, or when the request resolves no account at all on a route with both a `webhook_id` and a `token` path parameter

<sup>4</sup> A value at or before the current second is replaced with the next second

<sup>5</sup> Rounded to millisecond precision with trailing zeros removed on a successful response, and emitted as the exact computed decimal on a denial

<sup>6</sup> The leading 16 hexadecimal characters of the SHA-256 digest of the route's declared bucket name before any path parameter is substituted, so it identifies the route and never the caller

A 429 from a limit enforced inside a handler has the other four route headers and no `X-RateLimit-Bucket`.

:::note[A browser client reads none of these headers]
The [cross-origin policy](/http-api/#cross-origin-requests) exposes only `X-Fluxer-Version`, so a script running on an allowed origin observes the 429 status and the response body, and no header.
:::

## Limits enforced inside a handler

Some operations bound a further allowance inside the handler. Each one is keyed independently of the route bucket and of the global bucket, so exhausting it denies the request while both buckets still have room. The set below is complete.

Two deployment switches disable part of this set. `disable_rate_limits` turns off the two login allowances along with both buckets. `relax_registration_rate_limits` turns off the three registration allowances. Every other allowance below is enforced on every deployment.

A denial takes one of two shapes. A send or submission allowance answers 429 with the [rate limit response object](#rate-limit-response-object) and the [rate limit headers](#rate-limit-headers) minus `X-RateLimit-Bucket`. A change allowance answers 400 `INVALID_FORM_BODY` with one [validation error](/http-api/#validation-error-object) entry whose `code` names the exhausted allowance.

The 400 shape has no `retry_after` member, no `X-RateLimit-*` header, and no `Retry-After` header. The remaining delay appears only in the entry's localised `message`.

### Allowances answering 429

| Operation | Allowance | Code |
| --- | --- | --- |
| [Log in with a password](/http-api/authentication/#log-in-with-a-password) | 5 per 15 minutes, keyed by the submitted email address | `RATE_LIMITED` |
| [Log in with a password](/http-api/authentication/#log-in-with-a-password) | 10 per 30 minutes, keyed by the client IP address, exactly for IPv4 and by the `/64` for IPv6 | `RATE_LIMITED` |
| [Register an account](/http-api/authentication/#register-an-account) | 3 per 15 minutes, keyed by the submitted email address | `RATE_LIMITED` |
| [Register an account](/http-api/authentication/#register-an-account) | 3 per hour, keyed by the client IP address | `RATE_LIMITED` |
| [Register an account](/http-api/authentication/#register-an-account) | 15 per hour, keyed by the client subnet, the IPv4 `/24` or the IPv6 `/48` | `RATE_LIMITED` |
| [Resend email verification](/http-api/authentication/#resend-email-verification) | 3 per 15 minutes, keyed by the account's stored email address | `RATE_LIMITED` |
| [Request password recovery](/http-api/authentication/#request-password-recovery) | 20 per 30 minutes, keyed by the client IP address | `RATE_LIMITED` |
| [Request password recovery](/http-api/authentication/#request-password-recovery) | 5 per 30 minutes, keyed by the submitted email address | `RATE_LIMITED` |
| [Start email change](/http-api/users/email-and-password/#start-email-change) and [Resend original email code](/http-api/users/email-and-password/#resend-original-email-code) | 3 sends per 15 minutes, keyed by the authenticated account | `RATE_LIMITED` |
| [Request new email](/http-api/users/email-and-password/#request-new-email), [Resend new email code](/http-api/users/email-and-password/#resend-new-email-code), and both bounced recovery sends | 5 sends per 15 minutes, keyed by the authenticated account | `RATE_LIMITED` |
| [Start password change](/http-api/users/email-and-password/#start-password-change) | 3 sends per 15 minutes, keyed by the authenticated account | `RATE_LIMITED` |
| [Resend password change code](/http-api/users/email-and-password/#resend-password-change-code) | 3 sends per 15 minutes, keyed by the authenticated account | `RATE_LIMITED` |
| Every code resend and every new-address request on an email or password change ticket | 1 send per 30 seconds, keyed by the previous send recorded on that ticket | `RATE_LIMITED` |
| [Report message](/http-api/reports/#report-message), [Report user](/http-api/reports/#report-user), [Report guild](/http-api/reports/#report-guild), and [Create DSA report](/http-api/reports/#create-dsa-report) | 5 per hour, keyed by the reporter, an account or a verified email address | `RATE_LIMITED` |
| [Report message](/http-api/reports/#report-message) | 3 per hour, keyed by the reporter and the channel together | `RATE_LIMITED` |
| [Report message](/http-api/reports/#report-message) | 20 per hour, keyed by the reported message, across all reporters | `RATE_LIMITED` |
| [Report message](/http-api/reports/#report-message) | 4 per hour, keyed by the reporter and the guild together, for a guild message | `RATE_LIMITED` |
| [Send phone verification](/http-api/users/phone-verification/#send-phone-verification) | 3 per 6 hours, keyed by the authenticated account | `PHONE_RATE_LIMIT_EXCEEDED` |
| [Send phone verification](/http-api/users/phone-verification/#send-phone-verification) | 3 per 5 days, keyed by the submitted number | `PHONE_RATE_LIMIT_EXCEEDED` |
| [Resend IP authorisation](/http-api/authentication/#resend-ip-authorisation) | Nothing in the first 30 seconds after the ticket was issued, keyed by the authorisation ticket | `IP_AUTHORIZATION_RESEND_COOLDOWN` |

Fluxer stores a further cooldown when the SMS provider itself throttles a send. The provider names the account, the number, or both, and a send inside that cooldown reports `PHONE_RATE_LIMIT_EXCEEDED` with the ordinary denial body and the remaining delay.

The Resend IP authorisation cooldown has no `X-RateLimit-*` header. It has a `Retry-After` header in whole seconds, and the body reports that delay again as a top-level `resend_available_in` and `retry_after`. A second resend on one ticket returns 400 `IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED`. The allowance never refills, and the ticket expires 15 minutes after it was issued.

### Allowances answering 400

| Operation | Allowance | Validation code |
| --- | --- | --- |
| [Modify current user](/http-api/users/current-user/#modify-current-user) | 5 per 3 hours on the resulting username or discriminator | `USERNAME_CHANGED_TOO_MANY_TIMES` |
| [Modify current user](/http-api/users/current-user/#modify-current-user) | 25 per 30 minutes on the biography, when the submitted value differs | `BIO_CHANGED_TOO_MANY_TIMES` |
| [Modify current user](/http-api/users/current-user/#modify-current-user) | 25 per 30 minutes on the pronouns, when the submitted value differs | `PRONOUNS_CHANGED_TOO_MANY_TIMES` |
| [Modify current user](/http-api/users/current-user/#modify-current-user) | 25 per 30 minutes on the accent colour, when the submitted value differs | `ACCENT_COLOR_CHANGED_TOO_MANY_TIMES` |
| [Modify current user](/http-api/users/current-user/#modify-current-user) | 25 per 30 minutes on any non-null avatar | `AVATAR_CHANGED_TOO_MANY_TIMES` |
| [Modify current user](/http-api/users/current-user/#modify-current-user) | 25 per 30 minutes on any banner value past the entitlement check | `BANNER_CHANGED_TOO_MANY_TIMES` |
| [Update bot profile](/http-api/applications/#update-bot-profile) | 5 per 3 hours on the bot's resulting username or discriminator | `USERNAME_CHANGED_TOO_MANY_TIMES` |
| [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) | 25 per 30 minutes on the guild avatar, whenever supplied | `AVATAR_CHANGED_TOO_MANY_TIMES` |
| [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) | 25 per 30 minutes on the guild banner, whenever supplied | `BANNER_CHANGED_TOO_MANY_TIMES` |
| [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) | 25 per 30 minutes on the guild biography, when the submitted value differs | `BIO_CHANGED_TOO_MANY_TIMES` |
| [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) | 25 per 30 minutes on the guild pronouns, when the submitted value differs | `PRONOUNS_CHANGED_TOO_MANY_TIMES` |
| [Modify current guild member](/http-api/guild-members/#modify-current-guild-member) | 25 per 30 minutes on the guild accent colour, when the submitted value differs | `ACCENT_COLOR_CHANGED_TOO_MANY_TIMES` |
| [Modify voice activity sharing](/http-api/users/settings/#modify-voice-activity-sharing) | 1 per 24 hours on the sharing default | `VOICE_ACTIVITY_SHARING_ON_COOLDOWN` |
| [Complete login with TOTP](/http-api/authentication/#complete-login-with-totp) and [Complete login with WebAuthn MFA](/http-api/authentication/#complete-login-with-webauthn-mfa) | 10 per 15 minutes on one multi-factor attempt | `INVALID_CODE` |
| [Complete login with TOTP](/http-api/authentication/#complete-login-with-totp) and [Complete login with WebAuthn MFA](/http-api/authentication/#complete-login-with-webauthn-mfa) | 5 per 5 minutes on one multi-factor attempt against one MFA ticket | `INVALID_CODE` |
| [Sudo mode](/http-api/users/mfa/#sudo-mode) with the `totp` method | 10 per 15 minutes on one multi-factor attempt | `INVALID_MFA_CODE` |

Every Modify current user allowance is keyed by the authenticated account, and the bot tag allowance by the bot account, so an owner changing a bot's tag draws on the bot's allowance. The five guild member allowances are keyed by the guild and the member together, and one account holds a separate allowance in each guild. The login allowances are keyed by the account and by the MFA ticket respectively, and the sudo allowance by the account.

Fluxer consumes every multi-factor allowance before it checks the code, so a correct code drawn against an exhausted allowance is reported exactly like a wrong one. A correct code clears the counter. The ticket allowance also destroys the MFA ticket as it denies, and the client restarts from [Log in with a password](/http-api/authentication/#log-in-with-a-password).

### Allowances answering neither shape

[Get desktop handoff information](/http-api/authentication/#get-desktop-handoff-information) and [Complete desktop handoff](/http-api/authentication/#complete-desktop-handoff) share one failed-attempt counter keyed by the client IP address. Five failures block both operations for 15 minutes from the most recent failure, and a blocked request returns 400 `INVALID_HANDOFF_CODE` as a top-level code rather than as a validation entry. Get desktop handoff information separately permits three successful lookups for each handoff code and reports a fourth with the same top-level code.

[Refund latest purchase](/http-api/billing/#refund-latest-purchase) permits one self-serve refund every 30 days for each account and reports a request inside that window as 403 `STRIPE_REFUND_COOLDOWN_ACTIVE`.

[Slowmode](#slowmode) is enforced inside the handler as well.

## Slowmode

Slowmode limits how often one account sends a message in one channel. Fluxer reports a denial as an ordinary request failure. A denied send returns 400 `SLOWMODE_RATE_LIMITED` with a top-level `retry_after` in fractional seconds and a `Retry-After` header in whole seconds. The response has no `X-RateLimit-*` header, so a client tells it apart from a bucket denial by the status and the code.

The allowance is one message for each interval the channel configures in `rate_limit_per_user`, counted separately for each account and channel pair. Fluxer counts it only for a non-bot account sending in a guild channel whose configured interval is above zero. A caller holding [BYPASS_SLOWMODE](/http-api/permissions/) is exempt. [Get channel slowmode state](/http-api/channels/#get-channel-slowmode-state) reports the caller's remaining delay before a send is attempted.

## Other surfaces

Each protocol surface documents its own rate limit contract. The [main Gateway](/gateway/overview/) states its session, command, replay, backpressure, and admission limits in [Gateway limits and rate limits](/gateway/limits-and-rate-limits/). The [Media Proxy API](/media-proxy/overview/) has no request-count rate limit and bounds work through concurrency, payload, and deadline limits. The [upload relay](/media-proxy/upload-relay/) authorises each transfer with a bounded capability.
