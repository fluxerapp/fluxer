---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Authentication
description: Credential syntax, token formats, authorisation outcomes, and sudo mode.
---

An authenticated request has one credential in the `Authorization` header. Fluxer accepts four kinds. A client acting for a person sends a user session token, an application's bot sends a bot token, a client acting on a user's behalf under OAuth2 sends an access token, and the [Admin API](/admin-api/) takes an Admin API key. The kind decides who Fluxer treats as the caller and which [authorisation policy](#authorisation-outcomes) the matched operation applies.

Fluxer returns every failure named here in the standard [error response](/http-api/#error-response) envelope. The operations that issue and revoke credentials belong to the [Authentication HTTP API](/http-api/authentication/) and the [OAuth2 HTTP API](/http-api/oauth2/).

:::caution[Credential namespaces are distinct]
`Bot`, `Bearer`, and `Admin` select different validation paths. A bot token, OAuth2 access token, user session token, [upload capability](/media-proxy/upload-relay/), or [signed media path](/media-proxy/overview/) cannot substitute for another credential type.
:::

## Credential handling

Every credential described here is a bearer secret, so whoever holds the value can act as its owner. A client MUST keep it out of logs, analytics, crash reports, source code, screenshots, and error messages, and MUST store it only while it needs the credential. A client MUST use TLS whenever the credential crosses a network it does not fully trust.

An `Authorization` credential MUST NOT be copied into a URL. Webhook tokens, signed media paths, and relay capabilities are in their documented URLs, so a client treats each complete URL as secret until it expires or is revoked. Redirect targets, request traces, and referrer data MUST NOT disclose those URLs to an unrelated origin.

## Authorization header

The header value must have no leading or trailing whitespace. A padded value never authenticates. A value beginning with `Bot `, `Bearer `, or `Admin ` selects that scheme, and the remainder must be non-empty and must have no surrounding whitespace either. The three prefixes match exactly, so any other spelling is not recognised as a scheme.

A value containing no space is parsed as a bare user session token. A value containing a space without a recognised scheme prefix is invalid.

An invalid, unknown, or unresolvable credential leaves the request unauthenticated, and the matched operation's authorisation policy decides the outcome. An operation that requires a credential returns 401 `UNAUTHORIZED`.

A user session token is sent bare, with no scheme prefix.

```text
Authorization: flx_ZDb1GURItsMuYl1zvrgxv2qLBxyNmgNSEaWT
```

The value after `Bot ` is the application's snowflake, a full stop, and the secret.

```text
Authorization: Bot 1501314428688998182.aDFplg-fYQ4of4I7dM-q9tRxkgmjslc2LSE_BTwsAUo
```

The value after `Bearer ` is an OAuth2 access token.

```text
Authorization: Bearer IE867jBd9L4M0_tGI8OUOppXVezR1u6x8Yj-Lduilxg
```

The value after `Admin ` is an Admin API key, which is read on a route below `/v1/admin` and nowhere else.

```text
Authorization: Admin fa_1508923117441703936_KaqkNax1BF3YSWHGkEPjDRKeO48jGb9F
```

### <span id="authorization-schemes"></span>Authorisation schemes

| Value | Name | Description |
| --- | --- | --- |
| `Bot <token>` | Bot token | The token is `<application_id>.<secret>`, accepted only while the application owns an active bot user and the secret is current |
| `Bearer <token>` | OAuth2 access token<sup>1</sup> | The token is resolved as an OAuth2 access token. Access is limited by its application, the account that granted it, and its scopes |
| `flx_<36 alphanumeric characters>` | User session token | A header value containing no space is parsed as a user session token whatever its shape<sup>2</sup> |
| `Admin <token>` | Admin API key<sup>3</sup> | The key is read only on a route below `/v1/admin`. Effective Admin ACLs are bounded by both the key and its owning user |

<sup>1</sup> One value is handled differently. `Bearer flx_` followed by 36 alphanumeric characters authenticates the user session it names

<sup>2</sup> A bare value that resolves to no live session leaves the request unauthenticated

<sup>3</sup> A valid key presented on any other route is ignored and the request stays unauthenticated

## Token formats

| Value | Name | Description |
| --- | --- | --- |
| `flx_<36 characters>` | User session token<sup>1</sup> | The literal prefix `flx_` followed by exactly 36 characters drawn from ASCII letters and digits |
| `<application_id>.<secret>` | Bot token<sup>2</sup> | The application's decimal snowflake, a single full stop, and the base64url secret |
| `<secret>` | OAuth2 access token | A 43-character unpadded base64url secret presented with the `Bearer` scheme |
| `fa_<key_id>_<32 characters>` | Admin API key<sup>3</sup> | The literal prefix `fa_`, the key's decimal [snowflake](/snowflakes/), an underscore, and 32 characters drawn from ASCII letters and digits |

<sup>1</sup> The token is opaque and has no client-readable claims

<sup>2</sup> The identifier before the full stop selects which application record to check, and the secret after it is the only part that authorises the request

<sup>3</sup> A key whose identifier segment is not a decimal integer is invalid

OAuth2 access tokens, OAuth2 refresh tokens, OAuth2 authorisation codes, bot token secrets, and application client secrets are 43-character unpadded base64url values.

:::caution[A secret is shown once]
A bot token, an Admin API key, and a client secret cannot be read back after the response that created them. The only retained fragment is the bot token preview, the first 8 characters of the secret.
:::

:::note[Rotation invalidates the previous value immediately]
Rotation applies to a bot token and a client secret, and rotating a bot token also terminates every Gateway session the bot holds. An Admin API key is not rotated. It is revoked and replaced.
:::

## User session tokens

A user session token authenticates an ordinary user account. It is issued by the login, registration, and session exchange operations documented in [Authentication](/http-api/authentication/). A token that does not identify a live session leaves the request unauthenticated.

A session token is also the credential the Gateway [Identify](/gateway/commands/#identify) command accepts for a user session.

The `Authorization` header holds a single credential. A [sudo mode](#sudo-mode) proof travels separately, in the `X-Fluxer-Sudo-Mode-JWT` header, and it proves that the already resolved account recently re-verified.

## Bot tokens

A bot token is the owning application's [snowflake](/snowflakes/), a single full stop, and a secret. It is valid only while the application has an active bot user and the secret is current.

The Gateway accepts a bot token in [Identify](/gateway/commands/#identify), and so do [`GET /v1/gateway/bot`](/http-api/gateway/#get-gateway-information) and [`GET /v1/applications/@me`](/http-api/applications/#get-bot-application). Those two operations match the scheme prefix without regard to case. `GET /v1/applications/@me` requires the `Bot` prefix and returns 401 `INVALID_TOKEN` for anything else.

A bot cannot use an operation restricted to ordinary user accounts, and such an operation returns 403 `ACCESS_DENIED`. An operation in [Authentication](/http-api/authentication/) that resolves an account from its request body or token, such as login, password recovery, email verification, email revert, and IP authorisation, returns 403 `BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED` when that account is a bot.

## OAuth2 access tokens

An OAuth2 access token is an unprefixed secret presented with the `Bearer` scheme. `Bearer` selects OAuth2 validation. One value is handled differently: `Bearer flx_` followed by 36 alphanumeric characters authenticates the user session it names.

Every access token is bound to an account. The token operation implements the authorisation code grant and the refresh token grant alone.

Fluxer admits a bearer credential only on an operation that explicitly opts in. On an operation that requires a user but has not opted in, a valid bearer credential returns 403 `ACCESS_DENIED`.

An operation that opts in either requires the bearer credential outright or accepts a session credential and enforces the scope requirement only when the credential is a bearer token. An operation that requires the bearer credential outright and receives a valid session, bot, or Admin API key credential returns 401 `UNAUTHORIZED`.

A missing scope returns 403 `MISSING_OAUTH_SCOPE`. An operation that names a scope names exactly one, and Fluxer matches that scope exactly against the set the token was granted. The [OAuth2 scope registry](/http-api/oauth2/#oauth2-scopes) is closed, and the authorisation code flow, token exchange, refresh, revocation, and introspection operations are specified in the [OAuth2 HTTP API](/http-api/oauth2/).

### Missing OAuth2 scope body

The response body has this member alongside `code` and `message`.

| Field | Type | Description |
| --- | --- | --- |
| required_scope | string | The [OAuth2 scope](/http-api/oauth2/#oauth2-scopes) the request did not have |

## Admin API keys

An Admin API key is read only on a route below `/v1/admin`, and an unknown, expired, or invalid key leaves the request unauthenticated. A valid key authenticates as the user who created it and has the ACLs stored on the key.

An Admin operation also accepts a user session token and an OAuth2 bearer token, and a bearer token is accepted only when it belongs to the built-in Admin OAuth2 application. A bearer token from any other application returns 403 `ACCESS_DENIED`. A request with a bot token returns 401 `UNAUTHORIZED`.

On every Admin request the resolved user must hold the `admin:authenticate` ACL or the wildcard, and a user without either returns 403 `MISSING_PERMISSIONS`. A key-authenticated request is checked twice, and either failure returns 403 `MISSING_ACL`. The [Admin API](/admin-api/) hub defines the complete ACL registry, the evaluation modes, the double check, and the audit contract.

## Authorisation outcomes

An operation that requires a credential declares one authorisation policy. There are four.

A user operation requires a resolved user and rejects an OAuth2 bearer credential it has not opted into. A user-only operation rejects a bot account as well. A bot operation accepts a bot token, which resolves the application's bot account as the request identity. An OAuth2 operation requires the `Bearer` scheme together with the scope it names. An Admin operation requires a session, Admin OAuth2 bearer, or Admin API key credential together with the required ACLs.

No authorisation policy requires the `Bot` scheme itself. [`GET /v1/applications/@me`](/http-api/applications/#get-bot-application) is the only operation that requires the prefix.

Fluxer still parses and resolves a credential sent to an operation that requires none. The resolved account keys the [rate limit](/topics/rate-limits/) buckets and can waive a [captcha](/topics/captcha/) requirement. Some operations read the resolved account or the raw header, and each states that on its own page.

Fluxer answers with 401 when it resolves no usable identity, and with 403 when it resolves one the operation refuses.

A missing, malformed, unknown, expired, or revoked credential returns 401 `UNAUTHORIZED`. A bot token on an Admin operation and a non-bearer credential on a bearer-only operation return 401 as well.

A resolved identity that the operation refuses returns 403 `ACCESS_DENIED`. That is the outcome for a bot account on a user-only operation and for a bearer credential on an operation that did not opt into OAuth2. An Admin OAuth2 bearer credential issued to an application other than the built-in Admin application returns 403 `ACCESS_DENIED` as well.

Other authorisation failures use `MISSING_OAUTH_SCOPE` for a missing scope, and `MISSING_ACL` or `MISSING_PERMISSIONS` for an Admin ACL failure. Fluxer sends no `WWW-Authenticate` header on a 401, so a client distinguishes the outcomes by `code` alone.

The [account state gate](#account-state-gates) below runs wherever the ordinary login requirement runs.

## Single sign-on enforcement

An instance can enforce single sign-on. Enforcement is active only while single sign-on is enabled, its configuration is ready, and enforcement is switched on.

While enforcement is active, an operation that uses a locally held credential returns 403 `SSO_REQUIRED`. [Authentication](/http-api/authentication/) defines all of them:

- [Register an account](/http-api/authentication/#register-an-account) and [Log in with a password](/http-api/authentication/#log-in-with-a-password).
- [Get discoverable WebAuthn options](/http-api/authentication/#get-discoverable-webauthn-options) and [Authenticate with WebAuthn](/http-api/authentication/#authenticate-with-webauthn).
- [Complete login with TOTP](/http-api/authentication/#complete-login-with-totp), [Get WebAuthn MFA options](/http-api/authentication/#get-webauthn-mfa-options), and [Complete login with WebAuthn MFA](/http-api/authentication/#complete-login-with-webauthn-mfa).
- [Verify an email address](/http-api/authentication/#verify-an-email-address) and [Resend email verification](/http-api/authentication/#resend-email-verification).
- [Request password recovery](/http-api/authentication/#request-password-recovery), [Validate a password reset token](/http-api/authentication/#validate-a-password-reset-token), and [Reset a password](/http-api/authentication/#reset-a-password).
- [Revert an email change](/http-api/authentication/#revert-an-email-change).
- [Authorise an IP address](/http-api/authentication/#authorise-an-ip-address), [Resend IP authorisation](/http-api/authentication/#resend-ip-authorisation), and [Poll IP authorisation](/http-api/authentication/#poll-ip-authorisation).
- [Get username suggestions](/http-api/authentication/#get-username-suggestions).

The single sign-on callback returns the same code when the provider claims match no existing account and the instance does not auto-provision.

Enforcement applies at those operations only. It does not gate password change or multi-factor management on an already authenticated account, and enabling it leaves an already issued session token, bot token, OAuth2 access token, or Admin API key valid.

## Account state gates

The ordinary login requirement rejects an account that has effective suspicious activity flags with 403 `ACCOUNT_SUSPICIOUS_ACTIVITY`.

A requirement disappears from the response as soon as it is met.

### Account suspicious activity body

| Field | Type | Description |
| --- | --- | --- |
| data | object | An object whose `suspicious_activity_flags` member is the integer [suspicious activity flag](/admin-api/users/#suspicious-activity-flags) bitfield still outstanding |

A route that explicitly admits restricted accounts still accepts the credential. These stay reachable while a requirement is outstanding:

- [Get current user](/http-api/users/current-user/#get-current-user) and [Modify current user](/http-api/users/current-user/#modify-current-user).
- [Get current user settings](/http-api/users/settings/#get-current-user-settings).
- The [email change flow](/http-api/users/email-and-password/) with its bounced-address variants, and the email verification resend.
- The [phone verification](/http-api/users/phone-verification/) flow.
- Session listing and session termination.
- The application and authorisation management operations in [Applications](/http-api/applications/) and [OAuth2](/http-api/oauth2/).

An Admin operation applies no suspicious activity gate.

No shared gate rejects a deleted or disabled account. Each operation that reads account state applies its own rule, and login, password, and email operations refuse a deleted account outright.

## Failed authentication

An unknown, expired, revoked, or malformed credential all return 401 `UNAUTHORIZED`. A client cannot tell the four apart from the response. A valid credential whose identity the operation resolves and refuses returns 403 `ACCESS_DENIED`, as [authorisation outcomes](#authorisation-outcomes) sets out.

Fluxer records a malformed header and a credential that resolves nothing against the originating address. An Admin API key presented outside `/v1/admin` records nothing.

Two triggers ban an address. Enough distinct rejected tokens inside the tracking window ban it on the first crossing. A failure score over its threshold bans it only after it crosses that threshold in several separate windows. The window and both thresholds are instance configuration. Fluxer never applies an automatic ban to an address it classifies as mobile. A banned address is refused before the operation runs, as [Errors](/http-api/errors/) sets out.

## Sudo mode

Sudo mode is a short-lived proof that the account holder recently re-verified a credential. Each operation that requires it states that on its own page, and [Multi-factor authentication](/http-api/users/mfa/#sudo-mode) defines the accepted proofs, the [sudo verification object](/http-api/users/mfa/#sudo-verification-object) fields, and the [sudo mode methods object](/http-api/users/mfa/#sudo-mode-methods-object) returned with 403 `SUDO_MODE_REQUIRED`.

A sudo proof is an HS256 JSON Web Token with the account ID as its subject, the fixed claim `type` set to `sudo`, an issue time, and an expiry five minutes after issue. A client presents it in the `X-Fluxer-Sudo-Mode-JWT` request header. An invalid, expired, or account-mismatched token is treated as absent. A bad proof and a missing proof produce the same response.

Fluxer mints a token only for an account holding a multi-factor authenticator, so a password-only account re-verifies for each operation that requires sudo mode. [Create WebAuthn registration options](/http-api/users/mfa/#create-webauthn-registration-options) and [Disable current account](/http-api/users/current-user/#disable-current-account) mint no token and return no header even for a multi-factor account. A bot account satisfies sudo mode immediately. So does an account that has neither a password nor a multi-factor authenticator.

:::note[A sudo proof covers every account session]
The check covers only the signature, the `type` claim, the subject, and the expiry. Revoking the session that obtained a proof leaves that proof valid.
:::

## Gateway authentication

`GET /v1/gateway/bot` accepts a bot token with the `Bot` prefix, with the `Bearer` prefix, or with no prefix at all, and an absent or empty header returns 401 `MISSING_AUTHORIZATION`. The operation then checks the form alone. The value must not begin with `flx_` and must have a decimal identifier before an interior full stop, so a user session token returns 401 `INVALID_AUTH_TOKEN`.

A value in bot token form that matches no application receives the same response as a valid bot token. The same bot token is the credential in the [Identify command](/gateway/commands/#identify).

A user session presents its session token in Identify. The Gateway does not read the `Authorization` header. An invalid or revoked credential closes the connection with [close code `4004`](/gateway/opcodes-and-close-codes/#close-codes). An Identify payload that has no `token` closes with `4002` and reason `Invalid identify payload`.

## Other credential surfaces

A webhook execution operation has the webhook identifier and token in its request path, and it accepts no `Authorization` credential. [Webhooks](/http-api/webhooks/) defines its contract.

The [Media Proxy API](/media-proxy/overview/) does not read the `Authorization` header on an ordinary media or relay route. A stored object is addressed by its path, external media is authorised by its path signature, and a relay request is authorised by the capability embedded in its URL. [Upload relay](/media-proxy/upload-relay/) defines the capability contract.
