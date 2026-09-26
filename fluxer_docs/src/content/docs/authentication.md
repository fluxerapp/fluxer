---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Authentication
description: Credential syntax, token formats, authorisation outcomes, and sudo mode.
---

Send one credential in the `Authorization` header. Each operation states which [credential types](#authorization-schemes) it accepts.

Failures use the standard [error response](/http-api/#error-response). The [Authentication HTTP API](/http-api/authentication/) and [OAuth2 HTTP API](/http-api/oauth2/) document credential issuance and revocation.

:::caution[Credential namespaces are distinct]
Bot tokens, OAuth2 access tokens, user session tokens, Admin API keys, [upload capabilities](/media-proxy/upload-relay/), and [signed media paths](/media-proxy/overview/) are not interchangeable.
:::

## Credential handling

Anyone holding a credential can act as its owner. Keep credentials out of logs, analytics, crash reports and source code, and retain them only while needed. Use TLS on untrusted networks.

Never put an `Authorization` credential in a URL. For webhook tokens, signed media paths and upload capabilities, treat the complete URL as a secret. Do not expose it through redirects, traces or referrers.

## Authorization header

Use one of the forms below with a non-empty token and no surrounding whitespace. Scheme prefixes are case-sensitive except on the two routes noted under [Bot tokens](#bot-tokens).

### <span id="authorization-schemes"></span>Authorisation schemes

| Value | Name | Description |
| --- | --- | --- |
| `Bot <token>` | Bot token | Authenticates the application's bot account |
| `Bearer <token>` | OAuth2 access token<sup>1</sup> | Access granted by an account, limited to the token's scopes |
| `<token>` | User session token | Sent without a scheme prefix |
| `Admin <token>` | Admin API key | Accepted only below `/v1/admin`, with permissions limited by the key and its owner |

<sup>1</sup> One value is handled differently. `Bearer flx_` followed by 36 alphanumeric characters authenticates the user session it names

## Token formats

Treat issued tokens as opaque values. Send them unchanged with the appropriate scheme. A bot token includes its application ID and secret as `<application_id>.<secret>`.

:::caution[A secret is shown once]
A bot token, Admin API key or client secret cannot be read back after the response that created it. A bot token preview cannot authenticate a request.
:::

:::note[Rotation invalidates the previous value immediately]
Rotation applies to a bot token and a client secret, and rotating a bot token also ends every Gateway session the bot holds. An Admin API key is not rotated. It is revoked and replaced.
:::

## User session tokens

A user session token authenticates an ordinary user account. Login, registration, and session exchange in the [Authentication HTTP API](/http-api/authentication/) issue it. The Gateway accepts it in [Identify](/gateway/commands/#identify).

A [sudo mode](#sudo-mode) proof supplements the token through the separate `X-Fluxer-Sudo-Mode-JWT` header.

## Bot tokens

The Gateway accepts a bot token in [Identify](/gateway/commands/#identify). The HTTP operations [`GET /v1/gateway/bot`](/http-api/gateway/#get-gateway-information) and [`GET /v1/applications/@me`](/http-api/applications/#get-bot-application) accept case-insensitive scheme prefixes. `GET /v1/applications/@me` specifically requires `Bot` and returns 401 `INVALID_TOKEN` for anything else. The [Gateway authentication](#gateway-authentication) section covers `GET /v1/gateway/bot`.

A bot cannot use an operation restricted to ordinary user accounts, and such an operation returns 403 `ACCESS_DENIED`. An operation in [Authentication](/http-api/authentication/) that resolves an account from its request body or token, such as login, password recovery, email verification, email revert, and IP authorisation, returns 403 `BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED` when that account is a bot.

## OAuth2 access tokens

Every OAuth2 access token belongs to an account. Supported grants are authorisation code and refresh token. See [authorisation schemes](#authorization-schemes) for the `Bearer` syntax and user session exception.

Only operations that explicitly support OAuth2 accept access tokens. A user operation without that support returns 403 `ACCESS_DENIED` for a valid access token.

An operation that accepts both a user session token and an OAuth2 access token checks the scope only on the access token. A bearer-only operation rejects a session token, bot token, or Admin API key with 401 `UNAUTHORIZED`.

A missing scope returns 403 `MISSING_OAUTH_SCOPE`. Each operation requires its named scope exactly. The [OAuth2 HTTP API](/http-api/oauth2/) defines the supported [scopes](/http-api/oauth2/#oauth2-scopes), grants, refresh, revocation, and introspection.

### Missing OAuth2 scope body

The response body has this member alongside `code` and `message`.

| Field | Type | Description |
| --- | --- | --- |
| required_scope | string | The [OAuth2 scope](/http-api/oauth2/#oauth2-scopes) the request did not have |

## Admin API keys

An Admin API key authenticates as its creator on routes below `/v1/admin`. Permissions are limited by both the key and its creator.

Fluxer also accepts a user session token or an OAuth2 bearer token on an Admin operation, and it accepts the bearer token only when it belongs to the built-in Admin OAuth2 application. A bearer token from any other application returns 403 `ACCESS_DENIED`. A request with a bot token returns 401 `UNAUTHORIZED`.

Every Admin request requires the user's `admin:authenticate` ACL or wildcard, otherwise it returns 403 `MISSING_PERMISSIONS`. Key-authenticated requests also require the operation's ACLs on both the key and its owner, otherwise they return 403 `MISSING_ACL`. The [Admin API](/admin-api/) defines the ACLs and audit contract.

## Authorisation outcomes

Each protected operation states its authentication policy:

- A user operation requires a resolved user and rejects an OAuth2 bearer credential it has not opted into. A user-only operation rejects a bot account as well.
- A bot operation accepts a bot token, which resolves the application's bot account as the request identity.
- An OAuth2 operation requires the `Bearer` scheme together with the scope it names.
- An Admin operation requires a session, Admin OAuth2 bearer, or Admin API key credential together with the required ACLs.

Only [`GET /v1/applications/@me`](/http-api/applications/#get-bot-application) requires the `Bot` prefix itself.

A credential can affect even an unauthenticated operation. It selects account-based [rate limits](/topics/rate-limits/) and can waive a [CAPTCHA](/topics/captcha/). Each operation documents any other use of the credential.

A protected operation returns 401 `UNAUTHORIZED` for a missing, malformed, unknown, expired, or revoked credential. Bot tokens on Admin operations and non-bearer credentials on bearer-only operations also return 401.

A valid identity denied by the operation returns 403 `ACCESS_DENIED`. An [Authentication](/http-api/authentication/) operation that resolves a bot account returns 403 `BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED`, as [Bot tokens](#bot-tokens) describes.

Scope and Admin permission failures use the specific codes above. A 401 has no `WWW-Authenticate` header, so clients must inspect `code`.

Ordinary authenticated operations also apply the [account state gates](#account-state-gates).

## Single sign-on enforcement

An instance can enforce single sign-on once it is configured and enabled. While enforcement is active, these [authentication operations](/http-api/authentication/) return 403 `SSO_REQUIRED`:

- [Register an account](/http-api/authentication/#register-an-account) and [Log in with a password](/http-api/authentication/#log-in-with-a-password).
- [Get discoverable WebAuthn options](/http-api/authentication/#get-discoverable-webauthn-options) and [Authenticate with WebAuthn](/http-api/authentication/#authenticate-with-webauthn).
- [Complete login with TOTP](/http-api/authentication/#complete-login-with-totp), [Get WebAuthn MFA options](/http-api/authentication/#get-webauthn-mfa-options), and [Complete login with WebAuthn MFA](/http-api/authentication/#complete-login-with-webauthn-mfa).
- [Start passkey bridge sign-in](/http-api/authentication/#start-passkey-bridge-sign-in) and [Redeem passkey bridge sign-in](/http-api/authentication/#redeem-passkey-bridge-sign-in).
- [Verify an email address](/http-api/authentication/#verify-an-email-address) and [Resend email verification](/http-api/authentication/#resend-email-verification).
- [Request password recovery](/http-api/authentication/#request-password-recovery), [Validate a password reset token](/http-api/authentication/#validate-a-password-reset-token), and [Reset a password](/http-api/authentication/#reset-a-password).
- [Revert an email change](/http-api/authentication/#revert-an-email-change).
- [Authorise an IP address](/http-api/authentication/#authorise-an-ip-address), [Resend IP authorisation](/http-api/authentication/#resend-ip-authorisation), and [Poll IP authorisation](/http-api/authentication/#poll-ip-authorisation).
- [Get username suggestions](/http-api/authentication/#get-username-suggestions).

The single sign-on callback returns the same code when the provider claims match no existing account and the instance does not auto-provision.

Enforcement applies at those operations only, and does not gate password change or multi-factor management on an already authenticated account. Enabling it leaves an already issued session token, bot token, OAuth2 access token, or Admin API key valid.

## Account state gates

An ordinary authenticated operation rejects an account that has an unmet suspicious activity requirement with 403 `ACCOUNT_SUSPICIOUS_ACTIVITY`. Each set flag in the response is one requirement the account has not met. A flag no longer appears in the response once the account meets that requirement.

### Account suspicious activity body

| Field | Type | Description |
| --- | --- | --- |
| data | object | An object whose `suspicious_activity_flags` member is the integer [suspicious activity flag](/admin-api/users/#suspicious-activity-flags) bitfield still outstanding |

A route that explicitly admits an account with an unmet suspicious activity requirement still accepts its credential. These stay reachable while a requirement is outstanding:

- [Get current user](/http-api/users/current-user/#get-current-user) and [Modify current user](/http-api/users/current-user/#modify-current-user).
- [Get current user settings](/http-api/users/settings/#get-current-user-settings).
- The [email change flow](/http-api/users/email-and-password/) with its bounced-address variants, and the email verification resend.
- The [phone verification](/http-api/users/phone-verification/) flow.
- Session listing and session termination.
- The application and authorisation management operations in [Applications](/http-api/applications/) and [OAuth2](/http-api/oauth2/).

An Admin operation applies no suspicious activity gate.

No shared gate rejects a deleted or disabled account. Each operation that reads account state applies its own rule, and login, password, and email operations refuse a deleted account outright.

## Failed authentication

Authentication errors do not distinguish between unknown, expired, revoked, or malformed credentials. See [authorisation outcomes](#authorisation-outcomes) for response codes.

Repeated authentication failures can result in an IP ban. Stop retrying a rejected credential and obtain a new one.

## Sudo mode

Sudo mode is a short-lived proof that the account holder recently re-verified a credential. Each operation that requires it states that on its own page, and [Multi-factor authentication](/http-api/users/mfa/#sudo-mode) defines the accepted proofs, the [sudo verification object](/http-api/users/mfa/#sudo-verification-object) fields, and the [sudo mode methods object](/http-api/users/mfa/#sudo-mode-methods-object) returned with 403 `SUDO_MODE_REQUIRED`.

A sudo proof lasts five minutes. Present it in the `X-Fluxer-Sudo-Mode-JWT` request header. An invalid, expired, or account-mismatched token produces the same response as a missing one.

Fluxer issues a token only for an account holding a TOTP secret or a registered WebAuthn credential, so a password-only account re-verifies for each operation that requires sudo mode. A passkey counts here whether or not the account enabled passkeys as a second factor. The `methods` object reports `totp`, `webauthn`, and `backup_codes`, and an account holding no TOTP secret presents an unconsumed backup code under `mfa_method` of `totp`. [Create WebAuthn registration options](/http-api/users/mfa/#create-webauthn-registration-options) and [Disable current account](/http-api/users/current-user/#disable-current-account) issue no sudo token and return no `X-Fluxer-Sudo-Mode-JWT` response header, even for an account that can prove a credential. A bot account satisfies sudo mode immediately. So does an account that has no password, no TOTP secret, and no registered WebAuthn credential.

:::note[A sudo proof covers every account session]
Revoking the session that obtained a proof leaves that proof valid until it expires.
:::

## Gateway authentication

Send a bot token or user session token in the [Identify command](/gateway/commands/#identify), not an `Authorization` header. An invalid or revoked credential closes the connection with [code `4004`](/gateway/opcodes-and-close-codes/#close-codes). A missing `token` closes with `4002` and reason `Invalid identify payload`.

The HTTP [Get Gateway information](/http-api/gateway/#get-gateway-information) endpoint checks only the bot token's form. A successful response does not prove the token is valid.

## Other credential surfaces

A webhook execution operation has the webhook identifier and token in its request path, and it accepts no `Authorization` credential. [Webhooks](/http-api/webhooks/) defines its contract.

The [Media Proxy API](/media-proxy/overview/) does not read the `Authorization` header on an ordinary media or relay route. A stored object is addressed by its path, external media is authorised by its path signature, and a relay request is authorised by the capability embedded in its URL. [Upload relay](/media-proxy/upload-relay/) defines the capability contract.
