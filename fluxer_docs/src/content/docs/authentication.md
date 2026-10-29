---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Authentication
description: How to send credentials to the HTTP API and Gateway, and the errors you get when one is missing or not enough.
---

Send one credential in the `Authorization` header. Each operation says which credential types it accepts. The [login and registration routes](/http-api/authentication/) and [OAuth2](/http-api/oauth2/) issue and revoke credentials.

Anyone who has a credential can act as its owner. Keep credentials out of URLs, logs and source code, and treat webhook URLs, [signed media paths](/media-proxy/overview/) and [upload capabilities](/media-proxy/upload-relay/) as secrets too.

## Authorization schemes

| Value | Name | Description |
| --- | --- | --- |
| `Bot <token>` | Bot token | Authenticates the application's bot account |
| `Bearer <token>` | OAuth2 access token | Access granted by an account, limited to the token's scopes |
| `<token>` | User session token | A logged-in user's session, also accepted as `Bearer <token>` |
| `Admin <token>` | Admin API key | Accepted only below `/v1/admin` |

Scheme prefixes are case-sensitive.

## Token formats

A bot token has the form `<application_id>.<secret>`. Don't rely on the format of any other token.

:::caution[A secret is shown once]
A bot token, Admin API key or client secret can't be read back after the response that created it.
:::

Rotating a bot token or client secret invalidates the old value immediately, and rotating a bot token also ends its Gateway sessions. To replace an Admin API key, revoke it and create a new one.

## Bot tokens

A bot can't use an operation restricted to user accounts and gets 403 `ACCESS_DENIED`. Login, password recovery and the other [login and registration routes](/http-api/authentication/) return 403 `BOT_USER_AUTH_ENDPOINT_ACCESS_DENIED` when the account is a bot.

## OAuth2 access tokens

An OAuth2 access token acts for the account that authorized it. Only operations that support OAuth2 accept access tokens, and any other user operation returns 403 `ACCESS_DENIED`.

An operation that accepts both a session token and an access token checks scopes only on the access token. An operation that accepts only access tokens returns 401 `UNAUTHORIZED` for any other credential, and a missing scope returns 403 `MISSING_OAUTH_SCOPE` (see [OAuth2 scopes](/http-api/oauth2/#oauth2-scopes)).

### Missing OAuth2 scope body

The response body has this field alongside `code` and `message`.

| Field | Type | Description |
| --- | --- | --- |
| required_scope | string | The [OAuth2 scope](/http-api/oauth2/#oauth2-scopes) the request did not have |

## Admin API keys

An Admin API key authenticates as its creator on routes below `/v1/admin`. A session token works there too, and so does an OAuth2 bearer token from the built-in Admin application. A bearer token from any other application returns 403 `ACCESS_DENIED`, and a bot token returns 401 `UNAUTHORIZED`.

The user needs the `admin:authenticate` ACL or `*`, otherwise the request returns 403 `MISSING_PERMISSIONS`. A request with a key also needs the operation's ACLs on both the key and its owner, otherwise it returns 403 `MISSING_ACL`. See the [Admin API](/admin-api/).

## Authorization outcomes

A protected operation returns 401 `UNAUTHORIZED` for a missing, malformed, unknown, expired or revoked credential, and the response doesn't say which. A valid identity that the operation denies returns 403 `ACCESS_DENIED`.

Requests to [threads](/http-api/threads/), [thread members](/http-api/thread-members/) and [forums](/http-api/forums/) need a bot token, or a session token with the `channel_threads` [client capability](/http-api/threads/#client-capability). Any other request to them gets 404 `NOT_FOUND` instead of 401.

A credential on an unauthenticated operation selects account-based [rate limits](/topics/rate-limits/) and can waive a [CAPTCHA](/topics/captcha/).

Repeated authentication failures can get your IP banned, and retrying a rejected credential counts as one.

## Single sign-on enforcement

When an instance enforces single sign-on, these [login and registration routes](/http-api/authentication/) and account operations return 403 `SSO_REQUIRED`. Turning on enforcement doesn't end existing sessions or revoke tokens and keys.

| Group | Operations |
| --- | --- |
| Sign-in | Identify an account, password login, discoverable WebAuthn options, WebAuthn login |
| Email code sign-in | Send and verify an email code |
| Registration | Register, start and create a website account |
| Multi-factor login | TOTP, WebAuthn MFA options, WebAuthn MFA |
| Email | Verify, resend verification, revert an email change |
| Recovery | Request, validate and reset a password, recover an account |
| IP authorization | Authorize, resend, poll |
| Account | Change password, create a recovery kit |
| Other | Get username suggestions, check username availability |

The single sign-on callback returns the same code when the provider claims match no account and the instance doesn't create accounts automatically.

An account linked to the provider can't change its email or password while enforcement is active. [Email and password](/http-api/users/email-and-password/) operations return 403 `SSO_REQUIRED`, and so does [Update current user](/http-api/users/current-user/) when the body sets `new_password` or `email_token`.

## Account restrictions

A [limited account](/http-api/users/#account-limitation) gets 403 `ACCOUNT_LIMITED` on the operations listed there. An account that still has to [set its date of birth](/http-api/users/current-user/#set-date-of-birth) gets 403 `DATE_OF_BIRTH_REQUIRED` from most operations. An unverified account can be refused operations that involve other people, as [account tiers](/http-api/users/#account-tiers) describe.

## Sudo mode

Sudo mode is a recent proof that the account holder re-verified a credential. An operation that needs it says so, and without a valid proof it returns 403 `SUDO_MODE_REQUIRED`. Send the proof as [sudo verification object](/http-api/users/mfa/#sudo-verification-object) fields in the body.

After a TOTP, passkey or emailed-code proof, the response returns a token in the `X-Fluxer-Sudo-Mode-JWT` header. Send it in that request header for five minutes to skip the proof. An account that proves sudo mode with its password gets no token and sends the password each time.

A bot account, or an account with no password, TOTP secret, WebAuthn credential or confirmed email, passes sudo mode without a proof. If that account is connected to SSO, it passes only from a session that signed in with SSO in the last 10 minutes. While the instance has SSO turned off, such an account can't pass sudo mode, and the person needs a password set before SSO goes off. An account with only a confirmed email proves it with an [emailed code](/http-api/users/mfa/#send-a-sudo-email-code). [Multi-factor authentication](/http-api/users/mfa/#sudo-mode) covers the accepted proofs and the [sudo mode methods object](/http-api/users/mfa/#sudo-mode-methods-object).

## Gateway authentication

Send a bot token or user session token in [Identify](/gateway/commands/#identify). An invalid or revoked credential closes the connection with [code `4004`](/gateway/opcodes-and-close-codes/#close-codes), and a missing `token` closes it with `4002`.

## Webhooks and media

[Webhook](/http-api/webhooks/) execution takes the webhook ID and token in the path and accepts no `Authorization` credential. The [Media Proxy](/media-proxy/overview/) ignores the `Authorization` header. A stored object is public at its path, external media needs a path signature, and a relay request needs the [upload capability](/media-proxy/upload-relay/) in its URL.
