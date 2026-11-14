---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: CAPTCHA handling
description: How a client solves the ALTCHA proof-of-work check.
---

Some operations return 400 `CAPTCHA_REQUIRED` with an [ALTCHA](https://altcha.org) proof-of-work challenge when the request has no solved challenge. Your client solves it without user input and retries with the `X-Captcha-Token` header.

## Turn the check on or off

The check is on by default. An administrator can turn it off or change its cost in the [captcha configuration](/admin-api/instance/#captcha-configuration-object). The `captcha.provider` field in [instance discovery](/http-api/instance/#captcha-configuration-object) is `altcha` while the check is on and `none` while it is off.

## Solve a challenge

1. Send the request without `X-Captcha-Token`.
2. Read `altcha_challenge` from the 400 `CAPTCHA_REQUIRED` [error response](/http-api/#error-response).
3. Solve it with an ALTCHA v2 solver for `PBKDF2/SHA-256`.
4. Retry the same request with `X-Captcha-Token` set to the [token](#token-format).

The error response has these fields next to `code` and `message`.

| Field | Type | Description |
| --- | --- | --- |
| captcha_provider | string | Always `altcha` |
| altcha_challenge | object | An ALTCHA v2 challenge, with `parameters` and `signature` |

A challenge works once and expires after 10 minutes. A rejected token returns 400 `INVALID_CAPTCHA` with a new challenge in the same fields.

Some operations make their challenge harder after heavy use. Solve the challenge from the latest error response.

## Token format

The token is the base64 encoding of this JSON object, and it can be at most 4096 characters. Copy `parameters` and `signature` unchanged from `altcha_challenge`.

```json
{
  "challenge": {"parameters": {}, "signature": "..."},
  "solution": {"counter": 0, "derivedKey": "..."}
}
```

`solution.time` is optional.

## Gated operations

These operations need a solved challenge while the check is on.

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/auth/register | [Register an account](/http-api/authentication/#register-an-account) |
| POST | /v1/auth/register/handoff | [Start an account for the website](/http-api/authentication/#start-an-account-for-the-website) |
| POST | /v1/auth/login | [Log in with a password](/http-api/authentication/#log-in-with-a-password) |
| POST | /v1/auth/forgot | [Request password recovery](/http-api/authentication/#request-password-recovery) |
| POST | /v1/auth/recover | [Recover an account](/http-api/authentication/#recover-an-account) |
| POST | /v1/oauth2/applications | [Create application](/http-api/applications/#create-application) |
| POST | /v1/gifts/{code}/redeem | [Redeem gift](/http-api/gifts/#redeem-gift) |
| POST | /v1/users/@me/channels | [Create private channel](/http-api/users/private-channels/#create-private-channel) |
| PUT | /v1/channels/{channel_id}/recipients/{user_id} | [Add group direct message recipient](/http-api/channels/#add-group-direct-message-recipient) |

Create private channel needs a challenge only when the body has `recipients`, which makes a group direct message. [Send an email code](/http-api/authentication/#send-an-email-code) needs one on sign-up. Send an email code, [identify an account](/http-api/authentication/#identify-an-account) and [verify an email code](/http-api/authentication/#verify-an-email-code) also need one after heavy use from one network, for one email address or across the instance.

Some accounts skip the check. Handle a challenge on every gated operation anyway.

## Error codes

| Code | Status | Description |
| --- | --- | --- |
| CAPTCHA_REQUIRED | 400 | The operation is gated and the request has no token |
| INVALID_CAPTCHA | 400 | The token is malformed, expired, already used or wrong |
