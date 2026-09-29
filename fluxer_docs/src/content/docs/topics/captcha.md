---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: CAPTCHA handling
description: How a client answers the ALTCHA proof-of-work check.
---

Fluxer asks for a CAPTCHA on a small set of abuse-sensitive operations. The CAPTCHA is an [ALTCHA](https://altcha.org) proof-of-work challenge that the API issues and verifies itself. No third-party service or widget is involved, and a client solves the challenge without user input.

## Overview

The check is on by default. An administrator can turn it off, or change its `cost` and `max_counter`, in the [captcha configuration](/admin-api/instance/#captcha-configuration-object). While it is off, a gated operation proceeds with no CAPTCHA header.

The `captcha` object in [instance discovery](/http-api/instance/#captcha-configuration-object) reports `altcha` while the check is on and `none` while it is off. A client does not need it, because every challenge arrives in the error response that asks for it.

## The retry handshake

Send the request without a CAPTCHA header. A gated operation answers 400 `CAPTCHA_REQUIRED`, and the [error response](/http-api/#error-response) has two more fields.

| Field | Type | Description |
| --- | --- | --- |
| captcha_provider | string | Always `altcha` |
| altcha_challenge | object | An ALTCHA v2 challenge, with `parameters` and `signature` |

The challenge uses `PBKDF2/SHA-256`. Solve it with an ALTCHA v2 solver, then retry the same request with `X-Captcha-Token` set to the [token](#token-format). An accepted token lets the operation proceed.

A rejected token returns 400 `INVALID_CAPTCHA` with a new challenge in the same two fields. A client solves that challenge and retries again.

:::caution[A challenge is single-use]
Each challenge is accepted once and expires 10 minutes after it is issued. A replayed, expired or wrong token returns `INVALID_CAPTCHA`. A client MUST NOT resend a token after `INVALID_CAPTCHA`.
:::

The route rate limit on registration, login and password recovery runs before the check. The challenge response and the retry each use one request from that allowance.

## Token format

The token is the base64 encoding of the UTF-8 JSON object `{"challenge": {"parameters": ..., "signature": ...}, "solution": {"counter": ..., "derivedKey": ...}}`. The solution can also have `time`. Copy `parameters` and `signature` unchanged from `altcha_challenge`. A token longer than 4096 characters, or one that does not decode to this shape, returns `INVALID_CAPTCHA`.

## Gated operations

The following operations verify a CAPTCHA while the check is on.

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/auth/register | [Register an account](/http-api/authentication/#register-an-account) |
| POST | /v1/auth/login | [Log in with a password](/http-api/authentication/#log-in-with-a-password) |
| POST | /v1/auth/forgot | [Request password recovery](/http-api/authentication/#request-password-recovery) |
| POST | /v1/oauth2/applications | [Create application](/http-api/applications/#create-application) |
| POST | /v1/gifts/{code}/redeem | [Redeem gift](/http-api/gifts/#redeem-gift) |
| POST | /v1/users/@me/channels | [Create private channel](/http-api/users/private-channels/#create-private-channel) |
| PUT | /v1/channels/{channel_id}/recipients/{user_id} | [Add group direct message recipient](/http-api/channels/#add-group-direct-message-recipient) |

Create private channel is gated only on the group direct message path, where the request body has a `recipients` member. A one-to-one direct message request omits the field and is never gated.

[Send phone verification](/http-api/users/phone-verification/#send-phone-verification) verifies a CAPTCHA only when the phone verification service asks for one. It then answers and accepts the handshake the same way. When the check is off or the account is exempt, that send is refused with `PHONE_RATE_LIMIT_EXCEEDED` instead.

## Exemption

Fluxer skips the check in three cases, and the operation then proceeds with no CAPTCHA header. The authenticated account's email address is on an exempt domain. The authenticated account holds the [`APP_STORE_REVIEWER`](/admin-api/users/#account-flags) flag. The request body has an `email` that belongs to an account holding that flag. Discovery does not report exemptions, so clients must handle a challenge on every gated operation.

## Error codes

| Code | Status | Description |
| --- | --- | --- |
| CAPTCHA_REQUIRED | 400 | The operation is gated and the request has no token |
| INVALID_CAPTCHA | 400 | The token is malformed, expired, already used, or wrong |

Both codes are defined in the [API error code registry](/http-api/errors/#api-error-code-registry). Both bodies are the ordinary [error response](/http-api/#error-response) envelope with `captcha_provider` and `altcha_challenge` added.
