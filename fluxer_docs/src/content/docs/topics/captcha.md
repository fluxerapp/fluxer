---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: CAPTCHA handling
description: How a client discovers the CAPTCHA provider and answers a challenge.
---

An instance can require a CAPTCHA solution on a small set of abuse-sensitive operations. A client reads the selected provider and its site key from [instance discovery](/http-api/instance/#instance-discovery-object), renders that provider's widget, and sends the solution on the gated request.

## Discovering the provider

`GET /.well-known/fluxer` publishes the [CAPTCHA configuration object](/http-api/instance/#captcha-configuration-object) inside the [instance discovery object](/http-api/instance/#instance-discovery-object). Its `provider` field names the selected [CAPTCHA provider](/http-api/instance/#captcha-providers), and `hcaptcha_site_key` or `turnstile_site_key` is that provider's site key. The other key is null, and both are null when `provider` is `none`.

The value `none` means the instance challenges no operation. A gated operation then proceeds with no CAPTCHA header. The values `hcaptcha` and `turnstile` name the provider whose widget a client renders.

## Gated operations

The following operations verify a CAPTCHA while discovery reports a `provider` other than `none`.

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

## Exemption

Fluxer skips the check in three cases, and the operation then proceeds with no CAPTCHA header. The instance account policy grants the `captcha_exempt` capability to the authenticated account's email address. The authenticated account holds the [`APP_STORE_REVIEWER`](/admin-api/users/#account-flags) flag. The request body has an `email` that belongs to an account holding that flag. Discovery does not report exemptions, so clients must handle a challenge on every gated operation.

## Request headers

| Field | Type | Description |
| --- | --- | --- |
| X-Captcha-Token?<sup>1</sup> | string | The solution issued by the provider widget |
| X-Captcha-Type?<sup>2</sup> | string | The provider that produced the solution, accepting `hcaptcha` or `turnstile` |

<sup>1</sup> An absent or empty value on a gated operation returns 400 `CAPTCHA_REQUIRED`

<sup>2</sup> An absent value selects the instance's configured provider, and so does any value other than `hcaptcha` or `turnstile`. Naming a provider the instance holds no secret key for returns 400 `INVALID_CAPTCHA`.

## The retry handshake

Send the request without CAPTCHA headers. On 400 `CAPTCHA_REQUIRED`, obtain a solution through the selected provider's widget using its advertised site key. Retry the same request with `X-Captcha-Token` set to the solution and, optionally, `X-Captcha-Type` set to the provider.

An accepted solution allows the operation to proceed. A rejected solution returns 400 `INVALID_CAPTCHA`.

:::caution[A solution is single-use]
The provider treats an already redeemed solution as invalid. A client obtains a new solution before retrying after `INVALID_CAPTCHA` and MUST NOT replay the previous `X-Captcha-Token` value.
:::

## Provider verification

A rejected solution or unavailable provider returns 400 `INVALID_CAPTCHA`. The response does not distinguish between these causes.

## Error codes

| Code | Status | Description |
| --- | --- | --- |
| CAPTCHA_REQUIRED<sup>1</sup> | 400 | The operation is gated and the request has no solution |
| INVALID_CAPTCHA | 400 | The provider rejected the solution, or verification could not be completed |

<sup>1</sup> [Send phone verification](/http-api/users/phone-verification/#send-phone-verification) also answers this code when Fluxer's risk check on the phone attempt decides that the request needs a CAPTCHA. That operation is not gated and accepts no solution, so retrying it with `X-Captcha-Token` never helps

Both codes are defined in the [API error code registry](/http-api/errors/#api-error-code-registry), and the body of each is the ordinary [error response](/http-api/#error-response) envelope.
