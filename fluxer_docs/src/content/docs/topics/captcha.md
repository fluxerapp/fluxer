---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: CAPTCHA handling
description: How a client discovers the CAPTCHA provider and answers a challenge.
---

An instance can require a CAPTCHA solution on a small set of abuse-sensitive operations. A client reads the selected provider and its site key from [instance discovery](/http-api/instance/#instance-discovery-object), renders that provider's widget, and sends the solution on the gated request.

## Discovering the provider

`GET /.well-known/fluxer` publishes the [CAPTCHA configuration object](/http-api/instance/#captcha-configuration-object) inside the [instance discovery object](/http-api/instance/#instance-discovery-object). Its `provider` field names the selected [CAPTCHA provider](/http-api/instance/#captcha-providers), and `hcaptcha_site_key` or `turnstile_site_key` is that provider's site key. The other key is null, and both are null when `provider` is `none`.

The value `none` means the instance challenges no operation. A gated operation then proceeds with no CAPTCHA header. The values `hcaptcha` and `turnstile` name the provider whose widget a client renders.

Fluxer names a provider only while that provider holds both a site key and a secret key. An incomplete pair reports `none`, so a named provider means the instance enforces verification.

## Gated operations

The following operations verify a CAPTCHA while the instance enforces verification.

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

Create application, redeem gift, create private channel, and add group direct message recipient reject an unauthenticated request before Fluxer reads the CAPTCHA. The three authentication operations accept a request with no credential, and each of them returns 403 `SSO_REQUIRED` before Fluxer reads the CAPTCHA when the instance enforces single sign-on.

## Exemption

Two exemptions skip the challenge. Fluxer tests both of them before it reads the token. A request that passes either one proceeds as though the instance had no provider configured.

The first exemption is the `captcha_exempt` capability the instance account policy grants to a contact address. A policy rule matches the address itself or the domain it belongs to, so one grant can cover a whole domain. Fluxer tests it against the resolved account's email address, then against an `email` member of the request body.

The second exemption is the `APP_STORE_REVIEWER` user flag. Fluxer tests it against the resolved account, then against the account an `email` member of the request body resolves to.

The body checks parse the request body as JSON and read a string `email` member, and a body that is absent, is not JSON, or is not a JSON object yields no address. Those checks exempt a login or a registration attempt before any account is resolved.

The exemptions run before request validation on the three authentication operations, on create application, and on redeem gift. Create private channel and add group direct message recipient validate the request first, so an invalid request is rejected before any exemption is tested.

No exemption is visible in an API response. A client cannot predict one and handles a challenge on every gated operation.

## Request headers

| Field | Type | Description |
| --- | --- | --- |
| X-Captcha-Token?<sup>1</sup> | string | The solution issued by the provider widget |
| X-Captcha-Type?<sup>2</sup> | string | The provider that produced the solution, accepting `hcaptcha` or `turnstile` |

<sup>1</sup> An absent or empty value on a gated operation returns 400 `CAPTCHA_REQUIRED`

<sup>2</sup> An absent value selects the instance's configured provider, and so does any value other than `hcaptcha` or `turnstile`. Naming a provider the instance holds no secret key for returns 400 `INVALID_CAPTCHA`.

## The retry handshake

A client that has never been challenged sends the gated request without any CAPTCHA header. When the instance enforces verification and no exemption applies, that request fails with 400 `CAPTCHA_REQUIRED`.

The client then renders the selected provider's widget with its advertised site key, obtains a solution, and repeats the identical request with `X-Captcha-Token` set to the solution. It can send `X-Captcha-Type` to state which provider produced the solution.

A retry succeeds when the provider accepts the solution and fails with 400 `INVALID_CAPTCHA` when it does not.

:::caution[A solution is single-use]
The provider treats an already redeemed solution as invalid. A client obtains a new solution before retrying after `INVALID_CAPTCHA` and MUST NOT replay the previous `X-Captcha-Token` value.
:::

## Provider verification

Fluxer submits the solution to the selected provider's verification endpoint over HTTPS with a 10-second deadline. It sends the caller's client IP address alongside the solution and omits it when the request resolves none.

Any outcome other than a successful provider verdict answers 400 `INVALID_CAPTCHA`. That covers an unsuccessful verdict, a non-2xx provider status, an unparseable provider payload, the 10-second timeout, and any transport failure. A rejected solution is therefore never distinguishable from an unreachable provider.

## Error codes

| Code | Status | Description |
| --- | --- | --- |
| CAPTCHA_REQUIRED<sup>1</sup> | 400 | The operation is gated and the request has no solution |
| INVALID_CAPTCHA | 400 | The provider rejected the solution, or verification could not be completed |

<sup>1</sup> [Send phone verification](/http-api/users/phone-verification/#send-phone-verification) also answers this code when its phone attempt risk controls return a captcha decision. That operation is not gated and accepts no solution, so retrying it with `X-Captcha-Token` never helps

Both codes are defined in the [API error code registry](/http-api/errors/#api-error-code-registry), and the body of each is the ordinary [error response](/http-api/#error-response) envelope.
