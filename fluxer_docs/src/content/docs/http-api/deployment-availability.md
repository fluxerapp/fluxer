---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Deployment availability
description: The hosted-only routes and the instance flags that report deployment kind.
---

A small set of routes exists only on the deployment Fluxer hosts. An operator runs the same release, and most of the HTTP API is identical on both.

A self-hosted deployment does not register those routes. A request to one returns 404 `NOT_FOUND` with no feature-specific code. A caller cannot tell an unavailable route from an unrecognised path.

The API decides registration once at process start from the deployment configuration. No credential, permission, premium state, or OAuth2 scope changes the answer. A client resolves the deployment kind from instance discovery.

## Deployment kind

Every deployment reports its kind in `self_hosted` on the [instance features object](/http-api/instance/#instance-features-object), which the unauthenticated [instance discovery document](/http-api/instance/#get-instance-discovery) publishes before a client holds any credential. `self_hosted` alone decides whether the API registers the routes below.

`stripe_enabled` on the same object reports the payment provider toggle alone. A hosted deployment that reports it false still serves every route in the table below. A deployment reporting `stripe_enabled` true with no provider secret key configured behaves exactly like one reporting it false.

:::caution[Read `self_hosted` for the deployment kind]
Neither flag promises that a provider-dependent operation succeeds.
:::

Without a provider client, the answer depends on the operation. An operation that has to reach the provider fails with 400 `STRIPE_PAYMENT_NOT_AVAILABLE`, and [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) fails with 400 `STRIPE_WEBHOOK_NOT_AVAILABLE`. Three read operations report the absence in a 200 body instead. [Get refund eligibility](/http-api/billing/#get-refund-eligibility) reports `eligible` false with the reason `feature_unavailable`, [Get current subscription price](/http-api/premium/#get-current-subscription-price) reports null, and [Get price IDs](/http-api/premium/#get-price-ids) reports the configured price IDs with every amount null.

## Hosted-only routes

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/donations/request-link | [Request donation management link](/http-api/donations/#request-donation-management-link) |
| GET | /v1/donations/manage | [Manage donation](/http-api/donations/#manage-donation) |
| POST | /v1/donations/checkout | [Create donation checkout](/http-api/donations/#create-donation-checkout) |
| GET | /v1/gifts/{code} | [Get gift](/http-api/gifts/#get-gift) |
| POST | /v1/gifts/{code}/redeem | [Redeem gift](/http-api/gifts/#redeem-gift) |
| GET | /v1/users/@me/gifts<sup>1</sup> | [List current user gifts](/http-api/users/gifts/#list-current-user-gifts) |
| POST | /v1/stripe/checkout/subscription | [Create subscription checkout](/http-api/billing/#create-subscription-checkout) |
| POST | /v1/stripe/checkout/subscription/preapproval | [Create localised card preapproval](/http-api/billing/#create-localised-card-preapproval) |
| POST | /v1/stripe/checkout/subscription/preapproval/continue | [Continue localised card preapproval](/http-api/billing/#continue-localised-card-preapproval) |
| POST | /v1/stripe/checkout/gift | [Create gift checkout](/http-api/billing/#create-gift-checkout) |
| POST | /v1/stripe/webhook<sup>2</sup> | [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) |
| POST | /v1/users/@me/age-verification<sup>1</sup> | [Create age verification session](/http-api/billing/#create-age-verification-session) |
| GET | /v1/premium/refund-eligibility<sup>3</sup> | [Get refund eligibility](/http-api/billing/#get-refund-eligibility) |
| POST | /v1/premium/refund-latest | [Refund latest purchase](/http-api/billing/#refund-latest-purchase) |
| GET | /v1/premium/price-ids | [Get price IDs](/http-api/premium/#get-price-ids) |
| GET | /v1/premium/current-subscription-price<sup>4</sup> | [Get current subscription price](/http-api/premium/#get-current-subscription-price) |
| POST | /v1/premium/customer-portal | [Create customer portal](/http-api/premium/#create-customer-portal) |
| POST | /v1/premium/grace/end | [End premium grace period](/http-api/premium/#end-premium-grace-period) |
| POST | /v1/premium/cancel-subscription | [Cancel subscription](/http-api/premium/#cancel-subscription) |
| POST | /v1/premium/reactivate-subscription | [Reactivate subscription](/http-api/premium/#reactivate-subscription) |
| POST | /v1/premium/change-subscription | [Change subscription billing cycle](/http-api/premium/#change-subscription-billing-cycle) |
| POST | /v1/premium/cancel-pending-subscription-change | [Cancel pending subscription change](/http-api/premium/#cancel-pending-subscription-change) |
| POST | /v1/premium/visionary/rejoin | [Rejoin Visionary guild](/http-api/premium/#rejoin-visionary-guild) |

<sup>1</sup> These two are the only `/users/@me` routes a self-hosted deployment does not serve

<sup>2</sup> The webhook takes no credential and is authenticated by the provider signature header alone

<sup>3</sup> The same object appears as `billing.refund_eligibility` on [Get premium state](/http-api/premium/#get-premium-state), which every deployment serves, and there a self-hosted deployment reports `eligible` false with the reason `feature_unavailable`

<sup>4</sup> The same object appears as `billing.current_subscription_price` on [Get premium state](/http-api/premium/#get-premium-state), which every deployment serves

## Registered routes that resolve differently

A route every deployment registers can still produce a different answer on a self-hosted instance. Each operation page documents that difference.

Premium state is the clearest case. Every deployment registers [Get premium state](/http-api/premium/#get-premium-state) and [Set premium perks disabled](/http-api/premium/#set-premium-perks-disabled). A self-hosted instance still reports premium state and still records the perks-disabled flag. The response repeats the deployment kind in `self_hosted` on the [effective premium state object](/http-api/premium/#effective-premium-state-object). That flag alone does not make `is_premium` true, because a self-hosted deployment grants premium to every account only while its instance [premium mode](/admin-api/instance/#premium-modes) is `everyone`. While that mode is in force it also overrides the perks-disabled flag, so `is_premium` stays true while `premium_perks_disabled` is true.

The other instance feature flags published by [instance discovery](/http-api/instance/#instance-features-object) work the same way. `voice_enabled`, `presigned_attachment_uploads`, and `emails_enabled` each switch off a capability that the surrounding routes still expose, so a client reads the flag.
