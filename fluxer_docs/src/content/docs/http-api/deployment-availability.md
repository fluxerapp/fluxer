---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Deployment availability
description: The routes a self-hosted deployment may not serve and the instance flags that decide them.
---

The routes listed under [Conditional routes](#conditional-routes) depend on the deployment. The deployment Fluxer hosts serves all of them. A self-hosted deployment serves some of them only while its operator runs a premium tier or sells it, and never serves the rest. Both deployments run the same release, and most of the HTTP API is identical on both.

When a self-hosted deployment does not serve one of these routes, a request to it returns 404 `NOT_FOUND` with no feature-specific code, so a caller cannot tell an unavailable route from an unrecognised path.

Credentials, permissions, premium state and OAuth2 scopes do not change route availability. Read the deployment kind and the premium flags from instance discovery.

## Deployment kind

Every deployment reports its kind in `self_hosted` on the [instance features object](/http-api/instance/#instance-features-object). The unauthenticated [instance discovery document](/http-api/instance/#get-instance-discovery) publishes it before a client holds any credential. A hosted deployment serves every route below whatever the other flags say.

On a self-hosted deployment, three flags on the same object decide which routes below are served. All three are computed on each request, so an operator's change applies without a restart.

- `premium_enabled` is true while the instance [premium mode](/admin-api/instance/#premium-modes) is `mirror`. The routes under [Served in the mirror premium mode](#served-in-the-mirror-premium-mode) are served only then.
- `stripe_enabled` is true while billing is active. Billing is active when the operator has switched it on, set a Stripe secret key, and set a monthly and a yearly price for at least one currency, and `premium_enabled` is true. The routes under [Served while billing is active](#served-while-billing-is-active) are served only then.
- `stripe_serviceable` is true while a Stripe secret key is set and `premium_enabled` is true. The routes under [Served while Stripe is serviceable](#served-while-stripe-is-serviceable) are served only then, so existing subscriptions can still be managed after the operator stops sales.
- The routes under [Never served on a self-hosted deployment](#never-served-on-a-self-hosted-deployment) are never served.

[Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) also needs a Stripe webhook secret.

:::caution[Read `self_hosted` for the deployment kind]
`stripe_enabled`, `stripe_serviceable` and `premium_enabled` do not promise that a provider-dependent operation succeeds.
:::

When billing is switched off or no Stripe secret key is configured on a hosted deployment, the answer depends on the operation. An operation that has to reach the provider fails with 400 `STRIPE_PAYMENT_NOT_AVAILABLE`, and [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) fails with 400 `STRIPE_WEBHOOK_NOT_AVAILABLE`. These read operations report the absence in a 200 body instead:

- [Get refund eligibility](/http-api/billing/#get-refund-eligibility) reports `eligible` false with the reason `feature_unavailable`.
- [Get current subscription price](/http-api/premium/#get-current-subscription-price) reports null.
- [Get price IDs](/http-api/premium/#get-price-ids) reports the configured price IDs with every amount null.

## Conditional routes

### Served in the mirror premium mode

| Method | Route | Operation |
| --- | --- | --- |
| GET | /v1/gifts/{code} | [Get gift](/http-api/gifts/#get-gift) |
| POST | /v1/gifts/{code}/redeem | [Redeem gift](/http-api/gifts/#redeem-gift) |
| GET | /v1/users/@me/gifts<sup>1</sup> | [List current user gifts](/http-api/users/gifts/#list-current-user-gifts) |

### Served while billing is active

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/stripe/checkout/subscription | [Create subscription checkout](/http-api/billing/#create-subscription-checkout) |
| POST | /v1/stripe/checkout/gift | [Create gift checkout](/http-api/billing/#create-gift-checkout) |
| GET | /v1/premium/price-ids | [Get price IDs](/http-api/premium/#get-price-ids) |

### Served while Stripe is serviceable

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/stripe/webhook<sup>2</sup> | [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) |
| GET | /v1/premium/current-subscription-price<sup>3</sup> | [Get current subscription price](/http-api/premium/#get-current-subscription-price) |
| POST | /v1/premium/customer-portal | [Create customer portal](/http-api/premium/#create-customer-portal) |
| POST | /v1/premium/grace/end | [End premium grace period](/http-api/premium/#end-premium-grace-period) |
| POST | /v1/premium/cancel-subscription | [Cancel subscription](/http-api/premium/#cancel-subscription) |
| POST | /v1/premium/reactivate-subscription | [Reactivate subscription](/http-api/premium/#reactivate-subscription) |
| POST | /v1/premium/change-subscription | [Change subscription billing cycle](/http-api/premium/#change-subscription-billing-cycle) |
| POST | /v1/premium/cancel-pending-subscription-change | [Cancel pending subscription change](/http-api/premium/#cancel-pending-subscription-change) |

### Never served on a self-hosted deployment

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/donations/request-link | [Request donation management link](/http-api/donations/#request-donation-management-link) |
| GET | /v1/donations/manage | [Manage donation](/http-api/donations/#manage-donation) |
| POST | /v1/donations/checkout | [Create donation checkout](/http-api/donations/#create-donation-checkout) |
| POST | /v1/users/@me/age-verification<sup>1</sup> | [Create age verification session](/http-api/billing/#create-age-verification-session) |
| GET | /v1/premium/refund-eligibility<sup>4</sup> | [Get refund eligibility](/http-api/billing/#get-refund-eligibility) |
| POST | /v1/premium/refund-latest | [Refund latest purchase](/http-api/billing/#refund-latest-purchase) |
| POST | /v1/premium/switch-to-list-price | [Switch subscription to the list price](/http-api/premium/#switch-subscription-to-the-list-price) |
| POST | /v1/premium/visionary/rejoin | [Rejoin Visionary guild](/http-api/premium/#rejoin-visionary-guild) |
| GET | /v1/premium/store | [Get in-app purchase context](/http-api/in-app-purchases/#get-in-app-purchase-context) |
| POST | /v1/premium/store/app-store/transactions | [Claim App Store transaction](/http-api/in-app-purchases/#claim-app-store-transaction) |
| POST | /v1/premium/store/google-play/purchases | [Claim Google Play purchase](/http-api/in-app-purchases/#claim-google-play-purchase) |
| GET | /v1/premium/store/purchases | [List in-app purchases](/http-api/in-app-purchases/#list-in-app-purchases) |
| DELETE | /v1/premium/store/purchases/{purchase_id} | [Release in-app subscription](/http-api/in-app-purchases/#release-in-app-subscription) |

<sup>1</sup> These are the only `/users/@me` routes a self-hosted deployment may not serve

<sup>2</sup> The webhook takes no credential and is authenticated by the provider signature header alone

<sup>3</sup> The same object appears as `billing.current_subscription_price` on [Get premium state](/http-api/premium/#get-premium-state), which every deployment serves

<sup>4</sup> The same object appears as `billing.refund_eligibility` on [Get premium state](/http-api/premium/#get-premium-state), which every deployment serves, and there a self-hosted deployment reports `eligible` false with the reason `feature_unavailable`

The two [store notification webhooks](/http-api/in-app-purchases/#store-notification-webhooks) are never served on a self-hosted deployment either.

## Registered routes that resolve differently

A route every deployment registers can still produce a different answer on a self-hosted instance. Each operation page documents that difference.

Premium state is the clearest case. Every deployment registers [Get premium state](/http-api/premium/#get-premium-state) and [Set premium perks disabled](/http-api/premium/#set-premium-perks-disabled). A self-hosted instance still reports premium state and still records the perks-disabled flag. The response repeats the deployment kind in `self_hosted` on the [effective premium state object](/http-api/premium/#effective-premium-state-object). That flag alone does not make `is_premium` true. A self-hosted deployment grants premium to every account only while its instance [premium mode](/admin-api/instance/#premium-modes) is `everyone`. That mode also overrides the perks-disabled flag, so `is_premium` stays true while `premium_perks_disabled` is true.

The other instance feature flags published by [instance discovery](/http-api/instance/#instance-features-object) work the same way. `voice_enabled`, `presigned_attachment_uploads`, and `emails_enabled` each report whether a capability is switched on. The routes for that capability stay registered when the flag is false, so a client reads the flag before it uses them.
