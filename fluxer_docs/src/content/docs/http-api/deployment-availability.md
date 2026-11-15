---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Deployment availability
description: The routes a self-hosted instance serves only while premium or billing is on, and the flags that decide them.
---

A self-hosted instance serves the routes below only while its premium mode is `mirror` or Stripe is set up. When the instance doesn't serve one, the API returns 404 `NOT_FOUND`, the same as for an unknown path.

## Flags

Three flags in the [instance features object](/http-api/instance/#instance-features-object) of the unauthenticated [instance discovery document](/http-api/instance/#get-instance-discovery) decide which of these routes an instance serves. A change to them applies without a restart, and both Stripe flags also need `premium_enabled`.

| Flag | True when |
| --- | --- |
| `premium_enabled` | The instance [premium mode](/admin-api/instance/#premium-modes) is `mirror` |
| `stripe_enabled` | Billing is switched on, a Stripe secret key is set and at least one currency has monthly and yearly prices |
| `stripe_serviceable` | A Stripe secret key is set |

When billing is switched off, `stripe_serviceable` stays true and people can still manage their existing subscriptions.

## Conditional routes

### Served in the mirror premium mode

These routes need `premium_enabled`.

| Method | Route | Operation |
| --- | --- | --- |
| GET | /v1/gifts/{code} | [Get gift](/http-api/gifts/#get-gift) |
| POST | /v1/gifts/{code}/redeem | [Redeem gift](/http-api/gifts/#redeem-gift) |
| GET | /v1/users/@me/gifts | [List current user gifts](/http-api/users/gifts/#list-current-user-gifts) |

### Served while billing is active

These routes need `stripe_enabled`.

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/stripe/checkout/subscription | [Create subscription checkout](/http-api/billing/#create-subscription-checkout) |
| POST | /v1/stripe/checkout/gift | [Create gift checkout](/http-api/billing/#create-gift-checkout) |
| GET | /v1/premium/price-ids | [Get price IDs](/http-api/premium/#get-price-ids) |

### Served while Stripe is serviceable

These routes need `stripe_serviceable`. [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) also needs a Stripe webhook secret.

| Method | Route | Operation |
| --- | --- | --- |
| POST | /v1/stripe/webhook | [Receive Stripe webhook](/http-api/billing/#receive-stripe-webhook) |
| GET | /v1/premium/current-subscription-price | [Get current subscription price](/http-api/premium/#get-current-subscription-price) |
| POST | /v1/premium/customer-portal | [Create customer portal](/http-api/premium/#create-customer-portal) |
| POST | /v1/premium/grace/end | [End premium grace period](/http-api/premium/#end-premium-grace-period) |
| POST | /v1/premium/cancel-subscription | [Cancel subscription](/http-api/premium/#cancel-subscription) |
| POST | /v1/premium/reactivate-subscription | [Reactivate subscription](/http-api/premium/#reactivate-subscription) |
| POST | /v1/premium/change-subscription | [Change subscription billing cycle](/http-api/premium/#change-subscription-billing-cycle) |
| POST | /v1/premium/cancel-pending-subscription-change | [Cancel pending subscription change](/http-api/premium/#cancel-pending-subscription-change) |

## Routes every instance serves

Every instance serves [Get premium state](/http-api/premium/#get-premium-state) and [Set premium perks disabled](/http-api/premium/#set-premium-perks-disabled). In the `everyone` premium mode, every account on a self-hosted instance has premium, and `is_premium` stays true even when `premium_perks_disabled` is true.

Other flags in instance discovery, such as `voice_enabled`, `emails_enabled` and `discovery_enabled`, don't remove routes.
