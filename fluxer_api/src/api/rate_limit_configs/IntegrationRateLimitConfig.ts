// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const IntegrationRateLimitConfigs = {
	GIF_SEARCH: {
		bucket: 'gif:search',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	GIF_FEATURED: {
		bucket: 'gif:featured',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	GIF_TRENDING: {
		bucket: 'gif:trending',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	GIF_SUGGEST: {
		bucket: 'gif:suggest',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	GIF_REGISTER_SHARE: {
		bucket: 'gif:register_share',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STRIPE_VISIONARY_SLOTS: {
		bucket: 'stripe:visionary:slots',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STRIPE_PRICE_IDS: {
		bucket: 'stripe:price:ids',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STRIPE_CHECKOUT_SUBSCRIPTION: {
		bucket: 'stripe:checkout:subscription',
		config: {limit: 3, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_CHECKOUT_SUBSCRIPTION_PREAPPROVAL: {
		bucket: 'stripe:checkout:subscription:preapproval',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_CHECKOUT_SUBSCRIPTION_PREAPPROVAL_CONTINUE: {
		bucket: 'stripe:checkout:subscription:preapproval:continue',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_CHECKOUT_GIFT: {
		bucket: 'stripe:checkout:gift',
		config: {limit: 3, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_CURRENT_SUBSCRIPTION_PRICE: {
		bucket: 'stripe:subscription:current_price',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STRIPE_PREMIUM_STATE: {
		bucket: 'stripe:premium:state',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STRIPE_PREMIUM_PERKS_DISABLED: {
		bucket: 'stripe:premium:perks_disabled',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_CUSTOMER_PORTAL: {
		bucket: 'stripe:customer_portal',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_SUBSCRIPTION_CANCEL: {
		bucket: 'stripe:subscription:cancel',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_PREMIUM_GRACE_END: {
		bucket: 'stripe:premium:grace:end',
		config: {limit: 3, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_REFUND_ELIGIBILITY: {
		bucket: 'stripe:refund:eligibility',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_REFUND_LATEST: {
		bucket: 'stripe:refund:latest',
		config: {limit: 3, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_SUBSCRIPTION_REACTIVATE: {
		bucket: 'stripe:subscription:reactivate',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_SUBSCRIPTION_CHANGE: {
		bucket: 'stripe:subscription:change',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STRIPE_VISIONARY_REJOIN: {
		bucket: 'stripe:visionary:rejoin',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	AGE_VERIFICATION: {
		bucket: 'age_verification',
		config: {limit: 3, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	GIFT_CODE_GET: {
		bucket: 'gift:get',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	GIFT_CODE_REDEEM: {
		bucket: 'gift:redeem',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	GIFTS_LIST: {
		bucket: 'gifts:list',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STRIPE_WEBHOOK: {
		bucket: 'stripe:webhook',
		config: {limit: 300, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
} as const;
