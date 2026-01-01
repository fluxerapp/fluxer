/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {RouteRateLimitConfig} from '~/middleware/RateLimitMiddleware';

export const IntegrationRateLimitConfigs = {
	TENOR_SEARCH: {
		bucket: 'tenor:search',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	TENOR_FEATURED: {
		bucket: 'tenor:featured',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	TENOR_TRENDING: {
		bucket: 'tenor:trending',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	TENOR_SUGGEST: {
		bucket: 'tenor:suggest',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	TENOR_REGISTER_SHARE: {
		bucket: 'tenor:register_share',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	STRIPE_VISIONARY_SLOTS: {
		bucket: 'stripe:visionary:slots',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	STRIPE_PRICE_IDS: {
		bucket: 'stripe:price:ids',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	STRIPE_CHECKOUT_SUBSCRIPTION: {
		bucket: 'stripe:checkout:subscription',
		config: {limit: 3, windowMs: 60000},
	} as RouteRateLimitConfig,

	STRIPE_CHECKOUT_GIFT: {
		bucket: 'stripe:checkout:gift',
		config: {limit: 3, windowMs: 60000},
	} as RouteRateLimitConfig,

	STRIPE_CUSTOMER_PORTAL: {
		bucket: 'stripe:customer_portal',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	STRIPE_SUBSCRIPTION_CANCEL: {
		bucket: 'stripe:subscription:cancel',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	STRIPE_SUBSCRIPTION_REACTIVATE: {
		bucket: 'stripe:subscription:reactivate',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	STRIPE_VISIONARY_REJOIN: {
		bucket: 'stripe:visionary:rejoin',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	GIFT_CODE_GET: {
		bucket: 'gift:get',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	GIFT_CODE_REDEEM: {
		bucket: 'gift:redeem',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GIFTS_LIST: {
		bucket: 'gifts:list',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,
} as const;
