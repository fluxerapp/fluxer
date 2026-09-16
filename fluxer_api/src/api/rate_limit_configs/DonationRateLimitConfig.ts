// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const DonationRateLimitConfigs = {
	DONATION_REQUEST_LINK: {
		bucket: 'donation:request_link',
		config: {limit: 3, windowMs: ms('1 hour')},
		trustDonorIpHeader: true,
		emailBucket: {
			bucket: 'donation:request_link:email',
			config: {limit: 10, windowMs: ms('1 hour')},
		},
	} as RouteRateLimitConfig,
	DONATION_MANAGE: {
		bucket: 'donation:manage',
		config: {limit: 10, windowMs: ms('1 minute')},
		trustDonorIpHeader: true,
	} as RouteRateLimitConfig,
	DONATION_CHECKOUT: {
		bucket: 'donation:checkout',
		config: {limit: 5, windowMs: ms('1 minute')},
		trustDonorIpHeader: true,
		emailBucket: {
			bucket: 'donation:checkout:email',
			config: {limit: 10, windowMs: ms('1 hour')},
		},
	} as RouteRateLimitConfig,
} as const;
