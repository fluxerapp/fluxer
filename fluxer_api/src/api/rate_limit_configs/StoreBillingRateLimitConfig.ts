// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const StoreBillingRateLimitConfigs = {
	STORE_BILLING_CONTEXT: {
		bucket: 'store:context',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STORE_BILLING_CLAIM_APP_STORE: {
		bucket: 'store:claim:app_store',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STORE_BILLING_CLAIM_GOOGLE_PLAY: {
		bucket: 'store:claim:google_play',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STORE_BILLING_PURCHASES_LIST: {
		bucket: 'store:purchases:list',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	STORE_BILLING_PURCHASE_RELEASE: {
		bucket: 'store:purchases:release',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	STORE_BILLING_APP_STORE_WEBHOOK: {
		bucket: 'store:webhook:app_store',
		config: {limit: 300, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	STORE_BILLING_GOOGLE_PLAY_WEBHOOK: {
		bucket: 'store:webhook:google_play',
		config: {limit: 300, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
} as const;
