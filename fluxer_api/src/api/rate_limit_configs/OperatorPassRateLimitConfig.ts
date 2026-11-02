// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const OperatorPassRateLimitConfigs = {
	OPERATOR_PASS_STATE: {
		bucket: 'operator_pass:state',
		config: {limit: 60, windowMs: ms('1 minute')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OPERATOR_PASS_CHECKOUT: {
		bucket: 'operator_pass:checkout',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_PURCHASES: {
		bucket: 'operator_pass:purchases',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_CERTIFICATE_LINK: {
		bucket: 'operator_pass:certificate_link',
		config: {limit: 60, windowMs: ms('1 minute')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OPERATOR_PASS_CERTIFICATE_LINK_RESET: {
		bucket: 'operator_pass:certificate_link_reset',
		config: {limit: 5, windowMs: ms('10 minutes')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_LISTING: {
		bucket: 'operator_pass:listing',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WALL: {
		bucket: 'operator_pass:wall',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_PASSES: {
		bucket: 'operator_pass:passes',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_LABS_JOIN: {
		bucket: 'operator_pass:labs_join',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_OFFER: {
		bucket: 'operator_pass:offer',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WITHDRAW: {
		bucket: 'operator_pass:withdraw',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_CHALLENGE: {
		bucket: 'operator_pass:waitlist_challenge',
		config: {limit: 30, windowMs: ms('10 minutes')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST: {
		bucket: 'operator_pass:waitlist',
		config: {limit: 5, windowMs: ms('10 minutes')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_ADDRESS: {
		bucket: 'operator_pass:waitlist:email',
		config: {limit: 3, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_SENDS: {
		bucket: 'operator_pass:waitlist:sends',
		config: {limit: 2000, windowMs: ms('10 minutes')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_CONFIRM: {
		bucket: 'operator_pass:waitlist_confirm',
		config: {limit: 10, windowMs: ms('1 minute')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_FOLLOWUP: {
		bucket: 'operator_pass:waitlist_followup',
		config: {limit: 20, windowMs: ms('10 minutes')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_FOLLOWUP_ENTRY: {
		bucket: 'operator_pass:waitlist_followup:entry',
		config: {limit: 10, windowMs: ms('30 minutes')},
	} as RouteRateLimitConfig,
	OPERATOR_PASS_WAITLIST_UNSUBSCRIBE: {
		bucket: 'operator_pass:waitlist_unsubscribe',
		config: {limit: 600, windowMs: ms('1 minute')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
} as const;
