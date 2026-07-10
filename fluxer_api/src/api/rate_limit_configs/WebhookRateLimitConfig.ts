// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import type {RouteRateLimitConfig} from '../middleware/RateLimitMiddleware';

export const WebhookRateLimitConfigs = {
	WEBHOOK_LIST_GUILD: {
		bucket: 'webhook:list::guild_id',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	WEBHOOK_LIST_CHANNEL: {
		bucket: 'webhook:list::channel_id',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	WEBHOOK_CREATE: {
		bucket: 'webhook:create::channel_id',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	WEBHOOK_GET: {
		bucket: 'webhook:read::webhook_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	WEBHOOK_UPDATE: {
		bucket: 'webhook:update::webhook_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	WEBHOOK_DELETE: {
		bucket: 'webhook:delete::webhook_id',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	WEBHOOK_EXECUTE: {
		bucket: 'webhook:execute::webhook_id',
		config: {limit: 60, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	WEBHOOK_MESSAGE_GET: {
		bucket: 'webhook:message_get::webhook_id',
		config: {limit: 60, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	WEBHOOK_MESSAGE_EDIT: {
		bucket: 'webhook:message_edit::webhook_id',
		config: {limit: 30, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	WEBHOOK_MESSAGE_DELETE: {
		bucket: 'webhook:message_delete::webhook_id',
		config: {limit: 30, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	WEBHOOK_GITHUB: {
		bucket: 'webhook:github::webhook_id',
		config: {limit: 200, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	WEBHOOK_INSTATUS: {
		bucket: 'webhook:instatus::webhook_id',
		config: {limit: 200, windowMs: ms('1 minute'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
} as const;
