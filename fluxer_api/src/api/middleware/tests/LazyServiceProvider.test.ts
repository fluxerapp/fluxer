// SPDX-License-Identifier: AGPL-3.0-or-later

import {Hono} from 'hono';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createApiTestHarness} from '../../test/ApiTestHarness';
import type {HonoEnv} from '../../types/HonoEnv';
import {installLazyServices, type LazyServiceProvider} from '../LazyServiceProvider';
import {ServiceMiddleware} from '../ServiceMiddleware';

const NO_CONTENT = 204;

const REQUEST_SERVICE_VARIABLES: ReadonlyArray<keyof HonoEnv['Variables']> = [
	'adminApiKeyService',
	'adminArchiveService',
	'adminService',
	'applicationRepository',
	'applicationService',
	'authRequestService',
	'blueskyOAuthService',
	'botAuthService',
	'cacheService',
	'channelRepository',
	'channelRequestService',
	'channelService',
	'connectionRequestService',
	'connectionService',
	'contactChangeLogService',
	'desktopHandoffService',
	'discoveryService',
	'downloadService',
	'emailChangeService',
	'emailService',
	'embedService',
	'entityAssetService',
	'entranceSoundPlayService',
	'entranceSoundService',
	'errorI18nService',
	'favoriteMemeRequestService',
	'favoriteMemeService',
	'gatewayRequestService',
	'gatewayService',
	'gifService',
	'guildService',
	'instanceConfigRepository',
	'inviteRequestService',
	'inviteService',
	'kvActivityTracker',
	'limitConfigService',
	'mediaService',
	'messageRequestService',
	'ncmecSubmissionService',
	'oauth2ApplicationsRequestService',
	'oauth2RequestService',
	'oauth2Service',
	'oauth2TokenRepository',
	'passwordChangeService',
	'rateLimitService',
	'readStateRequestService',
	'readStateService',
	'reportRequestService',
	'reportService',
	'rpcService',
	'searchService',
	'singleCommunityService',
	'snowflakeService',
	'ssoService',
	'storageService',
	'streamPreviewService',
	'streamService',
	'stripeService',
	'sweegoWebhookService',
	'themeService',
	'userAccountRequestService',
	'userActivityBuffer',
	'userAuthRequestService',
	'userCacheService',
	'userChannelRequestService',
	'userContentRequestService',
	'userRelationshipRequestService',
	'userRepository',
	'userService',
	'webhookRequestService',
	'webhookService',
	'workerService',
];

describe('installLazyServices', () => {
	it('defers construction until the variable is read and memoises the result', async () => {
		let builds = 0;
		const provider: LazyServiceProvider = {
			get requestLocale() {
				builds += 1;
				return 'en-US';
			},
		};
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			installLazyServices(ctx, provider);
			expect(builds).toBe(0);
			await next();
		});
		app.get('/probe', (ctx) => {
			expect(ctx.get('requestLocale')).toBe('en-US');
			expect(ctx.get('requestLocale')).toBe('en-US');
			return ctx.body(null, NO_CONTENT);
		});

		const response = await app.request('/probe');

		expect(response.status).toBe(NO_CONTENT);
		expect(builds).toBe(1);
	});

	it('prefers a value written with set over the lazy provider', async () => {
		let builds = 0;
		const provider: LazyServiceProvider = {
			get requestLocale() {
				builds += 1;
				return 'en-US';
			},
		};
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			installLazyServices(ctx, provider);
			ctx.set('requestLocale', 'fr');
			await next();
		});
		app.get('/probe', (ctx) => {
			expect(ctx.get('requestLocale')).toBe('fr');
			return ctx.body(null, NO_CONTENT);
		});

		const response = await app.request('/probe');

		expect(response.status).toBe(NO_CONTENT);
		expect(builds).toBe(0);
	});

	it('leaves variables the provider does not cover untouched', async () => {
		const app = new Hono<HonoEnv>();
		app.use(async (ctx, next) => {
			installLazyServices(ctx, {});
			await next();
		});
		app.get('/probe', (ctx) => {
			expect(ctx.get('requestLocale')).toBeUndefined();
			ctx.set('requestLocale', 'de');
			expect(ctx.get('requestLocale')).toBe('de');
			return ctx.body(null, NO_CONTENT);
		});

		const response = await app.request('/probe');

		expect(response.status).toBe(NO_CONTENT);
	});
});

describe('ServiceMiddleware lazy request services', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('only materialises the services a request actually reads', async () => {
		const app = new Hono<HonoEnv>();
		app.use(ServiceMiddleware);
		app.get('/probe', (ctx) => {
			ctx.get('userRepository');
			return ctx.json({resolved: Object.keys(ctx.var)});
		});

		const response = await app.request('/probe');
		const body = (await response.json()) as {resolved: Array<string>};

		expect(body.resolved).toContain('userRepository');
		expect(body.resolved).not.toContain('guildService');
		expect(body.resolved).not.toContain('channelService');
		expect(body.resolved).not.toContain('adminService');
		expect(body.resolved).not.toContain('ssoService');
	});

	it('resolves every service variable the middleware is responsible for', async () => {
		const missing: Array<string> = [];
		const app = new Hono<HonoEnv>();
		app.use(ServiceMiddleware);
		app.get('/probe', (ctx) => {
			for (const key of REQUEST_SERVICE_VARIABLES) {
				if (ctx.get(key) === undefined) {
					missing.push(key);
				}
			}
			return ctx.body(null, NO_CONTENT);
		});

		const response = await app.request('/probe');

		expect(response.status).toBe(NO_CONTENT);
		expect(missing).toEqual([]);
	});

	it('shares stateless services across requests and rebuilds request-scoped ones', async () => {
		const reads: Array<Record<string, unknown>> = [];
		const app = new Hono<HonoEnv>();
		app.use(ServiceMiddleware);
		app.get('/probe', (ctx) => {
			reads.push({
				guildService: ctx.get('guildService'),
				guildServiceAgain: ctx.get('guildService'),
				channelService: ctx.get('channelService'),
				userCacheService: ctx.get('userCacheService'),
				userRepository: ctx.get('userRepository'),
				readStateRequestService: ctx.get('readStateRequestService'),
			});
			return ctx.body(null, NO_CONTENT);
		});

		await app.request('/probe');
		await app.request('/probe');

		const [first, second] = reads;
		expect(first.guildService).toBe(first.guildServiceAgain);
		expect(first.guildService).not.toBe(second.guildService);
		expect(first.channelService).not.toBe(second.channelService);
		expect(first.userCacheService).toBe(second.userCacheService);
		expect(first.userRepository).toBe(second.userRepository);
		expect(first.readStateRequestService).toBe(second.readStateRequestService);
	});
});
