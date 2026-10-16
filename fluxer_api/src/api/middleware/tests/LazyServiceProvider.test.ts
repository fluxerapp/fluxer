import {ServiceMiddleware} from '@app/api/middleware/ServiceMiddleware';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Hono} from 'hono';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

const NO_CONTENT = 204;

describe('ServiceMiddleware lazy request services', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
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
