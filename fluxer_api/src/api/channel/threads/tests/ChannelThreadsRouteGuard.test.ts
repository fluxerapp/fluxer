// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {ChannelThreadsRouteGuard} from '@app/api/channel/threads/ChannelThreadsRouteGuard';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, PreparedQuery} from '@app/api/database/CassandraTypes';
import {BotOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	applyChannelThreadsConfigUpdate,
	type ChannelThreadsConfigUpdateRequest,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {ChannelIdParam, GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {z} from 'zod';

class RecordingExecutor extends InMemoryCassandraQueryExecutor {
	reads: Array<string> = [];

	override async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (query.cql.trimStart().toUpperCase().startsWith('SELECT')) {
			this.reads.push(query.cql);
		}
		return super.executeQuery<T>(query);
	}
}

const SentinelBody = z.object({name: z.string()});

function registerSentinelRoutes(routes: HonoApp): void {
	routes.get(
		'/channels/:channel_id/threads-sentinel',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.DEFAULT),
		LoginRequired,
		Validator('param', ChannelIdParam),
		(ctx) => ctx.json({ok: true}),
	);
	routes.post(
		'/channels/:channel_id/threads-sentinel',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.DEFAULT),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', SentinelBody),
		(ctx) => ctx.json({ok: true}),
	);
	routes.get(
		'/guilds/:guild_id/threads-sentinel',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.DEFAULT),
		LoginRequired,
		Validator('param', GuildIdParam),
		(ctx) => ctx.json({ok: true}),
	);
	routes.get(
		'/channels/:channel_id/threads-sentinel-bot',
		ChannelThreadsRouteGuard({botOnly: true}),
		RateLimitMiddleware(RateLimitConfigs.DEFAULT),
		LoginRequired,
		BotOnly,
		(ctx) => ctx.json({ok: true}),
	);
}

interface Snapshot {
	status: number;
	body: string;
	headers: Array<[string, string]>;
	reads: Array<string>;
}

const CAPABLE = {'X-Fluxer-Features': 'channel_threads'};

describe('ChannelThreadsRouteGuard', () => {
	let harness: ApiTestHarness;
	let executor: RecordingExecutor;
	let owner: TestAccount;
	let guildId: string;
	let channelId: string;

	beforeAll(async () => {
		harness = await createApiTestHarness({registerRoutes: registerSentinelRoutes});
	});

	beforeEach(async () => {
		await harness.reset();
		executor = new RecordingExecutor();
		setCassandraQueryExecutorForTesting(executor);
		owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'threads guard');
		guildId = guild.id;
		channelId = (await createChannel(harness, owner.token, guildId, 'general')).id;
	});

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(null);
		await harness.shutdown();
	});

	async function setConfig(update: ChannelThreadsConfigUpdateRequest): Promise<void> {
		await getInstanceConfigRepository().updateChannelThreadsConfig((current) =>
			applyChannelThreadsConfigUpdate(current, update),
		);
	}

	async function snapshot(
		path: string,
		options: {method?: string; token?: string; headers?: Record<string, string>; body?: string} = {},
	): Promise<Snapshot> {
		const headers: Record<string, string> = {'x-fluxer-test-enable-rate-limits': 'true', ...options.headers};
		if (options.token) headers.Authorization = options.token;
		executor.reads = [];
		const response = await harness.requestJson({path, method: options.method ?? 'GET', headers, body: options.body});
		const reads = executor.reads;
		return {
			status: response.status,
			body: await response.text(),
			headers: [...response.headers].filter(([name]) => name !== 'x-request-id'),
			reads,
		};
	}

	async function expectLikeUnregistered(
		sentinelPath: string,
		unregisteredPath: string,
		options: {method?: string; token?: string; headers?: Record<string, string>; body?: string} = {},
	): Promise<void> {
		await snapshot(unregisteredPath, options);
		const sentinel = await snapshot(sentinelPath, options);
		const unregistered = await snapshot(unregisteredPath, options);
		expect(sentinel.status).toBe(404);
		expect(sentinel).toEqual(unregistered);
		expect(sentinel.headers.some(([name]) => name.startsWith('x-ratelimit'))).toBe(false);
	}

	const sentinel = () => `/channels/${channelId}/threads-sentinel`;
	const unregistered = () => `/channels/${channelId}/threads-unregistered`;

	it('answers like an unregistered path while the experiment is off', async () => {
		await expectLikeUnregistered(sentinel(), unregistered(), {token: owner.token, headers: CAPABLE});
	});

	it('answers like an unregistered path for a user who is not enrolled', async () => {
		await setConfig({enabled: true, enabled_guild_ids: [guildId]});
		await expectLikeUnregistered(sentinel(), unregistered(), {token: owner.token, headers: CAPABLE});
	});

	it('answers like an unregistered path without the client capability', async () => {
		await setConfig({enabled: true, enabled_guild_ids: [guildId], included_user_ids: [owner.userId]});
		await expectLikeUnregistered(sentinel(), unregistered(), {token: owner.token});
	});

	it('answers like an unregistered path for an excluded user', async () => {
		await setConfig({
			enabled: true,
			enabled_guild_ids: [guildId],
			included_user_ids: [owner.userId],
			excluded_user_ids: [owner.userId],
		});
		await expectLikeUnregistered(sentinel(), unregistered(), {token: owner.token, headers: CAPABLE});
	});

	it('answers like an unregistered path for an OAuth2 bearer token', async () => {
		await setConfig({enabled: true, enabled_guild_ids: [guildId], included_user_ids: [owner.userId]});
		const bearer = await createBuilder<{token: string}>(harness, '')
			.post('/test/oauth2/access-token')
			.body({user_id: owner.userId, scopes: ['identify', 'guilds']})
			.execute();
		await expectLikeUnregistered(sentinel(), unregistered(), {token: `Bearer ${bearer.token}`, headers: CAPABLE});
	});

	it('answers like an unregistered path when unauthenticated', async () => {
		await setConfig({enabled: true, enabled_guild_ids: [guildId], user_basis_points: 10000});
		await expectLikeUnregistered(sentinel(), unregistered(), {headers: CAPABLE});
	});

	it('answers like an unregistered path for a malformed snowflake', async () => {
		await expectLikeUnregistered('/channels/not-a-snowflake/threads-sentinel', '/channels/not-a-snowflake/threads-x', {
			token: owner.token,
			headers: CAPABLE,
		});
	});

	it('answers like an unregistered path for a malformed body', async () => {
		await expectLikeUnregistered(sentinel(), unregistered(), {
			method: 'POST',
			token: owner.token,
			headers: CAPABLE,
			body: '{"name":',
		});
	});

	it('answers like an unregistered path for a user token on a bot-only route', async () => {
		await setConfig({enabled: true, enabled_guild_ids: [guildId], included_user_ids: [owner.userId]});
		await expectLikeUnregistered(`/channels/${channelId}/threads-sentinel-bot`, unregistered(), {
			token: owner.token,
			headers: CAPABLE,
		});
	});

	it('answers like an unregistered path when the path guild is not enrolled', async () => {
		await setConfig({enabled: true, included_user_ids: [owner.userId]});
		await expectLikeUnregistered(sentinel(), unregistered(), {token: owner.token, headers: CAPABLE});
		await expectLikeUnregistered(`/guilds/${guildId}/threads-sentinel`, `/guilds/${guildId}/threads-unregistered`, {
			token: owner.token,
			headers: CAPABLE,
		});
	});

	it('lets an enrolled capable user through on an enrolled guild', async () => {
		await setConfig({enabled: true, enabled_guild_ids: [guildId], included_user_ids: [owner.userId]});
		const channel = await snapshot(sentinel(), {token: owner.token, headers: CAPABLE});
		expect(channel.status).toBe(200);
		const guild = await snapshot(`/guilds/${guildId}/threads-sentinel`, {token: owner.token, headers: CAPABLE});
		expect(guild.status).toBe(200);
	});

	it('lets a bot through by the guild gate alone unless it is excluded', async () => {
		const bot = await createTestBotAccount(harness);
		await setConfig({enabled: true, enabled_guild_ids: [guildId]});
		const allowed = await snapshot(`/channels/${channelId}/threads-sentinel-bot`, {token: `Bot ${bot.botToken}`});
		expect(allowed.status).toBe(200);
		expect(allowed.headers.some(([name]) => name.startsWith('x-ratelimit'))).toBe(true);
		await setConfig({excluded_user_ids: [bot.botUserId]});
		await expectLikeUnregistered(`/channels/${channelId}/threads-sentinel-bot`, unregistered(), {
			token: `Bot ${bot.botToken}`,
		});
	});
});
