// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {resetChannelThreadsConfig, setChannelThreadsConfig} from '@app/api/channel/tests/ThreadTestUtils';
import type {PreparedQuery} from '@app/api/database/CassandraTypes';
import {acceptInvite, createChannelInvite, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {ChannelThreadsConfig} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const FEATURES = 'X-Fluxer-Features';
const CAPABLE = 'channel_threads';

describe('threads_active on GET /users/@me/guilds', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function listGuilds(token: string, capable: boolean): Promise<Array<GuildResponse>> {
		const builder = createBuilder<Array<GuildResponse>>(harness, token).get('/users/@me/guilds');
		return (capable ? builder.header(FEATURES, CAPABLE) : builder).execute();
	}

	async function setup(): Promise<{
		owner: TestAccount;
		member: TestAccount;
		enrolled: GuildResponse;
		plain: GuildResponse;
		ownerBaseline: Array<GuildResponse>;
		memberBaseline: Array<GuildResponse>;
	}> {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const enrolled = await createGuild(harness, owner.token, 'Enrolled');
		const plain = await createGuild(harness, owner.token, 'Plain');
		const invite = await createChannelInvite(harness, owner.token, enrolled.system_channel_id!);
		await acceptInvite(harness, member.token, invite.code);
		return {
			owner,
			member,
			enrolled,
			plain,
			ownerBaseline: await listGuilds(owner.token, true),
			memberBaseline: await listGuilds(member.token, true),
		};
	}

	function enrol(enrolled: GuildResponse, userIds: Array<string>): Partial<ChannelThreadsConfig> {
		return {enabled: true, enabled_guild_ids: [enrolled.id], included_user_ids: userIds};
	}

	test('flags only the enrolled guild for an enrolled capable viewer', async () => {
		const {owner, enrolled, plain, ownerBaseline} = await setup();
		await setChannelThreadsConfig(enrol(enrolled, [owner.userId]));
		const guilds = await listGuilds(owner.token, true);
		const byId = new Map(guilds.map((guild) => [guild.id, guild]));
		const baseline = new Map(ownerBaseline.map((guild) => [guild.id, guild]));
		expect(byId.get(enrolled.id)).toEqual({...baseline.get(enrolled.id), threads_active: true});
		expect(byId.get(plain.id)).toEqual(baseline.get(plain.id));
		expect(byId.get(plain.id)).not.toHaveProperty('threads_active');
	});

	test('control users, plain guilds, incapable clients, excluded users and the kill switch get no key', async () => {
		const {owner, member, enrolled, ownerBaseline, memberBaseline} = await setup();
		const expectUnchanged = (guilds: Array<GuildResponse>, baseline: Array<GuildResponse>) => {
			expect(guilds).toEqual(baseline);
			for (const guild of guilds) expect(guild).not.toHaveProperty('threads_active');
		};

		await setChannelThreadsConfig(enrol(enrolled, [owner.userId]));
		expectUnchanged(await listGuilds(member.token, true), memberBaseline);
		expectUnchanged(await listGuilds(owner.token, false), ownerBaseline);

		await setChannelThreadsConfig({enabled: true, enabled_guild_ids: [], included_user_ids: [owner.userId]});
		expectUnchanged(await listGuilds(owner.token, true), ownerBaseline);

		await setChannelThreadsConfig({...enrol(enrolled, [owner.userId]), excluded_user_ids: [owner.userId]});
		expectUnchanged(await listGuilds(owner.token, true), ownerBaseline);

		await setChannelThreadsConfig({...enrol(enrolled, [owner.userId]), enabled: false});
		expectUnchanged(await listGuilds(owner.token, true), ownerBaseline);
	});

	test('reads no thread marker while the experiment has never been enabled', async () => {
		const {owner, ownerBaseline} = await setup();
		const markerReads: Array<string> = [];
		let queries = 0;
		const original = InMemoryCassandraQueryExecutor.prototype.executeQuery;
		vi.spyOn(InMemoryCassandraQueryExecutor.prototype, 'executeQuery').mockImplementation(async function (
			this: InMemoryCassandraQueryExecutor,
			query: PreparedQuery,
		) {
			queries++;
			if (query.cql.includes('guild_thread_state')) markerReads.push(query.cql);
			return original.call(this, query);
		} as typeof original);
		const guilds = await listGuilds(owner.token, true);
		expect(guilds).toEqual(ownerBaseline);
		expect(queries).toBeGreaterThan(0);
		expect(markerReads).toEqual([]);
	});
});
