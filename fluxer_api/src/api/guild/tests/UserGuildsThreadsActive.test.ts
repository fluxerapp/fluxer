// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {acceptInvite, createChannelInvite, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
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
		clearChannelThreadsTaintCacheForTesting();
	});

	afterEach(() => {
		vi.restoreAllMocks();
		clearChannelThreadsTaintCacheForTesting();
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

	test('flags every guild for a capable viewer and hides the key from incapable clients', async () => {
		const {owner, enrolled, plain, ownerBaseline} = await setup();
		const guilds = await listGuilds(owner.token, true);
		const byId = new Map(guilds.map((guild) => [guild.id, guild]));
		const baseline = new Map(ownerBaseline.map((guild) => [guild.id, guild]));
		expect(byId.get(enrolled.id)).toEqual({...baseline.get(enrolled.id), threads_active: true});
		expect(byId.get(plain.id)).toEqual({...baseline.get(plain.id), threads_active: true});
		const incapable = await listGuilds(owner.token, false);
		expect(incapable.map(({threads_active: _active, ...rest}) => rest)).toEqual(
			ownerBaseline.map(({threads_active: _active, ...rest}) => rest),
		);
		for (const entry of incapable) expect(entry).not.toHaveProperty('threads_active');
	});
});
