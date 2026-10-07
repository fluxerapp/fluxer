// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, markGuildChannelsAsIndexed} from '@app/api/message/tests/MessageTestUtils';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@fluxer/constants/src/ThreadConstants', async (importOriginal) => ({
	...(await importOriginal<typeof import('@fluxer/constants/src/ThreadConstants')>()),
	SEARCH_INDEX_ENQUEUE_MAX: 1,
}));

describe('guild search thread scope', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup() {
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'search');
		const first = await createChannel(harness, owner.token, guild.id, 'first');
		await createChannel(harness, owner.token, guild.id, 'second');
		return {owner, guildId: guild.id, channelId: first.id};
	}

	function search(account: TestAccount, body: Record<string, unknown>) {
		return threadsRequest<{indexing?: boolean}>(harness, account.token)
			.post('/search/messages')
			.body({content: 'needle', ...body})
			.expect(200)
			.execute();
	}

	function spyIndexJobs() {
		const addJob = vi.spyOn(getWorkerService(), 'addJob');
		return () => addJob.mock.calls.filter(([task]) => task === 'indexChannelMessages');
	}

	it('caps index enqueues per search in an active guild', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const s = await setup();
		const indexJobs = spyIndexJobs();
		expect(await search(s.owner, {context_guild_id: s.guildId})).toEqual({indexing: true});
		expect(indexJobs()).toHaveLength(1);
	});

	it('keeps enqueueing every unindexed channel in a control guild', async () => {
		const s = await setup();
		const indexJobs = spyIndexJobs();
		expect(await search(s.owner, {context_guild_id: s.guildId})).toEqual({indexing: true});
		expect(indexJobs().length).toBeGreaterThan(1);
	});

	it('never reports indexing for threads', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const s = await setup();
		await markGuildChannelsAsIndexed(harness, s.owner.token, s.guildId);
		const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.channelId}/threads`)
			.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		const indexJobs = spyIndexJobs();
		expect(await search(s.owner, {context_guild_id: s.guildId})).not.toHaveProperty('indexing');
		expect(await search(s.owner, {context_guild_id: s.guildId, channel_ids: [thread.id]})).not.toHaveProperty(
			'indexing',
		);
		expect(indexJobs()).toHaveLength(0);
	});
});
