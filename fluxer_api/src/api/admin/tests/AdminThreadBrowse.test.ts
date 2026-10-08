// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BrowseResponse {
	messages: Array<{id: string; channel_id: string}>;
	message_responses?: Array<MessageResponse>;
}

describe('admin thread browse', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	it('returns thread messages and thread artifacts unmasked', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'browse');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const source = await sendMessage(harness, owner.token, channel.id, 'source');
		await threadsRequest(harness, owner.token)
			.post(`/channels/${channel.id}/messages/${source.id}/threads`)
			.body({name: 'from source'})
			.expect(201)
			.execute();
		const standalone = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'standalone', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		const inThread = await threadsRequest<MessageResponse>(harness, owner.token)
			.post(`/channels/${standalone.id}/messages`)
			.body({content: 'inside'})
			.expect(200)
			.execute();
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.MESSAGE_LOOKUP,
		]);
		const parent = await createBuilder<BrowseResponse>(harness, admin.token)
			.get(`/admin/channels/${channel.id}/messages?limit=50`)
			.expect(200)
			.execute();
		const responses = parent.message_responses ?? [];
		const sourceResponse = responses.find((message) => message.id === source.id);
		expect((sourceResponse?.flags ?? 0) & ServerMessageFlags.HAS_THREAD).toBe(ServerMessageFlags.HAS_THREAD);
		expect(responses.some((message) => message.type === MessageTypes.THREAD_CREATED)).toBe(true);
		const thread = await createBuilder<BrowseResponse>(harness, admin.token)
			.get(`/admin/channels/${standalone.id}/messages?limit=50`)
			.expect(200)
			.execute();
		expect(thread.messages.map((message) => message.id)).toContain(inThread.id);
	});
});
