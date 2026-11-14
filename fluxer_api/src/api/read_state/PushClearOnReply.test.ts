// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface AckResponse {
	read_states: Array<{
		id: string;
		last_message_id: string | null;
	}>;
}

async function readLastMessageId(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	olderMessageId: string,
): Promise<string | null | undefined> {
	const response = await createBuilder<AckResponse>(harness, token)
		.post('/read-states/ack')
		.body({read_states: [{channel_id: channelId, message_id: olderMessageId}]})
		.expect(HTTP_STATUS.OK)
		.execute();
	return response.read_states[0]?.last_message_id;
}

async function setupDm(harness: ApiTestHarness): Promise<{alice: TestAccount; bob: TestAccount; channelId: string}> {
	const alice = await createTestAccount(harness);
	const bob = await createTestAccount(harness);
	await ensureSessionStarted(harness, alice.token);
	await ensureSessionStarted(harness, bob.token);
	await createFriendship(harness, alice, bob);
	const dm = await createDmChannel(harness, alice.token, bob.userId);
	return {alice, bob, channelId: dm.id};
}

describe('read state on reply', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('replying in a DM marks the channel read up to the reply', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		const fromBob = await sendMessage(harness, bob.token, channelId, 'ping from bob');
		const reply = await sendMessage(harness, alice.token, channelId, 'reply from alice');
		expect(await readLastMessageId(harness, alice.token, channelId, fromBob.id)).toBe(reply.id);
	});

	it('the read state RPC reports the reply as the last read message', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		await sendMessage(harness, bob.token, channelId, 'ping from bob');
		const reply = await sendMessage(harness, alice.token, channelId, 'reply from alice');
		const readState = async (userId: string, channel: string) =>
			(
				await createBuilder<{type: 'get_read_state'; data: {last_message_id: string | null}}>(harness, '')
					.post('/test/rpc-session-init')
					.body({type: 'get_read_state', user_id: userId, channel_id: channel})
					.expect(HTTP_STATUS.OK)
					.execute()
			).data.last_message_id;
		expect(await readState(alice.userId, channelId)).toBe(reply.id);
		expect(await readState(alice.userId, '1')).toBeNull();
	});

	it('replying in a guild text channel marks the channel read up to the reply', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await ensureSessionStarted(harness, alice.token);
		await ensureSessionStarted(harness, bob.token);
		const guild = await createGuild(harness, alice.token, 'Push Clear Guild');
		const channel = await createChannel(harness, alice.token, guild.id, 'push-clear');
		const invite = await createChannelInvite(harness, alice.token, channel.id);
		await acceptInvite(harness, bob.token, invite.code);
		const fromBob = await sendMessage(harness, bob.token, channel.id, 'ping from bob');
		const reply = await sendMessage(harness, alice.token, channel.id, 'reply from alice');
		expect(await readLastMessageId(harness, alice.token, channel.id, fromBob.id)).toBe(reply.id);
	});
});
