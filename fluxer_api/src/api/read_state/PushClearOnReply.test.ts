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
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface AckResponse {
	read_states: Array<{
		id: string;
		last_message_id: string | null;
	}>;
}

interface ReadSignals {
	clears: Array<{channelId: string; messageId: string}>;
	acks: Array<{channel_id: string; message_id: string}>;
}

async function captureReadSignals(userId: string, action: () => Promise<void>): Promise<ReadSignals> {
	const clearSpy = vi.spyOn(NoopGatewayService.prototype, 'clearPushChannelNotifications');
	const presenceSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
	try {
		await action();
		return {
			clears: clearSpy.mock.calls
				.filter(([params]) => params.userId.toString() === userId)
				.map(([params]) => ({channelId: params.channelId.toString(), messageId: params.messageId.toString()})),
			acks: presenceSpy.mock.calls
				.filter(([params]) => params.userId.toString() === userId && params.event === 'MESSAGE_ACK')
				.map(([params]) => {
					const data = params.data as {channel_id: string; message_id: string};
					return {channel_id: data.channel_id, message_id: data.message_id};
				}),
		};
	} finally {
		clearSpy.mockRestore();
		presenceSpy.mockRestore();
	}
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

async function ackMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	body: {manual?: boolean; mention_count?: number} = {},
): Promise<void> {
	await createBuilder(harness, token)
		.post(`/channels/${channelId}/messages/${messageId}/ack`)
		.body(body)
		.expect(HTTP_STATUS.NO_CONTENT)
		.execute();
}

async function ringCall(harness: ApiTestHarness, token: string, channelId: string): Promise<void> {
	await createBuilder(harness, token)
		.post(`/channels/${channelId}/call/ring`)
		.body({})
		.expect(HTTP_STATUS.NO_CONTENT)
		.execute();
}

async function latestMessageId(harness: ApiTestHarness, token: string, channelId: string): Promise<string> {
	const messages = await createBuilder<Array<{id: string}>>(harness, token)
		.get(`/channels/${channelId}/messages?limit=1`)
		.expect(HTTP_STATUS.OK)
		.execute();
	return messages[0]!.id;
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

describe('push clear on reply', () => {
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

	it('replying in a DM clears the partner message push up to the reply without a MESSAGE_ACK', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		const fromBob = await sendMessage(harness, bob.token, channelId, 'ping from bob');
		let replyId = '';
		const signals = await captureReadSignals(alice.userId, async () => {
			const reply = await sendMessage(harness, alice.token, channelId, 'reply from alice');
			replyId = reply.id;
		});
		expect(BigInt(replyId) > BigInt(fromBob.id)).toBe(true);
		expect(signals.clears).toEqual([{channelId, messageId: replyId}]);
		expect(signals.acks).toEqual([]);
		expect(await readLastMessageId(harness, alice.token, channelId, fromBob.id)).toBe(replyId);
	});

	it('a follow-up message with nothing new unread sends no push clear', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		await sendMessage(harness, bob.token, channelId, 'ping from bob');
		await sendMessage(harness, alice.token, channelId, 'reply from alice');
		const signals = await captureReadSignals(alice.userId, async () => {
			await sendMessage(harness, alice.token, channelId, 'second reply from alice');
		});
		expect(signals.clears).toEqual([]);
		expect(signals.acks).toEqual([]);
	});

	it('the first message in a DM with nothing unread sends no push clear', async () => {
		const {alice, channelId} = await setupDm(harness);
		const signals = await captureReadSignals(alice.userId, async () => {
			await sendMessage(harness, alice.token, channelId, 'hello from alice');
		});
		expect(signals.clears).toEqual([]);
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

	it('an explicit ack in a DM sends exactly one push clear and one MESSAGE_ACK', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		const fromBob = await sendMessage(harness, bob.token, channelId, 'ping from bob');
		const signals = await captureReadSignals(alice.userId, async () => {
			await createBuilder(harness, alice.token)
				.post(`/channels/${channelId}/messages/${fromBob.id}/ack`)
				.body({})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		expect(signals.clears).toEqual([{channelId, messageId: fromBob.id}]);
		expect(signals.acks).toEqual([{channel_id: channelId, message_id: fromBob.id}]);
	});

	it('replying in a guild text channel clears the other member message push up to the reply without a MESSAGE_ACK', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await ensureSessionStarted(harness, alice.token);
		await ensureSessionStarted(harness, bob.token);
		const guild = await createGuild(harness, alice.token, 'Push Clear Guild');
		const channel = await createChannel(harness, alice.token, guild.id, 'push-clear');
		const invite = await createChannelInvite(harness, alice.token, channel.id);
		await acceptInvite(harness, bob.token, invite.code);
		const fromBob = await sendMessage(harness, bob.token, channel.id, 'ping from bob');
		let replyId = '';
		const signals = await captureReadSignals(alice.userId, async () => {
			const reply = await sendMessage(harness, alice.token, channel.id, 'reply from alice');
			replyId = reply.id;
		});
		expect(BigInt(replyId) > BigInt(fromBob.id)).toBe(true);
		expect(signals.clears).toEqual([{channelId: channel.id, messageId: replyId}]);
		expect(signals.acks).toEqual([]);
		expect(await readLastMessageId(harness, alice.token, channel.id, fromBob.id)).toBe(replyId);
	});

	it('starting a call clears the caller push for messages still unread up to the call message', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		const first = await sendMessage(harness, bob.token, channelId, 'first from bob');
		const second = await sendMessage(harness, bob.token, channelId, 'second from bob');
		await ackMessage(harness, alice.token, channelId, first.id);
		const signals = await captureReadSignals(alice.userId, async () => {
			await ringCall(harness, alice.token, channelId);
		});
		const callMessageId = await latestMessageId(harness, alice.token, channelId);
		expect(BigInt(callMessageId) > BigInt(second.id)).toBe(true);
		expect(signals.clears).toEqual([{channelId, messageId: callMessageId}]);
		expect(signals.acks).toEqual([]);
	});

	it('starting a call with nothing unread sends the caller no push clear', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		const fromBob = await sendMessage(harness, bob.token, channelId, 'ping from bob');
		await ackMessage(harness, alice.token, channelId, fromBob.id);
		const signals = await captureReadSignals(alice.userId, async () => {
			await ringCall(harness, alice.token, channelId);
		});
		expect(signals.clears).toEqual([]);
	});

	it('ending a call clears a participant push up to the call message only when it was unread', async () => {
		const {alice, bob, channelId} = await setupDm(harness);
		const fromBob = await sendMessage(harness, bob.token, channelId, 'ping from bob');
		await ringCall(harness, alice.token, channelId);
		const callMessageId = await latestMessageId(harness, alice.token, channelId);
		await ackMessage(harness, bob.token, channelId, fromBob.id, {manual: true, mention_count: 0});
		const endCall = async () => {
			await createBuilder(harness, '')
				.post('/test/rpc-session-init')
				.body({
					type: 'call_ended',
					channel_id: channelId,
					message_id: callMessageId,
					participants: [alice.userId, bob.userId],
					ended_timestamp: Date.now(),
				})
				.expect(HTTP_STATUS.OK)
				.execute();
		};
		const clearSpy = vi.spyOn(NoopGatewayService.prototype, 'clearPushChannelNotifications');
		try {
			await endCall();
			const clears = clearSpy.mock.calls.map(([params]) => ({
				userId: params.userId.toString(),
				channelId: params.channelId.toString(),
				messageId: params.messageId.toString(),
			}));
			expect(clears).toEqual([{userId: bob.userId, channelId, messageId: callMessageId}]);
		} finally {
			clearSpy.mockRestore();
		}
	});

	it('joining a guild with an active system channel sends the joiner no push clear', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await ensureSessionStarted(harness, alice.token);
		await ensureSessionStarted(harness, bob.token);
		const guild = await createGuild(harness, alice.token, 'Push Clear Join Guild');
		const systemChannelId = guild.system_channel_id!;
		await sendMessage(harness, alice.token, systemChannelId, 'welcome');
		const invite = await createChannelInvite(harness, alice.token, systemChannelId);
		const signals = await captureReadSignals(bob.userId, async () => {
			await acceptInvite(harness, bob.token, invite.code);
		});
		expect(signals.clears).toEqual([]);
	});
});
