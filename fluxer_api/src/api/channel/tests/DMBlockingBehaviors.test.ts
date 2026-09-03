// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	acceptInvite,
	blockUser,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
	getChannel,
	initiateCall,
	pinMessage,
	sendChannelMessage,
} from './ChannelTestUtils';

describe('DM Blocking Behaviors', () => {
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
	describe('DM Creation Blocking', () => {
		it('allows DM creation when the other user has blocked you', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const guild = await createGuild(harness, user1.token, 'Test Community');
			const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
			await acceptInvite(harness, user2.token, invite.code);
			await blockUser(harness, user1, user2.userId);
			await createBuilder(harness, user2.token)
				.post('/users/@me/channels')
				.body({recipient_id: user1.userId})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		it('allows DM creation with someone you have blocked', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const guild = await createGuild(harness, user1.token, 'Test Community');
			const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
			await acceptInvite(harness, user2.token, invite.code);
			await blockUser(harness, user1, user2.userId);
			await createBuilder(harness, user1.token)
				.post('/users/@me/channels')
				.body({recipient_id: user2.userId})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
	describe('Voice Call Blocking', () => {
		it('prevents the user who blocked someone from initiating calls in the DM', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await blockUser(harness, user1, user2.userId);
			const {response, json} = await initiateCall(harness, user1.token, channel.id, [user2.userId], 400);
			expect(response.status).toBe(400);
			expect(
				(
					json as {
						code: string;
					}
				).code,
			).toBe('CANNOT_SEND_MESSAGES_TO_USER');
		});
		it('prevents calls in a DM after one user blocks the other', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await blockUser(harness, user1, user2.userId);
			const {response, json} = await initiateCall(harness, user2.token, channel.id, [user1.userId], 400);
			expect(response.status).toBe(400);
			expect(
				(
					json as {
						code: string;
					}
				).code,
			).toBe('CANNOT_SEND_MESSAGES_TO_USER');
		});
	});
	describe('Pin Operation Blocking', () => {
		it('prevents the user who blocked someone from pinning messages in the DM', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			const msg = await sendChannelMessage(harness, user2.token, channel.id, 'message to pin');
			await blockUser(harness, user1, user2.userId);
			const {response, json} = await pinMessage(harness, user1.token, channel.id, msg.id, 400);
			expect(response.status).toBe(400);
			expect(
				(
					json as {
						code: string;
					}
				).code,
			).toBe('CANNOT_SEND_MESSAGES_TO_USER');
		});
		it('prevents pinning messages in a DM after one user blocks the other', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			const msg = await sendChannelMessage(harness, user1.token, channel.id, 'message to pin');
			await blockUser(harness, user1, user2.userId);
			const {response, json} = await pinMessage(harness, user2.token, channel.id, msg.id, 400);
			expect(response.status).toBe(400);
			expect(
				(
					json as {
						code: string;
					}
				).code,
			).toBe('CANNOT_SEND_MESSAGES_TO_USER');
		});
	});
	describe('Message Blocking', () => {
		it('prevents messages in a DM after one user blocks the other', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await blockUser(harness, user1, user2.userId);
			await createBuilder(harness, user2.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'hello'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
		});
	});
});
