// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	blockUser,
	createDmChannel,
	createFriendship,
	initiateCall,
	pinMessage,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
