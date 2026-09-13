// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
	getChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {IncomingCallFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Voice Call Eligibility', () => {
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
	async function setupUsersWithMutualGuild() {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		await ensureSessionStarted(harness, user1.token);
		await ensureSessionStarted(harness, user2.token);
		const guild = await createGuild(harness, user1.token, 'Mutual Guild');
		const invite = await createChannelInvite(harness, user1.token, guild.system_channel_id!);
		await acceptInvite(harness, user2.token, invite.code);
		return {user1, user2, guild};
	}
	describe('DM call eligibility', () => {
		it('returns ringable true for DM between friends', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await createFriendship(harness, user1, user2);
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			const callData = await createBuilder<{
				ringable: boolean;
				silent?: boolean;
			}>(harness, user1.token)
				.get(`/channels/${dmChannel.id}/call`)
				.execute();
			expect(callData.ringable).toBe(true);
		});
		it('returns ringable true for DM with mutual guild membership', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();
			await updateUserSettings(harness, user2.token, {
				incoming_call_flags: IncomingCallFlags.GUILD_MEMBERS,
			});
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			const callData = await createBuilder<{
				ringable: boolean;
			}>(harness, user1.token)
				.get(`/channels/${dmChannel.id}/call`)
				.execute();
			expect(callData.ringable).toBe(true);
		});
		it('returns ringable false for unclaimed account trying DM call', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			await unclaimAccount(harness, user1.userId);
			const callData = await createBuilder<{
				ringable: boolean;
			}>(harness, user1.token)
				.get(`/channels/${dmChannel.id}/call`)
				.execute();
			expect(callData.ringable).toBe(false);
		});
	});
	describe('Channel type validation', () => {
		it('returns 404 for non-existent channel', async () => {
			const user = await createTestAccount(harness);
			await createBuilder(harness, user.token)
				.get('/channels/999999999999999999/call')
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});
		it('returns error for text channel call eligibility check', async () => {
			const user = await createTestAccount(harness);
			await ensureSessionStarted(harness, user.token);
			const guild = await createGuild(harness, user.token, 'Test Guild');
			const textChannel = await getChannel(harness, user.token, guild.system_channel_id!);
			await createBuilder(harness, user.token)
				.get(`/channels/${textChannel.id}/call`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_CHANNEL_TYPE_FOR_CALL')
				.execute();
		});
	});
});
