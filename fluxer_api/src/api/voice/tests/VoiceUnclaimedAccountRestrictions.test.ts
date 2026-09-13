// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {IncomingCallFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Voice Unclaimed Account Restrictions', () => {
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
	describe('DM call restrictions', () => {
		it('unclaimed account cannot initiate DM call', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			const guild = await createGuild(harness, user1.token, 'Mutual Guild');
			const invite = await createChannelInvite(harness, user1.token, guild.system_channel_id!);
			await acceptInvite(harness, user2.token, invite.code);
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			await unclaimAccount(harness, user1.userId);
			const callData = await createBuilder<{
				ringable: boolean;
			}>(harness, user1.token)
				.get(`/channels/${dmChannel.id}/call`)
				.execute();
			expect(callData.ringable).toBe(false);
		});
		it('claimed account can initiate DM call with unclaimed recipient', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			const guild = await createGuild(harness, user1.token, 'Mutual Guild');
			const invite = await createChannelInvite(harness, user1.token, guild.system_channel_id!);
			await acceptInvite(harness, user2.token, invite.code);
			await updateUserSettings(harness, user2.token, {
				incoming_call_flags: IncomingCallFlags.GUILD_MEMBERS,
			});
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			await unclaimAccount(harness, user2.userId);
			const callData = await createBuilder<{
				ringable: boolean;
			}>(harness, user1.token)
				.get(`/channels/${dmChannel.id}/call`)
				.execute();
			expect(callData.ringable).toBe(true);
		});
		it('unclaimed account cannot call friend', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await createFriendship(harness, user1, user2);
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
});
