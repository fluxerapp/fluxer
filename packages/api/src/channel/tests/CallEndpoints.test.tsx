/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	createGuild,
	type MinimalChannelResponse,
} from '@fluxer/api/src/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import type {GuildInviteMetadataResponse} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	errors?: Array<{code?: string}>;
}

interface PrivateChannelsResponse extends Array<MinimalChannelResponse> {}

describe('Call Endpoints', () => {
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

		const guild = await createGuild(harness, user1.token, 'Test Guild');

		const invite = await createBuilder<GuildInviteMetadataResponse>(harness, user1.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({})
			.execute();

		await createBuilder(harness, user2.token).post(`/invites/${invite.code}`).body(null).execute();

		return {user1, user2};
	}

	async function setupGroupDmUsers() {
		const owner = await createTestAccount(harness);
		const memberOne = await createTestAccount(harness);
		const memberTwo = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);

		await createFriendship(harness, owner, memberOne);
		await createFriendship(harness, owner, memberTwo);

		const groupDm = await createGroupDmChannel(harness, owner.token, [memberOne.userId, memberTwo.userId]);

		return {owner, memberOne, memberTwo, outsider, groupDm};
	}

	describe('GET /channels/:channel_id/call', () => {
		it('returns call eligibility for DM channel when voice is disabled', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			const callData = await createBuilder<{ringable: boolean}>(harness, user1.token)
				.get(`/channels/${dmChannel.id}/call`)
				.execute();

			expect(callData).toHaveProperty('ringable');
			expect(typeof callData.ringable).toBe('boolean');
		});

		it('returns call eligibility for non-existent channel', async () => {
			const user = await createTestAccount(harness);

			await createBuilder(harness, user.token).get('/channels/999999999999999999/call').expect(404).execute();
		});
	});

	describe('PATCH /channels/:channel_id/call', () => {
		it('updates call region for DM channel', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.patch(`/channels/${dmChannel.id}/call`)
				.body({region: 'us-west'})
				.expect(404)
				.execute();
		});

		it('returns error when updating non-existent call', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token).patch(`/channels/${dmChannel.id}/call`).body({}).expect(404).execute();
		});
	});

	describe('POST /channels/:channel_id/call/ring', () => {
		it('rings call recipients for DM channel', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/ring`)
				.body({recipients: [user2.userId]})
				.expect(204)
				.execute();
		});

		it('creates silent call without ringing when recipients is empty array (shift-click)', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/ring`)
				.body({recipients: []})
				.expect(204)
				.execute();
		});

		it('rings call without specifying recipients for DM channel', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/ring`)
				.body({})
				.expect(204)
				.execute();
		});

		it('rejects ringing for non-existent channel', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			await createBuilder(harness, user1.token)
				.post('/channels/123456789/call/ring')
				.body({recipients: [user2.userId]})
				.expect(404)
				.execute();
		});

		it('non-member cannot create or start a group DM call', async () => {
			const {outsider, groupDm} = await setupGroupDmUsers();

			await createBuilder(harness, outsider.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({})
				.expect(404, 'UNKNOWN_CHANNEL')
				.execute();
		});

		it('non-member cannot ring an existing group DM call', async () => {
			const {owner, memberOne, outsider, groupDm} = await setupGroupDmUsers();

			await createBuilder(harness, owner.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId]})
				.expect(204)
				.execute();

			await createBuilder(harness, outsider.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId]})
				.expect(404, 'UNKNOWN_CHANNEL')
				.execute();
		});

		it('rejects group DM ring recipients outside the channel recipient set', async () => {
			const {owner, memberOne, outsider, groupDm} = await setupGroupDmUsers();

			const error = await createBuilder<ErrorResponse>(harness, owner.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId, outsider.userId]})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();

			expect(error.errors?.[0]?.code).toBe('USER_NOT_IN_CHANNEL');

			const outsiderChannels = await createBuilder<PrivateChannelsResponse>(harness, outsider.token)
				.get('/users/@me/channels')
				.execute();

			expect(outsiderChannels.some((channel) => channel.id === groupDm.id)).toBe(false);
		});

		it('group DM member can ring a valid recipient subset', async () => {
			const {memberOne, owner, groupDm} = await setupGroupDmUsers();

			await createBuilder(harness, memberOne.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [owner.userId]})
				.expect(204)
				.execute();
		});

		it('group DM member can ring default recipients when recipients are omitted', async () => {
			const {memberTwo, groupDm} = await setupGroupDmUsers();

			await createBuilder(harness, memberTwo.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({})
				.expect(204)
				.execute();
		});
	});

	describe('POST /channels/:channel_id/call/stop-ringing', () => {
		it('returns error when stopping ringing for non-existent call', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/stop-ringing`)
				.body({recipients: [user2.userId]})
				.expect(404)
				.execute();
		});

		it('returns error when stopping ringing all for non-existent call', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/stop-ringing`)
				.body({})
				.expect(404)
				.execute();
		});
	});

	describe('POST /channels/:channel_id/call/end', () => {
		it('ends call for DM channel', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/end`)
				.body(null)
				.expect(204)
				.execute();
		});
	});
});
