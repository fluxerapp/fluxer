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
	acceptInvite,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	createGuild,
	getChannel,
} from '@fluxer/api/src/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@fluxer/api/src/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	errors?: Array<{code?: string}>;
}

describe('Voice Call Ringing', () => {
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

	describe('Ring call', () => {
		it('rings call recipients in DM channel', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/ring`)
				.body({recipients: [user2.userId]})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});

		it('rings call without specifying recipients', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/ring`)
				.body({})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});

		it('rings call for friends DM', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);

			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);

			await createFriendship(harness, user1, user2);

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/ring`)
				.body({recipients: [user2.userId]})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});

		it('returns 404 for non-existent channel', async () => {
			const user = await createTestAccount(harness);

			await createBuilder(harness, user.token)
				.post('/channels/999999999999999999/call/ring')
				.body({recipients: []})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});

		it('returns error for text channel ring', async () => {
			const user = await createTestAccount(harness);
			await ensureSessionStarted(harness, user.token);

			const guild = await createGuild(harness, user.token, 'Test Guild');
			const textChannel = await getChannel(harness, user.token, guild.system_channel_id!);

			await createBuilder(harness, user.token)
				.post(`/channels/${textChannel.id}/call/ring`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_CHANNEL_TYPE_FOR_CALL')
				.execute();
		});

		it('non-member cannot ring group DM call', async () => {
			const {owner, memberOne, outsider, groupDm} = await setupGroupDmUsers();

			await createBuilder(harness, owner.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId]})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();

			await createBuilder(harness, outsider.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId]})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});

		it('rejects non-recipient ids in group DM ring payload', async () => {
			const {owner, memberOne, outsider, groupDm} = await setupGroupDmUsers();

			const error = await createBuilder<ErrorResponse>(harness, owner.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId, outsider.userId]})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();

			expect(error.errors?.[0]?.code).toBe('USER_NOT_IN_CHANNEL');
		});

		it('group DM members can ring valid subset and default recipients', async () => {
			const {owner, memberOne, memberTwo, groupDm} = await setupGroupDmUsers();

			await createBuilder(harness, memberOne.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [owner.userId]})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();

			await createBuilder(harness, memberTwo.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
	});

	describe('Stop ringing', () => {
		it('returns 404 when stopping ring for non-existent call', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/stop-ringing`)
				.body({recipients: [user2.userId]})
				.expect(HTTP_STATUS.NOT_FOUND, 'NO_ACTIVE_CALL')
				.execute();
		});

		it('returns 404 when stopping ring without recipients for non-existent call', async () => {
			const {user1, user2} = await setupUsersWithMutualGuild();

			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);

			await createBuilder(harness, user1.token)
				.post(`/channels/${dmChannel.id}/call/stop-ringing`)
				.body({})
				.expect(HTTP_STATUS.NOT_FOUND, 'NO_ACTIVE_CALL')
				.execute();
		});
	});
});
