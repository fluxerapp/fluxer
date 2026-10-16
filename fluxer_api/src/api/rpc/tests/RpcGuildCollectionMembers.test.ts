// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {createUsersServiceClient} from '@app/api/infrastructure/UsersServiceClient';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface RpcGuildCollectionMembersResponse {
	type: 'guild_collection';
	data: {
		collection: 'members';
		members: Array<{
			user: {
				id: string;
			};
		}>;
		has_more: boolean;
		next_after_user_id: string | null;
	};
}

async function fetchMemberPage(
	harness: ApiTestHarness,
	guildId: string,
	limit: number,
	afterUserId?: string,
): Promise<RpcGuildCollectionMembersResponse['data']> {
	const response = await createBuilder<RpcGuildCollectionMembersResponse>(harness, '')
		.post('/test/rpc-session-init')
		.body({
			type: 'guild_collection',
			guild_id: guildId,
			collection: 'members',
			limit,
			...(afterUserId ? {after_user_id: afterUserId} : {}),
		})
		.expect(HTTP_STATUS.OK)
		.execute();
	return response.data;
}

describe('RpcService guild member collection pagination', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('walks every member across cursor pages', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const expectedIds = [owner.userId, ...members.map((member) => member.userId)];
		const seenIds: Array<string> = [];
		let afterUserId: string | undefined;
		for (let page = 0; page <= expectedIds.length; page++) {
			const data = await fetchMemberPage(harness, guild.id, 1, afterUserId);
			expect(data.collection).toBe('members');
			seenIds.push(...data.members.map((member) => member.user.id));
			if (!data.has_more) {
				expect(data.next_after_user_id).toBeNull();
				break;
			}
			expect(data.next_after_user_id).toBe(seenIds[seenIds.length - 1]);
			afterUserId = data.next_after_user_id ?? undefined;
		}
		expect(seenIds.slice().sort()).toStrictEqual(expectedIds.slice().sort());
	});

	test('looks up every member user on a page in one users service request', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 3);
		const memberIds = [owner.userId, ...members.map((member) => member.userId)].sort();
		const lookups = vi.spyOn(createUsersServiceClient(), 'getUserPartialResponses');
		try {
			const data = await fetchMemberPage(harness, guild.id, 1000);
			expect(data.members.map((member) => member.user.id).sort()).toStrictEqual(memberIds);
			const memberLookups = lookups.mock.calls
				.map(([userIds]) => userIds.map(String).sort())
				.filter((userIds) => userIds.some((userId) => memberIds.includes(userId)));
			expect(memberLookups).toStrictEqual([memberIds]);
		} finally {
			lookups.mockRestore();
		}
	});

	test('rejects an unknown guild on the first member page', async () => {
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'guild_collection',
				guild_id: TEST_IDS.NONEXISTENT_GUILD,
				collection: 'members',
			})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('accepts the member page size the gateway requests', () => {
		const atCeiling = RpcRequest.safeParse({
			type: 'guild_collection',
			guild_id: TEST_IDS.NONEXISTENT_GUILD,
			collection: 'members',
			limit: 1000,
		});
		expect(atCeiling.success).toBe(true);
		const aboveCeiling = RpcRequest.safeParse({
			type: 'guild_collection',
			guild_id: TEST_IDS.NONEXISTENT_GUILD,
			collection: 'members',
			limit: 1001,
		});
		expect(aboveCeiling.success).toBe(false);
	});
});
