// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserNotificationSettings} from '@fluxer/constants/src/UserConstants';
import {RpcRequest} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {blockUser} from '../../user/tests/RelationshipTestUtils';

interface RpcUserGuildSettingsResponse {
	type: 'get_user_guild_settings';
	data: {
		user_guild_settings: Array<{message_notifications: number} | null>;
	};
}

interface RpcUserBlockedIdsResponse {
	type: 'get_user_blocked_ids';
	data: Record<string, Array<string>>;
}

const UNKNOWN_USER_ID = '1';
const BATCH_SIZE = 96;

async function setDmNotificationLevel(harness: ApiTestHarness, token: string, level: number): Promise<void> {
	await createBuilder(harness, token)
		.patch('/users/@me/guilds/@me/settings')
		.body({message_notifications: level})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('RpcService user batch fanout', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('keeps get_user_guild_settings index aligned for batches larger than the concurrency cap', async () => {
		const allMessages = await createTestAccount(harness);
		const onlyMentions = await createTestAccount(harness);
		const noMessages = await createTestAccount(harness);
		await setDmNotificationLevel(harness, allMessages.token, UserNotificationSettings.ALL_MESSAGES);
		await setDmNotificationLevel(harness, onlyMentions.token, UserNotificationSettings.ONLY_MENTIONS);
		await setDmNotificationLevel(harness, noMessages.token, UserNotificationSettings.NO_MESSAGES);
		const cycle = [
			{userId: allMessages.userId, expected: UserNotificationSettings.ALL_MESSAGES},
			{userId: onlyMentions.userId, expected: UserNotificationSettings.ONLY_MENTIONS},
			{userId: UNKNOWN_USER_ID, expected: null},
			{userId: noMessages.userId, expected: UserNotificationSettings.NO_MESSAGES},
		];
		const requested = Array.from({length: BATCH_SIZE}, (_value, index) => cycle[index % cycle.length]!);
		const rpcResponse = await createBuilder<RpcUserGuildSettingsResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'get_user_guild_settings',
				user_ids: requested.map((entry) => entry.userId),
				guild_id: '0',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(rpcResponse.data.user_guild_settings).toHaveLength(BATCH_SIZE);
		expect(
			rpcResponse.data.user_guild_settings.map((settings) => settings?.message_notifications ?? null),
		).toStrictEqual(requested.map((entry) => entry.expected));
	});

	test('rejects user batch requests above the schema bound', () => {
		for (const type of ['get_user_guild_settings', 'get_user_blocked_ids']) {
			const withinBound = RpcRequest.safeParse({
				type,
				user_ids: Array.from({length: 1000}, () => UNKNOWN_USER_ID),
				guild_id: '0',
			});
			expect(withinBound.success).toBe(true);
			const aboveBound = RpcRequest.safeParse({
				type,
				user_ids: Array.from({length: 1001}, () => UNKNOWN_USER_ID),
				guild_id: '0',
			});
			expect(aboveBound.success).toBe(false);
		}
	});

	test('returns blocked ids per user for get_user_blocked_ids', async () => {
		const blocker = await createTestAccount(harness);
		const blockedFirst = await createTestAccount(harness);
		const blockedSecond = await createTestAccount(harness);
		const bystander = await createTestAccount(harness);
		await blockUser(harness, blocker.token, blockedFirst.userId);
		await blockUser(harness, blocker.token, blockedSecond.userId);
		const rpcResponse = await createBuilder<RpcUserBlockedIdsResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'get_user_blocked_ids',
				user_ids: [blocker.userId, bystander.userId],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(rpcResponse.data[blocker.userId]?.slice().sort()).toStrictEqual(
			[blockedFirst.userId, blockedSecond.userId].sort(),
		);
		expect(rpcResponse.data[bystander.userId]).toStrictEqual([]);
	});
});
