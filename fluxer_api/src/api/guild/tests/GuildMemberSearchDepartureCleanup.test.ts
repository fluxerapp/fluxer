// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {getGuildMemberSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function markMembersIndexed(harness: ApiTestHarness, guildId: string): Promise<void> {
	await createBuilder(harness, '').post(`/test/guilds/${guildId}/mark-members-indexed`).body({}).execute();
}

async function indexedMemberIds(guildId: string): Promise<Array<string>> {
	const service = getGuildMemberSearchService();
	if (!service) throw new Error('member search is not enabled');
	const result = await service.searchMembers('', {guildId}, {limit: 100});
	return result.hits.map((hit) => hit.userId).sort();
}

describe('Guild member search cleanup when a member is removed outside the guild routes', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('account deletion removes the member search docs', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		await markMembersIndexed(harness, guild.id);
		const [leaving, staying] = members as [(typeof members)[0], (typeof members)[0]];
		expect(await indexedMemberIds(guild.id)).toEqual([owner.userId, leaving.userId, staying.userId].sort());

		await deleteAccount(harness, leaving.token, leaving.password);
		await setPendingDeletionAt(harness, leaving.userId, new Date(Date.now() - 1000));
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, leaving.userId);

		expect(await indexedMemberIds(guild.id)).toEqual([owner.userId, staying.userId].sort());
	});

	test('kicking a temporary member removes the member search doc', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [temporary, staying] = members as [(typeof members)[0], (typeof members)[0]];
		const guildRepository = new GuildRepository();
		const member = await guildRepository.getMember(
			createGuildID(BigInt(guild.id)),
			createUserID(BigInt(temporary.userId)),
		);
		if (!member) throw new Error('expected the member row');
		await guildRepository.upsertMember({...member.toRow(), temporary: true}, member.toRow());
		await markMembersIndexed(harness, guild.id);
		expect(await indexedMemberIds(guild.id)).toEqual([owner.userId, temporary.userId, staying.userId].sort());

		const response = await createBuilder<{type: string; data: {success: boolean}}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'kick_temporary_member', user_id: temporary.userId, guild_ids: [guild.id]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.data.success).toBe(true);

		expect(await indexedMemberIds(guild.id)).toEqual([owner.userId, staying.userId].sort());
	});
});
