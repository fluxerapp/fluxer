// SPDX-License-Identifier: AGPL-3.0-or-later

import {processRecordedGuildDeletions, recordGuildDeletions} from '@app/api/guild/tests/GuildDeletionTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {getGuildMemberSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function indexGuildMembers(harness: ApiTestHarness, guildId: string): Promise<void> {
	await createBuilder(harness, '').post(`/test/guilds/${guildId}/mark-members-indexed`).body({}).execute();
}

async function indexedMemberIds(guildId: string): Promise<Array<string>> {
	const service = getGuildMemberSearchService();
	if (!service) throw new Error('member search is not enabled');
	const result = await service.searchMembers('', {guildId}, {limit: 100});
	return result.hits.map((hit) => hit.userId).sort();
}

describe('Guild deletion member search purge', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('deleting a guild removes its member search docs and keeps other guilds docs', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const other = await createGuild(harness, owner.token, 'Other Guild');
		const otherInvite = await createChannelInvite(harness, owner.token, other.system_channel_id!);
		await acceptInvite(harness, members[0]!.token, otherInvite.code);
		await indexGuildMembers(harness, guild.id);
		await indexGuildMembers(harness, other.id);
		expect(await indexedMemberIds(guild.id)).toEqual([owner.userId, ...members.map((m) => m.userId)].sort());
		expect(await indexedMemberIds(other.id)).toEqual([owner.userId, members[0]!.userId].sort());

		const worker = recordGuildDeletions();
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/delete`)
			.body({password: owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await processRecordedGuildDeletions(worker);

		expect(await indexedMemberIds(guild.id)).toEqual([]);
		expect(await indexedMemberIds(other.id)).toEqual([owner.userId, members[0]!.userId].sort());
	});
});
