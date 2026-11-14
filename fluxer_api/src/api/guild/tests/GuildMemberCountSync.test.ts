// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	getChannel,
	leaveGuild,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function setupGuild(
	harness: ApiTestHarness,
	memberTotal: number,
): Promise<{owner: TestAccount; members: Array<TestAccount>; guildId: string; inviteCode: string}> {
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, 'Member Count Sync');
	const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
	const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
	const members: Array<TestAccount> = [];
	for (let i = 0; i < memberTotal; i++) {
		const member = await createTestAccount(harness);
		await acceptInvite(harness, member.token, invite.code);
		members.push(member);
	}
	return {owner, members, guildId: guild.id, inviteCode: invite.code};
}

async function expectCounterMatchesRows(guildId: string): Promise<void> {
	const repository = new GuildRepository();
	const id = createGuildID(BigInt(guildId));
	const guild = await repository.findUnique(id);
	expect(guild?.memberCount).toBe(await repository.countMembers(id));
}

describe('guild member_count follows guild_members rows', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('a join replaces a drifted counter with the row count', async () => {
		const {guildId, inviteCode} = await setupGuild(harness, 1);
		await setGuildMemberCount(harness, guildId, 40);
		const joiner = await createTestAccount(harness);
		await acceptInvite(harness, joiner.token, inviteCode);
		await expectCounterMatchesRows(guildId);
	});

	test('a leave replaces a drifted counter with the row count', async () => {
		const {members, guildId} = await setupGuild(harness, 2);
		await setGuildMemberCount(harness, guildId, 40);
		await leaveGuild(harness, members[0]!.token, guildId);
		await expectCounterMatchesRows(guildId);
	});

	test('a kick replaces a drifted counter with the row count', async () => {
		const {owner, members, guildId} = await setupGuild(harness, 2);
		await setGuildMemberCount(harness, guildId, 0);
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guildId}/members/${members[0]!.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await expectCounterMatchesRows(guildId);
	});

	test('a ban replaces a drifted counter with the row count', async () => {
		const {owner, members, guildId} = await setupGuild(harness, 2);
		await setGuildMemberCount(harness, guildId, 40);
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guildId}/bans/${members[0]!.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await expectCounterMatchesRows(guildId);
	});

	test('different users joining at once are all counted', async () => {
		const {guildId, inviteCode} = await setupGuild(harness, 0);
		const joiners = await Promise.all(Array.from({length: 5}, () => createTestAccount(harness)));
		await Promise.all(joiners.map((joiner) => acceptInvite(harness, joiner.token, inviteCode)));
		await expectCounterMatchesRows(guildId);
	});

	test('different users leaving at once are all counted', async () => {
		const {members, guildId} = await setupGuild(harness, 5);
		await Promise.all(members.map((member) => leaveGuild(harness, member.token, guildId)));
		await expectCounterMatchesRows(guildId);
	});
});
