// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {GuildBanByEmailRow, GuildBanByUserIdRow, GuildBanRow} from '@app/api/database/types/GuildTypes';
import {processRecordedGuildDeletions, recordGuildDeletions} from '@app/api/guild/tests/GuildDeletionTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {GuildBans, GuildBansByEmail, GuildBansByUserId} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function guildBanRows(guildId: string): Promise<Array<GuildBanRow>> {
	return fetchMany<GuildBanRow>(GuildBans.selectCql({where: GuildBans.where.eq('guild_id')}), {
		guild_id: createGuildID(BigInt(guildId)),
	});
}

async function userBanGuildIds(userId: string): Promise<Array<string>> {
	const rows = await fetchMany<GuildBanByUserIdRow>(
		GuildBansByUserId.selectCql({where: GuildBansByUserId.where.eq('user_id')}),
		{user_id: createUserID(BigInt(userId))},
	);
	return rows.map((row) => row.guild_id.toString());
}

async function emailBanRows(guildId: string, email: string): Promise<Array<GuildBanByEmailRow>> {
	return fetchMany<GuildBanByEmailRow>(
		GuildBansByEmail.selectCql({where: [GuildBansByEmail.where.eq('guild_id'), GuildBansByEmail.where.eq('email')]}),
		{guild_id: createGuildID(BigInt(guildId)), email},
	);
}

describe('Guild deletion ban purge', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('deleting a guild removes its bans from every ban table and keeps bans in other guilds', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [permanent, temporary] = members;
		const other = await createGuild(harness, owner.token, 'Other Guild');
		const otherInvite = await createChannelInvite(harness, owner.token, other.system_channel_id!);
		await acceptInvite(harness, permanent.token, otherInvite.code);
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${permanent.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${temporary.userId}`)
			.body({ban_duration_seconds: 86400})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, owner.token)
			.put(`/guilds/${other.id}/bans/${permanent.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const banned = await guildBanRows(guild.id);
		expect(banned.map((row) => row.user_id.toString()).sort()).toEqual([permanent.userId, temporary.userId].sort());
		const emails = banned.map((row) => row.email).filter((email): email is string => email != null);
		expect(emails).toHaveLength(2);
		for (const email of emails) {
			expect(await emailBanRows(guild.id, email)).toHaveLength(1);
		}

		const worker = recordGuildDeletions();
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/delete`)
			.body({password: owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await processRecordedGuildDeletions(worker);

		expect(await guildBanRows(guild.id)).toEqual([]);
		expect(await userBanGuildIds(permanent.userId)).toEqual([other.id]);
		expect(await userBanGuildIds(temporary.userId)).toEqual([]);
		for (const email of emails) {
			expect(await emailBanRows(guild.id, email)).toEqual([]);
		}
		expect((await guildBanRows(other.id)).map((row) => row.user_id.toString())).toEqual([permanent.userId]);
	});
});
