// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MAX_GUILD_MEMBERS} from '@fluxer/constants/src/LimitConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface InviteResponse {
	code: string;
	uses?: number;
}

async function setupInvite(harness: ApiTestHarness, maxUses: number, joinerCount: number) {
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, 'Max uses guild');
	if (!guild.system_channel_id) {
		throw new Error('Guild system channel is missing');
	}
	const invite = await createBuilder<InviteResponse>(harness, owner.token)
		.post(`/channels/${guild.system_channel_id}/invites`)
		.body({max_uses: maxUses, unique: true})
		.execute();
	const joiners: Array<TestAccount> = [];
	for (let i = 0; i < joinerCount; i++) {
		joiners.push(await createTestAccount(harness));
	}
	return {owner, guild, invite, joiners};
}

async function countMembers(harness: ApiTestHarness, accounts: Array<TestAccount>, guildId: string): Promise<number> {
	let count = 0;
	for (const account of accounts) {
		const guilds = await createBuilder<Array<{id: string}>>(harness, account.token).get('/users/@me/guilds').execute();
		if (guilds.some((guild) => guild.id === guildId)) count++;
	}
	return count;
}

async function findGuildInvite(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	code: string,
): Promise<InviteResponse | null> {
	const invites = await createBuilder<Array<InviteResponse>>(harness, token)
		.get(`/guilds/${guildId}/invites`)
		.execute();
	return invites.find((invite) => invite.code === code) ?? null;
}

describe('Invite max uses', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it.each([
		[1, 8],
		[3, 10],
	])('admits at most max_uses=%i of %i simultaneous joiners', async (maxUses, joinerCount) => {
		const {owner, guild, invite, joiners} = await setupInvite(harness, maxUses, joinerCount);
		const responses = await Promise.all(
			joiners.map((joiner) =>
				createBuilder(harness, joiner.token).post(`/invites/${invite.code}`).body(null).executeRaw(),
			),
		);
		const statuses = responses.map((result) => result.response.status);
		expect(statuses.filter((status) => status === HTTP_STATUS.OK)).toHaveLength(maxUses);
		expect(
			statuses.filter((status) => status !== HTTP_STATUS.OK).every((status) => status === HTTP_STATUS.NOT_FOUND),
		).toBe(true);
		expect(await countMembers(harness, joiners, guild.id)).toBe(maxUses);
		expect(await findGuildInvite(harness, owner.token, guild.id, invite.code)).toBeNull();
	});
	it('counts every use when joiners arrive together', async () => {
		const {owner, guild, invite, joiners} = await setupInvite(harness, 10, 4);
		await Promise.all(
			joiners.map((joiner) =>
				createBuilder(harness, joiner.token)
					.post(`/invites/${invite.code}`)
					.body(null)
					.expect(HTTP_STATUS.OK)
					.execute(),
			),
		);
		const after = await findGuildInvite(harness, owner.token, guild.id, invite.code);
		expect(after?.uses).toBe(4);
	});
	it('returns the use when the join fails', async () => {
		const {owner, guild, invite, joiners} = await setupInvite(harness, 1, 2);
		const [first, second] = joiners;
		await createBuilder(harness, '')
			.post(`/test/guilds/${guild.id}/member-count`)
			.body({member_count: MAX_GUILD_MEMBERS})
			.execute();
		await createBuilder(harness, first!.token)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.MAX_GUILD_MEMBERS)
			.execute();
		const afterFailure = await findGuildInvite(harness, owner.token, guild.id, invite.code);
		expect(afterFailure?.uses).toBe(0);
		await createBuilder(harness, '').post(`/test/guilds/${guild.id}/member-count`).body({member_count: 1}).execute();
		await createBuilder(harness, second!.token)
			.post(`/invites/${invite.code}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await countMembers(harness, [second!], guild.id)).toBe(1);
	});
});
