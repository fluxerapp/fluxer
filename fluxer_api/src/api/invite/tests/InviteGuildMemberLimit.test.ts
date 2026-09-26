// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_GUILD_MEMBERS, MAX_GUILD_MEMBERS_VERY_LARGE_GUILD} from '@fluxer/constants/src/LimitConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface InviteResponse {
	code: string;
}

interface InviteAcceptResponse {
	guild: {
		id: string;
	};
}

async function createGuildInvite(harness: ApiTestHarness, token: string, channelId: string): Promise<string> {
	const invite = await createBuilder<InviteResponse>(harness, token)
		.post(`/channels/${channelId}/invites`)
		.body({})
		.execute();
	return invite.code;
}

async function addGuildFeatureForTesting(harness: ApiTestHarness, guildId: string, feature: string): Promise<void> {
	await createBuilder<{
		success: boolean;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/features`)
		.body({add_features: [feature]})
		.execute();
}

async function setGuildMemberCountForTesting(
	harness: ApiTestHarness,
	guildId: string,
	memberCount: number,
): Promise<void> {
	await createBuilder<{
		success: boolean;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

describe('Invite guild member limit', () => {
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
	it('rejects joining when guild reaches the default cap', async () => {
		const owner = await createTestAccount(harness);
		const joiner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Default cap guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild system channel is missing');
		}
		const inviteCode = await createGuildInvite(harness, owner.token, guild.system_channel_id);
		await setGuildMemberCountForTesting(harness, guild.id, MAX_GUILD_MEMBERS);
		await createBuilder(harness, joiner.token)
			.post(`/invites/${inviteCode}`)
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.MAX_GUILD_MEMBERS)
			.execute();
	});
	it('raises guild member cap to 10000 when VERY_LARGE_GUILD is enabled', async () => {
		const owner = await createTestAccount(harness);
		const firstJoiner = await createTestAccount(harness);
		const secondJoiner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Very large guild');
		if (!guild.system_channel_id) {
			throw new Error('Guild system channel is missing');
		}
		const inviteCode = await createGuildInvite(harness, owner.token, guild.system_channel_id);
		await addGuildFeatureForTesting(harness, guild.id, GuildFeatures.VERY_LARGE_GUILD);
		await setGuildMemberCountForTesting(harness, guild.id, MAX_GUILD_MEMBERS);
		const accepted = await createBuilder<InviteAcceptResponse>(harness, firstJoiner.token)
			.post(`/invites/${inviteCode}`)
			.body(null)
			.execute();
		expect(accepted.guild.id).toBe(guild.id);
		await setGuildMemberCountForTesting(harness, guild.id, MAX_GUILD_MEMBERS_VERY_LARGE_GUILD);
		await createBuilder(harness, secondJoiner.token)
			.post(`/invites/${inviteCode}`)
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.MAX_GUILD_MEMBERS)
			.execute();
	});
});
