// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createGuild,
	createRole,
	getChannel,
	leaveGuild,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Guild Operation Permissions', () => {
	let harness: ApiTestHarness;
	async function addGuildFeaturesForTesting(guildId: string, features: Array<string>): Promise<void> {
		await createBuilder<{
			success: boolean;
		}>(harness, '')
			.post(`/test/guilds/${guildId}/features`)
			.body({add_features: features})
			.execute();
	}
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('should reject member from updating guild without MANAGE_GUILD', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Perms Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}`)
			.body({name: 'Hacked Guild'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('should reject nonmember from getting guild', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const nonmember = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Perms Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, nonmember.token).get(`/guilds/${guild.id}`).expect(HTTP_STATUS.FORBIDDEN).execute();
	});
	it('should allow member to leave guild', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Perms Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await leaveGuild(harness, member.token, guild.id);
		await createBuilder(harness, member.token).get(`/guilds/${guild.id}`).expect(HTTP_STATUS.FORBIDDEN).execute();
	});
	it('should reject owner from leaving guild without deleting', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Perms Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, owner.token)
			.delete(`/users/@me/guilds/${guild.id}`)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('should allow member with MANAGE_GUILD to update guild nsfw_level', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'NSFW Perms Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		const manageGuildRole = await createRole(harness, owner.token, guild.id, {
			name: 'Manage Guild',
			permissions: Permissions.MANAGE_GUILD.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, manageGuildRole.id);
		const updated = await createBuilder<GuildResponse>(harness, member.token)
			.patch(`/guilds/${guild.id}`)
			.body({nsfw_level: GuildNSFWLevel.AGE_RESTRICTED})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.nsfw_level).toBe(GuildNSFWLevel.AGE_RESTRICTED);
	});
	it('should allow bot with MANAGE_GUILD to update guild vanity URL', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Vanity URL Bot Permissions');
		const botAccount = await createTestBotAccount(harness);
		const vanityCode = 'botvanity';
		await authorizeBot(harness, owner.token, botAccount.appId, ['bot'], guild.id, Permissions.MANAGE_GUILD.toString());
		await addGuildFeaturesForTesting(guild.id, [GuildFeatures.VANITY_URL]);
		const response = await createBuilder<{
			code: string | null;
		}>(harness, `Bot ${botAccount.botToken}`)
			.patch(`/guilds/${guild.id}/vanity-url`)
			.body({code: vanityCode})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.code).toBe(vanityCode);
		const removedResponse = await createBuilder<{
			code: string | null;
		}>(harness, `Bot ${botAccount.botToken}`)
			.patch(`/guilds/${guild.id}/vanity-url`)
			.body({code: null})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(removedResponse.code).toBeNull();
	});
});
