// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createFriendship,
	createGroupDmChannel,
	createGuild,
	createRole,
	getChannel,
	getMember,
} from '@app/api/channel/tests/ChannelTestUtils';
import {createOAuth2Application, createUniqueApplicationName} from '@app/api/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Bot Guild Add', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should add bot to guild with proper role creation', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: Permissions.SEND_MESSAGES.toString(),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const botMember = await getMember(harness, owner.token, guild.id, app.botUserId);
		expect(botMember.user?.id).toBe(app.botUserId);
	});
	test('should add bot to group DM when requested with channel_id', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		await createFriendship(harness, owner, friend);
		const groupDm = await createGroupDmChannel(harness, owner.token, [friend.userId]);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				channel_id: groupDm.id,
				permissions: Permissions.ADMINISTRATOR.toString(),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const updatedChannel = await getChannel(harness, owner.token, groupDm.id);
		expect(updatedChannel.recipients?.some((recipient) => recipient.id === app.botUserId)).toBe(true);
	});
	test('should reject group DM bot invite from users outside the group DM', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);
		await createFriendship(harness, owner, friend);
		const groupDm = await createGroupDmChannel(harness, owner.token, [friend.userId]);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, outsider.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				channel_id: groupDm.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
	});
	test('should reject bot invite requests with both guild_id and channel_id', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		await createFriendship(harness, owner, friend);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const groupDm = await createGroupDmChannel(harness, owner.token, [friend.userId]);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				channel_id: groupDm.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
	});
	test('should preserve channel_id when redirecting API authorize requests to the web authorize page', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		await createFriendship(harness, owner, friend);
		const groupDm = await createGroupDmChannel(harness, owner.token, [friend.userId]);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		const query = new URLSearchParams({
			client_id: app.application.id,
			scope: 'bot',
			channel_id: groupDm.id,
			permissions: '0',
		});
		const {response} = await createBuilder(harness, '').get(`/oauth2/authorize?${query.toString()}`).executeRaw();
		expect(response.status).toBe(302);
		const location = response.headers.get('location');
		expect(location).toBeTruthy();
		expect(new URL(location!).searchParams.get('channel_id')).toBe(groupDm.id);
	});
	test('should require MANAGE_GUILD permission to add bot', async () => {
		const owner = await createTestAccount(harness);
		const regularUser = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, regularUser.token, invite.code);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, regularUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should reject requested bot permissions the inviter does not have', async () => {
		const owner = await createTestAccount(harness);
		const manager = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, manager.token, invite.code);
		const manageGuildRole = await createRole(harness, owner.token, guild.id, {
			name: 'Community manager',
			permissions: Permissions.MANAGE_GUILD.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, manager.userId, manageGuildRole.id);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, manager.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: Permissions.ADMINISTRATOR.toString(),
			})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});
	test('should reject adding bot that is already in guild', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should gracefully handle unknown permission bits', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		const unknownPermissionBits = (1n << 60n).toString();
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: unknownPermissionBits,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('should add bot without permissions when permissions is 0', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const botMember = await getMember(harness, owner.token, guild.id, app.botUserId);
		expect(botMember.user?.id).toBe(app.botUserId);
	});
	test('should reject bot scope for non-public bot without owner consent', async () => {
		const owner = await createTestAccount(harness);
		const otherUser = await createTestAccount(harness);
		const guild = await createGuild(harness, otherUser.token, 'Other User Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: false,
		});
		await createBuilder(harness, otherUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should allow public bot to be added by any user with MANAGE_GUILD', async () => {
		const owner = await createTestAccount(harness);
		const otherUser = await createTestAccount(harness);
		const guild = await createGuild(harness, otherUser.token, 'Other User Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, otherUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '0',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('should reject negative permissions', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: '-1',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should reject invalid permissions string', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: 'not_a_number',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should create role with correct permissions and assign to bot', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		const requestedPermissions = Permissions.SEND_MESSAGES | Permissions.MANAGE_MESSAGES;
		await createBuilder(harness, owner.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: requestedPermissions.toString(),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const roles = await createBuilder<Array<GuildRoleResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/roles`)
			.execute();
		const botRole = roles.find((r) => r.name === app.application.name);
		expect(botRole).toBeDefined();
		expect(BigInt(botRole!.permissions)).toBe(requestedPermissions);
		const botMember = await getMember(harness, owner.token, guild.id, app.botUserId);
		expect(botMember.roles).toContain(botRole!.id);
	});
	test('should allow administrator to grant bot permissions they lack explicitly', async () => {
		const owner = await createTestAccount(harness);
		const admin = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Test Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, admin.token, invite.code);
		const adminRole = await createRole(harness, owner.token, guild.id, {
			name: 'Admin',
			permissions: Permissions.ADMINISTRATOR.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, admin.userId, adminRole.id);
		const app = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
			redirect_uris: ['https://example.com/callback'],
			bot_public: true,
		});
		const botPermissions = Permissions.MANAGE_MESSAGES | Permissions.MANAGE_CHANNELS;
		await createBuilder(harness, admin.token)
			.post('/oauth2/authorize/consent')
			.body({
				client_id: app.application.id,
				scope: 'bot',
				guild_id: guild.id,
				permissions: botPermissions.toString(),
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const roles = await createBuilder<Array<GuildRoleResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/roles`)
			.execute();
		const botRole = roles.find((r) => r.name === app.application.name);
		expect(botRole).toBeDefined();
		expect(BigInt(botRole!.permissions)).toBe(botPermissions);
		const botMember = await getMember(harness, owner.token, guild.id, app.botUserId);
		expect(botMember.roles).toContain(botRole!.id);
	});
});
