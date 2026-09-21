// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	addMemberRole,
	createChannel,
	createGuild,
	createPermissionOverwrite,
	createRole,
	getChannel,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Channel Permission Overwrites', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should create permission overwrite for role', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
		const overwrite = await createPermissionOverwrite(harness, account.token, channel.id, role.id, {
			type: 0,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		expect(overwrite.id).toBe(role.id);
		expect(overwrite.type).toBe(0);
	});
	test('should create permission overwrite for member', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const overwrite = await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		expect(overwrite.id).toBe(member.userId);
		expect(overwrite.type).toBe(1);
	});
	test('should deny permission via overwrite', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'Test message'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should allow permission via overwrite', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'Test message'})
			.execute();
	});
	test('should update existing permission overwrite', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
		await createPermissionOverwrite(harness, account.token, channel.id, role.id, {
			type: 0,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		const updated = await createPermissionOverwrite(harness, account.token, channel.id, role.id, {
			type: 0,
			allow: (Permissions.SEND_MESSAGES | Permissions.EMBED_LINKS).toString(),
			deny: '0',
		});
		expect(BigInt(updated.allow)).toBe(Permissions.SEND_MESSAGES | Permissions.EMBED_LINKS);
	});
	test('should allow updating an overwrite when unchanged deny bits use permissions the editor lacks', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const manager = members[0];
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Channel Manager',
			permissions: (Permissions.MANAGE_ROLES | Permissions.MANAGE_CHANNELS).toString(),
		});
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Community Team'});
		await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, targetRole.id, {
			type: 0,
			allow: '0',
			deny: Permissions.MANAGE_MESSAGES.toString(),
		});
		await createBuilder(harness, manager.token)
			.put(`/channels/${systemChannel.id}/permissions/${targetRole.id}`)
			.body({
				type: 0,
				allow: Permissions.VIEW_CHANNEL.toString(),
				deny: Permissions.MANAGE_MESSAGES.toString(),
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const channelData = await getChannel(harness, owner.token, systemChannel.id);
		const overwrite = channelData.permission_overwrites?.find((o) => o.id === targetRole.id);
		expect(overwrite?.allow).toBe(Permissions.VIEW_CHANNEL.toString());
		expect(overwrite?.deny).toBe(Permissions.MANAGE_MESSAGES.toString());
	});
	test('should delete permission overwrite', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
		await createPermissionOverwrite(harness, account.token, channel.id, role.id, {
			type: 0,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		await createBuilder(harness, account.token)
			.delete(`/channels/${channel.id}/permissions/${role.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const channelData = await getChannel(harness, account.token, channel.id);
		const overwrite = channelData.permission_overwrites?.find((o) => o.id === role.id);
		expect(overwrite).toBeUndefined();
	});
	test('should require MANAGE_ROLES to create overwrites', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const role = await createRole(harness, owner.token, guild.id, {name: 'Test Role'});
		await createBuilder(harness, member.token)
			.put(`/channels/${systemChannel.id}/permissions/${role.id}`)
			.body({
				type: 0,
				allow: Permissions.SEND_MESSAGES.toString(),
				deny: '0',
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should show overwrites in channel response', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
		await createPermissionOverwrite(harness, account.token, channel.id, role.id, {
			type: 0,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		const channelData = await getChannel(harness, account.token, channel.id);
		expect(channelData.permission_overwrites).toBeDefined();
		expect(channelData.permission_overwrites?.some((o) => o.id === role.id)).toBe(true);
	});
	test('should prioritize member overwrite over role overwrite', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const role = await createRole(harness, owner.token, guild.id, {name: 'Deny Role'});
		await createBuilder<void>(harness, owner.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${role.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, role.id, {
			type: 0,
			allow: '0',
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'Test message'})
			.execute();
	});
	test('should reject invalid overwrite type', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		await createBuilder(harness, account.token)
			.put(`/channels/${channel.id}/permissions/123456789`)
			.body({
				type: 999,
				allow: '0',
				deny: '0',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should handle multiple overlapping role overwrites', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Test Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
		const role1 = await createRole(harness, account.token, guild.id, {name: 'Role 1'});
		const role2 = await createRole(harness, account.token, guild.id, {name: 'Role 2'});
		await createPermissionOverwrite(harness, account.token, channel.id, role1.id, {
			type: 0,
			allow: Permissions.SEND_MESSAGES.toString(),
			deny: '0',
		});
		await createPermissionOverwrite(harness, account.token, channel.id, role2.id, {
			type: 0,
			allow: Permissions.EMBED_LINKS.toString(),
			deny: '0',
		});
		const channelData = await getChannel(harness, account.token, channel.id);
		expect(channelData.permission_overwrites?.length).toBeGreaterThanOrEqual(2);
	});
	test('should allow patching overwrites when unchanged denies use permissions the editor lacks', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const manager = members[0];
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Channel Manager',
			permissions: (Permissions.MANAGE_ROLES | Permissions.MANAGE_CHANNELS).toString(),
		});
		const existingRole = await createRole(harness, owner.token, guild.id, {name: 'Existing Deny Role'});
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Community Team'});
		await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, existingRole.id, {
			type: 0,
			allow: '0',
			deny: Permissions.MANAGE_MESSAGES.toString(),
		});
		await createBuilder(harness, manager.token)
			.patch(`/channels/${systemChannel.id}`)
			.body({
				permission_overwrites: [
					{
						id: existingRole.id,
						type: 0,
						allow: '0',
						deny: Permissions.MANAGE_MESSAGES.toString(),
					},
					{
						id: targetRole.id,
						type: 0,
						allow: Permissions.VIEW_CHANNEL.toString(),
						deny: '0',
					},
				],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const channelData = await getChannel(harness, owner.token, systemChannel.id);
		const existingOverwrite = channelData.permission_overwrites?.find((o) => o.id === existingRole.id);
		const targetOverwrite = channelData.permission_overwrites?.find((o) => o.id === targetRole.id);
		expect(existingOverwrite?.deny).toBe(Permissions.MANAGE_MESSAGES.toString());
		expect(targetOverwrite?.allow).toBe(Permissions.VIEW_CHANNEL.toString());
	});
	test('should allow copying category overwrites that deny permissions the editor lacks', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const manager = members[0];
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Channel Manager',
			permissions: (Permissions.MANAGE_ROLES | Permissions.MANAGE_CHANNELS).toString(),
		});
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Community Team'});
		const category = await createChannel(harness, owner.token, guild.id, 'mods', ChannelTypes.GUILD_CATEGORY);
		const textChannel = await createChannel(harness, owner.token, guild.id, 'bot-moderation-log');
		await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
		await createPermissionOverwrite(harness, owner.token, category.id, targetRole.id, {
			type: 0,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.MANAGE_MESSAGES.toString(),
		});
		const parentChannel = await getChannel(harness, owner.token, category.id);
		expect(parentChannel.permission_overwrites).toBeDefined();
		await createBuilder(harness, manager.token)
			.patch(`/channels/${textChannel.id}`)
			.body({
				permission_overwrites: parentChannel.permission_overwrites,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const channelData = await getChannel(harness, owner.token, textChannel.id);
		const overwrite = channelData.permission_overwrites?.find((o) => o.id === targetRole.id);
		expect(overwrite?.allow).toBe(Permissions.VIEW_CHANNEL.toString());
		expect(overwrite?.deny).toBe(Permissions.MANAGE_MESSAGES.toString());
	});
	test('should let an editor change an overwrite that already allows a permission they lack', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const manager = members[0];
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Queue Manager',
			permissions: Permissions.MANAGE_ROLES.toString(),
		});
		const botRole = await createRole(harness, owner.token, guild.id, {name: 'Bot'});
		await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, botRole.id, {
			type: 0,
			allow: Permissions.PIN_MESSAGES.toString(),
			deny: '0',
		});
		await createBuilder(harness, manager.token)
			.put(`/channels/${systemChannel.id}/permissions/${botRole.id}`)
			.body({
				type: 0,
				allow: (Permissions.PIN_MESSAGES | Permissions.SEND_MESSAGES).toString(),
				deny: '0',
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const updated = await getChannel(harness, owner.token, systemChannel.id);
		const botOverwrite = updated.permission_overwrites?.find((o) => o.id === botRole.id);
		expect(botOverwrite?.allow).toBe((Permissions.PIN_MESSAGES | Permissions.SEND_MESSAGES).toString());
	});
	test('should reject an editor granting a permission they lack', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const manager = members[0];
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Queue Manager',
			permissions: Permissions.MANAGE_ROLES.toString(),
		});
		const botRole = await createRole(harness, owner.token, guild.id, {name: 'Bot'});
		await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
		await createBuilder(harness, manager.token)
			.put(`/channels/${systemChannel.id}/permissions/${botRole.id}`)
			.body({type: 0, allow: Permissions.PIN_MESSAGES.toString(), deny: '0'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should propagate category permission patches only to children that were synced when the category changed', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Readers'});
		const category = await createChannel(harness, owner.token, guild.id, 'restricted', ChannelTypes.GUILD_CATEGORY);
		await createPermissionOverwrite(harness, owner.token, category.id, targetRole.id, {
			type: 0,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: '0',
		});
		const syncedChild = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'synced-child',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
			})
			.execute();
		const unsyncedChild = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'unsynced-child',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
			})
			.execute();
		await createBuilder(harness, owner.token)
			.patch(`/channels/${unsyncedChild.id}`)
			.body({
				permission_overwrites: [
					{
						id: targetRole.id,
						type: 0,
						allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
						deny: '0',
					},
				],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, owner.token)
			.patch(`/channels/${category.id}`)
			.body({
				permission_overwrites: [
					{
						id: targetRole.id,
						type: 0,
						allow: (Permissions.VIEW_CHANNEL | Permissions.EMBED_LINKS).toString(),
						deny: '0',
					},
				],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const syncedChildData = await getChannel(harness, owner.token, syncedChild.id);
		const unsyncedChildData = await getChannel(harness, owner.token, unsyncedChild.id);
		const syncedOverwrite = syncedChildData.permission_overwrites?.find((o) => o.id === targetRole.id);
		const unsyncedOverwrite = unsyncedChildData.permission_overwrites?.find((o) => o.id === targetRole.id);
		expect(syncedOverwrite?.allow).toBe((Permissions.VIEW_CHANNEL | Permissions.EMBED_LINKS).toString());
		expect(unsyncedOverwrite?.allow).toBe((Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString());
	});
	test('should propagate category overwrite updates from the single-overwrite endpoint to synced children', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Moderators'});
		const category = await createChannel(harness, owner.token, guild.id, 'ops', ChannelTypes.GUILD_CATEGORY);
		await createPermissionOverwrite(harness, owner.token, category.id, targetRole.id, {
			type: 0,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: '0',
		});
		const childChannel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'ops-log',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
			})
			.execute();
		await createBuilder(harness, owner.token)
			.put(`/channels/${category.id}/permissions/${targetRole.id}`)
			.body({
				type: 0,
				allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
				deny: '0',
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const childData = await getChannel(harness, owner.token, childChannel.id);
		const overwrite = childData.permission_overwrites?.find((o) => o.id === targetRole.id);
		expect(overwrite?.allow).toBe((Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString());
	});
	test('should propagate category overwrite deletions from the single-overwrite endpoint to synced children', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Moderators'});
		const category = await createChannel(harness, owner.token, guild.id, 'ops', ChannelTypes.GUILD_CATEGORY);
		await createPermissionOverwrite(harness, owner.token, category.id, targetRole.id, {
			type: 0,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: '0',
		});
		const childChannel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'ops-log',
				type: ChannelTypes.GUILD_TEXT,
				parent_id: category.id,
			})
			.execute();
		await createBuilder(harness, owner.token)
			.delete(`/channels/${category.id}/permissions/${targetRole.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const childData = await getChannel(harness, owner.token, childChannel.id);
		const overwrite = childData.permission_overwrites?.find((o) => o.id === targetRole.id);
		expect(overwrite).toBeUndefined();
	});
});
