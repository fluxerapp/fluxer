// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelOverwriteResponse, ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {GuildInviteMetadataResponse} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {createTestAccount, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';

export async function createGuild(harness: ApiTestHarness, token: string, name: string): Promise<GuildResponse> {
	return createBuilder<GuildResponse>(harness, token).post('/guilds').body({name}).execute();
}

export async function createChannel(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	name: string,
	type = 0,
): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token)
		.post(`/guilds/${guildId}/channels`)
		.body({name, type})
		.execute();
}

export async function getChannel(harness: ApiTestHarness, token: string, channelId: string): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token).get(`/channels/${channelId}`).execute();
}

export async function updateChannel(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	updates: Partial<
		Pick<
			ChannelResponse,
			'name' | 'type' | 'topic' | 'parent_id' | 'position' | 'rate_limit_per_user' | 'nsfw' | 'owner_id'
		>
	>,
): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token).patch(`/channels/${channelId}`).body(updates).execute();
}

export async function deleteChannel(harness: ApiTestHarness, token: string, channelId: string): Promise<void> {
	return createBuilder<void>(harness, token).delete(`/channels/${channelId}`).expect(204).execute();
}

export async function createChannelInvite(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<GuildInviteMetadataResponse> {
	return createBuilder<GuildInviteMetadataResponse>(harness, token)
		.post(`/channels/${channelId}/invites`)
		.body({})
		.execute();
}

export async function acceptInvite(
	harness: ApiTestHarness,
	token: string,
	inviteCode: string,
): Promise<{
	guild: GuildResponse;
}> {
	return createBuilder<{
		guild: GuildResponse;
	}>(harness, token)
		.post(`/invites/${inviteCode}`)
		.body(null)
		.execute();
}

export async function createRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	role: Partial<Omit<GuildRoleResponse, 'id' | 'position'>>,
): Promise<GuildRoleResponse> {
	return createBuilder<GuildRoleResponse>(harness, token).post(`/guilds/${guildId}/roles`).body(role).execute();
}

export async function updateRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	roleId: string,
	updates: Partial<GuildRoleResponse>,
): Promise<GuildRoleResponse> {
	return createBuilder<GuildRoleResponse>(harness, token)
		.patch(`/guilds/${guildId}/roles/${roleId}`)
		.body(updates)
		.execute();
}

export async function addMemberRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
	roleId: string,
): Promise<void> {
	return createBuilder<void>(harness, token)
		.put(`/guilds/${guildId}/members/${userId}/roles/${roleId}`)
		.expect(204)
		.execute();
}

export async function getMember(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
): Promise<GuildMemberResponse> {
	return createBuilder<GuildMemberResponse>(harness, token).get(`/guilds/${guildId}/members/${userId}`).execute();
}

export async function getGuild(harness: ApiTestHarness, token: string, guildId: string): Promise<GuildResponse> {
	return createBuilder<GuildResponse>(harness, token).get(`/guilds/${guildId}`).execute();
}

export async function updateGuild(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	updates: Partial<GuildResponse>,
): Promise<GuildResponse> {
	return createBuilder<GuildResponse>(harness, token).patch(`/guilds/${guildId}`).body(updates).execute();
}

export async function leaveGuild(harness: ApiTestHarness, token: string, guildId: string): Promise<void> {
	return createBuilder<void>(harness, token).delete(`/users/@me/guilds/${guildId}`).expect(204).execute();
}

export async function createPermissionOverwrite(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	overwriteId: string,
	overwrite: Omit<ChannelOverwriteResponse, 'id'>,
): Promise<ChannelOverwriteResponse> {
	await createBuilder<void>(harness, token)
		.put(`/channels/${channelId}/permissions/${overwriteId}`)
		.body({
			type: overwrite.type,
			allow: overwrite.allow,
			deny: overwrite.deny,
		})
		.expect(204)
		.execute();
	return {
		id: overwriteId,
		type: overwrite.type,
		allow: overwrite.allow,
		deny: overwrite.deny,
	};
}

export async function setupTestGuildWithMembers(
	harness: ApiTestHarness,
	memberCount = 2,
): Promise<{
	owner: TestAccount;
	members: Array<TestAccount>;
	guild: GuildResponse;
	systemChannel: ChannelResponse;
}> {
	const owner = await createTestAccount(harness);
	const members: Array<TestAccount> = [];
	for (let i = 0; i < memberCount; i++) {
		members.push(await createTestAccount(harness));
	}
	const guild = await createGuild(harness, owner.token, 'Test Guild');
	const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
	const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
	for (const member of members) {
		await acceptInvite(harness, member.token, invite.code);
	}
	return {owner, members, guild, systemChannel};
}

export interface MinimalChannelResponse {
	id: string;
	type: number;
	recipients?: Array<{
		id: string;
		username: string;
	}>;
}

export async function createFriendship(harness: ApiTestHarness, user1: TestAccount, user2: TestAccount): Promise<void> {
	await createBuilder<unknown>(harness, user1.token)
		.post(`/users/@me/relationships/${user2.userId}`)
		.body({})
		.execute();
	await createBuilder<unknown>(harness, user2.token).put(`/users/@me/relationships/${user1.userId}`).body({}).execute();
}

export async function createDmChannel(
	harness: ApiTestHarness,
	token: string,
	recipientId: string,
): Promise<MinimalChannelResponse> {
	const channel = await createBuilder<MinimalChannelResponse>(harness, token)
		.post('/users/@me/channels')
		.body({
			recipient_id: recipientId,
		})
		.execute();
	if (!channel.id) {
		throw new Error('DM channel response missing id');
	}
	return channel;
}

export async function sendChannelMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	content: string,
): Promise<MessageResponse> {
	await ensureSessionStarted(harness, token);
	const msg = await createBuilder<MessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body({
			content,
		})
		.execute();
	if (!msg.id) {
		throw new Error('Message response missing id');
	}
	return msg;
}

export async function blockUser(harness: ApiTestHarness, user: TestAccount, targetUserId: string): Promise<void> {
	await createBuilder<unknown>(harness, user.token)
		.put(`/users/@me/relationships/${targetUserId}`)
		.body({
			type: 2,
		})
		.execute();
}

export async function initiateCall(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	recipients: Array<string>,
	expectedStatus: number = 204,
): Promise<{
	response: Response;
	json: unknown;
}> {
	return createBuilder(harness, token)
		.post(`/channels/${channelId}/call/ring`)
		.body({recipients})
		.expect(expectedStatus)
		.executeWithResponse();
}

export async function pinMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	expectedStatus: number = 204,
): Promise<{
	response: Response;
	json: unknown;
}> {
	return createBuilder(harness, token)
		.put(`/channels/${channelId}/pins/${messageId}`)
		.body(null)
		.expect(expectedStatus)
		.executeWithResponse();
}

export interface GroupDmChannelResponse {
	id: string;
	type: number;
	name: string | null;
	owner_id: string;
	recipients: Array<{
		id: string;
		username: string;
	}>;
}

export async function createGroupDmChannel(
	harness: ApiTestHarness,
	token: string,
	recipientUserIds: Array<string>,
): Promise<GroupDmChannelResponse> {
	const channel = await createBuilder<GroupDmChannelResponse>(harness, token)
		.post('/users/@me/channels')
		.body({
			recipients: recipientUserIds,
		})
		.execute();
	if (!channel.id) {
		throw new Error('Group DM channel response missing id');
	}
	return channel;
}

export async function updateUserSettings(
	harness: ApiTestHarness,
	token: string,
	settings: Record<string, unknown>,
): Promise<void> {
	await createBuilder<unknown>(harness, token).patch('/users/@me/settings').body(settings).execute();
}

export async function removeRecipientFromGroupDm(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	userId: string,
): Promise<void> {
	await createBuilder(harness, token).delete(`/channels/${channelId}/recipients/${userId}`).expect(204).execute();
}

interface SeedPrivateChannelsResult {
	group_dms: Array<{
		channel_id: string;
		last_message_id: string;
	}>;
	dms: Array<{
		channel_id: string;
		last_message_id: string;
	}>;
}

export async function seedPrivateChannels(
	harness: ApiTestHarness,
	_token: string,
	userId: string,
	params: {
		group_dm_count?: number;
		dm_count?: number;
		recipients?: Array<string>;
		clear_existing?: boolean;
	},
): Promise<SeedPrivateChannelsResult> {
	return createBuilder<SeedPrivateChannelsResult>(harness, '')
		.post(`/test/users/${userId}/private-channels`)
		.body(params)
		.execute();
}
