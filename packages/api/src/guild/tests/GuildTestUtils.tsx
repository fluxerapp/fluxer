/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount, type TestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {GuildInviteMetadataResponse} from '@fluxer/schema/src/domains/invite/InviteSchemas';

export async function createGuild(harness: ApiTestHarness, token: string, name: string): Promise<GuildResponse> {
	return createBuilder<GuildResponse>(harness, token).post('/guilds').body({name}).execute();
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
	await createBuilder(harness, token).delete(`/users/@me/guilds/${guildId}`).expect(204).execute();
}

export async function deleteGuild(harness: ApiTestHarness, token: string, guildId: string): Promise<void> {
	await createBuilder(harness, token).delete(`/guilds/${guildId}`).expect(204).execute();
}

export async function getUserGuilds(harness: ApiTestHarness, token: string): Promise<Array<GuildResponse>> {
	return createBuilder<Array<GuildResponse>>(harness, token).get('/users/@me/guilds').execute();
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
	updates: Partial<ChannelResponse>,
): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token).patch(`/channels/${channelId}`).body(updates).execute();
}

export async function deleteChannel(harness: ApiTestHarness, token: string, channelId: string): Promise<void> {
	await createBuilder(harness, token).delete(`/channels/${channelId}`).expect(204).execute();
}

export async function getGuildChannels(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
): Promise<Array<ChannelResponse>> {
	return createBuilder<Array<ChannelResponse>>(harness, token).get(`/guilds/${guildId}/channels`).execute();
}

export async function updateChannelPositions(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	positions: Array<{
		id: string;
		position?: number;
		lock_permissions?: boolean | null;
		parent_id?: string | null;
		preceding_sibling_id?: string | null;
	}>,
): Promise<void> {
	await createBuilder(harness, token).patch(`/guilds/${guildId}/channels`).body(positions).expect(204).execute();
}

export async function createRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	role: Partial<Omit<GuildRoleResponse, 'id' | 'position'>>,
): Promise<GuildRoleResponse> {
	return createBuilder<GuildRoleResponse>(harness, token).post(`/guilds/${guildId}/roles`).body(role).execute();
}

export async function getRoles(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
): Promise<Array<GuildRoleResponse>> {
	return createBuilder<Array<GuildRoleResponse>>(harness, token).get(`/guilds/${guildId}/roles`).execute();
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

export async function deleteRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	roleId: string,
): Promise<void> {
	await createBuilder(harness, token).delete(`/guilds/${guildId}/roles/${roleId}`).expect(204).execute();
}

export async function updateRolePositions(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	positions: Array<{id: string; position: number}>,
): Promise<void> {
	await createBuilder(harness, token).patch(`/guilds/${guildId}/roles`).body(positions).expect(204).execute();
}

export async function addMemberRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
	roleId: string,
): Promise<void> {
	await createBuilder(harness, token).put(`/guilds/${guildId}/members/${userId}/roles/${roleId}`).expect(204).execute();
}

export async function removeMemberRole(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
	roleId: string,
): Promise<void> {
	await createBuilder(harness, token)
		.delete(`/guilds/${guildId}/members/${userId}/roles/${roleId}`)
		.expect(204)
		.execute();
}

export async function updateMember(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
	updates: {roles?: Array<string>; nick?: string},
): Promise<GuildMemberResponse> {
	return createBuilder<GuildMemberResponse>(harness, token)
		.patch(`/guilds/${guildId}/members/${userId}`)
		.body(updates)
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
): Promise<{guild: GuildResponse}> {
	return createBuilder<{guild: GuildResponse}>(harness, token).post(`/invites/${inviteCode}`).body(null).execute();
}

export async function getInvite(harness: ApiTestHarness, inviteCode: string): Promise<GuildInviteMetadataResponse> {
	return createBuilder<GuildInviteMetadataResponse>(harness, '').get(`/invites/${inviteCode}`).execute();
}

export async function deleteInvite(harness: ApiTestHarness, token: string, inviteCode: string): Promise<void> {
	await createBuilder(harness, token).delete(`/invites/${inviteCode}`).expect(204).execute();
}

export async function setupTestGuild(
	harness: ApiTestHarness,
	account?: TestAccount,
): Promise<{
	account: TestAccount;
	guild: GuildResponse;
}> {
	const testAccount = account ?? (await createTestAccount(harness));
	const guild = await createGuild(harness, testAccount.token, 'Test Guild');
	return {account: testAccount, guild};
}

export async function setupTestGuildWithChannels(
	harness: ApiTestHarness,
	account?: TestAccount,
): Promise<{
	account: TestAccount;
	guild: GuildResponse;
	channels: Array<ChannelResponse>;
}> {
	const testAccount = account ?? (await createTestAccount(harness));
	const guild = await createGuild(harness, testAccount.token, 'Test Guild');

	const channels: Array<ChannelResponse> = [];
	if (guild.system_channel_id) {
		channels.push(await getChannel(harness, testAccount.token, guild.system_channel_id));
	}

	return {account: testAccount, guild, channels};
}

export async function setupTestGuildWithMembers(
	harness: ApiTestHarness,
	memberCount = 2,
): Promise<{
	owner: TestAccount;
	members: Array<TestAccount>;
	guild: GuildResponse;
	channels: Array<ChannelResponse>;
}> {
	const owner = await createTestAccount(harness);
	const members: Array<TestAccount> = [];

	for (let i = 0; i < memberCount; i++) {
		members.push(await createTestAccount(harness));
	}

	const guild = await createGuild(harness, owner.token, 'Test Guild');
	const channels: Array<ChannelResponse> = [];

	if (guild.system_channel_id) {
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id);
		channels.push(systemChannel);

		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);

		for (const member of members) {
			await acceptInvite(harness, member.token, invite.code);
		}
	}

	return {owner, members, guild, channels};
}
