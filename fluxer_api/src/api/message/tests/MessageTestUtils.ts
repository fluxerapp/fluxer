// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
export async function sendMessage(
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
export async function getMessages(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	queryParams?: Record<string, string>,
): Promise<Array<MessageResponse>> {
	const queryString = queryParams
		? `?${new URLSearchParams(Object.entries(queryParams).map(([k, v]) => [k, v] as [string, string])).toString()}`
		: '';
	return createBuilder<Array<MessageResponse>>(harness, token)
		.get(`/channels/${channelId}/messages${queryString}`)
		.execute();
}
export async function deleteMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<void> {
	await createBuilder<void>(harness, token)
		.delete(`/channels/${channelId}/messages/${messageId}`)
		.expect(204)
		.execute();
}
export async function pinMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<void> {
	await createBuilder<void>(harness, token)
		.put(`/channels/${channelId}/pins/${messageId}`)
		.body(null)
		.expect(204)
		.execute();
}
export async function createGuild(harness: ApiTestHarness, token: string, name: string): Promise<GuildResponse> {
	const guild = await createBuilder<GuildResponse>(harness, token)
		.post('/guilds')
		.body({
			name,
		})
		.execute();
	if (!guild.id) {
		throw new Error('Guild response missing id');
	}
	return guild;
}
export async function createDMChannel(
	harness: ApiTestHarness,
	token: string,
	recipientUserId: string,
): Promise<MessageResponse> {
	const channel = await createBuilder<MessageResponse>(harness, token)
		.post('/users/@me/channels')
		.body({
			recipient_id: recipientUserId,
		})
		.execute();
	if (!channel.id) {
		throw new Error('Channel response missing id');
	}
	return channel;
}
export async function createChannelInvite(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<{
	code: string;
}> {
	return createBuilder<{
		code: string;
	}>(harness, token)
		.post(`/channels/${channelId}/invites`)
		.body({})
		.execute();
}
export async function acceptInvite(harness: ApiTestHarness, token: string, inviteCode: string): Promise<void> {
	await createBuilder<void>(harness, token).post(`/invites/${inviteCode}`).body({}).expect(200).execute();
}
export async function updateChannelPermissions(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	overwriteId: string,
	overwrite: {
		type: number;
		allow?: string;
		deny?: string;
	},
): Promise<void> {
	await createBuilder<void>(harness, token)
		.put(`/channels/${channelId}/permissions/${overwriteId}`)
		.body(overwrite)
		.expect(204)
		.execute();
}
export async function createFriendship(harness: ApiTestHarness, user1: TestAccount, user2: TestAccount): Promise<void> {
	await createBuilder<unknown>(harness, user1.token)
		.post(`/users/@me/relationships/${user2.userId}`)
		.body({})
		.execute();
	await createBuilder<unknown>(harness, user2.token).put(`/users/@me/relationships/${user1.userId}`).body({}).execute();
}
export async function ensureSessionStarted(harness: ApiTestHarness, token: string): Promise<void> {
	const me = await createBuilder<{
		id: string;
		flags?: string | number;
	}>(harness, token)
		.get('/users/@me')
		.execute();
	const HAS_SESSION_STARTED = BigInt(1) << BigInt(39);
	const currentFlags = BigInt(me.flags ?? 0);
	if ((currentFlags & HAS_SESSION_STARTED) !== BigInt(0)) {
		return;
	}
	await createBuilder<unknown>(harness, token)
		.patch(`/test/users/${me.id}/flags`)
		.body({
			flags: (currentFlags | HAS_SESSION_STARTED).toString(),
		})
		.execute();
}
export async function markChannelAsIndexed(harness: ApiTestHarness, channelId: string): Promise<void> {
	await createBuilder<{
		channel_id: string;
		indexed_at: string;
	}>(harness, '')
		.post(`/test/channels/${channelId}/mark-indexed`)
		.body({})
		.execute();
}
export async function markGuildChannelsAsIndexed(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
): Promise<void> {
	const channels = await createBuilder<
		Array<{
			id: string;
		}>
	>(harness, token)
		.get(`/guilds/${guildId}/channels`)
		.execute();
	await Promise.all(channels.map((channel) => markChannelAsIndexed(harness, channel.id)));
}
export async function markUserDmChannelsAsIndexed(harness: ApiTestHarness, token: string): Promise<void> {
	const channels = await createBuilder<
		Array<{
			id: string;
		}>
	>(harness, token)
		.get('/users/@me/channels')
		.execute();
	await Promise.all(channels.map((channel) => markChannelAsIndexed(harness, channel.id)));
}
