// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {
	addMemberRole,
	createChannel,
	createChannelInvite,
	createRole,
	deleteInvite,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createGuildEmoji, createWebhook, deleteWebhook, updateWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {GuildStickerWithUserResponse} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function listAuditLogs(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	actionType?: AuditLogActionType,
): Promise<GuildAuditLogListResponse> {
	const query = actionType === undefined ? '' : `?action_type=${actionType}`;
	return createBuilder<GuildAuditLogListResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs${query}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function grantGuildPermissions(
	harness: ApiTestHarness,
	owner: TestAccount,
	guildId: string,
	member: TestAccount,
	permissions: bigint,
): Promise<void> {
	const role = await createRole(harness, owner.token, guildId, {
		name: 'Audit moderator',
		permissions: (Permissions.VIEW_CHANNEL | permissions).toString(),
	});
	await addMemberRole(harness, owner.token, guildId, member.userId, role.id);
}

describe('Guild audit log content writers', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('types invite create options', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'invites');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.INVITE_CREATE);
		const entry = response.audit_log_entries.find((log) => log.target_id === invite.code);
		expect(entry).toBeDefined();
		expect(Object.keys(entry?.options ?? {}).sort()).toEqual([
			'channel_id',
			'inviter_id',
			'max_age',
			'max_uses',
			'temporary',
		]);
		expect(entry?.options?.channel_id).toBe(channel.id);
		expect(entry?.options?.inviter_id).toBe(owner.userId);
		expect(typeof entry?.options?.max_age).toBe('number');
		expect(typeof entry?.options?.max_uses).toBe('number');
		expect(typeof entry?.options?.temporary).toBe('boolean');
	});

	test('returns the inviter of an invite deleted by a moderator', async () => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const invite = await createChannelInvite(harness, member.token, channels[0]!.id);
		await deleteInvite(harness, owner.token, invite.code);
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.INVITE_DELETE);
		const entry = response.audit_log_entries.find((log) => log.target_id === invite.code);
		expect(entry?.user_id).toBe(owner.userId);
		expect(entry?.options?.inviter_id).toBe(member.userId);
		expect(response.users.map((user) => user.id)).toContain(member.userId);
	});

	test('does not record a webhook update that changes nothing', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'hooks');
		const webhook = await createWebhook(harness, channel.id, owner.token, 'Audit Hook');
		await updateWebhook(harness, webhook.id, owner.token, {name: 'Audit Hook'});
		const unchanged = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.WEBHOOK_UPDATE);
		expect(unchanged.audit_log_entries.filter((log) => log.target_id === webhook.id)).toHaveLength(0);
		await updateWebhook(harness, webhook.id, owner.token, {name: 'Renamed Hook'});
		const renamed = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.WEBHOOK_UPDATE);
		const entries = renamed.audit_log_entries.filter((log) => log.target_id === webhook.id);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.changes).toEqual([{key: 'name', old_value: 'Audit Hook', new_value: 'Renamed Hook'}]);
	});

	test('returns the creator of a webhook deleted by another moderator', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await grantGuildPermissions(harness, owner, guild.id, member, Permissions.MANAGE_WEBHOOKS);
		const channel = await createChannel(harness, owner.token, guild.id, 'hooks');
		const webhook = await createWebhook(harness, channel.id, owner.token, 'Owner Hook');
		await deleteWebhook(harness, webhook.id, member.token);
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.WEBHOOK_DELETE);
		const entry = response.audit_log_entries.find((log) => log.target_id === webhook.id);
		expect(entry?.user_id).toBe(member.userId);
		expect(response.users.map((user) => user.id)).toContain(owner.userId);
	});

	test('does not record an emoji rename to the same name', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const emoji = await createGuildEmoji(harness, owner.token, guild.id, 'audit_emoji');
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/emojis/${emoji.id}`)
			.body({name: 'audit_emoji'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.EMOJI_UPDATE);
		expect(response.audit_log_entries.filter((log) => log.target_id === emoji.id)).toHaveLength(0);
	});

	test('returns the uploader of an emoji deleted by a moderator', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await grantGuildPermissions(harness, owner, guild.id, member, Permissions.MANAGE_EXPRESSIONS);
		const emoji = await createGuildEmoji(harness, owner.token, guild.id, 'owner_emoji');
		await createBuilder(harness, member.token)
			.delete(`/guilds/${guild.id}/emojis/${emoji.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.EMOJI_DELETE);
		const entry = response.audit_log_entries.find((log) => log.target_id === emoji.id);
		expect(entry?.user_id).toBe(member.userId);
		expect(response.users.map((user) => user.id)).toContain(owner.userId);
	});

	test('does not record a sticker update that changes nothing', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const sticker = await createBuilder<GuildStickerWithUserResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/stickers`)
			.body({name: 'audit_sticker', description: 'audit sticker', tags: [], image: getPngDataUrl()})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/stickers/${sticker.id}`)
			.body({name: 'audit_sticker', description: 'audit sticker', tags: []})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.STICKER_UPDATE);
		expect(response.audit_log_entries.filter((log) => log.target_id === sticker.id)).toHaveLength(0);
	});

	test('serves the same entries on the admin route', async () => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const invite = await createChannelInvite(harness, member.token, channels[0]!.id);
		await deleteInvite(harness, owner.token, invite.code);
		await createRole(harness, owner.token, guild.id, {
			name: 'Admin parity role',
			permissions: Permissions.VIEW_CHANNEL.toString(),
		});
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:audit_log:view']);
		const publicResponse = await listAuditLogs(harness, owner.token, guild.id);
		const adminResponse = await createBuilder<GuildAuditLogListResponse>(harness, admin.token)
			.get(`/admin/guilds/${guild.id}/audit-logs`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(publicResponse.audit_log_entries.length).toBeGreaterThan(0);
		expect(adminResponse.audit_log_entries).toEqual(publicResponse.audit_log_entries);
		expect(adminResponse.users.map((user) => user.id).sort()).toEqual(
			publicResponse.users.map((user) => user.id).sort(),
		);
	});
});
