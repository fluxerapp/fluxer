// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {deleteMessage, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, executeWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface AuditLogOptions {
	channel_id?: string;
	count?: number;
	message_id?: string;
}

interface AuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: AuditLogOptions;
}

interface AuditLogResponse {
	audit_log_entries: Array<AuditLogEntry>;
}

async function fetchAuditLogEntries(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	actionType: AuditLogActionType,
): Promise<Array<AuditLogEntry>> {
	const response = await createBuilder<AuditLogResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs?action_type=${actionType}`)
		.expect(HTTP_STATUS.OK)
		.execute();
	return response.audit_log_entries;
}

function requireEntry(entries: Array<AuditLogEntry>, predicate: (entry: AuditLogEntry) => boolean): AuditLogEntry {
	const entry = entries.find(predicate);
	if (!entry) {
		throw new Error('Expected audit log entry was not recorded');
	}
	return entry;
}

describe('Guild audit log message writers', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test("records a moderator deleting another member's message with the header reason", async () => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const channel = channels[0];
		const message = await sendMessage(harness, member.token, channel.id, 'please remove this');
		const reason = 'Off-topic';
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}/messages/${message.id}`)
			.header('X-Audit-Log-Reason', reason)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await fetchAuditLogEntries(harness, owner.token, guild.id, AuditLogActionType.MESSAGE_DELETE);
		const entry = requireEntry(entries, (candidate) => candidate.target_id === message.id);
		expect(entry.user_id).toBe(owner.userId);
		expect(entry.reason).toBe(reason);
		expect(entry.options?.channel_id).toBe(channel.id);
	});
	test('records an author deleting their own message', async () => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const channel = channels[0];
		const message = await sendMessage(harness, member.token, channel.id, 'my own mistake');
		await deleteMessage(harness, member.token, channel.id, message.id);
		const entries = await fetchAuditLogEntries(harness, owner.token, guild.id, AuditLogActionType.MESSAGE_DELETE);
		const entry = requireEntry(entries, (candidate) => candidate.target_id === message.id);
		expect(entry.user_id).toBe(member.userId);
		expect(entry.reason).toBeUndefined();
		expect(entry.options?.channel_id).toBe(channel.id);
	});
	test('records a moderator deleting a webhook message', async () => {
		const {owner, guild, channels} = await setupTestGuildWithMembers(harness, 0);
		const channel = channels[0];
		const webhook = await createWebhook(harness, channel.id, owner.token, 'Relay');
		const {json: message} = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{content: 'relayed update', wait: true},
			200,
		);
		if (!message) {
			throw new Error('Webhook execution did not return a message');
		}
		const reason = 'Relay spam';
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}/messages/${message.id}`)
			.header('X-Audit-Log-Reason', reason)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await fetchAuditLogEntries(harness, owner.token, guild.id, AuditLogActionType.MESSAGE_DELETE);
		const entry = requireEntry(entries, (candidate) => candidate.target_id === message.id);
		expect(entry.user_id).toBe(owner.userId);
		expect(entry.reason).toBe(reason);
		expect(entry.options?.channel_id).toBe(channel.id);
	});
	test('records a bulk delete with the header reason and a numeric count', async () => {
		const {owner, guild, channels} = await setupTestGuildWithMembers(harness, 0);
		const channel = channels[0];
		const messageIds: Array<string> = [];
		for (let index = 0; index < 3; index++) {
			const message = await sendMessage(harness, owner.token, channel.id, `bulk ${index}`);
			messageIds.push(message.id);
		}
		const reason = 'Clearing a raid';
		await createBuilder(harness, owner.token)
			.post(`/channels/${channel.id}/messages/bulk-delete`)
			.header('X-Audit-Log-Reason', reason)
			.body({message_ids: messageIds})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await fetchAuditLogEntries(harness, owner.token, guild.id, AuditLogActionType.MESSAGE_BULK_DELETE);
		const entry = requireEntry(entries, (candidate) => candidate.options?.channel_id === channel.id);
		expect(entry.user_id).toBe(owner.userId);
		expect(entry.reason).toBe(reason);
		expect(entry.options?.count).toBe(3);
	});
	test('records pin and unpin with the header reason', async () => {
		const {owner, guild, channels} = await setupTestGuildWithMembers(harness, 0);
		const channel = channels[0];
		const message = await sendMessage(harness, owner.token, channel.id, 'pin me');
		await createBuilder(harness, owner.token)
			.put(`/channels/${channel.id}/pins/${message.id}`)
			.header('X-Audit-Log-Reason', 'Important announcement')
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}/pins/${message.id}`)
			.header('X-Audit-Log-Reason', 'Announcement expired')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const pinEntries = await fetchAuditLogEntries(harness, owner.token, guild.id, AuditLogActionType.MESSAGE_PIN);
		const pinEntry = requireEntry(pinEntries, (candidate) => candidate.target_id === message.id);
		expect(pinEntry.user_id).toBe(owner.userId);
		expect(pinEntry.reason).toBe('Important announcement');
		expect(pinEntry.options?.channel_id).toBe(channel.id);
		const unpinEntries = await fetchAuditLogEntries(harness, owner.token, guild.id, AuditLogActionType.MESSAGE_UNPIN);
		const unpinEntry = requireEntry(unpinEntries, (candidate) => candidate.target_id === message.id);
		expect(unpinEntry.user_id).toBe(owner.userId);
		expect(unpinEntry.reason).toBe('Announcement expired');
		expect(unpinEntry.options?.channel_id).toBe(channel.id);
	});
});
