// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupTestGuildWithMembers, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface AuditLogChange {
	key: string;
	old_value?: unknown;
	new_value?: unknown;
}

interface AuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: Record<string, unknown>;
	changes?: Array<AuditLogChange>;
}

interface AuditLogResponse {
	audit_log_entries: Array<AuditLogEntry>;
	users: Array<{
		id: string;
	}>;
}

async function getGuildUpdateLogs(harness: ApiTestHarness, token: string, guildId: string): Promise<AuditLogResponse> {
	return createBuilder<AuditLogResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs?action_type=${AuditLogActionType.GUILD_UPDATE}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function addGuildFeaturesForTesting(
	harness: ApiTestHarness,
	guildId: string,
	features: Array<string>,
): Promise<void> {
	await createBuilder<{
		success: boolean;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/features`)
		.body({add_features: features})
		.execute();
}

async function setVanityCode(harness: ApiTestHarness, token: string, guildId: string, code: string | null) {
	await createBuilder<{
		code: string | null;
	}>(harness, token)
		.patch(`/guilds/${guildId}/vanity-url`)
		.body({code})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Guild update audit log writers', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('records a rename with an afk timeout change as exactly those two changes', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		await updateGuild(harness, owner.token, guild.id, {name: 'Renamed Guild', afk_timeout: 900});
		const response = await getGuildUpdateLogs(harness, owner.token, guild.id);
		expect(response.audit_log_entries).toHaveLength(1);
		const entry = response.audit_log_entries[0];
		expect(entry?.user_id).toBe(owner.userId);
		expect(entry?.target_id).toBe(guild.id);
		expect(entry?.options).toBeUndefined();
		expect(entry?.changes).toEqual([
			{key: 'name', old_value: guild.name, new_value: 'Renamed Guild'},
			{key: 'afk_timeout', old_value: 300, new_value: 900},
		]);
	});
	test('records feature toggles as features changes', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const enabled = await updateGuild(harness, owner.token, guild.id, {
			features: [...guild.features, GuildFeatures.INVITES_DISABLED, GuildFeatures.CLONE_EMOJI_ENABLED],
		});
		await updateGuild(harness, owner.token, guild.id, {
			features: enabled.features.filter((feature: string) => feature !== GuildFeatures.INVITES_DISABLED),
		});
		const response = await getGuildUpdateLogs(harness, owner.token, guild.id);
		expect(response.audit_log_entries).toHaveLength(2);
		const [disableEntry, enableEntry] = response.audit_log_entries;
		expect(enableEntry?.options).toBeUndefined();
		expect(enableEntry?.changes?.map((change) => change.key)).toEqual(['features']);
		const enableChange = enableEntry?.changes?.[0];
		expect(enableChange?.old_value).not.toContain(GuildFeatures.INVITES_DISABLED);
		expect(enableChange?.old_value).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
		expect(enableChange?.new_value).toContain(GuildFeatures.INVITES_DISABLED);
		expect(enableChange?.new_value).toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
		expect(disableEntry?.options).toBeUndefined();
		expect(disableEntry?.changes?.map((change) => change.key)).toEqual(['features']);
		const disableChange = disableEntry?.changes?.[0];
		expect(disableChange?.old_value).toContain(GuildFeatures.INVITES_DISABLED);
		expect(disableChange?.new_value).not.toContain(GuildFeatures.INVITES_DISABLED);
		expect(disableChange?.new_value).toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
	});
	test('writes nothing when a patch sends the current name', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		await updateGuild(harness, owner.token, guild.id, {name: guild.name});
		const response = await getGuildUpdateLogs(harness, owner.token, guild.id);
		expect(response.audit_log_entries).toEqual([]);
	});
	test('records the audit log reason header', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const auditLogReason = 'Rebranding the community';
		await createBuilder<GuildResponse>(harness, owner.token)
			.patch(`/guilds/${guild.id}`)
			.header('X-Audit-Log-Reason', auditLogReason)
			.body({name: 'Reasoned Guild'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await getGuildUpdateLogs(harness, owner.token, guild.id);
		expect(response.audit_log_entries).toHaveLength(1);
		const entry = response.audit_log_entries[0];
		expect(entry?.reason).toBe(auditLogReason);
		expect(entry?.options).toBeUndefined();
		expect(entry?.changes).toEqual([{key: 'name', old_value: guild.name, new_value: 'Reasoned Guild'}]);
	});
	test('records an ownership transfer as only an owner_id change', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createBuilder<GuildResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/transfer-ownership`)
			.body({new_owner_id: member.userId, password: TEST_CREDENTIALS.STRONG_PASSWORD})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await getGuildUpdateLogs(harness, member.token, guild.id);
		expect(response.audit_log_entries).toHaveLength(1);
		const entry = response.audit_log_entries[0];
		expect(entry?.user_id).toBe(owner.userId);
		expect(entry?.target_id).toBe(guild.id);
		expect(entry?.options).toBeUndefined();
		expect(entry?.changes).toEqual([{key: 'owner_id', old_value: owner.userId, new_value: member.userId}]);
		const userIds = response.users.map((user) => user.id);
		expect(userIds).toContain(owner.userId);
		expect(userIds).toContain(member.userId);
	});
	test('writes nothing when ownership is transferred to the current owner', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		await createBuilder<GuildResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/transfer-ownership`)
			.body({new_owner_id: owner.userId, password: TEST_CREDENTIALS.STRONG_PASSWORD})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await getGuildUpdateLogs(harness, owner.token, guild.id);
		expect(response.audit_log_entries).toEqual([]);
	});
	test('records vanity url set, change and removal as only vanity_url_code changes', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		await addGuildFeaturesForTesting(harness, guild.id, [GuildFeatures.VANITY_URL]);
		const firstCode = 'audit-vanity-one';
		const secondCode = 'audit-vanity-two';
		await setVanityCode(harness, owner.token, guild.id, firstCode);
		await setVanityCode(harness, owner.token, guild.id, secondCode);
		await setVanityCode(harness, owner.token, guild.id, null);
		const response = await getGuildUpdateLogs(harness, owner.token, guild.id);
		expect(response.audit_log_entries).toHaveLength(3);
		const [removeEntry, changeEntry, setEntry] = response.audit_log_entries;
		for (const entry of response.audit_log_entries) {
			expect(entry.user_id).toBe(owner.userId);
			expect(entry.target_id).toBe(guild.id);
			expect(entry.options).toBeUndefined();
		}
		expect(setEntry?.changes).toEqual([{key: 'vanity_url_code', old_value: null, new_value: firstCode}]);
		expect(changeEntry?.changes).toEqual([{key: 'vanity_url_code', old_value: firstCode, new_value: secondCode}]);
		expect(removeEntry?.changes).toEqual([{key: 'vanity_url_code', old_value: secondCode, new_value: null}]);
	});
});
