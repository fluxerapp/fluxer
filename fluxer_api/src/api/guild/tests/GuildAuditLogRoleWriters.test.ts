// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild, createRole} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface AuditLogChange {
	key: string;
	old_value?: unknown;
	new_value?: unknown;
}

interface AuditLogOptions {
	role_name?: string;
}

interface AuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: AuditLogOptions;
	changes?: Array<AuditLogChange>;
}

interface AuditLogResponse {
	audit_log_entries: Array<AuditLogEntry>;
}

const ROLE_SNAPSHOT_KEYS = [
	'role_id',
	'name',
	'permissions',
	'position',
	'hoist_position',
	'color',
	'icon_hash',
	'unicode_emoji',
	'hoist',
	'mentionable',
];

async function listEntries(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	actionType: AuditLogActionType,
): Promise<Array<AuditLogEntry>> {
	const response = await createBuilder<AuditLogResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs?action_type=${actionType}&limit=100`)
		.expect(HTTP_STATUS.OK)
		.execute();
	return response.audit_log_entries;
}

describe('Guild audit log role writers', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let guild: GuildResponse;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		owner = await createTestAccount(harness);
		guild = await createGuild(harness, owner.token, 'Role Audit Guild');
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('records the role snapshot and reason on delete', async () => {
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Doomed',
			permissions: Permissions.VIEW_CHANNEL.toString(),
		});
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/roles/${role.id}`)
			.header('X-Audit-Log-Reason', 'Cleanup')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_DELETE);
		const entry = entries.find((log) => log.target_id === role.id);
		expect(entry).toBeDefined();
		expect(entry?.reason).toBe('Cleanup');
		expect(entry?.options).toBeUndefined();
		expect(entry?.changes?.map((change) => change.key)).toEqual(ROLE_SNAPSHOT_KEYS);
		expect(entry?.changes?.find((change) => change.key === 'name')).toEqual({key: 'name', old_value: 'Doomed'});
		expect(entry?.changes?.find((change) => change.key === 'permissions')).toEqual({
			key: 'permissions',
			old_value: role.permissions,
		});
	});
});
