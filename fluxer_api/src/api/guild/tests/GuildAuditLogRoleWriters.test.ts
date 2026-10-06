// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild, createRole, updateRole, updateRolePositions} from '@app/api/guild/tests/GuildTestUtils';
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

function hasChangeKey(entry: AuditLogEntry, key: string): boolean {
	return entry.changes?.some((change) => change.key === key) ?? false;
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

	async function createOrderedRoles(names: Array<string>) {
		const roles = [];
		for (const name of names) {
			roles.push(await createRole(harness, owner.token, guild.id, {name}));
		}
		await updateRolePositions(
			harness,
			owner.token,
			guild.id,
			roles.map((role, index) => ({id: role.id, position: roles.length - index})),
		);
		return roles;
	}

	test('records the full role snapshot on create without options', async () => {
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Snapshot',
			color: 0xe67e22,
			permissions: Permissions.VIEW_CHANNEL.toString(),
		});
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_CREATE);
		const entry = entries.find((log) => log.target_id === role.id);
		expect(entry).toBeDefined();
		expect(entry?.user_id).toBe(owner.userId);
		expect(entry?.options).toBeUndefined();
		expect(entry?.changes?.map((change) => change.key)).toEqual(ROLE_SNAPSHOT_KEYS);
		expect(entry?.changes).toEqual([
			{key: 'role_id', new_value: role.id},
			{key: 'name', new_value: 'Snapshot'},
			{key: 'permissions', new_value: role.permissions},
			{key: 'position', new_value: 1},
			{key: 'hoist_position', new_value: null},
			{key: 'color', new_value: 0xe67e22},
			{key: 'icon_hash', new_value: null},
			{key: 'unicode_emoji', new_value: null},
			{key: 'hoist', new_value: false},
			{key: 'mentionable', new_value: false},
		]);
	});

	test('writes nothing for a role patch that changes nothing', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Stable', color: 0x3498db});
		await updateRole(harness, owner.token, guild.id, role.id, {name: 'Stable', color: 0x3498db});
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		expect(entries.filter((log) => log.target_id === role.id)).toEqual([]);
	});

	test('records the new role name on a rename', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Mods'});
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/${role.id}`)
			.header('X-Audit-Log-Reason', 'Clearer name')
			.body({name: 'Moderators'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		const roleEntries = entries.filter((log) => log.target_id === role.id);
		expect(roleEntries).toHaveLength(1);
		expect(roleEntries[0]?.reason).toBe('Clearer name');
		expect(roleEntries[0]?.options).toEqual({role_name: 'Moderators'});
		expect(roleEntries[0]?.changes).toEqual([{key: 'name', old_value: 'Mods', new_value: 'Moderators'}]);
	});

	test('keeps permissions_diff and records the role name on a permission update', async () => {
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Helpers',
			permissions: Permissions.VIEW_CHANNEL.toString(),
		});
		await updateRole(harness, owner.token, guild.id, role.id, {
			permissions: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
		});
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		const entry = entries.find((log) => log.target_id === role.id);
		expect(entry?.options).toEqual({role_name: 'Helpers'});
		expect(entry?.changes?.map((change) => change.key)).toEqual(['permissions', 'permissions_diff']);
	});

	test('writes no role update for the duplicate flow', async () => {
		const source = await createRole(harness, owner.token, guild.id, {
			name: 'Source',
			color: 0xe67e22,
			permissions: Permissions.VIEW_CHANNEL.toString(),
		});
		const copy = await createRole(harness, owner.token, guild.id, {
			name: 'Source copy',
			color: source.color,
			permissions: source.permissions,
		});
		await updateRole(harness, owner.token, guild.id, copy.id, {hoist: false, mentionable: false});
		const createEntries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_CREATE);
		expect(createEntries.some((log) => log.target_id === copy.id)).toBe(true);
		const updateEntries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		expect(updateEntries.filter((log) => log.target_id === copy.id)).toEqual([]);
	});

	test('writes nothing for a reorder that only renumbers roles', async () => {
		await createOrderedRoles(['Role A', 'Role B', 'Role C']);
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		expect(entries).toEqual([]);
	});

	test('records only the dragged role when a reorder moves one role', async () => {
		const [roleA, roleB, roleC] = await createOrderedRoles(['Role A', 'Role B', 'Role C']);
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles`)
			.header('X-Audit-Log-Reason', 'Promote C')
			.body([
				{id: roleC!.id, position: 3},
				{id: roleA!.id, position: 2},
				{id: roleB!.id, position: 1},
			])
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.target_id).toBe(roleC!.id);
		expect(entries[0]?.reason).toBe('Promote C');
		expect(entries[0]?.options).toEqual({role_name: 'Role C'});
		expect(entries[0]?.changes).toEqual([{key: 'position', old_value: 1, new_value: 3}]);
	});

	test('records hoist position set and reset only for the role that moved in the member list', async () => {
		const [roleA, roleB, roleC] = await createOrderedRoles(['Role A', 'Role B', 'Role C']);
		for (const role of [roleA!, roleB!, roleC!]) {
			await updateRole(harness, owner.token, guild.id, role.id, {hoist: true});
		}
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/hoist-positions`)
			.body([
				{id: roleC!.id, hoist_position: 3},
				{id: roleA!.id, hoist_position: 2},
				{id: roleB!.id, hoist_position: 1},
			])
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const afterSet = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		const setEntries = afterSet.filter((log) => hasChangeKey(log, 'hoist_position'));
		expect(setEntries).toHaveLength(1);
		expect(setEntries[0]?.target_id).toBe(roleC!.id);
		expect(setEntries[0]?.options).toEqual({role_name: 'Role C'});
		expect(setEntries[0]?.changes).toEqual([{key: 'hoist_position', old_value: null, new_value: 3}]);

		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/roles/hoist-positions`)
			.header('X-Audit-Log-Reason', 'Back to role order')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const afterReset = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		const resetEntries = afterReset.filter((log) =>
			log.changes?.some((change) => change.key === 'hoist_position' && change.new_value === null),
		);
		expect(resetEntries).toHaveLength(1);
		expect(resetEntries[0]?.target_id).toBe(roleC!.id);
		expect(resetEntries[0]?.reason).toBe('Back to role order');
		expect(resetEntries[0]?.options).toEqual({role_name: 'Role C'});
		expect(resetEntries[0]?.changes).toEqual([{key: 'hoist_position', old_value: 3, new_value: null}]);
	});

	test('records a hoist position write for a role outside the member list', async () => {
		const role = await createRole(harness, owner.token, guild.id, {name: 'Hidden'});
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/hoist-positions`)
			.body([{id: role.id, hoist_position: 5}])
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await listEntries(harness, owner.token, guild.id, AuditLogActionType.ROLE_UPDATE);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.target_id).toBe(role.id);
		expect(entries[0]?.options).toEqual({role_name: 'Hidden'});
		expect(entries[0]?.changes).toEqual([{key: 'hoist_position', old_value: null, new_value: 5}]);
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
