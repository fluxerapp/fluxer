// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_PERMISSIONS, ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface Setup {
	owner: TestAccount;
	guildId: string;
	channelId: string;
	thread: ThreadChannelResponse;
	role: GuildRoleResponse;
}

describe('channel_threads kill switch', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'kill switch');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		const role = await threadsRequest<GuildRoleResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/roles`)
			.body({name: 'mods', permissions: (Permissions.SEND_MESSAGES | ThreadPermissionFlags.MANAGE_THREADS).toString()})
			.execute();
		await threadsRequest(harness, owner.token)
			.patch(`/guilds/${guild.id}/roles/${role.id}`)
			.body({permissions: Permissions.SEND_MESSAGES.toString()})
			.execute();
		return {owner, guildId: guild.id, channelId: channel.id, thread, role};
	}

	async function auditLog(s: Setup): Promise<GuildAuditLogListResponse> {
		return threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token)
			.get(`/guilds/${s.guildId}/audit-logs`)
			.execute();
	}

	function roleEntries(log: GuildAuditLogListResponse, roleId: string, action: AuditLogActionType) {
		return log.audit_log_entries.filter((entry) => entry.target_id === roleId && entry.action_type === action);
	}

	function permissionsChange(entry: GuildAuditLogListResponse['audit_log_entries'][number] | undefined) {
		return entry?.changes?.find((change) => change.key === 'permissions');
	}

	test('viewers see thread entries and thread bits while the guild is active', async () => {
		const s = await setup();
		const log = await auditLog(s);
		expect(log.audit_log_entries.map((entry) => entry.action_type)).toContain(AuditLogActionType.THREAD_CREATE);
		const created = permissionsChange(roleEntries(log, s.role.id, AuditLogActionType.ROLE_CREATE)[0]);
		expect(BigInt(created?.new_value as string) & THREAD_PERMISSIONS).toBe(ThreadPermissionFlags.MANAGE_THREADS);
		expect(roleEntries(log, s.role.id, AuditLogActionType.ROLE_UPDATE)).toHaveLength(1);
	});

	test('after the kill switch threads are unknown and the audit log is filtered and masked', async () => {
		const s = await setup();
		await setChannelThreadsConfig({enabled: false});
		await threadsRequest(harness, s.owner.token)
			.get(`/channels/${s.thread.id}`)
			.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
			.execute();
		const log = await auditLog(s);
		expect(log).not.toHaveProperty('threads');
		expect(log.audit_log_entries.map((entry) => entry.action_type)).not.toContain(AuditLogActionType.THREAD_CREATE);
		expect(log.audit_log_entries.some((entry) => entry.target_id === s.thread.id)).toBe(false);
		const created = permissionsChange(roleEntries(log, s.role.id, AuditLogActionType.ROLE_CREATE)[0]);
		expect(BigInt(created?.new_value as string)).toBe(Permissions.SEND_MESSAGES);
		expect(roleEntries(log, s.role.id, AuditLogActionType.ROLE_UPDATE)).toHaveLength(0);
	});

	test('after the kill switch the gateway channels collection keeps the taint marker only', async () => {
		const s = await setup();
		await setChannelThreadsConfig({enabled: false});
		const {data} = await createBuilder<{data: Record<string, unknown>}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'guild_collection', guild_id: s.guildId, collection: 'channels'})
			.execute();
		expect(data.thread_tainted).toBe(true);
		for (const key of ['thread_gate', 'threads', 'thread_members', 'thread_only_channels']) {
			expect(data).not.toHaveProperty(key);
		}
	});
});
