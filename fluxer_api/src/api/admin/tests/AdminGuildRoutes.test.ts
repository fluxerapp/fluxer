// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createGuild} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AdminGuildDetail {
	guild: {
		id: string;
		name: string;
		owner_id: string;
		features: Array<string>;
	} | null;
}

interface AdminGuildUpdate {
	guild: {
		id: string;
		name: string;
		owner_id: string;
		features: Array<string>;
	};
}

describe('Admin guild routes', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	test('GET /admin/guilds/{guild_id} returns the guild detail', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
		const guild = await createGuild(harness, admin.token, `Detail Guild ${Date.now()}`);
		const result = await createBuilder<AdminGuildDetail>(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.guild?.id).toBe(guild.id);
		expect(result.guild?.owner_id).toBe(admin.userId);
	});
	test('PATCH /admin/guilds/{guild_id} renames a guild with guild:update:name', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:update:name']);
		const guild = await createGuild(harness, admin.token, `Rename Guild ${Date.now()}`);
		const renamed = `Renamed Guild ${Date.now()}`;
		const result = await createBuilder<AdminGuildUpdate>(harness, `${admin.token}`)
			.patch(`/admin/guilds/${guild.id}`)
			.body({name: renamed})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.guild.name).toBe(renamed);
	});
	test('PATCH /admin/guilds/{guild_id} applies every supplied field group in one call', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'guild:update:name',
			'guild:update:settings',
			'guild:update:features',
		]);
		const guild = await createGuild(harness, admin.token, `Combined Guild ${Date.now()}`);
		const renamed = `Combined Renamed ${Date.now()}`;
		const result = await createBuilder<AdminGuildUpdate>(harness, `${admin.token}`)
			.patch(`/admin/guilds/${guild.id}`)
			.body({name: renamed, verification_level: 1, add_features: ['VERIFIED']})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.guild.name).toBe(renamed);
		expect(result.guild.features).toContain('VERIFIED');
	});
	test('PATCH /admin/guilds/{guild_id} requires the ACL selected by every supplied field', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:update:settings']);
		const guild = await createGuild(harness, admin.token, `Partial ACL Guild ${Date.now()}`);
		await createBuilder(harness, `${admin.token}`)
			.patch(`/admin/guilds/${guild.id}`)
			.body({name: 'Not Allowed', verification_level: 1})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
	test('PATCH /admin/guilds/{guild_id} rejects an empty patch without the wildcard ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:update:name']);
		const guild = await createGuild(harness, admin.token, `Empty Patch Guild ${Date.now()}`);
		await createBuilder(harness, `${admin.token}`)
			.patch(`/admin/guilds/${guild.id}`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
	test('guild member add, ban and removal use the member and ban sub-resources', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'guild:lookup',
			'guild:list:members',
			'guild:force_add_member',
			'guild:kick_member',
			'guild:ban_member',
		]);
		const target = await createTestAccount(harness);
		const guild = await createGuild(harness, admin.token, `Membership Guild ${Date.now()}`);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/guilds/${guild.id}/members/${target.userId}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, `${admin.token}`)
			.delete(`/admin/guilds/${guild.id}/members/${target.userId}`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('GET /admin/guilds/{guild_id}/members lists members', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:list:members']);
		const guild = await createGuild(harness, admin.token, `Member List Guild ${Date.now()}`);
		const result = await createBuilder<{
			members: Array<unknown>;
			total: number;
			limit: number;
			offset: number;
		}>(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}/members?limit=10&offset=0`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.limit).toBe(10);
		expect(result.offset).toBe(0);
	});
	test('GET /admin/guilds/{guild_id}/members requires guild:list:members', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
		const guild = await createGuild(harness, admin.token, `Member ACL Guild ${Date.now()}`);
		await createBuilder(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}/members`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
	test('guild expression listings and asset purge live under the guild', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'asset:purge', 'asset:purge']);
		const guild = await createGuild(harness, admin.token, `Expression Guild ${Date.now()}`);
		const emojis = await createBuilder<{
			guild_id: string;
			emojis: Array<unknown>;
		}>(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}/emojis`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(emojis.guild_id).toBe(guild.id);
		const stickers = await createBuilder<{
			guild_id: string;
			stickers: Array<unknown>;
		}>(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}/stickers`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(stickers.guild_id).toBe(guild.id);
		const purge = await createBuilder<{
			processed: Array<{id: string; asset_type: string}>;
			errors: Array<unknown>;
		}>(harness, `${admin.token}`)
			.delete(`/admin/guilds/${guild.id}/assets`)
			.body({ids: ['123456789012345678']})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(purge.processed).toHaveLength(1);
		expect(purge.processed[0].asset_type).toBe('unknown');
	});
	test('GET /admin/guilds/{guild_id}/audit-logs returns the guild audit log', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:audit_log:view']);
		const guild = await createGuild(harness, admin.token, `Audit Guild ${Date.now()}`);
		await createBuilder<{
			audit_log_entries: Array<unknown>;
		}>(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}/audit-logs?limit=10`)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('guild reload and shutdown are collection sub-resources', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:reload', 'guild:shutdown']);
		const guild = await createGuild(harness, admin.token, `Lifecycle Guild ${Date.now()}`);
		await createBuilder<{success: boolean}>(harness, `${admin.token}`)
			.post(`/admin/guilds/${guild.id}/reloads`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder<{success: boolean}>(harness, `${admin.token}`)
			.post(`/admin/guilds/${guild.id}/shutdowns`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('DELETE /admin/guilds/{guild_id} deletes the guild', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup', 'guild:delete']);
		const guild = await createGuild(harness, admin.token, `Doomed Guild ${Date.now()}`);
		await createBuilder<{success: boolean}>(harness, `${admin.token}`)
			.delete(`/admin/guilds/${guild.id}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		const result = await createBuilder<AdminGuildDetail>(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.guild).toBeNull();
	});
});
