// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BlocklistEntryPage {
	items: Array<{value: string; reason: string | null; expires_at: string | null; created_at: string | null}>;
}

interface BlocklistCheck {
	banned: boolean;
	expires_at: string | null;
}

const HOUR_MS = 3_600_000;

describe('Admin IP bans with an expiry', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.BAN_IP_ADD,
			AdminACLs.BAN_IP_CHECK,
			AdminACLs.BAN_IP_REMOVE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function addIpBan(body: Record<string, unknown>, status: number = HTTP_STATUS.NO_CONTENT): Promise<void> {
		await createBuilder(harness, admin.token).post('/admin/blocklists/ip/entries').body(body).expect(status).execute();
	}

	async function listIpBans(): Promise<BlocklistEntryPage['items']> {
		const page = await createBuilder<BlocklistEntryPage>(harness, admin.token)
			.get('/admin/blocklists/ip/entries')
			.expect(HTTP_STATUS.OK)
			.execute();
		return page.items;
	}

	async function checkIpBan(ip: string): Promise<BlocklistCheck> {
		return createBuilder<BlocklistCheck>(harness, admin.token)
			.get(`/admin/blocklists/ip/entries/${encodeURIComponent(ip)}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function banIpAuditLogs(): Promise<Array<AdminAuditLog>> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
		return logs.filter((log) => log.action === 'ban_ip');
	}

	it('stores an expiring ban that the listing and the check both report', async () => {
		const before = Date.now();
		await addIpBan({ip: '198.51.100.7', duration_hours: 24});

		const [entry] = await listIpBans();
		expect(entry?.value).toBe('198.51.100.7');
		expect(entry?.reason).toBe('platform_admin_enforcement');
		const expiresAt = Date.parse(entry?.expires_at ?? '');
		expect(expiresAt).toBeGreaterThanOrEqual(before + 24 * HOUR_MS);
		expect(expiresAt).toBeLessThanOrEqual(Date.now() + 24 * HOUR_MS);

		const check = await checkIpBan('198.51.100.7');
		expect(check.banned).toBe(true);
		expect(Date.parse(check.expires_at ?? '')).toBe(expiresAt);
	});

	it('records the duration and expiry in the audit log', async () => {
		await addIpBan({ip: '198.51.100.8', duration_hours: 168});

		const [log] = await banIpAuditLogs();
		const metadata = Object.fromEntries(log!.metadata);
		expect(metadata['ip']).toBe('198.51.100.8');
		expect(metadata['duration_hours']).toBe('168');
		expect(Date.parse(metadata['expires_at'] ?? '')).toBeGreaterThan(Date.now() + 167 * HOUR_MS);
	});

	it('keeps a ban permanent when no duration is given', async () => {
		await addIpBan({ip: '198.51.100.9'});
		await addIpBan({ip: '198.51.100.10', duration_hours: 0});

		const entries = await listIpBans();
		expect(entries.map((entry) => entry.expires_at)).toEqual([null, null]);
		expect(await checkIpBan('198.51.100.9')).toEqual({banned: true, expires_at: null});
		const [log] = await banIpAuditLogs();
		expect(log!.metadata.has('duration_hours')).toBe(false);
	});

	it('replaces a permanent ban with an expiring one when the address is banned again', async () => {
		await addIpBan({ip: '198.51.100.11'});
		await addIpBan({ip: '198.51.100.11', duration_hours: 24});

		const [entry] = await listIpBans();
		expect(entry?.expires_at).not.toBeNull();
		const check = await checkIpBan('198.51.100.11');
		expect(check.banned).toBe(true);
		expect(check.expires_at).not.toBeNull();
	});

	it('rejects a duration beyond one year', async () => {
		await addIpBan({ip: '198.51.100.12', duration_hours: 8761}, HTTP_STATUS.BAD_REQUEST);
		await addIpBan({ip: '198.51.100.12', duration_hours: 1.5}, HTTP_STATUS.BAD_REQUEST);

		expect(await listIpBans()).toEqual([]);
	});

	it('reports a check with no match as not banned with no expiry', async () => {
		expect(await checkIpBan('198.51.100.13')).toEqual({banned: false, expires_at: null});
	});

	it('stops applying an expiring ban once its expiry has passed', async () => {
		await new AdminRepository().banIp('198.51.100.14', 1);
		await ipBanCache.refresh();
		expect(ipBanCache.isBanned('198.51.100.14')).toBe(true);

		await new Promise((resolve) => setTimeout(resolve, 1100));

		expect(ipBanCache.isBanned('198.51.100.14')).toBe(false);
		await ipBanCache.refresh();
		expect(ipBanCache.isBanned('198.51.100.14')).toBe(false);
		expect(await listIpBans()).toEqual([]);
	});
});
