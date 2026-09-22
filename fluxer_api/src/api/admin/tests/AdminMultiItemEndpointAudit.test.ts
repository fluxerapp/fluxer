// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const AUDIT_REASON = 'Ticket 4471';

describe('Multi-item admin endpoint audit entries', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function createAdmin(acls: Array<string>): Promise<TestAccount> {
		return setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE, ...acls]);
	}

	async function findAuditLog(action: string): Promise<AdminAuditLog | undefined> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(100);
		return logs.find((log) => log.action === action);
	}

	test('records a gateway reload of many guilds', async () => {
		const admin = await createAdmin([AdminACLs.GATEWAY_RELOAD_ALL]);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Reload Guild');

		await createBuilder(harness, `${admin.token}`)
			.post('/admin/gateway/reloads')
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.body({guild_ids: [guild.id]})
			.expect(HTTP_STATUS.OK)
			.execute();

		const log = await findAuditLog('reload_guilds');
		expect(log).toBeDefined();
		expect(log?.adminUserId.toString()).toBe(admin.userId);
		expect(log?.targetType).toBe('guild');
		expect(log?.auditLogReason).toBe(AUDIT_REASON);
		expect(log?.metadata.get('guild_count')).toBe('1');
		expect(log?.metadata.get('reloaded')).toBeDefined();
	});

	test('records a gift code mint', async () => {
		const admin = await createAdmin([AdminACLs.GIFT_CODES_GENERATE]);

		await createBuilder(harness, `${admin.token}`)
			.post('/admin/gift-codes')
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.body({count: 3, duration_type: 'months', duration_quantity: 1})
			.expect(HTTP_STATUS.OK)
			.execute();

		const log = await findAuditLog('generate_gift_codes');
		expect(log).toBeDefined();
		expect(log?.adminUserId.toString()).toBe(admin.userId);
		expect(log?.targetType).toBe('gift_code');
		expect(log?.auditLogReason).toBe(AUDIT_REASON);
		expect(Object.fromEntries(log!.metadata)).toEqual({
			count: '3',
			duration_type: 'months',
			duration_quantity: '1',
		});
	});

	test('records a bulk discovery listing move with its failure count', async () => {
		const admin = await createAdmin([AdminACLs.DISCOVERY_REVIEW]);

		const result = await createBuilder<{
			updated: number;
			failed_guild_ids: Array<string>;
		}>(harness, `${admin.token}`)
			.patch('/admin/discovery/listings')
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.body({guild_ids: [TEST_IDS.NONEXISTENT_GUILD], category_type: DiscoveryCategories.EDUCATION})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(result.failed_guild_ids).toEqual([TEST_IDS.NONEXISTENT_GUILD]);
		const log = await findAuditLog('update_discovery_categories');
		expect(log).toBeDefined();
		expect(log?.adminUserId.toString()).toBe(admin.userId);
		expect(log?.targetType).toBe('guild');
		expect(log?.auditLogReason).toBe(AUDIT_REASON);
		expect(Object.fromEntries(log!.metadata)).toEqual({
			category_type: DiscoveryCategories.EDUCATION.toString(),
			guild_count: '1',
			updated: '0',
			failed: '1',
		});
	});
});
