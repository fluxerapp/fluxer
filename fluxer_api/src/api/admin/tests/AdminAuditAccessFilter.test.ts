// SPDX-License-Identifier: AGPL-3.0-or-later

import {ADMIN_AUDIT_READ_ACTIONS, getAdminAuditAccess} from '@app/api/admin/AdminAuditActions';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {getAuditLogSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {AuditLogsListResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterEach, describe, expect, test} from 'vitest';

const TARGET_USER_ID = 910000000000000001n;

describe('Admin audit log access filter', () => {
	let harness: ApiTestHarness | undefined;

	afterEach(async () => {
		await harness?.shutdown();
		harness = undefined;
	});

	async function createAuditViewer(activeHarness: ApiTestHarness): Promise<TestAccount> {
		return setUserACLs(activeHarness, await createTestAccount(activeHarness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.AUDIT_LOG_VIEW,
		]);
	}

	async function seedEntry(admin: TestAccount, logId: bigint, action: string): Promise<void> {
		const log = await getAdminRepository().createAuditLog({
			log_id: logId,
			admin_user_id: createUserID(BigInt(admin.userId)),
			target_type: 'user',
			target_id: TARGET_USER_ID,
			action,
			audit_log_reason: null,
			metadata: new Map(),
			created_at: new Date(),
		});
		await getAuditLogSearchService()?.indexAuditLog(log);
	}

	async function listActions(
		activeHarness: ApiTestHarness,
		admin: TestAccount,
		query: string,
	): Promise<Array<{action: string; access: string}>> {
		const result = await createBuilder<AuditLogsListResponse>(activeHarness, admin.token)
			.get(`/admin/audit-logs?target_type=user&target_id=${TARGET_USER_ID}${query}`)
			.execute();
		return result.logs
			.map((log) => ({action: log.action, access: log.access}))
			.sort((a, b) => a.action.localeCompare(b.action));
	}

	test('classifies every registered read action as a read and anything else as a write', () => {
		expect(ADMIN_AUDIT_READ_ACTIONS.length).toBe(new Set(ADMIN_AUDIT_READ_ACTIONS).size);
		for (const action of ADMIN_AUDIT_READ_ACTIONS) {
			expect(getAdminAuditAccess(action)).toBe('read');
		}
		expect(getAdminAuditAccess('update_flags')).toBe('write');
		expect(getAdminAuditAccess('NCMEC Report')).toBe('write');
	});

	for (const search of ['disabled', 'enabled'] as const) {
		test(`filters entries by access with search ${search}`, async () => {
			harness = await createApiTestHarness({search});
			const admin = await createAuditViewer(harness);
			await seedEntry(admin, 910000000000000011n, 'get_user');
			await seedEntry(admin, 910000000000000012n, 'list_user_sessions');
			await seedEntry(admin, 910000000000000013n, 'update_flags');
			await seedEntry(admin, 910000000000000014n, 'temp_ban');

			expect(await listActions(harness, admin, '')).toEqual([
				{action: 'get_user', access: 'read'},
				{action: 'list_user_sessions', access: 'read'},
				{action: 'temp_ban', access: 'write'},
				{action: 'update_flags', access: 'write'},
			]);
			expect(await listActions(harness, admin, '&access=read')).toEqual([
				{action: 'get_user', access: 'read'},
				{action: 'list_user_sessions', access: 'read'},
			]);
			expect(await listActions(harness, admin, '&access=write')).toEqual([
				{action: 'temp_ban', access: 'write'},
				{action: 'update_flags', access: 'write'},
			]);
		});
	}

	test('filters a full-text search by access', async () => {
		harness = await createApiTestHarness({search: 'enabled'});
		const admin = await createAuditViewer(harness);
		await seedEntry(admin, 910000000000000021n, 'list_user_sessions');
		await seedEntry(admin, 910000000000000022n, 'terminate_sessions');

		expect(await listActions(harness, admin, '&q=sessions&access=write')).toEqual([
			{action: 'terminate_sessions', access: 'write'},
		]);
		expect(await listActions(harness, admin, '&q=sessions&access=read')).toEqual([
			{action: 'list_user_sessions', access: 'read'},
		]);
	});

	test('rejects an unknown access value', async () => {
		harness = await createApiTestHarness();
		const admin = await createAuditViewer(harness);
		await createBuilder(harness, admin.token).get('/admin/audit-logs?access=delete').expect(400).execute();
	});
});
