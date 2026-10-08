// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {decodeHeaderUtf8} from '@app/api/middleware/AuditLogMiddleware';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

function asReceivedHeader(value: string): string {
	return Buffer.from(value, 'utf8').toString('latin1');
}

async function auditLogsFor(targetUserId: string, action: string): Promise<Array<AdminAuditLog>> {
	const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
	return logs.filter((log) => log.action === action && log.targetId.toString() === targetUserId);
}

describe('decodeHeaderUtf8', () => {
	test('reads UTF-8 bytes that arrived as a latin1 string', () => {
		expect(decodeHeaderUtf8(asReceivedHeader('§ 4.2 räksmörgås 日本'))).toBe('§ 4.2 räksmörgås 日本');
	});

	test('keeps ASCII and latin1 text that is not UTF-8', () => {
		expect(decodeHeaderUtf8('Batch 12')).toBe('Batch 12');
		expect(decodeHeaderUtf8('café')).toBe('café');
	});
});

describe('Admin ban reasons and notes', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await setUserACLs(harness, await createTestAccount(harness), ['admin:authenticate', 'user:temp_ban']);
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	async function ban(target: TestAccount, reason: string): Promise<AdminAuditLog> {
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/ban`)
			.header('X-Audit-Log-Reason', asReceivedHeader(reason))
			.body({duration_hours: 24})
			.expect(HTTP_STATUS.OK)
			.execute();
		const [log] = await auditLogsFor(target.userId, 'temp_ban');
		return log!;
	}

	test('stores a UTF-8 ban reason as sent', async () => {
		const target = await createTestAccount(harness);
		const log = await ban(target, '§ 3 Regel – wiederholt');
		expect(log.auditLogReason).toBe('§ 3 Regel – wiederholt');
	});

	test('appends a note to the current ban without changing the ban entry', async () => {
		const target = await createTestAccount(harness);
		const banLog = await ban(target, 'Original reason');
		await createBuilder(harness, admin.token)
			.post(`/admin/users/${target.userId}/ban/notes`)
			.body({ban_audit_log_id: banLog.logId.toString(), note: 'Also sent § 4 links'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const [note] = await auditLogsFor(target.userId, 'annotate_ban');
		expect(note?.auditLogReason).toBe('Also sent § 4 links');
		expect(note?.adminUserId.toString()).toBe(admin.userId);
		expect(Object.fromEntries(note!.metadata)).toEqual({ban_audit_log_id: banLog.logId.toString()});
		const [unchanged] = await auditLogsFor(target.userId, 'temp_ban');
		expect(unchanged?.auditLogReason).toBe('Original reason');
	});

	test('refuses a note that names another user ban', async () => {
		const target = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		await ban(target, 'Target ban');
		const otherBan = await ban(other, 'Other ban');
		await createBuilder(harness, admin.token)
			.post(`/admin/users/${target.userId}/ban/notes`)
			.body({ban_audit_log_id: otherBan.logId.toString(), note: 'Wrong ban'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(await auditLogsFor(target.userId, 'annotate_ban')).toHaveLength(0);
	});

	test('refuses a note once the ban has been lifted', async () => {
		const target = await createTestAccount(harness);
		const banLog = await ban(target, 'Lifted ban');
		await createBuilder(harness, admin.token)
			.delete(`/admin/users/${target.userId}/ban`)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, admin.token)
			.post(`/admin/users/${target.userId}/ban/notes`)
			.body({ban_audit_log_id: banLog.logId.toString(), note: 'Too late'})
			.expect(HTTP_STATUS.CONFLICT)
			.execute();
	});
});
