// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {
	clearTestEmails,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

function createUniqueTestIp(): string {
	return `198.51.${randomInt(0, 256)}.${randomInt(1, 255)}`;
}

describe('Admin Deletion Queue', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('admin scheduling queues deletion and rescheduling replaces the queued deletion', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const firstSchedule = await createBuilder<{
			user: {
				pending_deletion_at: string;
			};
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({days_until_deletion: 60, basis: {type: 'holder_request'}})
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		await createBuilder<{
			user: {
				pending_deletion_at: string;
			};
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({
				days_until_deletion: 62,
				basis: {type: 'holder_request'},
				replace_pending_deletion_at: firstSchedule.user.pending_deletion_at,
			})
			.execute();

		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
	});
	test('cancel deletion clears the queued KV entry', async () => {
		const adminIp = createUniqueTestIp();
		const targetIp = createUniqueTestIp();
		const admin = await createTestAccount(harness, {ipAddress: adminIp});
		const targetUser = await createTestAccount(harness, {ipAddress: targetIp});
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const schedule = await createBuilder<{
			user: {
				pending_deletion_at: string;
			};
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({days_until_deletion: 60, basis: {type: 'holder_request'}})
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		await createBuilder<{
			user: {
				pending_deletion_at: string | null;
			};
		}>(harness, `${admin.token}`)
			.delete(`/admin/users/${targetUser.userId}/deletion`)
			.header('x-forwarded-for', adminIp)
			.body({expected_pending_deletion_at: schedule.user.pending_deletion_at})
			.execute();
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
	});
	test('rejects a closure without a basis', async () => {
		const admin = await createTestAccount(harness);
		const targetUser = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
		const {json} = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
			}>;
		}>(harness, `${admin.token}`)
			.put(`/admin/users/${targetUser.userId}/deletion`)
			.body({reason_code: DeletionReasons.SPAM, days_until_deletion: 60})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors.some((entry) => entry.path === 'basis')).toBe(true);
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
	});
	test('rejects a bulk schedule_user_deletion job in the old shape', async () => {
		const admin = await createTestAccount(harness);
		const targetUser = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:delete:users']);
		const {json} = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
			}>;
		}>(harness, `${admin.token}`)
			.post('/admin/bulk-jobs')
			.body({
				task: 'schedule_user_deletion',
				user_ids: [targetUser.userId],
				reason_code: DeletionReasons.SPAM,
				days_until_deletion: 60,
			})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors.some((entry) => entry.path === 'basis')).toBe(true);
	});

	describe('naming the deletion', () => {
		interface ScheduledUser {
			user: {
				pending_deletion_at: string | null;
				deletion_scheduled_by: string | null;
				deletion_scheduled_at: string | null;
				deletion_audit_log_reason: string | null;
			};
		}

		async function scheduleAs(admin: TestAccount, target: TestAccount, reason: string) {
			return createBuilder<ScheduledUser>(harness, admin.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.header('X-Audit-Log-Reason', reason)
				.body({days_until_deletion: 60, basis: {type: 'holder_request'}})
				.expect(HTTP_STATUS.OK)
				.execute();
		}

		async function createAdmin(acls: Array<string> = []): Promise<TestAccount> {
			return setUserACLs(harness, await createTestAccount(harness), ['admin:authenticate', 'user:delete', ...acls]);
		}

		test('records who scheduled the deletion and no longer stores the internal note on the account', async () => {
			const admin = await createAdmin(['audit_log:view']);
			const target = await createTestAccount(harness);
			const scheduled = await scheduleAs(admin, target, 'Batch review');
			expect(scheduled.user.deletion_scheduled_by).toBe(admin.userId);
			expect(scheduled.user.deletion_scheduled_at).not.toBeNull();
			expect(scheduled.user.deletion_audit_log_reason).toBeNull();
			const other = await createAdmin(['user:lookup']);
			const viewed = await createBuilder<{users: Array<ScheduledUser['user']>}>(harness, other.token)
				.get(`/admin/users/${target.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(viewed.users[0]?.deletion_scheduled_by).toBe(admin.userId);
			expect(viewed.users[0]?.deletion_audit_log_reason).toBeNull();
		});

		test('a second schedule without naming the pending deletion is refused and keeps it', async () => {
			const first = await createAdmin();
			const second = await createAdmin();
			const target = await createTestAccount(harness);
			const scheduled = await scheduleAs(first, target, 'First review');
			await createBuilder(harness, second.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.body({days_until_deletion: 60, basis: {type: 'holder_request'}})
				.expect(HTTP_STATUS.CONFLICT)
				.execute();
			await createBuilder(harness, first.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.body({days_until_deletion: 60, basis: {type: 'housekeeping'}})
				.expect(HTTP_STATUS.CONFLICT)
				.execute();
			const user = await new UserRepository().findUnique(createUserID(BigInt(target.userId)));
			expect(user?.pendingDeletionAt?.toISOString()).toBe(scheduled.user.pending_deletion_at);
			expect(user?.deletionScheduledBy?.toString()).toBe(first.userId);
		});

		test('replacing a named deletion records the deletion it replaced', async () => {
			const first = await createAdmin();
			const second = await createAdmin();
			const target = await createTestAccount(harness);
			const scheduled = await scheduleAs(first, target, 'First review');
			const replaced = await createBuilder<ScheduledUser>(harness, second.token)
				.put(`/admin/users/${target.userId}/deletion`)
				.body({
					days_until_deletion: 90,
					basis: {type: 'housekeeping'},
					replace_pending_deletion_at: scheduled.user.pending_deletion_at,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(replaced.user.deletion_scheduled_by).toBe(second.userId);
			const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
			const log = logs.find(
				(entry) =>
					entry.action === 'schedule_deletion' &&
					entry.targetId.toString() === target.userId &&
					entry.adminUserId.toString() === second.userId,
			);
			expect(log?.metadata.get('replaced_pending_deletion_at')).toBe(scheduled.user.pending_deletion_at);
			expect(log?.metadata.get('replaced_scheduled_by')).toBe(first.userId);
			expect(log?.metadata.get('replaced_reason_code')).toBe(DeletionReasons.USER_REQUESTED.toString());
			expect(log?.metadata.get('reason_code')).toBe(DeletionReasons.INACTIVITY.toString());
			expect(log?.metadata.get('pending_deletion_at')).toBe(replaced.user.pending_deletion_at);
		});

		test('cancel requires the pending deletion it cancels', async () => {
			const admin = await createAdmin();
			const target = await createTestAccount(harness);
			await scheduleAs(admin, target, 'Review');
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/deletion`)
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/deletion`)
				.body({expected_pending_deletion_at: new Date(Date.now() + 86_400_000).toISOString()})
				.expect(HTTP_STATUS.CONFLICT)
				.execute();
			const user = await new UserRepository().findUnique(createUserID(BigInt(target.userId)));
			expect(user?.pendingDeletionAt).not.toBeNull();
			expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		});

		test('cancel refuses when nothing is pending', async () => {
			const admin = await createAdmin();
			const target = await createTestAccount(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/deletion`)
				.body({expected_pending_deletion_at: new Date().toISOString()})
				.expect(HTTP_STATUS.BAD_REQUEST, 'NO_PENDING_DELETION')
				.execute();
		});

		test('cancel records whose deletion it cancelled and always tells the holder', async () => {
			const scheduler = await createAdmin();
			const canceller = await createAdmin();
			const target = await createTestAccount(harness);
			const scheduled = await scheduleAs(scheduler, target, 'Review');
			await clearTestEmails(harness);
			await createBuilder(harness, canceller.token)
				.delete(`/admin/users/${target.userId}/deletion`)
				.header('X-Audit-Log-Reason', 'Private cancel note')
				.body({expected_pending_deletion_at: scheduled.user.pending_deletion_at})
				.expect(HTTP_STATUS.OK)
				.execute();
			const emails = await listTestEmails(harness, {recipient: target.email});
			const email = findLastTestEmail(emails, 'account_deletion_cancelled');
			expect(email).not.toBeNull();
			expect(JSON.stringify(email)).not.toContain('Private cancel note');
			expect(findLastTestEmail(emails, 'enforcement_ended')).toBeNull();
			const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
			const log = logs.find(
				(entry) => entry.action === 'cancel_deletion' && entry.targetId.toString() === target.userId,
			);
			expect(log?.metadata.get('cancelled_pending_deletion_at')).toBe(scheduled.user.pending_deletion_at);
			expect(log?.metadata.get('cancelled_scheduled_by')).toBe(scheduler.userId);
			expect(log?.metadata.get('cancelled_reason_code')).toBe(DeletionReasons.USER_REQUESTED.toString());
			expect(log?.metadata.get('notice')).toBe('account_deletion_cancelled');
		});
	});
});
