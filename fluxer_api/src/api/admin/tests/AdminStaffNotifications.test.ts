// SPDX-License-Identifier: AGPL-3.0-or-later

import {scheduledDeletionEmailTemplate} from '@app/api/admin/services/AdminUserDeletionService';
import {
	clearTestEmails,
	createTestAccount,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getAdminRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const DELETION_TEMPLATES = [
	'account_deletion_scheduled_requested',
	'account_deletion_scheduled_inactivity',
	'scheduled_deletion_notification',
	'account_scheduled_deletion',
];

const NON_ENFORCEMENT_TEMPLATES: Record<number, string> = {
	1: 'account_deletion_scheduled_requested',
	2: 'scheduled_deletion_notification',
	19: 'account_deletion_scheduled_inactivity',
};

describe('scheduledDeletionEmailTemplate', () => {
	test.each(Object.entries(NON_ENFORCEMENT_TEMPLATES))('code %s picks %s', (code, template) => {
		expect(scheduledDeletionEmailTemplate(Number(code))).toBe(template);
	});

	test.each(Object.entries(DeletionReasons).filter(([, code]) => !(code in NON_ENFORCEMENT_TEMPLATES)))(
		'%s picks the enforcement template',
		(_name, code) => {
			expect(scheduledDeletionEmailTemplate(code)).toBe('account_scheduled_deletion');
		},
	);
});

describe('Admin staff notifications', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function auditMetadata(action: string, targetId: string): Promise<Map<string, string> | undefined> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(1000);
		return logs.find((log) => log.action === action && log.targetId.toString() === targetId)?.metadata;
	}

	async function emailsTo(account: TestAccount) {
		return listTestEmails(harness, {recipient: account.email});
	}

	async function systemDmMessages(account: TestAccount): Promise<Array<{content: string | null}>> {
		const channels = await createBuilder<Array<{id: string; recipients?: Array<{id: string}>}>>(harness, account.token)
			.get('/users/@me/channels')
			.expect(HTTP_STATUS.OK)
			.execute();
		const systemChannels = channels.filter((channel) => channel.recipients?.some((recipient) => recipient.id === '0'));
		const messages = await Promise.all(
			systemChannels.map((channel) =>
				createBuilder<Array<{content: string | null}>>(harness, account.token)
					.get(`/channels/${channel.id}/messages?limit=50`)
					.expect(HTTP_STATUS.OK)
					.execute(),
			),
		);
		return messages.flat();
	}

	async function expireTempBan(target: TestAccount) {
		const users = getUserRepository();
		const user = (await users.findUnique(createUserID(BigInt(target.userId))))!;
		await users.patchUpsert(user.id, {temp_banned_until: new Date(Date.now() - 3_600_000)}, user.toRow());
	}

	async function scheduleDeletion(target: TestAccount, body: Record<string, unknown>) {
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/deletion`)
			.body({days_until_deletion: 60, ...body})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function tempBan(target: TestAccount, body: Record<string, unknown>) {
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/ban`)
			.body(body)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	describe('schedule deletion', () => {
		test.each([
			[DeletionReasons.USER_REQUESTED, 'account_deletion_scheduled_requested'],
			[DeletionReasons.INACTIVITY, 'account_deletion_scheduled_inactivity'],
			[DeletionReasons.OTHER, 'scheduled_deletion_notification'],
			[DeletionReasons.SPAM, 'account_scheduled_deletion'],
		])('reason code %s sends %s', async (reasonCode, template) => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await scheduleDeletion(target, {reason_code: reasonCode, public_reason: 'Shown to the user'});
			const sent = (await emailsTo(target)).filter((email) => DELETION_TEMPLATES.includes(email.type));
			expect(sent.map((email) => email.type)).toEqual([template]);
			expect(sent[0]?.metadata.reason).toBe('Shown to the user');
			const metadata = await auditMetadata('schedule_deletion', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('true');
			expect(metadata?.get('notification_template')).toBe(template);
		});

		test('notify_user false sends nothing and records it', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await scheduleDeletion(target, {reason_code: DeletionReasons.SPAM, notify_user: false});
			expect((await emailsTo(target)).filter((email) => DELETION_TEMPLATES.includes(email.type))).toEqual([]);
			const metadata = await auditMetadata('schedule_deletion', target.userId);
			expect(metadata?.get('notify_user')).toBe('false');
			expect(metadata?.get('notification_sent')).toBe('false');
			expect(metadata?.has('notification_template')).toBe(false);
		});
	});

	describe('temp ban', () => {
		test('emails by default', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await tempBan(target, {duration_hours: 24, reason: 'Spam'});
			expect((await emailsTo(target)).map((email) => email.type)).toContain('account_temp_banned');
			const metadata = await auditMetadata('temp_ban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('true');
		});

		test('notify_user false sends nothing', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('account_temp_banned');
			const metadata = await auditMetadata('temp_ban', target.userId);
			expect(metadata?.get('notify_user')).toBe('false');
			expect(metadata?.get('notification_sent')).toBe('false');
		});

		test('a permanent ban sends nothing', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await tempBan(target, {duration_hours: 0});
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('account_temp_banned');
			const metadata = await auditMetadata('temp_ban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('false');
		});
	});

	describe('unban', () => {
		test('emails the public reason and never the audit log reason', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.header('X-Audit-Log-Reason', 'Private staff note')
				.body({public_reason: 'Appeal accepted'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const email = (await emailsTo(target)).find((entry) => entry.type === 'unban_notification');
			expect(email?.metadata.reason).toBe('Appeal accepted');
			expect(JSON.stringify(email)).not.toContain('Private staff note');
			const metadata = await auditMetadata('unban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('true');
			expect(metadata?.get('public_reason')).toBe('Appeal accepted');
		});

		test('accepts a request without a body', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.header('X-Audit-Log-Reason', 'Private staff note')
				.expect(HTTP_STATUS.OK)
				.execute();
			const email = (await emailsTo(target)).find((entry) => entry.type === 'unban_notification');
			expect(email?.metadata.reason).toBe('');
			expect(JSON.stringify(email)).not.toContain('Private staff note');
		});

		test('notify_user false sends nothing', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.body({notify_user: false})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			const metadata = await auditMetadata('unban', target.userId);
			expect(metadata?.get('notify_user')).toBe('false');
			expect(metadata?.get('notification_sent')).toBe('false');
		});

		test('a temp ban that already expired is not emailed', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await expireTempBan(target);
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			const metadata = await auditMetadata('unban', target.userId);
			expect(metadata?.get('notify_user')).toBe('true');
			expect(metadata?.get('notification_sent')).toBe('false');
		});

		test('an account pending deletion is not told it can log back in', async () => {
			const target = await createTestAccount(harness);
			await tempBan(target, {duration_hours: 24, notify_user: false});
			await scheduleDeletion(target, {reason_code: DeletionReasons.SPAM, notify_user: false});
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			expect((await auditMetadata('unban', target.userId))?.get('notification_sent')).toBe('false');
		});

		test('a user who was never banned is not emailed', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${target.userId}/ban`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(target)).map((email) => email.type)).not.toContain('unban_notification');
			expect((await auditMetadata('unban', target.userId))?.get('notification_sent')).toBe('false');
		});
	});

	describe('report resolve', () => {
		async function fileReport(): Promise<{reporter: TestAccount; reportId: string}> {
			const reporter = await createTestAccount(harness);
			const reported = await createTestAccount(harness);
			const report = await createBuilder<{report_id: string}>(harness, reporter.token)
				.post('/reports/user')
				.body({user_id: reported.userId, category: 'harassment'})
				.execute();
			return {reporter, reportId: report.report_id};
		}

		test('notifies the reporter by default', async () => {
			const {reporter, reportId} = await fileReport();
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', public_comment: 'Handled'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(reporter)).map((email) => email.type)).toContain('report_resolved');
			expect(await systemDmMessages(reporter)).toHaveLength(1);
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('notify_reporter')).toBe('true');
			expect(metadata?.get('reporter_dm_sent')).toBe('true');
			expect(metadata?.get('reporter_email_sent')).toBe('true');
		});

		test('notify_reporter false sends neither DM nor email', async () => {
			const {reporter, reportId} = await fileReport();
			await clearTestEmails(harness);
			await createBuilder(harness, admin.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', notify_reporter: false})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await emailsTo(reporter)).map((email) => email.type)).not.toContain('report_resolved');
			expect(await systemDmMessages(reporter)).toEqual([]);
			const metadata = await auditMetadata('resolve_report', reportId);
			expect(metadata?.get('notify_reporter')).toBe('false');
			expect(metadata?.get('reporter_dm_sent')).toBe('false');
			expect(metadata?.get('reporter_email_sent')).toBe('false');
		});
	});
});
