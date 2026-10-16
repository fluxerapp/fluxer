// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {
	createStaff,
	decisionBody,
	emailsTo,
	endDecision,
	getDecision,
	INTERNAL_NOTE,
	limitFeatures,
	limitRequest,
	loadUser,
	suspend,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {EnforcementDecisions} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ms} from 'itty-time';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const REPORT_ABUSE = decisionBody({
	reason_code: DeletionReasons.REPORTING_ABUSE,
	statement: 'You sent many reports about the same person that were not true.',
});
const PAYMENTS = decisionBody({
	reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
	statement: 'You disputed several charges you had made yourself.',
});

describe('Enforcement feature limits', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		staff = await createStaff(harness);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function reportUser(reporter: TestAccount, target: TestAccount) {
		return createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/user')
			.body({user_id: target.userId, category: 'harassment'});
	}

	async function expireDecision(decisionId: string) {
		await upsertOne(
			EnforcementDecisions.patchByPk(
				{decision_id: BigInt(decisionId)},
				{ends_at: Db.set(new Date(Date.now() - ms('1 hour')))},
			),
		);
	}

	function patchFlags(target: TestAccount, body: Record<string, Array<string | number>>, path = 'flags') {
		return createBuilder<{decision_id?: string}>(harness, staff.token)
			.patch(`/admin/users/${target.userId}/${path}`)
			.body(body);
	}

	test('a direct message limit records one limit, sets the spammer flag and notifies by email and DM', async () => {
		const target = await createTestAccount(harness);
		await clearTestEmails(harness);
		const decisionId = await limitFeatures(harness, staff, target.userId, 'direct_messages');
		expect((await loadUser(target)).flags & UserFlags.SPAMMER).toBe(UserFlags.SPAMMER);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision).toMatchObject({kind: 'feature_limit', limit: 'direct_messages', ends_at: null});
		expect(decision.notice_channels).toEqual(['email', 'system_dm']);
		expect(decision.notice_text).toContain('We limited a feature on your account.');
		expect(decision.notice_text).toContain('Your one-to-one messages and friend requests are not delivered');
		const [email] = await emailsTo(harness, target, 'enforcement_notice');
		expect(email?.metadata.kind_label).toBe('feature_limit');
	});

	test.each([
		['harassment', DeletionReasons.HARASSMENT_OR_BULLYING, ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED],
		['misuse of reporting', DeletionReasons.REPORTING_ABUSE, ValidationErrorCodes.ENFORCEMENT_LIMIT_NOT_ALLOWED],
	])('a direct message limit is refused for %s', async (_label, reasonCode, errorCode) => {
		const target = await createTestAccount(harness);
		const {json} = await limitRequest(harness, staff, target.userId, {
			limit: 'direct_messages',
			decision: decisionBody({reason_code: reasonCode}),
		})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		expect(JSON.stringify(json)).toContain(errorCode);
		expect((await loadUser(target)).flags & UserFlags.SPAMMER).toBe(0n);
	});

	describe('reports', () => {
		test('ends after 90 days by default and refuses an end beyond 365 days', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await limitFeatures(harness, staff, target.userId, 'reports', REPORT_ABUSE);
			const decision = await getDecision(harness, staff, decisionId);
			expect(Math.round((new Date(decision.ends_at!).getTime() - Date.now()) / ms('1 day'))).toBe(90);
			expect(decision.notice_text).toContain('use the safety route on our company information page');
			const {json} = await limitRequest(harness, staff, target.userId, {
				limit: 'reports',
				ends_at: new Date(Date.now() + ms('400 days')).toISOString(),
				decision: REPORT_ABUSE,
			})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expect(JSON.stringify(json)).toContain(ValidationErrorCodes.ENFORCEMENT_LIMIT_END_INVALID);
		});

		test('an end date is refused for other limits', async () => {
			const target = await createTestAccount(harness);
			const {json} = await limitRequest(harness, staff, target.userId, {
				limit: 'purchases',
				ends_at: new Date(Date.now() + ms('30 days')).toISOString(),
				decision: PAYMENTS,
			})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expect(JSON.stringify(json)).toContain(ValidationErrorCodes.ENFORCEMENT_LIMIT_END_INVALID);
		});

		test('refuses reports while active and expires lazily at the next report', async () => {
			const reporter = await createTestAccount(harness);
			const reported = await createTestAccount(harness);
			const decisionId = await limitFeatures(harness, staff, reporter.userId, 'reports', REPORT_ABUSE);
			await reportUser(reporter, reported).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REPORT_BANNED).execute();
			await expireDecision(decisionId);
			expect((await getDecision(harness, staff, decisionId)).expired).toBe(true);
			await reportUser(reporter, reported).expect(HTTP_STATUS.OK).execute();
			expect((await loadUser(reporter)).flags & UserFlags.REPORT_BANNED).toBe(0n);
		});

		test('a flag set before decisions existed keeps refusing reports', async () => {
			const reporter = await createTestAccount(harness);
			const reported = await createTestAccount(harness);
			await getUserRepository().updateFlags((await loadUser(reporter)).id, (flags) => flags | UserFlags.REPORT_BANNED);
			await reportUser(reporter, reported).expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REPORT_BANNED).execute();
		});

		test('a limited reporter still hears the outcome of reports filed before the limit', async () => {
			const reporter = await createTestAccount(harness);
			const reported = await createTestAccount(harness);
			const {report_id: reportId} = await reportUser(reporter, reported).expect(HTTP_STATUS.OK).execute();
			await limitFeatures(harness, staff, reporter.userId, 'reports', REPORT_ABUSE);
			await clearTestEmails(harness);
			await createBuilder(harness, staff.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', resolution: 'no_violation', public_comment: 'Handled'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await emailsTo(harness, reporter, 'report_resolved')).toHaveLength(1);
		});
	});

	test('a purchase limit sets the purchase flag and ending it clears the flag', async () => {
		const target = await createTestAccount(harness);
		const decisionId = await limitFeatures(harness, staff, target.userId, 'purchases', PAYMENTS);
		expect((await loadUser(target)).premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
		await clearTestEmails(harness);
		await endDecision(harness, staff, decisionId, 'lifted').expect(HTTP_STATUS.OK).execute();
		expect((await loadUser(target)).premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
		const [email] = await emailsTo(harness, target, 'enforcement_ended');
		expect(email?.metadata).toMatchObject({outcome: 'lifted', restores_access: 'no'});
	});

	test('ending one limit keeps the flag while another decision holds the same limit', async () => {
		const target = await createTestAccount(harness);
		const first = await limitFeatures(harness, staff, target.userId, 'direct_messages');
		const second = await limitFeatures(
			harness,
			staff,
			target.userId,
			'direct_messages',
			decisionBody({reason_code: DeletionReasons.MALICIOUS_LINKS_OR_MALWARE}),
		);
		await endDecision(harness, staff, first).expect(HTTP_STATUS.OK).execute();
		expect((await loadUser(target)).flags & UserFlags.SPAMMER).toBe(UserFlags.SPAMMER);
		await endDecision(harness, staff, second).expect(HTTP_STATUS.OK).execute();
		expect((await loadUser(target)).flags & UserFlags.SPAMMER).toBe(0n);
	});

	describe('flag route guards', () => {
		test.each([
			['DISABLED', UserFlags.DISABLED],
			['DELETED', UserFlags.DELETED],
			['SPAMMER', UserFlags.SPAMMER],
			['REPORT_BANNED', UserFlags.REPORT_BANNED],
		])('adding %s is refused', async (_label, flag) => {
			const target = await createTestAccount(harness);
			await patchFlags(target, {add_flags: [flag.toString()]})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect((await loadUser(target)).flags & flag).toBe(0n);
		});

		test('adding the purchase flag is refused', async () => {
			const target = await createTestAccount(harness);
			await patchFlags(target, {add_flags: [PremiumFlags.PURCHASE_DISABLED]}, 'premium-flags')
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('removing a flag a decision holds is refused with the decision', async () => {
			const target = await createTestAccount(harness);
			const spam = await limitFeatures(harness, staff, target.userId, 'direct_messages');
			const purchases = await limitFeatures(harness, staff, target.userId, 'purchases', PAYMENTS);
			const spamResponse = await patchFlags(target, {remove_flags: [UserFlags.SPAMMER.toString()]})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
				.execute();
			expect(spamResponse.decision_id).toBe(spam);
			const purchaseResponse = await patchFlags(
				target,
				{remove_flags: [PremiumFlags.PURCHASE_DISABLED]},
				'premium-flags',
			)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
				.execute();
			expect(purchaseResponse.decision_id).toBe(purchases);
		});

		test('removing DISABLED from a suspended account and removing DELETED are refused', async () => {
			const target = await createTestAccount(harness);
			await suspend(harness, staff, target.userId);
			await patchFlags(target, {remove_flags: [UserFlags.DISABLED.toString()]})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			const other = await createTestAccount(harness);
			await getUserRepository().updateFlags((await loadUser(other)).id, (flags) => flags | UserFlags.DELETED);
			await patchFlags(other, {remove_flags: [UserFlags.DELETED.toString()]})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		test('a self-disabled account can still have DISABLED removed', async () => {
			const target = await createTestAccount(harness);
			await getUserRepository().updateFlags((await loadUser(target)).id, (flags) => flags | UserFlags.DISABLED);
			await patchFlags(target, {remove_flags: [UserFlags.DISABLED.toString()]})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await loadUser(target)).flags & UserFlags.DISABLED).toBe(0n);
		});

		test('a bulk flag job cannot add a restriction flag', async () => {
			const target = await createTestAccount(harness);
			await createBuilder(harness, staff.token)
				.post('/admin/bulk-jobs')
				.header('X-Audit-Log-Reason', INTERNAL_NOTE)
				.body({
					task: 'update_user_flags',
					user_ids: [target.userId],
					add_flags: [UserFlags.SPAMMER.toString()],
					remove_flags: [],
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
});
