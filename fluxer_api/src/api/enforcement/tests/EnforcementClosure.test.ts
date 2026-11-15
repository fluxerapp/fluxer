// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {resolveDeletionDays} from '@app/api/admin/services/AdminUserDeletionService';
import {clearTestEmails, createTestAccount, loginAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {createNoticeLookupDeps, loadAccountNotice} from '@app/api/enforcement/EnforcementAccountNotice';
import {formatNoticeDate} from '@app/api/enforcement/NoticeDates';
import {
	close,
	closeRequest,
	createStaff,
	decisionBody,
	emailsTo,
	endDecision,
	getDecision,
	listDecisions,
	loadUser,
	suspend,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import type {Event} from '@app/api/infrastructure/activity/Contract.generated';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

class CapturingPublisher implements ActivityPublisher {
	readonly events: Array<Event> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.events.push(JSON.parse(payload) as Event);
	}

	adminActions(targetId: string): Array<Extract<Event, {kind: 'admin_action'}>['data']> {
		return this.events
			.filter((event): event is Extract<Event, {kind: 'admin_action'}> => event.kind === 'admin_action')
			.filter((event) => event.data.target_id === targetId)
			.map((event) => event.data);
	}
}

function daysFromNow(iso: string | null): number {
	return Math.round((new Date(iso!).getTime() - Date.now()) / ms('1 day'));
}

describe('Enforcement closures', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;
	let publisher: CapturingPublisher;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		staff = await createStaff(harness);
		publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
	});

	afterEach(() => {
		resetActivityEventsForTests();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function isEmailBanned(account: TestAccount): Promise<boolean> {
		return new AdminRepository().isEmailBanned(account.email);
	}

	describe('holder request', () => {
		test('writes the self-deletion state with code 1 and at least 14 days, with no decision', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			const {user} = await closeRequest(harness, staff, target.userId, {
				days_until_deletion: 1,
				basis: {type: 'holder_request'},
			})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(daysFromNow(user.pending_deletion_at)).toBe(14);
			expect(user.enforcement_decision_id).toBeNull();
			const stored = await loadUser(target);
			expect(stored.flags & UserFlags.SELF_DELETED).toBe(UserFlags.SELF_DELETED);
			expect(stored.flags & UserFlags.DELETED).toBe(0n);
			expect(stored.deletionReasonCode).toBe(DeletionReasons.USER_REQUESTED);
			expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
			const [email] = await emailsTo(harness, target, 'account_deletion_scheduled_requested');
			expect(email?.metadata.deletion_date).toBe(formatNoticeDate(stored.pendingDeletionAt!, stored.locale));
			expect(await emailsTo(harness, target, 'enforcement_notice')).toEqual([]);
			expect(await isEmailBanned(target)).toBe(false);
			expect(publisher.adminActions(target.userId)).toEqual([
				expect.objectContaining({action: 'schedule_deletion', reason_code: DeletionReasons.USER_REQUESTED, ips: []}),
			]);
		});

		test('signing in cancels it', async () => {
			const target = await createTestAccount(harness);
			await closeRequest(harness, staff, target.userId, {days_until_deletion: 14, basis: {type: 'holder_request'}})
				.expect(HTTP_STATUS.OK)
				.execute();
			await loginAccount(harness, target);
			const stored = await loadUser(target);
			expect(stored.pendingDeletionAt).toBeNull();
			expect(stored.flags & UserFlags.SELF_DELETED).toBe(0n);
		});

		test('remove_evidence is refused without a broken rule', async () => {
			const target = await createTestAccount(harness);
			const {json} = await closeRequest(harness, staff, target.userId, {
				days_until_deletion: 14,
				basis: {type: 'holder_request'},
				remove_evidence: true,
			})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expect(JSON.stringify(json)).toContain(ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED);
			expect((await loadUser(target)).pendingDeletionAt).toBeNull();
		});
	});

	test('housekeeping writes code 19, sends the inactivity email and never bans the email', async () => {
		const target = await createTestAccount(harness);
		await clearTestEmails(harness);
		await closeRequest(harness, staff, target.userId, {days_until_deletion: 30, basis: {type: 'housekeeping'}})
			.expect(HTTP_STATUS.OK)
			.execute();
		const stored = await loadUser(target);
		expect(stored.deletionReasonCode).toBe(DeletionReasons.INACTIVITY);
		expect(stored.flags & UserFlags.SELF_DELETED).toBe(UserFlags.SELF_DELETED);
		expect(await emailsTo(harness, target, 'account_deletion_scheduled_inactivity')).toHaveLength(1);
		expect(await isEmailBanned(target)).toBe(false);
		expect(publisher.adminActions(target.userId)[0]?.ips).toEqual([]);
	});

	test.each([
		['holder_request', 'holder_request'],
		['housekeeping', 'housekeeping'],
	])('a %s closure is refused while a decision owns the account', async (_label, type) => {
		const target = await createTestAccount(harness);
		const decisionId = await suspend(harness, staff, target.userId);
		const response = await closeRequest(harness, staff, target.userId, {days_until_deletion: 30, basis: {type}})
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
			.execute();
		expect(response).toMatchObject({decision_id: decisionId});
		expect((await loadUser(target)).pendingDeletionAt).toBeNull();
	});

	describe('enforcement basis', () => {
		test('records a closure decision that lasts at least 74 days and bans the email', async () => {
			const target = await createTestAccount(harness);
			await clearTestEmails(harness);
			const {user} = await closeRequest(harness, staff, target.userId, {
				days_until_deletion: 14,
				basis: {type: 'enforcement', decision: decisionBody()},
			})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(daysFromNow(user.pending_deletion_at)).toBe(74);
			const stored = await loadUser(target);
			expect(stored.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(stored.flags & UserFlags.SELF_DELETED).toBe(0n);
			expect(stored.deletionReasonCode).toBe(DeletionReasons.SPAM);
			expect(stored.deletionPublicReason).toBeNull();
			expect(stored.deletionAuditLogReason).toBeNull();
			expect(stored.enforcementDecisionId?.toString()).toBe(user.enforcement_decision_id);
			const decision = await getDecision(harness, staff, user.enforcement_decision_id!);
			expect(decision).toMatchObject({kind: 'closure', status: 'active', notice_status: 'sent'});
			expect(decision.deletion_at).toBe(user.pending_deletion_at);
			expect(decision.notice_text).toContain(
				"We closed your account.\n\nYou can't sign in or use Fluxer anywhere. We'll delete the account and its data on ",
			);
			expect(decision.notice_text).toContain('You may not create or use other accounts.');
			expect(await emailsTo(harness, target, 'enforcement_notice')).toHaveLength(1);
			expect(await isEmailBanned(target)).toBe(true);
			const [action] = publisher.adminActions(target.userId);
			expect(action).toMatchObject({action: 'schedule_deletion', reason_code: DeletionReasons.SPAM});
			expect(action?.ips.length).toBeGreaterThan(0);
		});

		test('a self-hosted instance has no appeal route, so a closure lasts at least 14 days', async () => {
			const config = getConfig();
			const selfHosted = config.instance.selfHosted;
			config.instance.selfHosted = true;
			try {
				const target = await createTestAccount(harness);
				const {user} = await closeRequest(harness, staff, target.userId, {
					days_until_deletion: 3,
					basis: {type: 'enforcement', decision: decisionBody()},
				})
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(daysFromNow(user.pending_deletion_at)).toBe(14);
				const decision = await getDecision(harness, staff, user.enforcement_decision_id!);
				expect(decision).toMatchObject({kind: 'closure', status: 'active'});
				expect(decision.deletion_at).toBe(user.pending_deletion_at);
				expect((await loadUser(target)).flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			} finally {
				config.instance.selfHosted = selfHosted;
			}
		});

		test('each mode raises every basis to its own floor and keeps longer requests', () => {
			const config = getConfig();
			const selfHosted = config.instance.selfHosted;
			try {
				config.instance.selfHosted = false;
				expect(resolveDeletionDays('enforcement', 1)).toBe(74);
				expect(resolveDeletionDays('holder_request', 1)).toBe(14);
				expect(resolveDeletionDays('housekeeping', 1)).toBe(14);
				expect(resolveDeletionDays('enforcement', 90)).toBe(90);
				config.instance.selfHosted = true;
				expect(resolveDeletionDays('enforcement', 1)).toBe(14);
				expect(resolveDeletionDays('holder_request', 1)).toBe(14);
				expect(resolveDeletionDays('housekeeping', 1)).toBe(14);
				expect(resolveDeletionDays('enforcement', 90)).toBe(90);
			} finally {
				config.instance.selfHosted = selfHosted;
			}
		});

		test('a rule that does not bar new accounts neither bans the email nor sends IPs', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await close(
				harness,
				staff,
				target.userId,
				decisionBody({reason_code: DeletionReasons.UNDERAGE_ACCOUNT}),
			);
			expect(await isEmailBanned(target)).toBe(false);
			expect(publisher.adminActions(target.userId)[0]?.ips).toEqual([]);
			const decision = await getDecision(harness, staff, decisionId);
			expect(decision.notice_text).toContain("you're welcome back once you're old enough");
			expect(decision.notice_text).not.toContain('You may not create or use other accounts.');
		});

		test('replaces a suspension and the suspension reads as replaced', async () => {
			const target = await createTestAccount(harness);
			const suspensionId = await suspend(harness, staff, target.userId);
			await clearTestEmails(harness);
			const closureId = await close(harness, staff, target.userId);
			const stored = await loadUser(target);
			expect(stored.enforcementDecisionId?.toString()).toBe(closureId);
			expect(stored.tempBannedUntil).toBeNull();
			expect(stored.flags & UserFlags.DISABLED).toBe(0n);
			const decisions = await listDecisions(harness, staff, target.userId);
			expect(decisions.find((decision) => decision.decision_id === suspensionId)?.replaced).toBe(true);
			expect(await emailsTo(harness, target, 'enforcement_ended')).toEqual([]);
		});

		test('restricted reasons cannot close an account', async () => {
			const target = await createTestAccount(harness);
			await closeRequest(harness, staff, target.userId, {
				days_until_deletion: 74,
				basis: {type: 'enforcement', decision: decisionBody({reason_code: DeletionReasons.CHILD_SAFETY_VIOLATION})},
			})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect((await loadUser(target)).pendingDeletionAt).toBeNull();
		});

		test('ending it cancels the deletion, lifts the email ban and tells the holder', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await close(harness, staff, target.userId);
			await clearTestEmails(harness);
			await endDecision(harness, staff, decisionId, 'lifted').expect(HTTP_STATUS.OK).execute();
			const stored = await loadUser(target);
			expect(stored.pendingDeletionAt).toBeNull();
			expect(stored.flags & UserFlags.DELETED).toBe(0n);
			expect(stored.enforcementDecisionId).toBeNull();
			expect(stored.deletionReasonCode).toBeNull();
			expect(await isEmailBanned(target)).toBe(false);
			expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
			const [email] = await emailsTo(harness, target, 'enforcement_ended');
			expect(email?.metadata).toMatchObject({outcome: 'lifted', decision_id: decisionId, restores_access: 'yes'});
			expect(publisher.adminActions(target.userId).map((action) => action.action)).toEqual([
				'schedule_deletion',
				'cancel_deletion',
			]);
		});

		test('ending it is refused once the deletion has started', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await close(harness, staff, target.userId);
			const users = getUserRepository();
			const user = await loadUser(target);
			const due = new Date(Date.now() - ms('1 minute'));
			await users.updateDeletionSchedule(user, {
				flags: user.flags,
				pending_deletion_at: due,
				deletion_reason_code: user.deletionReasonCode,
				deletion_scheduled_by: user.deletionScheduledBy,
				deletion_scheduled_at: user.deletionScheduledAt,
			});
			expect((await users.startDeletion(user.id, due))?.deletionStartedAt).not.toBeNull();
			await endDecision(harness, staff, decisionId)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DELETION_STARTED)
				.execute();
			expect((await getDecision(harness, staff, decisionId)).status).toBe('active');
		});

		test('the legacy cancel route refuses a closure a decision owns', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await close(harness, staff, target.userId);
			const user = await loadUser(target);
			const response = await createBuilder<{decision_id: string}>(harness, staff.token)
				.delete(`/admin/users/${target.userId}/deletion`)
				.body({expected_pending_deletion_at: user.pendingDeletionAt!.toISOString()})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
				.execute();
			expect(response.decision_id).toBe(decisionId);
			expect((await loadUser(target)).pendingDeletionAt).not.toBeNull();
		});
	});

	describe('closures from before decisions', () => {
		async function legacyClose(
			target: TestAccount,
			publicReason: string | null,
			reasonCode: number = DeletionReasons.HARASSMENT_OR_BULLYING,
		) {
			const users = getUserRepository();
			const user = await loadUser(target);
			return users.updateDeletionSchedule(user, {
				flags: user.flags | UserFlags.DELETED,
				pending_deletion_at: new Date(Date.now() + ms('60 days')),
				deletion_reason_code: reasonCode,
				deletion_public_reason: publicReason,
				deletion_scheduled_at: new Date(),
			});
		}

		test('the notice shows the stored public reason for a standard reason', async () => {
			const target = await createTestAccount(harness);
			const closed = await legacyClose(target, 'Sent the same invite to many people', DeletionReasons.SPAM);
			const notice = await loadAccountNotice(createNoticeLookupDeps(), closed, 'en-US');
			expect(notice).toMatchObject({
				decision_id: null,
				kind: 'closure',
				statement: 'Sent the same invite to many people',
				facts: [],
				rule: {title: 'Spam'},
			});
		});

		test.each([DeletionReasons.HARASSMENT_OR_BULLYING, DeletionReasons.CHILD_SEXUAL_CONTENT])(
			'the notice withholds the stored public reason for code %s',
			async (code) => {
				const target = await createTestAccount(harness);
				const closed = await legacyClose(target, 'Details about another person', code);
				const notice = await loadAccountNotice(createNoticeLookupDeps(), closed, 'en-US');
				expect(notice.statement).toBeNull();
				expect(notice.facts).toEqual(["We don't have more detail on record for this decision."]);
			},
		);

		test('the legacy cancel route cancels it and sends the ended email', async () => {
			const target = await createTestAccount(harness);
			const closed = await legacyClose(target, null);
			await clearTestEmails(harness);
			await createBuilder(harness, staff.token)
				.delete(`/admin/users/${target.userId}/deletion`)
				.body({expected_pending_deletion_at: closed.pendingDeletionAt!.toISOString()})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await loadUser(target)).pendingDeletionAt).toBeNull();
			const [email] = await emailsTo(harness, target, 'enforcement_ended');
			expect(email?.metadata).toMatchObject({outcome: 'lifted', decision_id: '', restores_access: 'yes'});
			expect(await emailsTo(harness, target, 'account_deletion_cancelled')).toEqual([]);
		});
	});
});
