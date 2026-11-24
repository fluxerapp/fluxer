// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {clearTestEmails, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	close,
	createStaff,
	type DecisionBody,
	decisionBody,
	emailsTo,
	endDecision,
	getDecision,
	limitFeatures,
	listDecisions,
	loadUser,
	suspend,
	warn,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {EnforcementDecisionResponse} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const BAN_EVASION = decisionBody({
	reason_code: DeletionReasons.BAN_EVASION,
	statement: 'This account belongs to someone we banned before.',
});

describe('Changing the reason for an enforcement decision', () => {
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

	function supersedeRequest(decisionId: string, decision: DecisionBody, actor: TestAccount = staff) {
		return createBuilder<EnforcementDecisionResponse>(harness, actor.token)
			.post(`/admin/enforcement-decisions/${decisionId}/supersede`)
			.header('X-Audit-Log-Reason', 'The account is a ban evasion, not hate')
			.body({decision});
	}

	function supersede(decisionId: string, decision: DecisionBody = BAN_EVASION) {
		return supersedeRequest(decisionId, decision).expect(HTTP_STATUS.OK).execute();
	}

	async function refused(decisionId: string, decision: DecisionBody): Promise<string> {
		const {json} = await supersedeRequest(decisionId, decision).expect(HTTP_STATUS.BAD_REQUEST).executeWithResponse();
		return JSON.stringify(json);
	}

	describe('a closure', () => {
		async function closeForHate(target: TestAccount): Promise<string> {
			return close(
				harness,
				staff,
				target.userId,
				decisionBody({
					reason_code: DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT,
					statement: 'You posted slurs in many communities.',
				}),
			);
		}

		test('moves to a new decision without lifting the closure, and sends one updated notice', async () => {
			const target = await createTestAccount(harness);
			const previousId = await closeForHate(target);
			const adminRepository = new AdminRepository();
			await adminRepository.banIpForDecision('203.0.113.20', BigInt(previousId), new Date(Date.now() + 86_400_000));
			const before = await loadUser(target);
			const previous = await getDecision(harness, staff, previousId);
			await clearTestEmails(harness);

			const decision = await supersede(previousId);

			expect(decision).toMatchObject({
				kind: 'closure',
				status: 'active',
				reason_code: DeletionReasons.BAN_EVASION,
				statement: 'This account belongs to someone we banned before.',
				supersedes: previousId,
				superseded_by: null,
				deletion_at: previous.deletion_at,
				notice_status: 'sent',
				notice_channels: ['email'],
			});
			expect(decision.notice.rule?.title).toBe('Evading restrictions');
			expect(decision.notice.summary).toBe(
				`We closed your account. We changed the reason for this decision. It replaces decision ${previousId} and the reason we gave you then.`,
			);
			expect(decision.notice_text).toContain(`It replaces decision ${previousId}`);
			expect(decision.notice_text).toContain('This account belongs to someone we banned before.');
			expect(decision.notes[0]?.body).toContain(
				`Replaces decision ${previousId}, which gave reason code ${DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT}`,
			);
			expect(decision.network_blocks.map((block) => block.ip)).toEqual(['203.0.113.20']);

			const after = await loadUser(target);
			expect(after.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(after.pendingDeletionAt).toEqual(before.pendingDeletionAt);
			expect(after.enforcementDecisionId?.toString()).toBe(decision.decision_id);
			expect(after.deletionReasonCode).toBe(DeletionReasons.BAN_EVASION);
			expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
			expect(await adminRepository.isEmailBanned(target.email)).toBe(true);

			const ended = await getDecision(harness, staff, previousId);
			expect(ended).toMatchObject({
				status: 'ended',
				end_outcome: 'superseded',
				superseded_by: decision.decision_id,
				ended_by: staff.userId,
				end_statement: null,
				reason_code: DeletionReasons.HATE_SPEECH_OR_EXTREMIST_CONTENT,
				statement: 'You posted slurs in many communities.',
				notice_text: previous.notice_text,
				network_blocks: [],
			});

			const notices = await emailsTo(harness, target, 'enforcement_notice');
			expect(notices).toHaveLength(1);
			expect(notices[0]?.metadata).toMatchObject({kind_label: 'updated', decision_id: decision.decision_id});
			expect(await emailsTo(harness, target, 'enforcement_ended')).toEqual([]);
			expect(await emailsTo(harness, target, 'account_deletion_cancelled')).toEqual([]);
		});

		test('a reason that does not bar new accounts lifts the email ban, and back again', async () => {
			const target = await createTestAccount(harness);
			const adminRepository = new AdminRepository();
			const previousId = await closeForHate(target);
			const underage = await supersede(
				previousId,
				decisionBody({reason_code: DeletionReasons.UNDERAGE_ACCOUNT, statement: 'You told us you are 12.'}),
			);
			expect(await adminRepository.isEmailBanned(target.email)).toBe(false);
			await supersede(underage.decision_id);
			expect(await adminRepository.isEmailBanned(target.email)).toBe(true);
			const decisions = await listDecisions(harness, staff, target.userId);
			expect(decisions.map((decision) => decision.status)).toEqual(['active', 'ended', 'ended']);
		});

		test('ending the new decision still cancels the deletion', async () => {
			const target = await createTestAccount(harness);
			const decision = await supersede(await closeForHate(target));
			await endDecision(harness, staff, decision.decision_id, 'reversed').expect(HTTP_STATUS.OK).execute();
			const stored = await loadUser(target);
			expect(stored.pendingDeletionAt).toBeNull();
			expect(stored.flags & UserFlags.DELETED).toBe(0n);
		});
	});

	test('a suspension keeps its end and stays in force', async () => {
		const target = await createTestAccount(harness);
		const previousId = await suspend(harness, staff, target.userId);
		const before = await loadUser(target);
		await clearTestEmails(harness);
		const decision = await supersede(previousId);
		expect(decision).toMatchObject({kind: 'suspension', supersedes: previousId});
		expect(decision.ends_at).toBe((await getDecision(harness, staff, previousId)).ends_at);
		const after = await loadUser(target);
		expect(after.tempBannedUntil).toEqual(before.tempBannedUntil);
		expect(after.flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
		expect(after.enforcementDecisionId?.toString()).toBe(decision.decision_id);
		expect(await emailsTo(harness, target, 'enforcement_notice')).toHaveLength(1);
		expect(await emailsTo(harness, target, 'enforcement_ended')).toEqual([]);
	});

	test('a direct message limit keeps the limit and the new decision holds it', async () => {
		const target = await createTestAccount(harness);
		const previousId = await limitFeatures(harness, staff, target.userId, 'direct_messages');
		const decision = await supersede(
			previousId,
			decisionBody({reason_code: DeletionReasons.SCAM_OR_SOCIAL_ENGINEERING, statement: 'You asked for card numbers.'}),
		);
		expect(decision).toMatchObject({kind: 'feature_limit', limit: 'direct_messages'});
		expect((await loadUser(target)).flags & UserFlags.SPAMMER).toBe(UserFlags.SPAMMER);
		await endDecision(harness, staff, decision.decision_id, 'lifted').expect(HTTP_STATUS.OK).execute();
		expect((await loadUser(target)).flags & UserFlags.SPAMMER).toBe(0n);
	});

	test('a warning sends one updated notice by email and system DM', async () => {
		const target = await createTestAccount(harness);
		const previousId = await warn(harness, staff, target.userId);
		await clearTestEmails(harness);
		const decision = await supersede(
			previousId,
			decisionBody({reason_code: DeletionReasons.COORDINATED_RAIDING, statement: 'You joined a raid.'}),
		);
		expect(decision).toMatchObject({kind: 'warning', notice_channels: ['email', 'system_dm']});
		expect(decision.notice_text).toContain(`It replaces decision ${previousId}`);
		expect(await emailsTo(harness, target, 'enforcement_notice')).toHaveLength(1);
		expect(await emailsTo(harness, target, 'enforcement_ended')).toEqual([]);
	});

	describe('the new reason is checked like a new decision', () => {
		test('a reason the kind does not allow is refused and nothing changes', async () => {
			const target = await createTestAccount(harness);
			const previousId = await close(harness, staff, target.userId);
			await clearTestEmails(harness);
			expect(await refused(previousId, decisionBody({reason_code: DeletionReasons.SELF_HARM_PROMOTION}))).toContain(
				ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED,
			);
			expect(await refused(previousId, decisionBody({statement: undefined}))).toContain(
				ValidationErrorCodes.ENFORCEMENT_FACTS_REQUIRED,
			);
			expect(await listDecisions(harness, staff, target.userId)).toHaveLength(1);
			expect((await getDecision(harness, staff, previousId)).status).toBe('active');
			expect((await loadUser(target)).enforcementDecisionId?.toString()).toBe(previousId);
			expect(await emailsTo(harness, target, 'enforcement_notice')).toEqual([]);
		});

		test('a direct message needs a linked report that covers it', async () => {
			const target = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await createFriendship(harness, target, recipient);
			const dm = await createDmChannel(harness, target.token, recipient.userId);
			const message = await sendChannelMessage(harness, target.token, dm.id, 'send me your password');
			const previousId = await suspend(harness, staff, target.userId);
			expect(
				await refused(previousId, {...BAN_EVASION, evidence: [{channel_id: dm.id, message_id: message.id}]}),
			).toContain(ValidationErrorCodes.ENFORCEMENT_EVIDENCE_DM_UNREPORTED);
		});

		test('restricted reasons refuse a note, need evidence, a permanent suspension and both confirmations', async () => {
			const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const target = members[0]!;
			const message = await sendChannelMessage(harness, target.token, systemChannel.id, 'join our cell');
			const restricted: DecisionBody = {
				reason_code: DeletionReasons.VIOLENT_EXTREMISM,
				source: 'own_review',
				automation: 'none',
				evidence: [{channel_id: systemChannel.id, message_id: message.id}],
				confirmations: ['evidence_preserved', 'reports_filed_or_not_required'],
			};
			const temporaryId = await suspend(harness, staff, target.userId);
			expect(await refused(temporaryId, restricted)).toContain(ValidationErrorCodes.ENFORCEMENT_PERMANENT_REQUIRED);
			await endDecision(harness, staff, temporaryId).expect(HTTP_STATUS.OK).execute();
			const permanentId = await suspend(harness, staff, target.userId, decisionBody(), {duration_hours: 0});
			expect(await refused(permanentId, {...restricted, statement: 'Stop'})).toContain(
				ValidationErrorCodes.ENFORCEMENT_STATEMENT_NOT_ALLOWED,
			);
			expect(await refused(permanentId, {...restricted, evidence: []})).toContain(
				ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED,
			);
			expect(await refused(permanentId, {...restricted, confirmations: ['evidence_preserved']})).toContain(
				ValidationErrorCodes.ENFORCEMENT_CONFIRMATIONS_REQUIRED,
			);
			const decision = await supersede(permanentId, restricted);
			expect(decision).toMatchObject({reason_code: DeletionReasons.VIOLENT_EXTREMISM, statement: null, ends_at: null});
			expect(decision.evidence.map((evidence) => evidence.content)).toEqual([null]);
			expect(decision.notice.statement).toBeNull();
		});

		test('the internal note is required', async () => {
			const target = await createTestAccount(harness);
			const previousId = await suspend(harness, staff, target.userId);
			const {json} = await createBuilder(harness, staff.token)
				.post(`/admin/enforcement-decisions/${previousId}/supersede`)
				.body({decision: BAN_EVASION})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expect(JSON.stringify(json)).toContain(ValidationErrorCodes.ENFORCEMENT_INTERNAL_NOTE_REQUIRED);
		});
	});

	describe('refusals', () => {
		test('a superseded or ended decision cannot change again', async () => {
			const target = await createTestAccount(harness);
			const previousId = await suspend(harness, staff, target.userId);
			const decision = await supersede(previousId);
			await supersedeRequest(previousId, BAN_EVASION)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_ENDED)
				.execute();
			await endDecision(harness, staff, decision.decision_id).expect(HTTP_STATUS.OK).execute();
			await supersedeRequest(decision.decision_id, BAN_EVASION)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_ENDED)
				.execute();
		});

		test('a suspension a closure replaced cannot change', async () => {
			const target = await createTestAccount(harness);
			const suspensionId = await suspend(harness, staff, target.userId);
			await close(harness, staff, target.userId);
			await supersedeRequest(suspensionId, BAN_EVASION)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_REPLACED)
				.execute();
		});

		test('it needs the permission of the route that made the decision', async () => {
			const target = await createTestAccount(harness);
			const previousId = await close(harness, staff, target.userId);
			const limited = await createStaff(harness, [AdminACLs.ENFORCEMENT_VIEW, AdminACLs.USER_TEMP_BAN]);
			await supersedeRequest(previousId, BAN_EVASION, limited)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACL)
				.execute();
			expect((await getDecision(harness, staff, previousId)).status).toBe('active');
		});
	});
});
