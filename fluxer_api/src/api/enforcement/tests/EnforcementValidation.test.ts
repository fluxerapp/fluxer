// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	createStaff,
	type DecisionBody,
	decisionBody,
	getDecision,
	INTERNAL_NOTE,
	listDecisions,
	loadUser,
	suspend,
	suspendRequest,
	warn,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface ValidationErrorBody {
	errors: Array<{path: string; code: string}>;
}

describe('Enforcement decision validation', () => {
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

	async function refusedSuspension(
		target: TestAccount,
		decision: DecisionBody,
		extra: Record<string, unknown> = {},
	): Promise<{path: string; code: string}> {
		const body = await createBuilder<ValidationErrorBody>(harness, staff.token)
			.put(`/admin/users/${target.userId}/ban`)
			.header('X-Audit-Log-Reason', INTERNAL_NOTE)
			.body({duration_hours: 24, decision, ...extra})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
		return body.errors[0]!;
	}

	async function refusedWarning(target: TestAccount, decision: DecisionBody): Promise<string> {
		const body = await createBuilder<ValidationErrorBody>(harness, staff.token)
			.post(`/admin/users/${target.userId}/warnings`)
			.header('X-Audit-Log-Reason', INTERNAL_NOTE)
			.body({decision})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		return body.errors[0]!.code;
	}

	async function guildMessages() {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const own = await sendChannelMessage(harness, target.token, systemChannel.id, 'buy cheap accounts here');
		const other = await sendChannelMessage(harness, owner.token, systemChannel.id, 'please stop');
		return {owner, target, channelId: systemChannel.id, own: own.id, other: other.id};
	}

	async function reportMessage(reporter: TestAccount, channelId: string, messageId: string): Promise<string> {
		const {report_id} = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/message')
			.body({channel_id: channelId, message_id: messageId, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		return report_id;
	}

	test('ACLs are checked before any lookup', async () => {
		const target = await createTestAccount(harness);
		const limited = await createStaff(harness, [AdminACLs.USER_TEMP_BAN]);
		for (const decision of [
			decisionBody({evidence: [{channel_id: '1', message_id: '2'}]}),
			decisionBody({source: 'report', report_ids: ['3']}),
		]) {
			await suspendRequest(harness, limited, target.userId, {duration_hours: 24, decision})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACL)
				.execute();
		}
		await suspendRequest(harness, limited, target.userId, {
			duration_hours: 24,
			decision: decisionBody(),
			remove_evidence: true,
		})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACL)
			.execute();
	});

	test('the internal note is required', async () => {
		const target = await createTestAccount(harness);
		const body = await suspendRequest(
			harness,
			staff,
			target.userId,
			{duration_hours: 24, decision: decisionBody()},
			null,
		)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect((body as unknown as ValidationErrorBody).errors[0]).toMatchObject({
			path: 'X-Audit-Log-Reason',
			code: ValidationErrorCodes.ENFORCEMENT_INTERNAL_NOTE_REQUIRED,
		});
	});

	test('source and automation have no defaults, and the reasons outside enforcement are refused', async () => {
		const target = await createTestAccount(harness);
		for (const decision of [
			{reason_code: DeletionReasons.SPAM, automation: 'none', statement: 'x'},
			{reason_code: DeletionReasons.SPAM, source: 'own_review', statement: 'x'},
			decisionBody({automation: 'automated' as never}),
			decisionBody({reason_code: DeletionReasons.OTHER as never}),
			decisionBody({reason_code: DeletionReasons.USER_REQUESTED as never}),
		]) {
			await suspendRequest(harness, staff, target.userId, {duration_hours: 24, decision})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		}
	});

	test('each reason allows only its kinds and subjects', async () => {
		const target = await createTestAccount(harness);
		expect(
			await refusedSuspension(target, decisionBody({reason_code: DeletionReasons.UNDERAGE_ACCOUNT})),
		).toMatchObject({code: ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED});
		expect(
			await refusedSuspension(target, decisionBody({reason_code: DeletionReasons.COPYRIGHT, rights_holder: 'Label'})),
		).toMatchObject({code: ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED});
		expect(await refusedWarning(target, decisionBody({reason_code: DeletionReasons.CHILD_SAFETY_VIOLATION}))).toBe(
			ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED,
		);
		const bot = await createTestAccount(harness);
		const user = await loadUser(bot);
		await getUserRepository().patchUpsert(user.id, {bot: true}, user.toRow());
		expect(await refusedWarning(bot, decisionBody())).toBe(ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED);
	});

	test('a report source needs reports about this subject', async () => {
		const target = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		const reporter = await createTestAccount(harness);
		expect(await refusedSuspension(target, decisionBody({source: 'report'}))).toMatchObject({
			code: ValidationErrorCodes.ENFORCEMENT_REPORT_REQUIRED,
		});
		const {report_id: aboutOther} = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/user')
			.body({user_id: other.userId, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		for (const reportId of [aboutOther, '999999']) {
			expect(await refusedSuspension(target, decisionBody({source: 'report', report_ids: [reportId]}))).toMatchObject({
				path: 'decision.report_ids.0',
				code: ValidationErrorCodes.ENFORCEMENT_REPORT_MISMATCH,
			});
		}
	});

	test('evidence must exist and be the subject’s own', async () => {
		const {target, channelId, other} = await guildMessages();
		expect(
			await refusedSuspension(target, decisionBody({evidence: [{channel_id: channelId, message_id: '123'}]})),
		).toMatchObject({path: 'decision.evidence.0', code: ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_FOUND});
		expect(
			await refusedSuspension(target, decisionBody({evidence: [{channel_id: channelId, message_id: other}]})),
		).toMatchObject({code: ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_SUBJECT});
	});

	test('report snapshots keep only the subject’s messages', async () => {
		const {owner, target, channelId, own} = await guildMessages();
		const reportId = await reportMessage(owner, channelId, own);
		const decisionId = await suspend(
			harness,
			staff,
			target.userId,
			decisionBody({source: 'report', report_ids: [reportId], statement: undefined}),
		);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.evidence.map((message) => message.content)).toEqual(['buy cheap accounts here']);
	});

	test('a direct message is evidence only when a linked report covers the conversation', async () => {
		const target = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await createFriendship(harness, target, recipient);
		const dm = await createDmChannel(harness, target.token, recipient.userId);
		const message = await sendChannelMessage(harness, target.token, dm.id, 'send me your password');
		const evidence = [{channel_id: dm.id, message_id: message.id}];
		expect(await refusedSuspension(target, decisionBody({evidence}))).toMatchObject({
			code: ValidationErrorCodes.ENFORCEMENT_EVIDENCE_DM_UNREPORTED,
		});
		const reportId = await reportMessage(recipient, dm.id, message.id);
		const decisionId = await suspend(
			harness,
			staff,
			target.userId,
			decisionBody({source: 'report', report_ids: [reportId], evidence}),
		);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.notice.how_we_decided[0]).toBe('We reviewed this after a report.');
	});

	test('a decision needs evidence or a note to the user', async () => {
		const target = await createTestAccount(harness);
		expect(await refusedSuspension(target, decisionBody({statement: undefined}))).toMatchObject({
			code: ValidationErrorCodes.ENFORCEMENT_FACTS_REQUIRED,
		});
		expect(await refusedSuspension(target, decisionBody(), {remove_evidence: true})).toMatchObject({
			code: ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED,
		});
	});

	test('restricted reasons refuse a note, need evidence, a permanent suspension and both confirmations', async () => {
		const {target, channelId, own} = await guildMessages();
		const restricted = {
			reason_code: DeletionReasons.VIOLENT_EXTREMISM,
			statement: undefined,
			evidence: [{channel_id: channelId, message_id: own}],
			confirmations: ['evidence_preserved', 'reports_filed_or_not_required'] as Array<
				'evidence_preserved' | 'reports_filed_or_not_required'
			>,
		};
		expect(
			await refusedSuspension(target, decisionBody({...restricted, statement: 'Stop'}), {duration_hours: 0}),
		).toMatchObject({code: ValidationErrorCodes.ENFORCEMENT_STATEMENT_NOT_ALLOWED});
		expect(
			await refusedSuspension(target, decisionBody({...restricted, evidence: []}), {duration_hours: 0}),
		).toMatchObject({
			code: ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED,
		});
		expect(await refusedSuspension(target, decisionBody(restricted))).toMatchObject({
			code: ValidationErrorCodes.ENFORCEMENT_PERMANENT_REQUIRED,
		});
		expect(
			await refusedSuspension(target, decisionBody({...restricted, confirmations: ['evidence_preserved']}), {
				duration_hours: 0,
			}),
		).toMatchObject({code: ValidationErrorCodes.ENFORCEMENT_CONFIRMATIONS_REQUIRED});
		const decisionId = await suspend(harness, staff, target.userId, decisionBody(restricted), {duration_hours: 0});
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.notes[0]!.body).toContain('evidence_preserved, reports_filed_or_not_required');
	});

	test('copyright needs the rights holder and other reasons refuse it', async () => {
		const target = await createTestAccount(harness);
		expect(await refusedWarning(target, decisionBody({reason_code: DeletionReasons.COPYRIGHT}))).toBe(
			ValidationErrorCodes.ENFORCEMENT_RIGHTS_HOLDER_REQUIRED,
		);
		expect(await refusedWarning(target, decisionBody({rights_holder: 'Example Records'}))).toBe(
			ValidationErrorCodes.ENFORCEMENT_RIGHTS_HOLDER_NOT_ALLOWED,
		);
		const decisionId = await warn(
			harness,
			staff,
			target.userId,
			decisionBody({reason_code: DeletionReasons.COPYRIGHT, rights_holder: 'Example Records'}),
		);
		expect((await getDecision(harness, staff, decisionId)).notice.facts).toContain(
			'Rights holder or right concerned: Example Records.',
		);
	});
});
