// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {
	createDmChannel,
	createFriendship,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {formatNoticeMonth} from '@app/api/enforcement/NoticeDates';
import {
	createStaff,
	type DecisionBody,
	decisionBody,
	emailsTo,
	getDecision,
	INTERNAL_NOTE,
	listDecisions,
	loadUser,
	suspend,
	warn,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface ValidationErrorBody {
	errors: Array<{path: string; code: string}>;
}

interface MessageRef {
	channel_id: string;
	message_id: string;
}

const RESTRICTED: DecisionBody = {
	reason_code: DeletionReasons.CHILD_SAFETY_VIOLATION,
	source: 'own_review',
	automation: 'none',
	confirmations: ['evidence_preserved', 'reports_filed_or_not_required'],
};

function enforcement(decision: DecisionBody = decisionBody()) {
	return {type: 'enforcement', decision};
}

describe('Enforcement content, profile and community decisions', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;
	let worker: RecordingWorkerService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
		resetApiServicesForTesting();
		staff = await createStaff(harness);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function guildMessages(count = 2) {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const target = members[0]!;
		const reporter = members[1]!;
		const entries: Array<MessageRef> = [];
		for (let index = 0; index < count; index += 1) {
			const message = await sendChannelMessage(harness, target.token, systemChannel.id, `spam link ${index}`);
			entries.push({channel_id: systemChannel.id, message_id: message.id});
		}
		return {owner, target, reporter, guildId: guild.id, entries};
	}

	async function reportMessage(reporter: TestAccount, entry: MessageRef): Promise<string> {
		const {report_id} = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/message')
			.body({...entry, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		return report_id;
	}

	function shredRequest(userId: string, body: Record<string, unknown>) {
		return createBuilder<{job_id: string; requested: number; decision_id?: string}>(harness, staff.token)
			.post(`/admin/users/${userId}/message-shreds`)
			.header('X-Audit-Log-Reason', INTERNAL_NOTE)
			.body(body);
	}

	function deleteAllRequest(userId: string, body: Record<string, unknown>, dryRun = false) {
		return createBuilder<{message_count: number; job_id?: string; decision_id?: string}>(harness, staff.token)
			.delete(`/admin/users/${userId}/messages?dry_run=${dryRun}`)
			.header('X-Audit-Log-Reason', INTERNAL_NOTE)
			.body(body);
	}

	function removeCommunityRequest(guildId: string, body: Record<string, unknown>) {
		return createBuilder<{success: true}>(harness, staff.token)
			.delete(`/admin/guilds/${guildId}`)
			.header('X-Audit-Log-Reason', INTERNAL_NOTE)
			.body(body);
	}

	async function refused<T>(request: {expect: (status: number) => {execute: () => Promise<T>}}): Promise<string> {
		const body = (await request.expect(HTTP_STATUS.BAD_REQUEST).execute()) as ValidationErrorBody;
		return body.errors[0]!.code;
	}

	describe('message shreds', () => {
		test('a holder request or housekeeping shred queues the job without a decision', async () => {
			const {target, entries} = await guildMessages(1);
			for (const type of ['holder_request', 'housekeeping']) {
				const response = await shredRequest(target.userId, {entries, basis: {type}}).expect(HTTP_STATUS.OK).execute();
				expect(response.decision_id).toBeUndefined();
			}
			expect(worker.byTask('messageShred')).toHaveLength(2);
			expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
		});

		test('an enforcement shred snapshots the entries, records the removal and tells the user', async () => {
			const {target, entries} = await guildMessages(2);
			await clearTestEmails(harness);
			const response = await shredRequest(target.userId, {entries, basis: enforcement()})
				.expect(HTTP_STATUS.OK)
				.execute();
			const [job] = worker.byTask('messageShred');
			expect(job?.payload).toMatchObject({target_user_id: target.userId, entries});
			const decision = await getDecision(harness, staff, response.decision_id!);
			expect(decision).toMatchObject({
				kind: 'content_removal',
				removed_message_count: 2,
				removed_all_messages: false,
				notice_status: 'sent',
			});
			expect(decision.notice_channels.sort()).toEqual(['email', 'system_dm']);
			expect(decision.evidence.map((message) => message.content)).toEqual(['spam link 0', 'spam link 1']);
			expect(decision.notice_text).toContain('We removed 2 messages you sent.');
			expect(decision.notice_text).toContain('Your account is not otherwise restricted.');
			expect(await emailsTo(harness, target, 'enforcement_notice')).toHaveLength(1);
		});

		test('evidence messages are refused because the entries are the evidence', async () => {
			const {target, entries} = await guildMessages(1);
			const code = await refused(
				shredRequest(target.userId, {entries, basis: enforcement(decisionBody({evidence: entries}))}),
			);
			expect(code).toBe(ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_ALLOWED);
			expect(worker.byTask('messageShred')).toEqual([]);
		});

		test('a restricted shred stores every entry without content', async () => {
			const {target, entries} = await guildMessages(2);
			const response = await shredRequest(target.userId, {entries, basis: enforcement(RESTRICTED)})
				.expect(HTTP_STATUS.OK)
				.execute();
			const decision = await getDecision(harness, staff, response.decision_id!);
			expect(decision.evidence.map((message) => [message.message_id, message.content])).toEqual(
				entries.map((entry) => [entry.message_id, null]),
			);
			expect(decision.notice.facts).toEqual([
				'We found content or activity on your account that breaks our rules on protecting children.',
			]);
		});

		test('a failed queue leaves no decision behind', async () => {
			const {target, entries} = await guildMessages(1);
			worker.failure = (taskType) => (taskType === 'messageShred' ? new Error('queue down') : null);
			await shredRequest(target.userId, {entries, basis: enforcement()})
				.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
				.execute();
			expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
		});
	});

	describe('delete all messages', () => {
		test('a dry run records nothing and a deletion needs a basis', async () => {
			const {target} = await guildMessages(1);
			const preview = await deleteAllRequest(target.userId, {}, true).expect(HTTP_STATUS.OK).execute();
			expect(preview.message_count).toBeGreaterThan(0);
			expect(await refused(deleteAllRequest(target.userId, {}))).toBe(ValidationErrorCodes.MODERATION_BASIS_REQUIRED);
			expect(worker.byTask('messageShred')).toEqual([]);
			expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
		});

		test('housekeeping deletes without a decision', async () => {
			const {target} = await guildMessages(1);
			const response = await deleteAllRequest(target.userId, {basis: {type: 'housekeeping'}})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(response.job_id).toEqual(expect.any(String));
			expect(response.decision_id).toBeUndefined();
			expect(worker.byTask('messageShred')).toHaveLength(1);
		});

		test('an enforcement deletion records that all messages were removed', async () => {
			const {target} = await guildMessages(2);
			const response = await deleteAllRequest(target.userId, {basis: enforcement()}).expect(HTTP_STATUS.OK).execute();
			const decision = await getDecision(harness, staff, response.decision_id!);
			expect(decision).toMatchObject({
				kind: 'content_removal',
				removed_all_messages: true,
				removed_message_count: null,
			});
			expect(decision.notice_text).toContain('We removed all messages you sent.');
			expect(worker.byTask('messageShred')).toHaveLength(1);
		});

		test('restricted reasons are refused because the messages cannot be snapshotted first', async () => {
			const {target, entries} = await guildMessages(1);
			const code = await refused(
				deleteAllRequest(target.userId, {basis: enforcement({...RESTRICTED, evidence: entries})}),
			);
			expect(code).toBe(ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED);
			expect(worker.byTask('messageShred')).toEqual([]);
			expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
		});
	});

	describe('single message delete', () => {
		test('takes a holder request or housekeeping basis and refuses enforcement', async () => {
			const {entries} = await guildMessages(2);
			const [first, second] = entries;
			await createBuilder(harness, staff.token)
				.delete(`/admin/channels/${first!.channel_id}/messages/${first!.message_id}`)
				.body({basis: enforcement()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			await createBuilder(harness, staff.token)
				.delete(`/admin/channels/${second!.channel_id}/messages/${second!.message_id}`)
				.body({basis: {type: 'holder_request'}})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});

	describe('profile reset', () => {
		test('saves the old values as evidence before clearing and needs no other facts', async () => {
			const target = await createTestAccount(harness);
			await createBuilder(harness, target.token)
				.patch('/users/@me')
				.body({bio: 'buy followers at example.com', pronouns: 'they/them'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, staff.token)
				.delete(`/admin/users/${target.userId}/profile-fields`)
				.header('X-Audit-Log-Reason', INTERNAL_NOTE)
				.body({fields: ['bio'], basis: enforcement(decisionBody({statement: undefined}))})
				.expect(HTTP_STATUS.OK)
				.execute();
			const user = await loadUser(target);
			expect(user.bio).toBeNull();
			expect(user.pronouns).toBe('they/them');
			const [summary] = await listDecisions(harness, staff, target.userId);
			const decision = await getDecision(harness, staff, summary!.decision_id);
			expect(decision).toMatchObject({kind: 'profile_reset', profile_fields: ['bio']});
			expect(decision.profile_values).toEqual({bio: 'buy followers at example.com'});
			expect(decision.notice_text).toContain('We reset parts of your profile: bio.');
			expect(decision.notice_text).toContain('Your profile: bio.');
			expect(decision.notice_text).not.toContain('buy followers');
		});

		test('a holder request clears without a decision', async () => {
			const target = await createTestAccount(harness);
			await createBuilder(harness, staff.token)
				.delete(`/admin/users/${target.userId}/profile-fields`)
				.body({fields: ['global_name'], basis: {type: 'holder_request'}})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
		});
	});

	describe('community removal', () => {
		test('records the community as the subject and notifies the owner', async () => {
			const {owner, guildId, entries} = await guildMessages(1);
			await clearTestEmails(harness);
			await removeCommunityRequest(guildId, {basis: enforcement(decisionBody({evidence: entries}))})
				.expect(HTTP_STATUS.OK)
				.execute();
			const [summary] = await createBuilder<{decisions: Array<{decision_id: string}>}>(harness, staff.token)
				.get(`/admin/enforcement-decisions?subject_type=guild&subject_id=${guildId}`)
				.expect(HTTP_STATUS.OK)
				.execute()
				.then((response) => response.decisions);
			const decision = await getDecision(harness, staff, summary!.decision_id);
			expect(decision).toMatchObject({
				kind: 'community_removal',
				subject_type: 'guild',
				subject_id: guildId,
				subject_name: 'Test Guild',
				recipient_user_id: owner.userId,
				notice_status: 'sent',
			});
			expect(decision.notice_text).toContain('We removed your community Test Guild.');
			expect(decision.notice_text).toContain('We found content in this community that breaks this rule.');
			expect(decision.notice_text).toMatch(/1 message posted in #\S+, on /);
			expect(await emailsTo(harness, owner, 'enforcement_notice')).toHaveLength(1);
			expect(worker.byTask('guildProcessDeletion')).toHaveLength(1);
		});

		test('evidence must be in the community', async () => {
			const {guildId} = await guildMessages(1);
			const other = await guildMessages(1);
			const code = await refused(
				removeCommunityRequest(guildId, {basis: enforcement(decisionBody({evidence: other.entries}))}),
			);
			expect(code).toBe(ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_SUBJECT);
		});

		test('restricted reasons are refused and the community stays', async () => {
			const {guildId, entries} = await guildMessages(1);
			const code = await refused(
				removeCommunityRequest(guildId, {basis: enforcement({...RESTRICTED, evidence: entries})}),
			);
			expect(code).toBe(ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED);
			expect(await getGuildRepository().findUnique(createGuildID(BigInt(guildId)))).not.toBeNull();
			expect(worker.byTask('guildProcessDeletion')).toEqual([]);
		});
	});

	describe('direct message evidence', () => {
		test('a reported conversation names the report as the source and protected reasons show the month only', async () => {
			const target = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await createFriendship(harness, target, recipient);
			const dm = await createDmChannel(harness, target.token, recipient.userId);
			const message = await sendChannelMessage(harness, target.token, dm.id, 'you will regret this');
			const entry = {channel_id: dm.id, message_id: message.id};
			const reportId = await reportMessage(recipient, entry);
			const response = await shredRequest(target.userId, {
				entries: [entry],
				basis: enforcement(
					decisionBody({
						reason_code: DeletionReasons.HARASSMENT_OR_BULLYING,
						source: 'report',
						report_ids: [reportId],
					}),
				),
			})
				.expect(HTTP_STATUS.OK)
				.execute();
			const decision = await getDecision(harness, staff, response.decision_id!);
			expect(decision.notice.how_we_decided[0]).toBe(
				'We reviewed this after a report, and a person on our team made the decision.',
			);
			expect(decision.notice.facts).toContain(
				`1 message in a direct message conversation in ${formatNoticeMonth(new Date(), 'en-US')}.`,
			);
			expect(decision.notice_text).not.toContain(' (UTC)');
		});

		test('an unreported conversation is refused', async () => {
			const target = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await createFriendship(harness, target, recipient);
			const dm = await createDmChannel(harness, target.token, recipient.userId);
			const message = await sendChannelMessage(harness, target.token, dm.id, 'hello');
			const code = await refused(
				shredRequest(target.userId, {entries: [{channel_id: dm.id, message_id: message.id}], basis: enforcement()}),
			);
			expect(code).toBe(ValidationErrorCodes.ENFORCEMENT_EVIDENCE_DM_UNREPORTED);
		});
	});

	describe('reporter outcomes', () => {
		test('linked reports are resolved and reporters are told what happened to the item', async () => {
			const {target, reporter, entries} = await guildMessages(2);
			const removedReport = await reportMessage(reporter, entries[0]!);
			const restrictedReport = await reportMessage(reporter, entries[1]!);
			await clearTestEmails(harness);
			await shredRequest(target.userId, {
				entries: [entries[0]],
				basis: enforcement(decisionBody({source: 'report', report_ids: [removedReport]})),
			})
				.expect(HTTP_STATUS.OK)
				.execute();
			await suspend(harness, staff, target.userId, decisionBody({source: 'report', report_ids: [restrictedReport]}));
			const sent = await emailsTo(harness, reporter, 'report_resolved');
			expect(sent.map((email) => [email.metadata.report_id, email.metadata.outcome])).toEqual([
				[removedReport, 'removed'],
				[restrictedReport, 'restricted'],
			]);
		});

		test('a warning resolves linked reports and tells the reporter only that action was taken', async () => {
			const {target, reporter, entries} = await guildMessages(1);
			const reportId = await reportMessage(reporter, entries[0]!);
			await clearTestEmails(harness);
			await warn(harness, staff, target.userId, decisionBody({source: 'report', report_ids: [reportId]}));
			const sent = await emailsTo(harness, reporter, 'report_resolved');
			expect(sent.map((email) => [email.metadata.report_id, email.metadata.outcome])).toEqual([
				[reportId, 'action_taken'],
			]);
			const report = await createBuilder<{status: number}>(harness, staff.token)
				.get(`/admin/reports/${reportId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(report.status).toBe(1);
		});

		test('a content removal that left the reported message alone leaves the report open', async () => {
			const {target, reporter, entries} = await guildMessages(2);
			const reportId = await reportMessage(reporter, entries[1]!);
			await clearTestEmails(harness);
			await shredRequest(target.userId, {
				entries: [entries[0]],
				basis: enforcement(decisionBody({source: 'report', report_ids: [reportId]})),
			})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await emailsTo(harness, reporter, 'report_resolved')).toEqual([]);
		});

		test('a suspension that removes the reported message as evidence tells the reporter it was removed', async () => {
			const {target, reporter, entries} = await guildMessages(1);
			const reportId = await reportMessage(reporter, entries[0]!);
			await clearTestEmails(harness);
			await suspend(
				harness,
				staff,
				target.userId,
				decisionBody({source: 'report', report_ids: [reportId], evidence: [entries[0]!]}),
				{remove_evidence: true},
			);
			const [sent] = await emailsTo(harness, reporter, 'report_resolved');
			expect(sent?.metadata.outcome).toBe('removed');
		});

		test('manual resolutions take only the outcome their resolution allows', async () => {
			const {reporter, entries} = await guildMessages(1);
			const reportId = await reportMessage(reporter, entries[0]!);
			const resolve = (body: Record<string, unknown>) =>
				createBuilder<ValidationErrorBody>(harness, staff.token)
					.patch(`/admin/reports/${reportId}`)
					.body({status: 'resolved', ...body});
			for (const body of [
				{resolution: 'actioned'},
				{resolution: 'actioned', outcome: 'no_violation'},
				{resolution: 'duplicate'},
				{resolution: 'duplicate', outcome: 'restricted', duplicate_of: reportId},
				{resolution: 'duplicate', duplicate_of: reportId},
				{resolution: 'no_violation', outcome: 'removed'},
				{resolution: 'no_violation', duplicate_of: reportId},
			]) {
				expect(await refused(resolve(body))).toBe(ValidationErrorCodes.REPORT_OUTCOME_INVALID);
			}
		});

		test('a duplicate tells the reporter the outcome of the report it repeats', async () => {
			const {owner: second, reporter, entries} = await guildMessages(1);
			const originalId = await reportMessage(reporter, entries[0]!);
			const duplicateId = await reportMessage(second, entries[0]!);
			const resolve = (reportId: string, body: Record<string, unknown>) =>
				createBuilder(harness, staff.token)
					.patch(`/admin/reports/${reportId}`)
					.body({status: 'resolved', ...body})
					.expect(HTTP_STATUS.OK)
					.execute();
			await resolve(originalId, {resolution: 'actioned', outcome: 'restricted'});
			await clearTestEmails(harness);
			await resolve(duplicateId, {resolution: 'duplicate', duplicate_of: originalId});
			const [email] = await emailsTo(harness, second, 'report_resolved');
			expect(email?.metadata.outcome).toBe('restricted');
		});

		test('a duplicate of a report with no outcome yet tells the reporter nothing', async () => {
			const {owner: second, reporter, entries} = await guildMessages(1);
			const originalId = await reportMessage(reporter, entries[0]!);
			const duplicateId = await reportMessage(second, entries[0]!);
			await clearTestEmails(harness);
			await createBuilder(harness, staff.token)
				.patch(`/admin/reports/${duplicateId}`)
				.body({status: 'resolved', resolution: 'duplicate', duplicate_of: originalId})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await emailsTo(harness, second, 'report_resolved')).toEqual([]);
		});

		test('a no violation resolution tells the reporter the item was left up', async () => {
			const {reporter, entries} = await guildMessages(1);
			const reportId = await reportMessage(reporter, entries[0]!);
			await clearTestEmails(harness);
			await createBuilder(harness, staff.token)
				.patch(`/admin/reports/${reportId}`)
				.body({status: 'resolved', resolution: 'no_violation'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const [email] = await emailsTo(harness, reporter, 'report_resolved');
			expect(email?.metadata.outcome).toBe('no_violation');
		});
	});
});
