// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {createNoticeLookupDeps, loadAccountNotice} from '@app/api/enforcement/EnforcementAccountNotice';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {formatNoticeDate} from '@app/api/enforcement/NoticeDates';
import {clearExpiredSuspension} from '@app/api/enforcement/SuspensionState';
import {
	closeRequest,
	createStaff,
	decisionBody,
	emailsTo,
	endDecision,
	getDecision,
	INTERNAL_NOTE,
	listDecisions,
	loadUser,
	suspend,
	suspendRequest,
	warn,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserDeletionEligibilityService} from '@app/api/user/services/UserDeletionEligibilityService';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {ms} from 'itty-time';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Enforcement suspensions', () => {
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

	async function guildMessage(): Promise<{
		target: TestAccount;
		channelId: string;
		channelName: string;
		messageId: string;
	}> {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const message = await sendChannelMessage(harness, target.token, systemChannel.id, 'join my raid server now');
		return {target, channelId: systemChannel.id, channelName: systemChannel.name!, messageId: message.id};
	}

	test('records the decision and points the suspended user at it', async () => {
		const target = await createTestAccount(harness);
		const decisionId = await suspend(harness, staff, target.userId);
		const user = await loadUser(target);
		expect(user.enforcementDecisionId).toBe(BigInt(decisionId));
		expect(user.tempBannedUntil).not.toBeNull();
		expect((user.flags & UserFlags.DISABLED) !== 0n).toBe(true);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision).toMatchObject({kind: 'suspension', status: 'active', notice_status: 'sent', replaced: false});
		expect(decision.notes.map((note) => note.body)).toEqual([INTERNAL_NOTE]);
	});

	test('the notice email shows every section with a pre-formatted deadline', async () => {
		const {target, channelId, channelName, messageId} = await guildMessage();
		await clearTestEmails(harness);
		const decisionId = await suspend(
			harness,
			staff,
			target.userId,
			decisionBody({evidence: [{channel_id: channelId, message_id: messageId}]}),
		);
		const decision = await getDecision(harness, staff, decisionId);
		const deadline = formatNoticeDate(new Date(new Date(decision.notice_sent_at!).getTime() + ms('60 days')), 'en-US');
		const [email] = await emailsTo(harness, target, 'enforcement_notice');
		expect(email?.metadata).toMatchObject({
			kind_label: 'suspension',
			neutral: 'no',
			rule_title: 'Spam',
			appeals_email: 'appeals@fluxer.com',
			appeal_deadline: deadline,
			decision_id: decisionId,
		});
		const text = decision.notice_text!;
		expect(text).toContain('We suspended your account until ');
		expect(text).toContain("You can't sign in or use Fluxer anywhere until then.");
		expect(text).toMatch(/Rule: Spam\n.+\/guidelines#6-spam-and-service-abuse\n/);
		expect(text).toContain('We found that your account sent unwanted messages in bulk.');
		expect(text).toContain(`1 message in #${channelName} of Test Guild, on `);
		expect(text).toContain('Note from our team: You sent the same invite link to many people.');
		expect(text).toContain(
			'How we decided\nWe found this in our own review, and a person on our team made the decision.',
		);
		expect(text).toContain(`reply to this email by ${deadline}.`);
		expect(text).toContain('certified out-of-court dispute settlement body');
		expect(text).toContain(`Decision ID: ${decisionId}`);
		expect(text).not.toContain(INTERNAL_NOTE);
		expect(text).not.toContain('join my raid server now');
		expect(decision.notice_channels).toEqual(['email']);
	});

	test('a permanent suspension is emailed as a ban', async () => {
		const target = await createTestAccount(harness);
		await clearTestEmails(harness);
		const {user} = await suspendRequest(harness, staff, target.userId, {duration_hours: 0, decision: decisionBody()})
			.expect(HTTP_STATUS.OK)
			.execute();
		const decision = await getDecision(harness, staff, user.enforcement_decision_id!);
		expect(decision.ends_at).toBeNull();
		expect(decision.notice.permanent).toBe(true);
		expect(decision.notice_text).toContain('We banned your account.');
		expect(decision.notice_text).toContain('You may not create or use other accounts.');
		const [email] = await emailsTo(harness, target, 'enforcement_notice');
		expect(email?.metadata.kind_label).toBe('ban');
	});

	test('a replacing suspension leaves the earlier row active, reads it as replaced and sends no ended email', async () => {
		const target = await createTestAccount(harness);
		const first = await suspend(harness, staff, target.userId);
		await clearTestEmails(harness);
		const second = await suspend(harness, staff, target.userId);
		const decisions = await listDecisions(harness, staff, target.userId);
		expect(decisions.map((decision) => [decision.decision_id, decision.status, decision.replaced])).toEqual([
			[second, 'active', false],
			[first, 'active', true],
		]);
		expect(await emailsTo(harness, target, 'enforcement_ended')).toEqual([]);
		await createBuilder(harness, staff.token)
			.post(`/admin/enforcement-decisions/${first}/end`)
			.body({outcome: 'lifted'})
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_REPLACED)
			.execute();
	});

	test('a lost race leaves exactly one current decision', async () => {
		const target = await createTestAccount(harness);
		const ids = await Promise.all([suspend(harness, staff, target.userId), suspend(harness, staff, target.userId)]);
		const user = await loadUser(target);
		expect(ids.map(BigInt)).toContain(user.enforcementDecisionId);
		const decisions = await listDecisions(harness, staff, target.userId);
		expect(decisions.filter((decision) => !decision.replaced)).toHaveLength(1);
	});

	test('a closed account cannot be suspended', async () => {
		const target = await createTestAccount(harness);
		await getUserRepository().updateFlags(createUserID(BigInt(target.userId)), (flags) => flags | UserFlags.DELETED);
		await suspendRequest(harness, staff, target.userId, {duration_hours: 24, decision: decisionBody()})
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_ACCOUNT_CLOSED)
			.execute();
		expect(await listDecisions(harness, staff, target.userId)).toEqual([]);
	});

	test('remove_evidence queues the shred of the snapshotted messages', async () => {
		const worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
		resetApiServicesForTesting();
		const {target, channelId, messageId} = await guildMessage();
		const decisionId = await suspend(
			harness,
			staff,
			target.userId,
			decisionBody({evidence: [{channel_id: channelId, message_id: messageId}]}),
			{remove_evidence: true},
		);
		const [job] = worker.byTask('messageShred');
		expect(job?.payload).toMatchObject({
			target_user_id: target.userId,
			entries: [{channel_id: channelId, message_id: messageId}],
		});
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.removed_message_count).toBe(1);
		expect(decision.evidence.map((message) => message.content)).toEqual(['join my raid server now']);
		expect(decision.notice_text).toContain('We also removed 1 message.');
	});

	test.each(['lifted', 'reversed'] as const)(
		'ending as %s lifts the suspension and emails the holder',
		async (outcome) => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			await clearTestEmails(harness);
			const ended = await createBuilder<{status: string; end_outcome: string}>(harness, staff.token)
				.post(`/admin/enforcement-decisions/${decisionId}/end`)
				.header('X-Audit-Log-Reason', 'Appeal upheld')
				.body({outcome, statement: 'We checked the messages again.'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(ended).toMatchObject({status: 'ended', end_outcome: outcome});
			const user = await loadUser(target);
			expect(user.tempBannedUntil).toBeNull();
			expect(user.enforcementDecisionId).toBeNull();
			expect((user.flags & UserFlags.DISABLED) === 0n).toBe(true);
			const [email] = await emailsTo(harness, target, 'enforcement_ended');
			expect(email?.metadata).toMatchObject({
				outcome,
				decision_id: decisionId,
				restores_access: 'yes',
				statement: 'We checked the messages again.',
			});
			await createBuilder(harness, staff.token)
				.post(`/admin/enforcement-decisions/${decisionId}/end`)
				.body({outcome})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_ENDED)
				.execute();
		},
	);

	test('the legacy unban route refuses a suspension a decision owns', async () => {
		const target = await createTestAccount(harness);
		const decisionId = await suspend(harness, staff, target.userId);
		const response = await createBuilder<{decision_id: string}>(harness, staff.token)
			.delete(`/admin/users/${target.userId}/ban`)
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
			.execute();
		expect(response.decision_id).toBe(decisionId);
	});

	test('a restricted suspension cancels the deletion the holder scheduled, so the account is kept', async () => {
		const {target, channelId, messageId} = await guildMessage();
		const users = getUserRepository();
		const user = await loadUser(target);
		const scheduledFor = new Date(Date.now() + ms('1 day'));
		await users.updateDeletionSchedule(user, {
			flags: user.flags | UserFlags.SELF_DELETED,
			pending_deletion_at: scheduledFor,
			deletion_reason_code: DeletionReasons.USER_REQUESTED,
		});
		const {user: suspended} = await suspendRequest(harness, staff, target.userId, {
			duration_hours: 0,
			decision: decisionBody({
				reason_code: DeletionReasons.CHILD_SEXUAL_CONTENT,
				statement: undefined,
				evidence: [{channel_id: channelId, message_id: messageId}],
				confirmations: ['evidence_preserved', 'reports_filed_or_not_required'],
			}),
		})
			.expect(HTTP_STATUS.OK)
			.execute();
		const kept = await loadUser(target);
		expect(kept.pendingDeletionAt).toBeNull();
		expect(kept.flags & UserFlags.SELF_DELETED).toBe(0n);
		expect(kept.flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
		const decision = await getDecision(harness, staff, suspended.enforcement_decision_id!);
		expect(decision.notes[0]?.body).toContain(
			`Cancelled the deletion the account holder had scheduled for ${scheduledFor.toISOString()}`,
		);
		expect(await users.startDeletion(kept.id, scheduledFor)).toBeNull();
		expect((await loadUser(target)).deletionStartedAt).toBeNull();
	});

	test('a bot suspension reaches an owner without email by system DM', async () => {
		const bot = await createTestBotAccount(harness);
		const users = getUserRepository();
		const owner = (await users.findUnique(createUserID(BigInt(bot.ownerUserId))))!;
		await users.patchUpsert(owner.id, {email: null}, owner.toRow());
		const decisionId = await suspend(harness, staff, bot.botUserId);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision).toMatchObject({notice_status: 'sent', notice_channels: ['system_dm']});
		expect(decision.recipient_user_id).toBe(bot.ownerUserId);
	});

	test('a pointer to a missing decision renders the legacy notice', async () => {
		const target = await createTestAccount(harness);
		const users = getUserRepository();
		const user = await loadUser(target);
		await users.patchUpsert(
			user.id,
			{temp_banned_until: new Date(Date.now() + ms('1 day')), enforcement_decision_id: 123456789n},
			user.toRow(),
		);
		const notice = await loadAccountNotice(createNoticeLookupDeps(), await loadUser(target), 'en-US');
		expect(notice).toMatchObject({
			decision_id: null,
			kind: 'suspension',
			permanent: false,
			facts: ["We don't have more detail on record for this decision."],
			how_we_decided: [],
		});
	});

	test('records undelivered when the account has no email address', async () => {
		const target = await createTestAccount(harness);
		const user = await loadUser(target);
		await getUserRepository().patchUpsert(user.id, {email: null}, user.toRow());
		const decisionId = await suspend(harness, staff, target.userId);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision).toMatchObject({notice_status: 'undelivered', notice_channels: [], notice_sent_at: null});
		expect(decision.notice.appeal).toMatchObject({account_has_email: false, deadline: null});
	});

	test('records undelivered when email is off on the instance', async () => {
		const target = await createTestAccount(harness);
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: 'off'}});
		await clearTestEmails(harness);
		const decisionId = await suspend(harness, staff, target.userId);
		expect((await getDecision(harness, staff, decisionId)).notice_status).toBe('undelivered');
		expect(await emailsTo(harness, target, 'enforcement_notice')).toEqual([]);
	});

	async function systemDmCount(account: TestAccount): Promise<number> {
		const channels = await createBuilder<Array<{id: string; recipients?: Array<{id: string}>}>>(harness, account.token)
			.get('/users/@me/channels')
			.expect(HTTP_STATUS.OK)
			.execute();
		let count = 0;
		for (const channel of channels.filter((entry) => entry.recipients?.some((recipient) => recipient.id === '0'))) {
			const messages = await createBuilder<Array<unknown>>(harness, account.token)
				.get(`/channels/${channel.id}/messages?limit=50`)
				.expect(HTTP_STATUS.OK)
				.execute();
			count += messages.length;
		}
		return count;
	}

	test('with DMs off a warning and its end go by email only', async () => {
		const target = await createTestAccount(harness);
		await getInstanceConfigRepository().setInstancePolicyConfig({direct_messages_disabled: true});
		await clearTestEmails(harness);
		const decisionId = await warn(harness, staff, target.userId);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision).toMatchObject({notice_status: 'sent', notice_channels: ['email']});
		await createBuilder(harness, staff.token)
			.post(`/admin/enforcement-decisions/${decisionId}/end`)
			.body({outcome: 'reversed'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await emailsTo(harness, target, 'enforcement_ended')).toHaveLength(1);
		expect(await systemDmCount(target)).toBe(0);
	});

	test('with DMs off a bot owner without email is recorded undelivered', async () => {
		const bot = await createTestBotAccount(harness);
		const users = getUserRepository();
		const owner = (await users.findUnique(createUserID(BigInt(bot.ownerUserId))))!;
		await users.patchUpsert(owner.id, {email: null}, owner.toRow());
		await getInstanceConfigRepository().setInstancePolicyConfig({direct_messages_disabled: true});
		const decisionId = await suspend(harness, staff, bot.botUserId);
		expect(await getDecision(harness, staff, decisionId)).toMatchObject({
			notice_status: 'undelivered',
			notice_channels: [],
		});
	});

	test('a warning changes nothing and is sent by email and system DM', async () => {
		const target = await createTestAccount(harness);
		await clearTestEmails(harness);
		const decisionId = await warn(harness, staff, target.userId);
		const user = await loadUser(target);
		expect(user.flags & UserFlags.DISABLED).toBe(0n);
		expect(user.enforcementDecisionId).toBeNull();
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision).toMatchObject({kind: 'warning', notice_status: 'sent'});
		expect(decision.notice_channels).toEqual(['email', 'system_dm']);
		expect(decision.notice_text).toContain("We're warning you about activity on your account.");
		const [email] = await emailsTo(harness, target, 'enforcement_notice');
		expect(email?.metadata.kind_label).toBe('warning');
		await createBuilder(harness, staff.token)
			.post(`/admin/enforcement-decisions/${decisionId}/end`)
			.body({outcome: 'lifted'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await createBuilder(harness, staff.token)
			.post(`/admin/enforcement-decisions/${decisionId}/end`)
			.body({outcome: 'reversed'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const [ended] = await emailsTo(harness, target, 'enforcement_ended');
		expect(ended?.metadata).toMatchObject({outcome: 'reversed', restores_access: 'no'});
	});

	test('linked reports are resolved as actioned and the reporter is told', async () => {
		const reporter = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const {report_id: reportId} = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/user')
			.body({user_id: target.userId, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await clearTestEmails(harness);
		const decisionId = await suspend(
			harness,
			staff,
			target.userId,
			decisionBody({source: 'report', report_ids: [reportId]}),
		);
		const report = await createBuilder<{status: number}>(harness, staff.token)
			.get(`/admin/reports/${reportId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(report.status).toBe(1);
		expect((await emailsTo(harness, reporter, 'report_resolved')).length).toBe(1);
		const decision = await getDecision(harness, staff, decisionId);
		expect(decision.report_ids).toEqual([reportId]);
		expect(decision.notice_text).toContain(
			'We reviewed this after a report, and a person on our team made the decision.',
		);
	});

	test('suspended accounts are not eligible for inactivity deletion', async () => {
		const target = await createTestAccount(harness);
		await suspend(harness, staff, target.userId);
		const users = getUserRepository();
		const user = await loadUser(target);
		const inactive = await users.patchUpsert(
			user.id,
			{last_active_at: new Date(Date.now() - ms('1000 days'))},
			user.toRow(),
		);
		const eligibility = new UserDeletionEligibilityService(new MockKVProvider());
		expect(await eligibility.isEligibleForInactivityDeletion(inactive)).toBe(false);
		const legacy = await users.patchUpsert(user.id, {enforcement_decision_id: null}, inactive.toRow());
		expect(await eligibility.isEligibleForInactivityDeletion(legacy)).toBe(false);
		const expired = await users.patchUpsert(
			user.id,
			{temp_banned_until: new Date(Date.now() - ms('1 day')), enforcement_decision_id: user.enforcementDecisionId},
			legacy.toRow(),
		);
		expect(await eligibility.isEligibleForInactivityDeletion(expired)).toBe(true);
	});

	test('an expired suspension the holder never signed in after does not block a holder request closure', async () => {
		const target = await createTestAccount(harness);
		await suspend(harness, staff, target.userId);
		const user = await loadUser(target);
		await getUserRepository().patchUpsert(user.id, {temp_banned_until: new Date(Date.now() - 1000)}, user.toRow());
		await closeRequest(harness, staff, target.userId, {days_until_deletion: 14, basis: {type: 'holder_request'}})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect((await loadUser(target)).pendingDeletionAt).not.toBeNull();
	});

	describe('expired suspensions in the inactivity worker', () => {
		async function runWorker(): Promise<{suspensions_cleared: number; errors: number}> {
			return createBuilderWithoutAuth<{suspensions_cleared: number; errors: number}>(harness)
				.post('/test/worker/process-inactivity-deletions')
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
		}

		async function expire(target: TestAccount): Promise<Date> {
			const endedAt = new Date(Date.now() - ms('1 hour'));
			const user = await loadUser(target);
			await getUserRepository().patchUpsert(user.id, {temp_banned_until: endedAt}, user.toRow());
			return endedAt;
		}

		test('clears an expired suspension once and records it as expired with no notice', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			const endedAt = await expire(target);
			await clearTestEmails(harness);
			expect((await runWorker()).suspensions_cleared).toBe(1);
			const cleared = await loadUser(target);
			expect(cleared.flags & UserFlags.DISABLED).toBe(0n);
			expect(cleared.tempBannedUntil).toBeNull();
			expect(cleared.enforcementDecisionId).toBeNull();
			const decision = await getDecision(harness, staff, decisionId);
			expect(decision).toMatchObject({
				status: 'ended',
				end_outcome: 'expired',
				ended_at: endedAt.toISOString(),
				ended_by: '0',
				end_statement: null,
				expired: false,
			});
			expect(await emailsTo(harness, target, 'enforcement_ended')).toEqual([]);
			expect((await runWorker()).suspensions_cleared).toBe(0);
			expect(await getDecision(harness, staff, decisionId)).toMatchObject({end_outcome: 'expired'});
			await endDecision(harness, staff, decisionId, 'lifted').expect(HTTP_STATUS.CONFLICT).execute();
			await endDecision(harness, staff, decisionId, 'reversed').expect(HTTP_STATUS.OK).execute();
			expect(await getDecision(harness, staff, decisionId)).toMatchObject({status: 'ended', end_outcome: 'reversed'});
			const ended = await emailsTo(harness, target, 'enforcement_ended');
			expect(ended.map((email) => email.metadata)).toMatchObject([{outcome: 'reversed', restores_access: 'no'}]);
		});

		test('leaves active and permanent suspensions alone', async () => {
			const temporary = await createTestAccount(harness);
			const temporaryId = await suspend(harness, staff, temporary.userId);
			const permanent = await createTestAccount(harness);
			const {user} = await suspendRequest(harness, staff, permanent.userId, {
				duration_hours: 0,
				decision: decisionBody(),
			})
				.expect(HTTP_STATUS.OK)
				.execute();
			const before = await Promise.all([loadUser(temporary), loadUser(permanent)]);
			expect((await runWorker()).suspensions_cleared).toBe(0);
			for (const [index, account] of [temporary, permanent].entries()) {
				const after = await loadUser(account);
				expect(after.flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
				expect(after.tempBannedUntil).toEqual(before[index]!.tempBannedUntil);
				expect(after.enforcementDecisionId).toEqual(before[index]!.enforcementDecisionId);
			}
			for (const decisionId of [temporaryId, user.enforcement_decision_id!]) {
				expect(await getDecision(harness, staff, decisionId)).toMatchObject({status: 'active', end_outcome: null});
			}
		});

		test('ends only the current decision and never one that was replaced', async () => {
			const target = await createTestAccount(harness);
			const replacedId = await suspend(harness, staff, target.userId);
			const currentId = await suspend(harness, staff, target.userId);
			await expire(target);
			expect((await runWorker()).suspensions_cleared).toBe(1);
			expect(await getDecision(harness, staff, currentId)).toMatchObject({status: 'ended', end_outcome: 'expired'});
			expect(await getDecision(harness, staff, replacedId)).toMatchObject({
				status: 'active',
				end_outcome: null,
				replaced: true,
			});
		});

		test('never ends a decision that was already reversed', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			await expire(target);
			await endDecision(harness, staff, decisionId, 'reversed').expect(HTTP_STATUS.OK).execute();
			expect((await runWorker()).suspensions_cleared).toBe(0);
			expect(await getDecision(harness, staff, decisionId)).toMatchObject({status: 'ended', end_outcome: 'reversed'});
			expect(await emailsTo(harness, target, 'enforcement_ended')).toHaveLength(1);
		});

		test('clears the account but keeps the outcome when the named decision already ended', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			const endedAt = await expire(target);
			await new EnforcementDecisionRepository().end(BigInt(decisionId), {
				outcome: 'reversed',
				endedAt,
				endedBy: BigInt(staff.userId),
				statement: null,
			});
			expect((await runWorker()).suspensions_cleared).toBe(1);
			const cleared = await loadUser(target);
			expect(cleared.flags & UserFlags.DISABLED).toBe(0n);
			expect(cleared.enforcementDecisionId).toBeNull();
			expect(await getDecision(harness, staff, decisionId)).toMatchObject({status: 'ended', end_outcome: 'reversed'});
		});

		test('keeps a suspension applied after the scan read the account', async () => {
			const target = await createTestAccount(harness);
			const expiredId = await suspend(harness, staff, target.userId);
			await expire(target);
			const scanned = await loadUser(target);
			const currentId = await suspend(harness, staff, target.userId);
			const expiry = await clearExpiredSuspension(
				{users: getUserRepository(), repository: new EnforcementDecisionRepository()},
				scanned,
			);
			expect(expiry).toBeNull();
			const after = await loadUser(target);
			expect(after.flags & UserFlags.DISABLED).toBe(UserFlags.DISABLED);
			expect(after.tempBannedUntil!.getTime()).toBeGreaterThan(Date.now());
			expect(after.enforcementDecisionId).toBe(BigInt(currentId));
			expect(await getDecision(harness, staff, currentId)).toMatchObject({status: 'active', end_outcome: null});
			expect(await getDecision(harness, staff, expiredId)).toMatchObject({status: 'active', replaced: true});
		});

		test('keeps a reversal made between reading and ending the decision', async () => {
			const target = await createTestAccount(harness);
			const decisionId = await suspend(harness, staff, target.userId);
			await expire(target);
			const repository = new EnforcementDecisionRepository();
			const expiry = await clearExpiredSuspension(
				{
					users: getUserRepository(),
					repository: {
						get: async (id) => {
							const row = await repository.get(id);
							await endDecision(harness, staff, decisionId, 'reversed').expect(HTTP_STATUS.OK).execute();
							return row;
						},
						endIfActive: (id, end) => repository.endIfActive(id, end),
					},
				},
				await loadUser(target),
			);
			expect(expiry).not.toBeNull();
			expect(await getDecision(harness, staff, decisionId)).toMatchObject({status: 'ended', end_outcome: 'reversed'});
			expect(await emailsTo(harness, target, 'enforcement_ended')).toHaveLength(1);
		});
	});
});
