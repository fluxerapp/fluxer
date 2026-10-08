// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createAttachmentID, createReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getChannel, sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import type {IARMessageContextRow, IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {getAdminRepository, getRateLimitService} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {getReportFlowVariant, type ReportFlowStepInput} from '@app/api/report/flows/ReportFlowRegistry';
import {findReportReason, listReportReasons} from '@app/api/report/flows/ReportReasonCatalog';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {getReportSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {deleteAccount, setPendingDeletionAt} from '@app/api/user/tests/UserTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	type AdminReportReasonsResponse,
	ReportAdminResponseSchema,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {
	ReportFlowResponse,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	type ReportProfileSnapshot,
	serializeReportProfileSnapshot,
} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import type {z} from 'zod';

type AdminReport = z.infer<typeof ReportAdminResponseSchema>;

interface ReportResponse {
	report_id: string;
}

interface AdminReportList {
	reports: Array<AdminReport>;
	total: number;
	offset: number;
	limit: number;
}

interface MessageTargets {
	reporter: TestAccount;
	channelId: string;
	messageIds: Array<string>;
}

const RATE_LIMIT_HEADER = 'x-fluxer-test-enable-rate-limits';

const MESSAGE_CSAM_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'abuse'},
	{screen_id: 'abuse', option_id: 'sexual'},
	{screen_id: 'sexual', option_id: 'minor_sexual'},
	{screen_id: 'minor_sexual', option_id: 'csam'},
];

const MESSAGE_TERRORISM_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'violence_misinfo'},
	{screen_id: 'violence_misinfo', option_id: 'terrorism'},
];

const MESSAGE_PRIVATE_INFO_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'private_info'},
	{screen_id: 'private_info', item_ids: ['phone', 'email']},
];

const USER_CSAM_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'profile_intro'},
	{screen_id: 'profile_parts', item_ids: ['photo']},
	{screen_id: 'root_user', option_id: 'abuse'},
	{screen_id: 'profile_abuse', option_id: 'sexual'},
	{screen_id: 'profile_sexual', option_id: 'minor_sexual'},
	{screen_id: 'profile_minor_sexual', option_id: 'csam'},
];

const USER_HARASSMENT_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'profile_intro'},
	{screen_id: 'profile_parts', item_ids: ['photo', 'profile_text']},
	{screen_id: 'root_user', option_id: 'abuse'},
	{screen_id: 'profile_abuse', option_id: 'harassment'},
];

function currentHash(target: ReportFlowTargetType, surface: ReportFlowSurface = 'in_app'): string {
	return getReportFlowVariant(target, surface).revisionHash;
}

async function setupMessages(harness: ApiTestHarness, count: number): Promise<MessageTargets> {
	const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
	const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
	const messageIds: Array<string> = [];
	for (let index = 0; index < count; index++) {
		messageIds.push((await sendChannelMessage(harness, members[0].token, channel.id, `Reported ${index}`)).id);
	}
	return {reporter: owner, channelId: channel.id, messageIds};
}

async function submitMessageFlow(
	harness: ApiTestHarness,
	targets: MessageTargets,
	index: number,
	steps: ReadonlyArray<ReportFlowStepInput>,
	locale?: string,
): Promise<string> {
	const result = await createBuilder<ReportResponse>(harness, targets.reporter.token)
		.post('/reports/flows/message/submissions')
		.body({
			channel_id: targets.channelId,
			message_id: targets.messageIds[index],
			revision_hash: currentHash('message'),
			steps,
			...(locale ? {locale} : {}),
		})
		.expect(HTTP_STATUS.OK)
		.execute();
	return result.report_id;
}

async function submitUserFlow(
	harness: ApiTestHarness,
	reporter: TestAccount,
	userId: string,
	steps: ReadonlyArray<ReportFlowStepInput>,
	locale?: string,
): Promise<string> {
	const result = await createBuilder<ReportResponse>(harness, reporter.token)
		.post('/reports/flows/user/submissions')
		.body({user_id: userId, revision_hash: currentHash('user'), steps, ...(locale ? {locale} : {})})
		.expect(HTTP_STATUS.OK)
		.execute();
	return result.report_id;
}

async function submitLegacyMessage(harness: ApiTestHarness, targets: MessageTargets, index: number): Promise<string> {
	const result = await createBuilder<ReportResponse>(harness, targets.reporter.token)
		.post('/reports/message')
		.body({channel_id: targets.channelId, message_id: targets.messageIds[index], category: 'child_safety'})
		.expect(HTTP_STATUS.OK)
		.execute();
	return result.report_id;
}

async function submitLegacyUser(harness: ApiTestHarness, reporter: TestAccount, userId: string): Promise<string> {
	const result = await createBuilder<ReportResponse>(harness, reporter.token)
		.post('/reports/user')
		.body({user_id: userId, category: 'harassment'})
		.expect(HTTP_STATUS.OK)
		.execute();
	return result.report_id;
}

async function createReportAdmin(harness: ApiTestHarness, acls = ['admin:authenticate', 'report:view']) {
	return setUserACLs(harness, await createTestAccount(harness), acls);
}

function listReports(harness: ApiTestHarness, admin: TestAccount, query: string): Promise<AdminReportList> {
	return createBuilder<AdminReportList>(harness, admin.token)
		.get(`/admin/reports?${query}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

function getAdminReport(harness: ApiTestHarness, admin: TestAccount, reportId: string): Promise<AdminReport> {
	return createBuilder<AdminReport>(harness, admin.token)
		.get(`/admin/reports/${reportId}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

function reportIds(list: AdminReportList): Array<string> {
	return list.reports.map((report) => report.report_id).sort();
}

function fetchFlow(harness: ApiTestHarness, target: ReportFlowTargetType, locale: string): Promise<ReportFlowResponse> {
	return createBuilderWithoutAuth<ReportFlowResponse>(harness)
		.get(`/reports/flows/${target}?locale=${locale}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

function describeWalk(flow: ReportFlowResponse, steps: ReadonlyArray<ReportFlowStepInput>) {
	const screens = new Map(flow.screens.map((screen) => [screen.id, screen]));
	return steps.map((step) => {
		const screen = screens.get(step.screen_id)!;
		const option = step.option_id ? screen.options.find((candidate) => candidate.id === step.option_id)! : null;
		return {
			screen_id: step.screen_id,
			screen_title: screen.title,
			option_id: step.option_id ?? null,
			option_label: option?.label ?? null,
			items: (screen.checklist?.items ?? [])
				.filter((item) => step.item_ids?.includes(item.id))
				.map((item) => ({id: item.id, label: item.label})),
		};
	});
}

function expectNoFlowFields(report: AdminReport): void {
	expect(report.reason).toBeNull();
	expect(report.reason_label).toBeNull();
	expect(report.reason_highest_priority).toBeNull();
	expect(report.flow).toBeNull();
	expect(report.reporter_good_faith_confirmed).toBeNull();
}

let seedSequence = 8_000_000_000_000_000_000n;

function nextSeedId(): bigint {
	seedSequence += 1n;
	return seedSequence;
}

function buildReportRow(overrides: Partial<IARSubmissionRow> & Pick<IARSubmissionRow, 'report_id'>): IARSubmissionRow {
	return {
		reporter_id: null,
		reporter_email: null,
		reporter_full_legal_name: null,
		reporter_country_of_residence: null,
		reported_at: new Date(),
		status: 0,
		report_type: 0,
		category: 'other',
		additional_info: null,
		reported_user_id: null,
		reported_user_avatar_hash: null,
		reported_guild_id: null,
		reported_guild_name: null,
		reported_guild_icon_hash: null,
		reported_message_id: null,
		reported_channel_id: null,
		reported_channel_name: null,
		message_context: null,
		guild_context_id: null,
		resolved_at: null,
		resolved_by_admin_id: null,
		public_comment: null,
		audit_log_reason: null,
		reported_guild_invite_code: null,
		reported_guild_nsfw: null,
		reported_guild_content_warning_level: null,
		reported_guild_content_warning_text: null,
		reported_channel_nsfw_override: null,
		reported_channel_content_warning_level: null,
		reported_channel_content_warning_text: null,
		reported_channel_effective_nsfw: null,
		reported_channel_effective_content_warning_level: null,
		reported_channel_effective_content_warning_text: null,
		reason: null,
		flow_revision: null,
		flow_steps: null,
		flow_locale: null,
		flow_surface: null,
		reporter_good_faith_confirmed: null,
		reported_webhook_id: null,
		reported_webhook_name: null,
		reported_webhook_avatar_hash: null,
		reported_webhook_default_name: null,
		reported_webhook_default_avatar_hash: null,
		reported_webhook_type: null,
		reported_webhook_application_id: null,
		reported_webhook_channel_id: null,
		reported_webhook_guild_id: null,
		reported_webhook_created_at: null,
		reported_webhook_creator_id: null,
		reported_webhook_creator_username: null,
		reported_webhook_creator_discriminator: null,
		reported_webhook_creator_global_name: null,
		reported_webhook_creator_avatar_hash: null,
		...overrides,
	};
}

function buildContextRow(overrides: Partial<IARMessageContextRow>): IARMessageContextRow {
	return {
		message_id: nextSeedId(),
		channel_id: null,
		author_id: null,
		webhook_id: null,
		author_username: 'context_author',
		author_discriminator: 1234,
		author_avatar_hash: null,
		content: 'Context message',
		timestamp: new Date(),
		edited_timestamp: null,
		type: 0,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		...overrides,
	};
}

function buildAttachment(filename: string): MessageAttachment {
	return {
		attachment_id: createAttachmentID(nextSeedId()),
		filename,
		size: 2048n,
		title: null,
		description: null,
		width: 640,
		height: 480,
		content_type: 'image/png',
		content_hash: null,
		placeholder: null,
		flags: 0,
		duration: null,
		nsfw: false,
		waveform: null,
	};
}

async function seedReport(overrides: Partial<IARSubmissionRow> = {}): Promise<string> {
	const row = buildReportRow({report_id: nextSeedId(), ...overrides});
	await new ReportRepository().createReport(row);
	return row.report_id.toString();
}

async function setBotFlag(harness: ApiTestHarness, userId: string): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${userId}/set-bot-flag`)
		.body({is_bot: true})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function issueDsaTicket(harness: ApiTestHarness): Promise<string> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-admin');
	await createBuilderWithoutAuth(harness)
		.post('/reports/dsa/email/send')
		.body({email})
		.expect(HTTP_STATUS.OK)
		.execute();
	const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
	const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
		.post('/reports/dsa/email/verify')
		.body({email, code})
		.expect(HTTP_STATUS.OK)
		.execute();
	return ticket;
}

describe('Report flow admin', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	describe('Search', () => {
		test('the reason filter finds message and user flow reports', async () => {
			const messages = await setupMessages(harness, 3);
			const messageCsam = await submitMessageFlow(harness, messages, 0, MESSAGE_CSAM_WALK);
			const messageTerrorism = await submitMessageFlow(harness, messages, 1, MESSAGE_TERRORISM_WALK);
			const legacyChildSafety = await submitLegacyMessage(harness, messages, 2);
			const reporter = await createTestAccount(harness);
			const [csamTarget, harassmentTarget] = await Promise.all([
				createTestAccount(harness),
				createTestAccount(harness),
			]);
			const userCsam = await submitUserFlow(harness, reporter, csamTarget.userId, USER_CSAM_WALK);
			const userHarassment = await submitUserFlow(harness, reporter, harassmentTarget.userId, USER_HARASSMENT_WALK);
			const admin = await createReportAdmin(harness);

			const csam = await listReports(harness, admin, 'reason=csam');
			expect(reportIds(csam)).toEqual([messageCsam, userCsam].sort());
			expect(csam.total).toBe(2);
			for (const report of csam.reports) {
				expect(report.reason).toBe('csam');
				expect(report.reason_label).toBe('Child sexual abuse material');
				expect(report.reason_highest_priority).toBe(true);
				expect(report.category).toBe('child_safety');
			}
			expect(reportIds(await listReports(harness, admin, 'reason=csam&report_type=message'))).toEqual([messageCsam]);
			expect(reportIds(await listReports(harness, admin, 'reason=csam&report_type=user'))).toEqual([userCsam]);
			expect(reportIds(await listReports(harness, admin, 'reason=terrorism_extremism'))).toEqual([messageTerrorism]);
			expect(reportIds(await listReports(harness, admin, 'reason=harassment'))).toEqual([userHarassment]);
			expect(reportIds(await listReports(harness, admin, 'reason=raid'))).toEqual([]);
			expect(reportIds(await listReports(harness, admin, 'category=child_safety'))).toEqual(
				[messageCsam, userCsam, legacyChildSafety].sort(),
			);

			const searchService = getReportSearchService()!;
			const {hits} = await searchService.searchReports('', {reason: 'csam'});
			expect(hits.map((hit) => hit.id).sort()).toEqual([messageCsam, userCsam].sort());
			expect(hits.every((hit) => hit.reason === 'csam')).toBe(true);
		});

		test('a free-text search for a reason key finds nothing', async () => {
			const messages = await setupMessages(harness, 2);
			const messageCsam = await submitMessageFlow(harness, messages, 0, MESSAGE_CSAM_WALK);
			await submitMessageFlow(harness, messages, 1, MESSAGE_TERRORISM_WALK);
			const admin = await createReportAdmin(harness);
			for (const key of ['csam', 'terrorism_extremism']) {
				const result = await listReports(harness, admin, `q=${key}`);
				expect(result.reports).toEqual([]);
				expect(result.total).toBe(0);
			}
			expect(reportIds(await listReports(harness, admin, 'q=child_safety'))).toEqual([messageCsam]);
		});

		test('a status filter with a reason uses the search index and records the reason', async () => {
			const messages = await setupMessages(harness, 2);
			const messageCsam = await submitMessageFlow(harness, messages, 0, MESSAGE_CSAM_WALK);
			await submitLegacyMessage(harness, messages, 1);
			const admin = await createReportAdmin(harness);
			expect(reportIds(await listReports(harness, admin, 'status=pending'))).toHaveLength(2);
			const before = new Set(
				(await getAdminRepository().listAllAuditLogsPaginated(100000)).map((log) => log.logId.toString()),
			);
			const filtered = await listReports(harness, admin, 'status=pending&reason=csam');
			expect(reportIds(filtered)).toEqual([messageCsam]);
			expect(filtered.total).toBe(1);
			const recorded = (await getAdminRepository().listAllAuditLogsPaginated(100000)).filter(
				(log) => !before.has(log.logId.toString()),
			);
			expect(recorded).toHaveLength(1);
			expect(recorded[0].action).toBe(AdminAuditReadActions.SEARCH_REPORTS);
			expect(Object.fromEntries(recorded[0].metadata)).toMatchObject({
				status: 'pending',
				reason: 'csam',
				sort_by: 'reported_at',
				result_count: '1',
			});
		});
	});

	describe('Report detail', () => {
		test('flow answers are described in English for a reporter who used German', async () => {
			const messages = await setupMessages(harness, 1);
			const reportId = await submitMessageFlow(harness, messages, 0, MESSAGE_PRIVATE_INFO_WALK, 'de');
			const admin = await createReportAdmin(harness);
			const english = await fetchFlow(harness, 'message', 'en-US');
			const german = await fetchFlow(harness, 'message', 'de');
			const expectedSteps = describeWalk(english, MESSAGE_PRIVATE_INFO_WALK);
			const germanSteps = describeWalk(german, MESSAGE_PRIVATE_INFO_WALK);
			expect(germanSteps).not.toEqual(expectedSteps);

			const report = await getAdminReport(harness, admin, reportId);
			expect(report.reason).toBe('doxxing');
			expect(report.reason_label).toBe(findReportReason('doxxing')!.label);
			expect(report.reason_highest_priority).toBe(false);
			expect(report.category).toBe('doxxing');
			expect(report.reporter_good_faith_confirmed).toBeNull();
			expect(report.flow).toEqual({
				revision_hash: currentHash('message'),
				surface: 'in_app',
				locale: 'de',
				steps: expectedSteps,
			});

			const listed = await listReports(harness, admin, 'reason=doxxing');
			expect(listed.reports).toHaveLength(1);
			expect(listed.reports[0].flow).toEqual(report.flow);
		});

		test('an info step is described with a null option and no items', async () => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const reportId = await submitUserFlow(harness, reporter, target.userId, USER_HARASSMENT_WALK, 'ja');
			const admin = await createReportAdmin(harness);
			const english = await fetchFlow(harness, 'user', 'en-US');

			const report = await getAdminReport(harness, admin, reportId);
			expect(report.reason).toBe('harassment');
			expect(report.reason_label).toBe('Harassment or bullying');
			expect(report.category).toBe('harassment');
			expect(report.flow?.locale).toBe('ja');
			expect(report.flow?.steps).toEqual(describeWalk(english, USER_HARASSMENT_WALK));
			expect(report.flow?.steps[0]).toEqual({
				screen_id: 'profile_intro',
				screen_title: english.screens.find((screen) => screen.id === 'profile_intro')!.title,
				option_id: null,
				option_label: null,
				items: [],
			});
			expect(report.flow?.steps[1].option_id).toBeNull();
			expect(report.flow?.steps[1].items.map((item) => item.id)).toEqual(['photo', 'profile_text']);
		});

		test('legacy reports return the new fields as null', async () => {
			const messages = await setupMessages(harness, 1);
			const legacyMessage = await submitLegacyMessage(harness, messages, 0);
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const legacyUser = await submitLegacyUser(harness, reporter, target.userId);
			const admin = await createReportAdmin(harness);

			for (const reportId of [legacyMessage, legacyUser]) {
				expectNoFlowFields(await getAdminReport(harness, admin, reportId));
			}
			const listed = await listReports(harness, admin, 'status=pending');
			expect(reportIds(listed)).toEqual([legacyMessage, legacyUser].sort());
			for (const report of listed.reports) {
				expectNoFlowFields(report);
			}
			const searched = await listReports(harness, admin, 'category=harassment');
			expect(reportIds(searched)).toEqual([legacyUser]);
			expectNoFlowFields(searched.reports[0]);
		});

		test('unknown reason keys, screens and options fall back to raw ids', async () => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const reportId = await submitUserFlow(harness, reporter, target.userId, USER_HARASSMENT_WALK);
			const repository = new ReportRepository();
			const stored = (await repository.getReport(createReportID(BigInt(reportId))))!;
			await repository.createReport(
				buildReportRow({
					report_id: BigInt(reportId),
					reporter_id: BigInt(reporter.userId),
					reported_at: stored.reportedAt,
					report_type: 1,
					reported_user_id: BigInt(target.userId),
					reason: 'future_reason',
					flow_revision: 'feedfacefeedface',
					flow_steps: JSON.stringify([
						{screen_id: 'future_screen', option_id: 'future_option'},
						{screen_id: 'root_user', option_id: 'future_option'},
						{screen_id: 'profile_parts', item_ids: ['photo', 'future_item']},
					]),
					flow_surface: 'in_app',
				}),
			);
			const admin = await createReportAdmin(harness);
			const english = await fetchFlow(harness, 'user', 'en-US');
			const screenTitle = (id: string) => english.screens.find((screen) => screen.id === id)!.title;
			const photoLabel = english.screens
				.find((screen) => screen.id === 'profile_parts')!
				.checklist!.items.find((item) => item.id === 'photo')!.label;

			const report = await getAdminReport(harness, admin, reportId);
			expect(report.reason).toBe('future_reason');
			expect(report.reason_label).toBe('future_reason');
			expect(report.reason_highest_priority).toBeNull();
			expect(report.flow).toEqual({
				revision_hash: 'feedfacefeedface',
				surface: 'in_app',
				locale: null,
				steps: [
					{
						screen_id: 'future_screen',
						screen_title: 'future_screen',
						option_id: 'future_option',
						option_label: 'future_option',
						items: [],
					},
					{
						screen_id: 'root_user',
						screen_title: screenTitle('root_user'),
						option_id: 'future_option',
						option_label: 'future_option',
						items: [],
					},
					{
						screen_id: 'profile_parts',
						screen_title: screenTitle('profile_parts'),
						option_id: null,
						option_label: null,
						items: [
							{id: 'photo', label: photoLabel},
							{id: 'future_item', label: 'future_item'},
						],
					},
				],
			});
		});

		test('a DSA flow report shows the good-faith statement and the DSA surface', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
			const message = await sendChannelMessage(harness, members[0].token, channel.id, 'Reported on the DSA form');
			const ticket = await issueDsaTicket(harness);
			const {report_id} = await createBuilderWithoutAuth<ReportResponse>(harness)
				.post('/reports/dsa')
				.body({
					ticket,
					report_type: 'message',
					message_link: `https://web.fluxer.app/channels/${guild.id}/${channel.id}/${message.id}`,
					revision_hash: currentHash('message', 'dsa'),
					steps: MESSAGE_CSAM_WALK,
					good_faith_confirmed: true,
					additional_info: 'This message shares child sexual abuse material.',
					reporter_country_of_residence: 'DE',
					locale: 'fr',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const admin = await createReportAdmin(harness);
			const report = await getAdminReport(harness, admin, report_id);
			expect(report.reason).toBe('csam');
			expect(report.reason_highest_priority).toBe(true);
			expect(report.reporter_good_faith_confirmed).toBe(true);
			expect(report.flow?.surface).toBe('dsa');
			expect(report.flow?.locale).toBe('fr');
			expect(report.flow?.revision_hash).toBe(currentHash('message', 'dsa'));
			expect(report.flow?.steps.map((step) => step.option_id)).toEqual(MESSAGE_CSAM_WALK.map((step) => step.option_id));
		});
	});

	describe('Stored evidence', () => {
		test('a context message with no channel is left out and the detail still parses', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			const channelId = BigInt(guild.system_channel_id!);
			const kept = buildContextRow({channel_id: channelId, author_id: BigInt(owner.userId)});
			const withoutChannel = buildContextRow({author_id: BigInt(owner.userId)});
			const reportId = await seedReport({
				reported_message_id: kept.message_id,
				reported_user_id: BigInt(owner.userId),
				message_context: [withoutChannel, kept],
			});
			const admin = await createReportAdmin(harness);

			const report = await getAdminReport(harness, admin, reportId);
			expect(report.reported_channel_id).toBeNull();
			expect(report.message_context?.map((message) => message.id)).toEqual([kept.message_id.toString()]);
			for (const message of report.message_context ?? []) {
				expect(message.channel_id).toMatch(/^(0|[1-9][0-9]*)$/);
			}
			expect(ReportAdminResponseSchema.safeParse(report).success).toBe(true);
		});

		test('a report whose context has no channel at all returns an empty context', async () => {
			const reportId = await seedReport({message_context: [buildContextRow({}), buildContextRow({})]});
			const admin = await createReportAdmin(harness);
			const report = await getAdminReport(harness, admin, reportId);
			expect(report.message_context).toEqual([]);
			expect(report.message_responses).toEqual([]);
			expect(ReportAdminResponseSchema.safeParse(report).success).toBe(true);
		});

		test('bot flags are true for a bot, false for a person and null for a webhook', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const channelId = BigInt(guild.system_channel_id!);
			const bot = members[0];
			await setBotFlag(harness, bot.userId);
			const botMessage = buildContextRow({channel_id: channelId, author_id: BigInt(bot.userId)});
			const personMessage = buildContextRow({channel_id: channelId, author_id: BigInt(owner.userId)});
			const webhookMessage = buildContextRow({channel_id: channelId, webhook_id: nextSeedId()});
			const unknownMessage = buildContextRow({channel_id: channelId, author_id: nextSeedId()});
			const context = [botMessage, personMessage, webhookMessage, unknownMessage];
			const botReport = await seedReport({
				reported_user_id: BigInt(bot.userId),
				reported_channel_id: channelId,
				reported_message_id: botMessage.message_id,
				message_context: context,
			});
			const personReport = await seedReport({report_type: 1, reported_user_id: BigInt(owner.userId)});
			const guildReport = await seedReport({report_type: 2, reported_guild_id: BigInt(guild.id)});
			const unknownReport = await seedReport({report_type: 1, reported_user_id: nextSeedId()});
			const admin = await createReportAdmin(harness);

			const detail = await getAdminReport(harness, admin, botReport);
			expect(detail.reported_user_bot).toBe(true);
			expect(detail.message_context?.map((message) => [message.id, message.author_bot])).toEqual([
				[botMessage.message_id.toString(), true],
				[personMessage.message_id.toString(), false],
				[webhookMessage.message_id.toString(), null],
				[unknownMessage.message_id.toString(), null],
			]);
			expect(ReportAdminResponseSchema.safeParse(detail).success).toBe(true);
			expect((await getAdminReport(harness, admin, personReport)).reported_user_bot).toBe(false);
			expect((await getAdminReport(harness, admin, guildReport)).reported_user_bot).toBeNull();
			expect((await getAdminReport(harness, admin, unknownReport)).reported_user_bot).toBeNull();
		});

		test('a listed report states whether the reported account is a bot', async () => {
			const reporter = await createTestAccount(harness);
			const [bot, person] = await Promise.all([createTestAccount(harness), createTestAccount(harness)]);
			await setBotFlag(harness, bot.userId);
			const botReport = await submitLegacyUser(harness, reporter, bot.userId);
			const personReport = await submitLegacyUser(harness, reporter, person.userId);
			const admin = await createReportAdmin(harness);
			const listed = await listReports(harness, admin, 'status=pending');
			const flags = new Map(listed.reports.map((report) => [report.report_id, report.reported_user_bot]));
			expect(flags.get(botReport)).toBe(true);
			expect(flags.get(personReport)).toBe(false);
		});

		test('attachments that were not preserved are listed without a download URL', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			const channelId = BigInt(guild.system_channel_id!);
			const preserved = buildAttachment('kept.png');
			const missing = buildAttachment('lost.png');
			const withGap = buildContextRow({
				channel_id: channelId,
				author_id: BigInt(owner.userId),
				attachments: [preserved],
				missing_attachments: [missing],
			});
			const withoutGap = buildContextRow({channel_id: channelId, author_id: BigInt(owner.userId)});
			const reportId = await seedReport({
				reported_channel_id: channelId,
				reported_message_id: withGap.message_id,
				message_context: [withGap, withoutGap],
			});
			const admin = await createReportAdmin(harness);

			const report = await getAdminReport(harness, admin, reportId);
			const [first, second] = report.message_context ?? [];
			expect(first.attachments.map((attachment) => attachment.filename)).toEqual(['kept.png']);
			expect(first.attachments[0].url).toBe('https://presigned.url/test');
			expect(first.missing_attachments).toEqual([
				{
					id: missing.attachment_id.toString(),
					filename: 'lost.png',
					nsfw: false,
					content_type: 'image/png',
					width: 640,
					height: 480,
					size: 2048,
				},
			]);
			expect(second.attachments).toEqual([]);
			expect(second.missing_attachments).toEqual([]);
			expect(ReportAdminResponseSchema.safeParse(report).success).toBe(true);
		});

		test('a stored profile snapshot is returned with download URLs for the copied assets', async () => {
			const target = await createTestAccount(harness);
			const {guild} = await setupTestGuildWithMembers(harness, 0);
			const reportIdValue = nextSeedId();
			const assetKey = (kind: string, hash: string) => `reports/${reportIdValue}/profile/${kind}/${hash}`;
			const snapshot: ReportProfileSnapshot = {
				captured_at: '2026-10-01T12:00:00.000Z',
				user: {
					id: target.userId,
					username: 'name_at_report_time',
					discriminator: 7,
					global_name: 'Name At Report Time',
					bio: 'Bio at report time',
					pronouns: 'they/them',
					avatar: {hash: 'avatarhash', key: assetKey('user_avatar', 'avatarhash')},
					banner: {hash: 'bannerhash', key: null},
				},
				member: {
					guild_id: guild.id,
					nick: 'Nick at report time',
					bio: null,
					pronouns: null,
					joined_at: '2026-09-01T08:00:00.000Z',
					avatar: {hash: 'memberavatar', key: assetKey('member_avatar', 'memberavatar')},
					banner: null,
				},
				guild: null,
			};
			const reportId = await seedReport({
				report_id: reportIdValue,
				report_type: 1,
				reported_user_id: BigInt(target.userId),
				guild_context_id: BigInt(guild.id),
				reported_profile_snapshot: serializeReportProfileSnapshot(snapshot),
			});
			const admin = await createReportAdmin(harness);
			harness.storageService.getPresignedDownloadURLSpy.mockClear();

			const report = await getAdminReport(harness, admin, reportId);
			expect(report.reported_profile_snapshot).toEqual({
				captured_at: '2026-10-01T12:00:00.000Z',
				user: {
					id: target.userId,
					username: 'name_at_report_time',
					discriminator: '0007',
					global_name: 'Name At Report Time',
					bio: 'Bio at report time',
					pronouns: 'they/them',
					avatar: {hash: 'avatarhash', url: 'https://presigned.url/test'},
					banner: {hash: 'bannerhash', url: null},
				},
				member: {
					guild_id: guild.id,
					nick: 'Nick at report time',
					bio: null,
					pronouns: null,
					joined_at: '2026-09-01T08:00:00.000Z',
					avatar: {hash: 'memberavatar', url: 'https://presigned.url/test'},
					banner: null,
				},
				guild: null,
			});
			expect(harness.storageService.getPresignedDownloadURLSpy.mock.calls.map(([params]) => params)).toEqual([
				{bucket: Config.s3.buckets.reports, key: assetKey('user_avatar', 'avatarhash'), expiresIn: 300},
				{bucket: Config.s3.buckets.reports, key: assetKey('member_avatar', 'memberavatar'), expiresIn: 300},
			]);
			expect(ReportAdminResponseSchema.safeParse(report).success).toBe(true);
		});

		test('a community snapshot is returned and a report without one returns null', async () => {
			const {guild} = await setupTestGuildWithMembers(harness, 0);
			const reportIdValue = nextSeedId();
			const iconKey = `reports/${reportIdValue}/profile/guild_icon/iconhash`;
			const withSnapshot = await seedReport({
				report_id: reportIdValue,
				report_type: 2,
				reported_guild_id: BigInt(guild.id),
				reported_profile_snapshot: serializeReportProfileSnapshot({
					captured_at: '2026-10-01T12:00:00.000Z',
					user: null,
					member: null,
					guild: {
						id: guild.id,
						name: 'Name at report time',
						vanity_url_code: null,
						icon: {hash: 'iconhash', key: iconKey},
						banner: null,
						splash: null,
					},
				}),
			});
			const withoutSnapshot = await seedReport({report_type: 2, reported_guild_id: BigInt(guild.id)});
			const unreadable = await seedReport({
				report_type: 2,
				reported_guild_id: BigInt(guild.id),
				reported_profile_snapshot: '{"captured_at":',
			});
			const admin = await createReportAdmin(harness);

			expect((await getAdminReport(harness, admin, withSnapshot)).reported_profile_snapshot).toEqual({
				captured_at: '2026-10-01T12:00:00.000Z',
				user: null,
				member: null,
				guild: {
					id: guild.id,
					name: 'Name at report time',
					vanity_url_code: null,
					icon: {hash: 'iconhash', url: 'https://presigned.url/test'},
					banner: null,
					splash: null,
				},
			});
			expect((await getAdminReport(harness, admin, withoutSnapshot)).reported_profile_snapshot).toBeNull();
			expect((await getAdminReport(harness, admin, unreadable)).reported_profile_snapshot).toBeNull();
		});

		test('a listed report has no profile snapshot field', async () => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const reportId = await submitLegacyUser(harness, reporter, target.userId);
			const admin = await createReportAdmin(harness);
			const listed = await listReports(harness, admin, 'status=pending');
			expect(reportIds(listed)).toEqual([reportId]);
			expect('reported_profile_snapshot' in listed.reports[0]).toBe(false);
			expect('message_context' in listed.reports[0]).toBe(false);
		});
	});

	describe('Reporter contact details', () => {
		test('the reporter email shown is the one on the reporter account', async () => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const filed = await submitLegacyUser(harness, reporter, target.userId);
			const seeded = await seedReport({
				report_type: 1,
				reporter_id: BigInt(reporter.userId),
				reporter_email: 'address-at-report-time@example.com',
				reported_user_id: BigInt(target.userId),
			});
			const admin = await createReportAdmin(harness, ['admin:authenticate', 'report:view', 'report:view:reporter_pii']);
			const adminWithoutContact = await createReportAdmin(harness);

			for (const reportId of [filed, seeded]) {
				expect((await getAdminReport(harness, admin, reportId)).reporter_email).toBe(reporter.email);
				expect((await getAdminReport(harness, adminWithoutContact, reportId)).reporter_email).toBeNull();
			}
			const listed = await listReports(harness, admin, 'status=pending');
			expect(listed.reports.map((report) => report.reporter_email)).toEqual([reporter.email]);
			expect((await listReports(harness, adminWithoutContact, 'status=pending')).reports[0].reporter_email).toBeNull();
		});

		test('a report from a deleted account shows no reporter email', async () => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const filed = await submitLegacyUser(harness, reporter, target.userId);
			const seeded = await seedReport({
				report_type: 1,
				reporter_id: BigInt(reporter.userId),
				reporter_email: reporter.email,
				reported_user_id: BigInt(target.userId),
			});
			const admin = await createReportAdmin(harness, ['admin:authenticate', 'report:view', 'report:view:reporter_pii']);
			expect((await getAdminReport(harness, admin, filed)).reporter_email).toBe(reporter.email);

			await deleteAccount(harness, reporter.token, reporter.password);
			await setPendingDeletionAt(harness, reporter.userId, new Date(Date.now() - 60_000));
			await createBuilderWithoutAuth(harness)
				.post(`/test/worker/process-pending-deletion/${reporter.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();

			for (const reportId of [filed, seeded]) {
				const report = await getAdminReport(harness, admin, reportId);
				expect(report.reporter_id).toBe(reporter.userId);
				expect(report.reporter_email).toBeNull();
				expect(JSON.stringify(report)).not.toContain(reporter.email);
			}
		});

		test('a report from an account that no longer exists shows no reporter email', async () => {
			const seeded = await seedReport({
				report_type: 1,
				reporter_id: nextSeedId(),
				reporter_email: 'address-at-report-time@example.com',
			});
			const admin = await createReportAdmin(harness, ['admin:authenticate', 'report:view', 'report:view:reporter_pii']);
			expect((await getAdminReport(harness, admin, seeded)).reporter_email).toBeNull();
		});

		test('a DSA report keeps the email the reporter verified', async () => {
			const seeded = await seedReport({
				report_type: 1,
				reporter_email: 'dsa-reporter@example.com',
				reporter_full_legal_name: 'Dana Reporter',
				reporter_country_of_residence: 'DE',
			});
			const admin = await createReportAdmin(harness, ['admin:authenticate', 'report:view', 'report:view:reporter_pii']);
			const adminWithoutContact = await createReportAdmin(harness);
			const report = await getAdminReport(harness, admin, seeded);
			expect(report.reporter_id).toBeNull();
			expect(report.reporter_email).toBe('dsa-reporter@example.com');
			expect(report.reporter_full_legal_name).toBe('Dana Reporter');
			expect((await getAdminReport(harness, adminWithoutContact, seeded)).reporter_email).toBeNull();
		});
	});

	describe('GET /admin/report-reasons', () => {
		test('requires report:view', async () => {
			const admin = await createReportAdmin(harness, ['admin:authenticate']);
			await createBuilder(harness, admin.token)
				.get('/admin/report-reasons')
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACL)
				.execute();
		});

		test('lists every reason with its English label, priority and legacy categories', async () => {
			const admin = await createReportAdmin(harness);
			const response = await createBuilder<AdminReportReasonsResponse>(harness, admin.token)
				.get('/admin/report-reasons')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(response.reasons).toEqual(
				listReportReasons().map((reason) => ({
					key: reason.key,
					label: reason.label,
					highest_priority: reason.highestPriority,
					legacy_category_message: reason.legacyCategories.message,
					legacy_category_user: reason.legacyCategories.user,
					legacy_category_guild: reason.legacyCategories.guild,
				})),
			);
			expect(new Set(response.reasons.map((reason) => reason.key)).size).toBe(response.reasons.length);
			expect(response.reasons.find((reason) => reason.key === 'csam')).toEqual({
				key: 'csam',
				label: 'Child sexual abuse material',
				highest_priority: true,
				legacy_category_message: 'child_safety',
				legacy_category_user: 'child_safety',
				legacy_category_guild: 'child_safety',
			});
			expect(response.reasons.find((reason) => reason.key === 'raid')).toMatchObject({
				legacy_category_message: 'harassment',
				legacy_category_guild: 'raid_coordination',
			});
		});

		test('writes a list_report_reasons read audit entry', async () => {
			const admin = await createReportAdmin(harness);
			const before = new Set(
				(await getAdminRepository().listAllAuditLogsPaginated(100000)).map((log) => log.logId.toString()),
			);
			await createBuilder(harness, admin.token).get('/admin/report-reasons').expect(HTTP_STATUS.OK).execute();
			const recorded = (await getAdminRepository().listAllAuditLogsPaginated(100000)).filter(
				(log) => !before.has(log.logId.toString()),
			);
			expect(recorded).toHaveLength(1);
			expect(recorded[0].action).toBe('list_report_reasons');
			expect(recorded[0].targetType).toBe('report');
			expect(recorded[0].adminUserId.toString()).toBe(admin.userId);
			expect(Object.fromEntries(recorded[0].metadata)).toEqual({
				result_count: String(listReportReasons().length),
			});
		});

		test('shares the admin lookup rate limit bucket', async () => {
			const admin = await createReportAdmin(harness);
			await createBuilder(harness, admin.token)
				.get('/admin/report-reasons')
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(HTTP_STATUS.OK)
				.execute();
			const bucket = `user:${admin.userId}:session:${RateLimitConfigs.ADMIN_LOOKUP.bucket}`;
			const rateLimitService = getRateLimitService();
			for (let attempt = 0; attempt < RateLimitConfigs.ADMIN_LOOKUP.config.limit * 2; attempt++) {
				const result = await rateLimitService.checkBucketLimit(bucket, {
					...RateLimitConfigs.ADMIN_LOOKUP.config,
					algorithm: 'leaky_bucket',
				});
				if (!result.allowed) break;
			}
			await createBuilder(harness, admin.token)
				.get('/admin/report-reasons')
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
			await createBuilder(harness, admin.token)
				.get('/admin/reports?reason=csam')
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
		});
	});
});
