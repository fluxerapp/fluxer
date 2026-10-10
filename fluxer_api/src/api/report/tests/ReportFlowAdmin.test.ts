// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {getChannel, sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import type {IARMessageContextRow, IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {getAdminRepository, getRateLimitService} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {getReportFlowVariant, type ReportFlowStepInput} from '@app/api/report/flows/ReportFlowRegistry';
import {listReportReasons} from '@app/api/report/flows/ReportReasonCatalog';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {deleteAccount, setPendingDeletionAt} from '@app/api/user/tests/UserTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ReportAdminResponseSchema} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {ReportFlowSurface, ReportFlowTargetType} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
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

async function seedReport(overrides: Partial<IARSubmissionRow> = {}): Promise<string> {
	const row = buildReportRow({report_id: nextSeedId(), ...overrides});
	await new ReportRepository().createReport(row);
	return row.report_id.toString();
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
