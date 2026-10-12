// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID, createReportID, createUserID} from '@app/api/BrandedTypes';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import type {IARMessageContextRow, IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {
	DSAReportEmailVerifications,
	DSAReportTickets,
	GuildReportSubmissionsByReporter,
	IARSubmissions,
	MessageReportSubmissionsByReporter,
} from '@app/api/Tables';
import {dockerAvailable, startPostgresKv, stopPostgresKv} from '@app/api/test/DockerTestContainer';
import {ReportAlreadyResolvedError} from '@fluxer/errors/src/domains/moderation/ReportAlreadyResolvedError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import {
	type ReportProfileSnapshot,
	serializeReportProfileSnapshot,
} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const KV_TABLE = 'kv_report_repository';
function attachment(id: bigint, filename: string): MessageAttachment {
	return {
		attachment_id: createAttachmentID(id),
		filename,
		size: 1234n,
		title: null,
		description: null,
		width: 10,
		height: 20,
		content_type: 'image/png',
		content_hash: 'hash',
		placeholder: null,
		flags: 0,
		duration: null,
		nsfw: false,
		waveform: null,
	};
}

function contextEntry(overrides: Partial<IARMessageContextRow> = {}): IARMessageContextRow {
	return {
		message_id: 30n,
		channel_id: 20n,
		author_id: 40n,
		webhook_id: null,
		author_username: 'author',
		author_discriminator: 1,
		author_avatar_hash: null,
		content: 'hello',
		timestamp: new Date('2026-10-01T10:00:00.000Z'),
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

function reportRow(reportId: bigint, overrides: Partial<IARSubmissionRow> = {}): IARSubmissionRow {
	return {
		report_id: reportId,
		reporter_id: 10n,
		reporter_email: 'reporter@example.com',
		reporter_full_legal_name: null,
		reporter_country_of_residence: null,
		reported_at: new Date('2026-10-01T10:00:00.000Z'),
		status: 0,
		report_type: 0,
		category: 'spam',
		additional_info: null,
		reported_user_id: 40n,
		reported_user_avatar_hash: null,
		reported_guild_id: null,
		reported_guild_name: null,
		reported_guild_icon_hash: null,
		reported_message_id: 30n,
		reported_channel_id: 20n,
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

const snapshot: ReportProfileSnapshot = {
	captured_at: '2026-10-01T10:00:00.000Z',
	user: {
		id: '40',
		username: 'target',
		discriminator: 1,
		global_name: 'Target',
		bio: 'Original bio',
		pronouns: null,
		avatar: {hash: 'avatar', key: 'reports/100/profile/avatar/avatar'},
		banner: null,
	},
	member: null,
	guild: null,
};

describe.skipIf(!dockerAvailable)('ReportRepository on Postgres KV', () => {
	let raw: IPostgresClient;
	const repository = new ReportRepository();

	async function storedRows(
		table: string,
	): Promise<Array<{row_data: Record<string, unknown>; expires_at: Date | null}>> {
		const result = await raw.query<{row_data: Record<string, unknown>; expires_at: Date | null}>(
			`SELECT row_data, expires_at FROM ${KV_TABLE} WHERE table_name = $1`,
			[table],
		);
		return result.rows;
	}

	function expectExpiresIn(value: Date | null, ttlSeconds: number): void {
		expect(value).toBeInstanceOf(Date);
		const remainingSeconds = (value!.getTime() - Date.now()) / 1000;
		expect(remainingSeconds).toBeGreaterThan(ttlSeconds - 60);
		expect(remainingSeconds).toBeLessThanOrEqual(ttlSeconds);
	}

	let postgresContainer: string;
	beforeAll(async () => {
		const started = await startPostgresKv({namePrefix: 'fluxer-reportkv', kvTable: KV_TABLE, maxConnections: 8});
		postgresContainer = started.container;
		raw = started.client;
	}, 900_000);

	beforeEach(async () => {
		setCassandraQueryExecutorForTesting(new PostgresKvQueryExecutor(raw));
		await raw.query(`DELETE FROM ${KV_TABLE}`);
	});

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(null);
		await stopPostgresKv(postgresContainer);
	});

	it('round-trips the context channel, missing attachments, profile snapshot and hold columns', async () => {
		const holdUntil = new Date('2027-10-01T00:00:00.000Z');
		await repository.createReport(
			reportRow(100n, {
				message_context: [
					contextEntry({
						attachments: [attachment(1n, 'kept.png')],
						missing_attachments: [attachment(2n, 'lost.png')],
					}),
				],
				reported_profile_snapshot: serializeReportProfileSnapshot(snapshot),
				legal_hold_until: holdUntil,
				legal_hold_reason: 'Court order',
			}),
		);
		const report = await repository.getReport(createReportID(100n));
		expect(report?.messageContext?.[0]?.channelId).toBe(20n);
		expect(report?.messageContext?.[0]?.attachments.map((entry) => entry.filename)).toEqual(['kept.png']);
		expect(report?.messageContext?.[0]?.missingAttachments).toEqual([attachment(2n, 'lost.png')]);
		expect(report?.reportedProfileSnapshot).toEqual(snapshot);
		expect(report?.legalHoldUntil?.getTime()).toBe(holdUntil.getTime());
		expect(report?.legalHoldReason).toBe('Court order');
	});

	it('reads rows without the new columns as empty', async () => {
		await repository.createReport(reportRow(101n, {message_context: [contextEntry({channel_id: null})]}));
		const report = await repository.getReport(createReportID(101n));
		expect(report?.messageContext?.[0]?.channelId).toBeNull();
		expect(report?.messageContext?.[0]?.missingAttachments).toEqual([]);
		expect(report?.reportedProfileSnapshot).toBeNull();
		expect(report?.legalHoldUntil).toBeNull();
		expect(report?.legalHoldReason).toBeNull();
	});

	it('reads an unreadable profile snapshot as null', async () => {
		await repository.createReport(reportRow(102n, {reported_profile_snapshot: '{"captured_at":'}));
		expect((await repository.getReport(createReportID(102n)))?.reportedProfileSnapshot).toBeNull();
	});

	it('does not resolve a report another resolve finished after it was read', async () => {
		await repository.createReport(reportRow(112n));
		const reportId = createReportID(112n);
		const pending = await repository.getReport(reportId);
		await repository.resolveReport(reportId, createUserID(1n), 'first', 'first reason');
		const staleRepository = new ReportRepository();
		vi.spyOn(staleRepository, 'getReport').mockResolvedValueOnce(pending);
		await expect(
			staleRepository.resolveReport(reportId, createUserID(2n), 'second', 'second reason'),
		).rejects.toBeInstanceOf(ReportAlreadyResolvedError);
		const stored = await repository.getReport(reportId);
		expect(stored?.publicComment).toBe('first');
		expect(stored?.auditLogReason).toBe('first reason');
		expect(stored?.resolvedByAdminId).toBe(1n);
	});

	it('resolves a report once when several resolves race', async () => {
		await repository.createReport(reportRow(110n));
		const results = await Promise.allSettled(
			['first', 'second', 'third', 'fourth', 'fifth'].map((comment) =>
				repository.resolveReport(createReportID(110n), createUserID(1n), comment, null),
			),
		);
		const fulfilled = results.filter((result) => result.status === 'fulfilled');
		const rejected = results.filter((result) => result.status === 'rejected');
		expect(fulfilled).toHaveLength(1);
		expect(rejected).toHaveLength(4);
		for (const result of rejected) {
			expect((result as PromiseRejectedResult).reason).toBeInstanceOf(ReportAlreadyResolvedError);
		}
		const winner = (fulfilled[0] as PromiseFulfilledResult<{publicComment: string | null}>).value;
		const stored = await repository.getReport(createReportID(110n));
		expect(stored?.status).toBe(1);
		expect(stored?.publicComment).toBe(winner.publicComment);
		await expect(
			repository.resolveReport(createReportID(110n), createUserID(1n), 'again', null),
		).rejects.toBeInstanceOf(ReportAlreadyResolvedError);
		await expect(repository.resolveReport(createReportID(111n), createUserID(1n), null, null)).rejects.toBeInstanceOf(
			UnknownReportError,
		);
	});

	it('sets and clears a legal hold, and never creates a report', async () => {
		await repository.createReport(reportRow(120n));
		const until = new Date('2027-01-01T00:00:00.000Z');
		const held = await repository.setReportLegalHold(createReportID(120n), until, 'Preserve for counsel');
		expect(held.legalHoldUntil?.getTime()).toBe(until.getTime());
		const stored = await repository.getReport(createReportID(120n));
		expect(stored?.legalHoldUntil?.getTime()).toBe(until.getTime());
		expect(stored?.legalHoldReason).toBe('Preserve for counsel');
		expect(stored?.category).toBe('spam');
		await repository.setReportLegalHold(createReportID(120n), null, null);
		const cleared = await repository.getReport(createReportID(120n));
		expect(cleared?.legalHoldUntil).toBeNull();
		expect(cleared?.legalHoldReason).toBeNull();
		await expect(repository.setReportLegalHold(createReportID(121n), until, 'x')).rejects.toBeInstanceOf(
			UnknownReportError,
		);
		expect(await repository.getReport(createReportID(121n))).toBeNull();
	});

	it('clears the stored reporter email only for the matching reporter', async () => {
		await repository.createReport(reportRow(130n));
		await repository.createReport(
			reportRow(131n, {reporter_id: null, reporter_email: 'notifier@example.com', report_type: 0}),
		);
		expect(await repository.clearReporterEmail(createReportID(130n), createUserID(99n))).toBe(false);
		expect((await repository.getReport(createReportID(130n)))?.reporterEmail).toBe('reporter@example.com');
		expect(await repository.clearReporterEmail(createReportID(130n), createUserID(10n))).toBe(true);
		const cleared = await repository.getReport(createReportID(130n));
		expect(cleared?.reporterEmail).toBeNull();
		expect(cleared?.reporterId).toBe(10n);
		expect(cleared?.category).toBe('spam');
		expect(await repository.clearReporterEmail(createReportID(131n), createUserID(10n))).toBe(false);
		expect((await repository.getReport(createReportID(131n)))?.reporterEmail).toBe('notifier@example.com');
		expect(await repository.clearReporterEmail(createReportID(132n), createUserID(10n))).toBe(false);
		expect(await repository.getReport(createReportID(132n))).toBeNull();
	});

	it('deletes a report', async () => {
		await repository.createReport(reportRow(140n));
		await repository.createReport(reportRow(141n));
		await repository.deleteReport(createReportID(140n));
		expect(await repository.getReport(createReportID(140n))).toBeNull();
		expect((await repository.getReport(createReportID(141n)))?.reportId).toBe(141n);
		const remaining = await repository.listAllReportsPaginated(10);
		expect(remaining.map((report) => report.reportId)).toEqual([141n]);
		await repository.resolveReport(createReportID(141n), createUserID(1n), 'done', null);
		await repository.deleteReport(createReportID(141n));
		await repository.deleteReport(createReportID(142n));
		expect(await storedRows(IARSubmissions.name)).toEqual([]);
	});

	it('reserves a guild report once per reporter and guild for a day', async () => {
		const first = {reporter_id: 10n, reported_guild_id: 50n, report_id: 150n, reported_at: new Date()};
		expect(await repository.reserveGuildReportByReporter(first)).toBe(true);
		expect(await repository.reserveGuildReportByReporter({...first, report_id: 151n})).toBe(false);
		expect(await repository.reserveGuildReportByReporter({...first, reported_guild_id: 51n, report_id: 152n})).toBe(
			true,
		);
		expect(await repository.reserveGuildReportByReporter({...first, reporter_id: 11n, report_id: 153n})).toBe(true);
		const rows = await storedRows(GuildReportSubmissionsByReporter.name);
		expect(rows).toHaveLength(3);
		for (const row of rows) {
			expectExpiresIn(row.expires_at, 86400);
		}
		await repository.releaseGuildReportByReporter({...first, report_id: 151n});
		expect(await repository.reserveGuildReportByReporter({...first, report_id: 154n})).toBe(false);
		await repository.releaseGuildReportByReporter(first);
		expect(await repository.reserveGuildReportByReporter({...first, report_id: 155n})).toBe(true);
	});

	it('gives message reservations, verification codes and tickets their table TTL', async () => {
		expect(
			await repository.reserveMessageReportByReporter({
				reporter_id: 10n,
				channel_id: 20n,
				message_id: 30n,
				report_id: 160n,
				reported_at: new Date(),
			}),
		).toBe(true);
		await repository.upsertDsaEmailVerification({
			email_lower: 'ttl@example.com',
			code_hash: 'first',
			expires_at: new Date(Date.now() + 600_000),
			last_sent_at: new Date(),
		});
		await repository.upsertDsaEmailVerification({
			email_lower: 'ttl@example.com',
			code_hash: 'second',
			expires_at: new Date(Date.now() + 600_000),
			last_sent_at: new Date(),
		});
		await repository.createDsaTicket({
			ticket: 't'.repeat(64),
			email_lower: 'ttl@example.com',
			expires_at: new Date(Date.now() + 3_600_000),
			created_at: new Date(),
		});
		const [message] = await storedRows(MessageReportSubmissionsByReporter.name);
		expectExpiresIn(message!.expires_at, 31536000);
		const [verification] = await storedRows(DSAReportEmailVerifications.name);
		expect(verification!.row_data.code_hash).toBe('second');
		expectExpiresIn(verification!.expires_at, 600);
		const [ticket] = await storedRows(DSAReportTickets.name);
		expectExpiresIn(ticket!.expires_at, 3600);
	});
});
