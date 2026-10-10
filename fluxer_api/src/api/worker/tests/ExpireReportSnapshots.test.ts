// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID, createChannelID, createReportID, createUserID, type ReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import {
	IAR_SUBMISSION_COLUMNS,
	type IARMessageContextRow,
	type IARSubmissionRow,
} from '@app/api/database/types/ReportTypes';
import type {ReportDeletionDeps} from '@app/api/report/ReportDeletion';
import type {IARSubmission} from '@app/api/report/ReportModels';
import {ReportRepository} from '@app/api/report/ReportRepository';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {InMemorySearchProvider} from '@app/api/test/search/InMemorySearchProvider';
import expireReportSnapshots, {
	processReportRetention,
	type ReportRetentionOptions,
	type ReportRetentionPolicy,
} from '@app/api/worker/tasks/ExpireReportSnapshots';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {serializeReportProfileSnapshot} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const NOW = new Date('2027-06-01T12:00:00.000Z');
const DAY_MS = ms('1 day');
const REPORTS_BUCKET = Config.s3.buckets.reports;
const WORKER_HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;
const DEFAULT_POLICY: ReportRetentionPolicy = {retentionDays: 365, resolvedRetentionDays: null};
let sequence = 1_480_000_000_000_000_000n;

function nextId(): bigint {
	sequence += 1n;
	return sequence;
}

function daysAgo(days: number): Date {
	return new Date(NOW.getTime() - days * DAY_MS);
}

function attachmentRow(attachmentId: bigint, filename: string): MessageAttachment {
	return {
		attachment_id: createAttachmentID(attachmentId),
		filename,
		size: 1024n,
		title: null,
		description: null,
		width: 64,
		height: 64,
		content_type: 'image/png',
		content_hash: null,
		placeholder: null,
		flags: 0,
		duration: null,
		nsfw: false,
		waveform: null,
	};
}

function contextRow(channelId: bigint, attachments: Array<MessageAttachment>): IARMessageContextRow {
	return {
		message_id: nextId(),
		channel_id: channelId,
		author_id: nextId(),
		webhook_id: null,
		author_username: 'context_author',
		author_discriminator: 1,
		author_avatar_hash: null,
		content: 'context',
		timestamp: NOW,
		edited_timestamp: null,
		type: 0,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments,
		embeds: null,
		sticker_items: null,
	};
}

interface SharedAttachment {
	channelId: bigint;
	attachmentId: bigint;
	filename: string;
}

interface SeedOptions {
	ageDays: number;
	attachment?: SharedAttachment;
	profile?: boolean;
	legalHoldUntil?: Date | null;
	status?: number;
	resolvedDaysAgo?: number;
	snapshotJson?: string;
}

interface SeededReport {
	reportId: ReportID;
	attachmentKey: string | null;
	profileKey: string | null;
}

function newAttachment(): SharedAttachment {
	return {channelId: nextId(), attachmentId: nextId(), filename: 'evidence.png'};
}

function attachmentKey(attachment: SharedAttachment): string {
	return makeAttachmentCdnKey(createChannelID(attachment.channelId), attachment.attachmentId, attachment.filename);
}

describe('report retention', () => {
	let repository: ReportRepository;
	let storage: MockStorageService;
	let search: IReportSearchService;
	let deps: ReportDeletionDeps;

	beforeEach(() => {
		repository = new ReportRepository();
		storage = new MockStorageService();
		search = new InMemorySearchProvider().getReportSearchService();
		deps = {reportRepository: repository, storageService: storage, reportSearchService: search};
	});

	async function seed(options: SeedOptions): Promise<SeededReport> {
		const id = nextId();
		const reportId = createReportID(id);
		let storedAttachmentKey: string | null = null;
		let profileKey: string | null = null;
		let messageContext: Array<IARMessageContextRow> | null = null;
		if (options.attachment) {
			const {channelId, attachmentId, filename} = options.attachment;
			storedAttachmentKey = attachmentKey(options.attachment);
			messageContext = [contextRow(channelId, [attachmentRow(attachmentId, filename)])];
			await storage.uploadObject({bucket: REPORTS_BUCKET, key: storedAttachmentKey, body: new Uint8Array([1])});
		}
		let snapshot: string | null = options.snapshotJson ?? null;
		if (options.profile) {
			profileKey = `reports/${reportId}/profile/user_avatar/abc${id}`;
			await storage.uploadObject({bucket: REPORTS_BUCKET, key: profileKey, body: new Uint8Array([2])});
			snapshot ??= serializeReportProfileSnapshot({
				captured_at: NOW.toISOString(),
				user: {
					id: nextId().toString(),
					username: 'reported',
					discriminator: 1,
					global_name: null,
					bio: 'bio at report time',
					pronouns: null,
					avatar: {hash: `abc${id}`, key: profileKey},
					banner: null,
				},
				member: null,
				guild: null,
			});
		}
		const row = {
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: id,
			reported_at: daysAgo(options.ageDays),
			status: options.status ?? (options.resolvedDaysAgo === undefined ? 0 : 1),
			resolved_at: options.resolvedDaysAgo === undefined ? null : daysAgo(options.resolvedDaysAgo),
			report_type: 0,
			category: 'other',
			reported_channel_id: options.attachment?.channelId ?? null,
			message_context: messageContext,
			reported_profile_snapshot: snapshot,
			legal_hold_until: options.legalHoldUntil ?? null,
			legal_hold_reason: options.legalHoldUntil ? 'Preservation request' : null,
		} as IARSubmissionRow;
		const report = await repository.createReport(row);
		await search.indexReport(report);
		return {reportId, attachmentKey: storedAttachmentKey, profileKey};
	}

	async function indexedIds(): Promise<Array<string>> {
		const result = await search.searchReports('', {}, {limit: 1000});
		return result.hits.map((hit) => hit.id).sort();
	}

	async function expectKept(seeded: SeededReport): Promise<void> {
		expect(await repository.getReport(seeded.reportId)).not.toBeNull();
		expect(await indexedIds()).toContain(seeded.reportId.toString());
		for (const key of [seeded.attachmentKey, seeded.profileKey]) {
			if (key) expect(storage.hasObject(REPORTS_BUCKET, key)).toBe(true);
		}
	}

	async function expectGone(seeded: SeededReport): Promise<void> {
		expect(await repository.getReport(seeded.reportId)).toBeNull();
		expect(await indexedIds()).not.toContain(seeded.reportId.toString());
		if (seeded.profileKey) expect(storage.hasObject(REPORTS_BUCKET, seeded.profileKey)).toBe(false);
	}

	it('deletes a report 365 days after it was filed with its objects, search document and row', async () => {
		const expired = await seed({ageDays: 365, attachment: newAttachment(), profile: true});
		const kept = await seed({ageDays: 364, attachment: newAttachment(), profile: true});

		const summary = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		await expectGone(expired);
		expect(storage.hasObject(REPORTS_BUCKET, expired.attachmentKey!)).toBe(false);
		await expectKept(kept);
		expect(summary).toEqual({
			dryRun: false,
			scanned: 2,
			expired: 1,
			held: 0,
			deleted: 1,
			objectsDeleted: 2,
			sharedObjectsKept: 0,
			failed: 0,
		});
	});

	it('deletes expired reports whatever their status', async () => {
		const pending = await seed({ageDays: 400, status: 0});
		const resolved = await seed({ageDays: 400, status: 1});

		await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		await expectGone(pending);
		await expectGone(resolved);
	});

	it('keeps an object that a retained report still uses', async () => {
		const shared = newAttachment();
		const expired = await seed({ageDays: 500, attachment: shared});
		const retained = await seed({ageDays: 10, attachment: shared});

		const summary = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		await expectGone(expired);
		await expectKept(retained);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary.sharedObjectsKept).toBe(1);
		expect(summary.objectsDeleted).toBe(0);
	});

	it('deletes an object shared by two expired reports once', async () => {
		const shared = newAttachment();
		const first = await seed({ageDays: 500, attachment: shared});
		const second = await seed({ageDays: 450, attachment: shared});

		const summary = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		await expectGone(first);
		await expectGone(second);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(false);
		expect(summary.objectsDeleted).toBe(1);
		expect(storage.deleteObjectSpy.mock.calls.filter(([, key]) => key === attachmentKey(shared))).toHaveLength(1);
	});

	it('keeps a report under a legal hold that ends later and deletes one whose hold has ended', async () => {
		const shared = newAttachment();
		const held = await seed({
			ageDays: 500,
			attachment: shared,
			profile: true,
			legalHoldUntil: new Date(NOW.getTime() + DAY_MS),
		});
		const holdEnded = await seed({ageDays: 500, profile: true, legalHoldUntil: daysAgo(1)});
		const sharingWithHeld = await seed({ageDays: 500, attachment: shared});

		const summary = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		await expectKept(held);
		expect((await repository.getReport(held.reportId))?.legalHoldReason).toBe('Preservation request');
		await expectGone(holdEnded);
		await expectGone(sharingWithHeld);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({scanned: 3, expired: 2, held: 1, deleted: 2, sharedObjectsKept: 1});
	});

	it('changes nothing in a dry run and reports what it would delete', async () => {
		const expired = await seed({ageDays: 366, attachment: newAttachment(), profile: true});
		const kept = await seed({ageDays: 1, profile: true});

		const summary = await processReportRetention(deps, {now: NOW, dryRun: true, policy: DEFAULT_POLICY});

		await expectKept(expired);
		await expectKept(kept);
		expect(storage.deleteObjectSpy).not.toHaveBeenCalled();
		expect(summary).toEqual({
			dryRun: true,
			scanned: 2,
			expired: 1,
			held: 0,
			deleted: 1,
			objectsDeleted: 2,
			sharedObjectsKept: 0,
			failed: 0,
		});
	});

	it('deletes nothing when the scan cannot finish', async () => {
		const expired = await seed({ageDays: 400, attachment: newAttachment(), profile: true});
		await seed({ageDays: 400});
		await seed({ageDays: 400});
		let pages = 0;
		const failingRepository = Object.create(repository) as ReportRepository;
		failingRepository.listAllReportsPaginated = async (limit: number, lastReportId?: ReportID) => {
			pages += 1;
			if (pages > 1) throw new Error('read timeout');
			return repository.listAllReportsPaginated(limit, lastReportId);
		};

		await expect(
			processReportRetention(
				{...deps, reportRepository: failingRepository},
				{now: NOW, dryRun: false, policy: DEFAULT_POLICY, pageSize: 2},
			),
		).rejects.toThrow('read timeout');

		await expectKept(expired);
		expect(storage.deleteObjectSpy).not.toHaveBeenCalled();
		expect(await repository.listAllReportsPaginated(10)).toHaveLength(3);
	});

	it('pages through every report', async () => {
		const expired: Array<SeededReport> = [];
		const kept: Array<SeededReport> = [];
		for (let index = 0; index < 7; index++) {
			expired.push(await seed({ageDays: 400 + index, profile: true}));
			kept.push(await seed({ageDays: index, profile: true}));
		}

		const summary = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY, pageSize: 3});

		expect(summary).toMatchObject({scanned: 14, expired: 7, deleted: 7, objectsDeleted: 7, failed: 0});
		for (const report of expired) await expectGone(report);
		for (const report of kept) await expectKept(report);
	});

	it('keeps the row and search document when an object cannot be deleted, then deletes them on the next run', async () => {
		const expired = await seed({ageDays: 400, attachment: newAttachment(), profile: true});
		storage.configure({shouldFailDelete: true});

		const failed = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		expect(failed).toMatchObject({expired: 1, deleted: 0, failed: 1});
		expect(await repository.getReport(expired.reportId)).not.toBeNull();
		expect(await indexedIds()).toContain(expired.reportId.toString());

		storage.configure({shouldFailDelete: false});
		const retried = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		expect(retried).toMatchObject({expired: 1, deleted: 1, failed: 0});
		await expectGone(expired);
		expect(storage.hasObject(REPORTS_BUCKET, expired.attachmentKey!)).toBe(false);
	});

	it('keeps the row when the search document cannot be deleted', async () => {
		const expired = await seed({ageDays: 400});
		const failingSearch = Object.create(search) as IReportSearchService;
		failingSearch.deleteReport = async () => {
			throw new Error('search unavailable');
		};

		const summary = await processReportRetention(
			{...deps, reportSearchService: failingSearch},
			{now: NOW, dryRun: false, policy: DEFAULT_POLICY},
		);

		expect(summary).toMatchObject({deleted: 0, failed: 1});
		expect(await repository.getReport(expired.reportId)).not.toBeNull();
	});

	it('deletes rows when search is not configured', async () => {
		const expired = await seed({ageDays: 400, profile: true});

		const summary = await processReportRetention(
			{...deps, reportSearchService: null},
			{now: NOW, dryRun: false, policy: DEFAULT_POLICY},
		);

		expect(summary).toMatchObject({deleted: 1, failed: 0});
		expect(await repository.getReport(expired.reportId)).toBeNull();
		expect(storage.hasObject(REPORTS_BUCKET, expired.profileKey!)).toBe(false);
	});

	it('skips a report that was put on hold after the scan', async () => {
		const shared = newAttachment();
		const seeded = [
			await seed({ageDays: 400, attachment: shared, profile: true}),
			await seed({ageDays: 400, attachment: shared}),
		];
		const scanOrder = (await repository.listAllReportsPaginated(10)).map((report) => report.reportId);
		const [first, second] = seeded.sort((a, b) => scanOrder.indexOf(a.reportId) - scanOrder.indexOf(b.reportId)) as [
			SeededReport,
			SeededReport,
		];
		const holdingRepository = Object.create(repository) as ReportRepository;
		holdingRepository.getReport = async (reportId: ReportID): Promise<IARSubmission | null> => {
			const report = await repository.getReport(reportId);
			if (report && reportId === first.reportId) {
				return {...report, legalHoldUntil: new Date(NOW.getTime() + DAY_MS), legalHoldReason: 'Late request'};
			}
			return report;
		};

		const summary = await processReportRetention(
			{...deps, reportRepository: holdingRepository},
			{now: NOW, dryRun: false, policy: DEFAULT_POLICY},
		);

		await expectKept(first);
		await expectGone(second);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({expired: 2, deleted: 1, failed: 0});
	});

	it('keeps an object that a report filed during the run stored again', async () => {
		const shared = newAttachment();
		const expired = await seed({ageDays: 400, attachment: shared, profile: true});
		const rewritingStorage = Object.create(storage) as MockStorageService;
		rewritingStorage.getObjectMetadata = async (bucket: string, key: string) => {
			const metadata = await storage.getObjectMetadata(bucket, key);
			if (metadata && key === attachmentKey(shared)) {
				return {...metadata, lastModified: new Date(NOW.getTime() + ms('1 second'))};
			}
			return metadata;
		};

		const summary = await processReportRetention(
			{...deps, storageService: rewritingStorage},
			{now: NOW, dryRun: false, policy: DEFAULT_POLICY},
		);

		await expectGone(expired);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({deleted: 1, objectsDeleted: 1, sharedObjectsKept: 1, failed: 0});
	});

	it('deletes profile images stored for a report whose snapshot cannot be read', async () => {
		const expired = await seed({ageDays: 400, profile: true, snapshotJson: '{"not":"a snapshot"'});

		const summary = await processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY});

		expect(summary).toMatchObject({deleted: 1, objectsDeleted: 1});
		await expectGone(expired);
	});

	function run(options: Partial<ReportRetentionOptions> = {}) {
		return processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY, ...options});
	}

	it('deletes earlier when the retention period is shorter', async () => {
		const expired = await seed({ageDays: 30, profile: true});
		const kept = await seed({ageDays: 29, profile: true});

		const summary = await run({policy: {retentionDays: 30, resolvedRetentionDays: null}});

		await expectGone(expired);
		await expectKept(kept);
		expect(summary).toMatchObject({scanned: 2, expired: 1, deleted: 1});
	});

	it('deletes a resolved report once the resolved-report period has passed since it was resolved', async () => {
		const policy = {retentionDays: 365, resolvedRetentionDays: 30};
		const resolvedLongAgo = await seed({ageDays: 100, resolvedDaysAgo: 30, attachment: newAttachment()});
		const resolvedRecently = await seed({ageDays: 100, resolvedDaysAgo: 29, profile: true});
		const pending = await seed({ageDays: 100, profile: true});
		const resolvedWithoutTime = await seed({ageDays: 100, status: 1, profile: true});

		const summary = await run({policy});

		await expectGone(resolvedLongAgo);
		expect(storage.hasObject(REPORTS_BUCKET, resolvedLongAgo.attachmentKey!)).toBe(false);
		await expectKept(resolvedRecently);
		await expectKept(pending);
		await expectKept(resolvedWithoutTime);
		expect(summary).toMatchObject({scanned: 4, expired: 1, deleted: 1, held: 0});
	});

	it('applies whichever rule ends first', async () => {
		const policy = {retentionDays: 100, resolvedRetentionDays: 300};
		const byAge = await seed({ageDays: 100, resolvedDaysAgo: 10});
		const neither = await seed({ageDays: 99, resolvedDaysAgo: 90});

		await run({policy});

		await expectGone(byAge);
		await expectKept(neither);
	});

	it('keeps a held resolved report past the resolved-report period and keeps its evidence', async () => {
		const shared = newAttachment();
		const held = await seed({
			ageDays: 50,
			resolvedDaysAgo: 40,
			attachment: shared,
			profile: true,
			legalHoldUntil: new Date(NOW.getTime() + DAY_MS),
		});
		const sharingWithHeld = await seed({ageDays: 50, resolvedDaysAgo: 40, attachment: shared});

		const summary = await run({policy: {retentionDays: 365, resolvedRetentionDays: 7}});

		await expectKept(held);
		await expectGone(sharingWithHeld);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({expired: 1, held: 1, deleted: 1, sharedObjectsKept: 1});
	});

	it('frees the reporter to report the same account again once the report is deleted', async () => {
		const reporterId = nextId();
		const reportedUserId = nextId();
		const reportId = createReportID(nextId());
		await repository.createReport({
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: BigInt(reportId),
			reporter_id: reporterId,
			reported_user_id: reportedUserId,
			reported_at: daysAgo(400),
			status: 0,
			report_type: 1,
			category: 'harassment',
		} as IARSubmissionRow);
		const reservation = {
			reporter_id: createUserID(reporterId),
			reported_user_id: createUserID(reportedUserId),
			report_id: reportId,
			reported_at: daysAgo(400),
		};
		expect(await repository.reserveUserReportByReporter(reservation)).toBe(true);

		await run();

		expect(await repository.getReport(reportId)).toBeNull();
		expect(await repository.reserveUserReportByReporter({...reservation, report_id: createReportID(nextId())})).toBe(
			true,
		);
	});

	describe('the daily task', () => {
		const configured = {...Config.reportRetention};

		beforeEach(() => {
			setWorkerDependenciesForTest({reportRepository: repository, storageService: storage});
		});

		afterEach(() => {
			vi.useRealTimers();
			Object.assign(Config.reportRetention, configured);
			clearWorkerDependencies();
		});

		async function runDailyTask() {
			vi.useFakeTimers({now: NOW, toFake: ['Date']});
			await expireReportSnapshots({}, WORKER_HELPERS);
		}

		it('deletes for real with the default settings', async () => {
			expect(configured).toEqual({days: 365, resolvedDays: null, dryRun: false});
			const expired = await seed({ageDays: 366, attachment: newAttachment(), profile: true});
			const kept = await seed({ageDays: 364, profile: true});

			await runDailyTask();

			expect(await repository.getReport(expired.reportId)).toBeNull();
			expect(storage.hasObject(REPORTS_BUCKET, expired.attachmentKey!)).toBe(false);
			expect(storage.hasObject(REPORTS_BUCKET, expired.profileKey!)).toBe(false);
			expect(await repository.getReport(kept.reportId)).not.toBeNull();
		});

		it('only counts what it would delete when FLUXER_REPORT_RETENTION_DRY_RUN is true', async () => {
			Object.assign(Config.reportRetention, {dryRun: true});
			const expired = await seed({ageDays: 366, attachment: newAttachment(), profile: true});

			await runDailyTask();

			expect(await repository.getReport(expired.reportId)).not.toBeNull();
			expect(storage.deleteObjectSpy).not.toHaveBeenCalled();
		});

		it('reads both day settings from the config', async () => {
			Object.assign(Config.reportRetention, {days: 60, resolvedDays: 5});
			const byAge = await seed({ageDays: 60});
			const byResolution = await seed({ageDays: 20, resolvedDaysAgo: 5});
			const kept = await seed({ageDays: 20, resolvedDaysAgo: 4});

			await runDailyTask();

			expect(await repository.getReport(byAge.reportId)).toBeNull();
			expect(await repository.getReport(byResolution.reportId)).toBeNull();
			expect(await repository.getReport(kept.reportId)).not.toBeNull();
		});
	});
});
