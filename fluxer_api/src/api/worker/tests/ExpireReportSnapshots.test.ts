// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID, createChannelID, createReportID, createUserID, type ReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import {ENFORCEMENT_DECISION_COLUMNS, type EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import {
	IAR_SUBMISSION_COLUMNS,
	type IARMessageContextRow,
	type IARSubmissionRow,
} from '@app/api/database/types/ReportTypes';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import type {IARSubmission} from '@app/api/report/ReportModels';
import {ReportRepository} from '@app/api/report/ReportRepository';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {InMemorySearchProvider} from '@app/api/test/search/InMemorySearchProvider';
import expireReportSnapshots, {
	CHILD_SAFETY_REPORT_RETENTION_DAYS,
	processReportRetention,
	REPORT_COMPLAINT_WINDOW_DAYS,
	type ReportRetentionDeps,
	type ReportRetentionOptions,
	type ReportRetentionPolicy,
	reportRetentionDates,
} from '@app/api/worker/tasks/ExpireReportSnapshots';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import type {ReportItemOutcome} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {serializeReportProfileSnapshot} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const NOW = new Date('2027-06-01T12:00:00.000Z');
const DAY_MS = ms('1 day');
const REPORTS_BUCKET = Config.s3.buckets.reports;
const WORKER_HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;
const DEFAULT_POLICY: ReportRetentionPolicy = {retentionDays: 365};
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
	ageDays?: number;
	resolvedDaysAgo?: number;
	attachment?: SharedAttachment;
	profile?: boolean;
	legalHoldUntil?: Date | null;
	status?: number;
	category?: string;
	outcome?: ReportItemOutcome | null;
	additionalInfo?: string | null;
	reporterId?: bigint | null;
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

function storedReport(overrides: Partial<IARSubmission>): IARSubmission {
	return {
		reportId: createReportID(nextId()),
		reportedAt: daysAgo(500),
		status: 1,
		resolvedAt: daysAgo(10),
		category: 'harassment',
		reporterOutcome: 'removed',
		...overrides,
	} as IARSubmission;
}

describe('report retention dates', () => {
	const day = DAY_MS;

	it('keeps a pending report until it is resolved', () => {
		expect(reportRetentionDates(storedReport({status: 0, resolvedAt: null}), DEFAULT_POLICY, null)).toEqual({
			deleteAt: null,
			contextExpiresAt: null,
		});
	});

	it('deletes an actioned report one year after it was resolved and keeps its context until then', () => {
		const report = storedReport({});
		expect(reportRetentionDates(report, DEFAULT_POLICY, null)).toEqual({
			deleteAt: report.resolvedAt!.getTime() + 365 * day,
			contextExpiresAt: null,
		});
	});

	it('drops the context of a no violation report once the complaint window has passed', () => {
		const report = storedReport({reporterOutcome: 'no_violation'});
		const dates = reportRetentionDates(report, DEFAULT_POLICY, null);
		expect(dates.contextExpiresAt).toBe(report.resolvedAt!.getTime() + REPORT_COMPLAINT_WINDOW_DAYS * day);
		expect(REPORT_COMPLAINT_WINDOW_DAYS).toBeGreaterThanOrEqual(183);
		expect(dates.deleteAt).toBe(report.resolvedAt!.getTime() + 365 * day);
	});

	it('keeps a child safety report and its context for at least a year whatever the configured period', () => {
		const report = storedReport({category: 'child_safety', reporterOutcome: 'no_violation'});
		expect(reportRetentionDates(report, {retentionDays: 30}, null)).toEqual({
			deleteAt: report.resolvedAt!.getTime() + CHILD_SAFETY_REPORT_RETENTION_DAYS * day,
			contextExpiresAt: null,
		});
		expect(CHILD_SAFETY_REPORT_RETENTION_DAYS).toBeGreaterThanOrEqual(365);
	});

	it('keeps a report linked to a decision as long as the decision evidence and keeps its context', () => {
		const report = storedReport({resolvedAt: daysAgo(300), reporterOutcome: 'no_violation'});
		const decidedAt = daysAgo(100);
		expect(reportRetentionDates(report, DEFAULT_POLICY, decidedAt)).toEqual({
			deleteAt: decidedAt.getTime() + 365 * day,
			contextExpiresAt: null,
		});
	});

	it('uses the longer period when the configured one is longer than a floor', () => {
		const report = storedReport({category: 'child_safety'});
		expect(reportRetentionDates(report, {retentionDays: 730}, null).deleteAt).toBe(
			report.resolvedAt!.getTime() + 730 * day,
		);
	});

	it('counts from the filing time for a resolved report without a resolution time', () => {
		const report = storedReport({resolvedAt: null});
		expect(reportRetentionDates(report, DEFAULT_POLICY, null).deleteAt).toBe(report.reportedAt.getTime() + 365 * day);
	});
});

describe('report retention', () => {
	let repository: ReportRepository;
	let decisions: EnforcementDecisionRepository;
	let storage: MockStorageService;
	let search: IReportSearchService;
	let deps: ReportRetentionDeps;

	beforeEach(() => {
		repository = new ReportRepository();
		decisions = new EnforcementDecisionRepository();
		storage = new MockStorageService();
		search = new InMemorySearchProvider().getReportSearchService();
		deps = {
			reportRepository: repository,
			storageService: storage,
			reportSearchService: search,
			decisionRepository: decisions,
		};
	});

	async function seed(options: SeedOptions = {}): Promise<SeededReport> {
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
		const resolved = options.resolvedDaysAgo !== undefined;
		const row = {
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: id,
			reporter_id: options.reporterId ?? null,
			reported_at: daysAgo(options.ageDays ?? (options.resolvedDaysAgo ?? 0) + 5),
			status: options.status ?? (resolved ? 1 : 0),
			resolved_at: resolved ? daysAgo(options.resolvedDaysAgo!) : null,
			reporter_outcome: options.outcome === undefined ? (resolved ? 'removed' : null) : options.outcome,
			report_type: 0,
			category: options.category ?? 'other',
			additional_info: options.additionalInfo ?? null,
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

	async function linkDecision(reportIds: Array<ReportID>, createdAt: Date): Promise<void> {
		const decisionId = nextId();
		const decision = {
			...Object.fromEntries(ENFORCEMENT_DECISION_COLUMNS.map((column) => [column, null])),
			decision_id: decisionId,
			subject_type: 'user',
			subject_id: nextId(),
			kind: 'warning',
			reason_code: DeletionReasons.HARASSMENT_OR_BULLYING,
			source: 'report',
			automation: 'none',
			report_ids: new Set<bigint>(reportIds),
			decided_by: nextId(),
			created_at: createdAt,
			status: 'active',
			notice_status: 'sent',
		} as EnforcementDecisionRow;
		await decisions.create({
			decision,
			evidence: null,
			firstNote: {decision_id: decisionId, note_id: nextId(), author_id: nextId(), body: 'note', created_at: createdAt},
		});
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

	async function expectContextCleared(seeded: SeededReport): Promise<void> {
		const report = await repository.getReport(seeded.reportId);
		expect(report).not.toBeNull();
		expect(report?.messageContext).toBeNull();
		expect(report?.reportedProfileSnapshot).toBeNull();
		expect(report?.additionalInfo).toBeNull();
		expect(await indexedIds()).toContain(seeded.reportId.toString());
		for (const key of [seeded.attachmentKey, seeded.profileKey]) {
			if (key) expect(storage.hasObject(REPORTS_BUCKET, key)).toBe(false);
		}
	}

	function run(options: Partial<ReportRetentionOptions> = {}) {
		return processReportRetention(deps, {now: NOW, dryRun: false, policy: DEFAULT_POLICY, ...options});
	}

	it('deletes a report 365 days after it was resolved with its objects, search document and row', async () => {
		const expired = await seed({resolvedDaysAgo: 365, attachment: newAttachment(), profile: true});
		const kept = await seed({resolvedDaysAgo: 364, attachment: newAttachment(), profile: true});

		const summary = await run();

		await expectGone(expired);
		expect(storage.hasObject(REPORTS_BUCKET, expired.attachmentKey!)).toBe(false);
		await expectKept(kept);
		expect(summary).toEqual({
			dryRun: false,
			scanned: 2,
			pending: 0,
			expired: 1,
			contextExpired: 0,
			held: 0,
			deleted: 1,
			contextCleared: 0,
			objectsDeleted: 2,
			sharedObjectsKept: 0,
			failed: 0,
		});
	});

	it('keeps a pending report however old it is', async () => {
		const pending = await seed({ageDays: 4000, attachment: newAttachment(), profile: true});

		const summary = await run({policy: {retentionDays: 1}});

		await expectKept(pending);
		expect(summary).toMatchObject({scanned: 1, pending: 1, expired: 0, deleted: 0, objectsDeleted: 0});
	});

	it('drops the copied context of a no violation report after the complaint window and keeps the record', async () => {
		const outsideWindow = await seed({
			resolvedDaysAgo: REPORT_COMPLAINT_WINDOW_DAYS,
			outcome: 'no_violation',
			attachment: newAttachment(),
			profile: true,
			additionalInfo: 'They keep messaging my friends',
		});
		const insideWindow = await seed({
			resolvedDaysAgo: REPORT_COMPLAINT_WINDOW_DAYS - 1,
			outcome: 'no_violation',
			attachment: newAttachment(),
			profile: true,
			additionalInfo: 'Still open to a complaint',
		});
		const actioned = await seed({
			resolvedDaysAgo: REPORT_COMPLAINT_WINDOW_DAYS + 10,
			attachment: newAttachment(),
			profile: true,
		});

		const summary = await run();

		await expectContextCleared(outsideWindow);
		const cleared = await repository.getReport(outsideWindow.reportId);
		expect(cleared).toMatchObject({status: 1, category: 'other', reporterOutcome: 'no_violation'});
		expect((await search.searchReports('friends', {}, {limit: 10})).hits).toHaveLength(0);
		await expectKept(insideWindow);
		expect((await repository.getReport(insideWindow.reportId))?.additionalInfo).toBe('Still open to a complaint');
		await expectKept(actioned);
		expect(summary).toMatchObject({
			scanned: 3,
			expired: 0,
			contextExpired: 1,
			contextCleared: 1,
			deleted: 0,
			objectsDeleted: 2,
			failed: 0,
		});

		const again = await run();
		expect(again).toMatchObject({contextExpired: 0, contextCleared: 0, objectsDeleted: 0});
	});

	it('deletes a no violation report one year after it was resolved', async () => {
		const expired = await seed({resolvedDaysAgo: 365, outcome: 'no_violation', profile: true});

		const summary = await run();

		await expectGone(expired);
		expect(summary).toMatchObject({expired: 1, deleted: 1, contextExpired: 0});
	});

	it('keeps an evidence copy that an actioned report shares with a no violation report', async () => {
		const shared = newAttachment();
		const noViolation = await seed({resolvedDaysAgo: 200, outcome: 'no_violation', attachment: shared});
		const actioned = await seed({resolvedDaysAgo: 200, attachment: shared});

		const summary = await run();

		expect((await repository.getReport(noViolation.reportId))?.messageContext).toBeNull();
		await expectKept(actioned);
		expect(summary).toMatchObject({contextCleared: 1, sharedObjectsKept: 1, objectsDeleted: 0});
	});

	it('keeps a child safety report and its context for a year even with a shorter period or no violation', async () => {
		const childSafety = await seed({
			resolvedDaysAgo: 364,
			category: 'child_safety',
			outcome: 'no_violation',
			attachment: newAttachment(),
			profile: true,
			additionalInfo: 'Context for a CyberTip',
		});
		const childSafetyAfterYear = await seed({resolvedDaysAgo: 365, category: 'child_safety', profile: true});
		const ordinary = await seed({resolvedDaysAgo: 40, profile: true});

		const summary = await run({policy: {retentionDays: 30}});

		await expectKept(childSafety);
		expect((await repository.getReport(childSafety.reportId))?.additionalInfo).toBe('Context for a CyberTip');
		await expectGone(childSafetyAfterYear);
		await expectGone(ordinary);
		expect(summary).toMatchObject({expired: 2, contextExpired: 0, deleted: 2});
	});

	it('keeps a child safety report under a legal hold past the year', async () => {
		const held = await seed({
			resolvedDaysAgo: 500,
			category: 'child_safety',
			profile: true,
			legalHoldUntil: new Date(NOW.getTime() + 30 * DAY_MS),
		});

		const summary = await run();

		await expectKept(held);
		expect(summary).toMatchObject({expired: 0, held: 1, deleted: 0});
	});

	it('keeps a report linked to a decision until the decision evidence expires', async () => {
		const linked = await seed({
			resolvedDaysAgo: 400,
			outcome: 'no_violation',
			profile: true,
			attachment: newAttachment(),
		});
		const linkedLongAgo = await seed({resolvedDaysAgo: 800, profile: true});
		const unlinked = await seed({resolvedDaysAgo: 400, profile: true});
		await linkDecision([linked.reportId], daysAgo(500));
		await linkDecision([linked.reportId], daysAgo(100));
		await linkDecision([linkedLongAgo.reportId], daysAgo(700));

		const summary = await run({pageSize: 1});

		await expectKept(linked);
		expect((await repository.getReport(linked.reportId))?.messageContext).toHaveLength(1);
		await expectGone(linkedLongAgo);
		await expectGone(unlinked);
		expect(summary).toMatchObject({scanned: 3, expired: 2, contextExpired: 0, deleted: 2});
	});

	it('deletes nothing when the decision scan cannot finish', async () => {
		const expired = await seed({resolvedDaysAgo: 400, profile: true});

		await expect(
			processReportRetention(
				{
					...deps,
					decisionRepository: {
						listReportLinks: async () => {
							throw new Error('read timeout');
						},
					},
				},
				{now: NOW, dryRun: false, policy: DEFAULT_POLICY},
			),
		).rejects.toThrow('read timeout');

		await expectKept(expired);
	});

	it('keeps an object that a retained report still uses', async () => {
		const shared = newAttachment();
		const expired = await seed({resolvedDaysAgo: 500, attachment: shared});
		const retained = await seed({resolvedDaysAgo: 10, attachment: shared});

		const summary = await run();

		await expectGone(expired);
		await expectKept(retained);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary.sharedObjectsKept).toBe(1);
		expect(summary.objectsDeleted).toBe(0);
	});

	it('deletes an object shared by two expired reports once', async () => {
		const shared = newAttachment();
		const first = await seed({resolvedDaysAgo: 500, attachment: shared});
		const second = await seed({resolvedDaysAgo: 450, attachment: shared});

		const summary = await run();

		await expectGone(first);
		await expectGone(second);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(false);
		expect(summary.objectsDeleted).toBe(1);
		expect(storage.deleteObjectSpy.mock.calls.filter(([, key]) => key === attachmentKey(shared))).toHaveLength(1);
	});

	it('keeps a report under a legal hold that ends later and deletes one whose hold has ended', async () => {
		const shared = newAttachment();
		const held = await seed({
			resolvedDaysAgo: 500,
			attachment: shared,
			profile: true,
			legalHoldUntil: new Date(NOW.getTime() + DAY_MS),
		});
		const holdEnded = await seed({resolvedDaysAgo: 500, profile: true, legalHoldUntil: daysAgo(1)});
		const sharingWithHeld = await seed({resolvedDaysAgo: 500, attachment: shared});

		const summary = await run();

		await expectKept(held);
		expect((await repository.getReport(held.reportId))?.legalHoldReason).toBe('Preservation request');
		await expectGone(holdEnded);
		await expectGone(sharingWithHeld);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({scanned: 3, expired: 2, held: 1, deleted: 2, sharedObjectsKept: 1});
	});

	it('keeps the context of a no violation report under a legal hold', async () => {
		const held = await seed({
			resolvedDaysAgo: 250,
			outcome: 'no_violation',
			attachment: newAttachment(),
			legalHoldUntil: new Date(NOW.getTime() + DAY_MS),
		});

		const summary = await run();

		await expectKept(held);
		expect((await repository.getReport(held.reportId))?.messageContext).toHaveLength(1);
		expect(summary).toMatchObject({held: 1, contextExpired: 0, contextCleared: 0});
	});

	it('changes nothing in a dry run and counts what it would do to existing reports', async () => {
		const expired = await seed({resolvedDaysAgo: 366, attachment: newAttachment(), profile: true});
		const contextDue = await seed({
			resolvedDaysAgo: 200,
			outcome: 'no_violation',
			attachment: newAttachment(),
			additionalInfo: 'details',
		});
		const childSafety = await seed({resolvedDaysAgo: 200, category: 'child_safety', outcome: 'no_violation'});
		const held = await seed({resolvedDaysAgo: 400, legalHoldUntil: new Date(NOW.getTime() + DAY_MS)});
		const pending = await seed({profile: true});
		const linked = await seed({resolvedDaysAgo: 400});
		await linkDecision([linked.reportId], daysAgo(30));

		const summary = await run({dryRun: true});

		for (const report of [expired, contextDue, childSafety, held, pending, linked]) {
			await expectKept(report);
		}
		expect((await repository.getReport(contextDue.reportId))?.additionalInfo).toBe('details');
		expect(storage.deleteObjectSpy).not.toHaveBeenCalled();
		expect(summary).toEqual({
			dryRun: true,
			scanned: 6,
			pending: 1,
			expired: 1,
			contextExpired: 1,
			held: 1,
			deleted: 1,
			contextCleared: 1,
			objectsDeleted: 3,
			sharedObjectsKept: 0,
			failed: 0,
		});
	});

	it('deletes nothing when the scan cannot finish', async () => {
		const expired = await seed({resolvedDaysAgo: 400, attachment: newAttachment(), profile: true});
		await seed({resolvedDaysAgo: 400});
		await seed({resolvedDaysAgo: 400});
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
				{
					now: NOW,
					dryRun: false,
					policy: DEFAULT_POLICY,
					pageSize: 2,
				},
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
			expired.push(await seed({resolvedDaysAgo: 400 + index, profile: true}));
			kept.push(await seed({resolvedDaysAgo: index, profile: true}));
		}

		const summary = await run({pageSize: 3});

		expect(summary).toMatchObject({scanned: 14, expired: 7, deleted: 7, objectsDeleted: 7, failed: 0});
		for (const report of expired) await expectGone(report);
		for (const report of kept) await expectKept(report);
	});

	it('keeps the row and search document when an object cannot be deleted, then deletes them on the next run', async () => {
		const expired = await seed({resolvedDaysAgo: 400, attachment: newAttachment(), profile: true});
		storage.configure({shouldFailDelete: true});

		const failed = await run();

		expect(failed).toMatchObject({expired: 1, deleted: 0, failed: 1});
		expect(await repository.getReport(expired.reportId)).not.toBeNull();
		expect(await indexedIds()).toContain(expired.reportId.toString());

		storage.configure({shouldFailDelete: false});
		const retried = await run();

		expect(retried).toMatchObject({expired: 1, deleted: 1, failed: 0});
		await expectGone(expired);
		expect(storage.hasObject(REPORTS_BUCKET, expired.attachmentKey!)).toBe(false);
	});

	it('keeps the copied context when an object cannot be deleted, then clears it on the next run', async () => {
		const due = await seed({resolvedDaysAgo: 200, outcome: 'no_violation', attachment: newAttachment()});
		storage.configure({shouldFailDelete: true});

		expect(await run()).toMatchObject({contextExpired: 1, contextCleared: 0, failed: 1});
		expect((await repository.getReport(due.reportId))?.messageContext).toHaveLength(1);

		storage.configure({shouldFailDelete: false});
		expect(await run()).toMatchObject({contextExpired: 1, contextCleared: 1, failed: 0});
		await expectContextCleared(due);
	});

	it('does not bring back a report deleted while its context was being cleared', async () => {
		const due = await seed({resolvedDaysAgo: 200, outcome: 'no_violation', additionalInfo: 'details'});
		const deletingRepository = Object.create(repository) as ReportRepository;
		deletingRepository.clearCopiedContext = async (reportId, reportType) => {
			await repository.deleteReport(reportId);
			return repository.clearCopiedContext(reportId, reportType);
		};

		await processReportRetention(
			{...deps, reportRepository: deletingRepository},
			{
				now: NOW,
				dryRun: false,
				policy: DEFAULT_POLICY,
			},
		);

		expect(await repository.getReport(due.reportId)).toBeNull();
	});

	it('keeps the row when the search document cannot be deleted', async () => {
		const expired = await seed({resolvedDaysAgo: 400});
		const failingSearch = Object.create(search) as IReportSearchService;
		failingSearch.deleteReport = async () => {
			throw new Error('search unavailable');
		};

		const summary = await processReportRetention(
			{...deps, reportSearchService: failingSearch},
			{
				now: NOW,
				dryRun: false,
				policy: DEFAULT_POLICY,
			},
		);

		expect(summary).toMatchObject({deleted: 0, failed: 1});
		expect(await repository.getReport(expired.reportId)).not.toBeNull();
	});

	it('deletes rows when search is not configured', async () => {
		const expired = await seed({resolvedDaysAgo: 400, profile: true});

		const summary = await processReportRetention(
			{...deps, reportSearchService: null},
			{
				now: NOW,
				dryRun: false,
				policy: DEFAULT_POLICY,
			},
		);

		expect(summary).toMatchObject({deleted: 1, failed: 0});
		expect(await repository.getReport(expired.reportId)).toBeNull();
		expect(storage.hasObject(REPORTS_BUCKET, expired.profileKey!)).toBe(false);
	});

	it('skips a report that was put on hold after the scan', async () => {
		const shared = newAttachment();
		const seeded = [
			await seed({resolvedDaysAgo: 400, attachment: shared, profile: true}),
			await seed({resolvedDaysAgo: 400, attachment: shared}),
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
			{
				now: NOW,
				dryRun: false,
				policy: DEFAULT_POLICY,
			},
		);

		await expectKept(first);
		await expectGone(second);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({expired: 2, deleted: 1, failed: 0});
	});

	it('keeps an object that a report filed during the run stored again', async () => {
		const shared = newAttachment();
		const expired = await seed({resolvedDaysAgo: 400, attachment: shared, profile: true});
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
			{
				now: NOW,
				dryRun: false,
				policy: DEFAULT_POLICY,
			},
		);

		await expectGone(expired);
		expect(storage.hasObject(REPORTS_BUCKET, attachmentKey(shared))).toBe(true);
		expect(summary).toMatchObject({deleted: 1, objectsDeleted: 1, sharedObjectsKept: 1, failed: 0});
	});

	it('deletes profile images stored for a report whose snapshot cannot be read', async () => {
		const expired = await seed({resolvedDaysAgo: 400, profile: true, snapshotJson: '{"not":"a snapshot"'});

		const summary = await run();

		expect(summary).toMatchObject({deleted: 1, objectsDeleted: 1});
		await expectGone(expired);
	});

	it('deletes earlier when the retention period is shorter', async () => {
		const expired = await seed({resolvedDaysAgo: 30, profile: true});
		const kept = await seed({resolvedDaysAgo: 29, profile: true});

		const summary = await run({policy: {retentionDays: 30}});

		await expectGone(expired);
		await expectKept(kept);
		expect(summary).toMatchObject({scanned: 2, expired: 1, deleted: 1});
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
			reported_at: daysAgo(420),
			status: 1,
			resolved_at: daysAgo(400),
			report_type: 1,
			category: 'harassment',
		} as IARSubmissionRow);
		const reservation = {
			reporter_id: createUserID(reporterId),
			reported_user_id: createUserID(reportedUserId),
			report_id: reportId,
			reported_at: daysAgo(420),
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
			expect(configured).toEqual({days: 365, dryRun: false});
			const expired = await seed({resolvedDaysAgo: 366, attachment: newAttachment(), profile: true});
			const kept = await seed({resolvedDaysAgo: 364, profile: true});
			const pending = await seed({profile: true});

			await runDailyTask();

			expect(await repository.getReport(expired.reportId)).toBeNull();
			expect(storage.hasObject(REPORTS_BUCKET, expired.attachmentKey!)).toBe(false);
			expect(storage.hasObject(REPORTS_BUCKET, expired.profileKey!)).toBe(false);
			expect(await repository.getReport(kept.reportId)).not.toBeNull();
			expect(await repository.getReport(pending.reportId)).not.toBeNull();
		});

		it('only counts what it would delete when FLUXER_REPORT_RETENTION_DRY_RUN is true', async () => {
			Object.assign(Config.reportRetention, {dryRun: true});
			const expired = await seed({resolvedDaysAgo: 366, attachment: newAttachment(), profile: true});

			await runDailyTask();

			expect(await repository.getReport(expired.reportId)).not.toBeNull();
			expect(storage.deleteObjectSpy).not.toHaveBeenCalled();
		});

		it('reads the day setting from the config and honors decisions', async () => {
			Object.assign(Config.reportRetention, {days: 60});
			const expired = await seed({resolvedDaysAgo: 60});
			const kept = await seed({resolvedDaysAgo: 59});
			const linked = await seed({resolvedDaysAgo: 90});
			await linkDecision([linked.reportId], daysAgo(90));

			await runDailyTask();

			expect(await repository.getReport(expired.reportId)).toBeNull();
			expect(await repository.getReport(kept.reportId)).not.toBeNull();
			expect(await repository.getReport(linked.reportId)).not.toBeNull();
		});
	});
});
