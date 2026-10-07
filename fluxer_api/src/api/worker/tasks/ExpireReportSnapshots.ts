// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {IARSubmission, IReportRepository} from '@app/api/report/IReportRepository';
import {getReportSearchService} from '@app/api/SearchFactory';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import {readOptionalEnv} from '@app/api/utils/IntegerOptions';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {listReportProfileSnapshotAssets} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const REPORT_RETENTION_DAYS = 365;
const REPORT_RETENTION_MS = REPORT_RETENTION_DAYS * ms('1 day');
const DEFAULT_SCAN_PAGE_SIZE = 500;

export interface ReportRetentionDeps {
	reportRepository: IReportRepository;
	storageService: IStorageService;
	reportSearchService: IReportSearchService | null;
}

export interface ReportRetentionOptions {
	now: Date;
	dryRun: boolean;
	pageSize?: number;
}

export interface ReportRetentionSummary {
	dryRun: boolean;
	scanned: number;
	expired: number;
	held: number;
	deleted: number;
	objectsDeleted: number;
	sharedObjectsKept: number;
	failed: number;
}

type ReportRetentionState = 'expired' | 'held' | 'retained';

export function resolveReportRetentionDryRun(raw: string | undefined): boolean {
	const value = raw?.trim().toLowerCase();
	return value !== 'false' && value !== '0';
}

function reportRetentionState(report: IARSubmission, now: Date): ReportRetentionState {
	const reportedAt = report.reportedAt instanceof Date ? report.reportedAt.getTime() : Number.NaN;
	if (Number.isNaN(reportedAt) || reportedAt > now.getTime() - REPORT_RETENTION_MS) {
		return 'retained';
	}
	if (report.legalHoldUntil && report.legalHoldUntil.getTime() > now.getTime()) {
		return 'held';
	}
	return 'expired';
}

function reportProfilePrefix(reportId: ReportID): string {
	return `reports/${reportId}/profile/`;
}

function referencedObjectKeys(report: IARSubmission): Array<string> {
	const keys: Array<string> = [];
	for (const message of report.messageContext ?? []) {
		const channelId = message.channelId ?? report.reportedChannelId;
		if (!channelId) continue;
		for (const attachment of message.attachments) {
			if (attachment.attachment_id == null || !attachment.filename) continue;
			keys.push(makeAttachmentCdnKey(channelId, attachment.attachment_id, String(attachment.filename)));
		}
	}
	for (const asset of listReportProfileSnapshotAssets(report.reportedProfileSnapshot)) {
		if (asset.key) {
			keys.push(asset.key);
		}
	}
	return keys;
}

async function storedObjectKeys(storageService: IStorageService, report: IARSubmission): Promise<Set<string>> {
	const keys = new Set(referencedObjectKeys(report));
	const listed = await storageService.listObjects({
		bucket: Config.s3.buckets.reports,
		prefix: reportProfilePrefix(report.reportId),
	});
	for (const object of listed) {
		keys.add(object.key);
	}
	return keys;
}

export async function processReportRetention(
	deps: ReportRetentionDeps,
	options: ReportRetentionOptions,
): Promise<ReportRetentionSummary> {
	const {reportRepository, storageService, reportSearchService} = deps;
	const {now, dryRun} = options;
	const pageSize = options.pageSize ?? DEFAULT_SCAN_PAGE_SIZE;
	const summary: ReportRetentionSummary = {
		dryRun,
		scanned: 0,
		expired: 0,
		held: 0,
		deleted: 0,
		objectsDeleted: 0,
		sharedObjectsKept: 0,
		failed: 0,
	};
	const retainedKeys = new Set<string>();
	const expiredIds: Array<ReportID> = [];
	let cursor: ReportID | undefined;
	while (true) {
		const page = await reportRepository.listAllReportsPaginated(pageSize, cursor);
		for (const report of page) {
			summary.scanned++;
			const state = reportRetentionState(report, now);
			if (state === 'expired') {
				expiredIds.push(report.reportId);
				continue;
			}
			if (state === 'held') {
				summary.held++;
			}
			for (const key of referencedObjectKeys(report)) {
				retainedKeys.add(key);
			}
		}
		if (page.length < pageSize) break;
		cursor = page[page.length - 1]!.reportId;
	}
	summary.expired = expiredIds.length;
	const deletedKeys = new Set<string>();
	for (const reportId of expiredIds) {
		try {
			const report = await reportRepository.getReport(reportId);
			if (!report) continue;
			if (reportRetentionState(report, now) !== 'expired') {
				for (const key of referencedObjectKeys(report)) {
					retainedKeys.add(key);
				}
				continue;
			}
			for (const key of await storedObjectKeys(storageService, report)) {
				if (retainedKeys.has(key)) {
					summary.sharedObjectsKept++;
					continue;
				}
				if (deletedKeys.has(key)) continue;
				const metadata = await storageService.getObjectMetadata(Config.s3.buckets.reports, key);
				if (metadata?.lastModified && metadata.lastModified.getTime() >= now.getTime()) {
					retainedKeys.add(key);
					summary.sharedObjectsKept++;
					continue;
				}
				if (!dryRun) {
					await storageService.deleteObject(Config.s3.buckets.reports, key);
				}
				deletedKeys.add(key);
				summary.objectsDeleted++;
			}
			if (!dryRun) {
				await reportSearchService?.deleteReport(reportId);
				await reportRepository.deleteReport(reportId);
			}
			summary.deleted++;
		} catch (error) {
			summary.failed++;
			Logger.error({error, reportId: reportId.toString()}, 'Failed to delete an expired report');
		}
	}
	Logger.info({...summary, retentionDays: REPORT_RETENTION_DAYS}, 'Processed report retention');
	return summary;
}

const expireReportSnapshots: WorkerTaskHandler = async () => {
	const {reportRepository, storageService} = getWorkerDependencies();
	await processReportRetention(
		{reportRepository, storageService, reportSearchService: getReportSearchService()},
		{now: new Date(), dryRun: resolveReportRetentionDryRun(readOptionalEnv('FLUXER_REPORT_RETENTION_DRY_RUN'))},
	);
};

export default expireReportSnapshots;
