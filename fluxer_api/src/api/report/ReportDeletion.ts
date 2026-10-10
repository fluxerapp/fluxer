// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IARSubmission} from '@app/api/report/ReportModels';
import type {ReportRepository} from '@app/api/report/ReportRepository';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import {ReportUnderLegalHoldError} from '@fluxer/errors/src/domains/moderation/ReportUnderLegalHoldError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import {listReportProfileSnapshotAssets} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';

const SCAN_PAGE_SIZE = 500;

export interface ReportDeletionDeps {
	reportRepository: ReportRepository;
	storageService: IStorageService;
	reportSearchService: IReportSearchService | null;
}

export interface ReportEvidenceLedger {
	retainedKeys: Set<string>;
	deletedKeys: Set<string>;
}

export interface ReportDeletionResult {
	objectsDeleted: number;
	sharedObjectsKept: number;
}

export function isReportUnderLegalHold(report: IARSubmission, now: Date): boolean {
	return report.legalHoldUntil != null && report.legalHoldUntil.getTime() > now.getTime();
}

export function referencedObjectKeys(report: IARSubmission): Array<string> {
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

export async function forEachStoredReport(
	reportRepository: ReportRepository,
	visit: (report: IARSubmission) => void,
	pageSize = SCAN_PAGE_SIZE,
): Promise<void> {
	let cursor: ReportID | undefined;
	while (true) {
		const page = await reportRepository.listAllReportsPaginated(pageSize, cursor);
		for (const report of page) {
			visit(report);
		}
		if (page.length < pageSize) return;
		cursor = page[page.length - 1]!.reportId;
	}
}

async function storedObjectKeys(storageService: IStorageService, report: IARSubmission): Promise<Set<string>> {
	const keys = new Set(referencedObjectKeys(report));
	const listed = await storageService.listObjects({
		bucket: Config.s3.buckets.reports,
		prefix: `reports/${report.reportId}/profile/`,
	});
	for (const object of listed) {
		keys.add(object.key);
	}
	return keys;
}

export async function deleteReportWithEvidence(
	deps: ReportDeletionDeps,
	report: IARSubmission,
	ledger: ReportEvidenceLedger,
	options: {now: Date; dryRun: boolean},
): Promise<ReportDeletionResult> {
	const {reportRepository, storageService, reportSearchService} = deps;
	const result: ReportDeletionResult = {objectsDeleted: 0, sharedObjectsKept: 0};
	for (const key of await storedObjectKeys(storageService, report)) {
		if (ledger.retainedKeys.has(key)) {
			result.sharedObjectsKept++;
			continue;
		}
		if (ledger.deletedKeys.has(key)) continue;
		const metadata = await storageService.getObjectMetadata(Config.s3.buckets.reports, key);
		if (metadata?.lastModified && metadata.lastModified.getTime() >= options.now.getTime()) {
			ledger.retainedKeys.add(key);
			result.sharedObjectsKept++;
			continue;
		}
		if (!options.dryRun) {
			await storageService.deleteObject(Config.s3.buckets.reports, key);
		}
		ledger.deletedKeys.add(key);
		result.objectsDeleted++;
	}
	if (!options.dryRun) {
		await reportSearchService?.deleteReport(report.reportId);
		await reportRepository.deleteReport(report.reportId);
	}
	return result;
}

async function loadDeletableReport(
	reportRepository: ReportRepository,
	reportId: ReportID,
	now: Date,
): Promise<IARSubmission> {
	const report = await reportRepository.getReport(reportId);
	if (!report) {
		throw new UnknownReportError();
	}
	if (isReportUnderLegalHold(report, now)) {
		throw new ReportUnderLegalHoldError();
	}
	return report;
}

export async function deleteReportNow(
	deps: ReportDeletionDeps,
	reportId: ReportID,
	now: Date,
): Promise<{report: IARSubmission} & ReportDeletionResult> {
	const {reportRepository} = deps;
	await loadDeletableReport(reportRepository, reportId, now);
	const retainedKeys = new Set<string>();
	await forEachStoredReport(reportRepository, (other) => {
		if (other.reportId === reportId) return;
		for (const key of referencedObjectKeys(other)) {
			retainedKeys.add(key);
		}
	});
	const report = await loadDeletableReport(reportRepository, reportId, now);
	const result = await deleteReportWithEvidence(
		deps,
		report,
		{retainedKeys, deletedKeys: new Set()},
		{now, dryRun: false},
	);
	return {report, ...result};
}
