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
import {ms} from 'itty-time';

const SCAN_PAGE_SIZE = 500;
const REPORT_EVIDENCE_COPY_GRACE_MS = ms('1 minute');

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

async function deleteReportEvidence(
	storageService: IStorageService,
	report: IARSubmission,
	ledger: ReportEvidenceLedger,
	options: {now: Date; dryRun: boolean},
): Promise<ReportDeletionResult> {
	const result: ReportDeletionResult = {objectsDeleted: 0, sharedObjectsKept: 0};
	const cutoff = Math.min(
		options.now.getTime(),
		Math.max(
			options.now.getTime() - REPORT_EVIDENCE_COPY_GRACE_MS,
			report.reportedAt.getTime() + REPORT_EVIDENCE_COPY_GRACE_MS,
		),
	);
	for (const key of await storedObjectKeys(storageService, report)) {
		if (ledger.retainedKeys.has(key)) {
			result.sharedObjectsKept++;
			continue;
		}
		if (ledger.deletedKeys.has(key)) continue;
		const metadata = await storageService.getObjectMetadata(Config.s3.buckets.reports, key);
		if (metadata?.lastModified && metadata.lastModified.getTime() >= cutoff) {
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
	return result;
}

async function rejectUnclaimedReport(
	deps: ReportDeletionDeps,
	report: IARSubmission,
	ledger: ReportEvidenceLedger,
	now: Date,
): Promise<never> {
	for (const key of referencedObjectKeys(report)) {
		ledger.retainedKeys.add(key);
	}
	const current = await deps.reportRepository.getReport(report.reportId);
	if (!current) {
		await deps.reportSearchService?.deleteReport(report.reportId);
		throw new UnknownReportError();
	}
	await deps.reportSearchService?.updateReport(current);
	if (isReportUnderLegalHold(current, now)) {
		throw new ReportUnderLegalHoldError();
	}
	throw new UnknownReportError();
}

export async function deleteReportWithEvidence(
	deps: ReportDeletionDeps,
	report: IARSubmission,
	ledger: ReportEvidenceLedger,
	options: {now: Date; dryRun: boolean},
): Promise<ReportDeletionResult> {
	const {reportRepository, storageService, reportSearchService} = deps;
	if (options.dryRun) {
		return deleteReportEvidence(storageService, report, ledger, options);
	}
	await reportSearchService?.deleteReport(report.reportId);
	const row = await reportRepository.deleteReport(report);
	if (!row) {
		return rejectUnclaimedReport(deps, report, ledger, options.now);
	}
	try {
		const result = await deleteReportEvidence(storageService, report, ledger, options);
		await reportRepository.releaseDeletedReport(report);
		return result;
	} catch (error) {
		await reportRepository.restoreReport(row);
		await reportSearchService?.updateReport(report);
		throw error;
	}
}

export async function clearCopiedContextWithEvidence(
	deps: ReportDeletionDeps,
	report: IARSubmission,
	ledger: ReportEvidenceLedger,
	options: {now: Date; dryRun: boolean},
): Promise<ReportDeletionResult> {
	const {reportRepository, storageService, reportSearchService} = deps;
	if (options.dryRun) {
		return deleteReportEvidence(storageService, report, ledger, options);
	}
	await reportSearchService?.updateReport({
		...report,
		messageContext: null,
		reportedProfileSnapshot: null,
		additionalInfo: null,
	});
	const row = await reportRepository.clearCopiedContext(report);
	if (!row) {
		return rejectUnclaimedReport(deps, report, ledger, options.now);
	}
	try {
		return await deleteReportEvidence(storageService, report, ledger, options);
	} catch (error) {
		await reportRepository.restoreCopiedContext(row);
		throw error;
	}
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
