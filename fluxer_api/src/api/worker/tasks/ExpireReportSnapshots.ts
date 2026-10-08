// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {type IARSubmission, ReportStatus} from '@app/api/report/IReportRepository';
import {
	deleteReportWithEvidence,
	forEachStoredReport,
	isReportUnderLegalHold,
	type ReportDeletionDeps,
	referencedObjectKeys,
} from '@app/api/report/ReportDeletion';
import {getReportSearchService} from '@app/api/SearchFactory';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

export interface ReportRetentionPolicy {
	retentionDays: number;
	resolvedRetentionDays: number | null;
}

export interface ReportRetentionOptions {
	now: Date;
	dryRun: boolean;
	policy: ReportRetentionPolicy;
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

function reportExpiresAt(report: IARSubmission, policy: ReportRetentionPolicy): number {
	const reportedAt = report.reportedAt instanceof Date ? report.reportedAt.getTime() : Number.NaN;
	const byAge = reportedAt + policy.retentionDays * ms('1 day');
	if (policy.resolvedRetentionDays === null || report.status !== ReportStatus.RESOLVED) {
		return byAge;
	}
	const resolvedAt = report.resolvedAt instanceof Date ? report.resolvedAt.getTime() : Number.NaN;
	if (Number.isNaN(resolvedAt)) {
		return byAge;
	}
	return Math.min(byAge, resolvedAt + policy.resolvedRetentionDays * ms('1 day'));
}

function reportRetentionState(report: IARSubmission, now: Date, policy: ReportRetentionPolicy): ReportRetentionState {
	const expiresAt = reportExpiresAt(report, policy);
	if (Number.isNaN(expiresAt) || expiresAt > now.getTime()) {
		return 'retained';
	}
	if (isReportUnderLegalHold(report, now)) {
		return 'held';
	}
	return 'expired';
}

export async function processReportRetention(
	deps: ReportDeletionDeps,
	options: ReportRetentionOptions,
): Promise<ReportRetentionSummary> {
	const {reportRepository} = deps;
	const {now, dryRun, policy} = options;
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
	await forEachStoredReport(
		reportRepository,
		(report) => {
			summary.scanned++;
			const state = reportRetentionState(report, now, policy);
			if (state === 'expired') {
				expiredIds.push(report.reportId);
				return;
			}
			if (state === 'held') {
				summary.held++;
			}
			for (const key of referencedObjectKeys(report)) {
				retainedKeys.add(key);
			}
		},
		options.pageSize,
	);
	summary.expired = expiredIds.length;
	const ledger = {retainedKeys, deletedKeys: new Set<string>()};
	for (const reportId of expiredIds) {
		try {
			const report = await reportRepository.getReport(reportId);
			if (!report) continue;
			if (reportRetentionState(report, now, policy) !== 'expired') {
				for (const key of referencedObjectKeys(report)) {
					retainedKeys.add(key);
				}
				continue;
			}
			const result = await deleteReportWithEvidence(deps, report, ledger, {now, dryRun});
			summary.objectsDeleted += result.objectsDeleted;
			summary.sharedObjectsKept += result.sharedObjectsKept;
			summary.deleted++;
		} catch (error) {
			summary.failed++;
			Logger.error({error, reportId: reportId.toString()}, 'Failed to delete an expired report');
		}
	}
	Logger.info(
		{
			...summary,
			retentionDays: policy.retentionDays,
			resolvedRetentionDays: policy.resolvedRetentionDays,
		},
		'Processed report retention',
	);
	return summary;
}

const expireReportSnapshots: WorkerTaskHandler = async () => {
	const {reportRepository, storageService} = getWorkerDependencies();
	const {days, resolvedDays, dryRun} = Config.reportRetention;
	await processReportRetention(
		{reportRepository, storageService, reportSearchService: getReportSearchService()},
		{now: new Date(), dryRun, policy: {retentionDays: days, resolvedRetentionDays: resolvedDays}},
	);
};

export default expireReportSnapshots;
