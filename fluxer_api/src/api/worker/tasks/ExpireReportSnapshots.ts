// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {Logger} from '@app/api/Logger';
import {
	clearCopiedContextWithEvidence,
	deleteReportWithEvidence,
	forEachStoredReport,
	isReportUnderLegalHold,
	type ReportDeletionDeps,
	type ReportEvidenceLedger,
	referencedObjectKeys,
} from '@app/api/report/ReportDeletion';
import {type IARSubmission, ReportStatus} from '@app/api/report/ReportModels';
import {getReportSearchService} from '@app/api/SearchFactory';
import {EnforcementDecisionEvidence} from '@app/api/Tables';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

export const REPORT_COMPLAINT_WINDOW_DAYS = 184;
export const CHILD_SAFETY_REPORT_RETENTION_DAYS = 365;
export const REPORT_CONTENT_RETENTION_DAYS = 365;
const CHILD_SAFETY_CATEGORY = 'child_safety';
const DECISION_EVIDENCE_RETENTION_MS = EnforcementDecisionEvidence.defaultTtlSeconds! * 1000;
const DEFAULT_SCAN_PAGE_SIZE = 500;

export interface ReportRetentionPolicy {
	retentionDays: number;
}

export interface ReportRetentionDeps extends ReportDeletionDeps {
	decisionRepository: Pick<EnforcementDecisionRepository, 'listReportLinks'>;
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
	pending: number;
	expired: number;
	contextExpired: number;
	held: number;
	deleted: number;
	contextCleared: number;
	objectsDeleted: number;
	sharedObjectsKept: number;
	failed: number;
}

export interface ReportRetentionDates {
	deleteAt: number | null;
	contextExpiresAt: number | null;
}

type ReportRetentionState = 'expired' | 'context_expired' | 'held' | 'retained';

function contentEndsAt(report: IARSubmission): number | null {
	const filedAt = report.reportedAt.getTime();
	return Number.isNaN(filedAt) ? null : filedAt + REPORT_CONTENT_RETENTION_DAYS * ms('1 day');
}

export function reportRetentionDates(
	report: IARSubmission,
	policy: ReportRetentionPolicy,
	decidedAt: Date | null,
): ReportRetentionDates {
	const contentEnd = contentEndsAt(report);
	if (report.status !== ReportStatus.RESOLVED) {
		return {deleteAt: null, contextExpiresAt: contentEnd};
	}
	const resolvedAt = (report.resolvedAt ?? report.reportedAt).getTime();
	if (Number.isNaN(resolvedAt)) {
		return {deleteAt: null, contextExpiresAt: contentEnd};
	}
	const childSafety = report.category === CHILD_SAFETY_CATEGORY;
	const ends = [resolvedAt + policy.retentionDays * ms('1 day')];
	if (childSafety) {
		ends.push(resolvedAt + CHILD_SAFETY_REPORT_RETENTION_DAYS * ms('1 day'));
	}
	if (decidedAt) {
		ends.push(decidedAt.getTime() + DECISION_EVIDENCE_RETENTION_MS);
	}
	const dropsContextEarly = report.reporterOutcome === 'no_violation' && !childSafety && !decidedAt;
	const contextEnds = [
		contentEnd,
		dropsContextEarly ? resolvedAt + REPORT_COMPLAINT_WINDOW_DAYS * ms('1 day') : null,
	].filter((end): end is number => end !== null);
	return {
		deleteAt: Math.max(...ends),
		contextExpiresAt: contextEnds.length > 0 ? Math.min(...contextEnds) : null,
	};
}

function hasCopiedContext(report: IARSubmission): boolean {
	return (
		(report.messageContext?.length ?? 0) > 0 ||
		report.reportedProfileSnapshot !== null ||
		report.additionalInfo !== null
	);
}

function reportRetentionState(
	report: IARSubmission,
	now: Date,
	policy: ReportRetentionPolicy,
	decidedAt: Date | null,
): ReportRetentionState {
	const {deleteAt, contextExpiresAt} = reportRetentionDates(report, policy, decidedAt);
	let due: 'expired' | 'context_expired' | null;
	if (deleteAt !== null && deleteAt <= now.getTime()) {
		due = 'expired';
	} else if (contextExpiresAt !== null && contextExpiresAt <= now.getTime() && hasCopiedContext(report)) {
		due = 'context_expired';
	} else {
		due = null;
	}
	if (due === null) {
		return 'retained';
	}
	return isReportUnderLegalHold(report, now) ? 'held' : due;
}

async function latestDecisionByReport(
	decisionRepository: ReportRetentionDeps['decisionRepository'],
	pageSize: number,
): Promise<Map<bigint, Date>> {
	const decidedAt = new Map<bigint, Date>();
	let cursor: bigint | undefined;
	while (true) {
		const page = await decisionRepository.listReportLinks(pageSize, cursor);
		for (const decision of page) {
			for (const reportId of decision.report_ids ?? []) {
				const known = decidedAt.get(reportId);
				if (!known || known.getTime() < decision.created_at.getTime()) {
					decidedAt.set(reportId, decision.created_at);
				}
			}
		}
		if (page.length < pageSize) return decidedAt;
		cursor = page[page.length - 1]!.decision_id;
	}
}

export async function processReportRetention(
	deps: ReportRetentionDeps,
	options: ReportRetentionOptions,
): Promise<ReportRetentionSummary> {
	const {reportRepository} = deps;
	const {now, dryRun, policy} = options;
	const pageSize = options.pageSize ?? DEFAULT_SCAN_PAGE_SIZE;
	const summary: ReportRetentionSummary = {
		dryRun,
		scanned: 0,
		pending: 0,
		expired: 0,
		contextExpired: 0,
		held: 0,
		deleted: 0,
		contextCleared: 0,
		objectsDeleted: 0,
		sharedObjectsKept: 0,
		failed: 0,
	};
	const decidedAt = await latestDecisionByReport(deps.decisionRepository, pageSize);
	const stateOf = (report: IARSubmission) =>
		reportRetentionState(report, now, policy, decidedAt.get(report.reportId) ?? null);
	const retainedKeys = new Set<string>();
	const due: Array<{reportId: ReportID; state: 'expired' | 'context_expired'}> = [];
	await forEachStoredReport(
		reportRepository,
		(report) => {
			summary.scanned++;
			if (report.status !== ReportStatus.RESOLVED) {
				summary.pending++;
			}
			const state = stateOf(report);
			if (state === 'expired' || state === 'context_expired') {
				due.push({reportId: report.reportId, state});
				return;
			}
			if (state === 'held') {
				summary.held++;
			}
			for (const key of referencedObjectKeys(report)) {
				retainedKeys.add(key);
			}
		},
		pageSize,
	);
	summary.expired = due.filter((entry) => entry.state === 'expired').length;
	summary.contextExpired = due.length - summary.expired;
	const ledger: ReportEvidenceLedger = {retainedKeys, deletedKeys: new Set<string>()};
	for (const {reportId, state} of due) {
		try {
			const report = await reportRepository.getReport(reportId);
			if (!report) continue;
			if (stateOf(report) !== state) {
				for (const key of referencedObjectKeys(report)) {
					retainedKeys.add(key);
				}
				continue;
			}
			const apply = state === 'expired' ? deleteReportWithEvidence : clearCopiedContextWithEvidence;
			const result = await apply(deps, report, ledger, {now, dryRun});
			summary.objectsDeleted += result.objectsDeleted;
			summary.sharedObjectsKept += result.sharedObjectsKept;
			if (state === 'expired') {
				summary.deleted++;
			} else {
				summary.contextCleared++;
			}
		} catch (error) {
			summary.failed++;
			Logger.error({error, reportId: reportId.toString(), state}, 'Failed to apply report retention');
		}
	}
	Logger.info(
		{
			...summary,
			retentionDays: policy.retentionDays,
			complaintWindowDays: REPORT_COMPLAINT_WINDOW_DAYS,
			contentRetentionDays: REPORT_CONTENT_RETENTION_DAYS,
		},
		'Processed report retention',
	);
	return summary;
}

const expireReportSnapshots: WorkerTaskHandler = async () => {
	const {reportRepository, storageService} = getWorkerDependencies();
	const {days, dryRun} = Config.reportRetention;
	await processReportRetention(
		{
			reportRepository,
			storageService,
			reportSearchService: getReportSearchService(),
			decisionRepository: new EnforcementDecisionRepository(),
		},
		{now: new Date(), dryRun, policy: {retentionDays: days}},
	);
};

export default expireReportSnapshots;
