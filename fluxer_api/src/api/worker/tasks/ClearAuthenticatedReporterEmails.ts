// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {IReportRepository} from '@app/api/report/IReportRepository';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const DEFAULT_SCAN_PAGE_SIZE = 500;

export interface ClearAuthenticatedReporterEmailsSummary {
	scanned: number;
	cleared: number;
	skipped: number;
	failed: number;
}

export async function clearAuthenticatedReporterEmails(
	reportRepository: IReportRepository,
	pageSize = DEFAULT_SCAN_PAGE_SIZE,
): Promise<ClearAuthenticatedReporterEmailsSummary> {
	const summary: ClearAuthenticatedReporterEmailsSummary = {scanned: 0, cleared: 0, skipped: 0, failed: 0};
	let cursor: ReportID | undefined;
	while (true) {
		const page = await reportRepository.listAllReportsPaginated(pageSize, cursor);
		for (const report of page) {
			summary.scanned++;
			if (!report.reporterId || report.reporterEmail === null) continue;
			try {
				if (await reportRepository.clearReporterEmail(report.reportId, report.reporterId)) {
					summary.cleared++;
				} else {
					summary.skipped++;
				}
			} catch (error) {
				summary.failed++;
				Logger.error({error, reportId: report.reportId.toString()}, 'Failed to clear a stored reporter email');
			}
		}
		if (page.length < pageSize) break;
		cursor = page[page.length - 1]!.reportId;
	}
	Logger.info(summary, 'Cleared stored email addresses from account reports');
	return summary;
}

const clearAuthenticatedReporterEmailsTask: WorkerTaskHandler = async () => {
	await clearAuthenticatedReporterEmails(getWorkerDependencies().reportRepository);
};

export default clearAuthenticatedReporterEmailsTask;
