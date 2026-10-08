// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {IAR_SUBMISSION_COLUMNS, type IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {GuildDataRepository} from '@app/api/guild/repositories/GuildDataRepository';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {setInjectedSearchProvider} from '@app/api/SearchFactory';
import {warmupAdminSearchIndexes} from '@app/api/search/SearchWarmup';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {InMemorySearchProvider} from '@app/api/test/search/InMemorySearchProvider';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const REPORT_COUNT = 250;
const PAGE_SIZE = 100;
const MAX_PAGES = 10;

class BoundedReportRepository extends ReportRepository {
	pages = 0;

	override async listAllReportsPaginated(limit: number, lastReportId?: ReportID) {
		this.pages += 1;
		if (this.pages > MAX_PAGES) {
			throw new Error(`report scan did not end after ${MAX_PAGES} pages`);
		}
		return super.listAllReportsPaginated(limit, lastReportId);
	}
}

function reportRow(reportId: bigint): IARSubmissionRow {
	const row: Record<string, unknown> = Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null]));
	return {
		...row,
		report_id: reportId,
		reported_at: new Date(Number(reportId % 1_000_000n) * 1000),
		status: 0,
		report_type: 1,
		category: 'other',
	} as IARSubmissionRow;
}

function shuffledReportIds(): Array<bigint> {
	const ids = Array.from({length: REPORT_COUNT}, (_, index) => 1_400_000_000_000_000_000n + BigInt(index) * 7919n);
	return ids.map((_, index) => ids[(index * 113) % ids.length]!);
}

async function seedReports(ids: Array<bigint>): Promise<void> {
	const repository = new ReportRepository();
	for (const id of ids) {
		await repository.createReport(reportRow(id));
	}
}

describe('InMemoryCassandraQueryExecutor', () => {
	let executor: InMemoryCassandraQueryExecutor;

	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	it('returns rows in primary key order when a select has no ordering', async () => {
		const ids = shuffledReportIds();
		expect(new Set(ids).size).toBe(REPORT_COUNT);
		await seedReports(ids);
		const firstPage = await new ReportRepository().listAllReportsPaginated(5);
		const sorted = [...ids].sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
		expect(firstPage.map((report) => report.reportId)).toEqual(sorted.slice(0, 5));
	});

	it('pages a token scan to the end with every row exactly once', async () => {
		const ids = shuffledReportIds();
		await seedReports(ids);
		const repository = new BoundedReportRepository();
		const seen: Array<bigint> = [];
		const pageSizes: Array<number> = [];
		let cursor: ReportID | undefined;
		while (true) {
			const page = await repository.listAllReportsPaginated(PAGE_SIZE, cursor);
			pageSizes.push(page.length);
			seen.push(...page.map((report) => report.reportId));
			if (page.length < PAGE_SIZE) break;
			cursor = page[page.length - 1]!.reportId;
		}
		expect(pageSizes).toEqual([100, 100, 50]);
		expect(seen).toHaveLength(REPORT_COUNT);
		expect(new Set(seen).size).toBe(REPORT_COUNT);
		expect([...seen].sort((a, b) => (a < b ? -1 : a > b ? 1 : 0))).toEqual(seen);
	});

	it('finishes the admin search warmup with more than one batch of reports', async () => {
		await seedReports(shuffledReportIds());
		const provider = new InMemorySearchProvider();
		await provider.initialize();
		setInjectedSearchProvider(provider);
		const reportRepository = new BoundedReportRepository();
		try {
			await warmupAdminSearchIndexes({
				userRepository: new UserRepository(),
				guildRepository: new GuildDataRepository(),
				reportRepository,
				logger: new NoopLogger(),
			});
			const indexed = await provider.getReportSearchService().search('', {}, {limit: 1});
			expect(indexed.total).toBe(REPORT_COUNT);
			expect(reportRepository.pages).toBe(3);
		} finally {
			setInjectedSearchProvider(undefined);
			await provider.shutdown();
		}
	});
});
