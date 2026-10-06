// SPDX-License-Identifier: AGPL-3.0-or-later

import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {JobByIdRow, JobStatus} from '@app/api/database/types/JobLedgerTypes';
import type {ListJobsCursor, ListJobsFilters} from '@app/api/jobs/IJobLedgerRepository';
import {JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';
import {JobsByDayBucket, JobsById} from '@app/api/Tables';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const DAY_MS = 86_400_000;
const HOUR_MS = 3_600_000;
const TODAY_NOON = new Date(`${new Date().toISOString().slice(0, 10)}T12:00:00.000Z`);
const ADMIN_USER_ID = 4_242n;

interface ListedPage {
	ids: Array<bigint>;
	cursor: ListJobsCursor | null;
}

function at(daysAgo: number, hour: number): Date {
	return new Date(TODAY_NOON.getTime() - daysAgo * DAY_MS + (hour - 12) * HOUR_MS);
}

async function seedJob(
	jobId: bigint,
	createdAt: Date,
	opts: {taskType?: string; requestedBy?: bigint | null; status?: JobStatus; bucketOnly?: boolean} = {},
): Promise<void> {
	const taskType = opts.taskType ?? 'A';
	const requestedBy = opts.requestedBy ?? null;
	await upsertOne(
		JobsByDayBucket.insert({
			bucket_day: createdAt.toISOString().slice(0, 10),
			created_at: createdAt,
			job_id: jobId,
			task_type: taskType,
			status: 'queued',
			requested_by_user_id: requestedBy,
		}),
	);
	if (opts.bucketOnly) return;
	const row: JobByIdRow = {
		job_id: jobId,
		task_type: taskType,
		status: opts.status ?? 'queued',
		progress_current: null,
		progress_total: null,
		progress_message: null,
		payload: '{}',
		result: null,
		error_message: null,
		created_at: createdAt,
		started_at: null,
		completed_at: null,
		requested_by_user_id: requestedBy,
		audit_log_reason: null,
		jet_stream_seq: null,
		jet_stream_lane: 'batch',
		attempts: 0,
		max_attempts: 5,
		run_at: null,
		cancel_requested: false,
		context_link: null,
	};
	await upsertOne(JobsById.insert(row));
}

async function listPages(opts: {
	limit: number;
	filters?: ListJobsFilters;
	maxLookbackDays?: number;
	cursor?: ListJobsCursor | null;
}): Promise<Array<ListedPage>> {
	const repository = new JobLedgerRepository();
	const pages: Array<ListedPage> = [];
	let cursor = opts.cursor ?? null;
	for (let page = 0; page < 50; page += 1) {
		const result = await repository.listJobs({
			limit: opts.limit,
			cursor,
			filters: opts.filters ?? {},
			maxLookbackDays: opts.maxLookbackDays ?? 14,
		});
		pages.push({ids: result.jobs.map((job) => job.job_id), cursor: result.nextCursor});
		if (result.nextCursor === null) return pages;
		cursor = {
			bucketDay: result.nextCursor.bucketDay,
			createdAt: new Date(result.nextCursor.createdAt.toISOString()),
			jobId: BigInt(result.nextCursor.jobId.toString()),
		};
	}
	throw new Error('listJobs never stopped paging');
}

function expectPages(pages: Array<ListedPage>, limit: number, expected: Array<Array<bigint>>): void {
	expect(pages.map((page) => page.ids)).toEqual(expected);
	const ids = pages.flatMap((page) => page.ids);
	expect(new Set(ids).size).toBe(ids.length);
	for (const page of pages.slice(0, -1)) {
		expect(page.ids).toHaveLength(limit);
		expect(page.cursor?.jobId).toBe(page.ids.at(-1));
	}
	expect(pages.at(-1)?.cursor).toBeNull();
}

export function describeListJobsPaging(): void {
	describe('listJobs paging', () => {
		beforeEach(() => {
			vi.useFakeTimers({toFake: ['Date']});
			vi.setSystemTime(TODAY_NOON);
		});

		afterEach(() => {
			vi.useRealTimers();
		});

		it('walks one day newest first through a tie group larger than the page', async () => {
			await seedJob(20n, at(0, 11));
			for (const jobId of [11n, 12n, 13n, 14n, 15n]) {
				await seedJob(jobId, at(0, 10));
			}
			await seedJob(9n, at(0, 9));
			await seedJob(5n, at(0, 8));
			await seedJob(6n, at(0, 8));

			const pages = await listPages({limit: 2});

			expectPages(pages, 2, [[20n, 15n], [14n, 13n], [12n, 11n], [9n, 6n], [5n]]);
			expect(pages[0]?.cursor).toEqual({
				bucketDay: at(0, 10).toISOString().slice(0, 10),
				createdAt: at(0, 10),
				jobId: 15n,
			});
		});

		it('crosses days and keeps the lookback window anchored on today', async () => {
			await seedJob(41n, at(0, 11));
			await seedJob(40n, at(0, 10));
			await seedJob(32n, at(1, 11));
			await seedJob(31n, at(1, 10));
			await seedJob(30n, at(1, 9));
			await seedJob(21n, at(2, 11));
			await seedJob(20n, at(2, 10));
			await seedJob(10n, at(3, 11));

			expectPages(await listPages({limit: 2, maxLookbackDays: 2}), 2, [[41n, 40n], [32n, 31n], [30n, 21n], [20n]]);
			expectPages(await listPages({limit: 2, maxLookbackDays: 3}), 2, [
				[41n, 40n],
				[32n, 31n],
				[30n, 21n],
				[20n, 10n],
				[],
			]);
			const outside = await new JobLedgerRepository().listJobs({
				limit: 2,
				cursor: {bucketDay: at(3, 12).toISOString().slice(0, 10), createdAt: at(3, 12), jobId: 1n},
				filters: {},
				maxLookbackDays: 2,
			});
			expect(outside).toEqual({jobs: [], nextCursor: null});
		});

		it('starts a cursor dated in the future at today', async () => {
			await seedJob(2n, at(0, 2));
			await seedJob(1n, at(1, 2));
			const farFuture = new Date('9999-12-31T00:00:00.000Z');

			expectPages(await listPages({limit: 5, cursor: {bucketDay: '9999-12-31', createdAt: farFuture, jobId: 1n}}), 5, [
				[2n, 1n],
			]);
		}, 2_000);

		it('never lists a day past the 90-day retention', async () => {
			await seedJob(890n, at(89, 11));
			await seedJob(910n, at(91, 11));

			expectPages(await listPages({limit: 10, maxLookbackDays: 120}), 10, [[890n]]);
		});

		it('fills a page past rows whose job record is missing instead of leaving the day', async () => {
			for (let hour = 1; hour <= 8; hour += 1) {
				await seedJob(BigInt(hour), at(0, hour), {bucketOnly: [3, 6, 7].includes(hour)});
			}

			expectPages(await listPages({limit: 3}), 3, [
				[8n, 5n, 4n],
				[2n, 1n],
			]);
		});

		it('fills pages through task type, requester and status filters across days', async () => {
			await seedJob(60n, at(0, 11));
			await seedJob(59n, at(0, 10.5), {taskType: 'B'});
			await seedJob(58n, at(0, 10));
			await seedJob(57n, at(0, 9.5), {taskType: 'B', requestedBy: ADMIN_USER_ID});
			await seedJob(56n, at(0, 9), {status: 'succeeded'});
			await seedJob(55n, at(0, 8.5), {taskType: 'B'});
			await seedJob(50n, at(1, 11));
			await seedJob(49n, at(1, 10.5), {taskType: 'B', requestedBy: ADMIN_USER_ID});
			await seedJob(48n, at(1, 10), {bucketOnly: true});
			await seedJob(47n, at(1, 9.5));

			expectPages(await listPages({limit: 2, filters: {taskType: 'A'}}), 2, [[60n, 58n], [56n, 50n], [47n]]);
			expectPages(await listPages({limit: 1, filters: {status: 'succeeded'}}), 1, [[56n], []]);
			expectPages(await listPages({limit: 5, filters: {requestedByUserId: ADMIN_USER_ID}}), 5, [[57n, 49n]]);
		});

		it('resumes after a cursor whose job was discarded between pages', async () => {
			for (let hour = 1; hour <= 5; hour += 1) {
				await seedJob(BigInt(hour), at(0, hour));
			}
			const repository = new JobLedgerRepository();
			const first = await repository.listJobs({limit: 2, cursor: null, filters: {}, maxLookbackDays: 14});
			expect(first.jobs.map((job) => job.job_id)).toEqual([5n, 4n]);

			await repository.discardJob(4n, at(0, 4));

			expectPages(await listPages({limit: 2, cursor: first.nextCursor}), 2, [[3n, 2n], [1n]]);
		});

		it('returns an empty page after a page that filled exactly', async () => {
			await seedJob(1n, at(0, 1));
			await seedJob(2n, at(0, 2));

			expectPages(await listPages({limit: 2}), 2, [[2n, 1n], []]);
		});
	});
}
