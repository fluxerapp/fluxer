// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {JobLedgerEntrySchema, ListJobsQuery, ListJobsRequest} from '../admin/JobsSchemas';

describe('jobs schemas', () => {
	test('the job status enum rejects failed, which the ledger never writes', () => {
		expect(ListJobsQuery.safeParse({status: 'failed'}).success).toBe(false);
		expect(ListJobsRequest.safeParse({status: 'failed'}).success).toBe(false);
		expect(JobLedgerEntrySchema.shape.status.safeParse('failed').success).toBe(false);
	});

	test.each([
		'queued',
		'running',
		'succeeded',
		'cancelled',
		'deadletter',
	])('accepts ledger status %s across query, request, and response', (status) => {
		expect(ListJobsQuery.parse({status}).status).toBe(status);
		expect(ListJobsRequest.parse({status}).status).toBe(status);
		expect(JobLedgerEntrySchema.shape.status.parse(status)).toBe(status);
	});
});

describe('job pagination cursor', () => {
	const cursor = {
		cursor_bucket_day: '2024-02-29',
		cursor_created_at: '2024-02-29T12:00:00Z',
		cursor_job_id: '123',
	};

	test('accepts a complete cursor and applies query defaults', () => {
		expect(ListJobsQuery.parse(cursor)).toEqual({...cursor, limit: 50, max_lookback_days: 14});
	});

	test.each([
		'cursor_bucket_day',
		'cursor_created_at',
		'cursor_job_id',
	] as const)('rejects a cursor missing %s', (field) => {
		expect(ListJobsQuery.safeParse({...cursor, [field]: undefined})).toMatchObject({
			success: false,
			error: {issues: [{path: [field]}]},
		});
	});

	test.each([
		{cursor_bucket_day: '2023-02-29'},
		{cursor_bucket_day: '2024-04-31'},
		{cursor_created_at: '2024-02-30T12:00:00Z'},
		{cursor_job_id: '01'},
	])('rejects malformed cursor component %j', (invalid) => {
		expect(ListJobsQuery.safeParse({...cursor, ...invalid}).success).toBe(false);
	});
});
