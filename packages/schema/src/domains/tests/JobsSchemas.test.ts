// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {JobLedgerEntrySchema, ListJobsQuery, ListJobsRequest} from '../admin/JobsSchemas';

describe('jobs schemas', () => {
	test('the job status enum rejects failed, which the ledger never writes', () => {
		expect(ListJobsQuery.safeParse({status: 'failed'}).success).toBe(false);
		expect(ListJobsRequest.safeParse({status: 'failed'}).success).toBe(false);
		expect(JobLedgerEntrySchema.shape.status.safeParse('failed').success).toBe(false);
	});

	test('the job status enum accepts every status the ledger writes', () => {
		for (const status of ['queued', 'running', 'succeeded', 'cancelled', 'deadletter']) {
			expect(ListJobsQuery.safeParse({status}).success).toBe(true);
			expect(JobLedgerEntrySchema.shape.status.safeParse(status).success).toBe(true);
		}
	});
});
