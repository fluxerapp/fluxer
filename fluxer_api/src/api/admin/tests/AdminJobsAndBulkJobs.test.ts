// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface JobEntry {
	job_id: string;
	task_type: string;
	status: string;
}

interface ListJobsResponse {
	jobs: Array<JobEntry>;
	next_cursor: {bucket_day: string; created_at: string; job_id: string} | null;
}

interface ActiveJobsResponse {
	jobs: Array<JobEntry>;
}

interface BulkJobResponse {
	job_id: string;
}

describe('Admin jobs and bulk jobs', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('lists jobs with query filters and lists the active index separately', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		const listed = await createBuilder<ListJobsResponse>(harness, `${updated.token}`)
			.get('/admin/jobs?limit=10&max_lookback_days=1&status=queued')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(Array.isArray(listed.jobs)).toBe(true);
		const active = await createBuilder<ActiveJobsResponse>(harness, `${updated.token}`)
			.get('/admin/jobs/active')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(Array.isArray(active.jobs)).toBe(true);
	});
	test('rejects a partially supplied list cursor', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		await createBuilder(harness, `${updated.token}`)
			.get('/admin/jobs?cursor_job_id=123')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('rejects a list cursor whose bucket day is not a calendar date', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		const response = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
			}>;
		}>(harness, `${updated.token}`)
			.get('/admin/jobs?cursor_bucket_day=nonsense&cursor_created_at=2026-01-01T00:00:00.000Z&cursor_job_id=123')
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		expect(response.errors[0]?.path).toBe('cursor_bucket_day');
	});
	test('rejects a list cursor whose creation time is not an ISO 8601 timestamp', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		const response = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
			}>;
		}>(harness, `${updated.token}`)
			.get('/admin/jobs?cursor_bucket_day=2026-01-01&cursor_created_at=yesterday&cursor_job_id=123')
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		expect(response.errors[0]?.path).toBe('cursor_created_at');
	});
	test('queues a bulk job and exposes it through the job routes', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, [
			'admin:authenticate',
			'bulk:update:user_flags',
			'jobs:view',
			'jobs:cancel',
		]);
		const target = await createTestAccount(harness);
		const queued = await createBuilder<BulkJobResponse>(harness, `${updated.token}`)
			.post('/admin/bulk-jobs')
			.body({task: 'update_user_flags', user_ids: [target.userId], add_flags: [], remove_flags: []})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(queued.job_id).toBeTruthy();
		const cancellation = await createBuilder<{cancelled: boolean}>(harness, `${updated.token}`)
			.put(`/admin/jobs/${queued.job_id}/cancellation`)
			.body({})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(typeof cancellation.cancelled).toBe('boolean');
	});
	test('answers an unknown job identifier with job_not_found', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		await createBuilder(harness, `${updated.token}`)
			.get('/admin/jobs/123456789012345678')
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('job cancellation requires jobs:cancel rather than jobs:view', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		await createBuilder(harness, `${updated.token}`)
			.put('/admin/jobs/123456789012345678/cancellation')
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('holding one bulk ACL does not authorise another task', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:update:user_flags']);
		await createBuilder(harness, `${updated.token}`)
			.post('/admin/bulk-jobs')
			.body({task: 'add_guild_members', guild_id: '1', user_ids: ['2']})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('rejects a malformed bulk job body before evaluating any ACL', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate']);
		await createBuilder(harness, `${updated.token}`)
			.post('/admin/bulk-jobs')
			.body({task: 'not_a_task'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
