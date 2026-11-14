// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';

const LEDGER_JOB_ID = 800000000000000101n;

async function seedLedgerJob(requestedByUserId: string): Promise<void> {
	await new JobLedgerRepository().createJob({
		jobId: LEDGER_JOB_ID,
		taskType: 'bulkUpdateUserFlags',
		payload: {},
		requestedByUserId: BigInt(requestedByUserId),
		auditLogReason: null,
		maxAttempts: 5,
		runAt: null,
		jetStreamLane: 'lifecycle',
		jetStreamSeq: null,
	});
}

export const JobsAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/jobs',
		exempt: true,
		async prepare({admin}) {
			await seedLedgerJob(admin.userId);
			return {request: {path: `/admin/jobs?limit=10&requested_by_user_id=${admin.userId}`}};
		},
	},
	{
		method: 'GET',
		route: '/admin/jobs/active',
		exempt: true,
		async prepare({admin}) {
			await seedLedgerJob(admin.userId);
			return {request: {path: '/admin/jobs/active'}};
		},
	},
	{
		method: 'GET',
		route: '/admin/jobs/:job_id',
		exempt: true,
		async prepare({admin}) {
			await seedLedgerJob(admin.userId);
			return {request: {path: `/admin/jobs/${LEDGER_JOB_ID}`}};
		},
	},
	{
		method: 'PUT',
		route: '/admin/jobs/:job_id/cancellation',
		async prepare({admin}) {
			await seedLedgerJob(admin.userId);
			return {
				request: {path: `/admin/jobs/${LEDGER_JOB_ID}/cancellation`, body: {}},
				expected: {
					action: 'cancel_job',
					targetType: 'bulk_job',
					targetId: LEDGER_JOB_ID.toString(),
					metadata: {cancelled: 'false'},
				},
			};
		},
	},
];
