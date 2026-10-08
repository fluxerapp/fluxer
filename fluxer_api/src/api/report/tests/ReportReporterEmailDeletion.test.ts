// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID, type ReportID} from '@app/api/BrandedTypes';
import {IAR_SUBMISSION_COLUMNS, type IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {deleteAccount, setPendingDeletionAt} from '@app/api/user/tests/UserTestUtils';
import {clearAuthenticatedReporterEmails} from '@app/api/worker/tasks/ClearAuthenticatedReporterEmails';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const NOTIFIER_EMAIL = 'notifier@example.com';
let sequence = 1_495_000_000_000_000_000n;

function nextId(): bigint {
	sequence += 1n;
	return sequence;
}

function serializeReport(value: unknown): string {
	return JSON.stringify(value, (_key, field) => (typeof field === 'bigint' ? field.toString() : field));
}

describe('reporter email after account deletion', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let repository: ReportRepository;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		repository = new ReportRepository();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function seedRow(reporterId: bigint | null, reporterEmail: string): Promise<ReportID> {
		const id = nextId();
		await repository.createReport({
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: id,
			reporter_id: reporterId,
			reporter_email: reporterEmail,
			reported_at: new Date(),
			status: 0,
			report_type: 1,
			category: 'harassment',
			reported_user_id: nextId(),
		} as IARSubmissionRow);
		return createReportID(id);
	}

	async function adminReportText(reportId: ReportID): Promise<{reporterEmail: string | null; text: string}> {
		const {response, text, json} = await createBuilder<{reporter_email: string | null}>(harness, admin.token)
			.get(`/admin/reports/${reportId}`)
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.OK);
		return {reporterEmail: json.reporter_email, text};
	}

	async function deleteReporterAccount(reporter: TestAccount): Promise<void> {
		await deleteAccount(harness, reporter.token, reporter.password);
		await setPendingDeletionAt(harness, reporter.userId, new Date(Date.now() - 60_000));
		await createBuilderWithoutAuth(harness)
			.post(`/test/worker/process-pending-deletion/${reporter.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	test('no stored report keeps the address of a deleted account', async () => {
		const reporter = await createTestAccount(harness);
		const reported = await createTestAccount(harness);
		const filed = await createBuilder<{report_id: string}>(harness, reporter.token)
			.post('/reports/user')
			.body({user_id: reported.userId, category: 'harassment'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const filedId = createReportID(BigInt(filed.report_id));
		const olderRow = await seedRow(BigInt(reporter.userId), reporter.email);
		const notice = await seedRow(null, NOTIFIER_EMAIL);

		expect((await repository.getReport(filedId))?.reporterEmail).toBeNull();
		expect((await adminReportText(filedId)).reporterEmail).toBe(reporter.email);

		await deleteReporterAccount(reporter);
		await clearAuthenticatedReporterEmails(repository);

		const stored = await repository.listAllReportsPaginated(100);
		expect(stored.map((report) => report.reportId).sort()).toEqual([filedId, olderRow, notice].sort());
		for (const report of stored) {
			if (report.reportId === notice) continue;
			expect(report.reporterId?.toString()).toBe(reporter.userId);
			expect(report.reporterEmail).toBeNull();
			expect(serializeReport(report)).not.toContain(reporter.email);
		}
		for (const reportId of [filedId, olderRow]) {
			const detail = await adminReportText(reportId);
			expect(detail.reporterEmail).toBeNull();
			expect(detail.text).not.toContain(reporter.email);
		}
	});

	test('a notice filed without an account keeps the verified address', async () => {
		const reporter = await createTestAccount(harness);
		const notice = await seedRow(null, NOTIFIER_EMAIL);
		await seedRow(BigInt(reporter.userId), reporter.email);

		await deleteReporterAccount(reporter);
		await clearAuthenticatedReporterEmails(repository);

		expect((await repository.getReport(notice))?.reporterEmail).toBe(NOTIFIER_EMAIL);
		expect((await adminReportText(notice)).reporterEmail).toBe(NOTIFIER_EMAIL);
	});
});
