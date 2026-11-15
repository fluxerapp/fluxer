// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditLog} from '@app/api/admin/AdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID, type ReportID} from '@app/api/BrandedTypes';
import {IAR_SUBMISSION_COLUMNS, type IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {ReportNoteListResponse, ReportNoteResponse} from '@fluxer/schema/src/domains/admin/AdminReportNoteSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ValidationErrorResponse {
	code: string;
	errors?: Array<{path: string; message: string}>;
}

const AUDIT_REASON = 'Ticket 314';
let sequence = 1_496_000_000_000_000_000n;

describe('report notes', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let repository: ReportRepository;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			AdminACLs.REPORT_VIEW,
			AdminACLs.REPORT_RESOLVE,
		]);
		repository = new ReportRepository();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	async function seedReport(): Promise<ReportID> {
		sequence += 1n;
		await repository.createReport({
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: sequence,
			reported_at: new Date(),
			status: 0,
			report_type: 1,
			category: 'harassment',
		} as IARSubmissionRow);
		return createReportID(sequence);
	}

	function addNote(account: TestAccount, reportId: ReportID | string, body: unknown) {
		return createBuilder<ReportNoteResponse & ValidationErrorResponse>(harness, account.token)
			.post(`/admin/reports/${reportId}/notes`)
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.body({body});
	}

	function listNotes(account: TestAccount, reportId: ReportID | string) {
		return createBuilder<ReportNoteListResponse>(harness, account.token).get(`/admin/reports/${reportId}/notes`);
	}

	async function auditEntries(action: string): Promise<Array<AdminAuditLog>> {
		const logs = await getAdminRepository().listAllAuditLogsPaginated(10000);
		return logs.filter((log) => log.action === action);
	}

	test('records the note in the audit log without its body', async () => {
		const reportId = await seedReport();

		const note = await addNote(admin, reportId, 'Private staff context').expect(HTTP_STATUS.OK).execute();

		const [entry, ...rest] = await auditEntries('annotate_report');
		expect(rest).toEqual([]);
		expect(entry?.adminUserId.toString()).toBe(admin.userId);
		expect(entry?.targetType).toBe('report');
		expect(entry?.targetId.toString()).toBe(reportId.toString());
		expect(entry?.auditLogReason).toBe(AUDIT_REASON);
		expect(Object.fromEntries(entry?.metadata ?? [])).toEqual({
			report_id: reportId.toString(),
			note_id: note.note_id,
		});
	});

	test('adding a note needs REPORT_RESOLVE', async () => {
		const reportId = await seedReport();
		const viewer = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			AdminACLs.REPORT_VIEW,
		]);

		await addNote(viewer, reportId, 'Note').expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL').execute();

		expect(await repository.listNotes(reportId)).toEqual([]);
		expect(await auditEntries('annotate_report')).toEqual([]);
	});

	test('listing notes needs REPORT_VIEW', async () => {
		const reportId = await seedReport();
		await addNote(admin, reportId, 'Note').expect(HTTP_STATUS.OK).execute();
		const resolver = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			AdminACLs.REPORT_RESOLVE,
		]);

		await listNotes(resolver, reportId).expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL').execute();
	});
});
