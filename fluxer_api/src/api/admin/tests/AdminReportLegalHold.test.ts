// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID, type ReportID} from '@app/api/BrandedTypes';
import {IAR_SUBMISSION_COLUMNS, type IARSubmissionRow} from '@app/api/database/types/ReportTypes';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {processReportRetention} from '@app/api/worker/tasks/ExpireReportSnapshots';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {ReportLegalHoldResponse} from '@fluxer/schema/src/domains/admin/AdminReportLegalHoldSchemas';
import {ms} from 'itty-time';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ValidationErrorResponse {
	code: string;
	errors?: Array<{path: string; message: string}>;
}

const HOLD_UNTIL = '2099-01-01T00:00:00.000Z';
let sequence = 1_497_000_000_000_000_000n;

describe('report legal hold', () => {
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

	async function seedReport(ageDays: number): Promise<ReportID> {
		sequence += 1n;
		await repository.createReport({
			...Object.fromEntries(IAR_SUBMISSION_COLUMNS.map((column) => [column, null])),
			report_id: sequence,
			reported_at: new Date(Date.now() - ageDays * ms('1 day')),
			status: 1,
			report_type: 1,
			category: 'harassment',
		} as IARSubmissionRow);
		return createReportID(sequence);
	}

	function holdRequest(account: TestAccount, reportId: ReportID | string, body: unknown) {
		return createBuilder<ReportLegalHoldResponse & ValidationErrorResponse>(harness, account.token)
			.post(`/admin/reports/${reportId}/legal-hold`)
			.body(body);
	}

	function reportDetail(reportId: ReportID) {
		return createBuilder<{legal_hold_until?: string | null; legal_hold_reason?: string | null}>(harness, admin.token)
			.get(`/admin/reports/${reportId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function runRetention() {
		return processReportRetention(
			{reportRepository: repository, storageService: new MockStorageService(), reportSearchService: null},
			{now: new Date(), dryRun: false, policy: {retentionDays: 365, resolvedRetentionDays: null}},
		);
	}

	test('a hold keeps an expired report until it is cleared', async () => {
		const reportId = await seedReport(400);

		const placed = await holdRequest(admin, reportId, {
			legal_hold_until: HOLD_UNTIL,
			legal_hold_reason: '  Preservation request 42  ',
		})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(placed).toEqual({
			report_id: reportId.toString(),
			legal_hold_until: HOLD_UNTIL,
			legal_hold_reason: 'Preservation request 42',
		});
		const stored = await repository.getReport(reportId);
		expect(stored?.legalHoldUntil?.toISOString()).toBe(HOLD_UNTIL);
		expect(stored?.legalHoldReason).toBe('Preservation request 42');
		expect(await reportDetail(reportId)).toMatchObject({
			legal_hold_until: HOLD_UNTIL,
			legal_hold_reason: 'Preservation request 42',
		});
		expect(await runRetention()).toMatchObject({held: 1, deleted: 0});
		expect(await repository.getReport(reportId)).not.toBeNull();

		const cleared = await holdRequest(admin, reportId, {legal_hold_until: null, legal_hold_reason: 'ignored'})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(cleared).toEqual({report_id: reportId.toString(), legal_hold_until: null, legal_hold_reason: null});
		expect(await reportDetail(reportId)).toMatchObject({legal_hold_until: null, legal_hold_reason: null});
		expect(await runRetention()).toMatchObject({held: 0, deleted: 1});
		expect(await repository.getReport(reportId)).toBeNull();
	});

	test('a hold that ends in the past is refused', async () => {
		const reportId = await seedReport(10);

		const response = await holdRequest(admin, reportId, {
			legal_hold_until: new Date(Date.now() - ms('1 minute')).toISOString(),
			legal_hold_reason: 'Preservation request',
		})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();

		expect(response.errors?.[0]?.path).toBe('legal_hold_until');
		expect((await repository.getReport(reportId))?.legalHoldUntil).toBeNull();
	});

	test.each([[{legal_hold_until: HOLD_UNTIL}], [{legal_hold_until: HOLD_UNTIL, legal_hold_reason: null}]])(
		'a hold without a reason is refused (%j)',
		async (body) => {
			const reportId = await seedReport(10);

			const response = await holdRequest(admin, reportId, body)
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();

			expect(response.errors?.[0]?.path).toBe('legal_hold_reason');
			expect((await repository.getReport(reportId))?.legalHoldUntil).toBeNull();
		},
	);

	test('an end time that is not an ISO 8601 timestamp is refused', async () => {
		const reportId = await seedReport(10);

		const response = await holdRequest(admin, reportId, {legal_hold_until: 'next year', legal_hold_reason: 'Request'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();

		expect(response.errors?.[0]?.path).toBe('legal_hold_until');
	});

	test('an unknown report gives UNKNOWN_REPORT and stores nothing', async () => {
		await holdRequest(admin, '1499999999999999999', {legal_hold_until: HOLD_UNTIL, legal_hold_reason: 'Request'})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_REPORT')
			.execute();

		expect(await repository.getReport(createReportID(1499999999999999999n))).toBeNull();
	});

	test('placing a hold needs REPORT_RESOLVE', async () => {
		const reportId = await seedReport(10);
		const viewer = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			AdminACLs.REPORT_VIEW,
		]);

		await holdRequest(viewer, reportId, {legal_hold_until: HOLD_UNTIL, legal_hold_reason: 'Request'})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();

		expect((await repository.getReport(reportId))?.legalHoldUntil).toBeNull();
	});
});
