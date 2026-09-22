// SPDX-License-Identifier: AGPL-3.0-or-later

import {getAdminAuditAccess} from '@app/api/admin/AdminAuditActions';
import type {AdminAuditLog} from '@app/api/admin/IAdminRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterEach, describe, expect, test} from 'vitest';

const ADMIN_AUDIT_COVERAGE_REASON = 'Audit coverage ticket 5120';

type AdminRouteMethod = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';

export interface AdminAuditCoverageContext {
	harness: ApiTestHarness;
	admin: TestAccount;
}

interface AdminAuditCoverageRequest {
	path: string;
	body?: unknown;
	headers?: Record<string, string>;
	expectStatus?: number;
}

interface AdminAuditExpectedEntry {
	action: string;
	targetType: string;
	targetId: unknown;
	metadata: Record<string, unknown>;
}

interface AdminAuditCoverageCaseBase {
	method: AdminRouteMethod;
	route: string;
	name?: string;
	search?: 'disabled' | 'enabled';
	auditLogReason?: unknown;
}

export interface AdminAuditAuditedCase extends AdminAuditCoverageCaseBase {
	prepare(
		context: AdminAuditCoverageContext,
	): Promise<{request: AdminAuditCoverageRequest; expected: AdminAuditExpectedEntry}>;
}

export interface AdminAuditExemptCase extends AdminAuditCoverageCaseBase {
	exempt: true;
	prepare(context: AdminAuditCoverageContext): Promise<{request: AdminAuditCoverageRequest}>;
}

export type AdminAuditCoverageCase = AdminAuditAuditedCase | AdminAuditExemptCase;

export function adminAuditCaseShape(coverageCase: AdminAuditCoverageCase): string {
	return `${coverageCase.method} ${coverageCase.route}`;
}

function isExemptCase(coverageCase: AdminAuditCoverageCase): coverageCase is AdminAuditExemptCase {
	return 'exempt' in coverageCase && coverageCase.exempt;
}

async function listAuditLogs(): Promise<Array<AdminAuditLog>> {
	return getAdminRepository().listAllAuditLogsPaginated(100000);
}

function describeEntry(log: AdminAuditLog) {
	return {
		action: log.action,
		targetType: log.targetType,
		targetId: log.targetId.toString(),
		metadata: Object.fromEntries(log.metadata),
	};
}

export function describeAdminAuditCoverage(area: string, cases: ReadonlyArray<AdminAuditCoverageCase>): void {
	describe(`Admin audit coverage: ${area}`, () => {
		let harness: ApiTestHarness | undefined;

		afterEach(async () => {
			await harness?.shutdown();
			harness = undefined;
		});

		for (const coverageCase of cases) {
			const label = coverageCase.name
				? `${adminAuditCaseShape(coverageCase)} (${coverageCase.name})`
				: adminAuditCaseShape(coverageCase);
			test(label, async () => {
				harness = await createApiTestHarness({search: coverageCase.search ?? 'disabled'});
				const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
				const prepared = await coverageCase.prepare({harness, admin});
				const {request} = prepared;
				const before = new Set((await listAuditLogs()).map((log) => log.logId.toString()));
				const response = await harness.requestJson({
					path: request.path,
					method: coverageCase.method,
					body: request.body,
					headers: {
						Authorization: admin.token,
						'X-Audit-Log-Reason': ADMIN_AUDIT_COVERAGE_REASON,
						...request.headers,
					},
				});
				const expectedStatus = request.expectStatus ?? 200;
				if (response.status !== expectedStatus) {
					throw new Error(`Expected ${expectedStatus}, got ${response.status}: ${await response.text()}`);
				}
				await response.arrayBuffer();
				const recorded = (await listAuditLogs()).filter((log) => !before.has(log.logId.toString()));

				if (isExemptCase(coverageCase)) {
					expect(recorded.map(describeEntry)).toEqual([]);
					return;
				}

				const {expected} = prepared as Awaited<ReturnType<AdminAuditAuditedCase['prepare']>>;
				const expectedAccess = coverageCase.method === 'GET' ? 'read' : 'write';
				expect(getAdminAuditAccess(expected.action)).toBe(expectedAccess);
				for (const log of recorded) {
					expect(log.adminUserId.toString()).toBe(admin.userId);
					expect(log.auditLogReason).toEqual(coverageCase.auditLogReason ?? ADMIN_AUDIT_COVERAGE_REASON);
					expect(getAdminAuditAccess(log.action)).toBe(expectedAccess);
				}
				if (coverageCase.method === 'GET') {
					expect(recorded.map(describeEntry)).toEqual([expected]);
				} else {
					expect(recorded.map(describeEntry)).toContainEqual(expected);
				}
			});
		}
	});
}
