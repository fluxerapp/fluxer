// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createUserID} from '@app/api/BrandedTypes';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';

const SEEDED_LOG_ID = 800000000000000001n;

async function seedWriteEntry(adminUserId: string): Promise<void> {
	await getAdminRepository().createAuditLog({
		log_id: SEEDED_LOG_ID,
		admin_user_id: createUserID(BigInt(adminUserId)),
		target_type: 'user',
		target_id: BigInt(adminUserId),
		action: 'update_flags',
		audit_log_reason: null,
		metadata: new Map(),
		created_at: new Date(),
	});
}

export const AuditLogAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/audit-logs',
		name: 'list',
		async prepare({admin}) {
			await seedWriteEntry(admin.userId);
			return {
				request: {path: `/admin/audit-logs?access=write&target_id=${admin.userId}&limit=10`},
				expected: {
					action: 'list_audit_logs',
					targetType: 'audit_log',
					targetId: '0',
					metadata: {
						filter_target_id: admin.userId,
						access: 'write',
						limit: '10',
						offset: '0',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/audit-logs',
		name: 'search',
		search: 'enabled',
		async prepare({admin}) {
			return {
				request: {
					path: `/admin/audit-logs?q=flags&admin_user_id=${admin.userId}&target_type=user&access=read&sort_by=relevance`,
				},
				expected: {
					action: 'search_audit_logs',
					targetType: 'audit_log',
					targetId: '0',
					metadata: {
						filter_admin_user_id: admin.userId,
						filter_target_type: 'user',
						access: 'read',
						sort_by: 'relevance',
						sort_order: 'desc',
						limit: '50',
						offset: '0',
						result_count: '0',
						total: '0',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/audit-logs',
		name: 'free-text target type',
		async prepare() {
			return {
				request: {path: `/admin/audit-logs?target_type=${encodeURIComponent('someone@example.com')}`},
				expected: {
					action: 'list_audit_logs',
					targetType: 'audit_log',
					targetId: '0',
					metadata: {
						has_target_type_filter: 'true',
						limit: '50',
						offset: '0',
						result_count: '0',
						total: '0',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/audit-logs/:log_id',
		async prepare({admin}) {
			await seedWriteEntry(admin.userId);
			return {
				request: {path: `/admin/audit-logs/${SEEDED_LOG_ID}`},
				expected: {
					action: 'get_audit_log',
					targetType: 'audit_log',
					targetId: SEEDED_LOG_ID.toString(),
					metadata: {},
				},
			};
		},
	},
];
