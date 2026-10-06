// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminApiKey} from '@app/api/admin/tests/AdminTestUtils';
import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {expect} from 'vitest';

export const AdminApiKeyAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/api-keys',
		async prepare() {
			return {
				request: {
					path: '/admin/api-keys',
					body: {name: 'Coverage key', acls: ['user:lookup', 'guild:lookup'], expires_in_days: 30},
				},
				expected: {
					action: 'create_admin_api_key',
					targetType: 'admin_api_key',
					targetId: expect.any(String),
					metadata: {acls: 'user:lookup,guild:lookup', expires_in_days: '30'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/api-keys',
		async prepare({harness, admin}) {
			await createAdminApiKey(harness, admin, 'First key', ['user:lookup'], null);
			await createAdminApiKey(harness, admin, 'Second key', ['guild:lookup'], 7);
			return {
				request: {path: '/admin/api-keys'},
				expected: {
					action: 'list_admin_api_keys',
					targetType: 'admin_api_key',
					targetId: '0',
					metadata: {result_count: '2'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/api-keys/:key_id',
		async prepare({harness, admin}) {
			const key = await createAdminApiKey(harness, admin, 'Readable key', ['user:lookup'], null);
			return {
				request: {path: `/admin/api-keys/${key.keyId}`},
				expected: {
					action: 'get_admin_api_key',
					targetType: 'admin_api_key',
					targetId: key.keyId,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/api-keys/:key_id',
		name: 'name and acls',
		async prepare({harness, admin}) {
			const key = await createAdminApiKey(harness, admin, 'Old name', ['user:lookup'], null);
			return {
				request: {path: `/admin/api-keys/${key.keyId}`, body: {name: 'New name', acls: ['guild:lookup']}},
				expected: {
					action: 'update_admin_api_key',
					targetType: 'admin_api_key',
					targetId: key.keyId,
					metadata: {fields: 'name,acls', acls: 'guild:lookup'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/api-keys/:key_id',
		name: 'name only',
		async prepare({harness, admin}) {
			const key = await createAdminApiKey(harness, admin, 'Old name', ['user:lookup'], 30);
			return {
				request: {path: `/admin/api-keys/${key.keyId}`, body: {name: 'New name'}},
				expected: {
					action: 'update_admin_api_key',
					targetType: 'admin_api_key',
					targetId: key.keyId,
					metadata: {fields: 'name'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/api-keys/:key_id',
		async prepare({harness, admin}) {
			const key = await createAdminApiKey(harness, admin, 'Revoked key', ['user:lookup'], null);
			return {
				request: {path: `/admin/api-keys/${key.keyId}`},
				expected: {
					action: 'revoke_admin_api_key',
					targetType: 'admin_api_key',
					targetId: key.keyId,
					metadata: {},
				},
			};
		},
	},
];
