// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';

export const SsoIdentityAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/users/:user_id/sso-identities',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/sso-identities`},
				expected: {
					action: 'list_user_sso_identities',
					targetType: 'user',
					targetId: target.userId,
					metadata: {identity_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/sso-identities',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await new SsoIdentityRepository().tryClaimIdentity({
				providerId: 'https://idp.example.com',
				subject: 'audit-subject',
				userId: createUserID(BigInt(target.userId)),
				claimedAt: new Date(),
			});
			const query = new URLSearchParams({provider_id: 'https://idp.example.com', subject: 'audit-subject'});
			return {
				request: {path: `/admin/sso-identities?${query.toString()}`},
				expected: {
					action: 'lookup_sso_identity',
					targetType: 'user',
					targetId: target.userId,
					metadata: {provider_id: 'https://idp.example.com', found: 'true'},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/sso-identities',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/sso-identities`, body: {subject: 'audit-link-subject'}},
				expected: {
					action: 'link_sso_identity',
					targetType: 'user',
					targetId: target.userId,
					metadata: {provider_id: 'sso'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/sso-identities',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await new SsoIdentityRepository().tryClaimIdentity({
				providerId: 'https://idp.example.com',
				subject: 'audit-unlink-subject',
				userId: createUserID(BigInt(target.userId)),
				claimedAt: new Date(),
			});
			return {
				request: {path: `/admin/users/${target.userId}/sso-identities`},
				expected: {
					action: 'unlink_sso_identities',
					targetType: 'user',
					targetId: target.userId,
					metadata: {identity_count: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/sso-identities/migrate',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await new SsoIdentityRepository().tryClaimIdentity({
				providerId: 'https://old-idp.example.com',
				subject: 'audit-migrate-subject',
				userId: createUserID(BigInt(target.userId)),
				claimedAt: new Date(),
			});
			return {
				request: {path: '/admin/sso-identities/migrate', body: {from_provider_id: 'https://old-idp.example.com'}},
				expected: {
					action: 'migrate_sso_identities',
					targetType: 'instance_config',
					targetId: '0',
					metadata: {
						from_provider_id: 'https://old-idp.example.com',
						to_provider_id: 'sso',
						moved_count: '1',
						skipped_count: '0',
					},
				},
			};
		},
	},
];
