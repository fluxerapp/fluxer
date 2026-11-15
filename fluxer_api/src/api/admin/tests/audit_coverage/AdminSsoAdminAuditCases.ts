// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ADMIN_CLIENT_BASIC_AUTH,
	ADMIN_SSO_ISSUER,
	ADMIN_SSO_NONCE,
	idTokenClaimsJson,
	useAdminSsoProvider,
} from '@app/api/admin/tests/AdminSsoTestUtils';
import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';

export const AdminSsoAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/sso/sign-in',
		async prepare({admin}) {
			const provider = await useAdminSsoProvider();
			const idToken = await provider.signRawClaims(idTokenClaimsJson(admin.userId));
			return {
				request: {
					path: '/admin/sso/sign-in',
					headers: {Authorization: ADMIN_CLIENT_BASIC_AUTH},
					body: {id_token: idToken, nonce: ADMIN_SSO_NONCE},
				},
				expected: {
					action: 'sso_sign_in',
					targetType: 'user',
					targetId: admin.userId,
					metadata: {provider_id: ADMIN_SSO_ISSUER},
				},
			};
		},
	},
];
