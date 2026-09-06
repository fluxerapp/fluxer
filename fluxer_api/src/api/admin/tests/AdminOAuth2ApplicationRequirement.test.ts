// SPDX-License-Identifier: AGPL-3.0-or-later

import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {beforeEach, describe, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAdminApiKey} from './AdminTestUtils';

interface OAuth2TokenResponse {
	token: string;
	user_id: string;
	scopes: Array<string>;
	application_id: string;
}

async function createOAuth2Token(
	harness: ApiTestHarness,
	userId: string,
	scopes: Array<string>,
	applicationId?: string,
): Promise<OAuth2TokenResponse> {
	return createBuilder<OAuth2TokenResponse>(harness, '')
		.post('/test/oauth2/access-token')
		.body({
			user_id: userId,
			scopes,
			...(applicationId ? {application_id: applicationId} : {}),
		})
		.execute();
}

async function createAdminOAuth2Token(
	harness: ApiTestHarness,
	userId: string,
	scopes: Array<string> = ['identify', 'email'],
): Promise<OAuth2TokenResponse> {
	return createOAuth2Token(harness, userId, scopes, ADMIN_OAUTH2_APPLICATION_ID.toString());
}

describe('Admin OAuth2 Application Requirement', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	describe('third-party OAuth2 bearer tokens', () => {
		test('cannot access admin endpoints even if the user has admin ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const oauth2Token = await createOAuth2Token(harness, admin.userId, ['identify', 'email']);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
		});

		test('cannot access admin endpoints with broad OAuth scopes', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
			const oauth2Token = await createOAuth2Token(harness, admin.userId, [
				'identify',
				'email',
				'guilds',
				'connections',
			]);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/admin/guilds/123')
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
		});

		test('cannot access multiple admin endpoints with wildcard user ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['*']);
			const oauth2Token = await createOAuth2Token(harness, admin.userId, ['identify', 'email']);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/admin/guilds/123')
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/admin/audit-logs?limit=10')
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
		});
	});

	describe('built-in admin OAuth2 bearer tokens', () => {
		test('can access admin endpoints when the user has the required ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const oauth2Token = await createAdminOAuth2Token(harness, admin.userId);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		});

		test('can access multiple admin endpoints without an admin OAuth scope', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'guild:lookup', 'audit_log:view']);
			const oauth2Token = await createAdminOAuth2Token(harness, admin.userId);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/admin/guilds/123')
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get('/admin/audit-logs?limit=10')
				.expect(HTTP_STATUS.OK)
				.execute();
		});

		test('still requires the requested admin ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			const oauth2Token = await createAdminOAuth2Token(harness, admin.userId);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
		});

		test('still requires admin:authenticate on the user', async () => {
			const user = await createTestAccount(harness);
			const oauth2Token = await createAdminOAuth2Token(harness, user.userId);

			await createBuilder(harness, `Bearer ${oauth2Token.token}`)
				.get(`/admin/users/${user.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
	});

	describe('non-OAuth admin authentication', () => {
		test('session token can access admin endpoints with proper ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);

			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		});

		test('session token without admin:authenticate ACL cannot access admin endpoints', async () => {
			const user = await createTestAccount(harness);

			await createBuilder(harness, `${user.token}`)
				.get(`/admin/users/${user.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});

		test('admin API key can access endpoints with granted ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
			const apiKey = await createAdminApiKey(harness, admin, 'Test Key', ['user:lookup'], null);

			await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
		});

		test('admin API key without required ACL cannot access endpoint', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
			const apiKey = await createAdminApiKey(harness, admin, 'Limited Key', ['audit_log:view'], null);

			await createBuilder(harness, apiKey.token)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
				.execute();
		});
	});
});
