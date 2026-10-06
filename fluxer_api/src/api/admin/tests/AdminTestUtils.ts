// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';

interface AdminApiKey {
	keyId: string;
	key: string;
	name: string;
	acls: Array<string>;
	token: string;
}

export async function createAdminApiKey(
	harness: ApiTestHarness,
	account: TestAccount,
	name: string,
	acls: Array<string>,
	expiresInDays: number | null,
): Promise<AdminApiKey> {
	const data = await createBuilder<{
		key_id: string;
		key: string;
		name: string;
		acls: Array<string>;
	}>(harness, `${account.token}`)
		.post('/admin/api-keys')
		.body({
			name,
			acls,
			...(expiresInDays !== null ? {expires_in_days: expiresInDays} : {}),
		})
		.execute();
	return {
		keyId: data.key_id,
		key: data.key,
		name: data.name,
		acls: data.acls,
		token: `Admin ${data.key}`,
	};
}

export async function createAdminApiKeyWithDefaultACLs(
	harness: ApiTestHarness,
	account: TestAccount,
	name: string,
): Promise<AdminApiKey> {
	return await createAdminApiKey(harness, account, name, ['audit_log:view', 'user:lookup', 'guild:lookup'], null);
}

export async function listAdminApiKeys(
	harness: ApiTestHarness,
	token: string,
): Promise<Array<Record<string, unknown>>> {
	return createBuilder<Array<Record<string, unknown>>>(harness, `${token}`).get('/admin/api-keys').execute();
}

export async function revokeAdminApiKey(harness: ApiTestHarness, token: string, keyId: string): Promise<void> {
	await createBuilder<void>(harness, `${token}`).delete(`/admin/api-keys/${keyId}`).body(null).expect(200).execute();
}
