// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {CustomBadgeType} from '@fluxer/constants/src/BadgeConstants';
import type {AdminCustomBadgeResponse} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';

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

export async function createAdminWithAcls(harness: ApiTestHarness, acls: Array<string>): Promise<TestAccount> {
	const account = await createTestAccount(harness);
	return await setUserACLs(harness, account, [AdminACLs.AUTHENTICATE, ...acls]);
}

export async function createTestCustomBadge(
	harness: ApiTestHarness,
	token: string,
	type: CustomBadgeType = 'user',
	name = 'Coverage badge',
): Promise<AdminCustomBadgeResponse> {
	return createBuilder<AdminCustomBadgeResponse>(harness, token)
		.post('/admin/badges')
		.body({type, name, image: getPngDataUrl()})
		.expect(200)
		.execute();
}
