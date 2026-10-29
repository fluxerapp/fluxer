// SPDX-License-Identifier: AGPL-3.0-or-later

import {legacySsoDiscoveryCacheKey, ssoDiscoveryCacheKey} from '@app/api/auth/services/SsoService';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const OLD_ISSUER = 'https://old-sso.example.com';
const NEW_ISSUER = 'https://new-sso.example.com';

describe('SSO provider discovery cache', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const saveSso = (sso: Record<string, unknown>) =>
		createBuilder(harness, admin.token).patch('/admin/instance/config').body({sso}).execute();

	const cachedFor = (issuer: string) => getCacheService().get(ssoDiscoveryCacheKey(issuer));

	it('forgets the discovered endpoints of the old and new issuer when the SSO settings are saved', async () => {
		await saveSso({enabled: false, issuer: OLD_ISSUER});
		const stale = {issuer: OLD_ISSUER, authorization_endpoint: 'https://old-sso.example.com/authorize'};
		await getCacheService().set(ssoDiscoveryCacheKey(OLD_ISSUER), stale, 3600);
		await getCacheService().set(ssoDiscoveryCacheKey(NEW_ISSUER), {...stale, issuer: NEW_ISSUER}, 3600);

		await saveSso({enabled: false, issuer: NEW_ISSUER});

		expect(await cachedFor(OLD_ISSUER)).toBeNull();
		expect(await cachedFor(NEW_ISSUER)).toBeNull();
	});

	it('forgets the entries older servers wrote for the issuer when the SSO settings are saved', async () => {
		await saveSso({enabled: false, issuer: OLD_ISSUER});
		await getCacheService().set(legacySsoDiscoveryCacheKey(OLD_ISSUER), {issuer: OLD_ISSUER}, 3600);

		await saveSso({client_id: 'another-client'});

		expect(await getCacheService().get(legacySsoDiscoveryCacheKey(OLD_ISSUER))).toBeNull();
	});

	it('forgets the discovered endpoints when other SSO settings change', async () => {
		await saveSso({enabled: false, issuer: OLD_ISSUER});
		await getCacheService().set(ssoDiscoveryCacheKey(OLD_ISSUER), {issuer: OLD_ISSUER}, 3600);

		await saveSso({client_id: 'another-client'});

		expect(await cachedFor(OLD_ISSUER)).toBeNull();
	});
});
