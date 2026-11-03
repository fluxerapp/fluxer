// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {ssoDiscoveryCacheKey} from '@app/api/auth/services/SsoService';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const ISSUER = 'https://1.1.1.1/realms/outage';
const DISCOVERY_URL = `${ISSUER}/.well-known/openid-configuration`;

const DISCOVERY_DOCUMENT = {
	issuer: ISSUER,
	authorization_endpoint: `${ISSUER}/auth`,
	token_endpoint: `${ISSUER}/token`,
	userinfo_endpoint: `${ISSUER}/userinfo`,
	jwks_uri: `${ISSUER}/certs`,
};

interface SsoStatusResponse {
	enabled: boolean;
	enforced: boolean;
}

interface ConfigSaveResponse {
	warnings: Array<string>;
	errors: Array<{path: string; code: string}>;
}

describe('SSO while the provider is unreachable', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let discoveryRequests = 0;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		discoveryRequests = 0;
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'instance:config:update',
			'instance:config:view',
		]);
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function providerUp(): void {
		server.use(
			http.get(DISCOVERY_URL, () => {
				discoveryRequests++;
				return HttpResponse.json(DISCOVERY_DOCUMENT);
			}),
		);
	}

	function providerDown(): void {
		server.use(
			http.get(DISCOVERY_URL, () => {
				discoveryRequests++;
				return HttpResponse.error();
			}),
		);
	}

	function saveIssuerSso(enforced: boolean) {
		return createBuilder<ConfigSaveResponse>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({
				sso: {
					enabled: true,
					enforced,
					issuer: ISSUER,
					authorization_url: null,
					token_url: null,
					userinfo_url: null,
					jwks_url: null,
					client_id: 'outage-client',
					allowed_domains: [],
					auto_provision: true,
					migrate_identities: false,
				},
			});
	}

	async function requireSsoThenLoseProvider(): Promise<void> {
		providerUp();
		await saveIssuerSso(true).expect(200).execute();
		providerDown();
		discoveryRequests = 0;
	}

	async function getStatus(): Promise<SsoStatusResponse> {
		return createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
	}

	it('still refuses password sign-in', async () => {
		const local = await createTestAccount(harness);
		await requireSsoThenLoseProvider();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: local.email, password: local.password})
			.expect(403, 'SSO_REQUIRED')
			.execute();
	});

	it('still refuses an MFA ticket', async () => {
		await requireSsoThenLoseProvider();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login/mfa/totp')
			.body({code: '123456', ticket: 'outage-ticket'})
			.expect(403, 'SSO_REQUIRED')
			.execute();
	});

	it('still treats the credentials of SSO accounts as managed by the provider', async () => {
		await enableSso(harness, admin.token);
		const start = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		const login = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/sso/complete')
			.body({code: createUniqueEmail('sso-outage'), state: start.state, client_binding: start.client_binding})
			.execute();
		await createBuilder(harness, login.token)
			.put('/users/@me/date-of-birth')
			.body({date_of_birth: '1990-01-01'})
			.execute();
		await requireSsoThenLoseProvider();
		await createBuilder(harness, login.token)
			.post('/users/@me/password-change/start')
			.body({})
			.expect(403, 'SSO_REQUIRED')
			.execute();
	});

	it('reports SSO as required but not available', async () => {
		await requireSsoThenLoseProvider();
		expect(await getStatus()).toMatchObject({enabled: false, enforced: true});
	});

	it('reports the provider as unavailable when a sign-in starts during the outage', async () => {
		await requireSsoThenLoseProvider();
		await createBuilderWithoutAuth(harness)
			.post('/auth/sso/start')
			.body({})
			.expect(503, 'SSO_PROVIDER_UNAVAILABLE')
			.execute();
	});

	it('still reports SSO that is turned off as disabled', async () => {
		await disableSso(harness, admin.token);
		await createBuilderWithoutAuth(harness)
			.post('/auth/sso/start')
			.body({})
			.expect(403, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});

	it('asks the provider only once for two status checks in a row', async () => {
		await requireSsoThenLoseProvider();
		await getStatus();
		await getStatus();
		expect(discoveryRequests).toBe(1);
	});

	it('keeps using the last discovered endpoints when a refresh fails', async () => {
		await requireSsoThenLoseProvider();
		const twoHoursAgo = Date.now() - 2 * 60 * 60 * 1000;
		await getCacheService().set(
			ssoDiscoveryCacheKey(ISSUER),
			{metadata: DISCOVERY_DOCUMENT, fetchedAt: twoHoursAgo, failed: false},
			3600,
		);
		expect(await getStatus()).toMatchObject({enabled: true, enforced: true});
		expect(discoveryRequests).toBe(1);
	});

	it('leaves discovery entries from older servers alone and does not read them', async () => {
		providerUp();
		await saveIssuerSso(true).expect(200).execute();
		const olderServerKey = `sso:oidc-discovery:${createHash('sha256').update(ISSUER).digest('hex').slice(0, 32)}`;
		const olderServerEntry = {issuer: ISSUER};
		await getCacheService().set(olderServerKey, olderServerEntry, 3600);
		expect(await getStatus()).toMatchObject({enabled: true, enforced: true});
		expect(await getCacheService().get(olderServerKey)).toEqual(olderServerEntry);
	});

	it('refuses to turn on Require SSO while the provider is unreachable', async () => {
		providerDown();
		const refused = await saveIssuerSso(true).expect(400).execute();
		expect(refused.errors).toEqual([expect.objectContaining({path: 'sso', code: 'SSO_DISCOVERY_FAILED'})]);
	});

	it('saves optional SSO with a warning while the provider is unreachable', async () => {
		providerDown();
		const saved = await saveIssuerSso(false).expect(200).execute();
		expect(saved.warnings).toHaveLength(1);
	});

	it('saves without a warning when the provider answers', async () => {
		providerUp();
		const saved = await saveIssuerSso(false).expect(200).execute();
		expect(saved.warnings).toEqual([]);
	});
});
