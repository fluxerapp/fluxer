// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from './AuthTestUtils';

interface SsoStartResponse {
	authorization_url: string;
	state: string;
	redirect_uri: string;
}

interface SsoCompleteResponse {
	token: string;
	user_id: string;
	redirect_to: string;
}

interface SsoStatusResponse {
	enabled: boolean;
	enforced: boolean;
	display_name?: string;
	redirect_uri: string;
}

function getAuthorizationUrlParam(authorizationUrlString: string, param: string): string | null {
	const queryStart = authorizationUrlString.indexOf('?');
	if (queryStart === -1) return null;
	return new URLSearchParams(authorizationUrlString.slice(queryStart + 1)).get(param);
}

describe('Auth SSO flow', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	describe('local auth blocking', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
			await enableSso(harness, admin.token);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('blocks local auth when SSO is enforced', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/auth/login')
				.body({
					email: 'someone@example.com',
					password: 'password123',
				})
				.expect(403)
				.execute();
		});
		it('allows local auth when SSO is enabled but not enforced', async () => {
			await enableSso(harness, admin.token, {enforced: false});
			const loginData = await createBuilderWithoutAuth<{
				token: string;
				user_id: string;
			}>(harness)
				.post('/auth/login')
				.body({
					email: admin.email,
					password: admin.password,
				})
				.execute();
			expect(loginData.token).toBeTruthy();
			expect(loginData.user_id).toBe(admin.userId);
		});
		it('rejects enforced SSO config that cannot resolve claims', async () => {
			await createBuilder(harness, admin.token)
				.post('/admin/instance-config/update')
				.body({
					sso: {
						enabled: true,
						enforced: true,
						authorization_url: 'https://1.1.1.1/oauth2/authorize',
						token_url: 'https://1.1.1.1/oauth2/token',
						userinfo_url: null,
						jwks_url: null,
						client_id: 'itest-client',
						client_secret: '',
						scope: 'openid email profile',
						allowed_domains: ['example.com'],
						auto_provision: true,
						redirect_uri: '',
					},
				})
				.expect(400)
				.execute();
		});
	});
	describe('complete SSO flow', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
			await enableSso(harness, admin.token);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('creates session through full SSO flow', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/me'})
				.execute();
			expect(startData.state).toBeTruthy();
			expect(startData.authorization_url).toBeTruthy();
			const authUrlString = startData.authorization_url;
			expect(authUrlString).toContain(`state=${startData.state}`);
			expect(authUrlString).toContain('code_challenge_method=S256');
			expect(authUrlString).toContain('code_challenge=');
			expect(authUrlString).toContain('nonce=');
			expect(getAuthorizationUrlParam(authUrlString, 'redirect_uri')).toBe(startData.redirect_uri);
			if (authUrlString.startsWith('http://') || authUrlString.startsWith('https://')) {
				const authUrl = new URL(authUrlString);
				const stateParam = authUrl.searchParams.get('state');
				expect(stateParam).toBe(startData.state);
				const codeChallengeMethod = authUrl.searchParams.get('code_challenge_method');
				expect(codeChallengeMethod).toBe('S256');
				const codeChallenge = authUrl.searchParams.get('code_challenge');
				expect(codeChallenge).toBeTruthy();
				const nonce = authUrl.searchParams.get('nonce');
				expect(nonce).toBeTruthy();
			}
			const email = `sso-user-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.token).toBeTruthy();
			expect(completeData.user_id).toBeTruthy();
			expect(completeData.redirect_to).toBe('/me');
			const meData = await createBuilder<{
				email: string | null;
			}>(harness, completeData.token)
				.get('/users/@me')
				.execute();
			expect(meData.email).toBe(email);
		});
		it('derives the SSO redirect URI instead of trusting admin-supplied overrides', async () => {
			await enableSso(harness, admin.token, {
				redirect_uri: 'https://evil.example/auth/sso/callback',
			});
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			expect(startData.redirect_uri).toContain('/auth/sso/callback');
			expect(startData.redirect_uri).not.toContain('evil.example');
			expect(startData.authorization_url).toContain(encodeURIComponent(startData.redirect_uri));
		});
		it('uses the requested mobile SSO redirect URI without changing the post-login redirect', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({
					redirect_to: '/me',
					redirect_uri: 'fluxer://auth/sso/callback',
				})
				.execute();
			expect(startData.redirect_uri).toBe('fluxer://auth/sso/callback');
			expect(getAuthorizationUrlParam(startData.authorization_url, 'redirect_uri')).toBe('fluxer://auth/sso/callback');
			const email = `sso-mobile-redirect-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.token).toBeTruthy();
			expect(completeData.redirect_to).toBe('/me');
		});
		it('rejects unapproved SSO redirect URIs', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/start')
				.body({redirect_uri: 'https://evil.example/auth/sso/callback'})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
		});
	});
	describe('redirect validation', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
			await enableSso(harness, admin.token);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('rejects open redirect URLs', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: 'https://evil.example/phish'})
				.execute();
			const email = `sso-open-redirect-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.redirect_to).toBe('');
		});
		it('rejects protocol-relative redirects', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '//evil.example/phish'})
				.execute();
			const email = `sso-protocol-relative-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.redirect_to).toBe('');
		});
		it('rejects redirects with newlines', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/dashboard\r\nSet-Cookie: evil=true'})
				.execute();
			const email = `sso-newline-redirect-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.redirect_to).toBe('');
		});
		it('rejects too long redirects', async () => {
			const longRedirect = `/${'a'.repeat(2100)}`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/start')
				.body({redirect_to: longRedirect})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
		});
		it('uses default redirect when missing', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const email = `sso-default-redirect-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.redirect_to.trim()).toBe('');
		});
		it('treats empty redirect as missing', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: ''})
				.execute();
			const email = `sso-empty-redirect-${Date.now()}@example.com`;
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
			expect(completeData.redirect_to.trim()).toBe('');
		});
	});
	describe('state validation', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
			await enableSso(harness, admin.token);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('ensures state is single-use', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/me'})
				.execute();
			const email1 = `sso-singleuse-${Date.now()}@example.com`;
			await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email1,
					state: startData.state,
				})
				.execute();
			const email2 = `sso-singleuse-2-${Date.now()}@example.com`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: email2,
					state: startData.state,
				})
				.expect(400)
				.execute();
		});
		it('rejects invalid state', async () => {
			const email = `sso-invalid-state-${Date.now()}@example.com`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: 'invalid-state-value-12345',
				})
				.expect(400)
				.execute();
		});
		it('rejects missing state', async () => {
			const email = `sso-missing-state-${Date.now()}@example.com`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
				})
				.expect(400)
				.execute();
		});
		it('rejects empty code', async () => {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: '',
					state: startData.state,
				})
				.expect(400)
				.execute();
		});
		it('generates unique states', async () => {
			const states = new Set<string>();
			for (let i = 0; i < 10; i++) {
				const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
					.post('/auth/sso/start')
					.body({})
					.execute();
				expect(states.has(startData.state)).toBe(false);
				states.add(startData.state);
			}
			expect(states.size).toBe(10);
		});
	});
	describe('domain validation', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('enforces allowed domains', async () => {
			await enableSso(harness, admin.token, {
				allowed_domains: ['example.com'],
			});
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/me'})
				.execute();
			const email = `sso-bad-domain-${Date.now()}@notexample.com`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.expect(400)
				.execute();
		});
		it('handles allowed domains case-insensitively', async () => {
			await enableSso(harness, admin.token, {
				allowed_domains: ['EXAMPLE.COM'],
			});
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const email = `sso-case-insensitive-${Date.now()}@example.com`;
			await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
		});
		it('allows any domain when allowed_domains is empty', async () => {
			await enableSso(harness, admin.token, {
				allowed_domains: [],
			});
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const email = `sso-any-domain-${Date.now()}@anydomain.org`;
			await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.execute();
		});
		it('rejects invalid allowed domains during config update', async () => {
			await createBuilder(harness, admin.token)
				.post('/admin/instance-config/update')
				.body({
					sso: {
						enabled: true,
						enforced: true,
						authorization_url: 'test',
						token_url: 'test',
						client_id: 'itest-client',
						client_secret: '',
						scope: 'openid email profile',
						allowed_domains: ['example.com', '*.example.com'],
						auto_provision: true,
					},
				})
				.expect(400)
				.execute();
		});
		it('rejects unsafe provider URLs during config update', async () => {
			await createBuilder(harness, admin.token)
				.post('/admin/instance-config/update')
				.body({
					sso: {
						enabled: true,
						enforced: true,
						authorization_url: 'http://idp.example.com/oauth2/authorize',
						token_url: 'https://idp.example.com/oauth2/token',
						userinfo_url: 'https://idp.example.com/oauth2/userinfo',
						client_id: 'itest-client',
						client_secret: '',
						scope: 'openid email profile',
						allowed_domains: ['example.com'],
						auto_provision: true,
					},
				})
				.expect(400)
				.execute();
		});
		it('normalizes and deduplicates allowed domains during config update', async () => {
			await enableSso(harness, admin.token, {
				allowed_domains: [' EXAMPLE.com ', 'example.com', 'bücher.example'],
			});
			const config = await createBuilder<{
				sso: {
					allowed_domains: Array<string>;
				};
			}>(harness, admin.token)
				.post('/admin/instance-config/get')
				.body({})
				.execute();
			expect(config.sso.allowed_domains).toEqual(['example.com', 'xn--bcher-kva.example']);
		});
	});
	describe('auto-provision', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('respects auto_provision flag', async () => {
			await enableSso(harness, admin.token, {
				auto_provision: false,
			});
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/me'})
				.execute();
			const email = `sso-noprovision-${Date.now()}@example.com`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData.state,
				})
				.expect(403)
				.execute();
		});
	});
	describe('existing user login', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
			await enableSso(harness, admin.token);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		it('logs in existing user via SSO', async () => {
			const email = `sso-existing-user-${Date.now()}@example.com`;
			const startData1 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const firstLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData1.state,
				})
				.execute();
			const startData2 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const secondLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: email,
					state: startData2.state,
				})
				.execute();
			expect(secondLogin.user_id).toBe(firstLogin.user_id);
		});
		it('uses stable subject mapping when provider email changes', async () => {
			const subject = `itest-sso-subject-${Date.now()}`;
			const firstEmail = createUniqueEmail('sso-subject-first');
			const secondEmail = createUniqueEmail('sso-subject-second');
			const startData1 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const firstLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email: firstEmail, sub: subject, email_verified: true}),
					state: startData1.state,
				})
				.execute();
			const startData2 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const secondLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email: secondEmail, sub: subject, email_verified: true}),
					state: startData2.state,
				})
				.execute();
			expect(secondLogin.user_id).toBe(firstLogin.user_id);
		});
		it('rejects a different subject trying to claim an already linked email', async () => {
			const email = createUniqueEmail('sso-subject-conflict');
			const startData1 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const firstLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email, sub: `itest-sso-subject-conflict-a-${Date.now()}`, email_verified: true}),
					state: startData1.state,
				})
				.execute();
			expect(firstLogin.user_id).toBeTruthy();
			const startData2 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email, sub: `itest-sso-subject-conflict-b-${Date.now()}`, email_verified: true}),
					state: startData2.state,
				})
				.expect(400)
				.execute();
		});
		it('does not collapse distinct provider subjects that differ by surrounding whitespace', async () => {
			const subject = `itest-sso-subject-space-${Date.now()}`;
			const firstEmail = createUniqueEmail('sso-subject-space-first');
			const secondEmail = createUniqueEmail('sso-subject-space-second');
			const startData1 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const firstLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email: firstEmail, sub: subject, email_verified: true}),
					state: startData1.state,
				})
				.execute();
			const startData2 = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const secondLogin = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email: secondEmail, sub: ` ${subject}`, email_verified: true}),
					state: startData2.state,
				})
				.execute();
			expect(secondLogin.user_id).not.toBe(firstLogin.user_id);
		});
		it('rejects unverified provider email claims', async () => {
			const email = createUniqueEmail('sso-unverified');
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: JSON.stringify({email, sub: `itest-sso-unverified-${Date.now()}`, email_verified: false}),
					state: startData.state,
				})
				.expect(400)
				.execute();
		});
	});
	describe('status endpoint', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await createTestAccount(harness);
			admin = await setUserACLs(harness, admin, [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
		});
		it('returns SSO status', async () => {
			const status1 = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(status1.enabled).toBe(false);
			expect(status1.enforced).toBe(false);
			await enableSso(harness, admin.token, {
				display_name: 'Test SSO Provider',
			});
			const status2 = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(status2.enabled).toBe(true);
			expect(status2.enforced).toBe(true);
			expect(status2.display_name).toBe('Test SSO Provider');
			await disableSso(harness, admin.token);
		});
		it('reports optional SSO separately from enforced SSO', async () => {
			await enableSso(harness, admin.token, {
				enforced: false,
			});
			const status = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(status.enabled).toBe(true);
			expect(status.enforced).toBe(false);
			await disableSso(harness, admin.token);
		});
		it('does not advertise enabled SSO when optional SSO cannot resolve claims', async () => {
			await createBuilder(harness, admin.token)
				.post('/admin/instance-config/update')
				.body({
					sso: {
						enabled: true,
						enforced: false,
						authorization_url: 'https://1.1.1.1/oauth2/authorize',
						token_url: 'https://1.1.1.1/oauth2/token',
						userinfo_url: null,
						jwks_url: null,
						client_id: 'itest-client',
						client_secret: '',
						scope: 'openid email profile',
						allowed_domains: ['example.com'],
						auto_provision: true,
						redirect_uri: '',
					},
				})
				.execute();
			const status = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(status.enabled).toBe(false);
			expect(status.enforced).toBe(false);
			await disableSso(harness, admin.token);
		});
	});
});
