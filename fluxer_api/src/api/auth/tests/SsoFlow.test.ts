// SPDX-License-Identifier: AGPL-3.0-or-later

import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	createUniqueEmail,
	disableSso,
	enableSso,
	enableTotp,
	setUserACLs,
	type TestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createChannelInvite, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {EmailDeliveryModes} from '@fluxer/constants/src/AccountConfigConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {SSO_MOBILE_CALLBACK_URI} from '@fluxer/constants/src/SsoConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {PendingRegistrationListResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SsoStartResponse {
	authorization_url: string;
	state: string;
	redirect_uri: string;
	client_binding: string;
}

interface SsoCompleteResponse {
	token: string;
	user_id: string;
	redirect_to: string;
}

interface SsoMfaChallengeResponse {
	mfa: true;
	ticket: string;
	backup_codes: boolean;
	redirect_to: string;
}

interface SsoStatusResponse {
	enabled: boolean;
	enforced: boolean;
	display_name?: string;
	button_label: string | null;
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
				.patch('/admin/instance/config')
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
			expect(startData.state.startsWith('m.')).toBe(false);
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
					client_binding: startData.client_binding,
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
		it('routes mobile SSO through the default redirect URI without changing the post-login redirect', async () => {
			const status = await createBuilderWithoutAuth<{redirect_uri: string}>(harness).get('/auth/sso/status').execute();
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({
					redirect_to: '/me',
					redirect_uri: 'fluxer://auth/sso/callback',
				})
				.execute();
			expect(startData.redirect_uri).toBe(status.redirect_uri);
			expect(getAuthorizationUrlParam(startData.authorization_url, 'redirect_uri')).toBe(status.redirect_uri);
			expect(startData.state.startsWith('m.')).toBe(true);
			expect(getAuthorizationUrlParam(startData.authorization_url, 'state')).toBe(startData.state);
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
					client_binding: startData.client_binding,
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
					client_binding: startData.client_binding,
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
					client_binding: startData.client_binding,
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
					client_binding: startData.client_binding,
				})
				.execute();
			const email2 = `sso-singleuse-2-${Date.now()}@example.com`;
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({
					code: email2,
					state: startData.state,
					client_binding: startData.client_binding,
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
					client_binding: startData.client_binding,
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
					client_binding: startData.client_binding,
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
					client_binding: startData.client_binding,
				})
				.execute();
		});
		it('rejects invalid allowed domains during config update', async () => {
			await createBuilder(harness, admin.token)
				.patch('/admin/instance/config')
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
				.patch('/admin/instance/config')
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
				.get('/admin/instance/config')
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
					client_binding: startData.client_binding,
				})
				.expect(403)
				.execute();
		});
		it('joins a provisioned user to the single community', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			await getInstanceConfigRepository().setInstancePolicyConfig({
				single_community_enabled: true,
				single_community_guild_id: guild.id,
			});
			await enableSso(harness, admin.token);
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({
					code: `sso-single-community-${Date.now()}@example.com`,
					state: startData.state,
					client_binding: startData.client_binding,
				})
				.execute();
			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${completeData.user_id}`)
				.expect(200)
				.execute();
		});
		it('joins a provisioned user to the auto-join invite community', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await enableSso(harness, admin.token);
			const originalAutoJoinInviteCode = Config.instance.autoJoinInviteCode;
			Config.instance.autoJoinInviteCode = invite.code;
			try {
				const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
					.post('/auth/sso/start')
					.body({})
					.execute();
				const completeData = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
					.post('/auth/sso/complete')
					.body({
						code: `sso-auto-join-${Date.now()}@example.com`,
						state: startData.state,
						client_binding: startData.client_binding,
					})
					.execute();
				await createBuilder(harness, owner.token)
					.get(`/guilds/${guild.id}/members/${completeData.user_id}`)
					.expect(200)
					.execute();
			} finally {
				Config.instance.autoJoinInviteCode = originalAutoJoinInviteCode;
			}
		});
		it('replays the auto-join invite when an SSO registration is approved', async () => {
			const reviewer = await setUserACLs(harness, await createTestAccount(harness), [
				AdminACLs.AUTHENTICATE,
				AdminACLs.REGISTRATION_REVIEW,
			]);
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await enableSso(harness, admin.token);
			await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
			const originalAutoJoinInviteCode = Config.instance.autoJoinInviteCode;
			Config.instance.autoJoinInviteCode = invite.code;
			try {
				const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
					.post('/auth/sso/start')
					.body({})
					.execute();
				await createBuilderWithoutAuth(harness)
					.post('/auth/sso/complete')
					.body({
						code: `sso-auto-join-approval-${Date.now()}@example.com`,
						state: startData.state,
						client_binding: startData.client_binding,
					})
					.expect(403, 'REGISTRATION_PENDING_APPROVAL')
					.execute();
				const listed = await createBuilder<PendingRegistrationListResponse>(harness, reviewer.token)
					.get('/admin/instance/pending-registrations')
					.expect(200)
					.execute();
				expect(listed.pending_registrations).toEqual([expect.objectContaining({invite_code: invite.code})]);
				const pendingUserId = listed.pending_registrations[0]!.user_id;
				await createBuilder(harness, owner.token)
					.get(`/guilds/${guild.id}/members/${pendingUserId}`)
					.expect(404)
					.execute();
				await createBuilder(harness, reviewer.token)
					.patch(`/admin/instance/pending-registrations/${pendingUserId}`)
					.body({status: 'approved'})
					.expect(200)
					.execute();
				await createBuilder(harness, owner.token)
					.get(`/guilds/${guild.id}/members/${pendingUserId}`)
					.expect(200)
					.execute();
			} finally {
				Config.instance.autoJoinInviteCode = originalAutoJoinInviteCode;
			}
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
					client_binding: startData1.client_binding,
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
					client_binding: startData2.client_binding,
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
					client_binding: startData1.client_binding,
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
					client_binding: startData2.client_binding,
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
					client_binding: startData1.client_binding,
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
					client_binding: startData2.client_binding,
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
					client_binding: startData1.client_binding,
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
					client_binding: startData2.client_binding,
				})
				.execute();
			expect(secondLogin.user_id).not.toBe(firstLogin.user_id);
		});
		async function signInWithSso(code: string): Promise<SsoCompleteResponse> {
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			return createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/sso/complete')
				.body({code, state: startData.state, client_binding: startData.client_binding})
				.execute();
		}
		it('joins an existing account to the single community when it signs in', async () => {
			await enableSso(harness, admin.token, {enforced: false});
			const email = createUniqueEmail('sso-existing-single');
			const first = await signInWithSso(email);
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			await getInstanceConfigRepository().setInstancePolicyConfig({
				single_community_enabled: true,
				single_community_guild_id: guild.id,
			});
			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${first.user_id}`)
				.expect(404)
				.execute();
			const second = await signInWithSso(email);
			expect(second.user_id).toBe(first.user_id);
			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${first.user_id}`)
				.expect(200)
				.execute();
		});
		it('keeps a banned account out of the single community when it signs in', async () => {
			await enableSso(harness, admin.token, {enforced: false});
			const email = createUniqueEmail('sso-existing-banned');
			const first = await signInWithSso(email);
			const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
			await createBuilder(harness, owner.token)
				.put(`/guilds/${guild.id}/bans/${first.user_id}`)
				.body({})
				.expect(204)
				.execute();
			await getInstanceConfigRepository().setInstancePolicyConfig({
				single_community_enabled: true,
				single_community_guild_id: guild.id,
			});
			const second = await signInWithSso(email);
			expect(second.token).toBeTruthy();
			await createBuilder(harness, owner.token)
				.get(`/guilds/${guild.id}/members/${first.user_id}`)
				.expect(404)
				.execute();
		});
		it('asks for the second factor before linking an account that has one', async () => {
			await disableSso(harness, admin.token);
			const account = await createTestAccount(harness);
			const secret = createTotpSecret();
			const enabled = await createBuilder<{backup_codes: Array<{code: string}>}>(harness, account.token)
				.post('/users/@me/mfa/totp/enable')
				.body({secret, code: totpCodeNow(secret), password: account.password})
				.execute();
			await enableSso(harness, admin.token);
			const startData = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_to: '/channels/@me'})
				.execute();
			const challenge = await createBuilderWithoutAuth<SsoMfaChallengeResponse>(harness)
				.post('/auth/sso/complete')
				.body({code: account.email, state: startData.state, client_binding: startData.client_binding})
				.execute();
			expect(challenge).toMatchObject({mfa: true, backup_codes: true, redirect_to: '/channels/@me'});
			expect(challenge).not.toHaveProperty('token');
			const linked = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/login/mfa/totp')
				.body({ticket: challenge.ticket, code: enabled.backup_codes[0]!.code})
				.execute();
			expect(linked.user_id).toBe(account.userId);
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
					client_binding: startData.client_binding,
				})
				.expect(400)
				.execute();
		});
	});
	describe('linking existing accounts', () => {
		let admin: TestAccount;
		beforeEach(async () => {
			admin = await setUserACLs(harness, await createTestAccount(harness), [
				'admin:authenticate',
				'instance:config:update',
				'instance:config:view',
			]);
		});
		afterEach(async () => {
			await disableSso(harness, admin.token);
		});
		async function accountThenSso(params?: {skipEmailVerification: boolean}): Promise<TestAccount> {
			const account = await createTestAccount(harness, params);
			await enableSso(harness, admin.token);
			return account;
		}
		function providerCode(email: string, sub: string): string {
			return JSON.stringify({email, sub, email_verified: true});
		}
		function startSignIn(): Promise<SsoStartResponse> {
			return createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body({}).execute();
		}
		function startLink(account: TestAccount): Promise<SsoStartResponse> {
			return createBuilder<SsoStartResponse>(harness, account.token)
				.post('/users/@me/sso/link/start')
				.body({password: account.password})
				.execute();
		}
		function complete<T>(start: SsoStartResponse, code: string): Promise<T> {
			return createBuilderWithoutAuth<T>(harness)
				.post('/auth/sso/complete')
				.body({code, state: start.state, client_binding: start.client_binding})
				.execute();
		}
		async function completeError(start: SsoStartResponse, code: string): Promise<string | undefined> {
			const body = await createBuilderWithoutAuth<{errors?: Array<{code: string}>}>(harness)
				.post('/auth/sso/complete')
				.body({code, state: start.state, client_binding: start.client_binding})
				.expect(400)
				.execute();
			return body.errors?.[0]?.code;
		}
		async function traitsOf(account: TestAccount): Promise<Array<string>> {
			return (await createBuilder<{traits: Array<string>}>(harness, account.token).get('/users/@me').execute()).traits;
		}

		it('does not sign in to an account whose email is not confirmed', async () => {
			const account = await accountThenSso({skipEmailVerification: true});
			expect(await completeError(await startSignIn(), account.email)).toBe(
				ValidationErrorCodes.SSO_ACCOUNT_LINK_REQUIRES_SIGN_IN,
			);
			expect(await traitsOf(account)).not.toContain('sso');
		});

		it('does not sign in to an existing account by email when the instance sends no email', async () => {
			const account = await accountThenSso();
			await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: EmailDeliveryModes.OFF}});
			expect(await completeError(await startSignIn(), account.email)).toBe(
				ValidationErrorCodes.SSO_ACCOUNT_LINK_REQUIRES_SIGN_IN,
			);
		});

		it('signs in to an account with a confirmed email on an instance that sends email', async () => {
			const account = await accountThenSso();
			expect(await getInstanceConfigRepository().isEmailEnabled()).toBe(true);
			const signedIn = await complete<SsoCompleteResponse>(await startSignIn(), account.email);
			expect(signedIn.user_id).toBe(account.userId);
		});

		it('asks a connected account for its second factor when it signs in with SSO', async () => {
			const account = await accountThenSso();
			const code = providerCode(account.email, `itest-sso-mfa-${Date.now()}`);
			await complete(await startSignIn(), code);
			await enableTotp(harness, account);
			const signIn = await complete<SsoMfaChallengeResponse>(await startSignIn(), code);
			expect(signIn).toMatchObject({mfa: true, backup_codes: true});
			expect(signIn).not.toHaveProperty('token');
		});

		function completeMobile<T>(state: string, code: string): Promise<T> {
			return createBuilderWithoutAuth<T>(harness).post('/auth/sso/complete').body({code, state}).execute();
		}
		async function startMobileSignIn(): Promise<string> {
			const {state} = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({redirect_uri: SSO_MOBILE_CALLBACK_URI})
				.execute();
			return state;
		}

		it('signs a linked account with a second factor in from the mobile app without asking for it', async () => {
			const account = await accountThenSso();
			const code = providerCode(account.email, `itest-sso-mfa-mobile-${Date.now()}`);
			await complete(await startSignIn(), code);
			await enableTotp(harness, account);
			const signIn = await completeMobile<SsoCompleteResponse>(await startMobileSignIn(), code);
			expect(signIn).toMatchObject({token: expect.any(String), user_id: account.userId});
			const webSignIn = await complete<SsoMfaChallengeResponse>(await startSignIn(), code);
			expect(webSignIn).toMatchObject({mfa: true});
			expect(webSignIn).not.toHaveProperty('token');
		});

		it('still asks the mobile app for the second factor before linking an account by email', async () => {
			const account = await accountThenSso();
			await enableTotp(harness, account);
			const code = providerCode(account.email, `itest-sso-mfa-mobile-first-${Date.now()}`);
			const signIn = await completeMobile<SsoMfaChallengeResponse>(await startMobileSignIn(), code);
			expect(signIn).toMatchObject({mfa: true});
			expect(signIn).not.toHaveProperty('token');
		});

		it('asks for the second factor when the account has lost its SSO link record', async () => {
			const account = await accountThenSso();
			const code = providerCode(account.email, `itest-sso-mfa-unmapped-${Date.now()}`);
			await complete(await startSignIn(), code);
			await enableTotp(harness, account);
			await new SsoIdentityRepository().releaseIdentitiesForUser(createUserID(BigInt(account.userId)));
			const signIn = await complete<SsoMfaChallengeResponse>(await startSignIn(), code);
			expect(signIn).toMatchObject({mfa: true});
			expect(signIn).not.toHaveProperty('token');
		});

		it('finishes a second factor sign-in that was waiting when the server was upgraded', async () => {
			const account = await accountThenSso();
			const sub = `itest-sso-mfa-legacy-${Date.now()}`;
			const code = providerCode(account.email, sub);
			await complete(await startSignIn(), code);
			const [backupCode] = await enableTotp(harness, account);
			const challenge = await complete<SsoMfaChallengeResponse>(await startSignIn(), code);
			const key = `sso:pending-link:${challenge.ticket}`;
			const stored = JSON.parse((await getKVClient().get(key)) ?? '{}');
			await getKVClient().set(
				key,
				JSON.stringify({userId: stored.userId, providerId: stored.providerId, subject: sub, groups: null}),
			);
			const login = await createBuilderWithoutAuth<{token: string}>(harness)
				.post('/auth/login/mfa/totp')
				.body({ticket: challenge.ticket, code: backupCode})
				.execute();
			expect(login.token).toEqual(expect.any(String));
			expect((await new SsoIdentityRepository().findUserId(stored.providerId, sub))?.toString()).toBe(account.userId);
		});

		it('stores the subject and groups an older server needs to finish a second factor sign-in', async () => {
			const account = await accountThenSso();
			const sub = `itest-sso-mfa-older-${Date.now()}`;
			const code = providerCode(account.email, sub);
			await complete(await startSignIn(), code);
			const [backupCode] = await enableTotp(harness, account);
			const challenge = await complete<SsoMfaChallengeResponse>(await startSignIn(), code);
			const key = `sso:pending-link:${challenge.ticket}`;
			const stored = JSON.parse((await getKVClient().get(key)) ?? '{}');
			expect(stored).toMatchObject({subject: sub, groups: null});
			const {claims: _claims, ...olderServerView} = stored;
			await getKVClient().set(key, JSON.stringify(olderServerView));
			const login = await createBuilderWithoutAuth<{token: string}>(harness)
				.post('/auth/login/mfa/totp')
				.body({ticket: challenge.ticket, code: backupCode})
				.execute();
			expect(login.token).toEqual(expect.any(String));
		});

		it('keeps the connect state under a key that older servers never read', async () => {
			const account = await accountThenSso();
			const {state} = await startLink(account);
			expect(await getKVClient().get(`sso:state:${state}`)).toBeNull();
			expect(await getKVClient().get(`sso:link-state:${state}`)).not.toBeNull();
		});

		it('connects the signed-in account from settings while SSO is required', async () => {
			const account = await accountThenSso();
			const sub = `itest-sso-link-${Date.now()}`;
			const linked = await complete<Record<string, unknown>>(
				await startLink(account),
				providerCode(createUniqueEmail('sso-link'), sub),
			);
			expect(linked).toEqual({linked: true, redirect_to: ''});
			const signedIn = await complete<SsoCompleteResponse>(
				await startSignIn(),
				providerCode(createUniqueEmail('sso-link-again'), sub),
			);
			expect(signedIn.user_id).toBe(account.userId);
		});

		it('refuses to connect an identity another account already uses', async () => {
			const account = await accountThenSso();
			const sub = `itest-sso-link-taken-${Date.now()}`;
			await complete(await startSignIn(), providerCode(createUniqueEmail('sso-link-holder'), sub));
			expect(await completeError(await startLink(account), providerCode(createUniqueEmail('sso-link'), sub))).toBe(
				ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT,
			);
			expect(await traitsOf(account)).not.toContain('sso');
		});

		it('asks for sudo before starting to connect', async () => {
			const account = await accountThenSso();
			await createBuilder(harness, account.token)
				.post('/users/@me/sso/link/start')
				.body({})
				.expect(403, 'SUDO_MODE_REQUIRED')
				.execute();
		});

		it('does not connect once the session that started it has signed out', async () => {
			const account = await accountThenSso();
			const start = await startLink(account);
			await createBuilder(harness, account.token).post('/auth/logout').expect(204).execute();
			expect(
				await completeError(start, providerCode(createUniqueEmail('sso-link'), `itest-sso-out-${Date.now()}`)),
			).toBe(ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE);
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
			const issuer = 'https://1.1.1.1/realms/no-claims';
			server.use(
				http.get(`${issuer}/.well-known/openid-configuration`, () =>
					HttpResponse.json({
						issuer,
						authorization_endpoint: `${issuer}/auth`,
						token_endpoint: `${issuer}/token`,
					}),
				),
			);
			await createBuilder(harness, admin.token)
				.patch('/admin/instance/config')
				.body({
					sso: {
						enabled: true,
						enforced: false,
						issuer,
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
		it('reports the configured button text in the status and discovery documents', async () => {
			await enableSso(harness, admin.token, {enforced: false, button_label: '  Sign in with Acme ID  '});
			const status = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(status.button_label).toBe('Sign in with Acme ID');
			const discovery = await createBuilderWithoutAuth<{sso: SsoStatusResponse}>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(discovery.sso.button_label).toBe('Sign in with Acme ID');
			const adminConfig = await createBuilder<{sso: {button_label: string | null}}>(harness, admin.token)
				.get('/admin/instance/config')
				.execute();
			expect(adminConfig.sso.button_label).toBe('Sign in with Acme ID');
			await createBuilder(harness, admin.token)
				.patch('/admin/instance/config')
				.body({sso: {button_label: '   '}})
				.execute();
			const cleared = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(cleared.button_label).toBeNull();
			await disableSso(harness, admin.token);
		});
		it('keeps the button text when another SSO field changes', async () => {
			await enableSso(harness, admin.token, {enforced: false, button_label: 'Use Acme ID'});
			await createBuilder(harness, admin.token)
				.patch('/admin/instance/config')
				.body({sso: {display_name: 'Acme'}})
				.execute();
			const status = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
			expect(status.button_label).toBe('Use Acme ID');
			await disableSso(harness, admin.token);
		});
		for (const [name, label] of [
			['too long', 'a'.repeat(65)],
			['with a line break', 'Sign in\nwith Acme'],
			['with a bidirectional override', 'Sign in \u202Ewith Acme'],
		] as const) {
			it(`rejects button text ${name}`, async () => {
				await createBuilder(harness, admin.token)
					.patch('/admin/instance/config')
					.body({sso: {button_label: label}})
					.expect(400)
					.execute();
				const status = await createBuilderWithoutAuth<SsoStatusResponse>(harness).get('/auth/sso/status').execute();
				expect(status.button_label).toBeNull();
			});
		}
	});
});
