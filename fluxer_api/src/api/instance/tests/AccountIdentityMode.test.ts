// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {createAuthHarness, createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getCachedAccountIdentityMode} from '@app/api/instance/AccountIdentityModeCache';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const ACCOUNT_IDENTITY_CONFIG_KEY = 'account_identity_config';
const SETUP_TOKEN = 'setup-token-for-tests-0123456789-abcdefghijklmnopqrstuvwxyz';
const WRONG_SETUP_TOKEN = 'wrong-setup-token-0123456789-abcdefghijklmnopqrstuvwxyz';

interface DiscoveryResponse {
	features: {account_identity: string; emails_enabled: boolean; tag_style: string};
	app_public: {
		setup: {
			configured: boolean;
			account_identity_locked?: boolean;
			account_identity_lock_reason?: string | null;
			setup_token_required?: boolean;
		};
	};
}

interface AdminInstanceConfigResponse {
	account_identity: {mode: string; locked: boolean; tag_style: string};
}

describe('Account identity mode', () => {
	let harness: ApiTestHarness;
	let repository: InstanceConfigRepository;
	const config = getConfig();
	let originalSelfHosted: boolean;
	let originalEmailEnabled: boolean;
	let originalSetupConfigured: boolean;
	let originalAccountIdentity: typeof config.instance.accountIdentity;
	let originalTagStyle: typeof config.instance.tagStyle;
	let originalSetupToken: typeof config.instance.setup.token;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		repository = getInstanceConfigRepository();
		originalSelfHosted = config.instance.selfHosted;
		originalEmailEnabled = config.email.enabled;
		originalSetupConfigured = config.instance.setup.configured;
		originalAccountIdentity = config.instance.accountIdentity;
		originalTagStyle = config.instance.tagStyle;
		originalSetupToken = config.instance.setup.token;
	});
	afterEach(() => {
		config.instance.selfHosted = originalSelfHosted;
		config.email.enabled = originalEmailEnabled;
		config.instance.setup.configured = originalSetupConfigured;
		config.instance.accountIdentity = originalAccountIdentity;
		config.instance.tagStyle = originalTagStyle;
		config.instance.setup.token = originalSetupToken;
		repository.clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function useFreshSelfHostedInstance(): void {
		config.instance.selfHosted = true;
		config.email.enabled = false;
		config.instance.setup.configured = false;
		config.instance.accountIdentity = null;
		config.instance.tagStyle = null;
	}

	async function readStoredRow(): Promise<{
		mode: string;
		source: string;
		decided_at: string;
		tag_style?: string;
	} | null> {
		const raw = await repository.getConfig(ACCOUNT_IDENTITY_CONFIG_KEY);
		return raw === null ? null : JSON.parse(raw);
	}

	async function createAccountWhileHosted(): Promise<void> {
		const selfHosted = config.instance.selfHosted;
		config.instance.selfHosted = false;
		try {
			await createTestAccount(harness);
		} finally {
			config.instance.selfHosted = selfHosted;
		}
	}

	async function fetchDiscovery(): Promise<DiscoveryResponse> {
		return await createBuilderWithoutAuth<DiscoveryResponse>(harness).get('/.well-known/fluxer').execute();
	}

	it('never stores a mode on the hosted service and always reports email', async () => {
		config.instance.selfHosted = false;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toBeNull();
		expect(await repository.getAccountIdentityMode()).toBe(AccountIdentityModes.EMAIL);
		expect(getCachedAccountIdentityMode()).toBe(AccountIdentityModes.EMAIL);
		const discovery = await fetchDiscovery();
		expect(discovery.features.account_identity).toBe(AccountIdentityModes.EMAIL);
		expect(discovery.features.tag_style).toBe('random');
	});

	it('stamps email when a users row exists', async () => {
		useFreshSelfHostedInstance();
		await createAccountWhileHosted();
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
	});

	it('stamps email when the admin was bootstrapped', async () => {
		useFreshSelfHostedInstance();
		await repository.claimAdminBootstrap();
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
	});

	it('stamps email when setup is configured in env', async () => {
		useFreshSelfHostedInstance();
		config.instance.setup.configured = true;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
	});

	it('stamps email when setup is configured in storage', async () => {
		useFreshSelfHostedInstance();
		await repository.setAppPublicConfig({setup: {configured: true}});
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
	});

	it('forces the effective email config off in username mode', async () => {
		useFreshSelfHostedInstance();
		await repository.setInstanceIntegrationsConfig({
			email: {
				enabled: true,
				provider: 'smtp',
				from_email: 'noreply@example.com',
				smtp: {host: 'smtp.example.com', port: 587, username: 'mailer', password: 'mailer-password'},
			},
		});
		await repository.setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
		expect(await repository.isEmailEnabled()).toBe(true);
		await repository.setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		expect((await repository.getEffectiveEmailConfig()).enabled).toBe(false);
		expect(await repository.isEmailEnabled()).toBe(false);
		expect((await repository.getInstanceIntegrationsAdminConfig()).email.effective_enabled).toBe(false);
		expect((await fetchDiscovery()).features.emails_enabled).toBe(false);
	});

	describe('setup choice', () => {
		async function putMode(mode: string, status = 200, tagStyle?: string) {
			return await createBuilderWithoutAuth<{mode: string; tag_style: string}>(harness)
				.put('/instance/setup/account-identity')
				.body(tagStyle === undefined ? {mode} : {mode, tag_style: tagStyle})
				.expect(status)
				.execute();
		}

		it('is locked on the hosted service', async () => {
			config.instance.selfHosted = false;
			const {json} = await createBuilderWithoutAuth<{code: string}>(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.USERNAME})
				.expect(409)
				.executeWithResponse();
			expect(json.code).toBe(APIErrorCodes.ACCOUNT_IDENTITY_LOCKED);
			expect(await readStoredRow()).toBeNull();
		});

		it('switches freely before the first account and before setup completes', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			expect(await putMode(AccountIdentityModes.EMAIL)).toEqual({
				mode: AccountIdentityModes.EMAIL,
				tag_style: 'none',
			});
			expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'setup'});
			expect((await fetchDiscovery()).features.account_identity).toBe(AccountIdentityModes.EMAIL);
			expect(await putMode(AccountIdentityModes.USERNAME)).toEqual({
				mode: AccountIdentityModes.USERNAME,
				tag_style: 'none',
			});
			expect((await fetchDiscovery()).features.account_identity).toBe(AccountIdentityModes.USERNAME);
		});

		it('takes the username style together with email', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			expect(await putMode(AccountIdentityModes.EMAIL, 200, 'random')).toEqual({
				mode: AccountIdentityModes.EMAIL,
				tag_style: 'random',
			});
			expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, tag_style: 'random'});
			expect(await putMode(AccountIdentityModes.EMAIL, 200, 'none')).toEqual({
				mode: AccountIdentityModes.EMAIL,
				tag_style: 'none',
			});
			expect((await fetchDiscovery()).features).toMatchObject({
				account_identity: AccountIdentityModes.EMAIL,
				tag_style: 'none',
			});
		});

		it('refuses random tags with username sign-in', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			const {json} = await createBuilderWithoutAuth<{errors: Array<{path: string; code: string}>}>(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.USERNAME, tag_style: 'random'})
				.expect(400, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect(json.errors.map(({path, code}) => ({path, code}))).toEqual([
				{path: 'tag_style', code: ValidationErrorCodes.TAG_STYLE_REQUIRES_EMAIL_SIGN_IN},
			]);
			expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.USERNAME, tag_style: 'none'});
			expect(await putMode(AccountIdentityModes.USERNAME, 200, 'none')).toEqual({
				mode: AccountIdentityModes.USERNAME,
				tag_style: 'none',
			});
		});

		it('refuses the retired zero_first style', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			await createBuilderWithoutAuth(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.EMAIL, tag_style: 'zero_first'})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
		});

		it('locks the tag choice once an account exists', async () => {
			useFreshSelfHostedInstance();
			await repository.setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup', 'random');
			await createBuilderWithoutAuth(harness)
				.post('/auth/register')
				.body({email: 'taggedadmin@example.com', username: 'taggedadmin', password: 'a-strong-password-123'})
				.execute();
			await createBuilderWithoutAuth(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.EMAIL, tag_style: 'none'})
				.expect(409, APIErrorCodes.ACCOUNT_IDENTITY_LOCKED)
				.execute();
			expect(await repository.getAccountIdentity()).toEqual({
				mode: AccountIdentityModes.EMAIL,
				tagStyle: 'random',
			});
		});

		it('locks once an account exists', async () => {
			useFreshSelfHostedInstance();
			await repository.setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
			await createBuilderWithoutAuth(harness)
				.post('/auth/register')
				.body({username: 'firstadmin', global_name: 'First', password: 'a-strong-password-123'})
				.execute();
			await createBuilderWithoutAuth(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.EMAIL})
				.expect(409, APIErrorCodes.ACCOUNT_IDENTITY_LOCKED)
				.execute();
			expect(await repository.getAccountIdentityMode()).toBe(AccountIdentityModes.USERNAME);
		});

		it('locks once setup is configured', async () => {
			useFreshSelfHostedInstance();
			await repository.setAppPublicConfig({setup: {configured: true}});
			await createBuilderWithoutAuth(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.EMAIL})
				.expect(409, APIErrorCodes.ACCOUNT_IDENTITY_LOCKED)
				.execute();
		});

		it('locks an instance stored as already in use even with no account', async () => {
			useFreshSelfHostedInstance();
			await repository.setAccountIdentityMode(AccountIdentityModes.EMAIL, 'existing_instance', 'random');
			expect(await repository.isAccountIdentityLocked()).toBe(true);
			await createBuilderWithoutAuth(harness)
				.put('/instance/setup/account-identity')
				.body({mode: AccountIdentityModes.USERNAME})
				.expect(409, APIErrorCodes.ACCOUNT_IDENTITY_LOCKED)
				.execute();
			expect((await fetchDiscovery()).app_public.setup.account_identity_locked).toBe(true);
		});

		it('reports the lock in discovery only while setup is open', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			expect((await fetchDiscovery()).app_public.setup.account_identity_locked).toBe(false);
			await createBuilderWithoutAuth(harness)
				.post('/auth/register')
				.body({username: 'lockedadmin', global_name: 'Locked', password: 'a-strong-password-123'})
				.execute();
			expect((await fetchDiscovery()).app_public.setup.account_identity_locked).toBe(true);
			await repository.setAppPublicConfig({setup: {configured: true}});
			expect((await fetchDiscovery()).app_public.setup).not.toHaveProperty('account_identity_locked');
			config.instance.selfHosted = false;
			expect((await fetchDiscovery()).app_public.setup).not.toHaveProperty('account_identity_locked');
		});

		it('reports why the sign-in method is locked', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			expect((await fetchDiscovery()).app_public.setup).toMatchObject({
				account_identity_locked: false,
				account_identity_lock_reason: null,
			});
			await createAccountWhileHosted();
			expect((await fetchDiscovery()).app_public.setup).toMatchObject({
				account_identity_locked: true,
				account_identity_lock_reason: 'accounts_exist',
			});
			await repository.claimAdminBootstrap();
			expect((await fetchDiscovery()).app_public.setup.account_identity_lock_reason).toBe('admin_exists');
			await repository.setAccountIdentityMode(AccountIdentityModes.EMAIL, 'existing_instance', 'random');
			expect((await fetchDiscovery()).app_public.setup.account_identity_lock_reason).toBe('existing_instance');
			config.instance.setup.configured = true;
			expect(await repository.getAccountIdentityLockReason()).toBe('setup_configured');
			config.instance.setup.configured = false;
			await repository.setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
			await repository.setAppPublicConfig({setup: {configured: true}});
			expect(await repository.getAccountIdentityLockReason()).toBe('setup_configured');
			expect((await fetchDiscovery()).app_public.setup).not.toHaveProperty('account_identity_lock_reason');
		});
	});

	describe('setup token', () => {
		async function putModeWithToken(setupToken?: string) {
			return await createBuilderWithoutAuth<{
				mode?: string;
				tag_style?: string;
				errors?: Array<{path: string; code: string}>;
			}>(harness)
				.put('/instance/setup/account-identity')
				.body(
					setupToken === undefined
						? {mode: AccountIdentityModes.EMAIL}
						: {mode: AccountIdentityModes.EMAIL, setup_token: setupToken},
				)
				.executeRaw();
		}

		it('reports in discovery whether registration needs the setup token', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			expect((await fetchDiscovery()).app_public.setup.setup_token_required).toBe(false);
			config.instance.setup.token = SETUP_TOKEN;
			const {text, json} = await createBuilderWithoutAuth<DiscoveryResponse>(harness)
				.get('/.well-known/fluxer')
				.executeRaw();
			expect(json.app_public.setup.setup_token_required).toBe(true);
			expect(text).not.toContain(SETUP_TOKEN);
			expect(text).not.toContain(createHash('sha256').update(SETUP_TOKEN).digest('hex'));
			await repository.claimAdminBootstrap();
			expect((await fetchDiscovery()).app_public.setup.setup_token_required).toBe(false);
			await repository.setAppPublicConfig({setup: {configured: true}});
			expect((await fetchDiscovery()).app_public.setup).not.toHaveProperty('setup_token_required');
		});

		it('leaves the setup token out of discovery on the hosted service', async () => {
			config.instance.selfHosted = false;
			config.instance.setup.token = SETUP_TOKEN;
			await repository.setAppPublicConfig({setup: {configured: false}});
			expect((await fetchDiscovery()).app_public.setup).not.toHaveProperty('setup_token_required');
		});

		it('needs the setup token to choose the sign-in method while one is required', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			config.instance.setup.token = SETUP_TOKEN;
			const missing = await putModeWithToken();
			expect(missing.response.status).toBe(400);
			expect(missing.json.errors?.map(({path, code}) => ({path, code}))).toEqual([
				{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_REQUIRED},
			]);
			const wrong = await putModeWithToken(WRONG_SETUP_TOKEN);
			expect(wrong.response.status).toBe(400);
			expect(wrong.json.errors?.map(({path, code}) => ({path, code}))).toEqual([
				{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_INVALID},
			]);
			expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.USERNAME});
			const correct = await putModeWithToken(SETUP_TOKEN);
			expect(correct.response.status).toBe(200);
			expect(correct.json).toEqual({mode: AccountIdentityModes.EMAIL, tag_style: 'none'});
			expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'setup'});
		});

		it('chooses the sign-in method without a setup token when none is required', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			const tokenless = await putModeWithToken();
			expect(tokenless.response.status).toBe(200);
			const ignored = await putModeWithToken(WRONG_SETUP_TOKEN);
			expect(ignored.response.status).toBe(200);
		});
	});

	describe('setup token check', () => {
		async function checkToken(body: Record<string, unknown>, ipAddress?: string) {
			const builder = createBuilderWithoutAuth<{code?: string; errors?: Array<{path: string; code: string}>}>(harness)
				.post('/instance/setup/token-checks')
				.body(body);
			if (ipAddress) builder.header('x-forwarded-for', ipAddress);
			return await builder.executeRaw();
		}

		function errorCodes(json: {errors?: Array<{path: string; code: string}>}) {
			return (json.errors ?? []).map(({path, code}) => ({path, code}));
		}

		it('accepts the setup token without using it up', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			config.instance.setup.token = SETUP_TOKEN;
			const accepted = await checkToken({setup_token: SETUP_TOKEN});
			expect(accepted.response.status).toBe(204);
			expect(accepted.text).toBe('');
			expect(await repository.isAdminBootstrapped()).toBe(false);
			expect(await repository.verifySetupToken(SETUP_TOKEN)).toBe(true);
			expect((await fetchDiscovery()).app_public.setup.setup_token_required).toBe(true);
			const again = await checkToken({setup_token: ` ${SETUP_TOKEN}\n`});
			expect(again.response.status).toBe(204);
		});

		it('refuses a wrong or blank setup token', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			config.instance.setup.token = SETUP_TOKEN;
			const wrong = await checkToken({setup_token: WRONG_SETUP_TOKEN});
			expect(wrong.response.status).toBe(400);
			expect(errorCodes(wrong.json)).toEqual([{path: 'setup_token', code: ValidationErrorCodes.SETUP_TOKEN_INVALID}]);
			const blank = await checkToken({setup_token: '   '});
			expect(blank.response.status).toBe(400);
			expect(errorCodes(blank.json).map(({path}) => path)).toEqual(['setup_token']);
			const missing = await checkToken({});
			expect(missing.response.status).toBe(400);
			expect(errorCodes(missing.json).map(({path}) => path)).toEqual(['setup_token']);
		});

		it('answers 204 when the instance needs no setup token', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			expect((await checkToken({setup_token: WRONG_SETUP_TOKEN})).response.status).toBe(204);
			config.instance.setup.token = SETUP_TOKEN;
			await repository.claimAdminBootstrap();
			expect((await checkToken({setup_token: WRONG_SETUP_TOKEN})).response.status).toBe(204);
		});

		it('counts wrong setup tokens toward the same lockout as registration', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			config.instance.setup.token = SETUP_TOKEN;
			const originalRelax = config.dev.relaxRegistrationRateLimits;
			config.dev.relaxRegistrationRateLimits = false;
			try {
				const attacker = '203.0.113.40';
				for (let attempt = 0; attempt < 5; attempt++) {
					const wrong = await createBuilderWithoutAuth(harness)
						.put('/instance/setup/account-identity')
						.header('x-forwarded-for', attacker)
						.body({mode: AccountIdentityModes.EMAIL, setup_token: WRONG_SETUP_TOKEN})
						.executeRaw();
					expect(wrong.response.status).toBe(400);
				}
				for (let attempt = 0; attempt < 5; attempt++) {
					const wrong = await checkToken({setup_token: WRONG_SETUP_TOKEN}, attacker);
					expect(wrong.response.status).toBe(400);
				}
				expect((await checkToken({setup_token: SETUP_TOKEN}, attacker)).response.status).toBe(429);
				expect((await checkToken({setup_token: SETUP_TOKEN}, '203.0.113.41')).response.status).toBe(204);
			} finally {
				config.dev.relaxRegistrationRateLimits = originalRelax;
			}
		});
	});

	describe('admin instance config', () => {
		async function fetchAdminConfig(token: string): Promise<AdminInstanceConfigResponse> {
			return await createBuilder<AdminInstanceConfigResponse>(harness, token).get('/admin/instance/config').execute();
		}

		it('reports email and locked on the hosted service', async () => {
			config.instance.selfHosted = false;
			const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
			expect((await fetchAdminConfig(admin.token)).account_identity).toEqual({
				mode: AccountIdentityModes.EMAIL,
				locked: true,
				tag_style: 'random',
				lock_reason: 'setup_configured',
			});
		});

		it('reports the stored mode and locks once an account exists on self-hosted', async () => {
			useFreshSelfHostedInstance();
			await repository.setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
			const registration = await createBuilderWithoutAuth<{token: string}>(harness)
				.post('/auth/register')
				.body({username: 'setupadmin', global_name: 'Setup Admin', password: 'a-strong-password-123'})
				.execute();
			expect((await fetchAdminConfig(registration.token)).account_identity).toEqual({
				mode: AccountIdentityModes.USERNAME,
				locked: true,
				tag_style: 'none',
				lock_reason: 'admin_exists',
			});
		});
	});
});
