// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getCachedAccountIdentity, getCachedAccountIdentityMode} from '@app/api/instance/AccountIdentityModeCache';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getCacheService, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const ACCOUNT_IDENTITY_CONFIG_KEY = 'account_identity_config';

interface DiscoveryResponse {
	features: {account_identity: string; emails_enabled: boolean; tag_style: string};
	app_public: {setup: {configured: boolean; account_identity_locked?: boolean}};
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
	});
	afterEach(() => {
		vi.restoreAllMocks();
		config.instance.selfHosted = originalSelfHosted;
		config.email.enabled = originalEmailEnabled;
		config.instance.setup.configured = originalSetupConfigured;
		config.instance.accountIdentity = originalAccountIdentity;
		config.instance.tagStyle = originalTagStyle;
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

	it('reports email on the hosted service even when a row exists', async () => {
		useFreshSelfHostedInstance();
		await repository.setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		config.instance.selfHosted = false;
		expect(await repository.getAccountIdentityMode()).toBe(AccountIdentityModes.EMAIL);
		expect(getCachedAccountIdentityMode()).toBe(AccountIdentityModes.EMAIL);
		expect((await fetchDiscovery()).features.account_identity).toBe(AccountIdentityModes.EMAIL);
	});

	it('stamps username on a fresh self-hosted instance', async () => {
		useFreshSelfHostedInstance();
		await repository.ensureAccountIdentityMode();
		const row = await readStoredRow();
		expect(row).toMatchObject({mode: AccountIdentityModes.USERNAME, source: 'new_instance'});
		expect(Number.isNaN(Date.parse(row!.decided_at))).toBe(false);
		expect(await repository.getAccountIdentityMode()).toBe(AccountIdentityModes.USERNAME);
		expect(getCachedAccountIdentityMode()).toBe(AccountIdentityModes.USERNAME);
		const discovery = await fetchDiscovery();
		expect(discovery.features).toMatchObject({
			account_identity: AccountIdentityModes.USERNAME,
			emails_enabled: false,
			tag_style: 'none',
		});
		expect(row).toMatchObject({tag_style: 'none'});
		expect(getCachedAccountIdentity().tagStyle).toBe('none');
	});

	it('ignores a random tag style env on a fresh username instance', async () => {
		useFreshSelfHostedInstance();
		config.instance.tagStyle = 'random';
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({
			mode: AccountIdentityModes.USERNAME,
			tag_style: 'none',
			source: 'new_instance',
		});
		expect(await repository.getAccountIdentity()).toEqual({
			mode: AccountIdentityModes.USERNAME,
			tagStyle: 'none',
		});
		expect((await fetchDiscovery()).features.tag_style).toBe('none');
	});

	it('ignores the tag style env on an existing instance', async () => {
		useFreshSelfHostedInstance();
		config.instance.tagStyle = 'none';
		await createAccountWhileHosted();
		await repository.ensureAccountIdentityMode();
		const row = await readStoredRow();
		expect(row).toMatchObject({mode: AccountIdentityModes.EMAIL, tag_style: 'random', source: 'existing_instance'});
		expect((await fetchDiscovery()).features.tag_style).toBe('random');
	});

	it('reads a username row written before the field existed as no tags', async () => {
		useFreshSelfHostedInstance();
		await repository.setConfig(
			ACCOUNT_IDENTITY_CONFIG_KEY,
			JSON.stringify({mode: AccountIdentityModes.USERNAME, decided_at: new Date().toISOString(), source: 'setup'}),
		);
		repository.clearCacheForTesting();
		expect(await repository.getAccountIdentity()).toEqual({mode: AccountIdentityModes.USERNAME, tagStyle: 'none'});
	});

	it('reads an email row written before the field existed as tagged', async () => {
		useFreshSelfHostedInstance();
		await repository.setConfig(
			ACCOUNT_IDENTITY_CONFIG_KEY,
			JSON.stringify({
				mode: AccountIdentityModes.EMAIL,
				decided_at: new Date().toISOString(),
				source: 'existing_instance',
			}),
		);
		repository.clearCacheForTesting();
		expect(await repository.getAccountIdentity()).toEqual({mode: AccountIdentityModes.EMAIL, tagStyle: 'random'});
	});

	it('stamps no tags for a fresh email instance', async () => {
		useFreshSelfHostedInstance();
		config.instance.accountIdentity = AccountIdentityModes.EMAIL;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({
			mode: AccountIdentityModes.EMAIL,
			tag_style: 'none',
			source: 'new_instance',
		});
		expect((await fetchDiscovery()).features).toMatchObject({
			account_identity: AccountIdentityModes.EMAIL,
			tag_style: 'none',
		});
	});

	it('stamps tags for a fresh email instance when the env asks for them', async () => {
		useFreshSelfHostedInstance();
		config.instance.accountIdentity = AccountIdentityModes.EMAIL;
		config.instance.tagStyle = 'random';
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, tag_style: 'random'});
	});

	it.each([
		[{mode: AccountIdentityModes.USERNAME, unique_usernames: true}, 'none'],
		[{mode: AccountIdentityModes.USERNAME, unique_usernames: false}, 'none'],
		[{mode: AccountIdentityModes.USERNAME, tag_style: 'random'}, 'none'],
		[{mode: AccountIdentityModes.USERNAME, tag_style: 'zero_first'}, 'none'],
		[{mode: AccountIdentityModes.EMAIL, unique_usernames: true}, 'none'],
		[{mode: AccountIdentityModes.EMAIL, unique_usernames: false}, 'random'],
		[{mode: AccountIdentityModes.EMAIL, tag_style: 'zero_first'}, 'none'],
		[{mode: AccountIdentityModes.EMAIL, tag_style: 'random'}, 'random'],
	])('reads the stored row %j as %s', async (row, tagStyle) => {
		useFreshSelfHostedInstance();
		await repository.setConfig(
			ACCOUNT_IDENTITY_CONFIG_KEY,
			JSON.stringify({...row, decided_at: new Date().toISOString(), source: 'setup'}),
		);
		repository.clearCacheForTesting();
		expect((await repository.getAccountIdentity()).tagStyle).toBe(tagStyle);
	});

	it.each(['none', 'random'] as const)('stamps the %s tag style from the env on email', async (tagStyle) => {
		useFreshSelfHostedInstance();
		config.instance.accountIdentity = AccountIdentityModes.EMAIL;
		config.instance.tagStyle = tagStyle;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({tag_style: tagStyle, source: 'new_instance'});
		expect((await fetchDiscovery()).features.tag_style).toBe(tagStyle);
	});

	it('uses the env override for a fresh instance', async () => {
		useFreshSelfHostedInstance();
		config.instance.accountIdentity = AccountIdentityModes.EMAIL;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'new_instance'});
	});

	it('stamps email when a users row exists', async () => {
		useFreshSelfHostedInstance();
		await createAccountWhileHosted();
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
	});

	it('stamps email when the env override says username but users already exist', async () => {
		useFreshSelfHostedInstance();
		config.instance.accountIdentity = AccountIdentityModes.USERNAME;
		await createAccountWhileHosted();
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
	});

	it('stamps email when the admin was bootstrapped', async () => {
		useFreshSelfHostedInstance();
		await repository.markAdminBootstrapped();
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

	it('starts a new instance with email delivery in env on email with no tags', async () => {
		useFreshSelfHostedInstance();
		config.email.enabled = true;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({
			mode: AccountIdentityModes.EMAIL,
			tag_style: 'none',
			source: 'new_instance',
		});
		expect(await repository.isAccountIdentityLocked()).toBe(false);
	});

	it('starts a new instance with email delivery in storage on email', async () => {
		useFreshSelfHostedInstance();
		await repository.setInstanceIntegrationsConfig({email: {enabled: true}});
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'new_instance'});
	});

	it('takes the tag style env on a new instance with email delivery', async () => {
		useFreshSelfHostedInstance();
		config.email.enabled = true;
		config.instance.tagStyle = 'random';
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({
			mode: AccountIdentityModes.EMAIL,
			tag_style: 'random',
			source: 'new_instance',
		});
	});

	it('lets the sign-in env win over email delivery on a new instance', async () => {
		useFreshSelfHostedInstance();
		config.email.enabled = true;
		config.instance.accountIdentity = AccountIdentityModes.USERNAME;
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({
			mode: AccountIdentityModes.USERNAME,
			tag_style: 'none',
			source: 'new_instance',
		});
	});

	it('keeps an existing row', async () => {
		useFreshSelfHostedInstance();
		await repository.setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'setup'});
	});

	it('writes nothing and stays on email when a probe fails', async () => {
		useFreshSelfHostedInstance();
		vi.spyOn(repository as unknown as {hasAnyUser: () => Promise<boolean>}, 'hasAnyUser').mockRejectedValue(
			new Error('database not ready'),
		);
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toBeNull();
		expect(await repository.getAccountIdentityMode()).toBe(AccountIdentityModes.EMAIL);
	});

	it('skips the users probe when a cheaper signal already decides', async () => {
		useFreshSelfHostedInstance();
		const probe = vi.spyOn(repository as unknown as {hasAnyUser: () => Promise<boolean>}, 'hasAnyUser');
		await repository.markAdminBootstrapped();
		await repository.ensureAccountIdentityMode();
		expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.EMAIL, source: 'existing_instance'});
		expect(await repository.isAccountIdentityLocked()).toBe(true);
		expect(probe).not.toHaveBeenCalled();
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

		it('waits for a registration holding the setup lock', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			const cache = getCacheService();
			const token = await cache.acquireLock('account-identity-setup', 30);
			expect(token).not.toBeNull();
			let settled = false;
			const pending = putMode(AccountIdentityModes.EMAIL).then((result) => {
				settled = true;
				return result;
			});
			await new Promise((resolve) => setTimeout(resolve, 250));
			expect(settled).toBe(false);
			expect(await readStoredRow()).toMatchObject({mode: AccountIdentityModes.USERNAME});
			await cache.releaseLock('account-identity-setup', token!);
			expect(await pending).toEqual({mode: AccountIdentityModes.EMAIL, tag_style: 'none'});
		});

		it('makes the first registration wait for a setup choice in progress', async () => {
			useFreshSelfHostedInstance();
			await repository.ensureAccountIdentityMode();
			const cache = getCacheService();
			const token = await cache.acquireLock('account-identity-setup', 30);
			expect(token).not.toBeNull();
			let settled = false;
			const pending = createBuilderWithoutAuth<{token: string}>(harness)
				.post('/auth/register')
				.body({username: 'waitingadmin', global_name: 'Waiting', password: 'a-strong-password-123'})
				.execute()
				.then((result) => {
					settled = true;
					return result;
				});
			await new Promise((resolve) => setTimeout(resolve, 250));
			expect(settled).toBe(false);
			await cache.releaseLock('account-identity-setup', token!);
			expect((await pending).token).toBeTruthy();
			expect(await repository.isAccountIdentityLocked()).toBe(true);
		});

		it('rejects an unknown mode', async () => {
			useFreshSelfHostedInstance();
			await createBuilderWithoutAuth(harness)
				.put('/instance/setup/account-identity')
				.body({mode: 'phone'})
				.expect(400)
				.execute();
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
			});
		});
	});
});
