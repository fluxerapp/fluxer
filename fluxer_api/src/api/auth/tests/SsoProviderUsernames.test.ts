// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SsoCompleteResponse {
	token: string;
	user_id: string;
}

interface CurrentUser {
	username: string;
	discriminator: string;
}

interface ChangeLog {
	entries: Array<{new_value: string | null; reason: string | null}>;
}

describe('Usernames from the SSO provider', () => {
	const config = getConfig();
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let originalSelfHosted: boolean;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		originalSelfHosted = config.instance.selfHosted;
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'instance:config:update',
			'instance:config:view',
			AdminACLs.USER_UPDATE_USERNAME,
			AdminACLs.USER_VIEW_CONTACT_LOG,
		]);
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
		config.instance.selfHosted = originalSelfHosted;
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function useUsernameMode(): Promise<void> {
		config.instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
	}

	async function enableUsernameSso(settings: {enforced: boolean; username_from_provider: boolean}): Promise<void> {
		await useUsernameMode();
		await enableSso(harness, admin.token, {allowed_domains: [], ...settings});
	}

	function subject(label: string): string {
		return `itest-${label}-${Date.now()}-${Math.random().toString(36).slice(2)}`;
	}

	async function completeSso(sub: string, preferredUsername?: string): Promise<SsoCompleteResponse> {
		const start = await createBuilderWithoutAuth<{state: string}>(harness).post('/auth/sso/start').body({}).execute();
		return createBuilderWithoutAuth<SsoCompleteResponse>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify({sub, preferred_username: preferredUsername}), state: start.state})
			.execute();
	}

	async function usernameAfterSignIn(sub: string, preferredUsername?: string): Promise<string> {
		const login = await completeSso(sub, preferredUsername);
		return (await createBuilder<CurrentUser>(harness, login.token).get('/users/@me').execute()).username;
	}

	it('leaves usernames alone while the setting is off, even under Require SSO', async () => {
		await enableUsernameSso({enforced: true, username_from_provider: false});
		const sub = subject('off');
		const login = await completeSso(sub, 'first');
		expect(await usernameAfterSignIn(sub, 'second')).toBe('first');
		const renamed = await createBuilder<CurrentUser>(harness, login.token)
			.patch('/users/@me')
			.body({username: 'localname'})
			.execute();
		expect(renamed.username).toBe('localname');
	});

	it('shows the setting in the admin config and the discovery document', async () => {
		await enableUsernameSso({enforced: false, username_from_provider: true});
		const read = async () => ({
			admin: (
				await createBuilder<{sso: {username_from_provider: boolean}}>(harness, admin.token)
					.get('/admin/instance/config')
					.execute()
			).sso.username_from_provider,
			discovery: (
				await createBuilderWithoutAuth<{sso: {username_from_provider: boolean}}>(harness)
					.get('/.well-known/fluxer')
					.execute()
			).sso.username_from_provider,
		});
		expect(await read()).toEqual({admin: true, discovery: true});
		await enableSso(harness, admin.token, {allowed_domains: [], username_from_provider: false});
		expect(await read()).toEqual({admin: false, discovery: false});
	});

	it('refuses the setting on an instance that is not in username mode', async () => {
		const {json} = await createBuilder<{errors: Array<{path: string}>}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({sso: {username_from_provider: true}})
			.expect(400, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(json.errors.map(({path}) => path)).toEqual(['sso.username_from_provider']);
	});

	describe('while the setting is on', () => {
		beforeEach(async () => {
			await enableUsernameSso({enforced: false, username_from_provider: true});
		});

		it('takes a changed provider username on sign-in', async () => {
			const sub = subject('rename');
			const first = await completeSso(sub, 'alice');
			const second = await completeSso(sub, 'alice2');
			expect(second.user_id).toBe(first.user_id);
			const me = await createBuilder<CurrentUser>(harness, second.token).get('/users/@me').execute();
			expect(me).toMatchObject({username: 'alice2', discriminator: '0000'});
			const log = await createBuilder<ChangeLog>(harness, admin.token)
				.get(`/admin/users/${first.user_id}/change-log`)
				.execute();
			expect(log.entries.find(({new_value}) => new_value === 'alice2')?.reason).toBe('sso_sync');
		});

		it('applies a change in letter case only', async () => {
			const sub = subject('case');
			expect(await usernameAfterSignIn(sub, 'Alice')).toBe('Alice');
			expect(await usernameAfterSignIn(sub, 'alice')).toBe('alice');
		});

		it('keeps the current username while another account holds the provider username', async () => {
			const holder = subject('holder');
			const renamer = subject('renamer');
			await usernameAfterSignIn(holder, 'taken');
			expect(await usernameAfterSignIn(renamer, 'mine')).toBe('mine');
			expect(await usernameAfterSignIn(renamer, 'taken')).toBe('mine');
			expect(await usernameAfterSignIn(holder, 'moved')).toBe('moved');
			expect(await usernameAfterSignIn(renamer, 'taken')).toBe('taken');
		});

		it('keeps the current username when the provider sends no usable username', async () => {
			profileSubstringBlocklistCache.add('username', 'blockedslug');
			const sub = subject('unusable');
			expect(await usernameAfterSignIn(sub, 'kept')).toBe('kept');
			for (const preferredUsername of [undefined, 'everyone', '@@@', 'blockedslug']) {
				expect(await usernameAfterSignIn(sub, preferredUsername)).toBe('kept');
			}
		});

		it('gives a blocked provider username to an account exempt from the blocklist', async () => {
			profileSubstringBlocklistCache.add('username', 'blockedstaff');
			const sub = subject('exempt');
			const first = await completeSso(sub, 'staffer');
			await createBuilder(harness, '')
				.patch(`/test/users/${first.user_id}/flags`)
				.body({flags: UserFlags.STAFF.toString()})
				.execute();
			expect(await usernameAfterSignIn(sub, 'blockedstaff')).toBe('blockedstaff');
		});

		it('applies every provider rename without a rate limit', async () => {
			const sub = subject('many');
			for (let index = 0; index < 6; index++) {
				expect(await usernameAfterSignIn(sub, `many${index}`)).toBe(`many${index}`);
			}
		});

		it('refuses a local rename of an SSO account', async () => {
			const login = await completeSso(subject('locked'), 'locked');
			const {json} = await createBuilder<{message: string}>(harness, login.token)
				.patch('/users/@me')
				.body({username: 'unlocked'})
				.expect(403, 'SSO_REQUIRED')
				.executeWithResponse();
			expect(json.message).toBe(
				'This instance requires single sign-on, so your username comes from your SSO provider.',
			);
			const same = await createBuilder<CurrentUser & {global_name: string | null}>(harness, login.token)
				.patch('/users/@me')
				.body({username: 'locked', global_name: 'Locked'})
				.execute();
			expect(same).toMatchObject({username: 'locked', global_name: 'Locked'});
		});

		it('lets an admin rename an SSO account until its next sign-in', async () => {
			const sub = subject('admin');
			const login = await completeSso(sub, 'provided');
			const renamed = await createBuilder<{user: {username: string}}>(harness, admin.token)
				.patch(`/admin/users/${login.user_id}/username`)
				.body({username: 'adminname'})
				.execute();
			expect(renamed.user.username).toBe('adminname');
			expect(await usernameAfterSignIn(sub, 'provided')).toBe('provided');
		});
	});
});
