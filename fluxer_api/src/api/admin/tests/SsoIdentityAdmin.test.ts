// SPDX-License-Identifier: AGPL-3.0-or-later

import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {deleteOneOrMany} from '@app/api/database/CassandraQueryExecution';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {UserSsoIdentitiesByUser} from '@app/api/Tables';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {
	AdminSsoIdentityLookupResponse,
	AdminSsoIdentityMigrateResponse,
	AdminUserSsoIdentityListResponse,
} from '@fluxer/schema/src/domains/admin/AdminSsoSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('SSO identity admin routes', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const originalSelfHosted = getConfig().instance.selfHosted;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.USER_LOOKUP,
			AdminACLs.USER_UPDATE_EMAIL,
		]);
		await enableSso(harness, admin.token, {enforced: false});
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({sso: {issuer: null, migrate_identities: false}})
			.execute();
		getConfig().instance.selfHosted = originalSelfHosted;
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function signInWithSso(sub: string, email: string | null = createUniqueEmail('sso-admin')): Promise<string> {
		const login = await completeSso(sub, email).expect(200).execute();
		return login.user_id;
	}

	function completeSso(sub: string, email: string | null) {
		const claims = email === null ? {sub} : {sub, email};
		return {
			expect(status: number, code?: string) {
				return {
					async execute(): Promise<{user_id: string; token: string}> {
						const start = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
							.post('/auth/sso/start')
							.body({})
							.execute();
						return createBuilderWithoutAuth<{user_id: string; token: string}>(harness)
							.post('/auth/sso/complete')
							.body({code: JSON.stringify(claims), state: start.state, client_binding: start.client_binding})
							.expect(status, code)
							.execute();
					},
				};
			},
		};
	}

	async function setIssuer(issuer: string): Promise<void> {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({sso: {issuer, migrate_identities: false}})
			.execute();
	}

	async function listIdentities(userId: string) {
		const listed = await createBuilder<AdminUserSsoIdentityListResponse>(harness, admin.token)
			.get(`/admin/users/${userId}/sso-identities`)
			.execute();
		return listed.sso_identities.map(({provider_id, subject}) => ({provider_id, subject}));
	}

	function migrate(fromProviderId: string) {
		return createBuilder<AdminSsoIdentityMigrateResponse>(harness, admin.token)
			.post('/admin/sso-identities/migrate')
			.body({from_provider_id: fromProviderId});
	}

	function link(userId: string, subject: string) {
		return createBuilder<AdminUserSsoIdentityListResponse>(harness, admin.token)
			.put(`/admin/users/${userId}/sso-identities`)
			.body({subject});
	}

	function unlink(userId: string) {
		return createBuilder<AdminUserSsoIdentityListResponse>(harness, admin.token).delete(
			`/admin/users/${userId}/sso-identities`,
		);
	}

	async function useUsernameMode(): Promise<void> {
		getConfig().instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
	}

	it('maps a user to the provider subject and back', async () => {
		const sub = `itest-admin-sso-${Date.now()}`;
		const userId = await signInWithSso(sub);
		const listed = await createBuilder<AdminUserSsoIdentityListResponse>(harness, admin.token)
			.get(`/admin/users/${userId}/sso-identities`)
			.execute();
		expect(listed.sso_identities).toEqual([
			{provider_id: 'test', subject: sub, user_id: userId, claimed_at: expect.any(String)},
		]);
		const query = new URLSearchParams({provider_id: 'test', subject: sub});
		const found = await createBuilder<AdminSsoIdentityLookupResponse>(harness, admin.token)
			.get(`/admin/sso-identities?${query.toString()}`)
			.execute();
		expect(found.sso_identity?.user_id).toBe(userId);
	});

	it('returns an empty list for an account without SSO and null for an unknown subject', async () => {
		const local = await createTestAccount(harness);
		const listed = await createBuilder<AdminUserSsoIdentityListResponse>(harness, admin.token)
			.get(`/admin/users/${local.userId}/sso-identities`)
			.execute();
		expect(listed.sso_identities).toEqual([]);
		const query = new URLSearchParams({provider_id: 'test', subject: 'nobody'});
		const missing = await createBuilder<AdminSsoIdentityLookupResponse>(harness, admin.token)
			.get(`/admin/sso-identities?${query.toString()}`)
			.execute();
		expect(missing.sso_identity).toBeNull();
	});

	it('returns UNKNOWN_USER for an account that does not exist', async () => {
		await createBuilder(harness, admin.token)
			.get('/admin/users/123456789012345678/sso-identities')
			.expect(404, 'UNKNOWN_USER')
			.execute();
	});

	it('requires the user lookup permission', async () => {
		const viewer = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE]);
		await createBuilder(harness, viewer.token)
			.get(`/admin/users/${viewer.userId}/sso-identities`)
			.expect(403)
			.execute();
	});

	it('lists an identity bound before the per-account index existed once it signs in again', async () => {
		const sub = `itest-admin-sso-legacy-${Date.now()}`;
		const email = createUniqueEmail('sso-admin-legacy');
		const userId = await signInWithSso(sub, email);
		await deleteOneOrMany(
			UserSsoIdentitiesByUser.deleteByPk({user_id: createUserID(BigInt(userId)), provider_id: 'test', subject: sub}),
		);
		expect(await listIdentities(userId)).toEqual([]);
		expect(await signInWithSso(sub, email)).toBe(userId);
		expect(await listIdentities(userId)).toEqual([{provider_id: 'test', subject: sub}]);
	});

	describe('after the issuer changes', () => {
		it('links the account again by email when the subjects stay on the old issuer', async () => {
			await setIssuer('test-old');
			const email = createUniqueEmail('sso-admin-relink');
			const userId = await signInWithSso(`itest-admin-sso-relink-old-${Date.now()}`, email);
			await setIssuer('test-new');
			expect(await signInWithSso(`itest-admin-sso-relink-new-${Date.now()}`, email)).toBe(userId);
		});

		it('moves the subjects to the new issuer, then signs the account in', async () => {
			await setIssuer('test-old');
			const sub = `itest-admin-sso-move-${Date.now()}`;
			const email = createUniqueEmail('sso-admin-move');
			const userId = await signInWithSso(sub, email);
			await setIssuer('test-new');
			const moved = await migrate('test-old').expect(200).execute();
			expect(moved).toEqual({to_provider_id: 'test-new', moved_count: 1, skipped_count: 0});
			expect(await signInWithSso(sub, email)).toBe(userId);
			expect(await listIdentities(userId)).toEqual([{provider_id: 'test-new', subject: sub}]);
			expect(await new SsoIdentityRepository().findUserId('test-old', sub)).toBeNull();
		});

		it('leaves a subject that another account already holds under the new issuer', async () => {
			await setIssuer('test-old');
			const sub = `itest-admin-sso-taken-${Date.now()}`;
			const oldOwner = await signInWithSso(sub);
			await setIssuer('test-new');
			const newOwner = await signInWithSso(sub);
			expect(newOwner).not.toBe(oldOwner);
			const moved = await migrate('test-old').expect(200).execute();
			expect(moved).toEqual({to_provider_id: 'test-new', moved_count: 0, skipped_count: 1});
			expect((await new SsoIdentityRepository().findUserId('test-new', sub))?.toString()).toBe(newOwner);
			expect(await listIdentities(oldOwner)).toEqual([{provider_id: 'test-old', subject: sub}]);
		});

		it('refuses to move subjects onto the issuer in use', async () => {
			await migrate('test').expect(400, 'INVALID_FORM_BODY').execute();
		});

		it('binds a new subject to an account by hand on a username instance', async () => {
			await useUsernameMode();
			await enableSso(harness, admin.token, {enforced: false, allowed_domains: []});
			const oldSub = `itest-admin-sso-old-${Date.now()}`;
			const newSub = `itest-admin-sso-new-${Date.now()}`;
			const userId = await signInWithSso(oldSub, null);
			const linked = await link(userId, newSub).expect(200).execute();
			expect(linked.sso_identities.map(({provider_id, subject}) => ({provider_id, subject}))).toEqual([
				{provider_id: 'test', subject: newSub},
			]);
			expect(await signInWithSso(newSub, null)).toBe(userId);
			expect(await signInWithSso(oldSub, null)).not.toBe(userId);
		});

		it('refuses to bind a subject another account holds', async () => {
			const taken = `itest-admin-sso-held-${Date.now()}`;
			await signInWithSso(taken);
			const other = await signInWithSso(`itest-admin-sso-other-${Date.now()}`);
			const body = await link(other, taken).expect(400, 'INVALID_FORM_BODY').execute();
			expect(body).toMatchObject({
				errors: [{path: 'subject', code: ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT}],
			});
		});

		it('unbinds an account so the next sign-in binds it again by email', async () => {
			const email = createUniqueEmail('sso-admin-unlink');
			const userId = await signInWithSso(`itest-admin-sso-before-${Date.now()}`, email);
			const unlinked = await unlink(userId).expect(200).execute();
			expect(unlinked.sso_identities).toEqual([]);
			const after = `itest-admin-sso-after-${Date.now()}`;
			expect(await signInWithSso(after, email)).toBe(userId);
			expect(await listIdentities(userId)).toEqual([{provider_id: 'test', subject: after}]);
		});

		it('signs the account out when it unbinds it', async () => {
			const login = await completeSso(`itest-admin-sso-signout-${Date.now()}`, createUniqueEmail('sso-admin-signout'))
				.expect(200)
				.execute();
			await unlink(login.user_id).expect(200).execute();
			await createBuilder(harness, login.token).get('/users/@me').expect(401).execute();
		});

		it('requires the email update permission to bind or unbind', async () => {
			const viewer = await setUserACLs(harness, await createTestAccount(harness), [
				AdminACLs.AUTHENTICATE,
				AdminACLs.USER_LOOKUP,
			]);
			await createBuilder(harness, viewer.token)
				.delete(`/admin/users/${viewer.userId}/sso-identities`)
				.expect(403)
				.execute();
			await createBuilder(harness, viewer.token)
				.post('/admin/sso-identities/migrate')
				.body({from_provider_id: 'test-old'})
				.expect(403)
				.execute();
		});
	});
});
