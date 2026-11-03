// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	enableTotp,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {deleteOneOrMany} from '@app/api/database/CassandraQueryExecution';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {UserSsoIdentitiesByUser} from '@app/api/Tables';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {triggerDeletionWorker} from '@app/api/user/tests/UserTestUtils';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SsoStartResponse {
	state: string;
	client_binding: string;
}

interface SsoCompleteResponse {
	token: string;
	user_id: string;
}

interface ValidationErrorBody {
	code: string;
	message?: string;
	errors?: Array<{path: string; code: string}>;
}

interface CurrentUser {
	id: string;
	username: string;
	global_name: string | null;
	email: string | null;
}

const ISSUER = 'test';

describe('SSO on self-hosted instances', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const config = getConfig();
	let originalSelfHosted: boolean;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		originalSelfHosted = config.instance.selfHosted;
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'instance:config:update',
			'instance:config:view',
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

	async function startSso(): Promise<SsoStartResponse> {
		return createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body({}).execute();
	}

	async function completeSso<T = SsoCompleteResponse>(claims: Record<string, unknown>): Promise<T> {
		const {state, client_binding} = await startSso();
		return createBuilderWithoutAuth<T>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify(claims), state, client_binding})
			.execute();
	}

	async function completeSsoFailure(claims: Record<string, unknown>, status: number): Promise<ValidationErrorBody> {
		const {state, client_binding} = await startSso();
		const {json} = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify(claims), state, client_binding})
			.expect(status)
			.executeWithResponse();
		return json;
	}

	async function currentUser(token: string): Promise<CurrentUser> {
		return createBuilder<CurrentUser>(harness, token).get('/users/@me').execute();
	}

	function subject(label: string): string {
		return `itest-${label}-${Date.now()}-${Math.random().toString(36).slice(2)}`;
	}

	describe('registration modes', () => {
		it('provisions an SSO user while registration is closed', async () => {
			await enableSso(harness, admin.token);
			await getInstanceConfigRepository().setRegistrationConfig({mode: 'closed'});
			const login = await completeSso({email: createUniqueEmail('sso-closed'), sub: subject('closed')});
			expect(login.token).toBeTruthy();
			const me = await currentUser(login.token);
			expect(me.id).toBe(login.user_id);
		});

		it('still requires an existing account when auto-provisioning is off and registration is closed', async () => {
			await enableSso(harness, admin.token, {auto_provision: false});
			await getInstanceConfigRepository().setRegistrationConfig({mode: 'closed'});
			const body = await completeSsoFailure(
				{email: createUniqueEmail('sso-closed-noprov'), sub: subject('closed-noprov')},
				403,
			);
			expect(body.code).toBe('SSO_REQUIRED');
			expect(body.message).toBe(
				"This instance only lets existing accounts sign in with SSO, and there's no account for you yet. Ask an admin of this instance for access.",
			);
		});

		it('keeps approval mode in force for SSO users', async () => {
			await enableSso(harness, admin.token);
			await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});
			const body = await completeSsoFailure({email: createUniqueEmail('sso-approval'), sub: subject('approval')}, 403);
			expect(body.code).toBe('REGISTRATION_PENDING_APPROVAL');
		});
	});

	describe('email claims', () => {
		it('names an unverified provider email on email instances', async () => {
			await enableSso(harness, admin.token);
			const body = await completeSsoFailure(
				{email: createUniqueEmail('sso-unverified'), sub: subject('unverified'), email_verified: false},
				400,
			);
			expect(body.errors).toEqual([
				{path: 'email_verified', code: ValidationErrorCodes.SSO_EMAIL_NOT_VERIFIED, message: expect.any(String)},
			]);
		});

		it('requires an email on email instances', async () => {
			await enableSso(harness, admin.token, {allowed_domains: []});
			const body = await completeSsoFailure({sub: subject('no-email'), preferred_username: 'noemail'}, 400);
			expect(body.errors?.map(({path, code}) => ({path, code}))).toEqual([
				{path: 'email', code: ValidationErrorCodes.SSO_PROVIDER_DID_NOT_RETURN_EMAIL},
			]);
		});

		it('provisions without an email on username instances', async () => {
			await useUsernameMode();
			await enableSso(harness, admin.token, {allowed_domains: []});
			const login = await completeSso({sub: subject('username-no-email'), preferred_username: 'nomail'});
			const me = await currentUser(login.token);
			expect(me.username).toBe('nomail');
			expect(me.email).toBeNull();
		});

		it('accepts an unverified email on username instances without a domain list', async () => {
			await useUsernameMode();
			await enableSso(harness, admin.token, {allowed_domains: []});
			const sub = subject('username-unverified');
			const first = await completeSso({
				sub,
				email: createUniqueEmail('sso-username-unverified'),
				email_verified: false,
				preferred_username: 'unverified',
			});
			const second = await completeSso({sub, preferred_username: 'unverified'});
			expect(second.user_id).toBe(first.user_id);
		});

		it('still needs a verified email on username instances with a domain list', async () => {
			await useUsernameMode();
			await enableSso(harness, admin.token, {allowed_domains: ['example.com']});
			const missing = await completeSsoFailure({sub: subject('username-domain-missing')}, 400);
			expect(missing.errors?.map(({code}) => code)).toEqual([ValidationErrorCodes.SSO_PROVIDER_DID_NOT_RETURN_EMAIL]);
			const unverified = await completeSsoFailure(
				{sub: subject('username-domain-unverified'), email: createUniqueEmail('sso-domain'), email_verified: false},
				400,
			);
			expect(unverified.errors?.map(({code}) => code)).toEqual([ValidationErrorCodes.SSO_EMAIL_NOT_VERIFIED]);
		});
	});

	describe('deleted accounts', () => {
		async function deleteAccount(userId: string, reasonCode: number): Promise<void> {
			const users = getUserRepository();
			const user = await users.findUnique(createUserID(BigInt(userId)));
			if (!user) throw new Error('user missing');
			await users.updateDeletionSchedule(user, {
				pending_deletion_at: new Date(Date.now() - 1000),
				deletion_reason_code: reasonCode,
			});
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${userId}/set-pending-deletion`)
				.body({
					pending_deletion_at: new Date(Date.now() - 1000).toISOString(),
					set_self_deleted_flag: reasonCode === DeletionReasons.USER_REQUESTED,
				})
				.execute();
			await triggerDeletionWorker(harness);
			const deleted = await users.findUnique(createUserID(BigInt(userId)));
			expect(deleted?.pendingDeletionAt ?? null).toBeNull();
		}

		it('provisions a new account for the same subject after the person deletes theirs', async () => {
			await enableSso(harness, admin.token);
			const sub = subject('deleted-self');
			const email = createUniqueEmail('sso-deleted-self');
			const first = await completeSso({sub, email});
			await deleteAccount(first.user_id, DeletionReasons.USER_REQUESTED);
			expect(await new SsoIdentityRepository().findUserId(ISSUER, sub)).toBeNull();
			const second = await completeSso({sub, email});
			expect(second.user_id).not.toBe(first.user_id);
		});

		it('releases a mapping left pointing at an account deleted before the release existed', async () => {
			await enableSso(harness, admin.token);
			const sub = subject('deleted-stale');
			const email = createUniqueEmail('sso-deleted-stale');
			const first = await completeSso({sub, email});
			await deleteAccount(first.user_id, DeletionReasons.USER_REQUESTED);
			await new SsoIdentityRepository().tryClaimIdentity({
				providerId: ISSUER,
				subject: sub,
				userId: createUserID(BigInt(first.user_id)),
				claimedAt: new Date(),
			});
			const second = await completeSso({sub, email});
			expect(second.user_id).not.toBe(first.user_id);
		});

		it('keeps refusing a subject whose account was closed by enforcement while its email is banned', async () => {
			await enableSso(harness, admin.token);
			const sub = subject('deleted-enforced');
			const email = createUniqueEmail('sso-deleted-enforced');
			const first = await completeSso({sub, email});
			await deleteAccount(first.user_id, DeletionReasons.SPAM);
			await new AdminRepository().banEmail(email);
			expect((await new SsoIdentityRepository().findUserId(ISSUER, sub))?.toString()).toBe(first.user_id);
			const body = await completeSsoFailure({sub, email}, 403);
			expect(body.code).toBe('ACCOUNT_SUSPENDED_PERMANENTLY');
		});

		it('lets the subject back in to a new account once the email ban is lifted', async () => {
			await enableSso(harness, admin.token);
			const sub = subject('deleted-unbanned');
			const email = createUniqueEmail('sso-deleted-unbanned');
			const first = await completeSso({sub, email});
			await deleteAccount(first.user_id, DeletionReasons.SPAM);
			const bans = new AdminRepository();
			await bans.banEmail(email);
			await completeSsoFailure({sub, email}, 403);
			await bans.unbanEmail(email);
			const second = await completeSso({sub, email});
			expect(second.user_id).not.toBe(first.user_id);
			expect((await new SsoIdentityRepository().findUserId(ISSUER, sub))?.toString()).toBe(second.user_id);
		});

		it('refuses to connect a subject held by enforcement from settings on a username instance', async () => {
			const other = await createTestAccount(harness);
			await useUsernameMode();
			await enableSso(harness, admin.token, {allowed_domains: []});
			const sub = subject('deleted-link');
			const email = createUniqueEmail('sso-deleted-link');
			const first = await completeSso({sub, email, email_verified: true});
			await deleteAccount(first.user_id, DeletionReasons.SPAM);
			const {state, client_binding} = await createBuilder<SsoStartResponse>(harness, other.token)
				.post('/users/@me/sso/link/start')
				.body({password: other.password})
				.execute();
			const body = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
				.post('/auth/sso/complete')
				.body({code: JSON.stringify({sub, email, email_verified: true}), state, client_binding})
				.expect(400)
				.execute();
			expect(body.errors?.[0]?.code).toBe(ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT);
		});

		it.each([
			['bound now', false],
			['bound before the per-account index', true],
		])('lets a subject %s back in on a username instance once an admin releases it', async (_label, legacy) => {
			const staff = await setUserACLs(harness, await createTestAccount(harness), [
				'admin:authenticate',
				AdminACLs.USER_UPDATE_EMAIL,
			]);
			await useUsernameMode();
			await enableSso(harness, admin.token, {allowed_domains: []});
			const sub = subject('deleted-username');
			const first = await completeSso({sub, preferred_username: 'removed'});
			await deleteAccount(first.user_id, DeletionReasons.SPAM);
			expect((await completeSsoFailure({sub}, 403)).code).toBe('ACCOUNT_SUSPENDED_PERMANENTLY');
			if (legacy) {
				await deleteOneOrMany(
					UserSsoIdentitiesByUser.deleteByPk({
						user_id: createUserID(BigInt(first.user_id)),
						provider_id: ISSUER,
						subject: sub,
					}),
				);
			}
			await createBuilder(harness, staff.token)
				.delete(`/admin/users/${first.user_id}/sso-identities`)
				.expect(200)
				.execute();
			const second = await completeSso({sub});
			expect(second.user_id).not.toBe(first.user_id);
		});
	});

	describe('email while SSO is required', () => {
		it('takes a changed provider email on sign-in', async () => {
			await enableSso(harness, admin.token);
			const sub = subject('email-sync');
			const first = await completeSso({sub, email: createUniqueEmail('sso-email-before')});
			const changed = createUniqueEmail('sso-email-after');
			const second = await completeSso({sub, email: changed});
			expect(second.user_id).toBe(first.user_id);
			expect((await currentUser(second.token)).email).toBe(changed);
		});

		it('takes a changed provider email once the second factor is done', async () => {
			const account = await createTestAccount(harness);
			await enableSso(harness, admin.token);
			const sub = subject('email-mfa');
			await completeSso({sub, email: account.email});
			const [backupCode] = await enableTotp(harness, account);
			const changed = createUniqueEmail('sso-email-mfa');
			const {ticket} = await completeSso<{ticket: string}>({sub, email: changed});
			const login = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
				.post('/auth/login/mfa/totp')
				.body({ticket, code: backupCode})
				.execute();
			expect((await currentUser(login.token)).email).toBe(changed);
		});

		it('keeps the account email when SSO is optional', async () => {
			await enableSso(harness, admin.token, {enforced: false});
			const sub = subject('email-optional');
			const original = createUniqueEmail('sso-email-optional');
			await completeSso({sub, email: original});
			const second = await completeSso({sub, email: createUniqueEmail('sso-email-optional-after')});
			expect((await currentUser(second.token)).email).toBe(original);
		});

		it('keeps the account email when another account holds the provider email', async () => {
			const other = await createTestAccount(harness);
			await enableSso(harness, admin.token);
			const sub = subject('email-taken');
			const original = createUniqueEmail('sso-email-taken');
			await completeSso({sub, email: original});
			const second = await completeSso({sub, email: other.email});
			expect(second.user_id).not.toBe(other.userId);
			expect((await currentUser(second.token)).email).toBe(original);
		});

		it('reports the instance mode before the SSO lock on username instances', async () => {
			await useUsernameMode();
			await enableSso(harness, admin.token, {allowed_domains: []});
			const login = await completeSso({sub: subject('username-lock'), preferred_username: 'locked'});
			const {json} = await createBuilder<ValidationErrorBody>(harness, login.token)
				.post('/users/@me/email-change/start')
				.body({})
				.expect(400)
				.executeWithResponse();
			expect(json.code).toBe('EMAIL_UNAVAILABLE_ON_INSTANCE');
		});
	});
});
