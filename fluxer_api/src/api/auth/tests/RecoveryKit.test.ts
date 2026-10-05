// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {resetPwnedPasswordCacheForTesting} from '@app/api/auth/AuthPassword';
import {findRecoveryKitCreatedAt} from '@app/api/auth/AuthRecoveryKit';
import {RecoveryKitRepository} from '@app/api/auth/services/RecoveryKitRepository';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueUsername,
	type TestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {
	getAdminRepository,
	getInstanceConfigRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const NEW_PASSWORD = 'recovered-strong-password-123';
const RECOVERY_KEY_PATTERN = /^[0-9A-HJKMNP-TV-Z]{4}(-[0-9A-HJKMNP-TV-Z]{4}){7}$/;

interface ValidationErrorBody {
	code: string;
	errors: Array<{path: string; code: string}>;
}

interface RecoveryKitStatus {
	has_recovery_kit: boolean;
	created_at: string | null;
}

interface RecoveryKitCreated {
	recovery_key: string;
	created_at: string;
}

interface RecoverTokenResponse {
	token: string;
	user_id: string;
	recovery_key: string;
	recovery_kit_created_at: string;
}

interface RecoverMfaResponse {
	mfa: true;
	ticket: string;
	totp: boolean;
	recovery_key: string;
	recovery_kit_created_at: string;
}

describe('Recovery kit', () => {
	let harness: ApiTestHarness;
	let originalSelfHosted: boolean;
	let originalBreachedPasswordCheck: boolean;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		const config = getConfig();
		originalSelfHosted = config.instance.selfHosted;
		originalBreachedPasswordCheck = config.breachedPasswordCheck.enabled;
		config.instance.selfHosted = true;
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
	});
	afterEach(() => {
		const config = getConfig();
		config.instance.selfHosted = originalSelfHosted;
		config.breachedPasswordCheck.enabled = originalBreachedPasswordCheck;
		resetPwnedPasswordCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createAccount(): Promise<TestAccount & {username: string}> {
		const config = getConfig();
		config.instance.selfHosted = false;
		try {
			const username = createUniqueUsername('kit');
			const account = await createTestAccount(harness, {username});
			return {...account, username};
		} finally {
			config.instance.selfHosted = true;
		}
	}

	async function createKit(account: TestAccount, password = account.password): Promise<RecoveryKitCreated> {
		return await createBuilder<RecoveryKitCreated>(harness, account.token)
			.post('/users/@me/recovery-kit')
			.body({password})
			.execute();
	}

	async function getStatus(account: TestAccount): Promise<RecoveryKitStatus> {
		return await createBuilder<RecoveryKitStatus>(harness, account.token).get('/users/@me/recovery-kit').execute();
	}

	async function expectInvalidRecoveryKey(body: Record<string, unknown>): Promise<void> {
		const {json} = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
			.post('/auth/recover')
			.body(body)
			.expect(400, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(errorFields(json)).toEqual([{path: 'recovery_key', code: ValidationErrorCodes.INVALID_RECOVERY_KEY}]);
	}

	function errorFields(body: ValidationErrorBody): Array<{path: string; code: string}> {
		return body.errors.map(({path, code}) => ({path, code}));
	}

	function sloppyKey(key: string): string {
		return key.replace(/-/g, ' ').replace(/0/g, 'o').replace(/1/g, 'l').toLowerCase();
	}

	it('reports no kit, creates one behind sudo and reports it', async () => {
		const account = await createAccount();
		expect(await getStatus(account)).toEqual({has_recovery_kit: false, created_at: null});
		await createBuilder(harness, account.token)
			.post('/users/@me/recovery-kit')
			.body({})
			.expect(403, APIErrorCodes.SUDO_MODE_REQUIRED)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/recovery-kit')
			.body({password: 'not-the-right-password'})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(await getStatus(account)).toEqual({has_recovery_kit: false, created_at: null});
		const kit = await createKit(account);
		expect(kit.recovery_key).toMatch(RECOVERY_KEY_PATTERN);
		expect(await getStatus(account)).toEqual({has_recovery_kit: true, created_at: kit.created_at});
	});

	it('replaces the previous kit when a new one is created', async () => {
		const account = await createAccount();
		const first = await createKit(account);
		const second = await createKit(account);
		expect(second.recovery_key).not.toBe(first.recovery_key);
		await expectInvalidRecoveryKey({login: account.username, recovery_key: first.recovery_key, password: NEW_PASSWORD});
	});

	it('recovers the account, ends every session and rotates the kit', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const recovered = await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: sloppyKey(kit.recovery_key), password: NEW_PASSWORD})
			.execute();
		expect(recovered.user_id).toBe(account.userId);
		expect(recovered.token).toBeTruthy();
		expect(recovered.recovery_key).toMatch(RECOVERY_KEY_PATTERN);
		expect(recovered.recovery_key).not.toBe(kit.recovery_key);
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
		const recoveredAccount = {...account, token: recovered.token, password: NEW_PASSWORD};
		expect(await getStatus(recoveredAccount)).toEqual({
			has_recovery_kit: true,
			created_at: recovered.recovery_kit_created_at,
		});
		await expectInvalidRecoveryKey({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD});
		await createBuilder(harness, recovered.token)
			.post('/users/@me/recovery-kit')
			.body({password: account.password})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		await createKit(recoveredAccount);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({login: account.username, password: account.password})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		const login = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/login')
			.body({login: account.username, password: NEW_PASSWORD})
			.execute();
		expect(login.user_id).toBe(account.userId);
	});

	it('accepts the full tag and the new key works for the next recovery', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const me = await createBuilder<{discriminator: string}>(harness, account.token).get('/users/@me').execute();
		const tag = `${account.username.toUpperCase()}#${me.discriminator}`;
		const first = await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.body({login: tag, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		const second = await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: first.recovery_key, password: `${NEW_PASSWORD}-again`})
			.execute();
		expect(second.user_id).toBe(account.userId);
		await expectInvalidRecoveryKey({
			login: `${account.username}#${me.discriminator === '9999' ? '0001' : '9999'}`,
			recovery_key: second.recovery_key,
			password: NEW_PASSWORD,
		});
	});

	it('gives the same error for a wrong key, a malformed key, an unknown user and a missing kit', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const otherKit = await createKit(await createAccount());
		await expectInvalidRecoveryKey({
			login: account.username,
			recovery_key: otherKit.recovery_key,
			password: NEW_PASSWORD,
		});
		await expectInvalidRecoveryKey({
			login: account.username,
			recovery_key: kit.recovery_key.replace(/^./, 'U'),
			password: NEW_PASSWORD,
		});
		await expectInvalidRecoveryKey({login: account.username, recovery_key: 'not-a-key', password: NEW_PASSWORD});
		await expectInvalidRecoveryKey({
			login: createUniqueUsername('ghost'),
			recovery_key: kit.recovery_key,
			password: NEW_PASSWORD,
		});
		const noKit = await createAccount();
		await expectInvalidRecoveryKey({login: noKit.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD});
		expect(await getStatus(account)).toEqual({has_recovery_kit: true, created_at: kit.created_at});
	});

	it('refuses a breached password without using up the kit', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const breached = 'breached-password-for-recovery';
		const hash = crypto.createHash('sha1').update(breached).digest('hex').toUpperCase();
		getConfig().breachedPasswordCheck.enabled = true;
		resetPwnedPasswordCacheForTesting();
		server.use(
			http.get('https://api.pwnedpasswords.com/range/:prefix', () => HttpResponse.text(`${hash.slice(5)}:42`)),
		);
		const {json} = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: breached})
			.expect(400, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(errorFields(json)).toEqual([{path: 'password', code: ValidationErrorCodes.PASSWORD_IS_TOO_COMMON}]);
		expect(await getStatus(account)).toEqual({has_recovery_kit: true, created_at: kit.created_at});
		await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
	});

	async function enableTotp(account: TestAccount): Promise<string> {
		const secret = 'JBSWY3DPEHPK3PXP';
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.execute();
		return secret;
	}

	it('changes nothing until the second factor passes on an account with two-factor authentication', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const secret = await enableTotp(account);
		const recovered = await createBuilderWithoutAuth<RecoverMfaResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		expect(recovered.mfa).toBe(true);
		expect(recovered.totp).toBe(true);
		expect(recovered.ticket).toBeTruthy();
		expect(recovered.recovery_key).toMatch(RECOVERY_KEY_PATTERN);
		expect('token' in recovered).toBe(false);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		expect(await getStatus(account)).toEqual({has_recovery_kit: true, created_at: kit.created_at});
		const oldPasswordLogin = await createBuilderWithoutAuth<{mfa: boolean}>(harness)
			.post('/auth/login')
			.body({login: account.username, password: account.password})
			.execute();
		expect(oldPasswordLogin.mfa).toBe(true);
		const login = await createBuilderWithoutAuth<{token: string; user_id: string}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: recovered.ticket, code: totpCodeNow(secret)})
			.execute();
		expect(login.user_id).toBe(account.userId);
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
		const recoveredAccount = {...account, token: login.token, password: NEW_PASSWORD};
		expect(await getStatus(recoveredAccount)).toEqual({
			has_recovery_kit: true,
			created_at: recovered.recovery_kit_created_at,
		});
		await expectInvalidRecoveryKey({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD});
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({login: account.username, password: account.password})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		const newPasswordLogin = await createBuilderWithoutAuth<{mfa: boolean}>(harness)
			.post('/auth/login')
			.body({login: account.username, password: NEW_PASSWORD})
			.execute();
		expect(newPasswordLogin.mfa).toBe(true);
		const next = await createBuilderWithoutAuth<RecoverMfaResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: recovered.recovery_key, password: `${NEW_PASSWORD}-again`})
			.execute();
		expect(next.mfa).toBe(true);
	});

	it('keeps the old kit working when the second factor is never given', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		await enableTotp(account);
		const abandoned = await createBuilderWithoutAuth<RecoverMfaResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		await expectInvalidRecoveryKey({
			login: account.username,
			recovery_key: abandoned.recovery_key,
			password: NEW_PASSWORD,
		});
		const retried = await createBuilderWithoutAuth<RecoverMfaResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		expect(retried.mfa).toBe(true);
		expect(await getStatus(account)).toEqual({has_recovery_kit: true, created_at: kit.created_at});
	});

	it('refuses a second factor on a stale recovery once the kit changed', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const secret = await enableTotp(account);
		const recovered = await createBuilderWithoutAuth<RecoverMfaResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		const replacement = await createBuilder<RecoveryKitCreated>(harness, account.token)
			.post('/users/@me/recovery-kit')
			.body({mfa_method: 'totp', mfa_code: totpCodeNow(secret)})
			.execute();
		const {json} = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: recovered.ticket, code: totpCodeNow(secret)})
			.expect(400, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expect(errorFields(json)).toEqual([{path: 'recovery_key', code: ValidationErrorCodes.INVALID_RECOVERY_KEY}]);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		expect(await getStatus(account)).toEqual({has_recovery_kit: true, created_at: replacement.created_at});
	});

	it('puts the old kit back when the reset fails after the kit was rotated', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const users = getUserRepository();
		const userId = createUserID(BigInt(account.userId));
		const user = await users.findUniqueAssert(userId);
		await users.patchUpsert(userId, {traits: new Set(['registration_pending_approval'])}, user.toRow());
		await createBuilderWithoutAuth(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.expect(403)
			.execute();
		expect((await findRecoveryKitCreatedAt(userId))?.toISOString()).toBe(kit.created_at);
	});

	it('ends every outstanding admin reset link', async () => {
		const admin = await createAccount();
		await createBuilder(harness, '')
			.post(`/test/users/${admin.userId}/acls`)
			.body({acls: [AdminACLs.WILDCARD]})
			.execute();
		const account = await createAccount();
		const link = await createBuilder<{url: string}>(harness, admin.token)
			.post(`/admin/users/${account.userId}/password-reset-link`)
			.execute();
		const token = link.url.split('/reset#token=')[1]!;
		const kit = await createKit(account);
		await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		const validation = await createBuilderWithoutAuth<{valid: boolean}>(harness).get(`/auth/reset/${token}`).execute();
		expect(validation.valid).toBe(false);
	});

	it('lifts an expired temporary ban and sets the password on the lifted account', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const users = getUserRepository();
		const userId = createUserID(BigInt(account.userId));
		const user = await users.findUniqueAssert(userId);
		await users.patchUpsert(
			userId,
			{flags: user.flags | UserFlags.DISABLED, temp_banned_until: new Date(Date.now() - 60_000)},
			user.toRow(),
		);
		await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		const after = await users.findUniqueAssert(userId);
		expect(after.flags & UserFlags.DISABLED).toBe(0n);
		expect(after.tempBannedUntil).toBeNull();
		expect(after.passwordLastChangedAt).not.toBeNull();
	});

	it('reports when the kit was created for the data export', async () => {
		const account = await createAccount();
		const userId = createUserID(BigInt(account.userId));
		expect(await findRecoveryKitCreatedAt(userId)).toBeNull();
		const kit = await createKit(account);
		expect((await findRecoveryKitCreatedAt(userId))?.toISOString()).toBe(kit.created_at);
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
		expect(await findRecoveryKitCreatedAt(userId)).toBeNull();
	});

	it('limits attempts per account from one source without locking out others', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const wrongKit = await createKit(await createAccount());
		for (let attempt = 0; attempt < 5; attempt++) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/recover')
				.header('x-forwarded-for', '198.51.100.20')
				.body({login: account.username, recovery_key: wrongKit.recovery_key, password: NEW_PASSWORD})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/recover')
			.header('x-forwarded-for', '198.51.100.20')
			.body({login: account.username.toUpperCase(), recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.expect(429)
			.execute();
		await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.header('x-forwarded-for', '198.51.100.21')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
	});

	it('keeps identifiers that are not usernames out of every named bucket', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		for (let attempt = 0; attempt < 5; attempt++) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/recover')
				.header('x-forwarded-for', '198.51.100.25')
				.body({
					login: `${account.username}:198.51.100.${attempt}`,
					recovery_key: kit.recovery_key,
					password: NEW_PASSWORD,
				})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/recover')
			.header('x-forwarded-for', '198.51.100.25')
			.body({login: 'not a username', recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.expect(429)
			.execute();
		await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.header('x-forwarded-for', '198.51.100.25')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
	});

	it('limits attempts per IP address', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		for (let attempt = 0; attempt < 10; attempt++) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/recover')
				.header('x-forwarded-for', '198.51.100.30')
				.body({login: createUniqueUsername('ghost'), recovery_key: kit.recovery_key, password: NEW_PASSWORD})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/recover')
			.header('x-forwarded-for', '198.51.100.30')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.expect(429)
			.execute();
		await createBuilderWithoutAuth<RecoverTokenResponse>(harness)
			.post('/auth/recover')
			.header('x-forwarded-for', '198.51.100.31')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
	});

	it('is refused with USERNAME_SIGN_IN_ONLY on email instances', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
		await assertUsernameSignInOnly(account, kit.recovery_key);
		getConfig().instance.selfHosted = false;
		await assertUsernameSignInOnly(account, kit.recovery_key);
	});

	async function createAdmin(acls: Array<string>): Promise<TestAccount> {
		const account = await createAccount();
		await createBuilder(harness, '').post(`/test/users/${account.userId}/acls`).body({acls}).execute();
		return account;
	}

	function hasKit(account: TestAccount): Promise<boolean> {
		return findRecoveryKitCreatedAt(createUserID(BigInt(account.userId))).then((createdAt) => createdAt !== null);
	}

	it('deletes the kit when the password changes', async () => {
		const account = await createAccount();
		await createKit(account);
		const changed = await createBuilder<{token: string}>(harness, account.token)
			.post('/users/@me/password')
			.body({password: account.password, new_password: NEW_PASSWORD})
			.execute();
		expect(await getStatus({...account, token: changed.token})).toEqual({has_recovery_kit: false, created_at: null});
	});

	it('deletes the kit when an admin creates a reset link and again when the link is used', async () => {
		const admin = await createAdmin([AdminACLs.WILDCARD]);
		const account = await createAccount();
		const kit = await createKit(account);
		const link = await createBuilder<{url: string}>(harness, admin.token)
			.post(`/admin/users/${account.userId}/password-reset-link`)
			.execute();
		expect(await hasKit(account)).toBe(false);
		await expectInvalidRecoveryKey({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD});
		await createKit(account);
		const users = getUserRepository();
		const userId = createUserID(BigInt(account.userId));
		const user = await users.findUniqueAssert(userId);
		await users.patchUpsert(userId, {email: null}, user.toRow());
		await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token: link.url.split('/reset#token=')[1]!, password: NEW_PASSWORD})
			.execute();
		expect(await hasKit(account)).toBe(false);
	});

	it('lets an admin revoke a kit and audits it', async () => {
		const admin = await createAdmin([AdminACLs.AUTHENTICATE, AdminACLs.USER_DELETE_RECOVERY_KIT]);
		const account = await createAccount();
		const kit = await createKit(account);
		await createBuilder(harness, admin.token)
			.delete(`/admin/users/${account.userId}/recovery-kit`)
			.expect(204)
			.execute();
		expect(await getStatus(account)).toEqual({has_recovery_kit: false, created_at: null});
		await expectInvalidRecoveryKey({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD});
		const audit = (await getAdminRepository().listAllAuditLogsPaginated(1000)).find(
			(log) => log.action === 'revoke_recovery_kit',
		);
		expect(audit?.targetId.toString()).toBe(account.userId);
		expect(audit?.adminUserId.toString()).toBe(admin.userId);
	});

	it('needs its own ACL to revoke a kit and refuses email instances', async () => {
		const admin = await createAdmin([AdminACLs.AUTHENTICATE, AdminACLs.USER_CREATE_PASSWORD_RESET_LINK]);
		const account = await createAccount();
		await createKit(account);
		await createBuilder(harness, admin.token)
			.delete(`/admin/users/${account.userId}/recovery-kit`)
			.expect(403, APIErrorCodes.MISSING_ACL)
			.execute();
		expect(await hasKit(account)).toBe(true);
		const wildcard = await createAdmin([AdminACLs.WILDCARD]);
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
		await createBuilder(harness, wildcard.token)
			.delete(`/admin/users/${account.userId}/recovery-kit`)
			.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
			.execute();
	});

	it('refuses a reset link or a revocation for an account holding ACLs the caller lacks', async () => {
		const admin = await createAdmin([
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_CREATE_PASSWORD_RESET_LINK,
			AdminACLs.USER_DELETE_RECOVERY_KIT,
		]);
		const target = await createAdmin([AdminACLs.AUTHENTICATE, AdminACLs.USER_UPDATE_EMAIL]);
		await createKit(target);
		await createBuilder(harness, admin.token)
			.post(`/admin/users/${target.userId}/password-reset-link`)
			.expect(403, APIErrorCodes.MISSING_ACL)
			.execute();
		await createBuilder(harness, admin.token)
			.delete(`/admin/users/${target.userId}/recovery-kit`)
			.expect(403, APIErrorCodes.MISSING_ACL)
			.execute();
		expect(await hasKit(target)).toBe(true);
		const peer = await createAdmin([AdminACLs.AUTHENTICATE]);
		await createBuilder(harness, admin.token).post(`/admin/users/${peer.userId}/password-reset-link`).execute();
		const wildcard = await createAdmin([AdminACLs.WILDCARD]);
		await createBuilder(harness, wildcard.token).post(`/admin/users/${target.userId}/password-reset-link`).execute();
	});

	it('refuses recovery for a registration awaiting approval or rejected', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const users = getUserRepository();
		const userId = createUserID(BigInt(account.userId));
		const changedBefore = (await users.findUniqueAssert(userId)).passwordLastChangedAt;
		for (const [trait, code] of [
			[REGISTRATION_PENDING_APPROVAL_TRAIT, APIErrorCodes.REGISTRATION_PENDING_APPROVAL],
			[REGISTRATION_REJECTED_TRAIT, APIErrorCodes.REGISTRATION_REJECTED],
		] as const) {
			const user = await users.findUniqueAssert(userId);
			await users.patchUpsert(userId, {traits: new Set([trait])}, user.toRow());
			await createBuilderWithoutAuth(harness)
				.post('/auth/recover')
				.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
				.expect(403, code)
				.execute();
		}
		expect((await findRecoveryKitCreatedAt(userId))?.toISOString()).toBe(kit.created_at);
		expect((await users.findUniqueAssert(userId)).passwordLastChangedAt).toEqual(changedBefore);
	});

	it('reads a kit row for an unknown name too', async () => {
		const find = vi.spyOn(RecoveryKitRepository.prototype, 'find');
		try {
			await expectInvalidRecoveryKey({
				login: createUniqueUsername('ghost'),
				recovery_key: 'ABCD-EFGH-JKMN-PQRS-TVWX-YZ01-2345-6789',
				password: NEW_PASSWORD,
			});
			expect(find).toHaveBeenCalledWith(createUserID(0n));
		} finally {
			find.mockRestore();
		}
	});

	it('ignores a pending recovery once the instance is no longer a username instance', async () => {
		const account = await createAccount();
		const kit = await createKit(account);
		const secret = await enableTotp(account);
		const userId = createUserID(BigInt(account.userId));
		const changedBefore = (await getUserRepository().findUniqueAssert(userId)).passwordLastChangedAt;
		const recovered = await createBuilderWithoutAuth<RecoverMfaResponse>(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: kit.recovery_key, password: NEW_PASSWORD})
			.execute();
		await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup');
		const login = await createBuilderWithoutAuth<{token: string; user_id: string}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: recovered.ticket, code: totpCodeNow(secret)})
			.execute();
		expect(login.user_id).toBe(account.userId);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		const user = await getUserRepository().findUniqueAssert(userId);
		expect(user.passwordLastChangedAt).toEqual(changedBefore);
	});

	async function assertUsernameSignInOnly(account: TestAccount & {username: string}, key: string): Promise<void> {
		await createBuilder(harness, account.token)
			.get('/users/@me/recovery-kit')
			.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/recovery-kit')
			.body({password: account.password})
			.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/recover')
			.body({login: account.username, recovery_key: key, password: NEW_PASSWORD})
			.expect(400, APIErrorCodes.USERNAME_SIGN_IN_ONLY)
			.execute();
	}
});
