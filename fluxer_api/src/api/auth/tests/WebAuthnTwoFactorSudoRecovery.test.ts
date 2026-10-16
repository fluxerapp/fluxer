// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginUser, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createWebAuthnDevice,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
	type WebAuthnDevice,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {Config} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface PrivateUserResponse {
	id: string;
	mfa_enabled: boolean;
	authenticator_types: Array<number>;
}

interface SudoMfaMethodsResponse {
	totp: boolean;
	webauthn: boolean;
	backup_codes: boolean;
	has_mfa: boolean;
}

interface SudoModeRequiredResponse {
	code: string;
	has_mfa?: boolean;
	methods?: {
		totp?: boolean;
		webauthn?: boolean;
		backup_codes?: boolean;
	};
}

interface ValidationErrorBody {
	code: string;
	errors: Array<{path: string; code: string}>;
}

interface BackupCode {
	code: string;
	consumed: boolean;
}

async function withTotpReplayProtection<T>(run: () => Promise<T>): Promise<T> {
	const previous = Config.dev.testModeEnabled;
	Config.dev.testModeEnabled = false;
	try {
		return await run();
	} finally {
		Config.dev.testModeEnabled = previous;
	}
}

async function createPasskeyOnlyTwoFactorAccount(
	harness: ApiTestHarness,
): Promise<{account: TestAccount; device: WebAuthnDevice; backupCodes: Array<string>}> {
	const account = await createTestAccount(harness);
	const device = createWebAuthnDevice();
	await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
	const enabled = await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
	expect(enabled.user.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
	expect(enabled.backup_codes).not.toBeNull();
	return {account, device, backupCodes: enabled.backup_codes!.map((backupCode) => backupCode.code)};
}

async function fetchMe(harness: ApiTestHarness, token: string): Promise<PrivateUserResponse> {
	return createBuilder<PrivateUserResponse>(harness, token).get('/users/@me').execute();
}

describe('Sudo mode recovery for passkey two-factor accounts without TOTP', () => {
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
	it('turns passkey two-factor off with a backup code when the passkey cannot be used', async () => {
		const {account, backupCodes} = await createPasskeyOnlyTwoFactorAccount(harness);
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false, password: account.password})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		await withTotpReplayProtection(async () => {
			const disabled = await createBuilder<{user: PrivateUserResponse}>(harness, account.token)
				.put('/users/@me/mfa/webauthn/two-factor')
				.body({enabled: false, mfa_method: 'totp', mfa_code: backupCodes[0]!})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(disabled.user.authenticator_types).toEqual([]);
			expect(disabled.user.mfa_enabled).toBe(false);
		});
		const me = await fetchMe(harness, account.token);
		expect(me.authenticator_types).toEqual([]);
		expect(me.mfa_enabled).toBe(false);
		const login = await loginUser(harness, {email: account.email, password: account.password});
		expect('mfa' in login).toBe(false);
	});
	it('advertises the backup code option in the sudo methods endpoint and the sudo mode challenge alike', async () => {
		const {account} = await createPasskeyOnlyTwoFactorAccount(harness);
		const methods = await createBuilder<SudoMfaMethodsResponse>(harness, account.token)
			.get('/users/@me/sudo/mfa-methods')
			.execute();
		expect(methods).toEqual({totp: false, webauthn: true, backup_codes: true, has_mfa: true});
		const challenge = await createBuilder<SudoModeRequiredResponse>(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(challenge.has_mfa).toBe(methods.has_mfa);
		expect(challenge.methods).toEqual({
			totp: methods.totp,
			webauthn: methods.webauthn,
			backup_codes: methods.backup_codes,
		});
	});
	it('spends a backup code accepted as a sudo proof and refuses the same code afterwards', async () => {
		const {account, backupCodes} = await createPasskeyOnlyTwoFactorAccount(harness);
		const spent = backupCodes[0]!;
		await withTotpReplayProtection(async () => {
			const stored = await createBuilder<{backup_codes: Array<BackupCode>}>(harness, account.token)
				.post('/users/@me/mfa/backup-codes')
				.body({regenerate: false, mfa_method: 'totp', mfa_code: spent})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(stored.backup_codes.find((backupCode) => backupCode.code === spent)?.consumed).toBe(true);
			const error = await createBuilder<ValidationErrorBody>(harness, account.token)
				.put('/users/@me/mfa/webauthn/two-factor')
				.body({enabled: false, mfa_method: 'totp', mfa_code: spent})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(error.errors[0]?.path).toBe('mfa_code');
			expect(error.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_MFA_CODE);
		});
		const me = await fetchMe(harness, account.token);
		expect(me.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
	});
	it('rejects a backup code that was never minted and leaves passkey two-factor on', async () => {
		const {account} = await createPasskeyOnlyTwoFactorAccount(harness);
		await withTotpReplayProtection(async () => {
			const error = await createBuilder<ValidationErrorBody>(harness, account.token)
				.put('/users/@me/mfa/webauthn/two-factor')
				.body({enabled: false, mfa_method: 'totp', mfa_code: 'aaaa-bbbb'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(error.errors[0]?.path).toBe('mfa_code');
			expect(error.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_MFA_CODE);
		});
		const me = await fetchMe(harness, account.token);
		expect(me.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
	});
	it('rejects a code-entry sudo proof for a passkey account that holds neither TOTP nor backup codes', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		const methods = await createBuilder<SudoMfaMethodsResponse>(harness, account.token)
			.get('/users/@me/sudo/mfa-methods')
			.execute();
		expect(methods).toEqual({totp: false, webauthn: true, backup_codes: false, has_mfa: true});
		await withTotpReplayProtection(async () => {
			const error = await createBuilder<ValidationErrorBody>(harness, account.token)
				.post('/users/@me/disable')
				.body({mfa_method: 'totp', mfa_code: 'aaaa-bbbb'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(error.errors[0]?.path).toBe('mfa_code');
			expect(error.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_MFA_CODE);
		});
	});
});
