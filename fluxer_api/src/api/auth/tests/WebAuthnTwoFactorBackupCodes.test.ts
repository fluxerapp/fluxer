// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	generateTotpCode,
	type LoginMfaResponse,
	loginUser,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createSudoWebAuthnBody,
	createWebAuthnDevice,
	registerWebAuthnCredential,
	type SudoVerificationBody,
	setWebAuthnTwoFactor,
	type WebAuthnCredentialMetadata,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BackupCode {
	code: string;
	consumed: boolean;
}

async function readBackupCodes(
	harness: ApiTestHarness,
	token: string,
	sudo: SudoVerificationBody,
): Promise<Array<BackupCode>> {
	const response = await createBuilder<{
		backup_codes: Array<BackupCode>;
	}>(harness, token)
		.post('/users/@me/mfa/backup-codes')
		.body({regenerate: false, ...sudo})
		.execute();
	return response.backup_codes;
}

describe('WebAuthn two-factor backup codes', () => {
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
	it('mints backup codes when passkey two-factor is turned on for an account with no TOTP', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		const enabled = await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		expect(enabled.backup_codes).not.toBeNull();
		expect(enabled.backup_codes!.length).toBeGreaterThan(0);
		expect(enabled.backup_codes!.every((backupCode) => !backupCode.consumed)).toBe(true);
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		const stored = await readBackupCodes(harness, account.token, sudoBody);
		expect(stored.map((backupCode) => backupCode.code).sort()).toEqual(
			enabled.backup_codes!.map((backupCode) => backupCode.code).sort(),
		);
	});
	it('mints nothing when the account already holds backup codes from TOTP', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		const enabled = await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		expect(enabled.backup_codes).toBeNull();
	});
	it('accepts a minted backup code at login when the passkey is unavailable', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		const enabled = await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		const login = (await loginUser(harness, {
			email: account.email,
			password: account.password,
		})) as LoginMfaResponse;
		expect(login.mfa).toBe(true);
		expect(login.totp).toBe(false);
		expect(login.webauthn).toBe(true);
		expect(login.backup_codes).toBe(true);
		expect(login.allowed_methods).toContain('backup_codes');
		const backupLogin = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: login.ticket, code: enabled.backup_codes![0]!.code})
			.execute();
		expect(backupLogin.token).toBeTruthy();
		const me = await createBuilder<{
			id: string;
		}>(harness, backupLogin.token)
			.get('/users/@me')
			.execute();
		expect(me.id).toBe(account.userId);
	});
	it('keeps backup codes when TOTP is disabled while passkey two-factor stays on', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: generateTotpCode(secret),
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		const stored = await readBackupCodes(harness, account.token, sudoBody);
		expect(stored.length).toBeGreaterThan(0);
	});
	it('keeps backup codes when passkey two-factor is turned off while TOTP stays on', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		await setWebAuthnTwoFactor(harness, account.token, false, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		const stored = await readBackupCodes(harness, account.token, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		expect(stored.length).toBeGreaterThan(0);
	});
	it('clears backup codes only once no second factor remains', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		await setWebAuthnTwoFactor(harness, account.token, false, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: generateTotpCode(secret),
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const stored = await readBackupCodes(harness, account.token, {password: account.password});
		expect(stored).toHaveLength(0);
	});
	it('clears backup codes when the last passkey is deleted and nothing else remains', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		const credentials = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		await createBuilder(harness, account.token)
			.delete(`/users/@me/mfa/webauthn/credentials/${credentials[0]!.id}`)
			.body(sudoBody)
			.expect(204)
			.execute();
		const stored = await readBackupCodes(harness, account.token, {password: account.password});
		expect(stored).toHaveLength(0);
	});
});
