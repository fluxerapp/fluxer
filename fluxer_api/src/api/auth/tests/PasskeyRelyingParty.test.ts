// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	LEGACY_ORIGIN,
	LEGACY_RP_ID,
	listPasskeys,
	registerPasskey,
	TARGET_ORIGIN,
	TARGET_RP_ID,
} from '@app/api/auth/tests/PasskeyTestUtils';
import {
	createAuthenticationResponse,
	createWebAuthnDevice,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

function credentialIdOf(device: WebAuthnDevice): string {
	return device.credentialId.toString('base64url');
}

describe('Passkey relying party selection', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterEach(() => {
		getConfig().instance.selfHosted = false;
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function registrationRpId(account: TestAccount, origin?: string): Promise<string> {
		const builder = createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({password: account.password});
		if (origin) builder.header('origin', origin);
		return (await builder.execute()).rp.id;
	}

	async function discoverableOptions(origin?: string): Promise<WebAuthnAuthenticationOptions> {
		const builder = createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/webauthn/authentication-options')
			.body(null);
		if (origin) builder.header('origin', origin);
		return builder.execute();
	}

	async function sudoOptions(token: string, origin?: string): Promise<WebAuthnAuthenticationOptions> {
		const builder = createBuilder<WebAuthnAuthenticationOptions>(harness, token)
			.post('/users/@me/sudo/webauthn/authentication-options')
			.body(null);
		if (origin) builder.header('origin', origin);
		return builder.execute();
	}

	async function createMixedAccount(): Promise<{account: TestAccount; legacy: WebAuthnDevice; target: WebAuthnDevice}> {
		const account = await createTestAccount(harness);
		const legacy = createWebAuthnDevice();
		const target = createWebAuthnDevice();
		await registerPasskey(harness, account.token, legacy, {password: account.password}, 'Old');
		await registerPasskey(harness, account.token, target, {password: account.password}, 'New', TARGET_ORIGIN);
		return {account, legacy, target};
	}

	it('uses the new relying party only for requests from the new origin', async () => {
		const account = await createTestAccount(harness);
		expect(await registrationRpId(account)).toBe(LEGACY_RP_ID);
		expect(await registrationRpId(account, LEGACY_ORIGIN)).toBe(LEGACY_RP_ID);
		expect(await registrationRpId(account, TARGET_ORIGIN)).toBe(TARGET_RP_ID);
		expect((await discoverableOptions()).rpId).toBe(LEGACY_RP_ID);
		expect((await discoverableOptions(LEGACY_ORIGIN)).rpId).toBe(LEGACY_RP_ID);
		expect((await discoverableOptions(TARGET_ORIGIN)).rpId).toBe(TARGET_RP_ID);
	});

	it('keeps the legacy relying party everywhere on a self-hosted instance', async () => {
		getConfig().instance.selfHosted = true;
		const account = await createTestAccount(harness);
		expect(await registrationRpId(account, TARGET_ORIGIN)).toBe(LEGACY_RP_ID);
		expect((await discoverableOptions(TARGET_ORIGIN)).rpId).toBe(LEGACY_RP_ID);
	});

	it('stores and exposes the relying party of each passkey', async () => {
		const {account, legacy, target} = await createMixedAccount();
		const credentials = await listPasskeys(harness, account.token);
		expect(credentials.map(({id, rp_id}) => ({id, rp_id}))).toEqual(
			expect.arrayContaining([
				{id: credentialIdOf(legacy), rp_id: LEGACY_RP_ID},
				{id: credentialIdOf(target), rp_id: TARGET_RP_ID},
			]),
		);
		const legacyRow = await getUserRepository().getWebAuthnCredential(
			createUserID(BigInt(account.userId)),
			credentialIdOf(legacy),
		);
		expect(legacyRow?.rpId).toBeNull();
	});

	it('keeps the legacy options unchanged for a legacy-only account off the new origin', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerPasskey(harness, account.token, device, {password: account.password}, 'Old');
		for (const origin of [undefined, LEGACY_ORIGIN]) {
			const options = await sudoOptions(account.token, origin);
			expect(options.rpId).toBe(LEGACY_RP_ID);
			expect(options.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(device)]);
			expect(options.userVerification).toBe('discouraged');
		}
	});

	it('offers one relying party group per request', async () => {
		const {account, legacy, target} = await createMixedAccount();
		const onTarget = await sudoOptions(account.token, TARGET_ORIGIN);
		expect(onTarget.rpId).toBe(TARGET_RP_ID);
		expect(onTarget.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(target)]);
		const offTarget = await sudoOptions(account.token);
		expect(offTarget.rpId).toBe(LEGACY_RP_ID);
		expect(offTarget.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(legacy)]);
	});

	it('falls back to the other group when the preferred one is empty', async () => {
		const legacyOnly = await createTestAccount(harness);
		const legacy = createWebAuthnDevice();
		await registerPasskey(harness, legacyOnly.token, legacy, {password: legacyOnly.password}, 'Old');
		expect((await sudoOptions(legacyOnly.token, TARGET_ORIGIN)).rpId).toBe(LEGACY_RP_ID);
		const targetOnly = await createTestAccount(harness);
		const target = createWebAuthnDevice();
		await registerPasskey(harness, targetOnly.token, target, {password: targetOnly.password}, 'New', TARGET_ORIGIN);
		expect((await sudoOptions(targetOnly.token)).rpId).toBe(TARGET_RP_ID);
	});

	it('rejects a passkey from another relying party before the test mode shortcut', async () => {
		const {legacy} = await createMixedAccount();
		const options = await discoverableOptions(TARGET_ORIGIN);
		await createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.header('origin', TARGET_ORIGIN)
			.body({response: createAuthenticationResponse(legacy, options), challenge: options.challenge})
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED)
			.execute();
	});

	it('rejects a passkey outside the offered list before the test mode shortcut', async () => {
		const account = await createTestAccount(harness);
		const visible = createWebAuthnDevice();
		const superseded = createWebAuthnDevice();
		await registerPasskey(harness, account.token, visible, {password: account.password}, 'Visible');
		await registerPasskey(harness, account.token, superseded, {password: account.password}, 'Superseded');
		await getUserRepository().setWebAuthnCredentialSupersededBy(
			createUserID(BigInt(account.userId)),
			credentialIdOf(superseded),
			credentialIdOf(visible),
		);
		const options = await sudoOptions(account.token, TARGET_ORIGIN);
		expect(options.rpId).toBe(LEGACY_RP_ID);
		expect(options.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(visible)]);
		await createBuilder(harness, account.token)
			.patch(`/users/@me/mfa/webauthn/credentials/${credentialIdOf(visible)}`)
			.header('origin', TARGET_ORIGIN)
			.body({
				name: 'Renamed',
				mfa_method: 'webauthn',
				webauthn_response: createAuthenticationResponse(superseded, options),
				webauthn_challenge: options.challenge,
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const retry = await sudoOptions(account.token, TARGET_ORIGIN);
		await createBuilder(harness, account.token)
			.patch(`/users/@me/mfa/webauthn/credentials/${credentialIdOf(visible)}`)
			.header('origin', TARGET_ORIGIN)
			.body({
				name: 'Renamed',
				mfa_method: 'webauthn',
				webauthn_response: createAuthenticationResponse(visible, retry),
				webauthn_challenge: retry.challenge,
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});

	it('accepts a superseded passkey only off the new origin', async () => {
		const account = await createTestAccount(harness);
		const legacy = createWebAuthnDevice();
		const target = createWebAuthnDevice();
		await registerPasskey(harness, account.token, legacy, {password: account.password}, 'Old');
		await registerPasskey(harness, account.token, target, {password: account.password}, 'New', TARGET_ORIGIN);
		await getUserRepository().setWebAuthnCredentialSupersededBy(
			createUserID(BigInt(account.userId)),
			credentialIdOf(legacy),
			credentialIdOf(target),
		);
		expect((await listPasskeys(harness, account.token)).map((cred) => cred.id)).toEqual([credentialIdOf(target)]);

		const offTarget = await discoverableOptions();
		await createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.body({response: createAuthenticationResponse(legacy, offTarget), challenge: offTarget.challenge})
			.expect(HTTP_STATUS.OK)
			.execute();
		const sudoOffTarget = await sudoOptions(account.token);
		expect(sudoOffTarget.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(legacy)]);

		const onTarget = await discoverableOptions(TARGET_ORIGIN);
		await createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.header('origin', TARGET_ORIGIN)
			.body({response: createAuthenticationResponse(legacy, onTarget), challenge: onTarget.challenge})
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED)
			.execute();
		const sudoOnTarget = await sudoOptions(account.token, TARGET_ORIGIN);
		expect(sudoOnTarget.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(target)]);
	});
});
