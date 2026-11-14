// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {
	createAuthHarness,
	createTestAccount,
	type LoginMfaResponse,
	loginUser,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	type BridgeNonce,
	createBridgeNonce,
	LEGACY_ORIGIN,
	LEGACY_RP_ID,
	registerPasskey,
	runNativeSudoBridge,
	setDomainMigration,
	TARGET_ORIGIN,
} from '@app/api/auth/tests/PasskeyTestUtils';
import {
	createAuthenticationResponse,
	createTotpSecret,
	createWebAuthnDevice,
	generateTotpCode,
	setWebAuthnTwoFactor,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {getConfig} from '@app/api/Config';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	PasskeyBridgeFinishResponse,
	PasskeyBridgeLoginRedeemResponse,
	PasskeyBridgeStartResponse,
	PasskeyBridgeSudoRedeemResponse,
} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const SUDO_MODE_HEADER = 'X-Fluxer-Sudo-Mode-JWT';

interface StartedBridge {
	ceremonyId: string;
	bridgeUrl: string | null;
	nonce: BridgeNonce;
}

describe('Passkey bridge', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await setDomainMigration(true);
	});

	afterEach(() => {
		getConfig().instance.selfHosted = false;
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createLegacyAccount(): Promise<{account: TestAccount; device: WebAuthnDevice}> {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerPasskey(harness, account.token, device, {password: account.password}, 'Old');
		return {account, device};
	}

	async function startLogin(body: Record<string, unknown> = {}, origin = TARGET_ORIGIN): Promise<StartedBridge> {
		const nonce = createBridgeNonce();
		const start = await createBuilderWithoutAuth<PasskeyBridgeStartResponse>(harness)
			.post('/auth/passkey-bridge')
			.header('origin', origin)
			.body({purpose: 'login', runner: 'page', nonce_hash: nonce.nonceHash, ...body})
			.execute();
		return {ceremonyId: start.ceremony_id, bridgeUrl: start.bridge_url, nonce};
	}

	async function startSudo(token: string, runner: 'page' | 'native' = 'page'): Promise<StartedBridge> {
		const nonce = createBridgeNonce();
		const start = await createBuilder<PasskeyBridgeStartResponse>(harness, token)
			.post('/users/@me/passkey-bridge')
			.header('origin', TARGET_ORIGIN)
			.body({runner, nonce_hash: nonce.nonceHash})
			.execute();
		return {ceremonyId: start.ceremony_id, bridgeUrl: start.bridge_url, nonce};
	}

	async function fetchOptions(ceremonyId: string, origin = LEGACY_ORIGIN): Promise<WebAuthnAuthenticationOptions> {
		const {options} = await createBuilderWithoutAuth<{options: WebAuthnAuthenticationOptions}>(harness)
			.post(`/auth/passkey-bridge/${ceremonyId}/options`)
			.header('origin', origin)
			.execute();
		return options;
	}

	async function complete(
		ceremonyId: string,
		device: WebAuthnDevice,
		origin = LEGACY_ORIGIN,
	): Promise<PasskeyBridgeFinishResponse> {
		const options = await fetchOptions(ceremonyId, origin);
		return createBuilderWithoutAuth<PasskeyBridgeFinishResponse>(harness)
			.post(`/auth/passkey-bridge/${ceremonyId}/complete`)
			.header('origin', origin)
			.body({response: createAuthenticationResponse(device, options)})
			.execute();
	}

	function completionCodeFrom(finish: PasskeyBridgeFinishResponse, ceremonyId: string): string {
		const url = new URL(finish.return_url!);
		const [id, code] = url.hash.slice('#passkey-bridge='.length).split('.');
		expect(id).toBe(ceremonyId);
		return code;
	}

	function redeemLogin(ceremonyId: string, nonce: string, completionCode: string) {
		return createBuilderWithoutAuth<PasskeyBridgeLoginRedeemResponse>(harness)
			.post(`/auth/passkey-bridge/${ceremonyId}/redeem`)
			.header('origin', TARGET_ORIGIN)
			.body({nonce, completion_code: completionCode});
	}

	it('refuses to start outside the new origin and on a self-hosted instance, whatever the switch', async () => {
		const nonce = createBridgeNonce();
		const body = {purpose: 'login', runner: 'native', nonce_hash: nonce.nonceHash};
		for (const origin of [LEGACY_ORIGIN, 'https://evil.example']) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/passkey-bridge')
				.header('origin', origin)
				.body(body)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/passkey-bridge')
			.body(body)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
			.execute();
		getConfig().instance.selfHosted = true;
		await createBuilderWithoutAuth(harness)
			.post('/auth/passkey-bridge')
			.header('origin', TARGET_ORIGIN)
			.body(body)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
			.execute();
		getConfig().instance.selfHosted = false;
		await setDomainMigration(false);
		await createBuilderWithoutAuth(harness)
			.post('/auth/passkey-bridge')
			.header('origin', TARGET_ORIGIN)
			.body(body)
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	it('runs the ceremony only on the paired origin and keeps going when the switch goes off', async () => {
		const {device} = await createLegacyAccount();
		const started = await startLogin();
		expect(started.bridgeUrl).toBe(`${LEGACY_ORIGIN}/passkey-bridge#${started.ceremonyId}`);
		await createBuilderWithoutAuth(harness)
			.post(`/auth/passkey-bridge/${started.ceremonyId}/options`)
			.header('origin', TARGET_ORIGIN)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.INVALID_API_ORIGIN)
			.execute();
		const options = await fetchOptions(started.ceremonyId);
		expect(options.rpId).toBe(LEGACY_RP_ID);
		expect(options.allowCredentials).toBeUndefined();
		expect(options.userVerification).toBe('required');
		await setDomainMigration(false);
		await createBuilderWithoutAuth(harness)
			.post(`/auth/passkey-bridge/${started.ceremonyId}/complete`)
			.header('origin', LEGACY_ORIGIN)
			.body({response: createAuthenticationResponse(device, options)})
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	it('signs in through a page ceremony and always returns to the bridge page', async () => {
		const {account, device} = await createLegacyAccount();
		const started = await startLogin({
			return_path: '/api/v1/oauth2/authorize?prompt=none&redirect_uri=https://evil.example/cb',
		});
		const finish = await complete(started.ceremonyId, device);
		expect(finish.completion_code).toBeNull();
		const returnUrl = new URL(finish.return_url!);
		expect(`${returnUrl.origin}${returnUrl.pathname}${returnUrl.search}`).toBe(`${TARGET_ORIGIN}/passkey-bridge`);
		const code = completionCodeFrom(finish, started.ceremonyId);
		const redeemed = await redeemLogin(started.ceremonyId, started.nonce.nonce, code).execute();
		expect(redeemed.status).toBe('completed');
		if (redeemed.status !== 'completed') return;
		expect(redeemed.user_id).toBe(account.userId);
		const me = await createBuilder<{id: string}>(harness, redeemed.token).get('/users/@me').execute();
		expect(me.id).toBe(account.userId);
		await redeemLogin(started.ceremonyId, started.nonce.nonce, code)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();
	});

	it('needs both the nonce and the completion code', async () => {
		const {device} = await createLegacyAccount();
		const started = await startLogin();
		const code = completionCodeFrom(await complete(started.ceremonyId, device), started.ceremonyId);
		const attacker = createBridgeNonce();
		await redeemLogin(started.ceremonyId, attacker.nonce, code)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_PASSKEY_BRIDGE_NONCE)
			.execute();
		await redeemLogin(started.ceremonyId, started.nonce.nonce, code)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();

		const second = await startLogin();
		completionCodeFrom(await complete(second.ceremonyId, device), second.ceremonyId);
		await redeemLogin(second.ceremonyId, second.nonce.nonce, 'A'.repeat(43))
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_PASSKEY_BRIDGE_NONCE)
			.execute();
		await redeemLogin(second.ceremonyId, second.nonce.nonce, 'A'.repeat(43))
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();
	});

	it('keeps a pending ceremony when redeemed early or verification fails', async () => {
		const {account, device} = await createLegacyAccount();
		const target = createWebAuthnDevice();
		await registerPasskey(harness, account.token, target, {password: account.password}, 'New', TARGET_ORIGIN);
		const started = await startLogin();
		await redeemLogin(started.ceremonyId, started.nonce.nonce, 'A'.repeat(43))
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();
		const options = await fetchOptions(started.ceremonyId);
		await createBuilderWithoutAuth(harness)
			.post(`/auth/passkey-bridge/${started.ceremonyId}/complete`)
			.header('origin', LEGACY_ORIGIN)
			.body({response: createAuthenticationResponse(target, options)})
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED)
			.execute();
		const code = completionCodeFrom(await complete(started.ceremonyId, device), started.ceremonyId);
		const redeemed = await redeemLogin(started.ceremonyId, started.nonce.nonce, code).execute();
		expect(redeemed.status).toBe('completed');
	});

	it('never lets a bridge challenge through the normal endpoints', async () => {
		const {device} = await createLegacyAccount();
		const started = await startLogin();
		const options = await fetchOptions(started.ceremonyId);
		await createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.header('origin', LEGACY_ORIGIN)
			.body({response: createAuthenticationResponse(device, options), challenge: options.challenge})
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.PASSKEY_AUTHENTICATION_FAILED)
			.execute();
	});

	it('reports a cancelled ceremony and refuses to cancel a completed one', async () => {
		const {device} = await createLegacyAccount();
		const started = await startLogin();
		const cancelled = await createBuilderWithoutAuth<PasskeyBridgeFinishResponse>(harness)
			.post(`/auth/passkey-bridge/${started.ceremonyId}/cancel`)
			.header('origin', LEGACY_ORIGIN)
			.execute();
		const code = completionCodeFrom(cancelled, started.ceremonyId);
		expect(await redeemLogin(started.ceremonyId, started.nonce.nonce, code).execute()).toEqual({status: 'cancelled'});

		const second = await startLogin();
		await complete(second.ceremonyId, device);
		await createBuilderWithoutAuth(harness)
			.post(`/auth/passkey-bridge/${second.ceremonyId}/cancel`)
			.header('origin', LEGACY_ORIGIN)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();
	});

	it('completes two-factor sign in for the ticket holder', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerPasskey(
			harness,
			account.token,
			device,
			{mfa_method: 'totp', mfa_code: generateTotpCode(secret)},
			'Old',
		);
		await setWebAuthnTwoFactor(harness, account.token, true, {mfa_method: 'totp', mfa_code: generateTotpCode(secret)});
		const login = (await loginUser(harness, {email: account.email, password: account.password})) as LoginMfaResponse;
		const started = await startLogin({purpose: 'login_mfa', ticket: login.ticket});
		const options = await fetchOptions(started.ceremonyId);
		expect(options.allowCredentials?.map((cred) => cred.id)).toEqual([device.credentialId.toString('base64url')]);
		expect(options.userVerification).toBe('discouraged');
		const code = completionCodeFrom(await complete(started.ceremonyId, device), started.ceremonyId);
		const redeemed = await redeemLogin(started.ceremonyId, started.nonce.nonce, code).execute();
		expect(redeemed.status).toBe('completed');
		await createBuilderWithoutAuth(harness)
			.post('/auth/login/mfa/totp')
			.body({code: generateTotpCode(secret), ticket: login.ticket})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('issues a sudo token that passes a sudo-protected route', async () => {
		const {account, device} = await createLegacyAccount();
		const credentialId = device.credentialId.toString('base64url');
		await createBuilder(harness, account.token)
			.patch(`/users/@me/mfa/webauthn/credentials/${credentialId}`)
			.body({name: 'Renamed'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const redeemed = await runNativeSudoBridge(harness, account.token, device);
		expect(redeemed.status).toBe('completed');
		if (redeemed.status !== 'completed') return;
		await createBuilder(harness, account.token)
			.patch(`/users/@me/mfa/webauthn/credentials/${credentialId}`)
			.header(SUDO_MODE_HEADER, redeemed.sudo_token)
			.body({name: 'Renamed'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});

	it('returns sudo page ceremonies to the bridge page on the new origin', async () => {
		const {account, device} = await createLegacyAccount();
		const started = await startSudo(account.token);
		const finish = await complete(started.ceremonyId, device);
		const returnUrl = new URL(finish.return_url!);
		expect(`${returnUrl.origin}${returnUrl.pathname}`).toBe(`${TARGET_ORIGIN}/passkey-bridge`);
		const code = completionCodeFrom(finish, started.ceremonyId);
		const redeemed = await createBuilder<PasskeyBridgeSudoRedeemResponse>(harness, account.token)
			.post(`/users/@me/passkey-bridge/${started.ceremonyId}/redeem`)
			.header('origin', TARGET_ORIGIN)
			.body({nonce: started.nonce.nonce, completion_code: code})
			.execute();
		expect(redeemed.status).toBe('completed');
	});

	it('does not consume a ceremony redeemed on the wrong route or by another user', async () => {
		const {account, device} = await createLegacyAccount();
		const other = await createTestAccount(harness);
		const started = await startSudo(account.token, 'native');
		const options = await fetchOptions(started.ceremonyId, TARGET_ORIGIN);
		const finish = await createBuilderWithoutAuth<PasskeyBridgeFinishResponse>(harness)
			.post(`/auth/passkey-bridge/${started.ceremonyId}/complete`)
			.header('origin', TARGET_ORIGIN)
			.body({response: createAuthenticationResponse(device, options)})
			.execute();
		const body = {nonce: started.nonce.nonce, completion_code: finish.completion_code};
		await redeemLogin(started.ceremonyId, body.nonce, body.completion_code!)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();
		await createBuilder(harness, other.token)
			.post(`/users/@me/passkey-bridge/${started.ceremonyId}/redeem`)
			.header('origin', TARGET_ORIGIN)
			.body(body)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_BRIDGE)
			.execute();
		const redeemed = await createBuilder<PasskeyBridgeSudoRedeemResponse>(harness, account.token)
			.post(`/users/@me/passkey-bridge/${started.ceremonyId}/redeem`)
			.header('origin', TARGET_ORIGIN)
			.body(body)
			.execute();
		expect(redeemed.status).toBe('completed');
	});

	it('always stores the ceremony with an expiry', async () => {
		const {device} = await createLegacyAccount();
		const started = await startLogin();
		const key = `passkey_bridge:${createHash('sha256').update(started.ceremonyId).digest('hex')}`;
		const cache = getCacheService();
		const ttls = [await cache.ttl(key)];
		await fetchOptions(started.ceremonyId);
		ttls.push(await cache.ttl(key));
		await complete(started.ceremonyId, device);
		ttls.push(await cache.ttl(key));
		for (const ttl of ttls) {
			expect(ttl).toBeGreaterThan(0);
			expect(ttl).toBeLessThanOrEqual(600);
		}
	});
});
