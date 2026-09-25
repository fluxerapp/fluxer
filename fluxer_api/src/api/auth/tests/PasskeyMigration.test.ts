// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	LEGACY_RP_ID,
	listPasskeys,
	registerPasskey,
	runNativeSudoBridge,
	setDomainMigration,
	TARGET_ORIGIN,
	TARGET_RP_ID,
} from '@app/api/auth/tests/PasskeyTestUtils';
import {
	createAuthenticationResponse,
	createRegistrationResponse,
	createWebAuthnDevice,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {PasskeyMigrationResponse} from '@fluxer/schema/src/domains/auth/PasskeyMigrationSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MIGRATION_PATH = '/users/@me/mfa/webauthn/migration';
const MIGRATION_OPTIONS_PATH = '/users/@me/mfa/webauthn/migration/registration-options';

function credentialIdOf(device: WebAuthnDevice): string {
	return device.credentialId.toString('base64url');
}

interface RpcSessionResponse {
	data: {webauthn_credentials: Array<{id: string; rp_id: string}>};
}

describe('Passkey migration', () => {
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

	async function createAssignedAccount(): Promise<{account: TestAccount; legacy: WebAuthnDevice}> {
		const account = await createTestAccount(harness);
		const legacy = createWebAuthnDevice();
		await registerPasskey(harness, account.token, legacy, {password: account.password}, 'Laptop');
		await setDomainMigration(true, [account.userId]);
		return {account, legacy};
	}

	async function getPending(token: string): Promise<PasskeyMigrationResponse['pending']> {
		const response = await createBuilder<PasskeyMigrationResponse>(harness, token).get(MIGRATION_PATH).execute();
		return response.pending;
	}

	async function migrationOptions(token: string): Promise<WebAuthnRegistrationOptions> {
		return createBuilder<WebAuthnRegistrationOptions>(harness, token)
			.post(MIGRATION_OPTIONS_PATH)
			.header('origin', TARGET_ORIGIN)
			.execute();
	}

	function completeMigration(token: string, device: WebAuthnDevice, options: WebAuthnRegistrationOptions) {
		return createBuilder(harness, token)
			.post(MIGRATION_PATH)
			.header('origin', TARGET_ORIGIN)
			.body({response: createRegistrationResponse(device, options, 'Laptop'), challenge: options.challenge});
	}

	async function migrate(account: TestAccount, legacy: WebAuthnDevice): Promise<WebAuthnDevice> {
		await runNativeSudoBridge(harness, account.token, legacy);
		const target = createWebAuthnDevice();
		await completeMigration(account.token, target, await migrationOptions(account.token))
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		return target;
	}

	async function discoverableLogin(device: WebAuthnDevice, origin?: string, status: number = HTTP_STATUS.OK) {
		const optionsBuilder = createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/webauthn/authentication-options')
			.body(null);
		if (origin) optionsBuilder.header('origin', origin);
		const options = await optionsBuilder.execute();
		const builder = createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.body({response: createAuthenticationResponse(device, options), challenge: options.challenge})
			.expect(status);
		if (origin) builder.header('origin', origin);
		await builder.execute();
	}

	it('records a pending update for any account on the new origin while the switch is on', async () => {
		const unassigned = await createTestAccount(harness);
		const unassignedDevice = createWebAuthnDevice();
		await registerPasskey(harness, unassigned.token, unassignedDevice, {password: unassigned.password}, 'Laptop');
		await setDomainMigration(false);
		expect((await runNativeSudoBridge(harness, unassigned.token, unassignedDevice)).status).toBe('completed');
		expect(await getPending(unassigned.token)).toBeNull();

		await setDomainMigration(true);
		expect((await runNativeSudoBridge(harness, unassigned.token, unassignedDevice)).status).toBe('completed');
		expect(await getPending(unassigned.token)).toEqual({
			credential_id: credentialIdOf(unassignedDevice),
			name: 'Laptop',
			cross_device: false,
		});
	});

	it('needs a pending update and the new origin for registration options', async () => {
		const {account, legacy} = await createAssignedAccount();
		await createBuilder(harness, account.token)
			.post(MIGRATION_OPTIONS_PATH)
			.header('origin', TARGET_ORIGIN)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_MIGRATION)
			.execute();
		await runNativeSudoBridge(harness, account.token, legacy);
		await createBuilder(harness, account.token)
			.post(MIGRATION_OPTIONS_PATH)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_PASSKEY_MIGRATION)
			.execute();
		const options = await migrationOptions(account.token);
		expect(options.rp.id).toBe(TARGET_RP_ID);
	});

	it('replaces the passkey under the same name and hides the old one', async () => {
		const {account, legacy} = await createAssignedAccount();
		const target = await migrate(account, legacy);
		const credentials = await listPasskeys(harness, account.token);
		expect(credentials).toEqual([
			expect.objectContaining({id: credentialIdOf(target), name: 'Laptop', rp_id: TARGET_RP_ID}),
		]);
		const old = await getUserRepository().getWebAuthnCredential(
			createUserID(BigInt(account.userId)),
			credentialIdOf(legacy),
		);
		expect(old?.supersededBy).toBe(credentialIdOf(target));
		expect(await getPending(account.token)).toBeNull();
		const ready = await createBuilder<RpcSessionResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.execute();
		expect(ready.data.webauthn_credentials.map(({id, rp_id}) => ({id, rp_id}))).toEqual([
			{id: credentialIdOf(target), rp_id: TARGET_RP_ID},
		]);
	});

	it('keeps the old passkey working off the new origin', async () => {
		const {account, legacy} = await createAssignedAccount();
		const target = await migrate(account, legacy);
		await discoverableLogin(legacy);
		await discoverableLogin(legacy, TARGET_ORIGIN, HTTP_STATUS.UNAUTHORIZED);
		await discoverableLogin(target, TARGET_ORIGIN);
		await createBuilder(harness, account.token)
			.patch(`/users/@me/mfa/webauthn/credentials/${credentialIdOf(legacy)}`)
			.body({name: 'Renamed', password: account.password})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_WEBAUTHN_CREDENTIAL)
			.execute();
		const sudoOptions = await createBuilder<WebAuthnAuthenticationOptions>(harness, account.token)
			.post('/users/@me/sudo/webauthn/authentication-options')
			.body(null)
			.execute();
		expect(sudoOptions.rpId).toBe(LEGACY_RP_ID);
		expect(sudoOptions.allowCredentials?.map((cred) => cred.id)).toEqual([credentialIdOf(legacy)]);
	});

	it('removes the old passkey together with its replacement', async () => {
		const {account, legacy} = await createAssignedAccount();
		const target = await migrate(account, legacy);
		await createBuilder(harness, account.token)
			.delete(`/users/@me/mfa/webauthn/credentials/${credentialIdOf(target)}`)
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await getUserRepository().listWebAuthnCredentials(createUserID(BigInt(account.userId)))).toEqual([]);
		await discoverableLogin(legacy, undefined, HTTP_STATUS.UNAUTHORIZED);
	});

	it('removes every remaining superseded passkey with the last visible one', async () => {
		const account = await createTestAccount(harness);
		const orphan = createWebAuthnDevice();
		const visible = createWebAuthnDevice();
		await registerPasskey(harness, account.token, orphan, {password: account.password}, 'Orphan');
		await registerPasskey(harness, account.token, visible, {password: account.password}, 'Visible');
		const userId = createUserID(BigInt(account.userId));
		await getUserRepository().setWebAuthnCredentialSupersededBy(userId, credentialIdOf(orphan), 'gone');
		await createBuilder(harness, account.token)
			.delete(`/users/@me/mfa/webauthn/credentials/${credentialIdOf(visible)}`)
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await getUserRepository().listWebAuthnCredentials(userId)).toEqual([]);
	});

	it('has no way to attach the old passkey to another one', async () => {
		const {account, legacy} = await createAssignedAccount();
		await runNativeSudoBridge(harness, account.token, legacy);
		const target = createWebAuthnDevice();
		await registerPasskey(harness, account.token, target, {password: account.password}, 'Phone', TARGET_ORIGIN);
		await createBuilder(harness, account.token)
			.delete(MIGRATION_PATH)
			.header('origin', TARGET_ORIGIN)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		expect((await listPasskeys(harness, account.token)).map((cred) => cred.id).sort()).toEqual(
			[credentialIdOf(legacy), credentialIdOf(target)].sort(),
		);
	});

	it('never lets a migration challenge through the normal registration route', async () => {
		const {account, legacy} = await createAssignedAccount();
		await runNativeSudoBridge(harness, account.token, legacy);
		const options = await migrationOptions(account.token);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.header('origin', TARGET_ORIGIN)
			.body({
				response: createRegistrationResponse(createWebAuthnDevice(), options, 'Sneaky'),
				challenge: options.challenge,
				name: 'Sneaky',
			})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_WEBAUTHN_CREDENTIAL)
			.execute();
	});

	it('creates one credential when two updates race', async () => {
		const {account, legacy} = await createAssignedAccount();
		await runNativeSudoBridge(harness, account.token, legacy);
		const first = await migrationOptions(account.token);
		const second = await migrationOptions(account.token);
		const results = await Promise.all(
			[first, second].map((options) =>
				completeMigration(account.token, createWebAuthnDevice(), options)
					.expect(HTTP_STATUS.NO_CONTENT)
					.executeWithResponse()
					.then(
						() => 'ok',
						() => 'failed',
					),
			),
		);
		expect(results.sort()).toEqual(['failed', 'ok']);
		const credentials = await listPasskeys(harness, account.token);
		expect(credentials).toHaveLength(1);
		expect(credentials[0].rp_id).toBe(TARGET_RP_ID);
	});
});
