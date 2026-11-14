// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import {
	createAuthenticationResponse,
	createRegistrationResponse,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {DEFAULT_DOMAIN_MIGRATION_CONFIG} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import type {
	PasskeyBridgeFinishResponse,
	PasskeyBridgeStartResponse,
	PasskeyBridgeSudoRedeemResponse,
} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';

export const TARGET_ORIGIN = 'https://fluxer.com';
export const LEGACY_ORIGIN = 'https://web.fluxer.app';
export const LEGACY_RP_ID = 'localhost';
export const TARGET_RP_ID = 'fluxer.com';

export interface PasskeyCredentialListItem {
	id: string;
	name: string;
	rp_id: string;
}

export interface BridgeNonce {
	nonce: string;
	nonceHash: string;
}

export function createBridgeNonce(): BridgeNonce {
	const nonce = randomBytes(32).toString('base64url');
	return {nonce, nonceHash: createHash('sha256').update(nonce).digest('hex')};
}

export async function setDomainMigration(enabled: boolean, includedUserIds: Array<string> = []): Promise<void> {
	await getInstanceConfigRepository().setDomainMigrationConfig({
		...DEFAULT_DOMAIN_MIGRATION_CONFIG,
		enabled,
		included_user_ids: includedUserIds,
	});
}

export async function registerPasskey(
	harness: ApiTestHarness,
	token: string,
	device: WebAuthnDevice,
	sudo: Record<string, unknown>,
	name: string,
	origin?: string,
): Promise<void> {
	const optionsBuilder = createBuilder<WebAuthnRegistrationOptions>(harness, token)
		.post('/users/@me/mfa/webauthn/credentials/registration-options')
		.body(sudo);
	if (origin) optionsBuilder.header('origin', origin);
	const options = await optionsBuilder.execute();
	const registerBuilder = createBuilder(harness, token)
		.post('/users/@me/mfa/webauthn/credentials')
		.body({response: createRegistrationResponse(device, options, name), challenge: options.challenge, name})
		.expect(204);
	if (origin) registerBuilder.header('origin', origin);
	await registerBuilder.execute();
}

export async function listPasskeys(harness: ApiTestHarness, token: string): Promise<Array<PasskeyCredentialListItem>> {
	return createBuilder<Array<PasskeyCredentialListItem>>(harness, token)
		.get('/users/@me/mfa/webauthn/credentials')
		.execute();
}

export async function runNativeSudoBridge(
	harness: ApiTestHarness,
	token: string,
	device: WebAuthnDevice,
	nonce: BridgeNonce = createBridgeNonce(),
): Promise<PasskeyBridgeSudoRedeemResponse> {
	const start = await createBuilder<PasskeyBridgeStartResponse>(harness, token)
		.post('/users/@me/passkey-bridge')
		.header('origin', TARGET_ORIGIN)
		.body({runner: 'native', nonce_hash: nonce.nonceHash})
		.execute();
	const {options} = await createBuilderWithoutAuth<{options: WebAuthnAuthenticationOptions}>(harness)
		.post(`/auth/passkey-bridge/${start.ceremony_id}/options`)
		.header('origin', TARGET_ORIGIN)
		.execute();
	const finish = await createBuilderWithoutAuth<PasskeyBridgeFinishResponse>(harness)
		.post(`/auth/passkey-bridge/${start.ceremony_id}/complete`)
		.header('origin', TARGET_ORIGIN)
		.body({response: createAuthenticationResponse(device, options)})
		.execute();
	return createBuilder<PasskeyBridgeSudoRedeemResponse>(harness, token)
		.post(`/users/@me/passkey-bridge/${start.ceremony_id}/redeem`)
		.header('origin', TARGET_ORIGIN)
		.body({nonce: nonce.nonce, completion_code: finish.completion_code})
		.execute();
}
