// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {PASSKEY_MIGRATION_RP_ID} from '@fluxer/constants/src/PasskeyConstants';

const PASSKEY_TARGET_TO_LEGACY_ORIGIN: ReadonlyMap<string, string> = new Map([
	['https://fluxer.com', 'https://web.fluxer.app'],
	['https://canary.fluxer.com', 'https://web.canary.fluxer.app'],
]);

export interface CredentialRpSelection {
	rpId: string;
	credentials: Array<WebAuthnCredential>;
}

export function isPasskeyTargetOrigin(ctx: ApiContext, origin: string | null | undefined): boolean {
	if (ctx.services.config.instance.selfHosted || !origin) return false;
	return PASSKEY_TARGET_TO_LEGACY_ORIGIN.has(origin);
}

export function passkeyLegacyOriginFor(targetOrigin: string): string {
	return PASSKEY_TARGET_TO_LEGACY_ORIGIN.get(targetOrigin)!;
}

export function effectiveRpId(ctx: ApiContext, credential: WebAuthnCredential): string {
	return credential.rpId ?? ctx.services.config.auth.passkeys.rpId;
}

export function visibleWebAuthnCredentials(credentials: Array<WebAuthnCredential>): Array<WebAuthnCredential> {
	return credentials.filter((credential) => credential.supersededBy === null);
}

function requestedPasskeyRpId(ctx: ApiContext, rpId: string | null | undefined): string | null {
	if (ctx.services.config.instance.selfHosted || !rpId) return null;
	return rpId === PASSKEY_MIGRATION_RP_ID || rpId === ctx.services.config.auth.passkeys.rpId ? rpId : null;
}

function prefersMigrationRp(
	ctx: ApiContext,
	origin: string | null | undefined,
	requestedRpId: string | null | undefined,
): boolean {
	const requested = requestedPasskeyRpId(ctx, requestedRpId);
	if (requested !== null) return requested === PASSKEY_MIGRATION_RP_ID;
	return isPasskeyTargetOrigin(ctx, origin);
}

export function originRpId(ctx: ApiContext, origin: string | null | undefined, requestedRpId?: string | null): string {
	return prefersMigrationRp(ctx, origin, requestedRpId)
		? PASSKEY_MIGRATION_RP_ID
		: ctx.services.config.auth.passkeys.rpId;
}

export async function isPasskeyMigrationActive(ctx: ApiContext, origin: string | null | undefined): Promise<boolean> {
	if (!isPasskeyTargetOrigin(ctx, origin)) return false;
	const config = await getInstanceConfigRepository().getDomainMigrationConfig();
	return config.enabled;
}

function credentialGroup(ctx: ApiContext, credentials: Array<WebAuthnCredential>, rpId: string): CredentialRpSelection {
	return {rpId, credentials: credentials.filter((credential) => effectiveRpId(ctx, credential) === rpId)};
}

export function selectCredentialRp(
	ctx: ApiContext,
	origin: string | null | undefined,
	credentials: Array<WebAuthnCredential>,
	requestedRpId?: string | null,
): CredentialRpSelection {
	const legacyRpId = ctx.services.config.auth.passkeys.rpId;
	const visible = visibleWebAuthnCredentials(credentials);
	if (prefersMigrationRp(ctx, origin, requestedRpId)) {
		const target = credentialGroup(ctx, visible, PASSKEY_MIGRATION_RP_ID);
		return target.credentials.length > 0 ? target : credentialGroup(ctx, visible, legacyRpId);
	}
	const legacy = credentialGroup(ctx, credentials, legacyRpId);
	return legacy.credentials.length > 0 ? legacy : credentialGroup(ctx, visible, PASSKEY_MIGRATION_RP_ID);
}
