// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import {
	effectiveRpId,
	isPasskeyTargetOrigin,
	visibleWebAuthnCredentials,
} from '@app/api/auth/services/PasskeyRelyingParty';
import type {UserID} from '@app/api/BrandedTypes';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {PASSKEY_MIGRATION_RP_ID} from '@fluxer/constants/src/PasskeyConstants';
import {UnknownPasskeyMigrationError} from '@fluxer/errors/src/domains/auth/UnknownPasskeyMigrationError';
import type {
	PasskeyMigrationCompleteRequest,
	PasskeyMigrationResponse,
} from '@fluxer/schema/src/domains/auth/PasskeyMigrationSchemas';
import type {PublicKeyCredentialCreationOptionsJSON} from '@simplewebauthn/server';
import {seconds} from 'itty-time';

const PASSKEY_MIGRATION_KEY_PREFIX = 'passkey_migration:';

interface PendingPasskeyMigration {
	user_id: string;
	credential_id: string;
	cross_device: boolean;
}

interface LivePasskeyMigration {
	key: string;
	pending: PendingPasskeyMigration;
	credential: WebAuthnCredential;
}

function passkeyMigrationKey(authSession: AuthSession): string {
	return `${PASSKEY_MIGRATION_KEY_PREFIX}${authSession.sessionIdHash.toString('base64url')}`;
}

function isLegacyVisibleCredential(ctx: ApiContext, credential: WebAuthnCredential): boolean {
	return credential.supersededBy === null && effectiveRpId(ctx, credential) === ctx.services.config.auth.passkeys.rpId;
}

export async function recordPendingPasskeyMigration(
	ctx: ApiContext,
	authSession: AuthSession,
	pending: PendingPasskeyMigration,
): Promise<void> {
	await ctx.services.cache.set(passkeyMigrationKey(authSession), pending, seconds('5 minutes'));
}

async function loadLivePasskeyMigration(
	ctx: ApiContext,
	userId: UserID,
	authSession: AuthSession | undefined,
): Promise<LivePasskeyMigration | null> {
	if (!authSession) return null;
	const {cache, users} = ctx.services;
	const key = passkeyMigrationKey(authSession);
	const pending = await cache.get<PendingPasskeyMigration>(key);
	if (!pending) return null;
	const credential =
		pending.user_id === userId.toString() ? await users.getWebAuthnCredential(userId, pending.credential_id) : null;
	if (credential === null || !isLegacyVisibleCredential(ctx, credential)) {
		await cache.delete(key);
		return null;
	}
	return {key, pending, credential};
}

async function requireLivePasskeyMigration(
	ctx: ApiContext,
	userId: UserID,
	authSession: AuthSession | undefined,
	origin: string | undefined,
): Promise<LivePasskeyMigration> {
	const live = isPasskeyTargetOrigin(ctx, origin) ? await loadLivePasskeyMigration(ctx, userId, authSession) : null;
	if (!live) {
		throw new UnknownPasskeyMigrationError();
	}
	return live;
}

async function takeLivePasskeyMigration(ctx: ApiContext, userId: UserID, key: string): Promise<WebAuthnCredential> {
	const pending = await ctx.services.cache.getAndDelete<PendingPasskeyMigration>(key);
	if (!pending || pending.user_id !== userId.toString()) {
		throw new UnknownPasskeyMigrationError();
	}
	const credential = await ctx.services.users.getWebAuthnCredential(userId, pending.credential_id);
	if (credential === null || !isLegacyVisibleCredential(ctx, credential)) {
		throw new UnknownPasskeyMigrationError();
	}
	return credential;
}

function visibleTargetCredentials(ctx: ApiContext, credentials: Array<WebAuthnCredential>): Array<WebAuthnCredential> {
	return visibleWebAuthnCredentials(credentials).filter(
		(credential) => effectiveRpId(ctx, credential) === PASSKEY_MIGRATION_RP_ID,
	);
}

export async function getPasskeyMigration(
	ctx: ApiContext,
	userId: UserID,
	authSession: AuthSession | undefined,
): Promise<PasskeyMigrationResponse> {
	const live = await loadLivePasskeyMigration(ctx, userId, authSession);
	if (!live) return {pending: null};
	return {
		pending: {
			credential_id: live.credential.credentialId,
			name: live.credential.name,
			cross_device: live.pending.cross_device,
		},
	};
}

export async function getPasskeyMigrationRegistrationOptions(
	ctx: ApiContext,
	userId: UserID,
	authSession: AuthSession | undefined,
	origin: string | undefined,
): Promise<PublicKeyCredentialCreationOptionsJSON> {
	const live = await requireLivePasskeyMigration(ctx, userId, authSession, origin);
	const credentials = await ctx.services.users.listWebAuthnCredentials(userId);
	const options = await AuthMfa.createWebAuthnRegistrationOptions(ctx, userId, {
		rpId: PASSKEY_MIGRATION_RP_ID,
		context: 'migration_registration',
		excludeCredentials: visibleTargetCredentials(ctx, credentials),
	});
	if (live.pending.cross_device) {
		options.hints = ['hybrid', 'security-key'];
	}
	return options;
}

export async function completePasskeyMigration(
	ctx: ApiContext,
	userId: UserID,
	authSession: AuthSession | undefined,
	origin: string | undefined,
	data: PasskeyMigrationCompleteRequest,
): Promise<void> {
	const {users} = ctx.services;
	const live = await requireLivePasskeyMigration(ctx, userId, authSession, origin);
	const verified = await AuthMfa.verifyWebAuthnRegistrationResponse(
		ctx,
		userId,
		data.response,
		data.challenge,
		'migration_registration',
		[origin!],
	);
	const legacy = await takeLivePasskeyMigration(ctx, userId, live.key);
	await users.createWebAuthnCredential(
		userId,
		verified.credentialId,
		verified.publicKey,
		verified.counter,
		verified.transports,
		legacy.name,
		AuthMfa.storedRpId(ctx, verified.rpId),
	);
	await users.setWebAuthnCredentialSupersededBy(userId, legacy.credentialId, verified.credentialId);
	await AuthMfa.dispatchWebAuthnCredentialsUpdate(ctx, userId);
}
