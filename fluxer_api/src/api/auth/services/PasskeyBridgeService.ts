// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes, timingSafeEqual} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import * as AuthLogin from '@app/api/auth/AuthLogin';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {recordPendingPasskeyMigration} from '@app/api/auth/services/PasskeyMigrationService';
import {
	effectiveRpId,
	isPasskeyMigrationActive,
	isPasskeyTargetOrigin,
	passkeyLegacyOriginFor,
	visibleWebAuthnCredentials,
} from '@app/api/auth/services/PasskeyRelyingParty';
import {getSudoModeService} from '@app/api/auth/services/SudoModeService';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {User} from '@app/api/models/User';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {mapUserToPartialResponse} from '@app/api/user/UserMappers';
import {PASSKEY_BRIDGE_PATH, PASSKEY_BRIDGE_RETURN_FRAGMENT_KEY} from '@fluxer/constants/src/PasskeyConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidPasskeyBridgeNonceError} from '@fluxer/errors/src/domains/auth/InvalidPasskeyBridgeNonceError';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import {NoPasskeysRegisteredError} from '@fluxer/errors/src/domains/auth/NoPasskeysRegisteredError';
import {PasskeyAuthenticationFailedError} from '@fluxer/errors/src/domains/auth/PasskeyAuthenticationFailedError';
import {UnknownPasskeyBridgeError} from '@fluxer/errors/src/domains/auth/UnknownPasskeyBridgeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InvalidApiOriginError} from '@fluxer/errors/src/domains/core/InvalidApiOriginError';
import type {
	PasskeyBridgeCompleteRequest,
	PasskeyBridgeFinishResponse,
	PasskeyBridgeLoginRedeemResponse,
	PasskeyBridgeLoginStartRequest,
	PasskeyBridgeRedeemRequest,
	PasskeyBridgeRunner,
	PasskeyBridgeStartResponse,
	PasskeyBridgeSudoRedeemResponse,
	PasskeyBridgeSudoStartRequest,
} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';
import type {PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';

type PasskeyBridgePurpose = 'login' | 'login_mfa' | 'sudo';

interface PasskeyBridgeRecord {
	purpose: PasskeyBridgePurpose;
	runner: PasskeyBridgeRunner;
	target_origin: string;
	ceremony_origin: string;
	nonce_hash: string;
	user_id: string | null;
	ticket: string | null;
	challenge: string | null;
	credential_id: string | null;
	cross_device: boolean;
	completion_code_hash: string | null;
	status: 'pending' | 'completed' | 'cancelled';
	created_at: number;
	expires_at: number;
}

interface CompletedPasskeyBridge {
	record: PasskeyBridgeRecord;
	userId: UserID;
}

const PASSKEY_BRIDGE_KEY_PREFIX = 'passkey_bridge:';
const PASSKEY_BRIDGE_LOCK_PREFIX = 'passkey_bridge_lock:';
const PASSKEY_BRIDGE_SECRET_BYTES = 32;

function sha256Hex(value: string): string {
	return createHash('sha256').update(value).digest('hex');
}

function hashMatches(value: string, storedHash: string | null): boolean {
	if (storedHash === null) return false;
	const presented = Buffer.from(sha256Hex(value), 'hex');
	const stored = Buffer.from(storedHash, 'hex');
	return presented.length === stored.length && timingSafeEqual(presented, stored);
}

function createSecret(): string {
	return randomBytes(PASSKEY_BRIDGE_SECRET_BYTES).toString('base64url');
}

function passkeyBridgeKey(ceremonyId: string): string {
	return `${PASSKEY_BRIDGE_KEY_PREFIX}${sha256Hex(ceremonyId)}`;
}

async function writeRecord(ctx: ApiContext, ceremonyId: string, record: PasskeyBridgeRecord): Promise<void> {
	const ttlSeconds = Math.floor((record.expires_at - Date.now()) / 1000);
	if (ttlSeconds <= 0) {
		throw new UnknownPasskeyBridgeError();
	}
	await ctx.services.cache.set(passkeyBridgeKey(ceremonyId), record, ttlSeconds);
}

function assertCeremonyOrigin(
	ctx: ApiContext,
	record: PasskeyBridgeRecord,
	origin: string | undefined,
	expectedOrigin: string,
): void {
	if (origin !== expectedOrigin || !isPasskeyTargetOrigin(ctx, record.target_origin)) {
		throw new InvalidApiOriginError();
	}
}

async function mutateRecord<T>(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
	mutate: (record: PasskeyBridgeRecord) => Promise<T>,
): Promise<T> {
	const {cache} = ctx.services;
	const lockKey = `${PASSKEY_BRIDGE_LOCK_PREFIX}${sha256Hex(ceremonyId)}`;
	const lockToken = await cache.acquireLock(lockKey, seconds('10 seconds'));
	if (!lockToken) {
		throw new UnknownPasskeyBridgeError();
	}
	try {
		const record = await cache.get<PasskeyBridgeRecord>(passkeyBridgeKey(ceremonyId));
		if (!record) {
			throw new UnknownPasskeyBridgeError();
		}
		assertCeremonyOrigin(ctx, record, origin, record.ceremony_origin);
		return await mutate(record);
	} finally {
		await cache.releaseLock(lockKey, lockToken);
	}
}

async function requireMfaTicketUser(ctx: ApiContext, ticket: string, expectedUserId?: string): Promise<User> {
	const userId = await ctx.services.cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userId || (expectedUserId !== undefined && userId !== expectedUserId)) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const user = await ctx.services.users.findUniqueAssert(createUserID(BigInt(userId)));
	AuthUtility.assertNonBotUser(ctx, user);
	return user;
}

async function requireLegacyCredentials(ctx: ApiContext, userId: UserID): Promise<Array<WebAuthnCredential>> {
	const legacyRpId = ctx.services.config.auth.passkeys.rpId;
	const credentials = visibleWebAuthnCredentials(await ctx.services.users.listWebAuthnCredentials(userId)).filter(
		(credential) => effectiveRpId(ctx, credential) === legacyRpId,
	);
	if (credentials.length === 0) {
		throw new NoPasskeysRegisteredError();
	}
	return credentials;
}

function assertBridgeStartOrigin(ctx: ApiContext, origin: string | undefined): string {
	if (!origin || !isPasskeyTargetOrigin(ctx, origin)) {
		throw new InvalidApiOriginError();
	}
	return origin;
}

async function startPasskeyBridge(
	ctx: ApiContext,
	origin: string,
	fields: Pick<PasskeyBridgeRecord, 'purpose' | 'runner' | 'nonce_hash' | 'user_id' | 'ticket'>,
): Promise<PasskeyBridgeStartResponse> {
	const ceremonyId = createSecret();
	const createdAt = Date.now();
	const ceremonyOrigin = fields.runner === 'page' ? passkeyLegacyOriginFor(origin) : origin;
	await writeRecord(ctx, ceremonyId, {
		...fields,
		target_origin: origin,
		ceremony_origin: ceremonyOrigin,
		challenge: null,
		credential_id: null,
		cross_device: false,
		completion_code_hash: null,
		status: 'pending',
		created_at: createdAt,
		expires_at: createdAt + (fields.purpose === 'login_mfa' ? ms('5 minutes') : ms('10 minutes')),
	});
	return {
		ceremony_id: ceremonyId,
		bridge_url: fields.runner === 'page' ? `${ceremonyOrigin}${PASSKEY_BRIDGE_PATH}#${ceremonyId}` : null,
	};
}

export async function startPasskeyBridgeLogin(
	ctx: ApiContext,
	origin: string | undefined,
	data: PasskeyBridgeLoginStartRequest,
): Promise<PasskeyBridgeStartResponse> {
	const targetOrigin = assertBridgeStartOrigin(ctx, origin);
	let userId: string | null = null;
	if (data.purpose === 'login_mfa') {
		const user = await requireMfaTicketUser(ctx, data.ticket!);
		if (!(await resolveWebAuthnSecondFactor(ctx, user))) {
			throw new MfaNotEnabledError();
		}
		await requireLegacyCredentials(ctx, user.id);
		userId = user.id.toString();
	}
	return startPasskeyBridge(ctx, targetOrigin, {
		purpose: data.purpose,
		runner: data.runner,
		nonce_hash: data.nonce_hash,
		user_id: userId,
		ticket: data.ticket ?? null,
	});
}

export async function startPasskeyBridgeSudo(
	ctx: ApiContext,
	origin: string | undefined,
	userId: UserID,
	data: PasskeyBridgeSudoStartRequest,
): Promise<PasskeyBridgeStartResponse> {
	const targetOrigin = assertBridgeStartOrigin(ctx, origin);
	await requireLegacyCredentials(ctx, userId);
	return startPasskeyBridge(ctx, targetOrigin, {
		purpose: 'sudo',
		runner: data.runner,
		nonce_hash: data.nonce_hash,
		user_id: userId.toString(),
		ticket: null,
	});
}

export async function getPasskeyBridgeOptions(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
): Promise<{options: PublicKeyCredentialRequestOptionsJSON}> {
	return mutateRecord(ctx, ceremonyId, origin, async (record) => {
		if (record.status !== 'pending') {
			throw new UnknownPasskeyBridgeError();
		}
		const legacyRpId = ctx.services.config.auth.passkeys.rpId;
		const userId = record.user_id === null ? undefined : createUserID(BigInt(record.user_id));
		const options = await AuthMfa.generateWebAuthnAuthenticationOptions(ctx, {
			selection: {
				rpId: legacyRpId,
				credentials: userId === undefined ? null : await requireLegacyCredentials(ctx, userId),
			},
			context: 'bridge',
			userId,
		});
		if (record.challenge !== null) {
			await AuthMfa.deleteWebAuthnChallenge(ctx, record.challenge);
		}
		await writeRecord(ctx, ceremonyId, {...record, challenge: options.challenge});
		return {options};
	});
}

function buildReturnUrl(record: PasskeyBridgeRecord, ceremonyId: string, completionCode: string): string {
	return `${record.target_origin}${PASSKEY_BRIDGE_PATH}#${PASSKEY_BRIDGE_RETURN_FRAGMENT_KEY}=${ceremonyId}.${completionCode}`;
}

async function finishRecord(
	ctx: ApiContext,
	ceremonyId: string,
	record: PasskeyBridgeRecord,
): Promise<PasskeyBridgeFinishResponse> {
	const completionCode = createSecret();
	await writeRecord(ctx, ceremonyId, {...record, completion_code_hash: sha256Hex(completionCode)});
	if (record.runner === 'native') {
		return {return_url: null, completion_code: completionCode};
	}
	return {return_url: buildReturnUrl(record, ceremonyId, completionCode), completion_code: null};
}

export async function completePasskeyBridge(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
	data: PasskeyBridgeCompleteRequest,
): Promise<PasskeyBridgeFinishResponse> {
	return mutateRecord(ctx, ceremonyId, origin, async (record) => {
		if (record.status !== 'pending') {
			throw new UnknownPasskeyBridgeError();
		}
		const {users} = ctx.services;
		const credentialId = data.response.id;
		const userId =
			record.user_id === null
				? await users.getUserIdByCredentialId(credentialId)
				: createUserID(BigInt(record.user_id));
		const credential = userId === null ? null : await users.getWebAuthnCredential(userId, credentialId);
		if (
			userId === null ||
			record.challenge === null ||
			credential === null ||
			credential.supersededBy !== null ||
			effectiveRpId(ctx, credential) !== ctx.services.config.auth.passkeys.rpId
		) {
			throw new PasskeyAuthenticationFailedError();
		}
		if (record.purpose === 'login_mfa') {
			await requireMfaTicketUser(ctx, record.ticket!, record.user_id!);
			await AuthLogin.consumeMfaAttempt(ctx, {userId: record.user_id!, ticket: record.ticket!, field: 'ticket'});
		} else if (record.purpose === 'sudo') {
			await AuthMfa.consumeSudoMfaAttempt(ctx, userId);
		}
		await AuthMfa.verifyWebAuthnAuthentication(ctx, userId, data.response, record.challenge, 'bridge', undefined, [
			record.ceremony_origin,
		]);
		return finishRecord(ctx, ceremonyId, {
			...record,
			status: 'completed',
			user_id: userId.toString(),
			credential_id: credentialId,
			cross_device: data.response.authenticatorAttachment === 'cross-platform',
		});
	});
}

export async function cancelPasskeyBridge(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
): Promise<PasskeyBridgeFinishResponse> {
	return mutateRecord(ctx, ceremonyId, origin, async (record) => {
		if (record.status === 'completed') {
			throw new UnknownPasskeyBridgeError();
		}
		return finishRecord(ctx, ceremonyId, {...record, status: 'cancelled'});
	});
}

function assertRedeemable(
	record: PasskeyBridgeRecord | null,
	purposes: ReadonlyArray<PasskeyBridgePurpose>,
	expectedUserId: UserID | null,
): asserts record is PasskeyBridgeRecord {
	if (
		!record ||
		!purposes.includes(record.purpose) ||
		(expectedUserId !== null && record.user_id !== expectedUserId.toString()) ||
		record.status === 'pending'
	) {
		throw new UnknownPasskeyBridgeError();
	}
}

async function redeemPasskeyBridge(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
	data: PasskeyBridgeRedeemRequest,
	purposes: ReadonlyArray<PasskeyBridgePurpose>,
	expectedUserId: UserID | null,
): Promise<CompletedPasskeyBridge | null> {
	const {cache} = ctx.services;
	const key = passkeyBridgeKey(ceremonyId);
	const record = await cache.get<PasskeyBridgeRecord>(key);
	if (!record) {
		throw new UnknownPasskeyBridgeError();
	}
	assertCeremonyOrigin(ctx, record, origin, record.target_origin);
	assertRedeemable(record, purposes, expectedUserId);
	if (!hashMatches(data.nonce, record.nonce_hash) || !hashMatches(data.completion_code, record.completion_code_hash)) {
		await cache.delete(key);
		throw new InvalidPasskeyBridgeNonceError();
	}
	const taken = await cache.getAndDelete<PasskeyBridgeRecord>(key);
	assertRedeemable(taken, purposes, expectedUserId);
	if (!hashMatches(data.nonce, taken.nonce_hash) || !hashMatches(data.completion_code, taken.completion_code_hash)) {
		throw new InvalidPasskeyBridgeNonceError();
	}
	if (taken.status === 'cancelled') {
		return null;
	}
	return {record: taken, userId: createUserID(BigInt(taken.user_id!))};
}

async function recordMigrationIfActive(
	ctx: ApiContext,
	origin: string | undefined,
	completed: CompletedPasskeyBridge,
	authSession: AuthSession | undefined,
): Promise<void> {
	if (!authSession || !(await isPasskeyMigrationActive(ctx, origin))) return;
	await recordPendingPasskeyMigration(ctx, authSession, {
		user_id: completed.userId.toString(),
		credential_id: completed.record.credential_id!,
		cross_device: completed.record.cross_device,
	});
}

export async function redeemPasskeyBridgeLogin(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
	data: PasskeyBridgeRedeemRequest,
	request: Request,
): Promise<PasskeyBridgeLoginRedeemResponse> {
	const completed = await redeemPasskeyBridge(ctx, ceremonyId, origin, data, ['login', 'login_mfa'], null);
	if (!completed) {
		return {status: 'cancelled'};
	}
	let token: string;
	let authSession: AuthSession;
	let user: User;
	if (completed.record.purpose === 'login_mfa') {
		user = await requireMfaTicketUser(ctx, completed.record.ticket!, completed.record.user_id!);
		if (!(await resolveWebAuthnSecondFactor(ctx, user))) {
			throw new MfaNotEnabledError();
		}
		[token, authSession] = await AuthLogin.completeMfaLogin(ctx, user, completed.record.ticket!, request);
	} else {
		user = await ctx.services.users.findUniqueAssert(completed.userId);
		[token, authSession] = await AuthLogin.createLoginSession(ctx, user, request);
	}
	await recordMigrationIfActive(ctx, origin, completed, authSession);
	return {status: 'completed', token, user_id: user.id.toString(), user: mapUserToPartialResponse(user)};
}

export async function redeemPasskeyBridgeSudo(
	ctx: ApiContext,
	ceremonyId: string,
	origin: string | undefined,
	data: PasskeyBridgeRedeemRequest,
	userId: UserID,
	authSession: AuthSession | undefined,
): Promise<PasskeyBridgeSudoRedeemResponse> {
	const completed = await redeemPasskeyBridge(ctx, ceremonyId, origin, data, ['sudo'], userId);
	if (!completed) {
		return {status: 'cancelled'};
	}
	const sudoToken = await getSudoModeService().generateSudoToken(userId);
	await recordMigrationIfActive(ctx, origin, completed, authSession);
	return {status: 'completed', sudo_token: sudoToken};
}
