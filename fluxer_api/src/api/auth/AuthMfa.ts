// SPDX-License-Identifier: AGPL-3.0-or-later

import {timingSafeEqual} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {
	type CredentialRpSelection,
	effectiveRpId,
	originRpId,
	selectCredentialRp,
	visibleWebAuthnCredentials,
} from '@app/api/auth/services/PasskeyRelyingParty';
import {deriveSudoMethods, userHasMfa, userHasSudoCapability} from '@app/api/auth/services/SudoMethods';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import type {User} from '@app/api/models/User';
import type {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {mapUserToPrivateResponse, mapWebAuthnCredentialToResponse} from '@app/api/user/UserMappers';
import {TotpGenerator} from '@app/api/utils/TotpGenerator';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidWebAuthnAuthenticationCounterError} from '@fluxer/errors/src/domains/auth/InvalidWebAuthnAuthenticationCounterError';
import {InvalidWebAuthnCredentialCounterError} from '@fluxer/errors/src/domains/auth/InvalidWebAuthnCredentialCounterError';
import {InvalidWebAuthnCredentialError} from '@fluxer/errors/src/domains/auth/InvalidWebAuthnCredentialError';
import {InvalidWebAuthnPublicKeyFormatError} from '@fluxer/errors/src/domains/auth/InvalidWebAuthnPublicKeyFormatError';
import {NoPasskeysRegisteredError} from '@fluxer/errors/src/domains/auth/NoPasskeysRegisteredError';
import {PasskeyAuthenticationFailedError} from '@fluxer/errors/src/domains/auth/PasskeyAuthenticationFailedError';
import {UnknownWebAuthnCredentialError} from '@fluxer/errors/src/domains/auth/UnknownWebAuthnCredentialError';
import {WebAuthnCredentialLimitReachedError} from '@fluxer/errors/src/domains/auth/WebAuthnCredentialLimitReachedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/server';
import {
	generateAuthenticationOptions,
	generateRegistrationOptions,
	type VerifiedAuthenticationResponse,
	type VerifiedRegistrationResponse,
	verifyAuthenticationResponse,
	verifyRegistrationResponse,
} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';

type WebAuthnChallengeContext = 'registration' | 'discoverable' | 'mfa' | 'sudo' | 'bridge' | 'migration_registration';

interface WebAuthnChallengeEntry {
	context: WebAuthnChallengeContext;
	userId?: string;
	ticket?: string;
	rpId?: string;
	credentialIds?: Array<string> | null;
}

interface WebAuthnChallengeScope {
	rpId: string;
	credentialIds: Array<string> | null;
}

interface WebAuthnAuthenticationOptionsParams {
	selection: CredentialRpSelection | {rpId: string; credentials: null};
	context: WebAuthnChallengeContext;
	userId?: UserID;
	ticket?: string;
}

interface WebAuthnRegistrationOptionsParams {
	rpId: string;
	context: WebAuthnChallengeContext;
	excludeCredentials: Array<WebAuthnCredential>;
}

interface VerifiedWebAuthnRegistration {
	credentialId: string;
	publicKey: Buffer;
	counter: bigint;
	transports: Set<string> | null;
	rpId: string;
}

interface SudoMfaVerificationParams {
	userId: UserID;
	method: 'totp' | 'webauthn';
	code?: string;
	webauthnResponse?: AuthenticationResponseJSON;
	webauthnChallenge?: string;
}

interface SudoMfaVerificationResult {
	success: boolean;
	error?: string;
}

interface VerifyMfaCodeParams {
	userId: UserID;
	mfaSecret: string | null;
	code: string;
	allowBackup?: boolean;
}

interface AvailableMfaMethods {
	totp: boolean;
	webauthn: boolean;
	backup_codes: boolean;
	has_mfa: boolean;
}

interface SetWebAuthnTwoFactorResult {
	user: User;
	backupCodes: Array<MfaBackupCode> | null;
}

function constantTimeEquals(a: string, b: string): boolean {
	const bufferA = Buffer.from(a);
	const bufferB = Buffer.from(b);
	if (bufferA.length !== bufferB.length) {
		return false;
	}
	return timingSafeEqual(bufferA, bufferB);
}

function normalizeBackupCode(code: string): string {
	return code.toLowerCase().replace(/[^a-z0-9]/g, '');
}

export async function hasUnconsumedBackupCodes(ctx: ApiContext, userId: UserID): Promise<boolean> {
	const backupCodes = await ctx.services.users.listMfaBackupCodes(userId);
	return backupCodes.some((backupCode) => !backupCode.consumed);
}

export async function verifyMfaCode(ctx: ApiContext, params: VerifyMfaCodeParams): Promise<boolean> {
	const {userId, mfaSecret, code, allowBackup = false} = params;
	const {users, cache, config} = ctx.services;
	if (mfaSecret !== null) {
		try {
			const totp = new TotpGenerator(mfaSecret);
			const isValidTotp = await totp.validateTotp(code);
			if (isValidTotp) {
				if (config.dev.testModeEnabled) {
					return true;
				}
				const reuseKey = `mfa-totp:${userId}:${code}`;
				const lockToken = await cache.acquireLock(reuseKey, seconds('90 seconds'));
				if (lockToken) {
					return true;
				}
			}
		} catch (error) {
			Logger.error({userId, code: `${code.slice(0, 3)}***`, error}, 'Failed to validate TOTP code');
		}
	}
	if (allowBackup) {
		const normalizedCode = normalizeBackupCode(code);
		if (normalizedCode.length > 0) {
			const backupCodes = await users.listMfaBackupCodes(userId);
			const backupCode = backupCodes.find(
				(bc) => !bc.consumed && constantTimeEquals(normalizeBackupCode(bc.code), normalizedCode),
			);
			if (backupCode) {
				await users.consumeMfaBackupCode(userId, backupCode.code);
				return true;
			}
		}
	}
	return false;
}

function toCredentialDescriptor(credential: WebAuthnCredential) {
	return {
		id: credential.credentialId,
		transports: credential.transports
			? (Array.from(credential.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
			: undefined,
	};
}

export function storedRpId(ctx: ApiContext, rpId: string): string | null {
	return rpId === ctx.services.config.auth.passkeys.rpId ? null : rpId;
}

export async function createWebAuthnRegistrationOptions(
	ctx: ApiContext,
	userId: UserID,
	{rpId, context, excludeCredentials}: WebAuthnRegistrationOptionsParams,
): Promise<PublicKeyCredentialCreationOptionsJSON> {
	const {users, config} = ctx.services;
	const user = await users.findUniqueAssert(userId);
	const options = await generateRegistrationOptions({
		rpName: config.auth.passkeys.rpName,
		rpID: rpId,
		userID: new TextEncoder().encode(user.id.toString()),
		userName: user.username!,
		userDisplayName: user.username!,
		attestationType: 'none',
		supportedAlgorithmIDs: [-8, -7, -257],
		excludeCredentials: excludeCredentials.map(toCredentialDescriptor),
		authenticatorSelection: {
			residentKey: 'preferred',
			requireResidentKey: false,
			userVerification: 'preferred',
		},
	});
	await saveWebAuthnChallenge(ctx, options.challenge, {context, userId, rpId, credentialIds: null});
	return options;
}

export async function generateWebAuthnRegistrationOptions(
	ctx: ApiContext,
	userId: UserID,
	origin: string | null | undefined,
): Promise<PublicKeyCredentialCreationOptionsJSON> {
	const existingCredentials = await ctx.services.users.listWebAuthnCredentials(userId);
	if (visibleWebAuthnCredentials(existingCredentials).length >= 10) {
		throw new WebAuthnCredentialLimitReachedError();
	}
	return createWebAuthnRegistrationOptions(ctx, userId, {
		rpId: originRpId(ctx, origin),
		context: 'registration',
		excludeCredentials: existingCredentials,
	});
}

export async function verifyWebAuthnRegistrationResponse(
	ctx: ApiContext,
	userId: UserID,
	response: RegistrationResponseJSON,
	expectedChallenge: string,
	context: WebAuthnChallengeContext,
	expectedOrigin: Array<string> = ctx.services.config.auth.passkeys.allowedOrigins,
): Promise<VerifiedWebAuthnRegistration> {
	const {config} = ctx.services;
	const {rpId} = await consumeWebAuthnChallenge(ctx, expectedChallenge, context, {userId});
	const responseObj = response as {id?: string; response?: {transports?: Array<string>}};
	const transports = responseObj.response?.transports ? new Set(responseObj.response.transports) : null;
	if (config.dev.testModeEnabled) {
		const credentialId = responseObj.id ?? `test-credential:${userId.toString()}:${Date.now()}`;
		return {credentialId, publicKey: Buffer.from(`test-public-key:${credentialId}`), counter: 0n, transports, rpId};
	}
	let verification: VerifiedRegistrationResponse;
	try {
		verification = await verifyRegistrationResponse({
			response,
			expectedChallenge,
			expectedOrigin,
			expectedRPID: rpId,
			requireUserVerification: false,
			supportedAlgorithmIDs: [-8, -7, -257],
		});
	} catch (error) {
		Logger.error({error, userId, expectedChallenge, rpId, expectedOrigin}, 'WebAuthn verification failed');
		throw new InvalidWebAuthnCredentialError();
	}
	if (!verification.verified || !verification.registrationInfo) {
		Logger.error(
			{userId, verified: verification.verified, hasRegistrationInfo: !!verification.registrationInfo},
			'WebAuthn verification result invalid',
		);
		throw new InvalidWebAuthnCredentialError();
	}
	const {credential} = verification.registrationInfo;
	let publicKeyBuffer: Buffer;
	let counterBigInt: bigint;
	try {
		publicKeyBuffer = Buffer.from(credential.publicKey);
	} catch (_error) {
		throw new InvalidWebAuthnPublicKeyFormatError();
	}
	try {
		if (credential.counter === undefined || credential.counter === null) {
			throw new Error('Counter value is undefined or null');
		}
		counterBigInt = BigInt(credential.counter);
	} catch (_error) {
		throw new InvalidWebAuthnCredentialCounterError();
	}
	return {credentialId: credential.id, publicKey: publicKeyBuffer, counter: counterBigInt, transports, rpId};
}

export async function verifyWebAuthnRegistration(
	ctx: ApiContext,
	userId: UserID,
	response: RegistrationResponseJSON,
	expectedChallenge: string,
	name: string,
): Promise<void> {
	const {users} = ctx.services;
	const existingCredentials = await users.listWebAuthnCredentials(userId);
	if (visibleWebAuthnCredentials(existingCredentials).length >= 10) {
		throw new WebAuthnCredentialLimitReachedError();
	}
	const verified = await verifyWebAuthnRegistrationResponse(ctx, userId, response, expectedChallenge, 'registration');
	await users.createWebAuthnCredential(
		userId,
		verified.credentialId,
		verified.publicKey,
		verified.counter,
		verified.transports,
		name,
		storedRpId(ctx, verified.rpId),
	);
	await dispatchWebAuthnCredentialsUpdate(ctx, userId);
}

export async function deleteWebAuthnCredential(ctx: ApiContext, userId: UserID, credentialId: string): Promise<void> {
	const {users, gateway, botMfaMirror} = ctx.services;
	const credential = await users.getWebAuthnCredential(userId, credentialId);
	if (!credential || credential.supersededBy !== null) {
		throw new UnknownWebAuthnCredentialError();
	}
	await users.deleteWebAuthnCredential(userId, credentialId);
	const remaining = await users.listWebAuthnCredentials(userId);
	const remainingCredentials = visibleWebAuthnCredentials(remaining);
	const orphanedTwins = remaining.filter(
		(cred) => cred.supersededBy === credentialId || (cred.supersededBy !== null && remainingCredentials.length === 0),
	);
	for (const twin of orphanedTwins) {
		await users.deleteWebAuthnCredential(userId, twin.credentialId);
	}
	if (remainingCredentials.length === 0) {
		const user = await users.findUniqueAssert(userId);
		if (user.authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN)) {
			const authenticatorTypes = new Set<number>(user.authenticatorTypes ?? []);
			authenticatorTypes.delete(UserAuthenticatorTypes.WEBAUTHN);
			const updatedUser = await users.patchUpsert(userId, {authenticator_types: authenticatorTypes}, user.toRow());
			if (!userHasMfa(updatedUser)) {
				await users.clearMfaBackupCodes(userId);
			}
			await gateway.dispatchPresence({userId, event: 'USER_UPDATE', data: mapUserToPrivateResponse(updatedUser)});
			await botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
		}
	}
	await dispatchWebAuthnCredentialsUpdate(ctx, userId);
}

export async function setWebAuthnTwoFactor(
	ctx: ApiContext,
	userId: UserID,
	enabled: boolean,
): Promise<SetWebAuthnTwoFactorResult> {
	const {users, gateway, botMfaMirror} = ctx.services;
	const user = await users.findUniqueAssert(userId);
	const credentials = await users.listWebAuthnCredentials(userId);
	if (enabled && credentials.length === 0) {
		throw new NoPasskeysRegisteredError();
	}
	const authenticatorTypes = new Set<number>(user.authenticatorTypes ?? []);
	if (authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN) === enabled) {
		return {user, backupCodes: null};
	}
	if (enabled) {
		authenticatorTypes.add(UserAuthenticatorTypes.WEBAUTHN);
	} else {
		authenticatorTypes.delete(UserAuthenticatorTypes.WEBAUTHN);
	}
	const updatedUser = await users.patchUpsert(userId, {authenticator_types: authenticatorTypes}, user.toRow());
	let backupCodes: Array<MfaBackupCode> | null = null;
	if (enabled) {
		const existingBackupCodes = await users.listMfaBackupCodes(userId);
		if (existingBackupCodes.every((backupCode) => backupCode.consumed)) {
			backupCodes = await users.createMfaBackupCodes(userId, AuthUtility.generateBackupCodes(ctx));
		}
	} else if (!userHasMfa(updatedUser)) {
		await users.clearMfaBackupCodes(userId);
	}
	await gateway.dispatchPresence({userId, event: 'USER_UPDATE', data: mapUserToPrivateResponse(updatedUser)});
	await botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
	return {user: updatedUser, backupCodes};
}

export async function renameWebAuthnCredential(
	ctx: ApiContext,
	userId: UserID,
	credentialId: string,
	name: string,
): Promise<void> {
	const {users} = ctx.services;
	const credential = await users.getWebAuthnCredential(userId, credentialId);
	if (!credential || credential.supersededBy !== null) {
		throw new UnknownWebAuthnCredentialError();
	}
	await users.updateWebAuthnCredentialName(userId, credentialId, name);
	await dispatchWebAuthnCredentialsUpdate(ctx, userId);
}

export async function dispatchWebAuthnCredentialsUpdate(ctx: ApiContext, userId: UserID): Promise<void> {
	const {users, gateway, config} = ctx.services;
	const credentials = await users.listWebAuthnCredentials(userId);
	await gateway.dispatchPresence({
		userId,
		event: 'WEBAUTHN_CREDENTIALS_UPDATE',
		data: visibleWebAuthnCredentials(credentials).map((cred) =>
			mapWebAuthnCredentialToResponse(cred, config.auth.passkeys.rpId),
		),
	});
}

export async function generateWebAuthnAuthenticationOptions(
	ctx: ApiContext,
	{selection, context, userId, ticket}: WebAuthnAuthenticationOptionsParams,
): Promise<PublicKeyCredentialRequestOptionsJSON> {
	const options = await generateAuthenticationOptions({
		rpID: selection.rpId,
		allowCredentials: selection.credentials?.map(toCredentialDescriptor),
		userVerification: selection.credentials === null ? 'required' : 'discouraged',
	});
	await saveWebAuthnChallenge(ctx, options.challenge, {
		context,
		userId,
		ticket,
		rpId: selection.rpId,
		credentialIds: selection.credentials?.map((cred) => cred.credentialId) ?? null,
	});
	return options;
}

function selectCredentialRpOrThrow(
	ctx: ApiContext,
	origin: string | null | undefined,
	credentials: Array<WebAuthnCredential>,
): CredentialRpSelection {
	const selection = selectCredentialRp(ctx, origin, credentials);
	if (selection.credentials.length === 0) {
		throw new NoPasskeysRegisteredError();
	}
	return selection;
}

export async function generateWebAuthnAuthenticationOptionsDiscoverable(
	ctx: ApiContext,
	origin: string | null | undefined,
): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return generateWebAuthnAuthenticationOptions(ctx, {
		selection: {rpId: originRpId(ctx, origin), credentials: null},
		context: 'discoverable',
	});
}

export async function verifyWebAuthnAuthenticationDiscoverable(
	ctx: ApiContext,
	response: AuthenticationResponseJSON,
	expectedChallenge: string,
): Promise<User> {
	const {users} = ctx.services;
	const credentialId = (response as {id: string}).id;
	const userId = await users.getUserIdByCredentialId(credentialId);
	if (!userId) {
		throw new PasskeyAuthenticationFailedError();
	}
	await verifyWebAuthnAuthentication(ctx, userId, response, expectedChallenge, 'discoverable');
	return users.findUniqueAssert(userId);
}

export async function generateWebAuthnAuthenticationOptionsForMfa(
	ctx: ApiContext,
	ticket: string,
	origin: string | null | undefined,
): Promise<PublicKeyCredentialRequestOptionsJSON> {
	const {users, cache} = ctx.services;
	const userIdStr = await cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userIdStr) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const userId = createUserID(BigInt(userIdStr));
	const credentials = await users.listWebAuthnCredentials(userId);
	return generateWebAuthnAuthenticationOptions(ctx, {
		selection: selectCredentialRpOrThrow(ctx, origin, credentials),
		context: 'mfa',
		userId,
		ticket,
	});
}

export async function verifyWebAuthnAuthentication(
	ctx: ApiContext,
	userId: UserID,
	response: AuthenticationResponseJSON,
	expectedChallenge: string,
	context: WebAuthnChallengeContext = 'mfa',
	ticket?: string,
	expectedOrigin: Array<string> = ctx.services.config.auth.passkeys.allowedOrigins,
): Promise<WebAuthnCredential> {
	const {users, config} = ctx.services;
	const scope = await consumeWebAuthnChallenge(ctx, expectedChallenge, context, {userId, ticket});
	const credentialId = (response as {id: string}).id;
	const credential = await users.getWebAuthnCredential(userId, credentialId);
	if (!credential) {
		throw new PasskeyAuthenticationFailedError();
	}
	if (
		effectiveRpId(ctx, credential) !== scope.rpId ||
		(scope.credentialIds !== null && !scope.credentialIds.includes(credentialId))
	) {
		throw new PasskeyAuthenticationFailedError();
	}
	if (config.dev.testModeEnabled) {
		await users.updateWebAuthnCredentialCounter(userId, credentialId, credential.counter + 1n);
		await users.updateWebAuthnCredentialLastUsed(userId, credentialId);
		return credential;
	}
	let verification: VerifiedAuthenticationResponse;
	try {
		let publicKeyUint8Array: Uint8Array<ArrayBuffer>;
		try {
			const buffer = Buffer.from(credential.publicKey);
			const arrayBuffer: ArrayBuffer = buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.byteLength);
			publicKeyUint8Array = new Uint8Array(arrayBuffer);
		} catch (_error) {
			throw new InvalidWebAuthnPublicKeyFormatError();
		}
		verification = await verifyAuthenticationResponse({
			response,
			expectedChallenge,
			expectedOrigin,
			expectedRPID: scope.rpId,
			requireUserVerification: requiresWebAuthnUserVerification(context, scope),
			credential: {
				...toCredentialDescriptor(credential),
				publicKey: publicKeyUint8Array,
				counter: Number(credential.counter),
			},
		});
	} catch (_error) {
		throw new PasskeyAuthenticationFailedError();
	}
	if (!verification.verified) {
		throw new PasskeyAuthenticationFailedError();
	}
	let newCounter: bigint;
	try {
		const reported = verification.authenticationInfo.newCounter;
		if (reported === undefined || reported === null) {
			throw new Error('Counter value is undefined or null');
		}
		newCounter = BigInt(reported);
	} catch (_error) {
		throw new InvalidWebAuthnAuthenticationCounterError();
	}
	await users.updateWebAuthnCredentialCounter(userId, credentialId, newCounter);
	await users.updateWebAuthnCredentialLastUsed(userId, credentialId);
	return credential;
}

export async function generateWebAuthnOptionsForSudo(
	ctx: ApiContext,
	userId: UserID,
	origin: string | null | undefined,
): Promise<PublicKeyCredentialRequestOptionsJSON> {
	const credentials = await ctx.services.users.listWebAuthnCredentials(userId);
	return generateWebAuthnAuthenticationOptions(ctx, {
		selection: selectCredentialRpOrThrow(ctx, origin, credentials),
		context: 'sudo',
		userId,
	});
}

const SUDO_MFA_USER_MAX_ATTEMPTS = 10;

export async function consumeSudoMfaAttempt(ctx: ApiContext, userId: UserID): Promise<void> {
	const {rateLimit} = ctx.services;
	const userLimit = await rateLimit.checkLimit({
		identifier: `sudo-mfa:user:${userId}`,
		maxAttempts: SUDO_MFA_USER_MAX_ATTEMPTS,
		windowMs: ms('15 minutes'),
	});
	if (!userLimit.allowed) {
		throw InputValidationError.fromCode('mfa_code', ValidationErrorCodes.INVALID_MFA_CODE);
	}
}

export async function verifySudoMfa(
	ctx: ApiContext,
	params: SudoMfaVerificationParams,
): Promise<SudoMfaVerificationResult> {
	const {users} = ctx.services;
	const {userId, method, code, webauthnResponse, webauthnChallenge} = params;
	const user = await users.findUnique(userId);
	if (!user) {
		return {success: false, error: 'MFA not enabled'};
	}
	const credentials = await users.listWebAuthnCredentials(userId);
	const hasPasskeyCredentials = credentials.length > 0;
	if (!userHasSudoCapability(user, hasPasskeyCredentials)) {
		return {success: false, error: 'MFA not enabled'};
	}
	switch (method) {
		case 'totp': {
			if (!code) return {success: false, error: 'TOTP code is required'};
			await consumeSudoMfaAttempt(ctx, userId);
			const isValid = await verifyMfaCode(ctx, {userId, mfaSecret: user.totpSecret, code, allowBackup: true});
			if (isValid) {
				await ctx.services.rateLimit.resetLimit(`sudo-mfa:user:${userId}`);
			}
			return {success: isValid, error: isValid ? undefined : 'Invalid TOTP code'};
		}
		case 'webauthn': {
			if (!webauthnResponse || !webauthnChallenge) {
				return {success: false, error: 'WebAuthn response and challenge are required'};
			}
			if (!hasPasskeyCredentials) {
				return {success: false, error: 'WebAuthn is not enabled'};
			}
			try {
				await verifyWebAuthnAuthentication(ctx, userId, webauthnResponse, webauthnChallenge, 'sudo');
				return {success: true};
			} catch {
				return {success: false, error: 'WebAuthn verification failed'};
			}
		}
		default:
			return {success: false, error: 'Invalid MFA method'};
	}
}

export async function getAvailableMfaMethods(ctx: ApiContext, userId: UserID): Promise<AvailableMfaMethods> {
	const {users} = ctx.services;
	const user = await users.findUnique(userId);
	if (!user) {
		return {totp: false, webauthn: false, backup_codes: false, has_mfa: false};
	}
	const credentials = await users.listWebAuthnCredentials(userId);
	const hasPasskeyCredentials = credentials.length > 0;
	const methods = deriveSudoMethods(user, hasPasskeyCredentials, await hasUnconsumedBackupCodes(ctx, userId));
	return {
		totp: methods.totp,
		webauthn: methods.webauthn,
		backup_codes: methods.backup_codes,
		has_mfa: userHasSudoCapability(user, hasPasskeyCredentials),
	};
}

function webAuthnChallengeCacheKey(challenge: string): string {
	return `webauthn:challenge:${challenge}`;
}

function requiresWebAuthnUserVerification(context: WebAuthnChallengeContext, scope: WebAuthnChallengeScope): boolean {
	return context === 'discoverable' || (context === 'bridge' && scope.credentialIds === null);
}

async function saveWebAuthnChallenge(
	ctx: ApiContext,
	challenge: string,
	entry: {
		context: WebAuthnChallengeContext;
		userId?: UserID;
		ticket?: string;
		rpId: string;
		credentialIds: Array<string> | null;
	},
): Promise<void> {
	const value: WebAuthnChallengeEntry = {
		context: entry.context,
		userId: entry.userId?.toString(),
		ticket: entry.ticket,
		rpId: entry.rpId,
		credentialIds: entry.credentialIds,
	};
	await ctx.services.cache.set(webAuthnChallengeCacheKey(challenge), value, seconds('5 minutes'));
}

export async function deleteWebAuthnChallenge(ctx: ApiContext, challenge: string): Promise<void> {
	await ctx.services.cache.delete(webAuthnChallengeCacheKey(challenge));
}

async function consumeWebAuthnChallenge(
	ctx: ApiContext,
	challenge: string,
	expectedContext: WebAuthnChallengeContext,
	{userId, ticket}: {userId?: UserID; ticket?: string} = {},
): Promise<WebAuthnChallengeScope> {
	const cached = await ctx.services.cache.getAndDelete<WebAuthnChallengeEntry>(webAuthnChallengeCacheKey(challenge));
	const challengeMatches =
		cached &&
		cached.context === expectedContext &&
		(userId === undefined || cached.userId === undefined || cached.userId === userId.toString()) &&
		(ticket === undefined || cached.ticket === undefined || cached.ticket === ticket);
	if (!challengeMatches) {
		Logger.error(
			{
				challenge,
				expectedContext,
				userId: userId?.toString(),
				ticket,
				cached,
				contextMatches: cached?.context === expectedContext,
				userIdMatches: userId === undefined || cached?.userId === undefined || cached?.userId === userId.toString(),
				ticketMatches: ticket === undefined || cached?.ticket === undefined || cached?.ticket === ticket,
			},
			'WebAuthn challenge mismatch',
		);
		throw createChallengeError(expectedContext);
	}
	return {
		rpId: cached.rpId ?? ctx.services.config.auth.passkeys.rpId,
		credentialIds: cached.credentialIds ?? null,
	};
}

function createChallengeError(context: WebAuthnChallengeContext) {
	if (context === 'registration' || context === 'migration_registration') {
		return new InvalidWebAuthnCredentialError();
	}
	return new PasskeyAuthenticationFailedError();
}
