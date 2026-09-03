// SPDX-License-Identifier: AGPL-3.0-or-later

import {timingSafeEqual} from 'node:crypto';
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
import type {AuthenticationResponseJSON, RegistrationResponseJSON} from '@simplewebauthn/server';
import {
	generateAuthenticationOptions,
	generateRegistrationOptions,
	type VerifiedAuthenticationResponse,
	type VerifiedRegistrationResponse,
	verifyAuthenticationResponse,
	verifyRegistrationResponse,
} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';
import type {ApiContext} from '../ApiContext';
import {createUserID, type UserID} from '../BrandedTypes';
import {Logger} from '../Logger';
import type {User} from '../models/User';
import type {WebAuthnCredential} from '../models/WebAuthnCredential';
import {getUserSearchService} from '../SearchFactory';
import {mapUserToPrivateResponse} from '../user/UserMappers';
import {TotpGenerator} from '../utils/TotpGenerator';

type WebAuthnChallengeContext = 'registration' | 'discoverable' | 'mfa' | 'sudo';

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
	mfaSecret: string;
	code: string;
	allowBackup?: boolean;
}

interface AvailableMfaMethods {
	totp: boolean;
	webauthn: boolean;
	has_mfa: boolean;
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

export async function verifyMfaCode(ctx: ApiContext, params: VerifyMfaCodeParams): Promise<boolean> {
	const {userId, mfaSecret, code, allowBackup = false} = params;
	const {users, cache, config} = ctx.services;
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

export async function generateWebAuthnRegistrationOptions(ctx: ApiContext, userId: UserID) {
	const {users, config} = ctx.services;
	const user = await users.findUniqueAssert(userId);
	const existingCredentials = await users.listWebAuthnCredentials(userId);
	if (existingCredentials.length >= 10) {
		throw new WebAuthnCredentialLimitReachedError();
	}
	const options = await generateRegistrationOptions({
		rpName: config.auth.passkeys.rpName,
		rpID: config.auth.passkeys.rpId,
		userID: new TextEncoder().encode(user.id.toString()),
		userName: user.username!,
		userDisplayName: user.username!,
		attestationType: 'none',
		excludeCredentials: existingCredentials.map((cred) => ({
			id: cred.credentialId,
			transports: cred.transports
				? (Array.from(cred.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
				: undefined,
		})),
		authenticatorSelection: {
			residentKey: 'preferred',
			requireResidentKey: false,
			userVerification: 'preferred',
		},
	});
	await saveWebAuthnChallenge(ctx, options.challenge, {context: 'registration', userId});
	return options;
}

export async function verifyWebAuthnRegistration(
	ctx: ApiContext,
	userId: UserID,
	response: RegistrationResponseJSON,
	expectedChallenge: string,
	name: string,
): Promise<void> {
	const {users, gateway, botMfaMirror, config} = ctx.services;
	const user = await users.findUniqueAssert(userId);
	const existingCredentials = await users.listWebAuthnCredentials(userId);
	await consumeWebAuthnChallenge(ctx, expectedChallenge, 'registration', {userId});
	if (existingCredentials.length >= 10) {
		throw new WebAuthnCredentialLimitReachedError();
	}
	if (config.dev.testModeEnabled) {
		const responseObj = response as {id?: string; response?: {transports?: Array<string>}};
		const credentialId = responseObj.id ?? `test-credential:${userId.toString()}:${Date.now()}`;
		const publicKeyBuffer = Buffer.from(`test-public-key:${credentialId}`);
		await users.createWebAuthnCredential(
			userId,
			credentialId,
			publicKeyBuffer,
			0n,
			responseObj.response?.transports ? new Set(responseObj.response.transports) : null,
			name,
		);
	} else {
		const expectedOrigin = config.auth.passkeys.allowedOrigins;
		const rpID = config.auth.passkeys.rpId;
		let verification: VerifiedRegistrationResponse;
		try {
			verification = await verifyRegistrationResponse({
				response,
				expectedChallenge,
				expectedOrigin,
				expectedRPID: rpID,
				requireUserVerification: false,
			});
		} catch (error) {
			Logger.error({error, userId, expectedChallenge, rpID, expectedOrigin}, 'WebAuthn verification failed');
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
		const responseObj = response as {response?: {transports?: Array<string>}};
		await users.createWebAuthnCredential(
			userId,
			credential.id,
			publicKeyBuffer,
			counterBigInt,
			responseObj.response?.transports ? new Set(responseObj.response.transports) : null,
			name,
		);
	}
	const authenticatorTypes = user.authenticatorTypes || new Set<number>();
	if (!authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN)) {
		authenticatorTypes.add(UserAuthenticatorTypes.WEBAUTHN);
		const updatedUser = await users.patchUpsert(userId, {authenticator_types: authenticatorTypes}, user.toRow());
		const userSearchService = getUserSearchService();
		if (userSearchService && 'updateUser' in userSearchService) {
			await userSearchService.updateUser(updatedUser).catch((error) => {
				Logger.error({userId, error}, 'Failed to update user in search');
			});
		}
		await gateway.dispatchPresence({userId, event: 'USER_UPDATE', data: mapUserToPrivateResponse(updatedUser)});
		await botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
	}
	await dispatchWebAuthnCredentialsUpdate(ctx, userId);
}

export async function deleteWebAuthnCredential(ctx: ApiContext, userId: UserID, credentialId: string): Promise<void> {
	const {users, gateway, botMfaMirror} = ctx.services;
	const credential = await users.getWebAuthnCredential(userId, credentialId);
	if (!credential) {
		throw new UnknownWebAuthnCredentialError();
	}
	await users.deleteWebAuthnCredential(userId, credentialId);
	const remainingCredentials = await users.listWebAuthnCredentials(userId);
	if (remainingCredentials.length === 0) {
		const user = await users.findUniqueAssert(userId);
		const authenticatorTypes = user.authenticatorTypes || new Set<number>();
		authenticatorTypes.delete(UserAuthenticatorTypes.WEBAUTHN);
		const updatedUser = await users.patchUpsert(userId, {authenticator_types: authenticatorTypes}, user.toRow());
		const userSearchService = getUserSearchService();
		if (userSearchService && 'updateUser' in userSearchService) {
			await userSearchService.updateUser(updatedUser).catch((error) => {
				Logger.error({userId, error}, 'Failed to update user in search');
			});
		}
		await gateway.dispatchPresence({userId, event: 'USER_UPDATE', data: mapUserToPrivateResponse(updatedUser)});
		await botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
	}
	await dispatchWebAuthnCredentialsUpdate(ctx, userId);
}

export async function renameWebAuthnCredential(
	ctx: ApiContext,
	userId: UserID,
	credentialId: string,
	name: string,
): Promise<void> {
	const {users} = ctx.services;
	const credential = await users.getWebAuthnCredential(userId, credentialId);
	if (!credential) {
		throw new UnknownWebAuthnCredentialError();
	}
	await users.updateWebAuthnCredentialName(userId, credentialId, name);
	await dispatchWebAuthnCredentialsUpdate(ctx, userId);
}

async function dispatchWebAuthnCredentialsUpdate(ctx: ApiContext, userId: UserID): Promise<void> {
	const {users, gateway} = ctx.services;
	const credentials = await users.listWebAuthnCredentials(userId);
	await gateway.dispatchPresence({
		userId,
		event: 'WEBAUTHN_CREDENTIALS_UPDATE',
		data: credentials.map((cred: WebAuthnCredential) => ({
			id: cred.credentialId,
			name: cred.name,
			created_at: cred.createdAt.toISOString(),
			last_used_at: cred.lastUsedAt?.toISOString() ?? null,
		})),
	});
}

export async function generateWebAuthnAuthenticationOptionsDiscoverable(ctx: ApiContext) {
	const options = await generateAuthenticationOptions({
		rpID: ctx.services.config.auth.passkeys.rpId,
		userVerification: 'required',
	});
	await saveWebAuthnChallenge(ctx, options.challenge, {context: 'discoverable'});
	return options;
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

export async function generateWebAuthnAuthenticationOptionsForMfa(ctx: ApiContext, ticket: string) {
	const {users, cache, config} = ctx.services;
	const userIdStr = await cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userIdStr) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const userId = createUserID(BigInt(userIdStr));
	const credentials = await users.listWebAuthnCredentials(userId);
	if (credentials.length === 0) {
		throw new NoPasskeysRegisteredError();
	}
	const options = await generateAuthenticationOptions({
		rpID: config.auth.passkeys.rpId,
		allowCredentials: credentials.map((cred) => ({
			id: cred.credentialId,
			transports: cred.transports
				? (Array.from(cred.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
				: undefined,
		})),
		userVerification: 'discouraged',
	});
	await saveWebAuthnChallenge(ctx, options.challenge, {context: 'mfa', userId, ticket});
	return options;
}

export async function verifyWebAuthnAuthentication(
	ctx: ApiContext,
	userId: UserID,
	response: AuthenticationResponseJSON,
	expectedChallenge: string,
	context: WebAuthnChallengeContext = 'mfa',
	ticket?: string,
): Promise<void> {
	const {users, config} = ctx.services;
	await consumeWebAuthnChallenge(ctx, expectedChallenge, context, {userId, ticket});
	const credentialId = (response as {id: string}).id;
	const credential = await users.getWebAuthnCredential(userId, credentialId);
	if (!credential) {
		throw new PasskeyAuthenticationFailedError();
	}
	if (config.dev.testModeEnabled) {
		await users.updateWebAuthnCredentialCounter(userId, credentialId, credential.counter + 1n);
		await users.updateWebAuthnCredentialLastUsed(userId, credentialId);
		return;
	}
	const expectedOrigin = config.auth.passkeys.allowedOrigins;
	const rpID = config.auth.passkeys.rpId;
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
			expectedRPID: rpID,
			requireUserVerification: requiresWebAuthnUserVerification(context),
			credential: {
				id: credential.credentialId,
				publicKey: publicKeyUint8Array,
				counter: Number(credential.counter),
				transports: credential.transports
					? (Array.from(credential.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
					: undefined,
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
}

export async function generateWebAuthnOptionsForSudo(ctx: ApiContext, userId: UserID) {
	const {users, config} = ctx.services;
	const credentials = await users.listWebAuthnCredentials(userId);
	if (credentials.length === 0) {
		throw new NoPasskeysRegisteredError();
	}
	const options = await generateAuthenticationOptions({
		rpID: config.auth.passkeys.rpId,
		allowCredentials: credentials.map((cred) => ({
			id: cred.credentialId,
			transports: cred.transports
				? (Array.from(cred.transports) as Array<'usb' | 'nfc' | 'ble' | 'internal' | 'cable' | 'hybrid'>)
				: undefined,
		})),
		userVerification: 'discouraged',
	});
	await saveWebAuthnChallenge(ctx, options.challenge, {context: 'sudo', userId});
	return options;
}

const SUDO_MFA_USER_MAX_ATTEMPTS = 10;

async function consumeSudoMfaAttempt(ctx: ApiContext, userId: UserID): Promise<void> {
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
	const hasMfa =
		(user?.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP) ?? false) ||
		(user?.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false);
	if (!user || !hasMfa) {
		return {success: false, error: 'MFA not enabled'};
	}
	switch (method) {
		case 'totp': {
			if (!code) return {success: false, error: 'TOTP code is required'};
			if (!user.totpSecret) return {success: false, error: 'TOTP is not enabled'};
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
			if (!user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN)) {
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
	const user = await ctx.services.users.findUnique(userId);
	if (!user) {
		return {totp: false, webauthn: false, has_mfa: false};
	}
	return {
		totp: user.totpSecret !== null,
		webauthn: user.authenticatorTypes?.has(UserAuthenticatorTypes.WEBAUTHN) ?? false,
		has_mfa: (user.authenticatorTypes?.size ?? 0) > 0,
	};
}

function webAuthnChallengeCacheKey(challenge: string): string {
	return `webauthn:challenge:${challenge}`;
}

function requiresWebAuthnUserVerification(context: WebAuthnChallengeContext): boolean {
	return context === 'discoverable';
}

async function saveWebAuthnChallenge(
	ctx: ApiContext,
	challenge: string,
	entry: {context: WebAuthnChallengeContext; userId?: UserID; ticket?: string},
): Promise<void> {
	await ctx.services.cache.set(
		webAuthnChallengeCacheKey(challenge),
		{context: entry.context, userId: entry.userId?.toString(), ticket: entry.ticket},
		seconds('5 minutes'),
	);
}

async function consumeWebAuthnChallenge(
	ctx: ApiContext,
	challenge: string,
	expectedContext: WebAuthnChallengeContext,
	{userId, ticket}: {userId?: UserID; ticket?: string} = {},
): Promise<void> {
	const {cache} = ctx.services;
	const key = webAuthnChallengeCacheKey(challenge);
	const cached = await cache.get<{context: WebAuthnChallengeContext; userId?: string; ticket?: string}>(key);
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
	await cache.delete(key);
}

function createChallengeError(context: WebAuthnChallengeContext) {
	if (context === 'registration') {
		return new InvalidWebAuthnCredentialError();
	}
	return new PasskeyAuthenticationFailedError();
}
