// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import {createMfaTicketResponse} from '@app/api/auth/AuthLogin';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {RecoveryKitRepository} from '@app/api/auth/services/RecoveryKitRepository';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {UserRecoveryKitRow} from '@app/api/database/types/AuthTypes';
import {usesUsernameSignIn} from '@app/api/instance/AccountIdentityModeCache';
import {
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {findPersonByLoginHandle, parseLoginHandle} from '@app/api/user/UniqueUsernames';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {
	generateRecoveryKey,
	normalizeRecoveryKey,
	RECOVERY_KEY_BYTE_LENGTH,
} from '@fluxer/constants/src/RecoveryKeyUtils';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {RegistrationRejectedError} from '@fluxer/errors/src/domains/auth/RegistrationRejectedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {RecoverAccountRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {
	RecoveryKitCreateResponse,
	RecoveryKitStatusResponse,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {ms, seconds} from 'itty-time';

const DUMMY_SECRET_HASH = crypto.createHash('sha256').update('fluxer-recovery-kit-dummy').digest('hex');

const recoveryKits = new RecoveryKitRepository();
const DUMMY_KIT_USER_ID = createUserID(0n);

interface RecoverAccountParams {
	data: RecoverAccountRequest;
	request: Request;
}

interface PendingRecovery {
	userId: string;
	expectedSecretHash: string;
	secretHash: string;
	createdAt: string;
	passwordHash: string;
}

export interface RecoverAccountResult {
	result: Awaited<ReturnType<typeof AuthPassword.applyPasswordReset>>;
	recoveryKey: string;
	createdAt: Date;
}

function hashRecoveryKey(normalizedKey: string): string {
	return crypto.createHash('sha256').update(normalizedKey).digest('hex');
}

function secretHashesMatch(left: string, right: string): boolean {
	const leftBuffer = Buffer.from(left, 'hex');
	const rightBuffer = Buffer.from(right, 'hex');
	return leftBuffer.length === rightBuffer.length && crypto.timingSafeEqual(leftBuffer, rightBuffer);
}

function issueRecoveryKey(): {formatted: string; secretHash: string; createdAt: Date} {
	const {key, formatted} = generateRecoveryKey(new Uint8Array(crypto.randomBytes(RECOVERY_KEY_BYTE_LENGTH)));
	return {formatted, secretHash: hashRecoveryKey(key), createdAt: new Date()};
}

function pendingRecoveryKey(ticket: string): string {
	return `mfa-recovery:${ticket}`;
}

async function restoreRecoveryKit(userId: UserID, issuedSecretHash: string, kit: UserRecoveryKitRow): Promise<void> {
	try {
		await recoveryKits.replaceIfUnchanged({
			userId,
			expectedSecretHash: issuedSecretHash,
			secretHash: kit.secret_hash,
			createdAt: kit.created_at,
		});
	} catch (error) {
		Logger.error({error, userId: userId.toString()}, 'Could not restore the recovery kit after a failed recovery');
	}
}

function invalidRecoveryKeyError(): InputValidationError {
	return InputValidationError.fromCode('recovery_key', ValidationErrorCodes.INVALID_RECOVERY_KEY);
}

async function checkRecoverRateLimits(ctx: ApiContext, username: string | null, request: Request): Promise<void> {
	const {rateLimit, config} = ctx.services;
	const clientIp = requireConfiguredClientIp(request, config.proxy);
	const sourceKey = getSameIpDecisionKey(clientIp) ?? clientIp;
	const ipRateLimit = await rateLimit.checkLimit({
		identifier: `recover:ip:${sourceKey}`,
		maxAttempts: 10,
		windowMs: ms('30 minutes'),
	});
	if (!ipRateLimit.allowed) {
		throw createRateLimitError(ipRateLimit);
	}
	const identifierRateLimit = await rateLimit.checkLimit({
		identifier:
			username === null ? `recover:id-unparsed:${sourceKey}` : `recover:id:${username.toLowerCase()}:${sourceKey}`,
		maxAttempts: 5,
		windowMs: ms('30 minutes'),
	});
	if (!identifierRateLimit.allowed) {
		throw createRateLimitError(identifierRateLimit);
	}
}

export async function getRecoveryKitStatus(userId: UserID): Promise<RecoveryKitStatusResponse> {
	const kit = await recoveryKits.find(userId);
	return {
		has_recovery_kit: kit !== null,
		created_at: kit ? kit.created_at.toISOString() : null,
	};
}

export async function createRecoveryKit(userId: UserID): Promise<RecoveryKitCreateResponse> {
	const issued = issueRecoveryKey();
	await recoveryKits.upsert({user_id: userId, secret_hash: issued.secretHash, created_at: issued.createdAt});
	return {recovery_key: issued.formatted, created_at: issued.createdAt.toISOString()};
}

async function instanceUsesRecoveryKits(): Promise<boolean> {
	return (await getInstanceConfigRepository().getAccountIdentityMode()) === AccountIdentityModes.USERNAME;
}

export async function deleteRecoveryKit(userId: UserID): Promise<void> {
	if (!(await instanceUsesRecoveryKits())) {
		return;
	}
	await recoveryKits.delete(userId);
}

export async function findRecoveryKitCreatedAt(userId: UserID): Promise<Date | null> {
	if (!(await instanceUsesRecoveryKits())) {
		return null;
	}
	return (await recoveryKits.find(userId))?.created_at ?? null;
}

export async function recoverAccount(
	ctx: ApiContext,
	{data, request}: RecoverAccountParams,
): Promise<RecoverAccountResult> {
	const handle = parseLoginHandle(data.login);
	await checkRecoverRateLimits(ctx, handle?.username ?? null, request);
	const normalizedKey = normalizeRecoveryKey(data.recovery_key);
	const providedHash = hashRecoveryKey(normalizedKey ?? data.recovery_key);
	const user = handle ? await findPersonByLoginHandle(ctx.services.users, handle) : null;
	const kit = await recoveryKits.find(user ? user.id : DUMMY_KIT_USER_ID);
	const keyMatches = secretHashesMatch(providedHash, kit?.secret_hash ?? DUMMY_SECRET_HASH);
	if (!user || !kit || normalizedKey === null || !keyMatches) {
		throw invalidRecoveryKeyError();
	}
	const currentUser = await AuthUtility.handleBanStatus(ctx, user);
	if (currentUser.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT)) {
		throw new RegistrationPendingApprovalError();
	}
	if (currentUser.traits.has(REGISTRATION_REJECTED_TRAIT)) {
		throw new RegistrationRejectedError();
	}
	if (await AuthPassword.isPasswordPwned(ctx, data.password)) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const issued = issueRecoveryKey();
	const webauthnIsSecondFactor = await resolveWebAuthnSecondFactor(ctx, currentUser);
	if (currentUser.authenticatorTypes.has(UserAuthenticatorTypes.TOTP) || webauthnIsSecondFactor) {
		const pending: PendingRecovery = {
			userId: currentUser.id.toString(),
			expectedSecretHash: kit.secret_hash,
			secretHash: issued.secretHash,
			createdAt: issued.createdAt.toISOString(),
			passwordHash: await AuthPassword.hashPassword(ctx, data.password),
		};
		const challenge = await createMfaTicketResponse(ctx, currentUser, webauthnIsSecondFactor);
		try {
			await ctx.services.cache.set<PendingRecovery>(
				pendingRecoveryKey(challenge.ticket),
				pending,
				seconds('5 minutes'),
			);
		} catch (error) {
			await ctx.services.cache.delete(`mfa-ticket:${challenge.ticket}`);
			throw error;
		}
		return {result: challenge, recoveryKey: issued.formatted, createdAt: issued.createdAt};
	}
	const rotated = await recoveryKits.replaceIfUnchanged({
		userId: currentUser.id,
		expectedSecretHash: kit.secret_hash,
		secretHash: issued.secretHash,
		createdAt: issued.createdAt,
	});
	if (!rotated) {
		throw invalidRecoveryKeyError();
	}
	let result: RecoverAccountResult['result'];
	try {
		result = await AuthPassword.applyPasswordReset(ctx, {
			user: currentUser,
			password: data.password,
			request,
			afterPasswordSet: () => ctx.services.users.deleteAllPasswordResetTokens(currentUser.id),
		});
	} catch (error) {
		await restoreRecoveryKit(currentUser.id, issued.secretHash, kit);
		throw error;
	}
	return {result, recoveryKey: issued.formatted, createdAt: issued.createdAt};
}

export async function applyPendingRecovery(ctx: ApiContext, user: User, ticket: string): Promise<User> {
	if (!usesUsernameSignIn()) {
		return user;
	}
	const {cache, users} = ctx.services;
	const key = pendingRecoveryKey(ticket);
	const pending = await cache.get<PendingRecovery>(key);
	if (!pending) {
		return user;
	}
	await cache.delete(key);
	if (pending.userId !== user.id.toString()) {
		throw invalidRecoveryKeyError();
	}
	const kit = await recoveryKits.find(user.id);
	const rotated =
		kit !== null &&
		(await recoveryKits.replaceIfUnchanged({
			userId: user.id,
			expectedSecretHash: pending.expectedSecretHash,
			secretHash: pending.secretHash,
			createdAt: new Date(pending.createdAt),
		}));
	if (!kit || !rotated) {
		await cache.delete(`mfa-ticket:${ticket}`);
		throw invalidRecoveryKeyError();
	}
	let updatedUser: User;
	try {
		updatedUser = await AuthPassword.commitPasswordReset(ctx, {
			user,
			passwordHash: pending.passwordHash,
			webauthnIsSecondFactor: await resolveWebAuthnSecondFactor(ctx, user),
		});
	} catch (error) {
		await restoreRecoveryKit(user.id, pending.secretHash, kit);
		throw error;
	}
	await users.deleteAllPasswordResetTokens(user.id);
	return updatedUser;
}
