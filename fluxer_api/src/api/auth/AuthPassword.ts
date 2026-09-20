// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import {createMfaTicketResponse, type LoginMfaResult} from '@app/api/auth/AuthLogin';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {createPasswordResetToken} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {hashPassword as hashPasswordUtil, verifyPassword as verifyPasswordUtil} from '@app/api/utils/PasswordUtils';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {FLUXER_USER_AGENT} from '@fluxer/constants/src/Core';
import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {ForgotPasswordRequest, ResetPasswordRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ms} from 'itty-time';

const PWNED_PASSWORDS_TIMEOUT_MS = ms('5 seconds');
const PWNED_PASSWORD_CACHE_MAX_PREFIXES = 128;

interface CacheEntry {
	pwnedSuffixes: ReadonlySet<string>;
	expiresAt: number;
}

class PwnedPasswordCache {
	private cache = new Map<string, CacheEntry>();
	private readonly maxSize: number;
	private readonly ttlMs: number;

	constructor(maxSize = PWNED_PASSWORD_CACHE_MAX_PREFIXES, ttlMs = ms('1 hour')) {
		this.maxSize = maxSize;
		this.ttlMs = ttlMs;
	}

	get(hashPrefix: string): ReadonlySet<string> | undefined {
		const entry = this.cache.get(hashPrefix);
		if (!entry) {
			return undefined;
		}
		if (Date.now() > entry.expiresAt) {
			this.cache.delete(hashPrefix);
			return undefined;
		}
		this.cache.delete(hashPrefix);
		this.cache.set(hashPrefix, entry);
		return entry.pwnedSuffixes;
	}

	set(hashPrefix: string, pwnedSuffixes: ReadonlySet<string>): void {
		if (this.cache.size >= this.maxSize && !this.cache.has(hashPrefix)) {
			const firstKey = this.cache.keys().next().value;
			if (firstKey !== undefined) {
				this.cache.delete(firstKey);
			}
		}
		this.cache.set(hashPrefix, {
			pwnedSuffixes,
			expiresAt: Date.now() + this.ttlMs,
		});
	}

	clear(): void {
		this.cache.clear();
	}
}

interface ForgotPasswordParams {
	data: ForgotPasswordRequest;
	request: Request;
}

interface ResetPasswordParams {
	data: ResetPasswordRequest;
	request: Request;
}

interface VerifyPasswordParams {
	password: string;
	passwordHash: string;
}

type ResetPasswordResult =
	| {
			user_id: string;
			token: string;
	  }
	| LoginMfaResult;

const pwnedPasswordCache = new PwnedPasswordCache(PWNED_PASSWORD_CACHE_MAX_PREFIXES, ms('1 hour'));

export function resetPwnedPasswordCacheForTesting(): void {
	pwnedPasswordCache.clear();
}

export async function hashPassword(_ctx: ApiContext, password: string): Promise<string> {
	return hashPasswordUtil(password);
}

export async function verifyPassword(
	_ctx: ApiContext,
	{password, passwordHash}: VerifyPasswordParams,
): Promise<boolean> {
	return verifyPasswordUtil({password, passwordHash});
}

export async function isPasswordPwned(_ctx: ApiContext, password: string): Promise<boolean> {
	if (!Config.breachedPasswordCheck.enabled) {
		return false;
	}
	const hashed = crypto.createHash('sha1').update(password).digest('hex').toUpperCase();
	const hashPrefix = hashed.slice(0, 5);
	const hashSuffix = hashed.slice(5);
	const cachedSuffixes = pwnedPasswordCache.get(hashPrefix);
	if (cachedSuffixes !== undefined) {
		return cachedSuffixes.has(hashSuffix);
	}
	try {
		const response = await fetch(`https://api.pwnedpasswords.com/range/${hashPrefix}`, {
			headers: {
				'User-Agent': FLUXER_USER_AGENT,
				'Add-Padding': 'true',
			},
			signal: AbortSignal.timeout(PWNED_PASSWORDS_TIMEOUT_MS),
		});
		if (!response.ok) {
			FetchUtils.discardResponseBody(response.body, response.status);
			Logger.warn(
				{
					status: response.status,
					statusText: response.statusText,
					hashPrefix,
				},
				'Pwned Passwords API returned non-OK status',
			);
			return false;
		}
		const body = await FetchUtils.streamToStringWithLimit(response.body, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.pwnedPasswordsBytes,
			headers: response.headers,
			url: response.url,
			description: 'Pwned Passwords API response',
		});
		const MAX_PWNED_LINES = 10000;
		const lines = body.split('\n');
		if (lines.length > MAX_PWNED_LINES) {
			Logger.warn(
				{
					lineCount: lines.length,
					maxAllowed: MAX_PWNED_LINES,
					hashPrefix,
				},
				'Pwned Passwords API response exceeded safe line limit, truncating',
			);
		}
		const limit = Math.min(lines.length, MAX_PWNED_LINES);
		const pwnedSuffixes = new Set<string>();
		for (let i = 0; i < limit; i++) {
			const line = lines[i];
			const [hashSuffixLine, count] = line.split(':', 2);
			if (hashSuffixLine.length === hashSuffix.length && Number.parseInt(count, 10) > 0) {
				pwnedSuffixes.add(hashSuffixLine);
			}
		}
		pwnedPasswordCache.set(hashPrefix, pwnedSuffixes);
		return pwnedSuffixes.has(hashSuffix);
	} catch (error) {
		Logger.error({error}, 'Failed to check password against Pwned Passwords API');
		return false;
	}
}

export async function forgotPassword(ctx: ApiContext, {data, request}: ForgotPasswordParams): Promise<void> {
	const {users, email, rateLimit, emailDnsValidation, config} = ctx.services;
	const clientIp = requireClientIp(request, {
		trustClientIpHeader: config.proxy.trust_client_ip_header,
		clientIpHeaderName: config.proxy.client_ip_header,
	});
	const ipRateLimit = await rateLimit.checkLimit({
		identifier: `password_reset:ip:${getSameIpDecisionKey(clientIp) ?? clientIp}`,
		maxAttempts: 20,
		windowMs: ms('30 minutes'),
	});
	const emailRateLimit = await rateLimit.checkLimit({
		identifier: `password_reset:email:${data.email.toLowerCase()}`,
		maxAttempts: 5,
		windowMs: ms('30 minutes'),
	});
	if (!ipRateLimit.allowed) {
		throw createRateLimitError(ipRateLimit);
	}
	if (!emailRateLimit.allowed) {
		throw createRateLimitError(emailRateLimit);
	}
	const hasValidDns = await emailDnsValidation.hasValidDnsRecords(data.email);
	if (!hasValidDns) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
	}
	const user = await users.findByEmail(data.email);
	if (!user) {
		return;
	}
	AuthUtility.assertNonBotUser(ctx, user);
	const token = createPasswordResetToken(await AuthUtility.generateSecureToken(ctx));
	await users.createPasswordResetToken({
		token_: token,
		user_id: user.id,
		email: user.email!,
	});
	await email.sendPasswordResetEmail(user.email!, user.username, token, user.locale);
}

export async function validateResetToken(ctx: ApiContext, token: string): Promise<boolean> {
	const {users} = ctx.services;
	const tokenData = await users.getPasswordResetToken(token);
	if (!tokenData) {
		return false;
	}
	const user = await users.findUnique(tokenData.userId);
	if (!user) {
		return false;
	}
	if (
		user.flags & UserFlags.DELETED ||
		!user.email ||
		user.email.trim().toLowerCase() !== tokenData.email.trim().toLowerCase()
	) {
		return false;
	}
	return true;
}

export async function resetPassword(
	ctx: ApiContext,
	{data, request}: ResetPasswordParams,
): Promise<ResetPasswordResult> {
	const {users} = ctx.services;
	const tokenData = await users.getPasswordResetToken(data.token);
	if (!tokenData) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
	}
	const user = await users.findUnique(tokenData.userId);
	if (!user) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
	}
	AuthUtility.assertNonBotUser(ctx, user);
	if (
		user.flags & UserFlags.DELETED ||
		!user.email ||
		user.email.trim().toLowerCase() !== tokenData.email.trim().toLowerCase()
	) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
	}
	await AuthUtility.handleBanStatus(ctx, user);
	if (await isPasswordPwned(ctx, data.password)) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const webauthnIsSecondFactor = await resolveWebAuthnSecondFactor(ctx, user);
	const hasMfa = user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP) || webauthnIsSecondFactor;
	const newPasswordHash = await hashPassword(ctx, data.password);
	const updates: Partial<UserRow> = {
		password_hash: newPasswordHash,
		password_last_changed_at: new Date(),
	};
	if (webauthnIsSecondFactor && !user.authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN)) {
		const authenticatorTypes = new Set<number>(user.authenticatorTypes);
		authenticatorTypes.add(UserAuthenticatorTypes.WEBAUTHN);
		updates.authenticator_types = authenticatorTypes;
	}
	const updatedUser = await users.patchUpsert(user.id, updates, user.toRow());
	if (updates.authenticator_types) {
		await ctx.services.botMfaMirror.syncAuthenticatorTypesForOwner(updatedUser);
	}
	await AuthSession.terminateAllUserSessions(ctx, user.id);
	await users.deletePasswordResetToken(data.token);
	if (hasMfa) {
		return await createMfaTicketResponse(ctx, updatedUser, webauthnIsSecondFactor);
	}
	const [token] = await AuthSession.createAuthSession(ctx, {
		user: updatedUser,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	return {user_id: updatedUser.id.toString(), token};
}
