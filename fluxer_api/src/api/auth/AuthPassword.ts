// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {FLUXER_USER_AGENT} from '@fluxer/constants/src/Core';
import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {ForgotPasswordRequest, ResetPasswordRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ms, seconds} from 'itty-time';
import type {ApiContext} from '../ApiContext';
import {createMfaTicket, createPasswordResetToken} from '../BrandedTypes';
import {Logger} from '../Logger';
import type {User} from '../models/User';
import {EXTERNAL_RESPONSE_LIMITS} from '../utils/ExternalResponseLimits';
import * as FetchUtils from '../utils/FetchUtils';
import {hashPassword as hashPasswordUtil, verifyPassword as verifyPasswordUtil} from '../utils/PasswordUtils';
import * as AuthSession from './AuthSession';
import * as AuthUtility from './AuthUtility';

interface CacheEntry {
	result: boolean;
	expiresAt: number;
}

class PwnedPasswordCache {
	private cache = new Map<string, CacheEntry>();
	private readonly maxSize: number;
	private readonly ttlMs: number;

	constructor(maxSize = 1000, ttlMs = ms('1 hour')) {
		this.maxSize = maxSize;
		this.ttlMs = ttlMs;
	}

	get(key: string): boolean | undefined {
		const entry = this.cache.get(key);
		if (!entry) {
			return undefined;
		}
		if (Date.now() > entry.expiresAt) {
			this.cache.delete(key);
			return undefined;
		}
		this.cache.delete(key);
		this.cache.set(key, entry);
		return entry.result;
	}

	set(key: string, result: boolean): void {
		if (this.cache.size >= this.maxSize && !this.cache.has(key)) {
			const firstKey = this.cache.keys().next().value;
			if (firstKey !== undefined) {
				this.cache.delete(firstKey);
			}
		}
		this.cache.set(key, {
			result,
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
	| {
			mfa: true;
			ticket: string;
			allowed_methods: Array<string>;
			totp: boolean;
			webauthn: boolean;
	  };

const pwnedPasswordCache = new PwnedPasswordCache(1000, ms('1 hour'));

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
	const hashed = crypto.createHash('sha1').update(password).digest('hex').toUpperCase();
	const hashPrefix = hashed.slice(0, 5);
	const hashSuffix = hashed.slice(5);
	const cachedResult = pwnedPasswordCache.get(hashed);
	if (cachedResult !== undefined) {
		return cachedResult;
	}
	try {
		const response = await fetch(`https://api.pwnedpasswords.com/range/${hashPrefix}`, {
			headers: {
				'User-Agent': FLUXER_USER_AGENT,
				'Add-Padding': 'true',
			},
		});
		if (!response.ok) {
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
		for (let i = 0; i < limit; i++) {
			const line = lines[i];
			const [hashSuffixLine, count] = line.split(':', 2);
			if (
				hashSuffixLine.length === hashSuffix.length &&
				crypto.timingSafeEqual(Buffer.from(hashSuffixLine), Buffer.from(hashSuffix)) &&
				Number.parseInt(count, 10) > 0
			) {
				pwnedPasswordCache.set(hashed, true);
				return true;
			}
		}
		pwnedPasswordCache.set(hashed, false);
		return false;
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
	const ipLimitConfig = {maxAttempts: 20, windowMs: ms('30 minutes')};
	const emailLimitConfig = {maxAttempts: 5, windowMs: ms('30 minutes')};
	const ipRateLimit = await rateLimit.checkLimit({
		identifier: `password_reset:ip:${getSameIpDecisionKey(clientIp) ?? clientIp}`,
		...ipLimitConfig,
	});
	const emailRateLimit = await rateLimit.checkLimit({
		identifier: `password_reset:email:${data.email.toLowerCase()}`,
		...emailLimitConfig,
	});
	const exceeded = !ipRateLimit.allowed
		? {result: ipRateLimit, config: ipLimitConfig}
		: !emailRateLimit.allowed
			? {result: emailRateLimit, config: emailLimitConfig}
			: null;
	if (exceeded) {
		const retryAfter =
			exceeded.result.retryAfter ?? Math.max(0, Math.ceil((exceeded.result.resetTime.getTime() - Date.now()) / 1000));
		throw new RateLimitError({
			retryAfter,
			limit: exceeded.result.limit,
			resetTime: exceeded.result.resetTime,
		});
	}
	const hasValidDns = await emailDnsValidation.hasValidDnsRecords(data.email);
	if (!hasValidDns) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
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
	if (user.flags & UserFlags.DELETED) {
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
	if (user.flags & UserFlags.DELETED) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
	}
	await AuthUtility.handleBanStatus(ctx, user);
	if (await isPasswordPwned(ctx, data.password)) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const newPasswordHash = await hashPassword(ctx, data.password);
	const updatedUser = await users.patchUpsert(
		user.id,
		{
			password_hash: newPasswordHash,
			password_last_changed_at: new Date(),
		},
		user.toRow(),
	);
	await users.deleteAllAuthSessions(user.id);
	await users.deletePasswordResetToken(data.token);
	const hasMfa =
		updatedUser.authenticatorTypes.has(UserAuthenticatorTypes.TOTP) ||
		updatedUser.authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN);
	if (hasMfa) {
		return await createMfaTicketResponse(ctx, updatedUser);
	}
	const [token] = await AuthSession.createAuthSession(ctx, {
		user: updatedUser,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	return {user_id: updatedUser.id.toString(), token};
}

async function createMfaTicketResponse(
	ctx: ApiContext,
	user: User,
): Promise<{
	mfa: true;
	ticket: string;
	allowed_methods: Array<string>;
	totp: boolean;
	webauthn: boolean;
}> {
	const {users, cache} = ctx.services;
	const ticket = createMfaTicket(await AuthUtility.generateSecureToken(ctx));
	await cache.set(`mfa-ticket:${ticket}`, user.id.toString(), seconds('5 minutes'));
	const credentials = await users.listWebAuthnCredentials(user.id);
	const hasWebauthn = credentials.length > 0;
	const hasTotp = user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP);
	const allowedMethods: Array<string> = [];
	if (hasTotp) allowedMethods.push('totp');
	if (hasWebauthn) allowedMethods.push('webauthn');
	return {
		mfa: true,
		ticket: ticket,
		allowed_methods: allowedMethods,
		totp: hasTotp,
		webauthn: hasWebauthn,
	};
}
