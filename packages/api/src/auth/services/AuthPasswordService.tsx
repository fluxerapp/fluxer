/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import crypto from 'node:crypto';
import {createPasswordResetToken} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {IEmailDnsValidationService} from '@fluxer/api/src/infrastructure/IEmailDnsValidationService';
import {Logger} from '@fluxer/api/src/Logger';
import type {AuthSession} from '@fluxer/api/src/models/AuthSession';
import type {User} from '@fluxer/api/src/models/User';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {
	hashPassword as hashPasswordUtil,
	verifyPassword as verifyPasswordUtil,
} from '@fluxer/api/src/utils/PasswordUtils';
import {FLUXER_USER_AGENT} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import type {ForgotPasswordRequest, ResetPasswordRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ms} from 'itty-time';

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

const pwnedPasswordCache = new PwnedPasswordCache(1000, ms('1 hour'));

export class AuthPasswordService {
	constructor(
		private repository: IUserRepository,
		private emailService: IEmailService,
		private emailDnsValidationService: IEmailDnsValidationService,
		private rateLimitService: IRateLimitService,
		private generateSecureToken: () => Promise<string>,
		private handleBanStatus: (user: User) => Promise<User>,
		private assertNonBotUser: (user: User) => void,
		private createMfaTicketResponse: (user: User) => Promise<{
			mfa: true;
			ticket: string;
			allowed_methods: Array<string>;
			sms_phone_hint: string | null;
			sms: boolean;
			totp: boolean;
			webauthn: boolean;
		}>,
		private createAuthSession: (params: {user: User; request: Request}) => Promise<[string, AuthSession]>,
	) {}

	async hashPassword(password: string): Promise<string> {
		return hashPasswordUtil(password);
	}

	async verifyPassword({password, passwordHash}: VerifyPasswordParams): Promise<boolean> {
		return verifyPasswordUtil({password, passwordHash});
	}

	async isPasswordPwned(password: string): Promise<boolean> {
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

			const body = await response.text();

			const MAX_PWNED_LINES = 10_000;
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

	async forgotPassword({data, request}: ForgotPasswordParams): Promise<void> {
		const clientIp = requireClientIp(request, {
			trustCfConnectingIp: Config.proxy.trust_cf_connecting_ip,
		});

		const ipLimitConfig = {maxAttempts: 20, windowMs: ms('30 minutes')};
		const emailLimitConfig = {maxAttempts: 5, windowMs: ms('30 minutes')};

		const ipRateLimit = await this.rateLimitService.checkLimit({
			identifier: `password_reset:ip:${clientIp}`,
			...ipLimitConfig,
		});
		const emailRateLimit = await this.rateLimitService.checkLimit({
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

		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(data.email);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}

		const user = await this.repository.findByEmail(data.email);
		if (!user) {
			return;
		}

		this.assertNonBotUser(user);

		const token = createPasswordResetToken(await this.generateSecureToken());
		await this.repository.createPasswordResetToken({
			token_: token,
			user_id: user.id,
			email: user.email!,
		});

		await this.emailService.sendPasswordResetEmail(user.email!, user.username, token, user.locale);
	}

	async resetPassword({data, request}: ResetPasswordParams): Promise<
		| {user_id: string; token: string}
		| {
				mfa: true;
				ticket: string;
				allowed_methods: Array<string>;
				sms_phone_hint: string | null;
				sms: boolean;
				totp: boolean;
				webauthn: boolean;
		  }
	> {
		const tokenData = await this.repository.getPasswordResetToken(data.token);
		if (!tokenData) {
			throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
		}

		const user = await this.repository.findUnique(tokenData.userId);
		if (!user) {
			throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
		}

		this.assertNonBotUser(user);

		if (user.flags & UserFlags.DELETED) {
			throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_RESET_TOKEN);
		}

		await this.handleBanStatus(user);

		if (await this.isPasswordPwned(data.password)) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
		}

		const newPasswordHash = await this.hashPassword(data.password);
		const updatedUser = await this.repository.patchUpsert(
			user.id,
			{
				password_hash: newPasswordHash,
				password_last_changed_at: new Date(),
			},
			user.toRow(),
		);

		await this.repository.deleteAllAuthSessions(user.id);
		await this.repository.deletePasswordResetToken(data.token);

		const hasMfa = (updatedUser.authenticatorTypes?.size ?? 0) > 0;
		if (hasMfa) {
			return await this.createMfaTicketResponse(updatedUser);
		}

		const [token] = await this.createAuthSession({user: updatedUser, request});
		return {user_id: updatedUser.id.toString(), token};
	}
}
