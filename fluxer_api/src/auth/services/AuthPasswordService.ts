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
import type {ForgotPasswordRequest, ResetPasswordRequest} from '~/auth/AuthModel';
import {createPasswordResetToken} from '~/BrandedTypes';
import {FLUXER_USER_AGENT, UserFlags} from '~/Constants';
import {InputValidationError, RateLimitError, UnauthorizedError} from '~/Errors';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import {Logger} from '~/Logger';
import type {AuthSession, User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import * as IpUtils from '~/utils/IpUtils';
import {hashPassword as hashPasswordUtil, verifyPassword as verifyPasswordUtil} from '~/utils/PasswordUtils';

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

export class AuthPasswordService {
	constructor(
		private repository: IUserRepository,
		private emailService: IEmailService,
		private rateLimitService: IRateLimitService,
		private generateSecureToken: () => Promise<string>,
		private handleBanStatus: (user: User) => Promise<User>,
		private assertNonBotUser: (user: User) => void,
		private createMfaTicketResponse: (
			user: User,
		) => Promise<{mfa: true; ticket: string; sms: boolean; totp: boolean; webauthn: boolean}>,
		private createAuthSession: (params: {user: User; request: Request}) => Promise<[string, AuthSession]>,
	) {}

	async hashPassword(password: string): Promise<string> {
		return hashPasswordUtil(password);
	}

	async verifyPassword({password, passwordHash}: VerifyPasswordParams): Promise<boolean> {
		return verifyPasswordUtil({password, passwordHash});
	}

	async isPasswordPwned(password: string): Promise<boolean> {
		try {
			const hashed = crypto.createHash('sha1').update(password).digest('hex').toUpperCase();
			const hashPrefix = hashed.slice(0, 5);
			const hashSuffix = hashed.slice(5);

			const response = await fetch(`https://api.pwnedpasswords.com/range/${hashPrefix}`, {
				headers: {
					'User-Agent': FLUXER_USER_AGENT,
					'Add-Padding': 'true',
				},
			});

			if (!response.ok) {
				return false;
			}

			const body = await response.text();
			const lines = body.split('\n');

			for (const line of lines) {
				const [hashSuffixLine, count] = line.split(':', 2);
				if (hashSuffixLine === hashSuffix && Number.parseInt(count, 10) > 0) {
					return true;
				}
			}

			return false;
		} catch (error) {
			Logger.error({error}, 'Failed to check password against Pwned Passwords API');
			return false;
		}
	}

	async forgotPassword({data, request}: ForgotPasswordParams): Promise<void> {
		const clientIp = IpUtils.requireClientIp(request);

		const ipLimitConfig = {maxAttempts: 20, windowMs: 30 * 60 * 1000};
		const emailLimitConfig = {maxAttempts: 5, windowMs: 30 * 60 * 1000};

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
				message: 'Too many password reset attempts. Please try again later.',
				retryAfter,
				limit: exceeded.config.maxAttempts,
				resetTime: exceeded.result.resetTime,
			});
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

	async resetPassword({
		data,
		request,
	}: ResetPasswordParams): Promise<
		| {mfa: false; user_id: string; token: string}
		| {mfa: true; ticket: string; sms: boolean; totp: boolean; webauthn: boolean}
	> {
		const tokenData = await this.repository.getPasswordResetToken(data.token);
		if (!tokenData) {
			throw InputValidationError.create('token', 'Invalid or expired reset token');
		}

		const user = await this.repository.findUnique(tokenData.userId);
		if (!user) {
			throw InputValidationError.create('token', 'Invalid or expired reset token');
		}

		this.assertNonBotUser(user);

		if (user.flags & UserFlags.DELETED) {
			throw InputValidationError.create('token', 'Invalid or expired reset token');
		}

		await this.handleBanStatus(user);

		if (await this.isPasswordPwned(data.password)) {
			throw InputValidationError.create('password', 'Password is too common');
		}

		const newPasswordHash = await this.hashPassword(data.password);
		const updatedUser = await this.repository.patchUpsert(user.id, {
			password_hash: newPasswordHash,
			password_last_changed_at: new Date(),
		});

		if (!updatedUser) {
			throw new UnauthorizedError();
		}

		await this.repository.deleteAllAuthSessions(user.id);
		await this.repository.deletePasswordResetToken(data.token);

		const hasMfa = (updatedUser.authenticatorTypes?.size ?? 0) > 0;
		if (hasMfa) {
			return await this.createMfaTicketResponse(updatedUser);
		}

		const [token] = await this.createAuthSession({user: updatedUser, request});
		return {mfa: false, user_id: updatedUser.id.toString(), token};
	}
}
