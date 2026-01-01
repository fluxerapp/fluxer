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
import {promisify} from 'node:util';
import type {UserID} from '~/BrandedTypes';
import {APIErrorCodes, UserFlags} from '~/Constants';
import {AccessDeniedError, FluxerAPIError, InputValidationError, UnauthorizedError} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import {Logger} from '~/Logger';
import {getUserSearchService} from '~/Meilisearch';
import type {User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import * as AgeUtils from '~/utils/AgeUtils';
import * as RandomUtils from '~/utils/RandomUtils';

const randomBytesAsync = promisify(crypto.randomBytes);
const ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

const base62Encode = (buffer: Uint8Array): string => {
	let num = BigInt(`0x${Buffer.from(buffer).toString('hex')}`);
	const base = BigInt(ALPHANUMERIC_CHARS.length);
	let encoded = '';
	while (num > 0) {
		const remainder = num % base;
		encoded = ALPHANUMERIC_CHARS[Number(remainder)] + encoded;
		num = num / base;
	}
	return encoded;
};

interface ValidateAgeParams {
	dateOfBirth: string;
	minAge: number;
}

interface CheckEmailChangeRateLimitParams {
	userId: UserID;
}

export class AuthUtilityService {
	constructor(
		private repository: IUserRepository,
		private rateLimitService: IRateLimitService,
		private gatewayService: IGatewayService,
	) {}

	async generateSecureToken(length = 64): Promise<string> {
		return RandomUtils.randomString(length);
	}

	async generateAuthToken(): Promise<string> {
		const bytes = await randomBytesAsync(27);
		let token = base62Encode(new Uint8Array(bytes));

		while (token.length < 36) {
			const extraBytes = await randomBytesAsync(1);
			token += ALPHANUMERIC_CHARS[extraBytes[0] % ALPHANUMERIC_CHARS.length];
		}

		if (token.length > 36) {
			token = token.slice(0, 36);
		}

		return `flx_${token}`;
	}

	generateBackupCodes(): Array<string> {
		return Array.from({length: 10}, () => {
			return `${RandomUtils.randomString(4).toLowerCase()}-${RandomUtils.randomString(4).toLowerCase()}`;
		});
	}

	getTokenIdHash(token: string): Uint8Array {
		return new Uint8Array(crypto.createHash('sha256').update(token).digest());
	}

	async checkEmailChangeRateLimit({
		userId,
	}: CheckEmailChangeRateLimitParams): Promise<{allowed: boolean; retryAfter?: number}> {
		const rateLimit = await this.rateLimitService.checkLimit({
			identifier: `email_change:${userId}`,
			maxAttempts: 3,
			windowMs: 60 * 60 * 1000,
		});

		return {
			allowed: rateLimit.allowed,
			retryAfter: rateLimit.retryAfter,
		};
	}

	validateAge({dateOfBirth, minAge}: ValidateAgeParams): boolean {
		const birthDate = new Date(dateOfBirth);
		const age = AgeUtils.calculateAge({
			year: birthDate.getFullYear(),
			month: birthDate.getMonth() + 1,
			day: birthDate.getDate(),
		});
		return age >= minAge;
	}

	assertNonBotUser(user: User): void {
		if (user.isBot) {
			throw new AccessDeniedError('Bot users cannot use auth endpoints');
		}
	}

	async authorizeIpByToken(token: string): Promise<{userId: UserID; email: string} | null> {
		return this.repository.authorizeIpByToken(token);
	}

	checkAccountBanStatus(user: User): {
		isPermanentlyBanned: boolean;
		isTempBanned: boolean;
		tempBanExpired: boolean;
	} {
		const isPermanentlyBanned = !!(user.flags & UserFlags.DELETED);
		const hasTempBan = !!(user.flags & UserFlags.DISABLED && user.tempBannedUntil);
		const tempBanExpired = hasTempBan && user.tempBannedUntil! <= new Date();

		return {
			isPermanentlyBanned,
			isTempBanned: hasTempBan && !tempBanExpired,
			tempBanExpired,
		};
	}

	async handleBanStatus(user: User): Promise<User> {
		const banStatus = this.checkAccountBanStatus(user);

		if (banStatus.isPermanentlyBanned) {
			throw new FluxerAPIError({
				code: APIErrorCodes.ACCOUNT_DISABLED,
				message: 'Your account has been permanently suspended',
				status: 403,
			});
		}

		if (banStatus.isTempBanned) {
			throw new FluxerAPIError({
				code: APIErrorCodes.ACCOUNT_DISABLED,
				message: 'Your account has been temporarily suspended',
				status: 403,
			});
		}

		if (banStatus.tempBanExpired) {
			const updatedUser = await this.repository.patchUpsert(user.id, {
				flags: user.flags & ~UserFlags.DISABLED,
				temp_banned_until: null,
			});

			if (!updatedUser) {
				throw new UnauthorizedError();
			}

			return updatedUser;
		}

		return user;
	}

	async redeemBetaCode(userId: UserID, betaCode: string): Promise<void> {
		const user = await this.repository.findUniqueAssert(userId);

		if ((user.flags & UserFlags.PENDING_MANUAL_VERIFICATION) === 0n) {
			throw InputValidationError.create('beta_code', 'Your account is already verified');
		}

		const code = await this.repository.getBetaCode(betaCode);
		if (!code || code.redeemerId) {
			throw InputValidationError.create('beta_code', 'Invalid or already used beta code');
		}

		await this.repository.updateBetaCodeRedeemed(betaCode, userId, new Date());
		await this.repository.deletePendingVerification(userId);

		const updatedUser = await this.repository.patchUpsert(userId, {
			flags: user.flags & ~UserFlags.PENDING_MANUAL_VERIFICATION,
		});

		const userSearchService = getUserSearchService();
		if (userSearchService && updatedUser) {
			await userSearchService.updateUser(updatedUser).catch((error) => {
				Logger.error({userId, error}, 'Failed to update user in search');
			});
		}

		await this.gatewayService.dispatchPresence({
			userId,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser!),
		});
	}
}
