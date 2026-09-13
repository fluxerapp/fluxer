// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {createEmailVerificationToken} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {SuspiciousActivityFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {BotUserAuthEndpointAccessDeniedError} from '@fluxer/errors/src/domains/auth/BotUserAuthEndpointAccessDeniedError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import type {VerifyEmailRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ms} from 'itty-time';

export const EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS =
	SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL |
	SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE;

function assertNonBotUser(user: User): void {
	if (user.isBot) {
		throw new BotUserAuthEndpointAccessDeniedError();
	}
}

export async function verifyEmail(ctx: ApiContext, data: VerifyEmailRequest): Promise<boolean> {
	const {users, gateway} = ctx.services;
	const tokenData = await users.getEmailVerificationToken(data.token);
	if (!tokenData) {
		return false;
	}
	const user = await users.findUnique(tokenData.userId);
	if (!user) {
		return false;
	}
	assertNonBotUser(user);
	if (
		user.flags & UserFlags.DELETED ||
		!user.email ||
		user.email.trim().toLowerCase() !== tokenData.email.trim().toLowerCase()
	) {
		return false;
	}
	const updates: {
		email_verified: boolean;
		email_bounced: boolean;
		suspicious_activity_flags?: number;
	} = {
		email_verified: true,
		email_bounced: false,
	};
	if (user.suspiciousActivityFlags !== null && user.suspiciousActivityFlags !== 0) {
		const newFlags = user.suspiciousActivityFlags & ~EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS;
		if (newFlags !== user.suspiciousActivityFlags) {
			updates.suspicious_activity_flags = newFlags;
		}
	}
	const updatedUser = await users.patchUpsert(user.id, updates, user.toRow());
	await users.deleteEmailVerificationToken(data.token);
	await gateway.dispatchPresence({
		userId: user.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(updatedUser),
	});
	return true;
}

export async function resendVerificationEmail(ctx: ApiContext, user: User): Promise<void> {
	const {users, email, rateLimit} = ctx.services;
	assertNonBotUser(user);
	const allowReverification =
		user.suspiciousActivityFlags !== null &&
		((user.suspiciousActivityFlags & SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL) !== 0 ||
			(user.suspiciousActivityFlags & SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE) !== 0 ||
			(user.suspiciousActivityFlags & SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE) !== 0 ||
			(user.suspiciousActivityFlags & SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE) !== 0);
	if (user.emailVerified && !allowReverification) {
		return;
	}
	const limit = await rateLimit.checkLimit({
		identifier: `email_verification:${user.email!}`,
		maxAttempts: 3,
		windowMs: ms('15 minutes'),
	});
	if (!limit.allowed) {
		throw new RateLimitError({
			retryAfter: limit.retryAfter || 0,
			limit: limit.limit,
			resetTime: limit.resetTime,
		});
	}
	const emailVerifyToken = createEmailVerificationToken(await RandomUtils.randomString(64));
	await users.createEmailVerificationToken({
		token_: emailVerifyToken,
		user_id: user.id,
		email: user.email!,
	});
	await email.sendEmailVerification(user.email!, user.username, emailVerifyToken, user.locale);
}
