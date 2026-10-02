// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {createEmailVerificationToken} from '@app/api/BrandedTypes';
import type {User} from '@app/api/models/User';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {BotUserAuthEndpointAccessDeniedError} from '@fluxer/errors/src/domains/auth/BotUserAuthEndpointAccessDeniedError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import type {VerifyEmailRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ms} from 'itty-time';

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
	const updatedUser = await users.patchUpsert(user.id, {email_verified: true, email_bounced: false}, user.toRow());
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
	if (user.emailVerified) {
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
