// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthSession from '@app/api/auth/AuthSession';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {createEmailRevertToken} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {enqueueStripeCustomerEmailSync} from '@app/api/stripe/StripeCustomer';
import {EMAIL_REVERT_TOKEN_TTL_SECONDS} from '@app/api/user/repositories/auth/TokenRepository';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';
import {hashPassword} from '@app/api/utils/PasswordUtils';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

interface IssueEmailRevertTokenParams {
	user: User;
	previousEmail: string;
	newEmail: string;
}

interface RevertEmailChangeParams {
	token: string;
	password: string;
	request: Request;
}

export async function issueEmailRevertToken(ctx: ApiContext, params: IssueEmailRevertTokenParams): Promise<void> {
	const {users, email} = ctx.services;
	const {user, previousEmail, newEmail} = params;
	const trimmed = previousEmail.trim();
	if (!trimmed) return;
	const token = createEmailRevertToken(await RandomUtils.randomString(64));
	await users.createEmailRevertToken({
		token_: token,
		user_id: user.id,
		email: trimmed,
	});
	await email.sendEmailChangeRevert(trimmed, user.username, newEmail, token, user.locale);
}

const EMAIL_REVERT_CLAIM_KEY_PREFIX = 'auth:email-revert:claimed:';

function emailRevertClaimKey(token: string): string {
	return `${EMAIL_REVERT_CLAIM_KEY_PREFIX}${crypto.createHash('sha256').update(token).digest('hex')}`;
}

function claimEmailRevertToken(token: string): Promise<boolean> {
	return getKVClient().setnx(emailRevertClaimKey(token), '1', EMAIL_REVERT_TOKEN_TTL_SECONDS);
}

async function releaseEmailRevertTokenClaim(token: string): Promise<void> {
	try {
		await getKVClient().del(emailRevertClaimKey(token));
	} catch (error) {
		Logger.error({error}, 'Could not release the claim of an email revert token whose revert failed');
	}
}

export async function revertEmailChange(
	ctx: ApiContext,
	params: RevertEmailChangeParams,
): Promise<{
	user_id: string;
	token: string;
}> {
	const {users, gateway, contactChangeLog, config, worker} = ctx.services;
	const {token, password, request} = params;
	const tokenData = await users.getEmailRevertToken(token);
	if (!tokenData) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_REVERT_TOKEN);
	}
	const user = await users.findUnique(tokenData.userId);
	if (!user) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_REVERT_TOKEN);
	}
	AuthUtility.assertNonBotUser(ctx, user);
	await AuthUtility.assertSignInAllowed(ctx, user);
	if (await AuthPassword.isPasswordPwned(ctx, password)) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	if (!(await claimEmailRevertToken(token))) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_REVERT_TOKEN);
	}
	let updatedUser: User;
	try {
		const passwordHash = await hashPassword(password);
		updatedUser = await users.patchUpsert(
			user.id,
			{
				email: tokenData.email,
				email_verified: true,
				totp_secret: null,
				authenticator_types: null,
				password_hash: passwordHash,
				password_last_changed_at: new Date(),
			},
			user.toRow(),
		);
	} catch (error) {
		await releaseEmailRevertTokenClaim(token);
		throw error;
	}
	await users.deleteAllPasswordResetTokens(user.id);
	await users.deleteEmailRevertToken(token);
	await users.deleteAllMfaBackupCodes(user.id);
	await users.deleteAllWebAuthnCredentials(user.id);
	await users.deleteAllAuthorizedIps(user.id);
	await revokeAllAuthSessions(ctx.services, user.id);
	await users.createAuthorizedIp(user.id, requireConfiguredClientIp(request, config.proxy));
	await dispatchUserUpdate(gateway, updatedUser);
	const [authToken] = await AuthSession.createAuthSession(ctx, {
		user: updatedUser,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	await contactChangeLog.recordDiff({
		oldUser: user,
		newUser: updatedUser,
		reason: 'user_requested',
		actorUserId: user.id,
	});
	await enqueueStripeCustomerEmailSync(worker, user, updatedUser);
	return {user_id: updatedUser.id.toString(), token: authToken};
}
