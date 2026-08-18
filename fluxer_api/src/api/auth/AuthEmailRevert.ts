// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {ApiContext} from '../ApiContext';
import {createEmailRevertToken} from '../BrandedTypes';
import {Logger} from '../Logger';
import type {User} from '../models/User';
import {getUserSearchService} from '../SearchFactory';
import {mapUserToPrivateResponse} from '../user/UserMappers';
import * as AuthPassword from './AuthPassword';
import * as AuthSession from './AuthSession';
import * as AuthUtility from './AuthUtility';

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
	const token = createEmailRevertToken(await AuthUtility.generateSecureToken(ctx));
	await users.createEmailRevertToken({
		token_: token,
		user_id: user.id,
		email: trimmed,
	});
	await email.sendEmailChangeRevert(trimmed, user.username, newEmail, token, user.locale);
}

export async function revertEmailChange(
	ctx: ApiContext,
	params: RevertEmailChangeParams,
): Promise<{
	user_id: string;
	token: string;
}> {
	const {users, gateway, contactChangeLog, config} = ctx.services;
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
	await AuthUtility.handleBanStatus(ctx, user);
	if (await AuthPassword.isPasswordPwned(ctx, password)) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const passwordHash = await AuthPassword.hashPassword(ctx, password);
	const now = new Date();
	const updatedUser = await users.patchUpsert(
		user.id,
		{
			email: tokenData.email,
			email_verified: true,
			totp_secret: null,
			authenticator_types: null,
			password_hash: passwordHash,
			password_last_changed_at: now,
		},
		user.toRow(),
	);
	await users.deleteEmailRevertToken(token);
	await users.deleteAllMfaBackupCodes(user.id);
	await users.deleteAllWebAuthnCredentials(user.id);
	await users.deleteAllAuthorizedIps(user.id);
	await AuthSession.terminateAllUserSessions(ctx, user.id);
	await users.createAuthorizedIp(
		user.id,
		requireClientIp(request, {
			trustClientIpHeader: config.proxy.trust_client_ip_header,
			clientIpHeaderName: config.proxy.client_ip_header,
		}),
	);
	const userSearchService = getUserSearchService();
	if (userSearchService && updatedUser && 'updateUser' in userSearchService) {
		await userSearchService
			.updateUser(updatedUser)
			.catch((error) =>
				Logger.debug({error, userId: updatedUser.id}, 'Failed to update search index after email revert'),
			);
	}
	await gateway.dispatchPresence({
		userId: updatedUser.id,
		event: 'USER_UPDATE',
		data: mapUserToPrivateResponse(updatedUser),
	});
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
	return {user_id: updatedUser.id.toString(), token: authToken};
}
