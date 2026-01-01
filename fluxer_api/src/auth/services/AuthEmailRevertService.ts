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

import type {UserID} from '~/BrandedTypes';
import {createEmailRevertToken} from '~/BrandedTypes';
import {InputValidationError, UnauthorizedError} from '~/Errors';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {getUserSearchService} from '~/Meilisearch';
import type {AuthSession, User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import type {UserContactChangeLogService} from '~/user/services/UserContactChangeLogService';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import * as IpUtils from '~/utils/IpUtils';

interface IssueTokenParams {
	user: User;
	previousEmail: string;
	newEmail: string;
}

interface RevertParams {
	token: string;
	password: string;
	request: Request;
}

export class AuthEmailRevertService {
	constructor(
		private readonly repository: IUserRepository,
		private readonly emailService: IEmailService,
		private readonly gatewayService: IGatewayService,
		private readonly hashPassword: (password: string) => Promise<string>,
		private readonly isPasswordPwned: (password: string) => Promise<boolean>,
		private readonly handleBanStatus: (user: User) => Promise<User>,
		private readonly assertNonBotUser: (user: User) => void,
		private readonly generateSecureToken: () => Promise<string>,
		private readonly createAuthSession: (params: {user: User; request: Request}) => Promise<[string, AuthSession]>,
		private readonly terminateAllUserSessions: (userId: UserID) => Promise<void>,
		private readonly contactChangeLogService: UserContactChangeLogService,
	) {}

	async issueRevertToken(params: IssueTokenParams): Promise<void> {
		const {user, previousEmail, newEmail} = params;
		const trimmed = previousEmail.trim();
		if (!trimmed) return;

		const token = createEmailRevertToken(await this.generateSecureToken());
		await this.repository.createEmailRevertToken({
			token_: token,
			user_id: user.id,
			email: trimmed,
		});

		await this.emailService.sendEmailChangeRevert(trimmed, user.username, newEmail, token, user.locale);
	}

	async revertEmailChange(params: RevertParams): Promise<{user_id: string; token: string}> {
		const {token, password, request} = params;
		const tokenData = await this.repository.getEmailRevertToken(token);
		if (!tokenData) {
			throw InputValidationError.create('token', 'Invalid or expired revert token');
		}

		const user = await this.repository.findUnique(tokenData.userId);
		if (!user) {
			throw InputValidationError.create('token', 'Invalid or expired revert token');
		}

		this.assertNonBotUser(user);
		await this.handleBanStatus(user);

		if (await this.isPasswordPwned(password)) {
			throw InputValidationError.create('password', 'Password is too common');
		}

		const passwordHash = await this.hashPassword(password);
		const now = new Date();

		const updatedUser = await this.repository.patchUpsert(user.id, {
			email: tokenData.email,
			email_verified: true,
			phone: null,
			totp_secret: null,
			authenticator_types: null,
			password_hash: passwordHash,
			password_last_changed_at: now,
		});

		if (!updatedUser) {
			throw new UnauthorizedError();
		}

		await this.repository.deleteEmailRevertToken(token);
		await this.repository.deleteAllMfaBackupCodes(user.id);
		await this.repository.deleteAllWebAuthnCredentials(user.id);
		await this.repository.deleteAllAuthorizedIps(user.id);
		await this.terminateAllUserSessions(user.id);
		await this.repository.createAuthorizedIp(user.id, IpUtils.requireClientIp(request));

		const userSearchService = getUserSearchService();
		if (userSearchService && updatedUser) {
			await userSearchService.updateUser(updatedUser).catch(() => {});
		}

		await this.gatewayService.dispatchPresence({
			userId: updatedUser.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser),
		});

		const [authToken] = await this.createAuthSession({user: updatedUser, request});

		await this.contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser,
			reason: 'user_requested',
			actorUserId: user.id,
		});

		return {user_id: updatedUser.id.toString(), token: authToken};
	}
}
