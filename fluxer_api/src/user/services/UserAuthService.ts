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

import type {AuthService} from '~/auth/AuthService';
import type {SudoVerificationResult} from '~/auth/services/SudoVerificationService';
import {userHasMfa} from '~/auth/services/SudoVerificationService';
import {createEmailVerificationToken} from '~/BrandedTypes';
import {UserAuthenticatorTypes} from '~/Constants';
import {InputValidationError, MfaNotDisabledError, MfaNotEnabledError} from '~/Errors';
import {SudoModeRequiredError} from '~/errors/SudoModeRequiredError';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {MfaBackupCode, User} from '~/Models';
import type {BotMfaMirrorService} from '~/oauth/BotMfaMirrorService';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import * as RandomUtils from '~/utils/RandomUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserAuthRepository} from '../repositories/IUserAuthRepository';

export class UserAuthService {
	constructor(
		private userAccountRepository: IUserAccountRepository,
		private userAuthRepository: IUserAuthRepository,
		private authService: AuthService,
		private emailService: IEmailService,
		private gatewayService: IGatewayService,
		private botMfaMirrorService?: BotMfaMirrorService,
	) {}

	async enableMfaTotp(params: {user: User; secret: string; code: string}): Promise<Array<MfaBackupCode>> {
		const {user, secret, code} = params;
		if (user.totpSecret) throw new MfaNotDisabledError();

		const userId = user.id;
		if (!(await this.authService.verifyMfaCode({userId: user.id, mfaSecret: secret, code}))) {
			throw InputValidationError.create('code', 'Invalid code');
		}

		const authenticatorTypes = user.authenticatorTypes || new Set<number>();
		authenticatorTypes.add(UserAuthenticatorTypes.TOTP);
		const updatedUser = await this.userAccountRepository.patchUpsert(userId, {
			totp_secret: secret,
			authenticator_types: authenticatorTypes,
		});
		const newBackupCodes = this.authService.generateBackupCodes();
		const mfaBackupCodes = await this.userAuthRepository.createMfaBackupCodes(userId, newBackupCodes);
		await this.dispatchUserUpdate(updatedUser!);
		if (updatedUser) {
			await this.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
		}
		return mfaBackupCodes;
	}

	async disableMfaTotp(params: {user: User; code: string; sudoContext: SudoVerificationResult}): Promise<void> {
		const {user, code, sudoContext} = params;
		if (!user.totpSecret) throw new MfaNotEnabledError();

		const identityVerifiedViaSudo = sudoContext.method === 'mfa' || sudoContext.method === 'sudo_token';
		const identityVerifiedViaPassword = sudoContext.method === 'password';
		const hasMfa = userHasMfa(user);
		if (!identityVerifiedViaSudo && !identityVerifiedViaPassword) {
			throw new SudoModeRequiredError(hasMfa);
		}

		if (
			!(await this.authService.verifyMfaCode({
				userId: user.id,
				mfaSecret: user.totpSecret,
				code,
				allowBackup: true,
			}))
		) {
			throw InputValidationError.create('code', 'Invalid code');
		}

		const userId = user.id;

		const authenticatorTypes = user.authenticatorTypes || new Set<number>();
		authenticatorTypes.delete(UserAuthenticatorTypes.TOTP);
		const hasSms = authenticatorTypes.has(UserAuthenticatorTypes.SMS);
		if (hasSms) {
			authenticatorTypes.delete(UserAuthenticatorTypes.SMS);
		}

		const updatedUser = await this.userAccountRepository.patchUpsert(userId, {
			totp_secret: null,
			authenticator_types: authenticatorTypes,
		});
		await this.userAuthRepository.clearMfaBackupCodes(userId);
		await this.dispatchUserUpdate(updatedUser!);
		if (updatedUser) {
			await this.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
		}
	}

	async getMfaBackupCodes(params: {
		user: User;
		regenerate: boolean;
		sudoContext: SudoVerificationResult;
	}): Promise<Array<MfaBackupCode>> {
		const {user, regenerate, sudoContext} = params;
		const identityVerifiedViaSudo = sudoContext.method === 'mfa' || sudoContext.method === 'sudo_token';
		const identityVerifiedViaPassword = sudoContext.method === 'password';
		const hasMfa = userHasMfa(user);
		if (!identityVerifiedViaSudo && !identityVerifiedViaPassword) {
			throw new SudoModeRequiredError(hasMfa);
		}

		if (regenerate) {
			return this.regenerateMfaBackupCodes(user);
		}

		return await this.userAuthRepository.listMfaBackupCodes(user.id);
	}

	async regenerateMfaBackupCodes(user: User): Promise<Array<MfaBackupCode>> {
		const userId = user.id;
		const newBackupCodes = this.authService.generateBackupCodes();
		await this.userAuthRepository.clearMfaBackupCodes(userId);
		return await this.userAuthRepository.createMfaBackupCodes(userId, newBackupCodes);
	}

	async verifyEmail(token: string): Promise<boolean> {
		const emailToken = await this.userAuthRepository.getEmailVerificationToken(token);
		if (!emailToken) {
			return false;
		}
		const user = await this.userAccountRepository.findUnique(emailToken.userId);
		if (!user) {
			return false;
		}
		const updatedUser = await this.userAccountRepository.patchUpsert(emailToken.userId, {
			email: emailToken.email,
			email_verified: true,
		});
		await this.userAuthRepository.deleteEmailVerificationToken(token);
		if (updatedUser) {
			await this.dispatchUserUpdate(updatedUser);
		}
		return true;
	}

	async resendVerificationEmail(user: User): Promise<boolean> {
		if (user.emailVerified) {
			return true;
		}
		const verificationToken = createEmailVerificationToken(RandomUtils.randomString(64));
		await this.userAuthRepository.createEmailVerificationToken({
			token_: verificationToken,
			user_id: user.id,
			email: user.email!,
		});
		await this.emailService.sendEmailVerification(user.email!, user.username, verificationToken, user.locale);
		return true;
	}

	async dispatchUserUpdate(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}
}
