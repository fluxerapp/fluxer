// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {SudoModeRequiredError} from '@fluxer/errors/src/domains/auth/SudoModeRequiredError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {UserUpdateRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {ApiContext} from '../../ApiContext';
import {Config} from '../../Config';
import * as AuthPassword from '../../auth/AuthPassword';
import * as AuthSession from '../../auth/AuthSession';
import type {SudoVerificationResult} from '../../auth/services/SudoVerificationService';
import {deriveSudoMethods, userHasMfa} from '../../auth/services/SudoVerificationService';
import type {UserRow} from '../../database/types/UserTypes';
import type {IDiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../limits/LimitMatchContextBuilder';
import {profileSubstringBlocklistCache} from '../../middleware/ProfileSubstringBlocklistCache';
import type {AuthSession as AuthSessionModel} from '../../models/AuthSession';
import type {User} from '../../models/User';
import {enforceFluxerTagChangeRateLimit} from '../FluxerTagChangeRateLimit';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import {isProfileSubstringExempt} from '../UserHelpers';

interface UserUpdateMetadata {
	invalidateAuthSessions?: boolean;
}

type UserFieldUpdates = Partial<UserRow>;

interface UserAccountSecurityServiceDeps {
	apiContext: ApiContext;
	userAccountRepository: IUserAccountRepository;
	discriminatorService: IDiscriminatorService;
	rateLimitService: IRateLimitService;
	limitConfigService: LimitConfigService;
}

export class UserAccountSecurityService {
	constructor(private readonly deps: UserAccountSecurityServiceDeps) {}

	async processSecurityUpdates(params: {
		user: User;
		data: UserUpdateRequest;
		sudoContext?: SudoVerificationResult;
	}): Promise<{
		updates: UserFieldUpdates;
		metadata: UserUpdateMetadata;
	}> {
		const {user, data, sudoContext} = params;
		const updates: UserFieldUpdates = {
			password_hash: user.passwordHash,
			username: user.username,
			discriminator: user.discriminator,
			global_name: user.isBot ? null : user.globalName,
			email: user.email,
		};
		const metadata: UserUpdateMetadata = {
			invalidateAuthSessions: false,
		};
		const isUnclaimedAccount = user.isUnclaimedAccount();
		const identityVerifiedViaSudo = sudoContext?.method === 'mfa' || sudoContext?.method === 'sudo_token';
		const identityVerifiedViaPassword = sudoContext?.method === 'password';
		const hasMfa = userHasMfa(user);
		const rawEmail = data.email?.trim();
		const normalizedEmail = rawEmail?.toLowerCase();
		const hasPasswordRequiredChanges =
			(data.username !== undefined && data.username !== user.username) ||
			(data.discriminator !== undefined && data.discriminator !== user.discriminator) ||
			(data.email !== undefined && normalizedEmail !== user.email?.toLowerCase()) ||
			data.new_password !== undefined;
		const requiresVerification = hasPasswordRequiredChanges && !isUnclaimedAccount;
		if (requiresVerification && !identityVerifiedViaSudo && !identityVerifiedViaPassword) {
			throw new SudoModeRequiredError(hasMfa, deriveSudoMethods(user));
		}
		if (isUnclaimedAccount && data.new_password) {
			updates.password_hash = await this.hashNewPassword(data.new_password);
			updates.password_last_changed_at = new Date();
			metadata.invalidateAuthSessions = false;
		} else if (data.new_password) {
			if (!data.password) {
				throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_NOT_SET);
			}
			if (!identityVerifiedViaSudo && !identityVerifiedViaPassword) {
				throw new SudoModeRequiredError(hasMfa, deriveSudoMethods(user));
			}
			updates.password_hash = await this.hashNewPassword(data.new_password);
			updates.password_last_changed_at = new Date();
			metadata.invalidateAuthSessions = true;
		}
		if (data.username !== undefined) {
			const {newUsername, newDiscriminator} = await this.updateUsername({
				user,
				username: data.username,
				requestedDiscriminator: data.discriminator,
			});
			if (
				!isProfileSubstringExempt(user) &&
				profileSubstringBlocklistCache.containsBannedSubstring('username', newUsername)
			) {
				throw new ContentBlockedError();
			}
			updates.username = newUsername;
			updates.discriminator = newDiscriminator;
		} else if (data.discriminator !== undefined) {
			updates.discriminator = await this.updateDiscriminator({user, discriminator: data.discriminator});
		}
		await this.enforceFluxerTagChangeRateLimit({
			user,
			nextUsername: updates.username ?? user.username,
			nextDiscriminator: updates.discriminator ?? user.discriminator,
			errorPath: data.discriminator !== undefined && data.username === undefined ? 'discriminator' : 'username',
		});
		const usernameRealChange =
			data.username !== undefined && data.username.toLowerCase() !== user.username.toLowerCase();
		const discriminatorChanged = updates.discriminator !== user.discriminator;
		const shouldMarkPremiumDiscriminator =
			(discriminatorChanged || usernameRealChange) &&
			user.isPremium() &&
			user.premiumType !== UserPremiumTypes.LIFETIME;
		if (shouldMarkPremiumDiscriminator && (user.premiumFlags & PremiumFlags.DISCRIMINATOR) === 0) {
			updates.premium_flags = user.premiumFlags | PremiumFlags.DISCRIMINATOR;
		}
		if (user.isBot) {
			updates.global_name = null;
		} else if (data.global_name !== undefined) {
			if (data.global_name !== user.globalName) {
			}
			if (
				data.global_name &&
				!isProfileSubstringExempt(user) &&
				profileSubstringBlocklistCache.containsBannedSubstring('global_name', data.global_name)
			) {
				throw new ContentBlockedError();
			}
			updates.global_name = data.global_name;
		}
		if (rawEmail) {
			if (normalizedEmail && normalizedEmail !== user.email?.toLowerCase()) {
				const existing = await this.deps.userAccountRepository.findByEmail(normalizedEmail);
				if (existing && existing.id !== user.id) {
					throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
				}
			}
			updates.email = rawEmail;
		}
		return {updates, metadata};
	}

	async invalidateAndRecreateSessions({
		user,
		oldAuthSession,
		request,
	}: {
		user: User;
		oldAuthSession: AuthSessionModel;
		request: Request;
	}): Promise<void> {
		await AuthSession.replaceCurrentAuthSession(this.deps.apiContext, {
			user,
			currentAuthSession: oldAuthSession,
			request,
		});
	}

	private async hashNewPassword(newPassword: string): Promise<string> {
		if (await AuthPassword.isPasswordPwned(this.deps.apiContext, newPassword)) {
			throw InputValidationError.fromCode('new_password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
		}
		return await AuthPassword.hashPassword(this.deps.apiContext, newPassword);
	}

	private async updateUsername({
		user,
		username,
		requestedDiscriminator,
	}: {
		user: User;
		username: string;
		requestedDiscriminator?: number;
	}): Promise<{
		newUsername: string;
		newDiscriminator: number;
	}> {
		const normalizedRequestedDiscriminator =
			requestedDiscriminator == null ? undefined : Number(requestedDiscriminator);
		if (
			user.username.toLowerCase() === username.toLowerCase() &&
			(normalizedRequestedDiscriminator === undefined || normalizedRequestedDiscriminator === user.discriminator)
		) {
			return {
				newUsername: username,
				newDiscriminator: user.discriminator,
			};
		}
		const ctx = createLimitMatchContext({user});
		const hasCustomDiscriminator = resolveLimitSafe(
			this.deps.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_custom_discriminator',
			0,
		);
		const isCaseOnlyChange = user.username.toLowerCase() === username.toLowerCase();
		if (hasCustomDiscriminator === 0) {
			if (isCaseOnlyChange) {
				return {
					newUsername: username,
					newDiscriminator: user.discriminator,
				};
			}
			const discriminatorResult = await this.deps.discriminatorService.generateDiscriminator({
				username,
				requestedDiscriminator: undefined,
				user: undefined,
			});
			if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
				throw InputValidationError.fromCode(
					'username',
					ValidationErrorCodes.TOO_MANY_USERS_WITH_USERNAME_TRY_DIFFERENT,
				);
			}
			return {
				newUsername: username,
				newDiscriminator: discriminatorResult.discriminator,
			};
		}
		const discriminatorToUse = normalizedRequestedDiscriminator ?? user.discriminator;
		if (this.requiresVisionaryForDiscriminator0000(user, discriminatorToUse)) {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.VISIONARY_REQUIRED_FOR_DISCRIMINATOR);
		}
		const discriminatorResult = await this.deps.discriminatorService.generateDiscriminator({
			username,
			requestedDiscriminator: discriminatorToUse,
			user,
		});
		if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
			throw InputValidationError.fromCode(
				'username',
				discriminatorToUse !== undefined
					? ValidationErrorCodes.TAG_ALREADY_TAKEN
					: ValidationErrorCodes.TOO_MANY_USERS_WITH_USERNAME_TRY_DIFFERENT,
			);
		}
		return {
			newUsername: username,
			newDiscriminator: discriminatorResult.discriminator,
		};
	}

	private async updateDiscriminator({user, discriminator}: {user: User; discriminator: number}): Promise<number> {
		const ctx = createLimitMatchContext({user});
		const hasCustomDiscriminator = resolveLimitSafe(
			this.deps.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_custom_discriminator',
			0,
		);
		if (hasCustomDiscriminator === 0) {
			throw InputValidationError.fromCode(
				'discriminator',
				ValidationErrorCodes.CHANGING_DISCRIMINATOR_REQUIRES_PREMIUM,
			);
		}
		if (this.requiresVisionaryForDiscriminator0000(user, discriminator)) {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.VISIONARY_REQUIRED_FOR_DISCRIMINATOR);
		}
		const discriminatorResult = await this.deps.discriminatorService.generateDiscriminator({
			username: user.username,
			requestedDiscriminator: discriminator,
			user,
		});
		if (!discriminatorResult.available) {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.TAG_ALREADY_TAKEN);
		}
		return discriminator;
	}

	private requiresVisionaryForDiscriminator0000(user: User, discriminator: number): boolean {
		if (Config.instance.selfHosted) {
			return false;
		}
		return discriminator === 0 && user.premiumType !== UserPremiumTypes.LIFETIME;
	}

	private async enforceFluxerTagChangeRateLimit(params: {
		user: User;
		nextUsername: string;
		nextDiscriminator: number;
		errorPath: 'username' | 'discriminator';
	}): Promise<void> {
		const {user, nextUsername, nextDiscriminator, errorPath} = params;
		if (nextUsername === user.username && nextDiscriminator === user.discriminator) {
			return;
		}
		await enforceFluxerTagChangeRateLimit({
			rateLimitService: this.deps.rateLimitService,
			userId: user.id,
			errorPath,
		});
	}
}
