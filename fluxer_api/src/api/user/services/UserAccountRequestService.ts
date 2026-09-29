// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthEmailRevert from '@app/api/auth/AuthEmailRevert';
import {requireEmailVerified} from '@app/api/auth/EmailVerificationUtils';
import {requireSudoMode, type SudoVerificationResult} from '@app/api/auth/services/SudoVerificationService';
import {createChannelID, createGuildID, type UserID} from '@app/api/BrandedTypes';
import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {isBlockedEmailDomain} from '@app/api/infrastructure/activity/SharedLists';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {EmailChangeService} from '@app/api/user/services/EmailChangeService';
import type {UserAccountService} from '@app/api/user/services/UserAccountService';
import type {UserChannelService} from '@app/api/user/services/UserChannelService';
import {mapUserToPartialResponseWithCache} from '@app/api/user/UserCacheHelpers';
import {
	canUseProfileTimezone,
	createPremiumClearPatch,
	getEffectiveSuspiciousFlags,
	shouldStripExpiredPremium,
} from '@app/api/user/UserHelpers';
import {
	mapGuildMemberToProfileResponse,
	mapUserToPrivateResponse,
	mapUserToProfileResponse,
} from '@app/api/user/UserMappers';
import {extractEmailDomain} from '@app/api/utils/EmailDomainUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {getCurrentTimeZoneOffsetMinutes} from '@fluxer/date_utils/src/TimeZoneUtils';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {AccountSuspiciousActivityError} from '@fluxer/errors/src/domains/user/AccountSuspiciousActivityError';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {
	EmailChangeApplyRequest,
	UserUpdateWithVerificationRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {UserPrivateResponse, UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {Context} from 'hono';

type UserUpdatePayload = Omit<
	UserUpdateWithVerificationRequest,
	'mfa_method' | 'mfa_code' | 'webauthn_response' | 'webauthn_challenge' | 'email_token'
>;

const EMAIL_VERIFICATION_REQUIRED_PROFILE_UPDATE_FIELDS: ReadonlyArray<keyof UserUpdatePayload> = [
	'username',
	'discriminator',
	'global_name',
	'avatar',
	'banner',
	'bio',
	'pronouns',
	'accent_color',
	'timezone',
	'timezone_privacy_flags',
	'premium_badge_hidden',
	'premium_badge_masked',
	'premium_badge_timestamp_hidden',
	'premium_badge_sequence_hidden',
];

function hasProfileCustomizationUpdate(data: UserUpdatePayload): boolean {
	return EMAIL_VERIFICATION_REQUIRED_PROFILE_UPDATE_FIELDS.some((field) => data[field] !== undefined);
}

async function stripUnauthorizedProfileTimezoneUpdate(
	user: User,
	body: UserUpdateWithVerificationRequest,
): Promise<UserUpdateWithVerificationRequest> {
	if (body.timezone === undefined && body.timezone_privacy_flags === undefined) {
		return body;
	}
	if (await canUseProfileTimezone(user)) {
		return body;
	}
	const {timezone: _timezone, timezone_privacy_flags: _timezonePrivacyFlags, ...rest} = body;
	return rest;
}

function hasDefinedUserUpdatePayload(data: UserUpdatePayload): boolean {
	return Object.values(data).some((value) => value !== undefined);
}

function profileFieldsChanged(before: User, after: User): boolean {
	return (
		before.username !== after.username ||
		before.globalName !== after.globalName ||
		before.bio !== after.bio ||
		before.avatarHash !== after.avatarHash ||
		before.pronouns !== after.pronouns
	);
}

interface UserProfileParams {
	currentUserId: UserID;
	targetUserId: UserID;
	guildId?: bigint;
	withMutualFriends?: boolean;
	withMutualGuilds?: boolean;
	requestCache: RequestCache;
}

export class UserAccountRequestService {
	constructor(
		private readonly emailChangeService: EmailChangeService,
		private readonly userAccountService: UserAccountService,
		private readonly userChannelService: UserChannelService,
		private readonly userRepository: IUserRepository,
		private readonly userCacheService: UserCacheService,
	) {}

	getCurrentUserResponse(params: {
		authTokenType?: 'session' | 'bearer' | 'bot' | 'admin_api_key';
		oauthBearerScopes?: Set<string> | null;
		allowSuspicious?: boolean;
		user?: User;
	}): UserPrivateResponse {
		const tokenType = params.authTokenType;
		const allowSuspicious = params.allowSuspicious ?? false;
		if (tokenType === 'bearer') {
			const bearerUser = params.user;
			if (!bearerUser) {
				throw new UnauthorizedError();
			}
			if (!allowSuspicious) {
				this.enforceUserAccess(bearerUser);
			}
			const includeEmail = params.oauthBearerScopes?.has('email') ?? false;
			const response = mapUserToPrivateResponse(bearerUser);
			if (!includeEmail) {
				response.email = null;
			}
			this.stripBearerSensitiveFields(response);
			return response;
		}
		const user = params.user;
		if (user) {
			if (!allowSuspicious) {
				this.enforceUserAccess(user);
			}
			return mapUserToPrivateResponse(user);
		}
		throw new UnauthorizedError();
	}

	async updateCurrentUser(params: {
		ctx: Context<HonoEnv>;
		user: User;
		body: UserUpdateWithVerificationRequest;
		authSession: AuthSession;
	}): Promise<UserPrivateResponse> {
		const {ctx, body, authSession} = params;
		const {user} = params;
		const oldEmail = user.email;
		const sanitizedBody = await stripUnauthorizedProfileTimezoneUpdate(user, body);
		const {
			mfa_method: _mfaMethod,
			mfa_code: _mfaCode,
			webauthn_response: _webauthnResponse,
			webauthn_challenge: _webauthnChallenge,
			email_token: emailToken,
			...userUpdateDataRest
		} = sanitizedBody;
		let userUpdateData: UserUpdatePayload = userUpdateDataRest;
		const emailTokenProvided = emailToken !== undefined;
		if (!emailTokenProvided && !hasDefinedUserUpdatePayload(userUpdateData)) {
			return mapUserToPrivateResponse(user);
		}
		this.enforceSuspiciousSelfUpdateAllowance(user, sanitizedBody);
		if (userUpdateData.email !== undefined) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_MUST_BE_CHANGED_VIA_TOKEN);
		}
		const isUnclaimed = user.isUnclaimedAccount();
		if (!isUnclaimed && userUpdateData.new_password !== undefined && !userUpdateData.password) {
			throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_NOT_SET);
		}
		if (isUnclaimed) {
			const allowed = new Set(['new_password', 'has_dismissed_premium_onboarding', 'has_unread_gift_inventory']);
			const disallowedField = Object.keys(userUpdateData).find((key) => !allowed.has(key));
			if (disallowedField) {
				throw InputValidationError.fromCode(
					disallowedField,
					ValidationErrorCodes.UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN,
				);
			}
		}
		if (!isUnclaimed && hasProfileCustomizationUpdate(userUpdateData)) {
			requireEmailVerified(user, 'profile');
		}
		let emailFromToken: string | null = null;
		let emailVerifiedViaToken = false;
		const needsVerification = this.requiresSensitiveUserVerification(user, userUpdateData, emailTokenProvided);
		let sudoResult: SudoVerificationResult | null = null;
		if (needsVerification) {
			sudoResult = await requireSudoMode(ctx, user, sanitizedBody);
		}
		if (emailTokenProvided && emailToken) {
			emailFromToken = await this.emailChangeService.getTokenEmail(user.id, emailToken);
			userUpdateData = {...userUpdateData, email: emailFromToken};
			emailVerifiedViaToken = true;
			if (isBlockedEmailDomain(extractEmailDomain(emailFromToken))) {
				throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
			}
		}
		const updatedUser = await this.userAccountService.update({
			user,
			oldAuthSession: authSession,
			data: userUpdateData,
			request: ctx.req.raw,
			sudoContext: sudoResult ?? undefined,
			emailVerifiedViaToken,
		});
		if (emailTokenProvided && emailToken) {
			await this.emailChangeService.deleteToken(emailToken);
		}
		const emailActuallyChanged =
			!!emailFromToken &&
			!!updatedUser.email &&
			(oldEmail == null || oldEmail.toLowerCase() !== updatedUser.email.toLowerCase());
		if (emailActuallyChanged && updatedUser.email) {
			await emitActivity('email_changed', updatedUser.id.toString(), {
				user_id: updatedUser.id.toString(),
				new_email: updatedUser.email,
				was_unclaimed: isUnclaimed,
				has_ever_purchased: updatedUser.hasEverPurchased,
				suspicious_flags: updatedUser.suspiciousActivityFlags ?? 0,
				suspicious_flags_before: user.suspiciousActivityFlags ?? 0,
			});
		}
		if (profileFieldsChanged(user, updatedUser)) {
			await emitActivity('profile_updated', updatedUser.id.toString(), {
				user_id: updatedUser.id.toString(),
				username: updatedUser.username,
				global_name: updatedUser.globalName,
				bio: updatedUser.bio,
				avatar_hash: updatedUser.avatarHash,
				pronouns: updatedUser.pronouns,
			});
		}
		if (emailActuallyChanged && oldEmail) {
			try {
				await AuthEmailRevert.issueEmailRevertToken(ctx.get('apiContext'), {
					user: updatedUser,
					previousEmail: oldEmail,
					newEmail: updatedUser.email,
				});
			} catch (error) {
				Logger.warn({error, userId: updatedUser.id}, 'Failed to issue email revert token');
			}
		}
		return mapUserToPrivateResponse(updatedUser);
	}

	async applyEmailChange(params: {
		ctx: Context<HonoEnv>;
		user: User;
		body: EmailChangeApplyRequest;
		authSession: AuthSession;
	}): Promise<UserPrivateResponse> {
		return this.updateCurrentUser({
			ctx: params.ctx,
			user: params.user,
			body: params.body,
			authSession: params.authSession,
		});
	}

	async preloadMessages(params: {
		userId: UserID;
		channels: ReadonlyArray<bigint>;
		requestCache: RequestCache;
	}): Promise<Record<string, unknown>> {
		const channelIds = params.channels.map((channelId) => createChannelID(channelId));
		return this.userChannelService.preloadDMMessages({
			userId: params.userId,
			channelIds,
		});
	}

	async getUserProfile(params: UserProfileParams): Promise<UserProfileFullResponse> {
		const guildId = params.guildId ? createGuildID(params.guildId) : undefined;
		const profile = await this.userAccountService.lookupService.getUserProfile({
			userId: params.currentUserId,
			targetId: params.targetUserId,
			guildId,
			withMutualFriends: params.withMutualFriends,
			withMutualGuilds: params.withMutualGuilds,
			requestCache: params.requestCache,
		});
		let profileUser = profile.user;
		let premiumType = profile.premiumType;
		let premiumSince = profile.premiumSince;
		let premiumLifetimeSequence = profile.premiumLifetimeSequence;
		if (shouldStripExpiredPremium(profileUser)) {
			try {
				const sanitizedUser = await this.userRepository.patchUpsert(
					profileUser.id,
					createPremiumClearPatch(),
					profileUser.toRow(),
				);
				if (sanitizedUser) {
					profileUser = sanitizedUser;
					profile.user = sanitizedUser;
					premiumType = undefined;
					premiumSince = undefined;
					premiumLifetimeSequence = undefined;
				}
			} catch (error) {
				Logger.warn(
					{userId: profileUser.id.toString(), error},
					'Failed to sanitize expired premium fields before returning profile',
				);
			}
		}
		const restrictProfile = profile.restrictProfile;
		const userProfile = mapUserToProfileResponse(profileUser, {restrictProfile});
		const guildMemberProfile = mapGuildMemberToProfileResponse(profile.guildMemberDomain ?? null, {restrictProfile});
		const timezoneOffset = profile.timezoneVisible ? getCurrentTimeZoneOffsetMinutes(profileUser.timezone) : null;
		const mutualFriends = profile.mutualFriends
			? profile.mutualFriends.map((user) =>
					mapUserToPartialResponseWithCache({
						user,
						userCacheService: this.userCacheService,
						requestCache: params.requestCache,
					}),
				)
			: undefined;
		const connectedAccounts = profile.connections ? this.mapConnectionsToResponse(profile.connections) : undefined;
		return {
			user: mapUserToPartialResponseWithCache({
				user: profileUser,
				userCacheService: this.userCacheService,
				requestCache: params.requestCache,
			}),
			user_profile: userProfile,
			guild_member: profile.guildMember ?? undefined,
			guild_member_profile: guildMemberProfile ?? undefined,
			premium_type: premiumType,
			premium_since: premiumSince?.toISOString(),
			premium_lifetime_sequence: premiumLifetimeSequence,
			mutual_friends: mutualFriends,
			mutual_guilds: profile.mutualGuilds,
			connected_accounts: connectedAccounts,
			timezone_offset: timezoneOffset,
			profile_limited: restrictProfile ? true : undefined,
		};
	}

	checkTagAvailability(params: {currentUser: User; username: string; discriminator: number}): boolean {
		const currentUser = params.currentUser;
		const discriminator = params.discriminator;
		if (
			params.username.toLowerCase() === currentUser.username.toLowerCase() &&
			discriminator === currentUser.discriminator
		) {
			return false;
		}
		return true;
	}

	private stripBearerSensitiveFields(response: UserPrivateResponse): void {
		response.acls = [];
		response.traits = [];
		response.email_bounced = undefined;
		response.mfa_enabled = false;
		response.authenticator_types = undefined;
		response.password_last_changed_at = null;
		response.required_actions = [];
		response.nsfw_allowed = false;
		response.premium_since = null;
		response.premium_until = null;
		response.premium_will_cancel = false;
		response.premium_billing_cycle = null;
		response.premium_lifetime_sequence = null;
		response.premium_badge_hidden = false;
		response.premium_badge_masked = false;
		response.premium_badge_timestamp_hidden = false;
		response.premium_badge_sequence_hidden = false;
		response.premium_purchase_disabled = false;
		response.premium_enabled_override = false;
		response.premium_perks_disabled = false;
		response.has_dismissed_premium_onboarding = false;
		response.has_ever_purchased = false;
		response.has_unread_gift_inventory = false;
		response.unread_gift_inventory_count = 0;
		response.pending_bulk_message_deletion = null;
	}

	private enforceUserAccess(user: User): void {
		const flags = getEffectiveSuspiciousFlags(user);
		if (flags !== 0) {
			throw new AccountSuspiciousActivityError(flags);
		}
	}

	private enforceSuspiciousSelfUpdateAllowance(user: User, body: UserUpdateWithVerificationRequest): void {
		const flags = getEffectiveSuspiciousFlags(user);
		if (flags === 0) {
			return;
		}
		if (this.isAllowedSuspiciousRecoveryUpdate(body)) {
			return;
		}
		throw new AccountSuspiciousActivityError(flags);
	}

	private isAllowedSuspiciousRecoveryUpdate(body: UserUpdateWithVerificationRequest): boolean {
		if (!body.email_token) {
			return false;
		}
		const allowedKeys = new Set([
			'email_token',
			'password',
			'mfa_method',
			'mfa_code',
			'webauthn_response',
			'webauthn_challenge',
		]);
		for (const [key, value] of Object.entries(body)) {
			if (value === undefined) {
				continue;
			}
			if (!allowedKeys.has(key)) {
				return false;
			}
		}
		return true;
	}

	private requiresSensitiveUserVerification(user: User, data: UserUpdatePayload, emailTokenProvided: boolean): boolean {
		const isUnclaimed = user.isUnclaimedAccount();
		const usernameChanged = data.username !== undefined && data.username !== user.username;
		const discriminatorChanged = data.discriminator !== undefined && data.discriminator !== user.discriminator;
		const emailChanged = data.email !== undefined && data.email !== user.email;
		const newPasswordProvided = data.new_password !== undefined;
		if (isUnclaimed) {
			return usernameChanged || discriminatorChanged;
		}
		return usernameChanged || discriminatorChanged || emailTokenProvided || emailChanged || newPasswordProvided;
	}

	private mapConnectionsToResponse(connections: Array<UserConnectionRow>): Array<ConnectionResponse> {
		return connections
			.sort((a, b) => a.sort_order - b.sort_order)
			.map((connection) => ({
				id: connection.connection_id,
				type: connection.connection_type,
				name: connection.name,
				verified: connection.verified,
				visibility_flags: connection.visibility_flags,
				sort_order: connection.sort_order,
			}));
	}
}
