// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFERRED_PHONE_ON_COMMUNITY_JOIN, imposePhoneRequirements} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {getCurrentTimeZoneOffsetMinutes} from '@fluxer/date_utils/src/TimeZoneUtils';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {AccountSuspiciousActivityError} from '@fluxer/errors/src/domains/user/AccountSuspiciousActivityError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {
	EmailChangeApplyRequest,
	UserUpdateWithVerificationRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {UserPrivateResponse, UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {Context} from 'hono';
import type {z} from 'zod';
import * as AuthEmailRevert from '../../auth/AuthEmailRevert';
import {requireEmailVerified} from '../../auth/EmailVerificationUtils';
import type {IRegistrationRiskEvaluator} from '../../auth/services/IRegistrationRiskEvaluator';
import {requireSudoMode, type SudoVerificationResult} from '../../auth/services/SudoVerificationService';
import {createChannelID, createGuildID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {UserConnectionRow} from '../../database/types/ConnectionTypes';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {AuthSession} from '../../models/AuthSession';
import type {User} from '../../models/User';
import {createAccountPolicyContactContext, type IAccountPolicyEvaluator} from '../../risk/AccountPolicyEvaluator';
import type {IRegistrationEventsRepository} from '../../risk/adapters/VelocityAdapter';
import type {IRiskHistoryRepository} from '../../risk/HistoricalOutcomeRepository';
import {derivePlusAddressBase} from '../../risk/PlusAddressUtils';
import type {IRiskAssessmentRepository} from '../../risk/RiskAssessmentRepository';
import {deriveLatestRiskContext} from '../../risk/RiskHistoryContext';
import {
	RecommendedAction,
	type RiskAssessment,
	RiskConfidence,
	RiskDecisionMethod,
	RiskLevel,
} from '../../risk/RiskTypes';
import type {HonoEnv} from '../../types/HonoEnv';
import type {IUserRepository} from '../IUserRepository';
import {mapUserToPartialResponseWithCache} from '../UserCacheHelpers';
import {
	canUseProfileTimezone,
	createPremiumClearPatch,
	getEffectiveSuspiciousFlags,
	shouldStripExpiredPremium,
} from '../UserHelpers';
import {mapGuildMemberToProfileResponse, mapUserToPrivateResponse, mapUserToProfileResponse} from '../UserMappers';
import type {EmailChangeService} from './EmailChangeService';
import type {UserAccountService} from './UserAccountService';
import type {UserChannelService} from './UserChannelService';

export type UserUpdateWithVerificationRequestData = z.infer<typeof UserUpdateWithVerificationRequest>;
type UserUpdatePayload = Omit<
	UserUpdateWithVerificationRequestData,
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

function stripUnauthorizedProfileTimezoneUpdate(
	user: User,
	body: UserUpdateWithVerificationRequestData,
): UserUpdateWithVerificationRequestData {
	if (canUseProfileTimezone(user)) {
		return body;
	}
	const {timezone: _timezone, timezone_privacy_flags: _timezonePrivacyFlags, ...rest} = body;
	return rest as UserUpdateWithVerificationRequestData;
}

function hasDefinedUserUpdatePayload(data: UserUpdatePayload): boolean {
	return Object.values(data).some((value) => value !== undefined);
}

function createPolicyOnlyEmailSetAssessment(): RiskAssessment {
	return {
		suspicious: false,
		level: RiskLevel.Low,
		confidence: RiskConfidence.Low,
		riskScore: 0,
		reasoning: 'email-set policy evaluation',
		recommendedAction: RecommendedAction.Allow,
		method: RiskDecisionMethod.Noop,
		modelUsed: 'account-policy',
		rounds: 0,
		elapsedMs: 0,
		signals: {},
	};
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
		private readonly isEmailDomainSuspicious: (domain: string) => Promise<boolean>,
		private readonly isEmailDomainDisposable: (domain: string) => Promise<boolean>,
		private readonly registrationRiskEvaluator: IRegistrationRiskEvaluator,
		private readonly accountPolicyEvaluator: IAccountPolicyEvaluator,
		private readonly registrationEventsRepository: IRegistrationEventsRepository,
		private readonly riskAssessmentRepository: IRiskAssessmentRepository,
		private readonly riskHistoryRepository: Pick<
			IRiskHistoryRepository,
			'upsertLatestContext' | 'recordOutcomeForUser'
		>,
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
		body: UserUpdateWithVerificationRequestData;
		authSession: AuthSession;
	}): Promise<UserPrivateResponse> {
		const {ctx, body, authSession} = params;
		let {user} = params;
		const oldEmail = user.email;
		const sanitizedBody = stripUnauthorizedProfileTimezoneUpdate(user, body);
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
			const request = ctx.req.raw;
			const userAgent = request.headers.get('user-agent');
			const currentSuspiciousFlags = user.suspiciousActivityFlags ?? 0;
			let nextSuspiciousFlags = currentSuspiciousFlags;
			let emailSetRiskAssessment: RiskAssessment | null = null;
			let emailSetRecommendedAction = RecommendedAction.Allow;
			let emailSetRiskIp: string | null = null;
			const contactContext = createAccountPolicyContactContext(emailFromToken);
			const contactPolicyDecision = this.accountPolicyEvaluator.evaluateContact(contactContext);
			const skipContactFollowupRisk = contactPolicyDecision.hasCapability('followup_risk_exempt');
			const skipStoredFollowupRiskChecks = this.shouldSkipFollowupRiskChecks(user);
			const skipFollowupRiskChecks = skipStoredFollowupRiskChecks || skipContactFollowupRisk;
			const plusTaggedEmailChange = derivePlusAddressBase(emailFromToken) !== null;
			const shouldRunPlusAddressRiskCheck = plusTaggedEmailChange && !user.hasEverPurchased;
			const newDomain = contactContext.domain;
			let contactDomainAdminListed = false;
			let contactDomainDisposable = false;
			let contactDomainBlocked = false;
			let contactDomainStepUpRequired = false;
			if (!skipFollowupRiskChecks && newDomain) {
				const [adminFlagged, isDisposable] = await Promise.all([
					this.isEmailDomainSuspicious(newDomain),
					this.isEmailDomainDisposable(newDomain),
				]);
				contactDomainAdminListed = adminFlagged;
				contactDomainDisposable = isDisposable;
				contactDomainBlocked = this.accountPolicyEvaluator.isBlockedRegistrationEmailDomain(newDomain);
				contactDomainStepUpRequired = contactDomainBlocked || contactDomainAdminListed || contactDomainDisposable;
			}
			if (
				!skipContactFollowupRisk &&
				((!skipStoredFollowupRiskChecks && isUnclaimed) || shouldRunPlusAddressRiskCheck)
			) {
				try {
					emailSetRiskIp = requireClientIp(request, {
						trustClientIpHeader: Config.proxy.trust_client_ip_header,
						clientIpHeaderName: Config.proxy.client_ip_header,
					});
					const riskResult = await this.registrationRiskEvaluator.evaluate({
						email: emailFromToken,
						clientIp: emailSetRiskIp,
						locale: null,
						timezone: null,
						userAgent,
					});
					emailSetRiskAssessment = riskResult.assessment;
					emailSetRecommendedAction = riskResult.recommendedAction;
				} catch (error) {
					Logger.warn({error, userId: user.id}, 'Risk assessment failed during email set');
					throw error;
				}
			}
			const policyAssessment = emailSetRiskAssessment ?? createPolicyOnlyEmailSetAssessment();
			const policyDecision = this.accountPolicyEvaluator.evaluate({
				contact: {
					...contactContext,
					domainAdminListed: contactDomainAdminListed,
					domainDisposable: contactDomainDisposable,
					domainBlocked: contactDomainBlocked,
					domainStepUpRequired: contactDomainStepUpRequired,
				},
				region: {
					code: null,
					stepUpRequired: false,
				},
				assessment: {
					raw: policyAssessment,
					level: policyAssessment.level,
					action: emailSetRecommendedAction,
				},
			});
			nextSuspiciousFlags = imposePhoneRequirements(nextSuspiciousFlags, policyDecision.flagBits);
			if (nextSuspiciousFlags !== currentSuspiciousFlags) {
				user = await this.userRepository.patchUpsert(
					user.id,
					{suspicious_activity_flags: nextSuspiciousFlags},
					user.toRow(),
				);
			}
			if (emailSetRiskAssessment || nextSuspiciousFlags !== currentSuspiciousFlags) {
				const occurredAt = new Date();
				const resolvedClientIp =
					emailSetRiskIp ??
					requireClientIp(request, {
						trustClientIpHeader: Config.proxy.trust_client_ip_header,
						clientIpHeaderName: Config.proxy.client_ip_header,
					});
				const riskContext = deriveLatestRiskContext({
					userId: user.id.toString(),
					email: emailFromToken,
					clientIp: resolvedClientIp,
					asn: emailSetRiskAssessment?.signals.geoIpAsn?.asn ?? null,
					updatedAt: occurredAt,
				});
				(async () => {
					try {
						await this.riskHistoryRepository.upsertLatestContext(riskContext);
						if (nextSuspiciousFlags !== currentSuspiciousFlags && nextSuspiciousFlags !== 0) {
							await this.riskHistoryRepository.recordOutcomeForUser({
								userId: user.id.toString(),
								occurredAt,
								source: isUnclaimed ? 'claim_risk' : 'email_change_risk',
								outcomeCodes: ['challenged'],
							});
						}
					} catch (error) {
						Logger.warn({error, userId: user.id}, 'Failed to persist claim-time risk history');
					}
				})();
				if (emailSetRiskAssessment) {
					this.riskAssessmentRepository
						.recordAssessment({
							userId: user.id,
							ip: resolvedClientIp,
							email: emailFromToken,
							locale: null,
							assessment: emailSetRiskAssessment,
						})
						.catch((error) => {
							Logger.warn({error, userId: user.id}, 'Failed to persist claim-time risk assessment');
						});
				}
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
		if (emailActuallyChanged) {
			try {
				const request = ctx.req.raw;
				const resolvedClientIp = requireClientIp(request, {
					trustClientIpHeader: Config.proxy.trust_client_ip_header,
					clientIpHeaderName: Config.proxy.client_ip_header,
				});
				const occurredAt = new Date();
				const riskContext = deriveLatestRiskContext({
					userId: updatedUser.id.toString(),
					email: updatedUser.email,
					clientIp: resolvedClientIp,
					asn: null,
					updatedAt: occurredAt,
				});
				this.registrationEventsRepository
					.recordEvent({
						userId: updatedUser.id.toString(),
						email: updatedUser.email,
						emailDomain: riskContext.emailDomain,
						ip: resolvedClientIp,
						locale: updatedUser.locale ?? null,
						createdAt: occurredAt,
					})
					.catch((error) => {
						Logger.warn({error, userId: updatedUser.id}, 'Failed to record email-set registration event');
					});
			} catch (error) {
				Logger.warn({error, userId: updatedUser.id}, 'Failed to resolve client IP for email-set registration event');
			}
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
			? await Promise.all(
					profile.mutualFriends.map((user) =>
						mapUserToPartialResponseWithCache({
							user,
							userCacheService: this.userCacheService,
							requestCache: params.requestCache,
						}),
					),
				)
			: undefined;
		const connectedAccounts = profile.connections ? this.mapConnectionsToResponse(profile.connections) : undefined;
		return {
			user: await mapUserToPartialResponseWithCache({
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

	private shouldSkipFollowupRiskChecks(user: User): boolean {
		return user.hasEverPurchased || ((user.suspiciousActivityFlags ?? 0) & ~DEFERRED_PHONE_ON_COMMUNITY_JOIN) === 0;
	}

	private enforceUserAccess(user: User): void {
		const flags = getEffectiveSuspiciousFlags(user);
		if (flags !== 0) {
			throw new AccountSuspiciousActivityError(flags);
		}
	}

	private enforceSuspiciousSelfUpdateAllowance(user: User, body: UserUpdateWithVerificationRequestData): void {
		const flags = getEffectiveSuspiciousFlags(user);
		if (flags === 0) {
			return;
		}
		if (this.isAllowedSuspiciousRecoveryUpdate(body)) {
			return;
		}
		throw new AccountSuspiciousActivityError(flags);
	}

	private isAllowedSuspiciousRecoveryUpdate(body: UserUpdateWithVerificationRequestData): boolean {
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
