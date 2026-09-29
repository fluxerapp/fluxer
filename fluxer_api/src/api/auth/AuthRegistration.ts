// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {assertEmailNotBlocklisted} from '@app/api/auth/EmailBlocklist';
import {createEmailVerificationToken, createInviteCode, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {APIConfig} from '@app/api/config/APIConfig';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {isBlockedEmailDomain} from '@app/api/infrastructure/activity/SharedLists';
import type {IDiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import {
	type InstanceConfigRepository,
	type InstanceRegistrationUrl,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	type RegistrationUrlClaim,
} from '@app/api/instance/InstanceConfigRepository';
import type {SingleCommunityService} from '@app/api/instance/SingleCommunityService';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {UserSettings} from '@app/api/models/UserSettings';
import * as AgeUtils from '@app/api/utils/AgeUtils';
import {extractEmailDomain} from '@app/api/utils/EmailDomainUtils';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {generateRandomUsername} from '@app/api/utils/UsernameGenerator';
import {deriveUsernameFromDisplayName} from '@app/api/utils/UsernameSuggestionUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {getRegionalMinimumAge} from '@fluxer/constants/src/RegionalMinimumAge';
import {ProfileFieldPrivacyFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {RegistrationClosedError} from '@fluxer/errors/src/domains/auth/RegistrationClosedError';
import {RegistrationUrlInvalidError} from '@fluxer/errors/src/domains/auth/RegistrationUrlInvalidError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey, getSubnet} from '@fluxer/ip_utils/src/IpAddress';
import type {RegisterRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {parseAcceptLanguage} from '@pkgs/locale/src/LocaleService';
import {types} from 'cassandra-driver';
import {ms} from 'itty-time';

function parseDobLocalDate(dateOfBirth: string): types.LocalDate {
	const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(dateOfBirth);
	if (!match) {
		throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.INVALID_DATE_OF_BIRTH_FORMAT);
	}
	const year = Number(match[1]);
	const month = Number(match[2]);
	const day = Number(match[3]);
	const probe = new Date(Date.UTC(year, month - 1, day));
	if (probe.getUTCFullYear() !== year || probe.getUTCMonth() !== month - 1 || probe.getUTCDate() !== day) {
		throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.INVALID_DATE_OF_BIRTH_FORMAT);
	}
	return new types.LocalDate(year, month, day);
}

interface RegisterParams {
	data: RegisterRequest;
	request: Request;
	requestCache: RequestCache;
}

export interface RegistrationDependencies {
	inviteService: InviteService | null;
	instanceConfigRepository: InstanceConfigRepository;
	singleCommunityService: SingleCommunityService;
	discriminatorService: IDiscriminatorService;
	kvActivityTracker: KVActivityTracker;
}

interface RegistrationTokenResult {
	user_id: string;
	token: string;
}

interface RegistrationPendingApprovalResult {
	registration_pending_approval: true;
	user_id: string;
}

type RegisterResult = RegistrationTokenResult | RegistrationPendingApprovalResult;

function shouldRequireHostedLegalConsent(config: APIConfig): boolean {
	return !config.instance.selfHosted;
}

export async function register(
	ctx: ApiContext,
	deps: RegistrationDependencies,
	{data, request, requestCache}: RegisterParams,
): Promise<RegisterResult> {
	const {users, snowflake, emailDnsValidation, config} = ctx.services;
	const {inviteService, instanceConfigRepository, singleCommunityService, discriminatorService, kvActivityTracker} =
		deps;
	const appPublicConfig = await instanceConfigRepository.getAppPublicConfig();
	const emailEnabled = await instanceConfigRepository.isEmailEnabled();
	const requiresTermsConsent = shouldRequireHostedLegalConsent(config) || appPublicConfig.legal.terms_url !== null;
	const requiresPrivacyConsent = shouldRequireHostedLegalConsent(config) || appPublicConfig.legal.privacy_url !== null;
	if ((requiresTermsConsent || requiresPrivacyConsent) && !data.consent) {
		throw InputValidationError.fromCode('consent', ValidationErrorCodes.MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY);
	}
	const now = new Date();
	const registrationAccess = await resolveRegistrationAccess(instanceConfigRepository, data.registration_url_code);
	const clientIp = requireClientIp(request, {
		trustClientIpHeader: config.proxy.trust_client_ip_header,
		clientIpHeaderName: config.proxy.client_ip_header,
	});
	const geoipResult = await lookupGeoip(clientIp);
	const countryCode = geoipResult.countryCode;
	const collectDateOfBirth = appPublicConfig.registration.collect_date_of_birth;
	let dateOfBirth: types.LocalDate | null = null;
	let isAdult = true;
	if (collectDateOfBirth) {
		const dateOfBirthInput = data.date_of_birth?.trim();
		if (!dateOfBirthInput) {
			throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.INVALID_DATE_OF_BIRTH_FORMAT);
		}
		dateOfBirth = parseDobLocalDate(dateOfBirthInput);
		const minAge = getRegionalMinimumAge(countryCode);
		if (!AuthUtility.validateAge(ctx, {dateOfBirth: dateOfBirthInput, minAge})) {
			throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.MUST_BE_MINIMUM_AGE, {minAge});
		}
		isAdult = AgeUtils.isUserAdult(dateOfBirthInput);
	}
	if (data.password && (await AuthPassword.isPasswordPwned(ctx, data.password))) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const rawEmail = data.email ?? null;
	const emailKey = rawEmail ? rawEmail.toLowerCase() : null;
	const enforceRateLimits = !config.dev.relaxRegistrationRateLimits;
	await enforceRegistrationRateLimits(ctx, {enforceRateLimits, clientIp, emailKey});
	if (rawEmail) {
		if (isBlockedEmailDomain(extractEmailDomain(rawEmail))) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}
		await assertEmailNotBlocklisted(rawEmail, 'email');
		const hasValidDns = await emailDnsValidation.hasValidDnsRecords(rawEmail);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
		}
		const emailTaken = await users.findByEmail(rawEmail);
		if (emailTaken) throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
	}
	let usernameCandidate: string | undefined = data.username ?? undefined;
	let discriminator: number | null = null;
	if (!usernameCandidate) {
		const derivedUsername = deriveUsernameFromDisplayName(data.global_name ?? '');
		if (derivedUsername) {
			try {
				discriminator = await allocateDiscriminator(discriminatorService, derivedUsername);
				usernameCandidate = derivedUsername;
			} catch (error) {
				if (!(error instanceof InputValidationError)) {
					throw error;
				}
			}
		}
	}
	if (!usernameCandidate) {
		usernameCandidate = generateRandomUsername();
		discriminator = await allocateDiscriminator(discriminatorService, usernameCandidate);
	} else if (discriminator === null) {
		discriminator = await allocateDiscriminator(discriminatorService, usernameCandidate);
	}
	const username = usernameCandidate!;
	const grantBootstrapAdmin =
		shouldAttemptBootstrapAdminGrant(config, {
			rawEmail,
			pendingApproval: registrationAccess.pendingApproval,
			setupConfigured: appPublicConfig.setup.configured,
		}) && !(await instanceConfigRepository.isAdminBootstrapped());
	if (
		profileSubstringBlocklistCache.containsBannedSubstring('username', username) ||
		(data.global_name && profileSubstringBlocklistCache.containsBannedSubstring('global_name', data.global_name))
	) {
		throw new ContentBlockedError();
	}
	const userId = createUserID(await snowflake.generate());
	const acceptLanguage = request.headers.get('accept-language');
	const userLocale = parseAcceptLanguage(acceptLanguage);
	const passwordHash = data.password ? await AuthPassword.hashPassword(ctx, data.password) : null;
	const flags = config.nodeEnv === 'development' ? UserFlags.STAFF : 0n;
	const userRow: UserRow = {
		user_id: userId,
		username,
		discriminator,
		global_name: data.global_name || null,
		bot: false,
		system: false,
		email: rawEmail,
		email_verified: !emailEnabled,
		email_bounced: false,
		password_hash: passwordHash,
		password_last_changed_at: passwordHash ? now : null,
		totp_secret: null,
		authenticator_types: new Set(),
		avatar_hash: null,
		avatar_color: null,
		banner_hash: null,
		banner_color: null,
		bio: null,
		pronouns: null,
		accent_color: null,
		timezone: null,
		timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
		date_of_birth: dateOfBirth,
		locale: userLocale,
		flags,
		premium_type: null,
		premium_since: null,
		premium_until: null,
		premium_gift_extension_ends_at: null,
		premium_will_cancel: null,
		premium_billing_cycle: null,
		premium_lifetime_sequence: null,
		premium_grace_ends_at: null,
		stripe_subscription_id: null,
		stripe_customer_id: null,
		has_ever_purchased: null,
		suspicious_activity_flags: null,
		terms_agreed_at: requiresTermsConsent ? now : null,
		privacy_agreed_at: requiresPrivacyConsent ? now : null,
		last_active_at: now,
		last_active_ip: clientIp,
		temp_banned_until: null,
		pending_deletion_at: null,
		pending_bulk_message_deletion_at: null,
		pending_bulk_message_deletion_channel_count: null,
		pending_bulk_message_deletion_message_count: null,
		deletion_reason_code: null,
		deletion_public_reason: null,
		deletion_audit_log_reason: null,
		acls: grantBootstrapAdmin ? new Set([AdminACLs.WILDCARD]) : null,
		traits: registrationAccess.pendingApproval ? new Set([REGISTRATION_PENDING_APPROVAL_TRAIT]) : null,
		first_refund_at: null,
		gift_inventory_server_seq: null,
		gift_inventory_client_seq: null,
		premium_onboarding_dismissed_at: null,
		mention_flags: null,
		last_voice_activity_sharing_change_at: null,
		version: 1,
	};
	const registrationUrlUse = await claimRegistrationUrlUse(
		instanceConfigRepository,
		registrationAccess.registrationUrl,
		userId,
	);
	let user: User;
	let createAttempted = false;
	try {
		if (registrationAccess.pendingApproval) {
			await instanceConfigRepository.addPendingRegistration({
				user_id: userId.toString(),
				username: userRow.username,
				discriminator: userRow.discriminator,
				global_name: userRow.global_name,
				email: rawEmail,
				requested_at: now.toISOString(),
				registration_url_id: registrationAccess.registrationUrl?.id ?? null,
				client_ip: clientIp,
			});
		}
		createAttempted = true;
		user = await users.create(userRow);
	} catch (error) {
		if (!createAttempted) {
			await withdrawSignupOfUncreatedAccount(instanceConfigRepository, {
				userId,
				registrationUrlUse,
				pendingApproval: registrationAccess.pendingApproval,
			});
		}
		throw error;
	}
	await users.upsertSettings(
		UserSettings.getDefaultUserSettings({
			userId,
			locale: userLocale,
			isAdult,
			theme: data.theme,
		}),
	);
	void kvActivityTracker.updateActivity(user.id, now).catch((error: unknown) => {
		Logger.warn({error, userId: user.id}, 'Failed to update real-time user activity');
	});
	await emitActivity(
		'registration',
		user.id.toString(),
		{
			user_id: user.id.toString(),
			method: data.password ? 'password' : rawEmail ? 'other' : 'unclaimed',
			email: rawEmail,
			username,
			username_user_chosen: data.username != null || data.global_name != null,
			global_name: data.global_name ?? null,
			locale: userLocale,
			timezone: null,
			invite_code: data.invite_code?.trim() || null,
			suspicious_flags: user.suspiciousActivityFlags ?? 0,
			flags: user.flags.toString(),
		},
		null,
		user.id.toString(),
	);
	if (rawEmail && emailEnabled) await maybeSendVerificationEmail(ctx, {user, email: rawEmail});
	await users.createAuthorizedIp(userId, clientIp);
	if (registrationAccess.pendingApproval) {
		return {
			registration_pending_approval: true,
			user_id: user.id.toString(),
		};
	}
	await maybeAutoJoinInvite(inviteService, {
		userId,
		inviteCode: data.invite_code || config.instance.autoJoinInviteCode,
		requestCache,
	});
	await singleCommunityService.joinStockCommunity(userId, requestCache);
	const [token] = await AuthSession.createAuthSession(ctx, {
		user,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	if (grantBootstrapAdmin) {
		await instanceConfigRepository.markAdminBootstrapped();
	}
	return {
		user_id: user.id.toString(),
		token,
	};
}

function shouldAttemptBootstrapAdminGrant(
	config: APIConfig,
	params: {
		rawEmail: string | null;
		pendingApproval: boolean;
		setupConfigured: boolean;
	},
): boolean {
	const localDevInstance = config.nodeEnv === 'development' && !config.dev.testModeEnabled;
	const setupBootstrapOpen = !params.setupConfigured;
	return (
		(config.instance.selfHosted || localDevInstance || setupBootstrapOpen) &&
		params.rawEmail !== null &&
		!params.pendingApproval
	);
}

async function claimRegistrationUrlUse(
	instanceConfigRepository: InstanceConfigRepository,
	registrationUrl: InstanceRegistrationUrl | null,
	userId: UserID,
): Promise<RegistrationUrlClaim | null> {
	if (registrationUrl === null) return null;
	const use = await instanceConfigRepository.claimRegistrationUrlUse(registrationUrl.id, userId.toString());
	if (use === null) {
		throw new RegistrationUrlInvalidError();
	}
	return use;
}

async function withdrawSignupOfUncreatedAccount(
	instanceConfigRepository: InstanceConfigRepository,
	signup: {userId: UserID; registrationUrlUse: RegistrationUrlClaim | null; pendingApproval: boolean},
): Promise<void> {
	try {
		if (signup.registrationUrlUse !== null) {
			await instanceConfigRepository.releaseRegistrationUrlUse(signup.registrationUrlUse);
		}
		if (signup.pendingApproval) {
			await instanceConfigRepository.removePendingRegistration(signup.userId.toString());
		}
	} catch (error) {
		Logger.warn(
			{userId: signup.userId.toString(), registrationUrlId: signup.registrationUrlUse?.registration_url_id, error},
			'[AuthRegistration] Failed to withdraw the registration URL use or pending approval of an account that was never created',
		);
	}
}

async function resolveRegistrationAccess(
	instanceConfigRepository: InstanceConfigRepository,
	registrationUrlCode: string | null | undefined,
): Promise<{pendingApproval: boolean; registrationUrl: InstanceRegistrationUrl | null}> {
	const registrationConfig = await instanceConfigRepository.getRegistrationConfig();
	const normalizedCode = registrationUrlCode?.trim();
	let registrationUrl: InstanceRegistrationUrl | null = null;
	if (normalizedCode) {
		if (!registrationConfig.admin_registration_urls_enabled) {
			throw new RegistrationUrlInvalidError();
		}
		registrationUrl = await instanceConfigRepository.resolveRegistrationUrlCode(normalizedCode);
		if (!registrationUrl) {
			throw new RegistrationUrlInvalidError();
		}
	}
	if (!registrationUrl && registrationConfig.mode === 'closed') {
		throw new RegistrationClosedError();
	}
	return {
		pendingApproval: registrationUrl ? registrationUrl.approval_required : registrationConfig.mode === 'approval',
		registrationUrl,
	};
}

async function maybeSendVerificationEmail(ctx: ApiContext, params: {user: User; email: string}): Promise<void> {
	const {users, email: emailService} = ctx.services;
	const {user, email} = params;
	const token = createEmailVerificationToken(await AuthUtility.generateSecureToken(ctx));
	await users.createEmailVerificationToken({
		token_: token,
		user_id: user.id,
		email,
	});
	await emailService.sendEmailVerification(email, user.username, token, user.locale);
}

async function maybeAutoJoinInvite(
	inviteService: InviteService | null,
	params: {
		userId: UserID;
		inviteCode: string | null | undefined;
		requestCache: RequestCache;
	},
): Promise<void> {
	const {userId, inviteCode, requestCache} = params;
	const normalizedInviteCode = inviteCode?.trim();
	if (!normalizedInviteCode) return;
	if (!inviteService) return;
	try {
		await inviteService.acceptInvite({
			userId,
			inviteCode: createInviteCode(normalizedInviteCode),
			requestCache,
		});
	} catch (error) {
		Logger.warn({inviteCode: normalizedInviteCode, error}, 'Failed to auto-join invite on registration');
	}
}

async function enforceRegistrationRateLimits(
	ctx: ApiContext,
	params: {
		enforceRateLimits: boolean;
		clientIp: string;
		emailKey: string | null;
	},
): Promise<void> {
	const {rateLimit} = ctx.services;
	const {enforceRateLimits, clientIp, emailKey} = params;
	if (!enforceRateLimits) return;
	if (emailKey) {
		const emailRateLimit = await rateLimit.checkLimit({
			identifier: `registration:email:${emailKey}`,
			maxAttempts: 3,
			windowMs: ms('15 minutes'),
		});
		if (!emailRateLimit.allowed) throw createRateLimitError(emailRateLimit);
	}
	const ipRateLimit = await rateLimit.checkLimit({
		identifier: `registration:ip:${getSameIpDecisionKey(clientIp) ?? clientIp}`,
		maxAttempts: 3,
		windowMs: ms('1 hour'),
	});
	if (!ipRateLimit.allowed) throw createRateLimitError(ipRateLimit);
	const subnet = getSubnet(clientIp);
	if (subnet) {
		const subnetRateLimit = await rateLimit.checkLimit({
			identifier: `registration:subnet:${subnet}`,
			maxAttempts: 15,
			windowMs: ms('1 hour'),
		});
		if (!subnetRateLimit.allowed) throw createRateLimitError(subnetRateLimit);
	}
}

async function allocateDiscriminator(discriminatorService: IDiscriminatorService, username: string): Promise<number> {
	const result = await discriminatorService.generateDiscriminator({username});
	if (!result.available || result.discriminator === -1) {
		throw InputValidationError.fromCode('username', ValidationErrorCodes.TOO_MANY_USERS_WITH_THIS_USERNAME);
	}
	return result.discriminator;
}
