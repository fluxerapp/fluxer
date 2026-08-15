// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ProfileFieldPrivacyFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {RegistrationClosedError} from '@fluxer/errors/src/domains/auth/RegistrationClosedError';
import {RegistrationUrlInvalidError} from '@fluxer/errors/src/domains/auth/RegistrationUrlInvalidError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey, getSubnet} from '@fluxer/ip_utils/src/IpAddress';
import type {RegisterRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {parseAcceptLanguage} from '@pkgs/locale/src/LocaleService';
import type {RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';
import {types} from 'cassandra-driver';
import {ms} from 'itty-time';
import type {ApiContext} from '../ApiContext';
import {createEmailVerificationToken, createInviteCode, createUserID, type UserID} from '../BrandedTypes';
import type {APIConfig} from '../config/APIConfig';
import type {IDiscriminatorService} from '../infrastructure/DiscriminatorService';
import type {KVActivityTracker} from '../infrastructure/KVActivityTracker';
import {
	type InstanceConfigRepository,
	type InstanceRegistrationUrl,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
} from '../instance/InstanceConfigRepository';
import type {SingleCommunityService} from '../instance/SingleCommunityService';
import type {InviteService} from '../invite/InviteService';
import {Logger} from '../Logger';
import {profileSubstringBlocklistCache} from '../middleware/ProfileSubstringBlocklistCache';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {User} from '../models/User';
import {UserSettings} from '../models/UserSettings';
import {countryRequiresInboundPhoneVerification} from '../risk/AbusePolicy';
import {
	type IAccountPolicyEvaluator,
	isAssessmentThresholdAuditEvent,
	normalizePolicyContactDomain,
} from '../risk/AccountPolicyEvaluator';
import type {IRegistrationEventsRepository} from '../risk/adapters/VelocityAdapter';
import type {IRiskHistoryRepository} from '../risk/HistoricalOutcomeRepository';
import type {IRiskAssessmentRepository} from '../risk/RiskAssessmentRepository';
import {deriveLatestRiskContext} from '../risk/RiskHistoryContext';
import {getUserSearchService} from '../SearchFactory';
import * as AgeUtils from '../utils/AgeUtils';
import {extractEmailDomain} from '../utils/EmailDomainUtils';
import {lookupGeoip} from '../utils/IpUtils';
import {generateRandomUsername} from '../utils/UsernameGenerator';
import {deriveUsernameFromDisplayName} from '../utils/UsernameSuggestionUtils';
import * as AuthPassword from './AuthPassword';
import * as AuthSession from './AuthSession';
import * as AuthUtility from './AuthUtility';
import type {IRegistrationRiskEvaluator} from './services/IRegistrationRiskEvaluator';

const DEFAULT_MINIMUM_AGE = 13;

function getRetryAfterSeconds(result: RateLimitResult): number {
	return result.retryAfter ?? Math.max(0, Math.ceil((result.resetTime.getTime() - Date.now()) / 1000));
}

function throwRegistrationRateLimit(result: RateLimitResult): never {
	throw new RateLimitError({
		retryAfter: getRetryAfterSeconds(result),
		limit: result.limit,
		resetTime: result.resetTime,
	});
}

function parseDobLocalDate(dateOfBirth: string): types.LocalDate {
	try {
		return types.LocalDate.fromString(dateOfBirth);
	} catch {
		throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.INVALID_DATE_OF_BIRTH_FORMAT);
	}
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
	registrationRiskEvaluator: IRegistrationRiskEvaluator;
	accountPolicyEvaluator: IAccountPolicyEvaluator;
	isEmailDomainSuspicious: (domain: string) => Promise<boolean>;
	isEmailDomainDisposable: (domain: string) => Promise<boolean>;
	registrationEventsRepository: IRegistrationEventsRepository;
	riskAssessmentRepository: IRiskAssessmentRepository;
	riskHistoryRepository: Pick<IRiskHistoryRepository, 'upsertLatestContext' | 'recordOutcomeForUser'>;
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
	const {
		inviteService,
		instanceConfigRepository,
		singleCommunityService,
		discriminatorService,
		kvActivityTracker,
		registrationRiskEvaluator,
		accountPolicyEvaluator,
		isEmailDomainSuspicious,
		isEmailDomainDisposable,
		registrationEventsRepository,
		riskAssessmentRepository,
		riskHistoryRepository,
	} = deps;
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
		const minAge = accountPolicyEvaluator.getMinimumAgeForRegion(countryCode, DEFAULT_MINIMUM_AGE);
		if (!AuthUtility.validateAge(ctx, {dateOfBirth: dateOfBirthInput, minAge})) {
			throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.MUST_BE_MINIMUM_AGE, {minAge});
		}
		dateOfBirth = parseDobLocalDate(dateOfBirthInput);
		isAdult = AgeUtils.isUserAdult(dateOfBirthInput);
	}
	if (data.password && (await AuthPassword.isPasswordPwned(ctx, data.password))) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const rawEmail = data.email ?? null;
	const emailKey = rawEmail ? rawEmail.toLowerCase() : null;
	const userAgent = request.headers.get('user-agent');
	const enforceRateLimits = !config.dev.relaxRegistrationRateLimits;
	await enforceRegistrationRateLimits(ctx, {enforceRateLimits, clientIp, emailKey});
	let contactDomain: string | null = null;
	let contactDomainAdminListed = false;
	let contactDomainDisposable = false;
	let contactDomainBlocked = false;
	let contactDomainStepUpRequired = false;
	if (rawEmail) {
		contactDomain = normalizePolicyContactDomain(extractEmailDomain(rawEmail));
		const hasValidDns = await emailDnsValidation.hasValidDnsRecords(rawEmail);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}
		contactDomainBlocked = accountPolicyEvaluator.isBlockedRegistrationEmailDomain(contactDomain);
		if (contactDomainBlocked) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}
		[contactDomainAdminListed, contactDomainDisposable] = contactDomain
			? await Promise.all([isEmailDomainSuspicious(contactDomain), isEmailDomainDisposable(contactDomain)])
			: [false, false];
		contactDomainStepUpRequired = contactDomainBlocked || contactDomainAdminListed || contactDomainDisposable;
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
	let user = await users.create({
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
	});
	await kvActivityTracker.updateActivity(user.id, now);
	await users.upsertSettings(
		UserSettings.getDefaultUserSettings({
			userId,
			locale: userLocale,
			isAdult,
			theme: data.theme,
		}),
	);
	const isUnclaimed = !rawEmail;
	const usernameIsUserChosen = data.username != null || data.global_name != null;
	const riskResult = await registrationRiskEvaluator.evaluate({
		email: rawEmail,
		clientIp,
		locale: userLocale,
		timezone: null,
		userAgent,
		username,
		globalName: data.global_name ?? null,
		usernameIsUserChosen,
		isUnclaimed,
	});
	const policyDecision = accountPolicyEvaluator.evaluate({
		contact: {
			value: rawEmail,
			domain: contactDomain,
			domainAdminListed: contactDomainAdminListed,
			domainDisposable: contactDomainDisposable,
			domainBlocked: contactDomainBlocked,
			domainStepUpRequired: contactDomainStepUpRequired,
		},
		region: {
			code: countryCode,
			stepUpRequired: countryRequiresInboundPhoneVerification(countryCode),
		},
		assessment: {
			raw: riskResult.assessment,
			level: riskResult.level,
			action: riskResult.recommendedAction,
		},
	});
	const combinedFlags = policyDecision.flagBits;
	const createdAt = new Date();
	const riskContext = deriveLatestRiskContext({
		userId: userId.toString(),
		email: rawEmail ?? null,
		clientIp,
		asn: riskResult.assessment.signals.geoIpAsn?.asn ?? null,
		updatedAt: createdAt,
	});
	if (combinedFlags !== 0) {
		user = await users.patchUpsert(user.id, {suspicious_activity_flags: combinedFlags}, user.toRow());
	}
	registrationEventsRepository
		.recordEvent({
			userId: userId.toString(),
			email: rawEmail ?? null,
			emailDomain: riskContext.emailDomain,
			ip: clientIp,
			locale: userLocale,
			createdAt,
		})
		.catch((error) => {
			Logger.warn(
				{userId: userId.toString(), error},
				'[AuthRegistration] Failed to record registration velocity event',
			);
		});
	(async () => {
		try {
			await riskHistoryRepository.upsertLatestContext(riskContext);
			if (policyDecision.riskHistoryOutcomeCodes.length > 0) {
				await riskHistoryRepository.recordOutcomeForUser({
					userId: userId.toString(),
					occurredAt: createdAt,
					source: 'registration_risk',
					outcomeCodes: [...policyDecision.riskHistoryOutcomeCodes],
				});
			}
		} catch (error) {
			Logger.warn({userId: userId.toString(), error}, '[AuthRegistration] Failed to persist direct risk history');
		}
	})();
	riskAssessmentRepository
		.recordAssessment({
			userId,
			ip: clientIp,
			email: rawEmail ?? null,
			locale: userLocale,
			assessment: riskResult.assessment,
		})
		.catch((error) => {
			Logger.warn({userId: userId.toString(), error}, '[AuthRegistration] Failed to persist risk assessment');
		});
	for (const event of policyDecision.auditEvents) {
		if (isAssessmentThresholdAuditEvent(event)) {
			Logger.warn(
				{
					userId: userId.toString(),
					email: rawEmail,
					ip: clientIp,
					score: riskResult.assessment.riskScore,
					reasoning: riskResult.assessment.reasoning,
					policyRuleId: event.ruleId,
				},
				'[AuthRegistration] Account policy emitted assessment threshold notice',
			);
		}
	}
	await maybeIndexUser(user);
	if (rawEmail && emailEnabled) await maybeSendVerificationEmail(ctx, {user, email: rawEmail});
	await users.createAuthorizedIp(userId, clientIp);
	if (registrationAccess.registrationUrl) {
		await instanceConfigRepository.recordRegistrationUrlUse(registrationAccess.registrationUrl.id, user.id.toString());
	}
	if (registrationAccess.pendingApproval) {
		await instanceConfigRepository.addPendingRegistration({
			user_id: user.id.toString(),
			username: user.username,
			discriminator: user.discriminator,
			global_name: user.globalName,
			email: rawEmail,
			requested_at: now.toISOString(),
			registration_url_id: registrationAccess.registrationUrl?.id ?? null,
			client_ip: clientIp,
		});
		return {
			registration_pending_approval: true,
			user_id: user.id.toString(),
		};
	}
	if (policyDecision.inviteAutoJoinEnabled) {
		await maybeAutoJoinInvite(inviteService, {
			userId,
			inviteCode: data.invite_code || config.instance.autoJoinInviteCode,
			requestCache,
		});
	} else {
		Logger.info(
			{
				userId: userId.toString(),
				riskLevel: riskResult.level,
				riskScore: riskResult.assessment.riskScore,
				inviteCode: data.invite_code,
				reason: policyDecision.inviteAutoJoinSkipReason,
			},
			'[AuthRegistration] Skipping invite auto-join because account policy disabled it',
		);
	}
	await singleCommunityService.joinStockCommunity(userId, requestCache);
	const [token] = await AuthSession.createAuthSession(ctx, {user, request});
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

async function maybeIndexUser(user: User): Promise<void> {
	const userSearchService = getUserSearchService();
	if (!userSearchService) return;
	if ('indexUser' in userSearchService) {
		try {
			await userSearchService.indexUser(user);
		} catch (error) {
			Logger.error({userId: user.id, error}, 'Failed to index user in search');
		}
	}
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
		if (!emailRateLimit.allowed) throwRegistrationRateLimit(emailRateLimit);
	}
	const ipRateLimit = await rateLimit.checkLimit({
		identifier: `registration:ip:${getSameIpDecisionKey(clientIp) ?? clientIp}`,
		maxAttempts: 3,
		windowMs: ms('1 hour'),
	});
	if (!ipRateLimit.allowed) throwRegistrationRateLimit(ipRateLimit);
	const subnet = getSubnet(clientIp);
	if (subnet) {
		const subnetRateLimit = await rateLimit.checkLimit({
			identifier: `registration:subnet:${subnet}`,
			maxAttempts: 15,
			windowMs: ms('1 hour'),
		});
		if (!subnetRateLimit.allowed) throwRegistrationRateLimit(subnetRateLimit);
	}
}

async function allocateDiscriminator(discriminatorService: IDiscriminatorService, username: string): Promise<number> {
	const result = await discriminatorService.generateDiscriminator({username});
	if (!result.available || result.discriminator === -1) {
		throw InputValidationError.fromCode('username', ValidationErrorCodes.TOO_MANY_USERS_WITH_THIS_USERNAME);
	}
	return result.discriminator;
}
