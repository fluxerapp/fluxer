// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {
	budgetEnvFor,
	chargeAddressLookup,
	enforceInstantAccountLimits,
	recordInstantAccountCreated,
} from '@app/api/auth/AccountBudgets';
import {issueDeviceProof, normalizeEmailKey} from '@app/api/auth/AccountCodes';
import {sendVerificationEmail} from '@app/api/auth/AuthEmail';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {assertEmailNotBlocklisted} from '@app/api/auth/EmailBlocklist';
import {
	getLocalPartAtInstance,
	getPrimaryInstanceHost,
	usernameFromInstanceLocalPart,
} from '@app/api/auth/InstanceAddress';
import {createInviteCode, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {APIConfig} from '@app/api/config/APIConfig';
import type {LocalDate} from '@app/api/database/LocalDate';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {RegMethod} from '@app/api/infrastructure/activity/Contract.generated';
import {isBlockedEmailDomain} from '@app/api/infrastructure/activity/SharedLists';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import {withAccountIdentitySetupLock} from '@app/api/instance/AccountIdentitySetupLock';
import {parseInviteCodeList} from '@app/api/instance/AutoJoinInviteCodes';
import {
	type InstanceConfigRepository,
	type InstanceRegistrationUrl,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	type RegistrationUrlClaim,
} from '@app/api/instance/InstanceConfigRepository';
import {assertSetupToken, clearUsedSetupToken, isSetupTokenEnforced} from '@app/api/instance/SetupToken';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {UserSettings} from '@app/api/models/UserSettings';
import {NO_TIER_CONTEXT, type TierContext} from '@app/api/user/AccountTier';
import {buildPendingEntries, executeEntries, type GuildEntryDeps, storePendingEntries} from '@app/api/user/GuildEntry';
import {
	deriveAvailableUsername,
	isUsernameTaken,
	reserveUsername,
	type UsernameReservation,
} from '@app/api/user/UniqueUsernames';
import {USERNAME_MODE_DISCRIMINATOR} from '@app/api/user/UserTag';
import {DOB_DEFERRED_TRAIT} from '@app/api/user/UserTraits';
import * as AgeUtils from '@app/api/utils/AgeUtils';
import {assertEmailDomainAllowed, extractEmailDomain} from '@app/api/utils/EmailDomainUtils';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {hashPassword} from '@app/api/utils/PasswordUtils';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {generateRandomUsername} from '@app/api/utils/UsernameGenerator';
import {
	deriveUsernameFromDisplayName,
	randomUsernameVariant,
	usernameBaseFromDisplayName,
} from '@app/api/utils/UsernameSuggestionUtils';
import {inputValidationErrorFromZodIssues} from '@app/api/Validator';
import {
	EmailDeliveryStates,
	InstantAccountAvailability,
	RegistrationModes,
} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {requiresLocalCredential} from '@fluxer/constants/src/AccountTierRules';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {getRegionalMinimumAge} from '@fluxer/constants/src/RegionalMinimumAge';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InstantAccountsDisabledError} from '@fluxer/errors/src/domains/auth/InstantAccountsDisabledError';
import {RegistrationClosedError} from '@fluxer/errors/src/domains/auth/RegistrationClosedError';
import {RegistrationCredentialRequiredError} from '@fluxer/errors/src/domains/auth/RegistrationCredentialRequiredError';
import {RegistrationCredentialsRequiredError} from '@fluxer/errors/src/domains/auth/RegistrationCredentialsRequiredError';
import {RegistrationUrlInvalidError} from '@fluxer/errors/src/domains/auth/RegistrationUrlInvalidError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {parseAcceptLanguage} from '@fluxer/i18n/src/locale/resolution/AcceptLanguageNegotiation';
import {getSameIpDecisionKey, getSubnet} from '@fluxer/ip_utils/src/IpAddress';
import type {RegisterRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';
import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {ms} from 'itty-time';

const DERIVED_USERNAME_ATTEMPTS = 5;

interface RegisterParams {
	data: RegisterRequest;
	request: Request;
	requestCache: RequestCache;
	tierContext: TierContext;
}

export interface RegistrationDependencies extends GuildEntryDeps {
	instanceConfigRepository: InstanceConfigRepository;
	discriminatorService: DiscriminatorService;
	kvActivityTracker: KVActivityTracker;
}

type RegistrationSource = 'app' | 'marketing';

interface RegistrationAccess {
	pendingApproval: boolean;
	registrationUrl: InstanceRegistrationUrl | null;
}

interface AccountCreationInput {
	email: string | null;
	emailProven: boolean;
	username?: string;
	globalName?: string;
	password?: string;
	dateOfBirth?: string;
	consent: boolean | undefined;
	inviteCode?: string | null;
	registrationUrlCode?: string | null;
	theme?: RegisterRequest['theme'];
	olderAppEmail?: string | null;
	locale?: string | null;
	setupToken?: string | null;
}

interface AccountCreationParams {
	input: AccountCreationInput;
	request: Request;
	clientIp: string;
	requestCache: RequestCache;
	source: RegistrationSource;
	tierContext: TierContext;
}

export type AccountCreationResult =
	| {
			pendingApproval: false;
			user: User;
			token: string;
			entry: GuildEntryResult | null;
			deviceProof: string;
	  }
	| {pendingApproval: true; user: User};

export async function resolveInstantAccountAvailabilityFor(
	repository: InstanceConfigRepository,
	registrationUrlCode: string | null | undefined,
): Promise<InstantAccountAvailability> {
	const code = registrationUrlCode?.trim();
	const registrationUrl = code ? await repository.resolveRegistrationUrlCode(code) : null;
	return await repository.getInstantAccountAvailability(
		registrationUrl ? {approvalRequired: registrationUrl.approval_required} : null,
	);
}

export async function instantAccountsAvailable(
	repository: InstanceConfigRepository,
	registrationUrlCode: string | null = null,
): Promise<boolean> {
	return (
		(await resolveInstantAccountAvailabilityFor(repository, registrationUrlCode)) ===
		InstantAccountAvailability.AVAILABLE
	);
}

export function assertInstantAccountAvailable(availability: InstantAccountAvailability): void {
	if (availability === InstantAccountAvailability.AVAILABLE) return;
	if (availability === InstantAccountAvailability.REGISTRATION_CLOSED) throw new RegistrationClosedError();
	throw new InstantAccountsDisabledError();
}

function shouldRequireHostedLegalConsent(config: APIConfig): boolean {
	return !config.instance.selfHosted;
}

export function assertRegistrationConsent(
	config: APIConfig,
	appPublicConfig: {legal: {terms_url: string | null; privacy_url: string | null}},
	consent: boolean | undefined,
): {requiresTermsConsent: boolean; requiresPrivacyConsent: boolean} {
	const requiresTermsConsent = shouldRequireHostedLegalConsent(config) || appPublicConfig.legal.terms_url !== null;
	const requiresPrivacyConsent = shouldRequireHostedLegalConsent(config) || appPublicConfig.legal.privacy_url !== null;
	if ((requiresTermsConsent || requiresPrivacyConsent) && consent !== true) {
		throw InputValidationError.fromCode('consent', ValidationErrorCodes.MUST_AGREE_TO_TOS_AND_PRIVACY_POLICY);
	}
	return {requiresTermsConsent, requiresPrivacyConsent};
}

export function isInstantAccountRequest(
	data: {email?: string | null; password?: string | null},
	{usernameMode}: {usernameMode: boolean},
): boolean {
	return !data.password && (usernameMode || !data.email);
}

export async function register(
	ctx: ApiContext,
	deps: RegistrationDependencies,
	{data, request, requestCache, tierContext}: RegisterParams,
): Promise<AccountCreationResult> {
	const {config} = ctx.services;
	const clientIp = requireConfiguredClientIp(request, config.proxy);
	return await createAccount(ctx, deps, {
		input: {
			email: data.email ?? null,
			emailProven: false,
			username: data.username,
			globalName: data.global_name,
			password: data.password,
			dateOfBirth: data.date_of_birth,
			consent: data.consent,
			inviteCode: data.invite_code,
			registrationUrlCode: data.registration_url_code,
			theme: data.theme,
			olderAppEmail: data.email ?? null,
			setupToken: data.setup_token,
		},
		request,
		clientIp,
		requestCache,
		source: 'app',
		tierContext,
	});
}

export async function assertAccountCreationPastSetup(
	ctx: ApiContext,
	repository: InstanceConfigRepository,
	clientIp: string,
): Promise<void> {
	const {config, rateLimit} = ctx.services;
	await assertSetupToken({rateLimit, config, repository, presented: null, clientIp});
	await assertSignUpsOpenAfterSetup(config, repository);
}

async function assertSignUpsOpenAfterSetup(config: APIConfig, repository: InstanceConfigRepository): Promise<void> {
	if (!config.instance.selfHosted) return;
	if ((await repository.getAppPublicConfig()).setup.configured) return;
	if (await repository.isAdminBootstrapped()) throw new RegistrationClosedError();
}

export async function createAccount(
	ctx: ApiContext,
	deps: RegistrationDependencies,
	params: AccountCreationParams,
): Promise<AccountCreationResult> {
	const {config, rateLimit, cache} = ctx.services;
	const setupTokenPassed = await assertSetupToken({
		rateLimit,
		config,
		repository: deps.instanceConfigRepository,
		presented: params.input.setupToken,
		clientIp: params.clientIp,
	});
	if (!config.instance.selfHosted || (await deps.instanceConfigRepository.isAccountIdentityLocked())) {
		return await createAccountUnlocked(ctx, deps, params, {setupTokenPassed});
	}
	return await withAccountIdentitySetupLock(cache, () => createAccountUnlocked(ctx, deps, params, {setupTokenPassed}));
}

async function createAccountUnlocked(
	ctx: ApiContext,
	deps: RegistrationDependencies,
	{input, request, clientIp, requestCache, source, tierContext}: AccountCreationParams,
	{setupTokenPassed}: {setupTokenPassed: boolean},
): Promise<AccountCreationResult> {
	const {users, snowflake, emailDnsValidation, config} = ctx.services;
	const {instanceConfigRepository, discriminatorService, kvActivityTracker} = deps;
	const setupTokenVerified = setupTokenPassed && !(await instanceConfigRepository.confirmAdminBootstrapped());
	const appPublicConfig = await instanceConfigRepository.getAppPublicConfig();
	const emailDeliveryState = await instanceConfigRepository.getEmailDeliveryState();
	const emailEnabled = emailDeliveryState === EmailDeliveryStates.ON;
	const accountIdentity = await instanceConfigRepository.getAccountIdentity();
	const usernameMode = accountIdentity.mode === AccountIdentityModes.USERNAME;
	const uniqueUsernames = accountIdentity.tagStyle === TagStyles.NONE;
	const instant = isInstantAccountRequest({email: input.email, password: input.password}, {usernameMode});
	let olderAppUsername: string | undefined;
	if (usernameMode) {
		assertUsernameModeRegistration(input);
		olderAppUsername = input.username ? undefined : requestedUsernameFromOlderApp(config, input.olderAppEmail);
	}
	const {requiresTermsConsent, requiresPrivacyConsent} = assertRegistrationConsent(
		config,
		appPublicConfig,
		input.consent,
	);
	const now = new Date();
	if (!setupTokenVerified) await assertSignUpsOpenAfterSetup(config, instanceConfigRepository);
	let registrationAccess: RegistrationAccess;
	if (setupTokenVerified) {
		registrationAccess = {pendingApproval: false, registrationUrl: null};
	} else {
		registrationAccess = await resolveRegistrationAccess(
			instanceConfigRepository,
			input.registrationUrlCode,
			usernameMode ? null : input.email,
		);
	}
	if (registrationAccess.pendingApproval && !hasSignInCredentials(input, usernameMode)) {
		throw new RegistrationCredentialsRequiredError();
	}
	const inviteCode = input.inviteCode?.trim() || registrationAccess.registrationUrl?.invite_code || null;
	if (
		!usernameMode &&
		input.email &&
		!input.password &&
		requiresLocalCredential({
			identityMode: accountIdentity.mode,
			deliveryState: emailDeliveryState,
			emailProven: input.emailProven,
		})
	) {
		throw new RegistrationCredentialRequiredError();
	}
	if (instant) {
		let approvalOverride: {approvalRequired: boolean} | null = null;
		if (setupTokenVerified) {
			approvalOverride = {approvalRequired: false};
		} else if (registrationAccess.registrationUrl) {
			approvalOverride = {approvalRequired: registrationAccess.registrationUrl.approval_required};
		}
		assertInstantAccountAvailable(await instanceConfigRepository.getInstantAccountAvailability(approvalOverride));
	}
	const geoipResult = await lookupGeoip(clientIp);
	const dateOfBirthResult = resolveRegistrationDateOfBirth({
		collect: appPublicConfig.registration.collect_date_of_birth,
		dateOfBirth: input.dateOfBirth,
		countryCode: geoipResult.countryCode,
	});
	if (input.password && (await AuthPassword.isPasswordPwned(ctx, input.password))) {
		throw InputValidationError.fromCode('password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
	}
	const rawEmail = usernameMode ? null : input.email;
	const enforceRateLimits = !config.dev.relaxRegistrationRateLimits && !setupTokenVerified;
	const budgetEnv = budgetEnvFor(ctx.services, request);
	if (instant) {
		if (enforceRateLimits) await enforceInstantAccountLimits({...budgetEnv, enforce: true}, clientIp);
	} else if (!input.emailProven) {
		await enforceRegistrationRateLimits(ctx, {
			enforceRateLimits,
			clientIp,
			emailKey: rawEmail ? rawEmail.toLowerCase() : null,
		});
	}
	if (rawEmail) {
		if (!input.emailProven) {
			await assertRegistrationEmailDeliverable(rawEmail, emailDnsValidation);
		}
		const emailTaken = await users.findByEmail(rawEmail);
		if (emailTaken) {
			if (!input.emailProven) {
				await chargeAddressLookup(budgetEnv, {
					clientIp,
					emailKey: normalizeEmailKey(rawEmail),
					requester: 'unknown',
				});
			}
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
		}
	}
	const requestedUsername =
		input.username ?? olderAppUsername ?? (input.emailProven && rawEmail ? usernameFromEmail(rawEmail) : undefined);
	const {username, discriminator} = await resolveRegistrationUsername({
		users,
		discriminatorService,
		uniqueUsernames,
		requestedUsername,
		explicitlyRequested: input.username !== undefined || olderAppUsername !== undefined,
		globalName: input.globalName,
	});
	const bootstrapAdminEligible =
		shouldAttemptBootstrapAdminGrant(config, {
			rawEmail,
			hasPassword: Boolean(input.password),
			usernameMode,
			pendingApproval: registrationAccess.pendingApproval,
			setupConfigured: appPublicConfig.setup.configured,
			setupTokenVerified,
		}) && !(await instanceConfigRepository.isAdminBootstrapped());
	if (
		isBlockedUsername(username) ||
		(input.globalName && profileSubstringBlocklistCache.containsBannedSubstring('global_name', input.globalName))
	) {
		throw new ContentBlockedError();
	}
	const userId = createUserID(await snowflake.generate());
	const userLocale = input.locale
		? parseAcceptLanguage(input.locale)
		: parseAcceptLanguage(request.headers.get('accept-language'));
	const passwordHash = input.password ? await hashPassword(input.password) : null;
	const emailOwnedAtCreation = rawEmail !== null && (input.emailProven || !emailEnabled);
	const traits = new Set<string>();
	if (registrationAccess.pendingApproval) traits.add(REGISTRATION_PENDING_APPROVAL_TRAIT);
	if (dateOfBirthResult.deferred) traits.add(DOB_DEFERRED_TRAIT);
	const userRow: UserRow = {
		...EMPTY_USER_ROW,
		user_id: userId,
		username,
		discriminator,
		global_name: input.globalName || null,
		bot: false,
		system: false,
		email: rawEmail,
		email_verified: rawEmail === null ? !emailEnabled : emailOwnedAtCreation,
		email_bounced: false,
		password_hash: passwordHash,
		password_last_changed_at: passwordHash ? now : null,
		authenticator_types: new Set(),
		timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
		date_of_birth: dateOfBirthResult.date,
		locale: userLocale,
		flags: 0n,
		terms_agreed_at: requiresTermsConsent ? now : null,
		privacy_agreed_at: requiresPrivacyConsent ? now : null,
		last_active_at: now,
		last_active_ip: clientIp,
		acls: null,
		traits: traits.size > 0 ? traits : null,
		version: 1,
	};
	const registrationUrlUse = await claimRegistrationUrlUse(
		instanceConfigRepository,
		registrationAccess.registrationUrl,
		userId,
	);
	let user: User;
	let creationAttempt: 'unattempted' | 'ordinary_account' | 'bootstrap_admin' = 'unattempted';
	let usernameReservation: UsernameReservation | null = null;
	try {
		if (uniqueUsernames) {
			usernameReservation = await reserveUsername({users, cache: ctx.services.cache}, username);
		}
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
				invite_code: inviteCode || config.instance.autoJoinInviteCode || null,
			});
		}
		const grantBootstrapAdmin = bootstrapAdminEligible && (await instanceConfigRepository.claimAdminBootstrap(userId));
		creationAttempt = grantBootstrapAdmin ? 'bootstrap_admin' : 'ordinary_account';
		user = await users.create(grantBootstrapAdmin ? {...userRow, acls: new Set([AdminACLs.WILDCARD])} : userRow);
	} catch (error) {
		if (creationAttempt === 'unattempted') {
			await withdrawSignupOfUncreatedAccount(instanceConfigRepository, {
				userId,
				registrationUrlUse,
				pendingApproval: registrationAccess.pendingApproval,
			});
		} else if (creationAttempt === 'bootstrap_admin') {
			await releaseBootstrapAdminOfUncreatedAccount(instanceConfigRepository, userId);
		}
		throw error;
	} finally {
		await usernameReservation?.release();
	}
	if (creationAttempt === 'bootstrap_admin') {
		await clearUsedSetupToken(instanceConfigRepository);
	}
	await users.upsertSettings(
		UserSettings.getDefaultUserSettings({
			userId,
			locale: userLocale,
			isAdult: dateOfBirthResult.adult,
			theme: input.theme,
		}),
	);
	void kvActivityTracker.updateActivity(user.id, now).catch((error: unknown) => {
		Logger.warn({error, userId: user.id}, 'Failed to update real-time user activity');
	});
	let registrationMethod: RegMethod = 'unclaimed';
	if (input.emailProven) {
		registrationMethod = 'email_code';
	} else if (input.password) {
		registrationMethod = 'password';
	} else if (rawEmail) {
		registrationMethod = 'other';
	}
	await emitActivity(
		'registration',
		user.id.toString(),
		{
			user_id: user.id.toString(),
			method: registrationMethod,
			source,
			email: rawEmail,
			username,
			username_user_chosen: input.username != null || input.globalName != null,
			global_name: input.globalName ?? null,
			locale: userLocale,
			timezone: null,
			invite_code: input.inviteCode?.trim() || null,
			flags: user.flags.toString(),
			date_of_birth: user.dateOfBirth,
		},
		null,
		user.id.toString(),
	);
	if (rawEmail && !input.emailProven && emailEnabled) {
		await sendVerificationEmail(ctx, {user, email: rawEmail});
	}
	await users.createAuthorizedIp(userId, clientIp);
	if (instant) await recordInstantAccountCreated(budgetEnv, clientIp);
	if (registrationAccess.pendingApproval) {
		return {pendingApproval: true, user};
	}
	const entries = buildPendingEntries({
		inviteCode,
		autoJoinInviteCode: config.instance.autoJoinInviteCode,
		singleCommunity: (await deps.singleCommunityService.getStockCommunityId()) !== null,
	});
	let entry: GuildEntryResult | null = null;
	if (dateOfBirthResult.deferred) {
		await storePendingEntries(ctx.services.kv, user.id, entries);
	} else {
		entry = await executeEntries(deps, {user, entries, tierContext, requestCache, ip: clientIp});
	}
	const [token] = await AuthSession.createAuthSession(ctx, {
		user,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	return {pendingApproval: false, user, token, entry, deviceProof: issueDeviceProof(user.id)};
}

interface RegistrationDateOfBirth {
	date: LocalDate | null;
	adult: boolean;
	deferred: boolean;
}

function resolveRegistrationDateOfBirth(params: {
	collect: boolean;
	dateOfBirth: string | undefined;
	countryCode: string | null | undefined;
}): RegistrationDateOfBirth {
	if (!params.collect) return {date: null, adult: true, deferred: false};
	const dateOfBirthInput = params.dateOfBirth?.trim();
	if (!dateOfBirthInput) return {date: null, adult: false, deferred: true};
	const date = AgeUtils.parseDateOfBirth(dateOfBirthInput);
	const minAge = getRegionalMinimumAge(params.countryCode);
	if (!AuthUtility.validateAge({dateOfBirth: dateOfBirthInput, minAge})) {
		throw InputValidationError.fromCode('date_of_birth', ValidationErrorCodes.MUST_BE_MINIMUM_AGE, {minAge});
	}
	return {date, adult: AgeUtils.isUserAdult(dateOfBirthInput), deferred: false};
}

export async function assertRegistrationEmailDeliverable(
	email: string,
	emailDnsValidation: ApiContext['services']['emailDnsValidation'],
): Promise<void> {
	if (isBlockedEmailDomain(extractEmailDomain(email))) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
	}
	await assertEmailNotBlocklisted(email, 'email');
	if (!(await emailDnsValidation.hasValidDnsRecords(email))) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
	}
}

export async function assertInstanceAcceptsEmailDomain(email: string, field: string): Promise<void> {
	const {allowed_email_domains} = await getInstanceConfigRepository().getRegistrationConfig();
	assertEmailDomainAllowed(email, allowed_email_domains, field);
}

function usernameFromEmail(email: string): string | undefined {
	const localPart = email.split('@')[0] ?? '';
	return deriveUsernameFromDisplayName(localPart) ?? undefined;
}

export async function assertRequestedProfileAllowed(
	ctx: ApiContext,
	deps: Pick<RegistrationDependencies, 'instanceConfigRepository'>,
	params: {username: string | undefined; globalName: string | undefined},
): Promise<void> {
	const {username, globalName} = params;
	if (username && isBlockedUsername(username)) throw new ContentBlockedError();
	if (globalName && profileSubstringBlocklistCache.containsBannedSubstring('global_name', globalName)) {
		throw new ContentBlockedError();
	}
	if (!username) return;
	const accountIdentity = await deps.instanceConfigRepository.getAccountIdentity();
	if (accountIdentity.tagStyle === TagStyles.NONE && (await isUsernameTaken(ctx.services.users, username))) {
		throw InputValidationError.fromCode('username', ValidationErrorCodes.USERNAME_ALREADY_TAKEN);
	}
}

export async function resolveRegistrationUsername(params: {
	users: ApiContext['services']['users'];
	discriminatorService: Pick<DiscriminatorService, 'generateDiscriminator'>;
	uniqueUsernames: boolean;
	requestedUsername: string | undefined;
	explicitlyRequested: boolean;
	globalName: string | undefined;
}): Promise<{username: string; discriminator: number}> {
	const {users, discriminatorService, uniqueUsernames, explicitlyRequested, globalName} = params;
	let usernameCandidate = params.requestedUsername;
	if (usernameCandidate && isBlockedUsername(usernameCandidate)) {
		if (explicitlyRequested) throw new ContentBlockedError();
		usernameCandidate = undefined;
	}
	if (uniqueUsernames) {
		if (usernameCandidate && explicitlyRequested) {
			if (await isUsernameTaken(users, usernameCandidate)) {
				throw InputValidationError.fromCode('username', ValidationErrorCodes.USERNAME_ALREADY_TAKEN);
			}
			return {username: usernameCandidate, discriminator: USERNAME_MODE_DISCRIMINATOR};
		}
		const base = usernameCandidate ?? usernameBaseFromGlobalName(globalName);
		return {username: await deriveAvailableUsername(users, base), discriminator: USERNAME_MODE_DISCRIMINATOR};
	}
	if (usernameCandidate) {
		const discriminator = await tryAllocateDiscriminator(discriminatorService, usernameCandidate);
		if (discriminator !== null) return {username: usernameCandidate, discriminator};
		if (explicitlyRequested) {
			throw InputValidationError.fromCode('username', ValidationErrorCodes.TOO_MANY_USERS_WITH_THIS_USERNAME);
		}
	}
	const base = usernameBaseFromGlobalName(globalName);
	for (let attempt = 0; attempt < DERIVED_USERNAME_ATTEMPTS; attempt++) {
		const username = attempt === 0 ? base : randomUsernameVariant(base);
		const discriminator = await tryAllocateDiscriminator(discriminatorService, username);
		if (discriminator !== null) return {username, discriminator};
	}
	const randomUsername = generateRandomUsername();
	return {username: randomUsername, discriminator: await allocateDiscriminator(discriminatorService, randomUsername)};
}

function isBlockedUsername(username: string): boolean {
	return profileSubstringBlocklistCache.containsBannedSubstring('username', username);
}

function usernameBaseFromGlobalName(globalName: string | undefined): string {
	return globalName ? usernameBaseFromDisplayName(globalName, isBlockedUsername) : generateRandomUsername();
}

function shouldAttemptBootstrapAdminGrant(
	config: APIConfig,
	params: {
		rawEmail: string | null;
		hasPassword: boolean;
		usernameMode: boolean;
		pendingApproval: boolean;
		setupConfigured: boolean;
		setupTokenVerified: boolean;
	},
): boolean {
	const claimedAccount = params.usernameMode ? params.hasPassword : params.rawEmail !== null;
	if (isSetupTokenEnforced(config)) return params.setupTokenVerified && claimedAccount;
	const localDevInstance = config.nodeEnv === 'development' && !config.dev.testModeEnabled;
	const setupBootstrapOpen = !params.setupConfigured;
	return (
		(config.instance.selfHosted || localDevInstance || setupBootstrapOpen) && claimedAccount && !params.pendingApproval
	);
}

function hasSignInCredentials(input: AccountCreationInput, usernameMode: boolean): boolean {
	if (input.emailProven && input.email) return true;
	if (!input.password) return false;
	return usernameMode || Boolean(input.email);
}

function assertUsernameModeRegistration(data: AccountCreationInput): void {
	if (data.password && !data.username && data.olderAppEmail == null) {
		throw InputValidationError.fromCode('username', ValidationErrorCodes.USERNAME_LENGTH_INVALID);
	}
}

function requestedUsernameFromOlderApp(config: APIConfig, email: string | null | undefined): string | undefined {
	if (email == null) return undefined;
	const localPart = getLocalPartAtInstance(config, email.trim());
	if (localPart === null) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.INSTANCE_ADDRESS_REQUIRED, {
			host: getPrimaryInstanceHost(config),
		});
	}
	const candidate = usernameFromInstanceLocalPart(localPart);
	if (candidate === null) {
		throw InputValidationError.fromCode('username', ValidationErrorCodes.USERNAME_INVALID_CHARACTERS);
	}
	const parsed = UsernameType.safeParse(candidate);
	if (!parsed.success) {
		throw inputValidationErrorFromZodIssues(parsed.error.issues.map((issue) => ({...issue, path: ['username']})));
	}
	return parsed.data;
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

async function releaseBootstrapAdminOfUncreatedAccount(
	instanceConfigRepository: InstanceConfigRepository,
	userId: UserID,
): Promise<void> {
	try {
		await instanceConfigRepository.releaseAdminBootstrap();
	} catch (error) {
		Logger.error(
			{userId: userId.toString(), error},
			'[AuthRegistration] Failed to release the admin bootstrap claim of an account that was never created',
		);
	}
}

export async function resolveRegistrationAccess(
	instanceConfigRepository: InstanceConfigRepository,
	registrationUrlCode: string | null | undefined,
	email: string | null,
): Promise<RegistrationAccess> {
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
	if (!registrationUrl && registrationConfig.mode === RegistrationModes.CLOSED) {
		throw new RegistrationClosedError();
	}
	if (!registrationUrl && email) {
		assertEmailDomainAllowed(email, registrationConfig.allowed_email_domains, 'email');
	}
	return {
		pendingApproval: registrationUrl
			? registrationUrl.approval_required
			: registrationConfig.mode === RegistrationModes.APPROVAL,
		registrationUrl,
	};
}

export async function maybeAutoJoinInvite(
	inviteService: InviteService,
	params: {
		userId: UserID;
		inviteCode: string | null | undefined;
		requestCache: RequestCache;
	},
): Promise<void> {
	const {userId, inviteCode, requestCache} = params;
	for (const code of parseInviteCodeList(inviteCode)) {
		try {
			await inviteService.acceptInvite({
				userId,
				inviteCode: createInviteCode(code),
				requestCache,
				tierContext: NO_TIER_CONTEXT,
			});
		} catch (error) {
			Logger.warn({inviteCode: code, error}, 'Failed to auto-join invite');
		}
	}
}

export async function enforceRegistrationRateLimits(
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

async function tryAllocateDiscriminator(
	discriminatorService: Pick<DiscriminatorService, 'generateDiscriminator'>,
	username: string,
): Promise<number | null> {
	const result = await discriminatorService.generateDiscriminator({username});
	return result.available && result.discriminator !== -1 ? result.discriminator : null;
}

async function allocateDiscriminator(
	discriminatorService: Pick<DiscriminatorService, 'generateDiscriminator'>,
	username: string,
): Promise<number> {
	const discriminator = await tryAllocateDiscriminator(discriminatorService, username);
	if (discriminator === null) {
		throw InputValidationError.fromCode('username', ValidationErrorCodes.TOO_MANY_USERS_WITH_THIS_USERNAME);
	}
	return discriminator;
}
