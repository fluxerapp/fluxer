// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes, timingSafeEqual} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import {createMfaTicketResponse, type LoginMfaResult} from '@app/api/auth/AuthLogin';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import {assertAccountCreationPastSetup, maybeAutoJoinInvite} from '@app/api/auth/AuthRegistration';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {assertEmailNotBlocklisted, isEmailBlocklisted} from '@app/api/auth/EmailBlocklist';
import {
	discoveredHttpsUrl,
	exchangeAuthorizationCode,
	fetchUserInfo,
	ProviderExchangeError,
	type ProviderTokens,
	resolveOidcDiscovery,
	verifyIdToken,
} from '@app/api/auth/services/OidcClient';
import {
	isLinkedToSsoProvider,
	ssoIdentityTrait,
	ssoIdentityTraits,
	ssoProviderIdOf,
} from '@app/api/auth/services/SsoIdentityLinks';
import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {sanitizeSsoRedirectTo} from '@app/api/auth/services/SsoUtils';
import {userHasMfa} from '@app/api/auth/services/SudoMethods';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {createRoleID, type UserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildMemberService} from '@app/api/guild/services/GuildMemberService';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import {instanceCollectsDateOfBirth} from '@app/api/instance/DateOfBirthCollectionCache';
import {
	type InstanceConfigRepository,
	type InstanceSsoConfig,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import {canJoinSingleCommunity, type SingleCommunityService} from '@app/api/instance/SingleCommunityService';
import {deriveSsoRedirectUri, isTestSsoProvider} from '@app/api/instance/SsoConfigValidation';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import {createRequestCache, type RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {UserSettings} from '@app/api/models/UserSettings';
import {enqueueStripeCustomerEmailSync} from '@app/api/stripe/StripeCustomer';
import type {PartialUserChangePropagationDeps} from '@app/api/user/services/PartialUserChangePropagation';
import {
	deriveAvailableUsername,
	isUsernameTakenError,
	reserveUsername,
	type UsernameReservation,
} from '@app/api/user/UniqueUsernames';
import {isEnforcementDeletion, isProfileSubstringExempt, isSignInRefused} from '@app/api/user/UserHelpers';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';
import {renameUser} from '@app/api/user/UserRename';
import {USERNAME_MODE_DISCRIMINATOR} from '@app/api/user/UserTag';
import {DOB_DEFERRED_TRAIT} from '@app/api/user/UserTraits';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {randomBase64Url} from '@app/api/utils/SecretHash';
import {generateRandomUsername} from '@app/api/utils/UsernameGenerator';
import {deriveUsernameFromDisplayName} from '@app/api/utils/UsernameSuggestionUtils';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {SSO_MOBILE_CALLBACK_URI, SSO_MOBILE_STATE_PREFIX} from '@fluxer/constants/src/SsoConstants';
import {ProfileFieldPrivacyFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {SsoProviderUnavailableError} from '@fluxer/errors/src/domains/auth/SsoProviderUnavailableError';
import {SsoRequiredError, SsoRequiredReasons} from '@fluxer/errors/src/domains/auth/SsoRequiredError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {EmailType} from '@fluxer/schema/src/primitives/UserValidators';
import {ms, seconds} from 'itty-time';
import type {JWTPayload} from 'jose';

interface SsoStatePayload {
	codeVerifier: string;
	nonce: string;
	clientBindingHash?: string;
	redirectTo?: string;
	redirectUri?: string;
	link?: SsoLinkRequest;
	createdAt: number;
}

interface SsoLinkRequest {
	userId: string;
	sessionIdHash: string;
}

interface PendingSsoLink {
	userId: string;
	providerId: string;
	subject: string;
	groups: Array<string> | null;
	claims: ResolvedSsoClaims;
}

interface LegacyPendingSsoLink {
	userId: string;
	providerId: string;
	subject: string;
	groups: Array<string> | null;
}

interface SsoSignIn {
	token: string;
	user_id: string;
	redirect_to: string;
}

type SsoCompletion = SsoSignIn | (LoginMfaResult & {redirect_to: string}) | {linked: true; redirect_to: string};

interface PublicSsoStatus {
	enabled: boolean;
	enforced: boolean;
	username_from_provider: boolean;
	display_name: string | null;
	button_label: string | null;
	redirect_uri: string;
}

interface ResolvedSsoConfig extends InstanceSsoConfig {
	redirectUri: string;
	scope: string;
	ready: boolean;
	providerId: string;
	isTestProvider: boolean;
	issuerForVerification: string | null;
	tokenEndpointAuthMethods: Array<string> | null;
	discoveryFailed: boolean;
}

interface ResolvedSsoClaims {
	email: string | null;
	emailVerified: boolean;
	preferredUsername: string | null;
	name: string | null;
	sub: string;
	groups: Array<string> | null;
	groupsOverage: boolean;
}

const CODE_VERIFIER_BYTE_LENGTH = 32;
const STATE_BYTE_LENGTH = 16;
const CLIENT_BINDING_BYTE_LENGTH = 32;
const MOBILE_REQUIRES_CLIENT_BINDING = false;
const MOBILE_REQUIRES_SSO_SECOND_FACTOR = false;
const NONCE_BYTE_LENGTH = 16;
const MISSING_ISSUER_WARNING_INTERVAL_MS = ms('1 hour');
let missingIssuerWarnedAt = 0;

let ssoLogger: LoggerInterface | undefined;

function getLogger(): LoggerInterface {
	ssoLogger ??= Logger.child({logger: 'SsoService'});
	return ssoLogger;
}

function randomHexToken(byteLength: number): string {
	return randomBytes(byteLength).toString('hex');
}

function buildCodeChallenge(codeVerifier: string): string {
	return createHash('sha256').update(codeVerifier).digest('base64url');
}

function buildStateCacheKey(state: string): string {
	return `sso:state:${state}`;
}

function buildLinkStateCacheKey(state: string): string {
	return `sso:link-state:${state}`;
}

function pendingSsoLinkKey(ticket: string): string {
	return `sso:pending-link:${ticket}`;
}

function hashClientBinding(clientBinding: string): string {
	return createHash('sha256').update(clientBinding).digest('base64url');
}

function isClientBindingValid(
	state: string,
	expectedHash: string | undefined,
	clientBinding: string | undefined,
): boolean {
	if (clientBinding === undefined) {
		return state.startsWith(SSO_MOBILE_STATE_PREFIX) && !MOBILE_REQUIRES_CLIENT_BINDING;
	}
	const actual = Buffer.from(hashClientBinding(clientBinding));
	const expected = Buffer.from(expectedHash ?? '');
	return actual.length === expected.length && timingSafeEqual(actual, expected);
}

function isMobileSsoRedirectUri(requestedRedirectUri: string | undefined, defaultRedirectUri: string): boolean {
	const trimmed = requestedRedirectUri?.trim();
	if (!trimmed || trimmed === defaultRedirectUri) return false;
	if (trimmed === SSO_MOBILE_CALLBACK_URI) return true;
	throw InputValidationError.fromCode('redirect_uri', ValidationErrorCodes.INVALID_URL_FORMAT);
}

function coerceEmailVerified(value: unknown): boolean | undefined {
	if (value === true || value === false) return value;
	if (typeof value === 'string') {
		const v = value.trim().toLowerCase();
		if (v === 'true') return true;
		if (v === 'false') return false;
	}
	return undefined;
}

function validateEmailAgainstAllowlist(email: string, domains: Array<string>): void {
	if (!domains || domains.length === 0) return;
	const domain = email.split('@')[1]?.toLowerCase() ?? '';
	const allowed = domains.map((d) => d.toLowerCase().trim()).filter(Boolean);
	if (!allowed.includes(domain)) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_NOT_ALLOWED_FOR_SSO);
	}
}

function normalizeSsoEmail(rawEmail: string): string {
	const email = rawEmail.trim().toLowerCase();
	if (!EmailType.safeParse(email).success) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
	}
	return email;
}

function normalizeSsoSubject(rawSubject: string): string {
	if (!rawSubject.trim() || rawSubject.length > 1024) {
		throw InputValidationError.fromCode('sub', ValidationErrorCodes.INVALID_SSO_TOKEN);
	}
	return rawSubject;
}

function readStringClaim(
	record: Record<string, unknown> | JWTPayload | null | undefined,
	key: string,
): string | undefined {
	const value = record?.[key];
	return typeof value === 'string' ? value : undefined;
}

function readTrimmedClaim(record: Record<string, unknown> | JWTPayload | null | undefined, key: string): string | null {
	return readStringClaim(record, key)?.trim() || null;
}

function readDisplayName(record: Record<string, unknown> | JWTPayload | null | undefined): string | null {
	const name = readTrimmedClaim(record, 'name');
	if (name) return name;
	const parts = [readTrimmedClaim(record, 'given_name'), readTrimmedClaim(record, 'family_name')];
	return parts.filter((part): part is string => part !== null).join(' ') || null;
}

const DEFAULT_GROUPS_CLAIM = 'groups';

function readGroupsClaim(
	record: Record<string, unknown> | JWTPayload | null | undefined,
	key: string,
): Array<string> | null {
	const value = record?.[key];
	if (typeof value === 'string') return [value];
	if (!Array.isArray(value)) return null;
	return value.filter((group): group is string => typeof group === 'string');
}

function hasGroupsOverage(record: Record<string, unknown> | JWTPayload | null | undefined, key: string): boolean {
	const claimNames = record?.['_claim_names'];
	return record?.['hasgroups'] === true || (typeof claimNames === 'object' && claimNames !== null && key in claimNames);
}

function parseTestProviderClaims(rawPayload: string, groupsClaim: string): ResolvedSsoClaims {
	const parsed = rawPayload.trim().startsWith('{') ? parseJsonRecord(rawPayload) : null;
	if (!parsed) {
		const email = normalizeSsoEmail(rawPayload);
		return {
			email,
			emailVerified: true,
			preferredUsername: null,
			name: 'Test SSO User',
			sub: normalizeSsoSubject(email),
			groups: null,
			groupsOverage: false,
		};
	}
	const rawEmail = readStringClaim(parsed, 'email');
	const email = rawEmail ? normalizeSsoEmail(rawEmail) : null;
	const sub = readStringClaim(parsed, 'sub') ?? email;
	if (!sub) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.SSO_TEST_CODE_MISSING_EMAIL);
	}
	return {
		email,
		emailVerified: email !== null && (coerceEmailVerified(parsed['email_verified']) ?? true),
		preferredUsername: readTrimmedClaim(parsed, 'preferred_username'),
		name: readDisplayName(parsed),
		sub: normalizeSsoSubject(sub),
		groups: readGroupsClaim(parsed, groupsClaim),
		groupsOverage: hasGroupsOverage(parsed, groupsClaim),
	};
}

function usernameFromPreferredUsername(claims: ResolvedSsoClaims): string | null {
	const preferredUsername = claims.preferredUsername?.split('@')[0];
	return preferredUsername ? deriveUsernameFromDisplayName(preferredUsername) : null;
}

function deriveSsoUsernameBase(claims: ResolvedSsoClaims): string | null {
	const preferredUsername = usernameFromPreferredUsername(claims);
	if (preferredUsername) return preferredUsername;
	for (const candidate of [claims.name, claims.email?.split('@')[0]]) {
		const username = candidate ? deriveUsernameFromDisplayName(candidate) : null;
		if (username) return username;
	}
	return null;
}

function resolveUsableSsoEmail(
	claims: ResolvedSsoClaims,
	allowedEmailDomains: Array<string>,
	usernameMode: boolean,
): string | null {
	const emailRequired = !usernameMode || allowedEmailDomains.length > 0;
	if (!emailRequired) {
		return claims.emailVerified ? claims.email : null;
	}
	if (!claims.email) {
		throw InputValidationError.fromCode('email', ValidationErrorCodes.SSO_PROVIDER_DID_NOT_RETURN_EMAIL);
	}
	if (!claims.emailVerified) {
		throw InputValidationError.fromCode('email_verified', ValidationErrorCodes.SSO_EMAIL_NOT_VERIFIED);
	}
	validateEmailAgainstAllowlist(claims.email, allowedEmailDomains);
	return claims.email;
}

function readMatchingEmailVerified(
	record: Record<string, unknown> | JWTPayload | null | undefined,
	recordEmail: string | undefined,
	expectedEmail: string,
): boolean | undefined {
	if (!record || recordEmail !== expectedEmail) {
		return undefined;
	}
	return coerceEmailVerified(record['email_verified']);
}

function resolveEmailVerified({
	claims,
	userInfo,
	idTokenEmail,
	userInfoEmail,
	email,
}: {
	claims: JWTPayload | null;
	userInfo: Record<string, unknown> | null;
	idTokenEmail: string | undefined;
	userInfoEmail: string | undefined;
	email: string;
}): boolean {
	const values = [
		readMatchingEmailVerified(claims, idTokenEmail, email),
		readMatchingEmailVerified(userInfo, userInfoEmail, email),
	].filter((value): value is boolean => value !== undefined);
	if (values.includes(false)) {
		return false;
	}
	if (values.length === 0) {
		return claims !== null && idTokenEmail === email && isEntraVerifiedDomainEmail(claims['xms_edov']);
	}
	return values.includes(true);
}

function isEntraVerifiedDomainEmail(value: unknown): boolean {
	return value === 1 || value === '1' || coerceEmailVerified(value) === true;
}

function rethrowAsSsoError(error: unknown): never {
	if (!(error instanceof ProviderExchangeError)) throw error;
	switch (error.stage) {
		case 'token_unavailable':
			throw new SsoProviderUnavailableError();
		case 'token_client_refused':
			throw InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_MISCONFIGURED);
		case 'token_rejected':
			throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_SSO_AUTHORIZATION_CODE);
		case 'id_token_invalid':
			throw InputValidationError.fromCode('id_token', ValidationErrorCodes.INVALID_SSO_TOKEN);
		case 'userinfo_failed':
			throw InputValidationError.fromCode('access_token', ValidationErrorCodes.FAILED_TO_FETCH_SSO_USER_INFO);
		case 'userinfo_signed':
			throw InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_USER_INFO_SIGNED_RESPONSE_UNSUPPORTED);
		case 'userinfo_unparseable':
			throw InputValidationError.fromCode('access_token', ValidationErrorCodes.FAILED_TO_PARSE_SSO_USER_INFO);
	}
}

function claimsFromLegacyPendingLink(pending: LegacyPendingSsoLink): ResolvedSsoClaims {
	return {
		email: null,
		emailVerified: false,
		preferredUsername: null,
		name: null,
		sub: pending.subject,
		groups: pending.groups,
		groupsOverage: false,
	};
}

function warnAboutMissingIssuer(): void {
	const now = Date.now();
	if (now - missingIssuerWarnedAt < MISSING_ISSUER_WARNING_INTERVAL_MS) return;
	missingIssuerWarnedAt = now;
	getLogger().warn(
		'SSO is turned on without an issuer and stays off until one is set with rexctl config set sso.issuer',
	);
}

export class SsoService {
	private static readonly STATE_TTL_SECONDS = seconds('10 minutes');
	private readonly ssoIdentityRepository = new SsoIdentityRepository();

	constructor(
		private readonly apiContext: ApiContext,
		private readonly instanceConfigRepository: InstanceConfigRepository,
		private readonly discriminatorService: DiscriminatorService,
		private readonly kvActivityTracker: KVActivityTracker,
		private readonly singleCommunityService: SingleCommunityService,
		private readonly inviteService: InviteService,
		private readonly guildMemberService: GuildMemberService,
		private readonly userUpdateDeps: PartialUserChangePropagationDeps,
		private readonly guildRepository: GuildRepository,
	) {}

	async getPublicStatus(): Promise<PublicSsoStatus> {
		const config = await this.getResolvedConfig();
		return {
			enabled: config.ready,
			enforced: config.enabled && config.enforced,
			username_from_provider: config.enabled && config.usernameFromProvider,
			display_name: config.displayName ?? null,
			button_label: config.buttonLabel,
			redirect_uri: config.redirectUri,
		};
	}

	async isEnforced(): Promise<boolean> {
		const stored = await this.instanceConfigRepository.getSsoConfig();
		return stored.enabled && stored.enforced;
	}

	async managesUsernames(): Promise<boolean> {
		return this.takesUsernameFromProvider(await this.instanceConfigRepository.getSsoConfig());
	}

	private async takesUsernameFromProvider(config: InstanceSsoConfig): Promise<boolean> {
		return (
			config.enabled &&
			config.usernameFromProvider &&
			(await this.instanceConfigRepository.getAccountIdentityMode()) === AccountIdentityModes.USERNAME
		);
	}

	async startLink(user: User, sessionIdHash: Buffer) {
		return this.startLogin({
			link: {userId: user.id.toString(), sessionIdHash: sessionIdHash.toString('base64url')},
		});
	}

	async startLogin({
		redirectTo,
		redirectUri,
		link,
	}: {
		redirectTo?: string;
		redirectUri?: string;
		link?: SsoLinkRequest;
	} = {}): Promise<{
		authorization_url: string;
		state: string;
		redirect_uri: string;
		client_binding: string;
	}> {
		const config = await this.requireReadyConfig();
		const isMobile = isMobileSsoRedirectUri(redirectUri, config.redirectUri);
		const state = `${isMobile ? SSO_MOBILE_STATE_PREFIX : ''}${randomHexToken(STATE_BYTE_LENGTH)}`;
		const codeVerifier = randomBase64Url(CODE_VERIFIER_BYTE_LENGTH);
		const codeChallenge = buildCodeChallenge(codeVerifier);
		const nonce = randomBase64Url(NONCE_BYTE_LENGTH);
		const clientBinding = randomBase64Url(CLIENT_BINDING_BYTE_LENGTH);
		const statePayload: SsoStatePayload = {
			codeVerifier,
			nonce,
			clientBindingHash: hashClientBinding(clientBinding),
			redirectTo: sanitizeSsoRedirectTo(redirectTo),
			redirectUri: config.redirectUri,
			link,
			createdAt: Date.now(),
		};
		const {cache} = this.apiContext.services;
		const stateKey = link ? buildLinkStateCacheKey(state) : buildStateCacheKey(state);
		await cache.set(stateKey, statePayload, SsoService.STATE_TTL_SECONDS);
		const searchParams = new URLSearchParams({
			response_type: 'code',
			client_id: config.clientId ?? '',
			redirect_uri: config.redirectUri,
			scope: config.scope,
			state,
			code_challenge: codeChallenge,
			code_challenge_method: 'S256',
			nonce,
		});
		let authorizationUrlString: string;
		try {
			const authorizationUrl = new URL(config.authorizationUrl ?? '');
			for (const [k, v] of searchParams.entries()) {
				authorizationUrl.searchParams.set(k, v);
			}
			authorizationUrlString = authorizationUrl.toString();
		} catch {
			if (config.isTestProvider) {
				const joiner = (config.authorizationUrl ?? '').includes('?') ? '&' : '?';
				authorizationUrlString = `${config.authorizationUrl}${joiner}${searchParams.toString()}`;
			} else {
				throw new FeatureTemporarilyDisabledError();
			}
		}
		return {
			authorization_url: authorizationUrlString,
			state,
			redirect_uri: config.redirectUri,
			client_binding: clientBinding,
		};
	}

	async completeLogin({
		code,
		state,
		clientBinding,
		request,
		requestCache,
		clientIp,
	}: {
		code: string;
		state: string;
		clientBinding: string | undefined;
		request: Request;
		requestCache: RequestCache;
		clientIp: string;
	}): Promise<SsoCompletion> {
		const config = await this.requireReadyConfig();
		const {cache} = this.apiContext.services;
		const statePayload =
			(await cache.getAndDelete<SsoStatePayload>(buildStateCacheKey(state))) ??
			(await cache.getAndDelete<SsoStatePayload>(buildLinkStateCacheKey(state)));
		if (!statePayload || !isClientBindingValid(state, statePayload.clientBindingHash, clientBinding)) {
			throw InputValidationError.fromCode('state', ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE);
		}
		const tokenResponse = await this.exchangeCode({
			code,
			codeVerifier: statePayload.codeVerifier,
			redirectUri: statePayload.redirectUri ?? config.redirectUri,
			config,
		});
		const claims = await this.resolveClaims(tokenResponse, config, statePayload.nonce);
		const redirectTo = statePayload.redirectTo ?? '';
		if (statePayload.link) {
			await this.linkSignedInUser(statePayload.link, claims, config);
			return {linked: true, redirect_to: redirectTo};
		}
		const linkedNeedsSecondFactor = !state.startsWith(SSO_MOBILE_STATE_PREFIX) || MOBILE_REQUIRES_SSO_SECOND_FACTOR;
		const resolved = await this.resolveUserFromClaims(claims, config, requestCache, clientIp, linkedNeedsSecondFactor);
		if (resolved.secondFactorRequired) {
			return {...(await this.challengeSecondFactor(resolved.user, claims, config)), redirect_to: redirectTo};
		}
		const {user} = resolved;
		await this.joinCommunityAndSyncRoles(user, claims, config, requestCache);
		const [token] = await AuthSession.createAuthSession(this.apiContext, {
			user,
			origin: AuthSession.resolveSessionOrigin(this.apiContext, request),
			authMethod: 'sso',
		});
		await this.syncFromProvider(user, claims, config);
		return {token, user_id: user.id.toString(), redirect_to: redirectTo};
	}

	async isPendingLinkTicket(ticket: string): Promise<boolean> {
		return this.apiContext.services.cache.exists(pendingSsoLinkKey(ticket));
	}

	async completePendingLink(user: User, ticket: string): Promise<{user: User; authMethod?: 'sso'}> {
		const {cache} = this.apiContext.services;
		const pending = await cache.getAndDelete<PendingSsoLink | LegacyPendingSsoLink>(pendingSsoLinkKey(ticket));
		if (!pending) {
			return {user};
		}
		try {
			const config = await this.requireReadyConfig();
			if (pending.userId !== user.id.toString() || pending.providerId !== config.providerId) {
				throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
			}
			const claims = 'claims' in pending ? pending.claims : claimsFromLegacyPendingLink(pending);
			const linkedUser = await this.bindSsoIdentity(user, claims.sub, config);
			await this.joinCommunityAndSyncRoles(linkedUser, claims, config, createRequestCache());
			if ('claims' in pending && !isSignInRefused(linkedUser)) {
				await this.syncFromProvider(linkedUser, claims, config);
			}
			return {user: linkedUser, authMethod: 'sso'};
		} catch (error) {
			await cache.delete(`mfa-ticket:${ticket}`);
			throw error;
		}
	}

	private async linkSignedInUser(
		link: SsoLinkRequest,
		claims: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
	): Promise<void> {
		const {users, gateway} = this.apiContext.services;
		const session = await users.getAuthSessionByToken(Buffer.from(link.sessionIdHash, 'base64url'));
		const user = session?.userId.toString() === link.userId ? await users.findUnique(session.userId) : null;
		if (!user || isSignInRefused(user)) {
			throw InputValidationError.fromCode('state', ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE);
		}
		const usernameMode =
			(await this.instanceConfigRepository.getAccountIdentityMode()) === AccountIdentityModes.USERNAME;
		const email = resolveUsableSsoEmail(claims, config.allowedEmailDomains, usernameMode);
		const holder = await this.findMappedUser(claims.sub, config, usernameMode ? null : email);
		if (holder && holder.id !== user.id) {
			throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT);
		}
		const linkedUser = await this.bindSsoIdentity(user, claims.sub, config);
		await dispatchUserUpdate(gateway, linkedUser);
		getLogger().info({user_id: user.id.toString()}, 'Connected an SSO identity from account settings');
	}

	private async challengeSecondFactor(
		user: User,
		claims: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
	): Promise<LoginMfaResult> {
		AuthUtility.assertMfaTicketAllowed(this.apiContext, user);
		const challenge = await createMfaTicketResponse(
			this.apiContext,
			user,
			await resolveWebAuthnSecondFactor(this.apiContext, user),
		);
		const pending: PendingSsoLink = {
			userId: user.id.toString(),
			providerId: config.providerId,
			subject: claims.sub,
			groups: claims.groups,
			claims,
		};
		await this.apiContext.services.cache.set(pendingSsoLinkKey(challenge.ticket), pending, seconds('5 minutes'));
		return challenge;
	}

	private async hasSecondFactor(user: User): Promise<boolean> {
		return (
			userHasMfa(user) ||
			(await resolveWebAuthnSecondFactor(this.apiContext, user)) ||
			(await AuthMfa.hasUnconsumedBackupCodes(this.apiContext, user.id))
		);
	}

	private async joinCommunityAndSyncRoles(
		user: User,
		claims: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
		requestCache: RequestCache,
	): Promise<void> {
		if (canJoinSingleCommunity(user)) {
			await this.singleCommunityService.joinStockCommunity(user.id, requestCache);
		}
		await this.syncGroupRoles(user, claims, config, requestCache);
	}

	private async resolveUserFromClaims(
		claims: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
		requestCache: RequestCache,
		clientIp: string,
		linkedNeedsSecondFactor: boolean,
	): Promise<{user: User; secondFactorRequired: boolean}> {
		const accountIdentity = await this.instanceConfigRepository.getAccountIdentity();
		const usernameMode = accountIdentity.mode === AccountIdentityModes.USERNAME;
		const email = resolveUsableSsoEmail(claims, config.allowedEmailDomains, usernameMode);
		const mappedUser = await this.findMappedUser(claims.sub, config, usernameMode ? null : email);
		if (mappedUser) {
			if (linkedNeedsSecondFactor && (await this.hasSecondFactor(mappedUser))) {
				return {user: mappedUser, secondFactorRequired: true};
			}
			return {user: await this.bindSsoIdentity(mappedUser, claims.sub, config), secondFactorRequired: false};
		}
		const {users} = this.apiContext.services;
		const existingUser = email ? await users.findByEmail(email) : null;
		if (existingUser) {
			this.assertLinkable(existingUser, claims.sub, config);
			if (
				!existingUser.traits.has(ssoIdentityTrait(config.providerId, claims.sub)) &&
				(!existingUser.emailVerified || !(await this.instanceConfigRepository.isEmailEnabled()))
			) {
				throw InputValidationError.fromCode('email', ValidationErrorCodes.SSO_ACCOUNT_LINK_REQUIRES_SIGN_IN);
			}
			if (await this.hasSecondFactor(existingUser)) {
				return {user: existingUser, secondFactorRequired: true};
			}
			return {user: await this.bindSsoIdentity(existingUser, claims.sub, config), secondFactorRequired: false};
		}
		AuthUtility.assertNetworkAllowed(this.apiContext);
		if (!config.autoProvision) {
			throw new SsoRequiredError(SsoRequiredReasons.NO_ACCOUNT);
		}
		await assertAccountCreationPastSetup(this.apiContext, this.instanceConfigRepository, clientIp);
		const registrationConfig = await this.instanceConfigRepository.getRegistrationConfig();
		if (email) {
			await assertEmailNotBlocklisted(email, 'email');
		}
		const pendingApproval = registrationConfig.mode === 'approval';
		const autoJoinInviteCode = this.apiContext.services.config.instance.autoJoinInviteCode?.trim() || null;
		const user = await this.provisionUserFromClaims({...claims, email}, config, {
			usernameMode,
			uniqueUsernames: accountIdentity.tagStyle === TagStyles.NONE,
			pendingApproval,
			autoJoinInviteCode,
		});
		if (pendingApproval) {
			throw new RegistrationPendingApprovalError();
		}
		await maybeAutoJoinInvite(this.inviteService, {
			userId: user.id,
			inviteCode: autoJoinInviteCode,
			requestCache,
			ip: clientIp,
		});
		return {user, secondFactorRequired: false};
	}

	private async syncGroupRoles(
		user: User,
		{groups, groupsOverage}: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
		requestCache: RequestCache,
	): Promise<void> {
		if (config.groupRoleMappings.length === 0) return;
		const guildId = await this.singleCommunityService.getStockCommunityId();
		if (guildId === null) return;
		if (groups === null) {
			getLogger().warn(
				{user_id: user.id.toString(), groups_overage: groupsOverage},
				groupsOverage
					? 'SSO provider left out the groups claim because the user is in too many groups; removing mapped roles. In Entra, limit the token to the groups you map'
					: 'SSO provider sent no groups claim; removing mapped roles',
			);
		}
		const memberOf = new Set(groups ?? []);
		try {
			await this.guildMemberService.systemSyncManagedRoles({
				targetId: user.id,
				guildId,
				managedRoleIds: new Set(config.groupRoleMappings.map(({roleId}) => createRoleID(BigInt(roleId)))),
				grantedRoleIds: new Set(
					config.groupRoleMappings
						.filter(({group}) => memberOf.has(group))
						.map(({roleId}) => createRoleID(BigInt(roleId))),
				),
				requestCache,
			});
		} catch (error) {
			getLogger().error({error, user_id: user.id.toString()}, 'Failed to sync community roles from SSO groups');
		}
	}

	private async findMappedUser(sub: string, config: ResolvedSsoConfig, email: string | null): Promise<User | null> {
		const identityUserId = await this.ssoIdentityRepository.findUserId(config.providerId, sub);
		if (!identityUserId) {
			return null;
		}
		const user = await this.apiContext.services.users.findUnique(identityUserId);
		if (!user) {
			getLogger().error(
				{user_id: identityUserId.toString()},
				'SSO identity mapping points at a missing user; refusing reassignment',
			);
			throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT);
		}
		if ((user.flags & UserFlags.DELETED) === 0n) {
			return user;
		}
		if (isEnforcementDeletion(user) && (email === null || (await isEmailBlocklisted(email)))) {
			return user;
		}
		await this.ssoIdentityRepository.releaseIdentity(config.providerId, sub);
		getLogger().info({user_id: user.id.toString()}, 'Released SSO identity of a deleted account');
		return null;
	}

	private async syncFromProvider(user: User, claims: ResolvedSsoClaims, config: ResolvedSsoConfig): Promise<void> {
		if (config.enforced && claims.email && claims.emailVerified) {
			await this.syncEnforcedEmail(user, claims.email);
		}
		if (!user.isBot && (await this.takesUsernameFromProvider(config))) {
			await this.syncProviderUsername(user, claims);
		}
	}

	private async syncEnforcedEmail(user: User, email: string): Promise<void> {
		if (user.email?.toLowerCase() === email) {
			return;
		}
		const accountIdentity = await this.instanceConfigRepository.getAccountIdentity();
		if (accountIdentity.mode === AccountIdentityModes.USERNAME) {
			return;
		}
		const {users, gateway, worker} = this.apiContext.services;
		const holder = await users.findByEmail(email);
		if (holder) {
			getLogger().warn(
				{user_id: user.id.toString(), holder_user_id: holder.id.toString()},
				'SSO provider email belongs to another account; keeping the current email',
			);
			return;
		}
		const updatedUser = await users.patchUpsert(
			user.id,
			{email, email_verified: true, email_bounced: false},
			user.toRow(),
		);
		await enqueueStripeCustomerEmailSync(worker, user, updatedUser);
		await dispatchUserUpdate(gateway, updatedUser);
	}

	private async syncProviderUsername(user: User, claims: ResolvedSsoClaims): Promise<void> {
		const userId = user.id.toString();
		const username = usernameFromPreferredUsername(claims);
		if (!username) {
			getLogger().info({user_id: userId}, 'SSO provider sent no usable username; keeping the current username');
			return;
		}
		if (username === user.username) {
			return;
		}
		if (
			!isProfileSubstringExempt(user) &&
			profileSubstringBlocklistCache.containsBannedSubstring('username', username)
		) {
			getLogger().warn({user_id: userId}, 'SSO provider username is blocked; keeping the current username');
			return;
		}
		try {
			await renameUser(
				{
					apiContext: this.apiContext,
					discriminatorService: this.discriminatorService,
					userUpdateDeps: this.userUpdateDeps,
					guildRepository: this.guildRepository,
				},
				user,
				{username, discriminator: USERNAME_MODE_DISCRIMINATOR},
				{reason: 'sso_sync', actorUserId: null},
			);
		} catch (error) {
			if (isUsernameTakenError(error)) {
				getLogger().warn({user_id: userId}, 'SSO provider username is unavailable; keeping the current username');
			} else {
				getLogger().error({error, user_id: userId}, 'Failed to sync username from SSO provider');
			}
			return;
		}
		getLogger().info({user_id: userId, old: user.username, new: username}, 'Synced username from SSO provider');
	}

	private async claimSsoIdentity(userId: UserID, sub: string, config: ResolvedSsoConfig): Promise<void> {
		const claimed = await this.ssoIdentityRepository.tryClaimIdentity({
			providerId: config.providerId,
			subject: sub,
			userId,
			claimedAt: new Date(),
		});
		if (claimed) {
			return;
		}
		const owned = await this.ssoIdentityRepository.findIdentity(config.providerId, sub);
		if (owned?.user_id === userId) {
			await this.ssoIdentityRepository.indexIdentity(owned);
			return;
		}
		getLogger().error(
			{user_id: userId.toString(), owner_user_id: owned?.user_id.toString() ?? null},
			'SSO identity is already linked to another account',
		);
		throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT);
	}

	private assertLinkable(user: User, sub: string, config: ResolvedSsoConfig): void {
		if (isLinkedToSsoProvider(user, config.providerId) && !user.traits.has(ssoIdentityTrait(config.providerId, sub))) {
			getLogger().error({user_id: user.id.toString()}, 'SSO identity claim did not match linked account');
			throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_ACCOUNT_LINKED_TO_ANOTHER_IDENTITY);
		}
	}

	private async bindSsoIdentity(user: User, sub: string, config: ResolvedSsoConfig): Promise<User> {
		const identityTrait = ssoIdentityTrait(config.providerId, sub);
		const traits = user.traits;
		this.assertLinkable(user, sub, config);
		await this.claimSsoIdentity(user.id, sub, config);
		if (traits.has(identityTrait)) {
			return user;
		}
		for (const trait of ssoIdentityTraits(config.providerId, sub)) {
			traits.add(trait);
		}
		const {users} = this.apiContext.services;
		return users.patchUpsert(user.id, {traits}, user.toRow());
	}

	private async allocateDiscriminator(username: string): Promise<number> {
		const result = await this.discriminatorService.generateDiscriminator({username});
		if (!result.available) {
			throw InputValidationError.fromCode('username', ValidationErrorCodes.SSO_UNABLE_TO_ALLOCATE_DISCRIMINATOR);
		}
		return result.discriminator;
	}

	private async provisionUserFromClaims(
		claims: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
		options: {
			usernameMode: boolean;
			uniqueUsernames: boolean;
			pendingApproval: boolean;
			autoJoinInviteCode: string | null;
		},
	): Promise<User> {
		const {users, snowflake, cache} = this.apiContext.services;
		const {usernameMode, uniqueUsernames} = options;
		const userId = (await snowflake.generate()) as UserID;
		const derivedUsername = deriveSsoUsernameBase(claims) ?? generateRandomUsername();
		const username = uniqueUsernames ? await deriveAvailableUsername(users, derivedUsername) : derivedUsername;
		const discriminator = uniqueUsernames ? USERNAME_MODE_DISCRIMINATOR : await this.allocateDiscriminator(username);
		const now = new Date();
		const traits = new Set<string>(ssoIdentityTraits(config.providerId, claims.sub));
		if (options.pendingApproval) {
			traits.add(REGISTRATION_PENDING_APPROVAL_TRAIT);
		}
		if (instanceCollectsDateOfBirth()) {
			traits.add(DOB_DEFERRED_TRAIT);
		}
		const globalName = claims.name?.substring(0, 256) ?? username;
		if (
			profileSubstringBlocklistCache.containsBannedSubstring('username', username) ||
			profileSubstringBlocklistCache.containsBannedSubstring('global_name', globalName)
		) {
			throw new ContentBlockedError();
		}
		const userRow: UserRow = {
			...EMPTY_USER_ROW,
			user_id: userId,
			username,
			discriminator,
			global_name: globalName,
			bot: false,
			system: false,
			email: usernameMode ? null : claims.email,
			email_verified: claims.email !== null && claims.emailVerified,
			email_bounced: false,
			timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
			flags: 0n,
			has_ever_purchased: false,
			terms_agreed_at: null,
			privacy_agreed_at: null,
			last_active_at: now,
			acls: new Set<string>(),
			traits,
			version: 1,
		};
		await this.claimSsoIdentity(userId, claims.sub, config);
		let creationStage: 'unattempted' | 'attempted' | 'created' = 'unattempted';
		let usernameReservation: UsernameReservation | null = null;
		try {
			if (uniqueUsernames) {
				usernameReservation = await reserveUsername({users, cache}, username);
			}
			if (options.pendingApproval) {
				await this.instanceConfigRepository.addPendingRegistration({
					user_id: userId.toString(),
					username,
					discriminator,
					global_name: globalName,
					email: userRow.email,
					requested_at: now.toISOString(),
					registration_url_id: null,
					client_ip: null,
					invite_code: options.autoJoinInviteCode,
				});
			}
			creationStage = 'attempted';
			const user = await users.create(userRow);
			creationStage = 'created';
			await users.upsertSettings(
				UserSettings.getDefaultUserSettings({
					userId,
					locale: 'en-US',
					isAdult: !instanceCollectsDateOfBirth(),
				}),
			);
			void this.kvActivityTracker.updateActivity(user.id, now).catch((error: unknown) => {
				getLogger().warn({error, userId: user.id}, 'Failed to update real-time user activity');
			});
			await emitActivity(
				'registration',
				user.id.toString(),
				{
					user_id: user.id.toString(),
					method: 'oauth',
					source: 'app',
					email: user.email,
					username: user.username,
					username_user_chosen: false,
					global_name: user.globalName,
					locale: user.locale,
					timezone: null,
					invite_code: null,
					flags: user.flags.toString(),
					date_of_birth: user.dateOfBirth,
				},
				null,
				user.id.toString(),
			);
			return user;
		} catch (error) {
			if (creationStage !== 'created') {
				await this.ssoIdentityRepository.releaseIdentity(config.providerId, claims.sub).catch((releaseError) => {
					getLogger().error({releaseError}, 'Failed to release SSO identity after user provisioning failed');
				});
				if (options.pendingApproval && creationStage === 'unattempted') {
					await this.instanceConfigRepository
						.removePendingRegistration(userId.toString())
						.catch((removeError: unknown) => {
							getLogger().error(
								{userId: userId.toString(), removeError},
								'Failed to withdraw the pending approval of an SSO user that was never created',
							);
						});
				}
			}
			throw error;
		} finally {
			await usernameReservation?.release();
		}
	}

	private async resolveClaims(
		tokenResponse: {
			id_token?: string;
			access_token?: string;
		},
		config: ResolvedSsoConfig,
		expectedNonce: string,
	): Promise<ResolvedSsoClaims> {
		if (config.isTestProvider) {
			const rawEmail = tokenResponse.id_token || tokenResponse.access_token;
			if (!rawEmail) {
				throw InputValidationError.fromCode('code', ValidationErrorCodes.SSO_TEST_CODE_MISSING_EMAIL);
			}
			return parseTestProviderClaims(rawEmail, config.groupsClaim ?? DEFAULT_GROUPS_CLAIM);
		}
		let claims: JWTPayload | null = null;
		if (tokenResponse.id_token) {
			if (!config.jwksUrl) {
				getLogger().warn('SSO id_token returned but no JWKS URL is configured; ignoring id_token claims');
			} else {
				claims = await this.verifyIdToken(tokenResponse.id_token, config, expectedNonce);
			}
		}
		let userInfo: Record<string, unknown> | null = null;
		if (config.userInfoUrl && tokenResponse.access_token) {
			userInfo = await fetchUserInfo(config.userInfoUrl, tokenResponse.access_token).catch(rethrowAsSsoError);
		}
		const idTokenSub = claims ? readStringClaim(claims, 'sub') : undefined;
		const userInfoSub = userInfo ? readStringClaim(userInfo, 'sub') : undefined;
		if (claims && userInfo) {
			if (!idTokenSub || !userInfoSub || idTokenSub !== userInfoSub) {
				getLogger().error('SSO sub mismatch between id_token and userinfo');
				throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_MISMATCH);
			}
		}
		if (!claims && !userInfo) {
			throw InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_MISCONFIGURED);
		}
		const rawSub = idTokenSub ?? userInfoSub;
		if (!rawSub) {
			throw InputValidationError.fromCode('sub', ValidationErrorCodes.INVALID_SSO_TOKEN);
		}
		const sub = normalizeSsoSubject(rawSub);
		const idTokenEmail = readStringClaim(claims, 'email');
		const userInfoEmail = readStringClaim(userInfo, 'email');
		const normalizedIdTokenEmail = idTokenEmail ? normalizeSsoEmail(idTokenEmail) : undefined;
		const normalizedUserInfoEmail = userInfoEmail ? normalizeSsoEmail(userInfoEmail) : undefined;
		if (normalizedIdTokenEmail && normalizedUserInfoEmail && normalizedIdTokenEmail !== normalizedUserInfoEmail) {
			getLogger().error('SSO email mismatch between id_token and userinfo');
			throw InputValidationError.fromCode('email', ValidationErrorCodes.SSO_IDENTITY_MISMATCH);
		}
		const email = normalizedIdTokenEmail ?? normalizedUserInfoEmail ?? null;
		const emailVerified =
			email !== null &&
			resolveEmailVerified({
				claims,
				userInfo,
				idTokenEmail: normalizedIdTokenEmail,
				userInfoEmail: normalizedUserInfoEmail,
				email,
			});
		return {
			email,
			emailVerified,
			preferredUsername:
				readTrimmedClaim(claims, 'preferred_username') ?? readTrimmedClaim(userInfo, 'preferred_username'),
			name: readDisplayName(claims) ?? readDisplayName(userInfo),
			sub,
			groups:
				readGroupsClaim(claims, config.groupsClaim ?? DEFAULT_GROUPS_CLAIM) ??
				readGroupsClaim(userInfo, config.groupsClaim ?? DEFAULT_GROUPS_CLAIM),
			groupsOverage:
				hasGroupsOverage(claims, config.groupsClaim ?? DEFAULT_GROUPS_CLAIM) ||
				hasGroupsOverage(userInfo, config.groupsClaim ?? DEFAULT_GROUPS_CLAIM),
		};
	}

	private async verifyIdToken(idToken: string, config: ResolvedSsoConfig, expectedNonce: string): Promise<JWTPayload> {
		if (!config.jwksUrl || !config.issuerForVerification) {
			getLogger().error('SSO id_token cannot be verified without a JWKS URL and an issuer');
			throw InputValidationError.fromCode('id_token', ValidationErrorCodes.INVALID_SSO_TOKEN);
		}
		return verifyIdToken({
			idToken,
			jwksUrl: config.jwksUrl,
			issuer: config.issuerForVerification,
			clientId: config.clientId ?? '',
			nonce: expectedNonce,
		}).catch(rethrowAsSsoError);
	}

	private async exchangeCode({
		code,
		codeVerifier,
		redirectUri,
		config,
	}: {
		code: string;
		codeVerifier: string;
		redirectUri: string;
		config: ResolvedSsoConfig;
	}): Promise<ProviderTokens> {
		if (config.isTestProvider) {
			return {id_token: code};
		}
		return exchangeAuthorizationCode(
			{
				tokenUrl: config.tokenUrl ?? '',
				clientId: config.clientId ?? '',
				clientSecret: config.clientSecret ?? null,
				authMethods: config.tokenEndpointAuthMethods,
			},
			{code, codeVerifier, redirectUri},
		).catch(rethrowAsSsoError);
	}

	private async getResolvedConfig(): Promise<ResolvedSsoConfig> {
		const {config} = this.apiContext.services;
		const stored = await this.instanceConfigRepository.getSsoConfig({includeSecret: true});
		const isTestProvider = isTestSsoProvider(stored, config.dev.testModeEnabled);
		const resolved: ResolvedSsoConfig = {
			...stored,
			redirectUri: deriveSsoRedirectUri(config.endpoints.webApp),
			scope: stored.scope?.trim() || 'openid email profile',
			ready: false,
			providerId: ssoProviderIdOf(stored),
			isTestProvider,
			issuerForVerification: stored.issuer,
			tokenEndpointAuthMethods: null,
			discoveryFailed: false,
		};
		if (!stored.enabled) {
			return resolved;
		}
		if (!isTestProvider && !stored.issuer) {
			warnAboutMissingIssuer();
			return resolved;
		}
		const needsDiscovery = !stored.authorizationUrl || !stored.tokenUrl || !stored.jwksUrl || !stored.userInfoUrl;
		const discovered =
			stored.issuer && needsDiscovery
				? await resolveOidcDiscovery(this.apiContext.services.cache, stored.issuer)
				: null;
		resolved.discoveryFailed = needsDiscovery && !discovered;
		if (discovered) {
			resolved.authorizationUrl ??= discoveredHttpsUrl(discovered.authorization_endpoint);
			resolved.tokenUrl ??= discoveredHttpsUrl(discovered.token_endpoint);
			resolved.userInfoUrl ??= discoveredHttpsUrl(discovered.userinfo_endpoint);
			resolved.jwksUrl ??= discoveredHttpsUrl(discovered.jwks_uri);
			resolved.issuerForVerification = discovered.issuer;
			resolved.tokenEndpointAuthMethods = discovered.token_endpoint_auth_methods_supported ?? null;
		}
		const canResolveClaims = isTestProvider || Boolean(resolved.jwksUrl) || Boolean(resolved.userInfoUrl);
		resolved.ready =
			(isTestProvider || Boolean(stored.issuer)) &&
			Boolean(resolved.authorizationUrl) &&
			Boolean(resolved.tokenUrl) &&
			Boolean(stored.clientId) &&
			canResolveClaims;
		return resolved;
	}

	private async requireReadyConfig(): Promise<ResolvedSsoConfig> {
		const config = await this.getResolvedConfig();
		if (!config.ready) {
			throw config.discoveryFailed ? new SsoProviderUnavailableError() : new FeatureTemporarilyDisabledError();
		}
		return config;
	}
}
