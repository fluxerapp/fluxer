// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {RegistrationClosedError} from '@fluxer/errors/src/domains/auth/RegistrationClosedError';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {SsoRequiredError} from '@fluxer/errors/src/domains/auth/SsoRequiredError';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {EmailType} from '@fluxer/schema/src/primitives/UserValidators';
import {ms, seconds} from 'itty-time';
import {
	type CryptoKey,
	createLocalJWKSet,
	decodeJwt,
	type FlattenedJWSInput,
	errors as JoseErrors,
	type JSONWebKeySet,
	type JWSHeaderParameters,
	type JWTPayload,
	jwtVerify,
} from 'jose';
import type {ApiContext} from '../../ApiContext';
import type {UserID} from '../../BrandedTypes';
import type {IDiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {KVActivityTracker} from '../../infrastructure/KVActivityTracker';
import {
	type InstanceConfigRepository,
	type InstanceSsoConfig,
	REGISTRATION_PENDING_APPROVAL_TRAIT,
} from '../../instance/InstanceConfigRepository';
import {
	deriveSsoRedirectUri,
	isTestSsoProvider,
	validateSsoPublicOutboundUrl,
} from '../../instance/SsoConfigValidation';
import {Logger} from '../../Logger';
import {profileSubstringBlocklistCache} from '../../middleware/ProfileSubstringBlocklistCache';
import type {User} from '../../models/User';
import {UserSettings} from '../../models/UserSettings';
import {EXTERNAL_RESPONSE_LIMITS} from '../../utils/ExternalResponseLimits';
import * as FetchUtils from '../../utils/FetchUtils';
import {isJsonRecord, parseJsonRecord, parseJsonWithGuard} from '../../utils/JsonBoundaryUtils';
import {generateRandomUsername} from '../../utils/UsernameGenerator';
import {deriveUsernameFromDisplayName} from '../../utils/UsernameSuggestionUtils';
import * as AuthSession from '../AuthSession';
import {SsoIdentityRepository} from './SsoIdentityRepository';
import {parseTokenEndpointResponse, sanitizeSsoRedirectTo, tryDiscoverOidcProviderMetadata} from './SsoUtils';

interface SsoStatePayload {
	codeVerifier: string;
	nonce: string;
	redirectTo?: string;
	redirectUri?: string;
	createdAt: number;
}

interface PublicSsoStatus {
	enabled: boolean;
	enforced: boolean;
	display_name: string | null;
	redirect_uri: string;
}

interface ResolvedSsoConfig extends InstanceSsoConfig {
	redirectUri: string;
	scope: string;
	ready: boolean;
	providerId: string;
	isTestProvider: boolean;
	issuerForVerification: string | null;
}

interface ResolvedSsoClaims {
	email: string;
	emailVerified: boolean;
	name?: string | null;
	sub: string;
}

interface RemoteJwkSetResolver {
	(protectedHeader?: JWSHeaderParameters, token?: FlattenedJWSInput): Promise<CryptoKey>;
	coolingDown: boolean;
	fresh: boolean;
	reloading: boolean;
	reload: () => Promise<void>;
	jwks: () => JSONWebKeySet | undefined;
}

interface JwksCacheEntry {
	jwks: RemoteJwkSetResolver;
	cachedAt: number;
}

const CODE_VERIFIER_BYTE_LENGTH = 32;
const STATE_BYTE_LENGTH = 16;
const NONCE_BYTE_LENGTH = 16;
const MOBILE_SSO_REDIRECT_URI = 'fluxer://auth/sso/callback';

function randomBase64UrlToken(byteLength: number): string {
	return randomBytes(byteLength).toString('base64url');
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

function buildDiscoveryCacheKey(issuer: string): string {
	const key = createHash('sha256').update(issuer).digest('hex').slice(0, 32);
	return `sso:oidc-discovery:${key}`;
}

function resolveSsoRedirectUri(requestedRedirectUri: string | undefined, defaultRedirectUri: string): string {
	if (!requestedRedirectUri) return defaultRedirectUri;
	const trimmed = requestedRedirectUri.trim();
	if (!trimmed) return defaultRedirectUri;
	if (trimmed === defaultRedirectUri || trimmed === MOBILE_SSO_REDIRECT_URI) return trimmed;
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

function buildSsoProviderTrait(providerId: string): string {
	const digest = createHash('sha256').update(providerId).digest('hex').slice(0, 32);
	return `sso_provider:${digest}`;
}

function buildSsoIdentityTrait(providerId: string, sub: string): string {
	const digest = createHash('sha256').update(providerId).update('\0').update(sub).digest('hex');
	return `sso_identity:${digest}`;
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

function parseTestProviderClaims(rawPayload: string): ResolvedSsoClaims {
	const parsed = rawPayload.trim().startsWith('{') ? parseJsonRecord(rawPayload) : null;
	const rawEmail = parsed ? readStringClaim(parsed, 'email') : rawPayload;
	if (!rawEmail) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.SSO_TEST_CODE_MISSING_EMAIL);
	}
	const email = normalizeSsoEmail(rawEmail);
	const sub = normalizeSsoSubject(parsed ? (readStringClaim(parsed, 'sub') ?? email) : email);
	const emailVerified = parsed ? (coerceEmailVerified(parsed['email_verified']) ?? true) : true;
	const name = parsed ? (readStringClaim(parsed, 'name') ?? 'Test SSO User') : 'Test SSO User';
	return {email, emailVerified, name, sub};
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
	return values.includes(true);
}

function isJsonWebKeySet(value: unknown): value is JSONWebKeySet {
	if (!isJsonRecord(value)) return false;
	const keys = value['keys'];
	return Array.isArray(keys) && keys.every(isJsonRecord);
}

export class SsoService {
	private readonly logger = Logger.child({logger: 'SsoService'});
	private static readonly STATE_TTL_SECONDS = seconds('10 minutes');
	private static readonly DISCOVERY_TTL_SECONDS = seconds('1 hour');
	private static readonly JWKS_CACHE_TTL_MS = ms('1 hour');
	private readonly jwksCache = new Map<string, JwksCacheEntry>();
	private readonly ssoIdentityRepository = new SsoIdentityRepository();

	constructor(
		private readonly apiContext: ApiContext,
		private readonly instanceConfigRepository: InstanceConfigRepository,
		private readonly discriminatorService: IDiscriminatorService,
		private readonly kvActivityTracker: KVActivityTracker,
	) {}

	async getPublicStatus(): Promise<PublicSsoStatus> {
		const config = await this.getResolvedConfig();
		const enabled = config.enabled && config.ready;
		return {
			enabled,
			enforced: enabled && config.enforced,
			display_name: config.displayName ?? null,
			redirect_uri: config.redirectUri,
		};
	}

	async isEnforced(): Promise<boolean> {
		const config = await this.getResolvedConfig();
		return config.enabled && config.ready && config.enforced;
	}

	async startLogin({redirectTo, redirectUri}: {redirectTo?: string; redirectUri?: string} = {}): Promise<{
		authorization_url: string;
		state: string;
		redirect_uri: string;
	}> {
		const config = await this.requireReadyConfig();
		const state = randomHexToken(STATE_BYTE_LENGTH);
		const codeVerifier = randomBase64UrlToken(CODE_VERIFIER_BYTE_LENGTH);
		const codeChallenge = buildCodeChallenge(codeVerifier);
		const nonce = randomBase64UrlToken(NONCE_BYTE_LENGTH);
		const ssoRedirectUri = resolveSsoRedirectUri(redirectUri, config.redirectUri);
		const statePayload: SsoStatePayload = {
			codeVerifier,
			nonce,
			redirectTo: sanitizeSsoRedirectTo(redirectTo),
			redirectUri: ssoRedirectUri,
			createdAt: Date.now(),
		};
		const {cache} = this.apiContext.services;
		await cache.set(buildStateCacheKey(state), statePayload, SsoService.STATE_TTL_SECONDS);
		const searchParams = new URLSearchParams({
			response_type: 'code',
			client_id: config.clientId ?? '',
			redirect_uri: ssoRedirectUri,
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
		return {authorization_url: authorizationUrlString, state, redirect_uri: ssoRedirectUri};
	}

	async completeLogin({code, state, request}: {code: string; state: string; request: Request}): Promise<{
		token: string;
		user_id: string;
		redirect_to: string;
	}> {
		const config = await this.requireReadyConfig();
		const {cache} = this.apiContext.services;
		const statePayload = await cache.getAndDelete<SsoStatePayload>(buildStateCacheKey(state));
		if (!statePayload) {
			throw InputValidationError.fromCode('state', ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE);
		}
		const tokenResponse = await this.exchangeCode({
			code,
			codeVerifier: statePayload.codeVerifier,
			redirectUri: statePayload.redirectUri ?? config.redirectUri,
			config,
		});
		const claims = await this.resolveClaims(tokenResponse, config, statePayload.nonce);
		const user = await this.resolveUserFromClaims(claims, config);
		const [token] = await AuthSession.createAuthSession(this.apiContext, {user, request});
		return {token, user_id: user.id.toString(), redirect_to: statePayload.redirectTo ?? ''};
	}

	private async resolveUserFromClaims(claims: ResolvedSsoClaims, config: ResolvedSsoConfig): Promise<User> {
		if (!claims.emailVerified) {
			throw InputValidationError.fromCode('email_verified', ValidationErrorCodes.INVALID_SSO_TOKEN);
		}
		const emailLower = claims.email.toLowerCase();
		this.logger.info({email: emailLower, has_sub: true}, 'SSO login with sub claim');
		const identityUserId = await this.ssoIdentityRepository.findUserId(config.providerId, claims.sub);
		if (identityUserId) {
			const user = await this.apiContext.services.users.findUnique(identityUserId);
			if (!user) {
				this.logger.error(
					{user_id: identityUserId.toString()},
					'SSO identity mapping points at a missing user; refusing reassignment',
				);
				throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_MISMATCH);
			}
			return this.bindSsoIdentity(user, claims.sub, config);
		}
		const {users} = this.apiContext.services;
		const existingUser = await users.findByEmail(emailLower);
		if (existingUser) {
			return this.bindSsoIdentity(existingUser, claims.sub, config);
		}
		if (!config.autoProvision) {
			throw new SsoRequiredError();
		}
		const registrationConfig = await this.instanceConfigRepository.getRegistrationConfig();
		if (registrationConfig.mode === 'closed') {
			throw new RegistrationClosedError();
		}
		const pendingApproval = registrationConfig.mode === 'approval';
		const user = await this.provisionUserFromClaims(claims, config, {pendingApproval});
		if (pendingApproval) {
			await this.instanceConfigRepository.addPendingRegistration({
				user_id: user.id.toString(),
				username: user.username,
				discriminator: user.discriminator,
				global_name: user.globalName,
				email: user.email,
				requested_at: new Date().toISOString(),
				registration_url_id: null,
				client_ip: null,
			});
			throw new RegistrationPendingApprovalError();
		}
		return user;
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
		const ownerId = await this.ssoIdentityRepository.findUserId(config.providerId, sub);
		if (ownerId?.toString() === userId.toString()) {
			return;
		}
		this.logger.error(
			{user_id: userId.toString(), owner_user_id: ownerId?.toString() ?? null},
			'SSO identity is already linked to another account',
		);
		throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_MISMATCH);
	}

	private async bindSsoIdentity(user: User, sub: string, config: ResolvedSsoConfig): Promise<User> {
		const identityTrait = buildSsoIdentityTrait(config.providerId, sub);
		const traits = user.traits;
		const existingIdentities = Array.from(traits).filter((trait) => trait.startsWith('sso_identity:'));
		if (existingIdentities.length > 0 && !traits.has(identityTrait)) {
			this.logger.error({user_id: user.id.toString()}, 'SSO identity claim did not match linked account');
			throw InputValidationError.fromCode('sub', ValidationErrorCodes.SSO_IDENTITY_MISMATCH);
		}
		await this.claimSsoIdentity(user.id, sub, config);
		if (traits.has(identityTrait)) {
			return user;
		}
		traits.add('sso');
		traits.add(`sso:${config.providerId}`);
		traits.add(buildSsoProviderTrait(config.providerId));
		traits.add(identityTrait);
		const {users} = this.apiContext.services;
		return users.patchUpsert(user.id, {traits}, user.toRow());
	}

	private async provisionUserFromClaims(
		claims: ResolvedSsoClaims,
		config: ResolvedSsoConfig,
		options?: {
			pendingApproval?: boolean;
		},
	): Promise<User> {
		const {users, snowflake} = this.apiContext.services;
		const userId = (await snowflake.generate()) as UserID;
		const baseName = claims.name?.trim() || claims.email.split('@')[0] || generateRandomUsername();
		const username = deriveUsernameFromDisplayName(baseName) ?? generateRandomUsername();
		const discriminatorResult = await this.discriminatorService.generateDiscriminator({username});
		if (!discriminatorResult.available) {
			throw InputValidationError.fromCode('username', ValidationErrorCodes.SSO_UNABLE_TO_ALLOCATE_DISCRIMINATOR);
		}
		const now = new Date();
		const traits = new Set<string>([
			'sso',
			`sso:${config.providerId}`,
			buildSsoProviderTrait(config.providerId),
			buildSsoIdentityTrait(config.providerId, claims.sub),
		]);
		if (options?.pendingApproval) {
			traits.add(REGISTRATION_PENDING_APPROVAL_TRAIT);
		}
		const globalName = claims.name?.substring(0, 256) ?? username;
		if (
			profileSubstringBlocklistCache.containsBannedSubstring('username', username) ||
			profileSubstringBlocklistCache.containsBannedSubstring('global_name', globalName)
		) {
			throw new ContentBlockedError();
		}
		const userRow = {
			user_id: userId,
			username,
			discriminator: discriminatorResult.discriminator,
			global_name: globalName,
			bot: false,
			system: false,
			email: claims.email.toLowerCase(),
			email_verified: claims.emailVerified,
			email_bounced: false,
			phone: null,
			password_hash: null,
			password_last_changed_at: null,
			totp_secret: null,
			authenticator_types: null,
			avatar_hash: null,
			avatar_color: null,
			banner_hash: null,
			banner_color: null,
			bio: null,
			pronouns: null,
			accent_color: null,
			timezone: null,
			timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
			date_of_birth: null,
			locale: null,
			flags: 0n,
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
			has_ever_purchased: false,
			suspicious_activity_flags: 0,
			terms_agreed_at: now,
			privacy_agreed_at: now,
			last_active_at: now,
			last_active_ip: null,
			temp_banned_until: null,
			pending_bulk_message_deletion_at: null,
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
			pending_deletion_at: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
			acls: new Set<string>(),
			traits,
			first_refund_at: null,
			gift_inventory_server_seq: null,
			gift_inventory_client_seq: null,
			premium_onboarding_dismissed_at: null,
			mention_flags: null,
			last_voice_activity_sharing_change_at: null,
			version: 1,
		} as const;
		await this.claimSsoIdentity(userId, claims.sub, config);
		let userCreated = false;
		try {
			const user = await users.create(userRow);
			userCreated = true;
			await users.upsertSettings(
				UserSettings.getDefaultUserSettings({
					userId,
					locale: 'en-US',
					isAdult: true,
				}),
			);
			await this.kvActivityTracker.updateActivity(user.id, now);
			return user;
		} catch (error) {
			if (!userCreated) {
				await this.ssoIdentityRepository.releaseIdentity(config.providerId, claims.sub).catch((releaseError) => {
					this.logger.error({releaseError}, 'Failed to release SSO identity after user provisioning failed');
				});
			}
			throw error;
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
			const testClaims = parseTestProviderClaims(rawEmail);
			validateEmailAgainstAllowlist(testClaims.email, config.allowedEmailDomains);
			return testClaims;
		}
		let claims: JWTPayload | null = null;
		if (tokenResponse.id_token) {
			if (!config.jwksUrl) {
				this.logger.warn('SSO id_token returned but no JWKS URL is configured; ignoring id_token claims');
			} else {
				claims = await this.verifyIdToken(tokenResponse.id_token, config, expectedNonce);
			}
		}
		let userInfo: Record<string, unknown> | null = null;
		if (config.userInfoUrl && tokenResponse.access_token) {
			userInfo = await this.fetchUserInfo(config.userInfoUrl, tokenResponse.access_token);
		}
		const idTokenSub = claims ? readStringClaim(claims, 'sub') : undefined;
		const userInfoSub = userInfo ? readStringClaim(userInfo, 'sub') : undefined;
		if (claims && userInfo) {
			if (!idTokenSub || !userInfoSub || idTokenSub !== userInfoSub) {
				this.logger.error('SSO sub mismatch between id_token and userinfo');
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
			this.logger.error('SSO email mismatch between id_token and userinfo');
			throw InputValidationError.fromCode('email', ValidationErrorCodes.SSO_IDENTITY_MISMATCH);
		}
		const email =
			normalizedIdTokenEmail ??
			normalizedUserInfoEmail ??
			(() => {
				throw InputValidationError.fromCode('email', ValidationErrorCodes.SSO_PROVIDER_DID_NOT_RETURN_EMAIL);
			})();
		const primaryClaims = normalizedIdTokenEmail ? claims : userInfo;
		const emailVerified = resolveEmailVerified({
			claims,
			userInfo,
			idTokenEmail: normalizedIdTokenEmail,
			userInfoEmail: normalizedUserInfoEmail,
			email,
		});
		if (!emailVerified) {
			throw InputValidationError.fromCode('email_verified', ValidationErrorCodes.INVALID_SSO_TOKEN);
		}
		const name = readStringClaim(primaryClaims, 'name') ?? null;
		validateEmailAgainstAllowlist(email, config.allowedEmailDomains);
		const resolvedClaims: ResolvedSsoClaims = {email, emailVerified, name, sub};
		return resolvedClaims;
	}

	private async verifyIdToken(idToken: string, config: ResolvedSsoConfig, expectedNonce: string): Promise<JWTPayload> {
		try {
			if (config.jwksUrl) {
				const jwks = await this.getOrCreateJwks(config.jwksUrl);
				const {payload} = await jwtVerify(idToken, jwks, {
					issuer: config.issuerForVerification ?? undefined,
					audience: config.clientId ?? undefined,
					clockTolerance: 10,
				});
				const nonce = payload['nonce'];
				if (nonce === undefined) {
					this.logger.warn('SSO id_token missing required nonce claim');
					throw new Error('nonce missing');
				}
				if (typeof nonce !== 'string' || nonce.length === 0 || nonce !== expectedNonce) {
					throw new Error('nonce mismatch');
				}
				return payload;
			}
			return decodeJwt(idToken);
		} catch (error) {
			this.logger.error({error}, 'Failed to verify SSO id_token');
			throw InputValidationError.fromCode('id_token', ValidationErrorCodes.INVALID_SSO_TOKEN);
		}
	}

	private async getOrCreateJwks(jwksUrl: string): Promise<RemoteJwkSetResolver> {
		await this.validatePublicOutboundUrl(jwksUrl, 'jwks_url');
		const now = Date.now();
		const cached = this.jwksCache.get(jwksUrl);
		if (cached && now - cached.cachedAt < SsoService.JWKS_CACHE_TTL_MS) {
			return cached.jwks;
		}
		const fetchJwks = async (): Promise<JSONWebKeySet> => {
			const response = await FetchUtils.sendRequest({
				url: jwksUrl,
				method: 'GET',
				headers: {Accept: 'application/json'},
				timeout: ms('5 seconds'),
				serviceName: 'sso_jwks',
			});
			if (response.status < 200 || response.status >= 300) {
				throw new Error(`Failed to fetch JWKS: HTTP ${response.status}`);
			}
			const rawBody = await FetchUtils.streamToStringWithLimit(response.stream, {
				maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoJwksBytes,
				headers: response.headers,
				url: jwksUrl,
				description: 'SSO JWKS response',
			});
			const parsed = parseJsonWithGuard(rawBody, isJsonWebKeySet);
			if (!parsed) {
				throw new Error('SSO JWKS response was not a JSON Web Key Set');
			}
			return parsed;
		};
		let currentJwks = await fetchJwks();
		let currentResolver = createLocalJWKSet(currentJwks);
		let fetchedAt = now;
		let pendingReload: Promise<void> | null = null;
		let jwks!: RemoteJwkSetResolver;
		const reload = async (): Promise<void> => {
			pendingReload ??= (async () => {
				currentJwks = await fetchJwks();
				currentResolver = createLocalJWKSet(currentJwks);
				fetchedAt = Date.now();
				this.jwksCache.set(jwksUrl, {jwks, cachedAt: fetchedAt});
			})().finally(() => {
				pendingReload = null;
			});
			await pendingReload;
		};
		jwks = (async (protectedHeader?: JWSHeaderParameters, token?: FlattenedJWSInput) => {
			if (Date.now() - fetchedAt >= SsoService.JWKS_CACHE_TTL_MS) {
				await reload();
			}
			try {
				return await currentResolver(protectedHeader, token);
			} catch (error) {
				if (error instanceof JoseErrors.JWKSNoMatchingKey) {
					await reload();
					return currentResolver(protectedHeader, token);
				}
				throw error;
			}
		}) as RemoteJwkSetResolver;
		Object.defineProperties(jwks, {
			coolingDown: {
				get: () => false,
				enumerable: true,
				configurable: false,
			},
			fresh: {
				get: () => Date.now() - fetchedAt < SsoService.JWKS_CACHE_TTL_MS,
				enumerable: true,
				configurable: false,
			},
			reloading: {
				get: () => pendingReload != null,
				enumerable: true,
				configurable: false,
			},
			reload: {
				value: reload,
				enumerable: true,
				configurable: false,
				writable: false,
			},
			jwks: {
				value: () => currentJwks,
				enumerable: true,
				configurable: false,
				writable: false,
			},
		});
		this.jwksCache.set(jwksUrl, {jwks, cachedAt: now});
		if (this.jwksCache.size > 10) {
			for (const [url, entry] of this.jwksCache.entries()) {
				if (now - entry.cachedAt >= SsoService.JWKS_CACHE_TTL_MS) {
					this.jwksCache.delete(url);
				}
			}
		}
		return jwks;
	}

	private async fetchUserInfo(userInfoUrl: string, accessToken: string): Promise<Record<string, unknown>> {
		const resp = await FetchUtils.sendRequest({
			url: userInfoUrl,
			method: 'GET',
			headers: {
				Authorization: `Bearer ${accessToken}`,
				Accept: 'application/json',
			},
			timeout: ms('15 seconds'),
			serviceName: 'sso_user_info',
		});
		if (resp.status < 200 || resp.status >= 300) {
			throw InputValidationError.fromCode('access_token', ValidationErrorCodes.FAILED_TO_FETCH_SSO_USER_INFO);
		}
		try {
			const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
				maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoUserInfoBytes,
				headers: resp.headers,
				url: userInfoUrl,
				description: 'SSO user info response',
			});
			const parsed = parseJsonRecord(rawBody);
			if (!parsed) {
				throw new Error('SSO user info response was not a JSON object');
			}
			return parsed;
		} catch {
			throw InputValidationError.fromCode('access_token', ValidationErrorCodes.FAILED_TO_PARSE_SSO_USER_INFO);
		}
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
	}): Promise<{
		id_token?: string;
		access_token?: string;
	}> {
		if (config.isTestProvider) {
			return {id_token: code};
		}
		const body = new URLSearchParams({
			grant_type: 'authorization_code',
			code,
			redirect_uri: redirectUri,
			client_id: config.clientId ?? '',
			code_verifier: codeVerifier,
		});
		if (config.clientSecret) {
			body.set('client_secret', config.clientSecret);
		}
		const headers: Record<string, string> = {
			Accept: 'application/json',
			'Content-Type': 'application/x-www-form-urlencoded',
		};
		const resp = await FetchUtils.sendRequest({
			url: config.tokenUrl ?? '',
			method: 'POST',
			headers,
			body,
			timeout: ms('15 seconds'),
			serviceName: 'sso_token_exchange',
		});
		if (resp.status < 200 || resp.status >= 300) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_SSO_AUTHORIZATION_CODE);
		}
		const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoTokenBytes,
			headers: resp.headers,
			url: config.tokenUrl ?? '',
			description: 'SSO token response',
		});
		return parseTokenEndpointResponse(resp.headers.get('content-type'), rawBody);
	}

	private async getResolvedConfig(): Promise<ResolvedSsoConfig> {
		const {config} = this.apiContext.services;
		const stored = await this.instanceConfigRepository.getSsoConfig({includeSecret: true});
		const redirectUri = deriveSsoRedirectUri(config.endpoints.webApp);
		const scope = stored.scope?.trim() || 'openid email profile';
		const providerId = stored.issuer || stored.authorizationUrl || 'sso';
		let authorizationUrl = stored.authorizationUrl;
		let tokenUrl = stored.tokenUrl;
		let userInfoUrl = stored.userInfoUrl;
		let jwksUrl = stored.jwksUrl;
		let issuerForVerification = stored.issuer;
		const isTestProvider = isTestSsoProvider(
			{
				authorizationUrl,
				tokenUrl,
			},
			config.dev.testModeEnabled,
		);
		const validatedIssuer =
			stored.issuer && !isTestProvider
				? await this.validateOptionalPublicOutboundUrl(stored.issuer, 'issuer')
				: stored.issuer;
		if (validatedIssuer) {
			const cacheKey = buildDiscoveryCacheKey(validatedIssuer);
			const {cache} = this.apiContext.services;
			let discovered = await cache.get<{
				issuer: string;
				authorization_endpoint?: string;
				token_endpoint?: string;
				userinfo_endpoint?: string;
				jwks_uri?: string;
			}>(cacheKey);
			if (!discovered && (!authorizationUrl || !tokenUrl || !jwksUrl || !userInfoUrl)) {
				discovered = await tryDiscoverOidcProviderMetadata(validatedIssuer);
				if (discovered) {
					await cache.set(cacheKey, discovered, SsoService.DISCOVERY_TTL_SECONDS);
				}
			}
			authorizationUrl = authorizationUrl ?? discovered?.authorization_endpoint ?? null;
			tokenUrl = tokenUrl ?? discovered?.token_endpoint ?? null;
			userInfoUrl = userInfoUrl ?? discovered?.userinfo_endpoint ?? null;
			jwksUrl = jwksUrl ?? discovered?.jwks_uri ?? null;
			issuerForVerification = discovered?.issuer ?? validatedIssuer;
		}
		if (!isTestProvider) {
			authorizationUrl = await this.validateOptionalPublicOutboundUrl(authorizationUrl, 'authorization_url');
			tokenUrl = await this.validateOptionalPublicOutboundUrl(tokenUrl, 'token_url');
			userInfoUrl = await this.validateOptionalPublicOutboundUrl(userInfoUrl, 'user_info_url');
			jwksUrl = await this.validateOptionalPublicOutboundUrl(jwksUrl, 'jwks_url');
		}
		const canResolveClaims = isTestProvider || Boolean(jwksUrl) || Boolean(userInfoUrl);
		const ready =
			stored.enabled && Boolean(authorizationUrl) && Boolean(tokenUrl) && Boolean(stored.clientId) && canResolveClaims;
		return {
			...stored,
			authorizationUrl,
			tokenUrl,
			userInfoUrl,
			jwksUrl,
			redirectUri,
			scope,
			ready,
			providerId,
			isTestProvider,
			issuerForVerification,
		};
	}

	private async validatePublicOutboundUrl(rawUrl: string, fieldName: string): Promise<URL> {
		return validateSsoPublicOutboundUrl(rawUrl, fieldName);
	}

	private async validateOptionalPublicOutboundUrl(rawUrl: string | null, fieldName: string): Promise<string | null> {
		if (!rawUrl) {
			return null;
		}
		try {
			const validUrl = await this.validatePublicOutboundUrl(rawUrl, fieldName);
			return validUrl.toString();
		} catch (error) {
			this.logger.warn({fieldName, rawUrl, error}, 'Ignoring SSO URL that failed outbound policy validation');
			return null;
		}
	}

	private async requireReadyConfig(): Promise<ResolvedSsoConfig> {
		const config = await this.getResolvedConfig();
		if (!config.ready) {
			throw new FeatureTemporarilyDisabledError();
		}
		return config;
	}
}
