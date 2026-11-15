// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, timingSafeEqual} from 'node:crypto';
import {recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {getOidcJwks} from '@app/api/auth/services/OidcJwks';
import {acceptedIssuers, tryDiscoverOidcProviderMetadata} from '@app/api/auth/services/SsoUtils';
import {createApplicationID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {isHttpsSsoUrl} from '@app/api/instance/SsoConfigValidation';
import {Logger} from '@app/api/Logger';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {isAccountClosed, isAccountDisabled, isTemporarilyBanned} from '@app/api/user/UserHelpers';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {SsoProviderUnavailableError} from '@fluxer/errors/src/domains/auth/SsoProviderUnavailableError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {InvalidClientError} from '@fluxer/errors/src/domains/oauth/InvalidClientError';
import type {AdminSsoSignInResponse} from '@fluxer/schema/src/domains/admin/AdminSsoSchemas';
import type {Context} from 'hono';
import {ms} from 'itty-time';
import {errors as JoseErrors, jwtVerify} from 'jose';

interface AdminSsoConfig {
	issuer: string;
	clientId: string;
}

interface AdminSsoProvider {
	jwksUri: string;
	algorithms: Array<string>;
}

interface DiscoveryCacheEntry {
	provider: AdminSsoProvider | null;
	fetchedAt: number;
}

const ALLOWED_ID_TOKEN_ALGS = new Set([
	'RS256',
	'RS384',
	'RS512',
	'PS256',
	'PS384',
	'PS512',
	'ES256',
	'ES384',
	'ES512',
	'EdDSA',
]);
const DISCOVERY_FRESH_MS = ms('1 hour');
const DISCOVERY_FAILURE_FRESH_MS = ms('1 minute');
const CLOCK_TOLERANCE_SECONDS = 10;
const ID_TOKEN_TYPE = 'id-token';
const FLUXER_ID_CLAIM = 'fluxer_id';
const FLUXER_ID_PATTERN = /^[1-9][0-9]{0,19}$/;
const MAX_SNOWFLAKE = 0xffffffffffffffffn;
const ADMIN_APPLICATION_ID = createApplicationID(ADMIN_OAUTH2_APPLICATION_ID);

const discoveryCache = new Map<string, DiscoveryCacheEntry>();
const discoveryRefreshes = new Map<string, Promise<AdminSsoProvider | null>>();

export function resetAdminSsoCachesForTests(): void {
	discoveryCache.clear();
	discoveryRefreshes.clear();
}

class AdminSsoTokenRejected extends Error {}

function adminSsoConfig(): AdminSsoConfig | null {
	const {issuer, clientId} = Config.admin.sso;
	if (!issuer || !clientId) return null;
	if (!isHttpsSsoUrl(issuer) || issuer.includes('?')) {
		throw misconfigured('issuer must be an https URL without credentials, query or fragment');
	}
	return {issuer, clientId};
}

function misconfigured(reason: string): InputValidationError {
	Logger.error({reason}, 'Admin SSO provider is misconfigured');
	return InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_MISCONFIGURED);
}

async function discoverProvider(issuer: string): Promise<AdminSsoProvider | null> {
	const metadata = await tryDiscoverOidcProviderMetadata(issuer);
	if (!metadata) return null;
	if (!metadata.jwks_uri || !isHttpsSsoUrl(metadata.jwks_uri)) {
		throw misconfigured('discovery has no https jwks_uri');
	}
	const algorithms = (metadata.id_token_signing_alg_values_supported ?? []).filter((alg) =>
		ALLOWED_ID_TOKEN_ALGS.has(alg),
	);
	if (algorithms.length === 0) {
		throw misconfigured('discovery advertises no supported ID token signing algorithm');
	}
	return {jwksUri: metadata.jwks_uri, algorithms};
}

async function loadProvider(issuer: string): Promise<AdminSsoProvider> {
	const cached = discoveryCache.get(issuer);
	const freshFor = cached?.provider ? DISCOVERY_FRESH_MS : DISCOVERY_FAILURE_FRESH_MS;
	let provider = cached && Date.now() - cached.fetchedAt < freshFor ? cached.provider : undefined;
	if (provider === undefined) {
		let refresh = discoveryRefreshes.get(issuer);
		if (!refresh) {
			refresh = discoverProvider(issuer)
				.then((discovered) => {
					discoveryCache.set(issuer, {provider: discovered, fetchedAt: Date.now()});
					return discovered;
				})
				.finally(() => discoveryRefreshes.delete(issuer));
			discoveryRefreshes.set(issuer, refresh);
		}
		provider = await refresh;
	}
	if (!provider) {
		throw new SsoProviderUnavailableError();
	}
	return provider;
}

function nonceMatches(actual: unknown, expected: string): boolean {
	if (typeof actual !== 'string') return false;
	const actualBytes = Buffer.from(actual, 'utf8');
	const expectedBytes = Buffer.from(expected, 'utf8');
	return actualBytes.length === expectedBytes.length && timingSafeEqual(actualBytes, expectedBytes);
}

function readFluxerIdClaim(idToken: string): UserID {
	const segment = idToken.split('.')[1] ?? '';
	const text = Buffer.from(segment, 'base64url').toString('utf8');
	const parsed: unknown = JSON.parse(text, (key: string, value: unknown, context?: {source?: string}) =>
		key === FLUXER_ID_CLAIM && typeof value === 'number' ? context?.source : value,
	);
	const raw =
		typeof parsed === 'object' && parsed !== null ? (parsed as Record<string, unknown>)[FLUXER_ID_CLAIM] : undefined;
	if (typeof raw !== 'string' || !FLUXER_ID_PATTERN.test(raw) || BigInt(raw) > MAX_SNOWFLAKE) {
		throw new AdminSsoTokenRejected('fluxer_id claim is not a snowflake');
	}
	return createUserID(BigInt(raw));
}

async function verifyAdminSsoIdToken(
	config: AdminSsoConfig,
	idToken: string,
	expectedNonce: string,
): Promise<{userId: UserID; expiresAt: number}> {
	try {
		const provider = await loadProvider(config.issuer);
		const {payload} = await jwtVerify(idToken, await getOidcJwks(provider.jwksUri), {
			issuer: acceptedIssuers(config.issuer),
			audience: config.clientId,
			algorithms: provider.algorithms,
			clockTolerance: CLOCK_TOLERANCE_SECONDS,
			maxTokenAge: '10 minutes',
			requiredClaims: ['iat', 'exp', 'nonce'],
		});
		const authorizedParty = payload['azp'];
		const requiresAuthorizedParty = Array.isArray(payload.aud) && payload.aud.length > 1;
		if ((authorizedParty !== undefined || requiresAuthorizedParty) && authorizedParty !== config.clientId) {
			throw new AdminSsoTokenRejected('azp does not match the client ID');
		}
		if (!nonceMatches(payload['nonce'], expectedNonce)) {
			throw new AdminSsoTokenRejected('nonce mismatch');
		}
		if (payload['type'] !== undefined && payload['type'] !== ID_TOKEN_TYPE) {
			throw new AdminSsoTokenRejected('token is not an ID token');
		}
		return {userId: readFluxerIdClaim(idToken), expiresAt: payload.exp ?? 0};
	} catch (error) {
		if (error instanceof SsoProviderUnavailableError || error instanceof InputValidationError) throw error;
		if (error instanceof AdminSsoTokenRejected) {
			Logger.warn({reason: error.message}, 'Admin SSO ID token refused');
		} else if (error instanceof JoseErrors.JOSEError) {
			Logger.warn(
				{code: error.code, claim: error instanceof JoseErrors.JWTClaimValidationFailed ? error.claim : undefined},
				'Admin SSO ID token refused',
			);
		} else {
			Logger.warn({reason: error instanceof Error ? error.name : 'unknown'}, 'Admin SSO ID token refused');
		}
		throw InputValidationError.fromCode('id_token', ValidationErrorCodes.INVALID_SSO_TOKEN);
	}
}

async function claimNonceOnce(ctx: Context<HonoEnv>, nonce: string, expiresAt: number): Promise<void> {
	const digest = createHash('sha256').update(nonce).digest('hex');
	const ttl = Math.max(1, expiresAt - Math.floor(Date.now() / 1000) + CLOCK_TOLERANCE_SECONDS);
	const lock = await ctx.get('apiContext').services.cache.acquireLock(`admin-sso:nonce:${digest}`, ttl);
	if (lock === null) {
		Logger.warn({reason: 'ID token already used'}, 'Admin SSO ID token refused');
		throw InputValidationError.fromCode('id_token', ValidationErrorCodes.INVALID_SSO_TOKEN);
	}
}

function refuse(userId: UserID, reason: string): never {
	Logger.warn({user_id: userId.toString(), reason}, 'Admin SSO sign-in refused');
	throw new MissingPermissionsError();
}

export async function signInWithAdminSso(
	ctx: Context<HonoEnv>,
	args: {authorizationHeader?: string; idToken: string; nonce: string},
): Promise<AdminSsoSignInResponse> {
	const config = adminSsoConfig();
	if (!config) {
		throw new FeatureTemporarilyDisabledError();
	}
	const oauth2Service = ctx.get('oauth2Service');
	const application = await oauth2Service.authenticateClient({authorizationHeader: args.authorizationHeader});
	if (application.applicationId !== ADMIN_APPLICATION_ID || !application.clientSecretHash) {
		throw new InvalidClientError();
	}
	const {userId, expiresAt} = await verifyAdminSsoIdToken(config, args.idToken, args.nonce);
	await claimNonceOnce(ctx, args.nonce, expiresAt);
	const user = await ctx.get('apiContext').services.users.findUnique(userId);
	if (!user) refuse(userId, 'unknown user');
	if (user.isBot) refuse(userId, 'bot account');
	if (isAccountClosed(user) || isAccountDisabled(user) || isTemporarilyBanned(user)) {
		refuse(userId, 'account closed, disabled or banned');
	}
	if (!user.acls.has(AdminACLs.AUTHENTICATE) && !user.acls.has(AdminACLs.WILDCARD)) {
		refuse(userId, 'missing admin:authenticate');
	}
	ctx.set('adminUserId', user.id);
	await recordAdminWrite(ctx, {
		targetType: 'user',
		targetId: user.id,
		action: 'sso_sign_in',
		metadata: {provider_id: config.issuer},
	});
	const tokens = await oauth2Service.issueAdminSsoTokens(application, user.id);
	return {user_id: user.id.toString(), ...tokens};
}
