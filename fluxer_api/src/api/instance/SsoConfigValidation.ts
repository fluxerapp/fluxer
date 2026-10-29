// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';
import {tryDiscoverOidcProviderMetadata} from '@app/api/auth/services/SsoUtils';
import {Config} from '@app/api/Config';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {RequestUrlPolicy} from '@pkgs/http_client/src/HttpClientTypes';
import {HttpError} from '@pkgs/http_client/src/HttpError';
import {createPublicInternetRequestUrlPolicy} from '@pkgs/http_client/src/PublicInternetRequestUrlPolicy';

interface SsoConfigValidationInput {
	enabled: boolean;
	enforced: boolean;
	issuer: string | null;
	authorizationUrl: string | null;
	tokenUrl: string | null;
	userInfoUrl: string | null;
	jwksUrl: string | null;
	clientId: string | null;
	allowedEmailDomains: Array<string>;
}

interface NormalizedSsoConfigValidationResult extends SsoConfigValidationInput {
	ready: boolean;
	providerUnreachable: boolean;
}

let ssoRequestUrlPolicy: RequestUrlPolicy | null = null;

export function resetSsoRequestUrlPolicyForTesting(): void {
	ssoRequestUrlPolicy = null;
}

export function getSsoRequestUrlPolicy(): RequestUrlPolicy {
	if (ssoRequestUrlPolicy === null) {
		ssoRequestUrlPolicy = createPublicInternetRequestUrlPolicy({
			allowPrivateAddresses: Config.auth.ssoAllowPrivateAddresses,
		});
	}
	return ssoRequestUrlPolicy;
}
const DOMAIN_LABEL_REGEX = /^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/;
const SsoAllowedDomainsSchema = InstanceConfigResponse.shape.sso.shape.allowed_domains;
const TrustedDomainsSchema = InstanceConfigResponse.shape.app_public.shape.branding.shape.trusted_domains;

function normalizeOptionalSsoString(value: string | null): string | null {
	if (typeof value !== 'string') {
		return null;
	}
	const trimmed = value.trim();
	return trimmed.length > 0 ? trimmed : null;
}

const SSO_BUTTON_LABEL_FORBIDDEN_REGEX = /[\p{Cc}\u2028\u2029\u202A-\u202E\u2066-\u2069]/u;
const SSO_BUTTON_LABEL_INVISIBLE_REGEX = /[\p{Default_Ignorable_Code_Point}\u2800\s]/gu;

export function normalizeSsoButtonLabel(value: string | null): string | null {
	const normalized = normalizeOptionalSsoString(value);
	if (normalized === null) {
		return null;
	}
	if (
		SSO_BUTTON_LABEL_FORBIDDEN_REGEX.test(normalized) ||
		normalized.replace(SSO_BUTTON_LABEL_INVISIBLE_REGEX, '').length === 0
	) {
		throw InputValidationError.fromCode('button_label', ValidationErrorCodes.INVALID_FORMAT);
	}
	return normalized;
}

function throwInvalidDomain(field: string, domain: string): never {
	throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_ALLOWED_DOMAIN, {domain});
}

export function deriveSsoRedirectUri(webAppEndpoint: string): string {
	return `${webAppEndpoint.replace(/\/+$/, '')}/auth/sso/callback`;
}

export function isTestSsoProvider(
	config: {
		authorizationUrl: string | null;
		tokenUrl: string | null;
	},
	testModeEnabled: boolean,
): boolean {
	if (!testModeEnabled) {
		return false;
	}
	return (
		config.authorizationUrl === 'test' ||
		config.tokenUrl === 'test' ||
		(config.authorizationUrl?.startsWith('test-') ?? false)
	);
}

export function normalizeAllowedEmailDomains(domains: unknown, field = 'allowed_domains'): Array<string> {
	return normalizeDomainList(SsoAllowedDomainsSchema, domains, field, false);
}

export function normalizeTrustedDomains(domains: unknown, field: string): Array<string> {
	return normalizeDomainList(TrustedDomainsSchema, domains, field, true);
}

function normalizeDomainList(
	schema: typeof SsoAllowedDomainsSchema,
	domains: unknown,
	field: string,
	allowWildcard: boolean,
): Array<string> {
	const result = schema.safeParse(domains);
	if (!result.success) throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_FORMAT);
	const normalized = new Set<string>();
	for (const rawDomain of result.data) {
		const entry = rawDomain.trim();
		if (!entry) {
			continue;
		}
		const lowered = entry.toLowerCase();
		const wildcard = allowWildcard && lowered.startsWith('*.');
		const host = wildcard ? lowered.slice(2) : lowered;
		if (
			host.includes('@') ||
			host.includes('/') ||
			host.includes('\\') ||
			host.includes(':') ||
			host.startsWith('.') ||
			host.endsWith('.') ||
			host.includes('*')
		) {
			throwInvalidDomain(field, entry);
		}
		const asciiDomain = domainToASCII(host);
		if (!asciiDomain || asciiDomain.length > 253 || !asciiDomain.includes('.')) {
			throwInvalidDomain(field, entry);
		}
		const labels = asciiDomain.split('.');
		if (labels.some((label) => !DOMAIN_LABEL_REGEX.test(label)) || /^\d+$/.test(labels[labels.length - 1] ?? '')) {
			throwInvalidDomain(field, entry);
		}
		normalized.add(wildcard ? `*.${asciiDomain}` : asciiDomain);
	}
	return Array.from(normalized);
}

export function isHttpsSsoUrl(rawUrl: string): boolean {
	const url = URL.parse(rawUrl);
	return url?.protocol === 'https:' && !url.username && !url.password && !url.hash;
}

async function validateSsoPublicOutboundUrl(rawUrl: string, fieldName: string): Promise<string> {
	let parsedUrl: URL;
	try {
		parsedUrl = new URL(rawUrl);
	} catch {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.INVALID_URL_FORMAT);
	}
	if (parsedUrl.protocol !== 'https:') {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.URL_MUST_USE_HTTPS);
	}
	if (parsedUrl.username || parsedUrl.password || parsedUrl.hash) {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.INVALID_URL_FORMAT);
	}
	try {
		await getSsoRequestUrlPolicy().validate(parsedUrl, {
			phase: 'initial',
			redirectCount: 0,
		});
	} catch (error) {
		const unresolvable = error instanceof HttpError && error.errorType === 'unresolvable_host';
		throw InputValidationError.fromCode(
			fieldName,
			unresolvable ? ValidationErrorCodes.URL_HOSTNAME_NOT_RESOLVABLE : ValidationErrorCodes.URL_NOT_PUBLICLY_ROUTABLE,
		);
	}
	// Return the caller's exact input rather than parsedUrl.toString(), which would normalize
	// the URL (e.g. appending a trailing slash) and break exact-match comparisons such as the
	// OIDC issuer check in jwtVerify. Invalid URLs have already thrown above.
	return rawUrl;
}

async function normalizeOptionalSsoUrl(
	rawUrl: string | null,
	fieldName: string,
	skipValidation: boolean,
): Promise<string | null> {
	const normalized = normalizeOptionalSsoString(rawUrl);
	if (!normalized || skipValidation) {
		return normalized;
	}
	return validateSsoPublicOutboundUrl(normalized, fieldName);
}

function resolveSsoReadiness(config: SsoConfigValidationInput, isTestProvider: boolean): boolean {
	if (!config.enabled) {
		return false;
	}
	return Boolean(config.clientId) && (isTestProvider || Boolean(config.issuer));
}

async function providerUnreachable(config: SsoConfigValidationInput): Promise<boolean> {
	const reliesOnIssuer = !config.authorizationUrl || !config.tokenUrl || (!config.jwksUrl && !config.userInfoUrl);
	if (!config.issuer || !reliesOnIssuer) {
		return false;
	}
	const discovered = await tryDiscoverOidcProviderMetadata(config.issuer);
	const found = (explicit: string | null, ...discoveredUrls: Array<string | undefined>) =>
		Boolean(explicit) || discoveredUrls.some((url) => url !== undefined && isHttpsSsoUrl(url));
	return !(
		found(config.authorizationUrl, discovered?.authorization_endpoint) &&
		found(config.tokenUrl, discovered?.token_endpoint) &&
		found(config.jwksUrl ?? config.userInfoUrl, discovered?.jwks_uri, discovered?.userinfo_endpoint)
	);
}

export async function normalizeAndValidateSsoConfig(
	config: SsoConfigValidationInput,
	options: {testModeEnabled: boolean},
): Promise<NormalizedSsoConfigValidationResult> {
	const baseConfig = {
		...config,
		issuer: normalizeOptionalSsoString(config.issuer),
		authorizationUrl: normalizeOptionalSsoString(config.authorizationUrl),
		tokenUrl: normalizeOptionalSsoString(config.tokenUrl),
		userInfoUrl: normalizeOptionalSsoString(config.userInfoUrl),
		jwksUrl: normalizeOptionalSsoString(config.jwksUrl),
		clientId: normalizeOptionalSsoString(config.clientId),
		allowedEmailDomains: normalizeAllowedEmailDomains(config.allowedEmailDomains),
	};
	const isTestProvider = isTestSsoProvider(baseConfig, options.testModeEnabled);
	const skipUrlValidation = isTestProvider || !baseConfig.enabled;
	const normalized: SsoConfigValidationInput = {
		...baseConfig,
		issuer: await normalizeOptionalSsoUrl(baseConfig.issuer, 'issuer', skipUrlValidation),
		authorizationUrl: await normalizeOptionalSsoUrl(
			baseConfig.authorizationUrl,
			'authorization_url',
			skipUrlValidation,
		),
		tokenUrl: await normalizeOptionalSsoUrl(baseConfig.tokenUrl, 'token_url', skipUrlValidation),
		userInfoUrl: await normalizeOptionalSsoUrl(baseConfig.userInfoUrl, 'userinfo_url', skipUrlValidation),
		jwksUrl: await normalizeOptionalSsoUrl(baseConfig.jwksUrl, 'jwks_url', skipUrlValidation),
	};
	if (normalized.enabled && !isTestProvider && !normalized.issuer) {
		throw InputValidationError.fromCode('issuer', ValidationErrorCodes.SSO_ISSUER_REQUIRED);
	}
	const ready = resolveSsoReadiness(normalized, isTestProvider);
	if (normalized.enabled && normalized.enforced && !ready) {
		throw InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_MISCONFIGURED);
	}
	const unreachable = ready && !isTestProvider && (await providerUnreachable(normalized));
	if (unreachable && normalized.enforced) {
		throw InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_DISCOVERY_FAILED);
	}
	return {...normalized, ready, providerUnreachable: unreachable};
}
