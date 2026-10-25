// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';
import {Config} from '@app/api/Config';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {RequestUrlPolicy} from '@pkgs/http_client/src/HttpClientTypes';
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

export async function validateSsoPublicOutboundUrl(rawUrl: string, fieldName: string): Promise<string> {
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
	} catch {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.URL_NOT_PUBLICLY_ROUTABLE);
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
	if (isTestProvider) {
		return Boolean(config.clientId);
	}
	const canUseIssuerDiscovery = Boolean(config.issuer);
	const hasAuthorizationEndpoint = Boolean(config.authorizationUrl) || canUseIssuerDiscovery;
	const hasTokenEndpoint = Boolean(config.tokenUrl) || canUseIssuerDiscovery;
	const canResolveClaims = Boolean(config.jwksUrl || config.userInfoUrl || canUseIssuerDiscovery);
	return Boolean(hasAuthorizationEndpoint && hasTokenEndpoint && config.clientId && canResolveClaims);
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
	const ready = resolveSsoReadiness(normalized, isTestProvider);
	if (normalized.enabled && normalized.enforced && !ready) {
		throw InputValidationError.fromCode('sso', ValidationErrorCodes.SSO_MISCONFIGURED);
	}
	return {...normalized, ready};
}
