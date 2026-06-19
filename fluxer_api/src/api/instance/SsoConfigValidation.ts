// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
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

const SSO_REQUEST_URL_POLICY = createPublicInternetRequestUrlPolicy();
const DOMAIN_LABEL_REGEX = /^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$/;

function normalizeOptionalSsoString(value: string | null): string | null {
	if (typeof value !== 'string') {
		return null;
	}
	const trimmed = value.trim();
	return trimmed.length > 0 ? trimmed : null;
}

function throwInvalidDomain(): never {
	throw InputValidationError.fromCode('allowed_domains', ValidationErrorCodes.INVALID_FORMAT);
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
	return (
		config.authorizationUrl === 'test' ||
		config.tokenUrl === 'test' ||
		(testModeEnabled && (config.authorizationUrl?.startsWith('test-') ?? false))
	);
}

export function normalizeSsoAllowedEmailDomains(domains: Array<string>): Array<string> {
	const normalized = new Set<string>();
	for (const rawDomain of domains) {
		const trimmed = rawDomain.trim().toLowerCase();
		if (!trimmed) {
			continue;
		}
		if (
			trimmed.includes('@') ||
			trimmed.includes('/') ||
			trimmed.includes('\\') ||
			trimmed.includes(':') ||
			trimmed.startsWith('.') ||
			trimmed.endsWith('.') ||
			trimmed.startsWith('*.') ||
			trimmed.includes('*')
		) {
			throwInvalidDomain();
		}
		const asciiDomain = domainToASCII(trimmed);
		if (!asciiDomain || asciiDomain.length > 253 || !asciiDomain.includes('.')) {
			throwInvalidDomain();
		}
		const labels = asciiDomain.split('.');
		if (labels.some((label) => !DOMAIN_LABEL_REGEX.test(label)) || /^\d+$/.test(labels[labels.length - 1] ?? '')) {
			throwInvalidDomain();
		}
		normalized.add(asciiDomain);
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
	if (parsedUrl.protocol !== 'https:' || parsedUrl.username || parsedUrl.password || parsedUrl.hash) {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.INVALID_URL_FORMAT);
	}
	try {
		await SSO_REQUEST_URL_POLICY.validate(parsedUrl, {
			phase: 'initial',
			redirectCount: 0,
		});
	} catch {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.INVALID_URL_FORMAT);
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
		allowedEmailDomains: normalizeSsoAllowedEmailDomains(config.allowedEmailDomains),
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
