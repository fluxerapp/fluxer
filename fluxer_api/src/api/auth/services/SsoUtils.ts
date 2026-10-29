// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSsoRequestUrlPolicy} from '@app/api/instance/SsoConfigValidation';
import {Logger} from '@app/api/Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {formatUrlForDiagnostics} from '@pkgs/http_client/src/HttpClientDiagnostics';
import {ms} from 'itty-time';

export interface DiscoveredOidcProviderMetadata {
	issuer: string;
	authorization_endpoint?: string;
	token_endpoint?: string;
	userinfo_endpoint?: string;
	jwks_uri?: string;
	token_endpoint_auth_methods_supported?: Array<string>;
}

const SSO_REDIRECT_BASE = 'https://sso-redirect.invalid';
const SSO_REDIRECT_MAX_LENGTH = 2048;
const SSO_REDIRECT_FORBIDDEN_CHARACTERS = /[\\\p{Cc}]/u;

export function sanitizeSsoRedirectTo(redirectTo?: string): string | undefined {
	const trimmed = redirectTo?.trim();
	if (!trimmed || trimmed.length > SSO_REDIRECT_MAX_LENGTH || !trimmed.startsWith('/')) return undefined;
	if (SSO_REDIRECT_FORBIDDEN_CHARACTERS.test(trimmed)) return undefined;
	const url = URL.parse(trimmed, SSO_REDIRECT_BASE);
	if (url?.origin !== SSO_REDIRECT_BASE) return undefined;
	const path = `${url.pathname}${url.search}${url.hash}`;
	return path.startsWith('//') || path.startsWith('/\\') ? undefined : path;
}

export function parseTokenEndpointResponse(
	contentTypeHeader: string | null,
	rawResponseBody: string,
): {
	id_token?: string;
	access_token?: string;
} {
	const contentType = contentTypeHeader?.toLowerCase() ?? '';
	const raw = rawResponseBody;
	if (contentType.includes('application/json')) {
		const parsed = parseJsonRecord(raw);
		if (!parsed) {
			return {id_token: undefined, access_token: undefined};
		}
		return {
			id_token: typeof parsed['id_token'] === 'string' ? parsed['id_token'] : undefined,
			access_token: typeof parsed['access_token'] === 'string' ? parsed['access_token'] : undefined,
		};
	}
	const params = new URLSearchParams(raw);
	return {
		id_token: params.get('id_token') ?? undefined,
		access_token: params.get('access_token') ?? undefined,
	};
}

function buildDiscoveryUrl(issuer: string): URL {
	const issuerUrl = URL.parse(issuer);
	if (!issuerUrl) {
		throw new Error('Invalid OIDC issuer URL');
	}
	const normalized = issuerUrl.toString().replace(/\/$/, '');
	return new URL(`${normalized}/.well-known/openid-configuration`);
}

function normalizeIssuerForCompare(value: string): string {
	return value.replace(/\/$/, '');
}

export function acceptedIssuers(issuer: string): Array<string> {
	const base = normalizeIssuerForCompare(issuer);
	return [base, `${base}/`];
}

function discoveryFailed(issuer: string, reason: string, details: Record<string, unknown> = {}): null {
	Logger.warn(
		{issuer: formatUrlForDiagnostics(issuer), reason, ...details},
		'Failed to discover OIDC provider metadata',
	);
	return null;
}

function readStringList(value: unknown): Array<string> | undefined {
	return Array.isArray(value) ? value.filter((item): item is string => typeof item === 'string') : undefined;
}

export async function tryDiscoverOidcProviderMetadata(issuer: string): Promise<DiscoveredOidcProviderMetadata | null> {
	try {
		const url = buildDiscoveryUrl(issuer);
		const resp = await FetchUtils.sendRequest(
			{
				url: url.toString(),
				method: 'GET',
				headers: {Accept: 'application/json'},
				timeout: ms('10 seconds'),
				serviceName: 'sso_oidc_discovery',
			},
			{requestUrlPolicy: getSsoRequestUrlPolicy(), maxRedirects: 0},
		);
		if (resp.status < 200 || resp.status >= 300) {
			FetchUtils.discardResponseBody(resp.stream, resp.status);
			return discoveryFailed(issuer, 'unexpected HTTP status', {status: resp.status});
		}
		const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoDiscoveryBytes,
			headers: resp.headers,
			url: url.toString(),
			description: 'OIDC discovery response',
		});
		const json = parseJsonRecord(rawBody);
		if (!json) return discoveryFailed(issuer, 'response is not a JSON object');
		const discoveredIssuer = typeof json['issuer'] === 'string' ? json['issuer'] : null;
		if (!discoveredIssuer) return discoveryFailed(issuer, 'response has no issuer');
		if (normalizeIssuerForCompare(discoveredIssuer) !== normalizeIssuerForCompare(issuer)) {
			return discoveryFailed(issuer, 'response issuer does not match', {
				discovered_issuer: formatUrlForDiagnostics(discoveredIssuer),
			});
		}
		return {
			issuer: discoveredIssuer,
			authorization_endpoint:
				typeof json['authorization_endpoint'] === 'string' ? json['authorization_endpoint'] : undefined,
			token_endpoint: typeof json['token_endpoint'] === 'string' ? json['token_endpoint'] : undefined,
			userinfo_endpoint: typeof json['userinfo_endpoint'] === 'string' ? json['userinfo_endpoint'] : undefined,
			jwks_uri: typeof json['jwks_uri'] === 'string' ? json['jwks_uri'] : undefined,
			token_endpoint_auth_methods_supported: readStringList(json['token_endpoint_auth_methods_supported']),
		};
	} catch (error) {
		return discoveryFailed(issuer, 'request failed', {error});
	}
}
