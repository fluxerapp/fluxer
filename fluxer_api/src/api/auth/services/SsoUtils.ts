// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {formatUrlForDiagnostics} from '@pkgs/http_client/src/HttpClientDiagnostics';
import {ms} from 'itty-time';

interface DiscoveredOidcProviderMetadata {
	issuer: string;
	authorization_endpoint?: string;
	token_endpoint?: string;
	userinfo_endpoint?: string;
	jwks_uri?: string;
}

export function sanitizeSsoRedirectTo(redirectTo?: string): string | undefined {
	if (!redirectTo) return undefined;
	const trimmed = redirectTo.trim();
	if (!trimmed) return undefined;
	if (!trimmed.startsWith('/')) return undefined;
	if (trimmed.startsWith('//')) return undefined;
	if (trimmed.length > 2048) return undefined;
	if (trimmed.includes('\r') || trimmed.includes('\n')) return undefined;
	return trimmed;
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
	if (contentType.includes('application/json') || contentType.includes('application/jwt')) {
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

export async function tryDiscoverOidcProviderMetadata(issuer: string): Promise<DiscoveredOidcProviderMetadata | null> {
	try {
		const url = buildDiscoveryUrl(issuer);
		const resp = await FetchUtils.sendRequest({
			url: url.toString(),
			method: 'GET',
			headers: {Accept: 'application/json'},
			timeout: ms('10 seconds'),
			serviceName: 'sso_oidc_discovery',
		});
		if (resp.status < 200 || resp.status >= 300) {
			FetchUtils.discardResponseBody(resp.stream, resp.status);
			return null;
		}
		const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoDiscoveryBytes,
			headers: resp.headers,
			url: url.toString(),
			description: 'OIDC discovery response',
		});
		const json = parseJsonRecord(rawBody);
		if (!json) return null;
		const discoveredIssuer = typeof json['issuer'] === 'string' ? json['issuer'] : null;
		if (!discoveredIssuer) return null;
		if (normalizeIssuerForCompare(discoveredIssuer) !== normalizeIssuerForCompare(issuer)) {
			return null;
		}
		return {
			issuer: discoveredIssuer,
			authorization_endpoint:
				typeof json['authorization_endpoint'] === 'string' ? json['authorization_endpoint'] : undefined,
			token_endpoint: typeof json['token_endpoint'] === 'string' ? json['token_endpoint'] : undefined,
			userinfo_endpoint: typeof json['userinfo_endpoint'] === 'string' ? json['userinfo_endpoint'] : undefined,
			jwks_uri: typeof json['jwks_uri'] === 'string' ? json['jwks_uri'] : undefined,
		};
	} catch (error) {
		Logger.debug({issuer: formatUrlForDiagnostics(issuer), error}, 'Failed to discover OIDC provider metadata');
		return null;
	}
}
