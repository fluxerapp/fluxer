// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {CompactSign, exportJWK, generateKeyPair, type JWK} from 'jose';
import {HttpResponse, http} from 'msw';

export const ADMIN_SSO_ISSUER = 'https://1.1.1.1/admin-sso';
const ADMIN_SSO_CLIENT_ID = 'admin-sso-test-client';
export const ADMIN_SSO_KID = 'admin-sso-key';

export const ADMIN_CLIENT_BASIC_AUTH = `Basic ${Buffer.from(
	`${ADMIN_OAUTH2_APPLICATION_ID}:test-admin-oauth-secret`,
).toString('base64')}`;

interface SignOptions {
	key?: CryptoKey;
	alg?: string;
	kid?: string;
}

export interface AdminSsoProvider {
	privateKey: CryptoKey;
	jwksRequests: () => number;
	rotate: () => Promise<CryptoKey>;
	signRawClaims: (json: string, options?: SignOptions) => Promise<string>;
}

export async function useAdminSsoProvider(): Promise<AdminSsoProvider> {
	let {publicKey, privateKey} = await generateKeyPair('RS256');
	let keys: Array<JWK> = [{...(await exportJWK(publicKey)), kid: ADMIN_SSO_KID, alg: 'RS256', use: 'sig'}];
	let jwksRequests = 0;
	server.use(
		http.get(`${ADMIN_SSO_ISSUER}/.well-known/openid-configuration`, () =>
			HttpResponse.json({
				issuer: ADMIN_SSO_ISSUER,
				authorization_endpoint: `${ADMIN_SSO_ISSUER}/authorize`,
				token_endpoint: `${ADMIN_SSO_ISSUER}/token`,
				jwks_uri: `${ADMIN_SSO_ISSUER}/jwks.json`,
				id_token_signing_alg_values_supported: ['RS256'],
				request_object_signing_alg_values_supported: ['none'],
			}),
		),
		http.get(`${ADMIN_SSO_ISSUER}/jwks.json`, () => {
			jwksRequests += 1;
			return HttpResponse.json({keys});
		}),
	);
	const provider: AdminSsoProvider = {
		privateKey,
		jwksRequests: () => jwksRequests,
		async rotate() {
			({publicKey, privateKey} = await generateKeyPair('RS256'));
			keys = [{...(await exportJWK(publicKey)), kid: `${ADMIN_SSO_KID}-rotated`, alg: 'RS256', use: 'sig'}];
			provider.privateKey = privateKey;
			return privateKey;
		},
		async signRawClaims(json, options = {}) {
			return new CompactSign(new TextEncoder().encode(json))
				.setProtectedHeader({alg: options.alg ?? 'RS256', kid: options.kid ?? ADMIN_SSO_KID, typ: 'JWT'})
				.sign(options.key ?? provider.privateKey);
		},
	};
	return provider;
}

export function idTokenClaimsJson(fluxerId: string, overrides: Record<string, unknown> = {}): string {
	const now = Math.floor(Date.now() / 1000);
	const claims = {
		iss: ADMIN_SSO_ISSUER,
		aud: ADMIN_SSO_CLIENT_ID,
		sub: `subject-${fluxerId}`,
		iat: now,
		exp: now + 300,
		nonce: 'admin-sso-test-nonce-0001',
		type: 'id-token',
		name: 'Test Admin',
		...overrides,
	};
	const json = JSON.stringify({...claims, fluxer_id: '__FLUXER_ID__'});
	return json.replace('"__FLUXER_ID__"', fluxerId);
}

export const ADMIN_SSO_NONCE = 'admin-sso-test-nonce-0001';

export function adminSsoSignInRequest(
	harness: ApiTestHarness,
	args: {idToken: string; nonce?: string; auth?: string},
): Promise<Response> {
	return harness.requestJson({
		path: '/admin/sso/sign-in',
		method: 'POST',
		headers: {Authorization: args.auth ?? ADMIN_CLIENT_BASIC_AUTH},
		body: {id_token: args.idToken, nonce: args.nonce ?? ADMIN_SSO_NONCE},
	});
}
