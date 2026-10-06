// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {
	ApplicationResponse,
	OAuth2ConsentResponse,
	OAuth2IntrospectResponse,
	OAuth2TokenResponse,
} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';

interface OAuth2Application extends Omit<ApplicationResponse, 'bot'> {
	client_secret: string;
	bot: {
		id: string;
		token: string;
	};
}

export async function createOAuth2Application(
	harness: ApiTestHarness,
	owner: TestAccount,
	params: {
		name?: string;
		redirect_uris: Array<string>;
	},
): Promise<OAuth2Application> {
	const {response, text, json} = await createBuilder<OAuth2Application>(harness, owner.token)
		.post('/oauth2/applications')
		.body({
			name: params.name ?? `Test App ${randomUUID()}`,
			redirect_uris: params.redirect_uris,
		})
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	const payload = json;
	if (!payload.id || !payload.client_secret || !payload.bot?.id || !payload.bot?.token) {
		throw new Error('Invalid OAuth2 application response');
	}
	return payload;
}

export async function authorizeOAuth2(
	harness: ApiTestHarness,
	userToken: string,
	params: {
		client_id: string;
		redirect_uri?: string;
		scope: string;
		state?: string;
	},
): Promise<{
	code: string;
	state: string;
}> {
	const {response, text, json} = await createBuilder<OAuth2ConsentResponse>(harness, userToken)
		.post('/oauth2/authorize/consent')
		.body({
			response_type: 'code',
			client_id: params.client_id,
			redirect_uri: params.redirect_uri,
			scope: params.scope,
			state: params.state ?? `state-${randomUUID()}`,
		})
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	const payload = json;
	if (!payload.redirect_to) {
		throw new Error('Missing redirect_to in response');
	}
	const url = new URL(payload.redirect_to);
	const code = url.searchParams.get('code');
	const state = url.searchParams.get('state') ?? '';
	if (!code) {
		throw new Error('Missing code in redirect');
	}
	return {code, state};
}

export async function exchangeOAuth2AuthorizationCode(
	harness: ApiTestHarness,
	params: {
		client_id: string;
		client_secret: string;
		code: string;
		redirect_uri: string;
	},
): Promise<OAuth2TokenResponse> {
	const formData = new URLSearchParams({
		grant_type: 'authorization_code',
		code: params.code,
		redirect_uri: params.redirect_uri,
		client_id: params.client_id,
	});
	const headers: Record<string, string> = {
		'Content-Type': 'application/x-www-form-urlencoded',
		Authorization: `Basic ${Buffer.from(`${params.client_id}:${params.client_secret}`).toString('base64')}`,
		'x-forwarded-for': '127.0.0.1',
	};
	const response = await harness.app.request('/oauth2/token', {
		method: 'POST',
		headers,
		body: formData.toString(),
	});
	if (response.status !== 200) {
		const text = await response.text();
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	const json = (await response.json()) as OAuth2TokenResponse;
	if (!json.access_token) {
		throw new Error('Missing access_token in response');
	}
	return json;
}

export async function refreshOAuth2Token(
	harness: ApiTestHarness,
	params: {
		client_id: string;
		client_secret: string;
		refresh_token: string;
	},
): Promise<OAuth2TokenResponse> {
	const formData = new URLSearchParams({
		grant_type: 'refresh_token',
		refresh_token: params.refresh_token,
		client_id: params.client_id,
	});
	const headers: Record<string, string> = {
		'Content-Type': 'application/x-www-form-urlencoded',
		Authorization: `Basic ${Buffer.from(`${params.client_id}:${params.client_secret}`).toString('base64')}`,
		'x-forwarded-for': '127.0.0.1',
	};
	const response = await harness.app.request('/oauth2/token', {
		method: 'POST',
		headers,
		body: formData.toString(),
	});
	if (response.status !== 200) {
		const text = await response.text();
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	const json = (await response.json()) as OAuth2TokenResponse;
	if (!json.access_token) {
		throw new Error('Missing access_token in response');
	}
	return json;
}

export async function revokeOAuth2Token(
	harness: ApiTestHarness,
	params: {
		client_id: string;
		client_secret: string;
		token: string;
		token_type_hint?: 'access_token' | 'refresh_token';
	},
): Promise<void> {
	const formData = new URLSearchParams({
		token: params.token,
	});
	if (params.token_type_hint) {
		formData.set('token_type_hint', params.token_type_hint);
	}
	const headers: Record<string, string> = {
		'Content-Type': 'application/x-www-form-urlencoded',
		Authorization: `Basic ${Buffer.from(`${params.client_id}:${params.client_secret}`).toString('base64')}`,
		'x-forwarded-for': '127.0.0.1',
	};
	const response = await harness.app.request('/oauth2/token/revoke', {
		method: 'POST',
		headers,
		body: formData.toString(),
	});
	if (response.status !== 200) {
		const text = await response.text();
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
}

export async function introspectOAuth2Token(
	harness: ApiTestHarness,
	params: {
		client_id: string;
		client_secret: string;
		token: string;
	},
): Promise<OAuth2IntrospectResponse> {
	const formData = new URLSearchParams({
		token: params.token,
	});
	const headers: Record<string, string> = {
		'Content-Type': 'application/x-www-form-urlencoded',
		Authorization: `Basic ${Buffer.from(`${params.client_id}:${params.client_secret}`).toString('base64')}`,
		'x-forwarded-for': '127.0.0.1',
	};
	const response = await harness.app.request('/oauth2/introspect', {
		method: 'POST',
		headers,
		body: formData.toString(),
	});
	if (response.status !== 200) {
		const text = await response.text();
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	return (await response.json()) as OAuth2IntrospectResponse;
}

export async function getOAuth2UserInfo(
	harness: ApiTestHarness,
	accessToken: string,
): Promise<Record<string, unknown>> {
	const {response, json} = await createBuilder<Record<string, unknown>>(harness, `Bearer ${accessToken}`)
		.get('/oauth2/userinfo')
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}`);
	}
	return json;
}

export async function listOAuth2Authorizations(
	harness: ApiTestHarness,
	userToken: string,
): Promise<
	Array<{
		application: {
			id: string;
		};
		scopes: Array<string>;
		authorized_at: string;
	}>
> {
	const {response, text, json} = await createBuilder<
		Array<{
			application: {
				id: string;
			};
			scopes: Array<string>;
			authorized_at: string;
		}>
	>(harness, userToken)
		.get('/oauth2/@me/authorizations')
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	return json;
}

export async function deauthorizeOAuth2Application(
	harness: ApiTestHarness,
	userToken: string,
	applicationId: string,
): Promise<void> {
	const {response, text} = await createBuilder(harness, userToken)
		.delete(`/oauth2/@me/authorizations/${applicationId}`)
		.executeRaw();
	if (response.status !== 204) {
		throw new Error(`Expected 204, got ${response.status}: ${text}`);
	}
}

export async function deauthorizeOAuth2Applications(
	harness: ApiTestHarness,
	userToken: string,
	applicationIds: Array<string>,
): Promise<void> {
	const {response, text} = await createBuilder(harness, userToken)
		.post('/oauth2/@me/authorizations/revoke')
		.body({application_ids: applicationIds})
		.executeRaw();
	if (response.status !== 204) {
		throw new Error(`Expected 204, got ${response.status}: ${text}`);
	}
}

export async function createOAuth2TestSetup(harness: ApiTestHarness) {
	const appOwner = await createTestAccount(harness);
	const endUser = await createTestAccount(harness);
	const redirectURI = `https://example.com/callback/${randomUUID()}`;
	const application = await createOAuth2Application(harness, appOwner, {
		name: `Test App ${randomUUID()}`,
		redirect_uris: [redirectURI],
	});
	return {
		appOwner,
		endUser,
		redirectURI,
		application,
	};
}
