// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {ApplicationResponse} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';

interface OAuth2CreateResult {
	application: ApplicationResponse;
	clientSecret: string;
	botUserId: string;
	botToken: string;
}

export async function createOAuth2Application(
	harness: ApiTestHarness,
	token: string,
	params: {
		name: string;
		redirect_uris?: Array<string> | null;
		bot_public?: boolean;
		bot_require_code_grant?: boolean;
	},
): Promise<OAuth2CreateResult> {
	const body = {
		name: params.name,
		redirect_uris: params.redirect_uris ?? [],
		...(params.bot_public !== undefined && {bot_public: params.bot_public}),
		...(params.bot_require_code_grant !== undefined && {bot_require_code_grant: params.bot_require_code_grant}),
	};
	const {response, text, json} = await createBuilder<ApplicationResponse>(harness, token)
		.post('/oauth2/applications')
		.body(body)
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	if (!json.id) {
		throw new Error('Application response missing id');
	}
	if (!json.bot?.id || !json.bot?.token) {
		throw new Error('Application response missing bot id or token');
	}
	if (!json.client_secret) {
		throw new Error('Application response missing client_secret');
	}
	return {
		application: json,
		clientSecret: json.client_secret,
		botUserId: json.bot.id,
		botToken: json.bot.token,
	};
}

export async function getOAuth2Application(
	harness: ApiTestHarness,
	token: string,
	applicationId: string,
): Promise<ApplicationResponse> {
	const {response, text, json} = await createBuilder<ApplicationResponse>(harness, token)
		.get(`/oauth2/applications/${applicationId}`)
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	return json;
}

export async function listOAuth2Applications(
	harness: ApiTestHarness,
	token: string,
): Promise<Array<ApplicationResponse>> {
	const {response, text, json} = await createBuilder<Array<ApplicationResponse>>(harness, token)
		.get('/oauth2/applications/@me')
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	return json;
}

export async function updateOAuth2Application(
	harness: ApiTestHarness,
	token: string,
	applicationId: string,
	params: {
		name?: string;
		redirect_uris?: Array<string> | null;
		bot_public?: boolean;
		bot_require_code_grant?: boolean;
	},
): Promise<ApplicationResponse> {
	const {response, text, json} = await createBuilder<ApplicationResponse>(harness, token)
		.patch(`/oauth2/applications/${applicationId}`)
		.body(params)
		.executeRaw();
	if (response.status !== 200) {
		throw new Error(`Expected 200, got ${response.status}: ${text}`);
	}
	return json;
}

export async function deleteOAuth2Application(
	harness: ApiTestHarness,
	token: string,
	applicationId: string,
	password: string,
): Promise<void> {
	const {response, text} = await createBuilder(harness, token)
		.delete(`/oauth2/applications/${applicationId}`)
		.body({password})
		.executeRaw();
	if (response.status !== 204) {
		throw new Error(`Expected 204, got ${response.status}: ${text}`);
	}
}

export function createUniqueApplicationName(prefix = 'Test App'): string {
	return `${prefix} ${randomUUID()}`;
}
