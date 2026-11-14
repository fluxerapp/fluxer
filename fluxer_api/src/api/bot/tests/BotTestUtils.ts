// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {ApplicationResponse} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';

interface TestBotAccount {
	appId: string;
	botUserId: string;
	botToken: string;
	clientSecret: string;
	ownerEmail: string;
	ownerPassword: string;
	ownerUserId: string;
	ownerToken: string;
}
async function createOAuth2BotApplication(
	harness: ApiTestHarness,
	ownerToken: string,
	name: string,
	redirectURIs: Array<string> = [],
): Promise<{
	appId: string;
	botUserId: string;
	botToken: string;
	clientSecret: string;
}> {
	const app = await createBuilder<ApplicationResponse>(harness, ownerToken)
		.post('/oauth2/applications')
		.body({
			name,
			redirect_uris: redirectURIs,
		})
		.execute();
	if (!app.id || !app.client_secret || !app.bot?.id || !app.bot?.token) {
		throw new Error('Application response missing required fields');
	}
	return {
		appId: app.id,
		botUserId: app.bot.id,
		botToken: app.bot.token,
		clientSecret: app.client_secret,
	};
}
export async function createTestBotAccount(
	harness: ApiTestHarness,
	params?: {
		appName?: string;
		redirectURIs?: Array<string>;
	},
): Promise<TestBotAccount> {
	const owner = await createTestAccount(harness);
	const appName = params?.appName ?? `Test Bot ${randomUUID()}`;
	const botApp = await createOAuth2BotApplication(harness, owner.token, appName, params?.redirectURIs);
	return {
		appId: botApp.appId,
		botUserId: botApp.botUserId,
		botToken: botApp.botToken,
		clientSecret: botApp.clientSecret,
		ownerEmail: owner.email,
		ownerPassword: owner.password,
		ownerUserId: owner.userId,
		ownerToken: owner.token,
	};
}
export async function authorizeBot(
	harness: ApiTestHarness,
	userToken: string,
	clientId: string,
	scopes: Array<string>,
	guildId?: string,
	permissions?: string,
): Promise<{
	redirectUrl: string;
}> {
	const body: Record<string, unknown> = {
		client_id: clientId,
		scope: scopes.join(' '),
	};
	if (guildId) {
		body.guild_id = guildId;
	}
	if (permissions) {
		body.permissions = permissions;
	}
	const consent = await createBuilder<{
		redirect_to: string;
	}>(harness, userToken)
		.post('/oauth2/authorize/consent')
		.body(body)
		.execute();
	if (!consent.redirect_to) {
		throw new Error('Authorization response missing redirect_to');
	}
	return {redirectUrl: consent.redirect_to};
}
