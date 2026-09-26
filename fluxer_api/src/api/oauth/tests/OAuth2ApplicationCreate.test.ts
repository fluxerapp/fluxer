// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {createOAuth2Application, createUniqueApplicationName} from '@app/api/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ValidationErrorResponse {
	errors?: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

async function withCaptchaEnabled<T>(run: () => Promise<T>): Promise<T> {
	const previousEnabled = Config.captcha.enabled;
	const previousTestModeEnabled = Config.dev.testModeEnabled;
	Config.captcha.enabled = true;
	Config.dev.testModeEnabled = true;
	try {
		return await run();
	} finally {
		Config.captcha.enabled = previousEnabled;
		Config.dev.testModeEnabled = previousTestModeEnabled;
	}
}

describe('OAuth2 Application Create', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('creates OAuth2 application with bot user', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		const redirectURIs = ['https://example.com/callback'];
		const result = await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: redirectURIs,
			bot_public: true,
		});
		expect(result.application.id).toBeTruthy();
		expect(result.application.name).toBe(appName);
		expect(result.application.redirect_uris).toEqual(redirectURIs);
		expect(result.application.bot).toBeDefined();
		expect(result.application.bot?.id).toBeTruthy();
		expect(result.application.bot?.username).toBeTruthy();
		expect(result.application.bot?.discriminator).toBeTruthy();
		expect(result.application.bot?.token).toBeTruthy();
		expect(result.clientSecret).toBeTruthy();
		expect(result.botUserId).toBe(result.application.bot?.id);
		expect(result.botToken).toBe(result.application.bot?.token);
		const botUser = await createBuilder<{
			id: string;
			bot: boolean;
		}>(harness, `Bot ${result.botToken}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(botUser.id).toBe(result.botUserId);
		expect(botUser.bot).toBe(true);
	});
	test('falls back to a valid random bot username when the application name sanitizes to a forbidden username', async () => {
		const account = await createTestAccount(harness);
		const result = await createOAuth2Application(harness, account.token, {
			name: 'fluxer system message',
		});
		const botUsername = result.application.bot?.username;
		expect(botUsername).toBeTruthy();
		expect(UsernameType.safeParse(botUsername).success).toBe(true);
		expect(botUsername?.toLowerCase()).not.toContain('fluxer');
		expect(botUsername?.toLowerCase()).not.toContain('systemmessage');
	});
	test('creates application without optional fields', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		const result = await createOAuth2Application(harness, account.token, {
			name: appName,
		});
		expect(result.application.id).toBeTruthy();
		expect(result.application.name).toBe(appName);
		expect(result.application.redirect_uris).toEqual([]);
		expect(result.application.bot).toBeDefined();
		expect(result.application.bot?.id).toBeTruthy();
	});
	test('requires captcha when creating a bot application', async () => {
		const account = await createTestAccount(harness);
		await withCaptchaEnabled(async () =>
			createBuilder(harness, account.token)
				.post('/oauth2/applications')
				.body({name: createUniqueApplicationName()})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
				.execute(),
		);
	});
	test('creates a bot application with a valid captcha token', async () => {
		const account = await createTestAccount(harness);
		await withCaptchaEnabled(async () =>
			createBuilder(harness, account.token)
				.post('/oauth2/applications')
				.header('x-captcha-token', 'test-captcha-token')
				.body({name: createUniqueApplicationName()})
				.expect(HTTP_STATUS.OK)
				.execute(),
		);
	});
	test('rejects missing name', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/oauth2/applications')
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('rejects non-localhost http redirect URI hostnames', async () => {
		const account = await createTestAccount(harness);
		const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
			.post('/oauth2/applications')
			.body({
				name: createUniqueApplicationName(),
				redirect_uris: ['http://example.com/callback'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(json.errors?.some((error) => error.path === 'redirect_uris.0')).toBe(true);
	});
	test('accepts https redirect URIs', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		const result = await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: ['https://example.com/callback'],
		});
		expect(result.application.redirect_uris).toEqual(['https://example.com/callback']);
	});
	test('accepts localhost redirect URIs with http', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		const result = await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: ['http://localhost:3000/callback'],
		});
		expect(result.application.redirect_uris).toEqual(['http://localhost:3000/callback']);
	});
	test('accepts IP address redirect URIs with http', async () => {
		const account = await createTestAccount(harness);
		const redirectURIs = ['http://192.168.1.42:3000/callback', 'http://[2001:db8::1]:3000/callback'];
		const result = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
			redirect_uris: redirectURIs,
		});
		expect(result.application.redirect_uris).toEqual(redirectURIs);
	});
});
