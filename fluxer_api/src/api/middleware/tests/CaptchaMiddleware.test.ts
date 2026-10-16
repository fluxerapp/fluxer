// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {CaptchaMiddleware} from '@app/api/middleware/CaptchaMiddleware';
import {CAPTCHA_TEST_HEADER} from '@app/api/test/CaptchaTestUtils';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {type CaptchaConfig, DEFAULT_CAPTCHA_CONFIG} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import {Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const CHEAP_CAPTCHA_CONFIG: CaptchaConfig = {...DEFAULT_CAPTCHA_CONFIG, cost: 1000, max_counter: 100};

function createHarness(captcha: CaptchaConfig): (headers: Record<string, string>) => Promise<Response> {
	const repository = {
		getCaptchaConfig: async () => captcha,
	} as unknown as InstanceConfigRepository;
	const app = new Hono<HonoEnv>();
	app.use(async (ctx, next) => {
		ctx.set('instanceConfigRepository', repository);
		await next();
	});
	app.use(CaptchaMiddleware);
	app.post('/auth/register', (ctx) => ctx.text('ok'));
	app.onError(AppErrorHandler);
	return async (headers) => app.request('http://localhost/auth/register', {method: 'POST', headers});
}

describe('CaptchaMiddleware', () => {
	let previousTestModeEnabled: boolean;

	beforeEach(() => {
		previousTestModeEnabled = Config.dev.testModeEnabled;
		Config.dev.testModeEnabled = false;
	});

	afterEach(() => {
		Config.dev.testModeEnabled = previousTestModeEnabled;
	});

	it('passes every request while the captcha is disabled', async () => {
		const response = await createHarness({...CHEAP_CAPTCHA_CONFIG, enabled: false})({});
		expect(response.status).toBe(200);
	});

	it('passes a test-mode request that does not opt in', async () => {
		Config.dev.testModeEnabled = true;
		const request = createHarness(CHEAP_CAPTCHA_CONFIG);
		expect((await request({})).status).toBe(200);
		expect((await request({[CAPTCHA_TEST_HEADER]: 'true'})).status).toBe(400);
	});

	it('answers a request with no token with CAPTCHA_REQUIRED and an ALTCHA challenge', async () => {
		const response = await createHarness(CHEAP_CAPTCHA_CONFIG)({});
		expect(response.status).toBe(400);
		expect(await response.json()).toMatchObject({
			code: 'CAPTCHA_REQUIRED',
			captcha_provider: 'altcha',
			altcha_challenge: {parameters: {algorithm: 'PBKDF2/SHA-256', cost: 1000}},
		});
	});

	it('answers a token that is not an ALTCHA payload with INVALID_CAPTCHA and a fresh challenge', async () => {
		const response = await createHarness(CHEAP_CAPTCHA_CONFIG)({'x-captcha-token': 'legacy-provider-token'});
		expect(response.status).toBe(400);
		expect(await response.json()).toMatchObject({
			code: 'INVALID_CAPTCHA',
			captcha_provider: 'altcha',
			altcha_challenge: {parameters: {algorithm: 'PBKDF2/SHA-256'}},
		});
	});
});
