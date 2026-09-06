// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppErrorHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import {Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {Config} from '../../Config';
import type {InstanceCaptchaEffectiveConfig, InstanceConfigRepository} from '../../instance/InstanceConfigRepository';
import type {HonoEnv} from '../../types/HonoEnv';
import {CaptchaMiddleware} from '../CaptchaMiddleware';

const HCAPTCHA_ONLY: InstanceCaptchaEffectiveConfig = {
	enabled: true,
	provider: 'hcaptcha',
	hcaptcha_site_key: 'hcaptcha-site-key',
	hcaptcha_secret_key: 'hcaptcha-secret-key',
	turnstile_site_key: null,
	turnstile_secret_key: null,
};

function createHarness(
	captcha: InstanceCaptchaEffectiveConfig,
): (headers: Record<string, string>) => Promise<Response> {
	const repository = {
		getEffectiveCaptchaConfig: async () => captcha,
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

describe('CaptchaMiddleware provider header', () => {
	let previousTestModeEnabled: boolean;

	beforeEach(() => {
		previousTestModeEnabled = Config.dev.testModeEnabled;
		Config.dev.testModeEnabled = false;
	});

	afterEach(() => {
		Config.dev.testModeEnabled = previousTestModeEnabled;
	});

	it('rejects a header naming a provider the instance holds no secret key for with 400 INVALID_CAPTCHA', async () => {
		const request = createHarness(HCAPTCHA_ONLY);
		const response = await request({'x-captcha-token': 'solution', 'x-captcha-type': 'turnstile'});
		expect(response.status).toBe(400);
		expect(await response.json()).toMatchObject({code: 'INVALID_CAPTCHA'});
	});

	it('rejects a request with no proof with 400 CAPTCHA_REQUIRED', async () => {
		const request = createHarness(HCAPTCHA_ONLY);
		const response = await request({});
		expect(response.status).toBe(400);
		expect(await response.json()).toMatchObject({code: 'CAPTCHA_REQUIRED'});
	});
});
