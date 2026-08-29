// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {Config} from '../../Config';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createAuthHarness, createUniqueEmail, createUniqueUsername, registerUser} from './AuthTestUtils';

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

async function registerAndFlag(
	harness: ApiTestHarness,
	flags: Array<string>,
): Promise<{email: string; password: string; userId: string}> {
	const email = createUniqueEmail('captcha-flags');
	const password = 'a-strong-password';
	const reg = await registerUser(harness, {
		email,
		username: createUniqueUsername('captchaflags'),
		global_name: 'Captcha Flags User',
		password,
		date_of_birth: '2000-01-01',
		consent: true,
	});
	if (flags.length > 0) {
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${reg.user_id}/security-flags`)
			.body({set_flags: flags})
			.execute();
	}
	return {email, password, userId: reg.user_id};
}

describe('Auth Captcha Bypass Flags', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('lets APP_STORE_REVIEWER accounts log in without solving a captcha', async () => {
		const account = await registerAndFlag(harness, ['APP_STORE_REVIEWER']);
		await withCaptchaEnabled(async () => {
			const resp = await createBuilderWithoutAuth<{token?: string; user_id?: string}>(harness)
				.post('/auth/login')
				.body({email: account.email, password: account.password})
				.execute();
			expect(resp.token).toBeTruthy();
			expect(resp.user_id).toBe(account.userId);
		});
	});
	it('still requires a captcha for accounts without the APP_STORE_REVIEWER flag', async () => {
		const account = await registerAndFlag(harness, []);
		await withCaptchaEnabled(async () => {
			await createBuilderWithoutAuth(harness)
				.post('/auth/login')
				.body({email: account.email, password: account.password})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
				.execute();
		});
	});
});
