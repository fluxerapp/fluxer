// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {CAPTCHA_TEST_HEADER, useCheapCaptcha} from '@app/api/test/CaptchaTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
		await useCheapCaptcha();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('lets APP_STORE_REVIEWER accounts log in without solving a captcha', async () => {
		const account = await registerAndFlag(harness, ['APP_STORE_REVIEWER']);
		const resp = await createBuilderWithoutAuth<{token?: string; user_id?: string}>(harness)
			.post('/auth/login')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body({email: account.email, password: account.password})
			.execute();
		expect(resp.token).toBeTruthy();
		expect(resp.user_id).toBe(account.userId);
	});
	it('still requires a captcha for accounts without the APP_STORE_REVIEWER flag', async () => {
		const account = await registerAndFlag(harness, []);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body({email: account.email, password: account.password})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
			.execute();
	});
	it('never looks up the email field for an exemption on a username instance', async () => {
		const account = await registerAndFlag(harness, ['APP_STORE_REVIEWER']);
		const config = getConfig();
		const originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.USERNAME, 'setup');
			await createBuilderWithoutAuth(harness)
				.post('/auth/login')
				.header(CAPTCHA_TEST_HEADER, 'true')
				.body({email: account.email, password: account.password})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
				.execute();
		} finally {
			config.instance.selfHosted = originalSelfHosted;
		}
	});
});
