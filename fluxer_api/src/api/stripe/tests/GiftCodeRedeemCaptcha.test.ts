// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

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

describe('Gift Code Redeem Captcha', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('rejects an unauthenticated redeem before reading the captcha', async () => {
		await withCaptchaEnabled(async () =>
			createBuilderWithoutAuth(harness)
				.post('/gifts/test-gift-code/redeem')
				.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.UNAUTHORIZED)
				.execute(),
		);
	});
	test('requires captcha when redeeming with a credential', async () => {
		const account = await createTestAccount(harness);
		await withCaptchaEnabled(async () =>
			createBuilder(harness, account.token)
				.post('/gifts/test-gift-code/redeem')
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
				.execute(),
		);
	});
});
