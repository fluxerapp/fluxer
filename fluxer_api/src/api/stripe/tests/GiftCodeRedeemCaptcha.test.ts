// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {CAPTCHA_TEST_HEADER, useCheapCaptcha} from '@app/api/test/CaptchaTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, test} from 'vitest';

describe('Gift Code Redeem Captcha', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		await useCheapCaptcha();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('rejects an unauthenticated redeem before reading the captcha', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/gifts/test-gift-code/redeem')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.UNAUTHORIZED)
			.execute();
	});
	test('requires captcha when redeeming with a credential', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/gifts/test-gift-code/redeem')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
			.execute();
	});
});
