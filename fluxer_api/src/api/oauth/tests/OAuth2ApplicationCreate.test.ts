// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUniqueApplicationName} from '@app/api/oauth/tests/OAuth2TestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {CAPTCHA_TEST_HEADER, useCheapCaptcha} from '@app/api/test/CaptchaTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ValidationErrorResponse {
	errors?: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

describe('OAuth2 Application Create', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('requires captcha when creating a bot application', async () => {
		const account = await createTestAccount(harness);
		await useCheapCaptcha();
		await createBuilder(harness, account.token)
			.post('/oauth2/applications')
			.header(CAPTCHA_TEST_HEADER, 'true')
			.body({name: createUniqueApplicationName()})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
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
});
