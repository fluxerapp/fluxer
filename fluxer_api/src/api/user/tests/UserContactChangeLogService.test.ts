// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('UserContactChangeLogService', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	describe('email change operations', () => {
		test('direct email change is rejected - requires email_token flow', async () => {
			const account = await createTestAccount(harness, {
				email: 'original@example.com',
			});
			const {json} = await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({email: 'updated@example.com', password: TEST_CREDENTIALS.STRONG_PASSWORD})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as {
				errors?: Array<{
					code: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('EMAIL_MUST_BE_CHANGED_VIA_TOKEN');
		});
		test('email change without password is rejected', async () => {
			const account = await createTestAccount(harness);
			const json = await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({email: 'new@example.com'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			const body = json as {
				errors?: Array<{
					code: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('EMAIL_MUST_BE_CHANGED_VIA_TOKEN');
		});
	});
	describe('username change operations', () => {
		test('username change without password requires sudo mode', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({username: 'newusername'})
				.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
				.execute();
		});
	});
});
