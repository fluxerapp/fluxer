// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	CAPTCHA_TEST_HEADER,
	type CaptchaErrorBody,
	solveCaptchaChallenge,
	useCheapCaptcha,
} from '@app/api/test/CaptchaTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {CaptchaConfigResponse} from '@fluxer/schema/src/domains/admin/CaptchaSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

async function rejectWith(builder: TestRequestBuilder<CaptchaErrorBody>, code: string): Promise<CaptchaErrorBody> {
	const {json} = await builder.expect(HTTP_STATUS.BAD_REQUEST, code).executeWithResponse();
	expect(json.code).toBe(code);
	return json;
}

function forgot(harness: ApiTestHarness): TestRequestBuilder<CaptchaErrorBody> {
	return createBuilderWithoutAuth<CaptchaErrorBody>(harness)
		.post('/auth/forgot')
		.header(CAPTCHA_TEST_HEADER, 'true')
		.body({email: 'captcha-nobody@example.com'});
}

describe('Captcha challenge', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await useCheapCaptcha();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('issues an ALTCHA challenge to a request without a token', async () => {
		const required = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		expect(required.captcha_provider).toBe('altcha');
		expect(required.altcha_challenge?.parameters).toMatchObject({
			algorithm: 'PBKDF2/SHA-256',
			cost: 1000,
			keyLength: 32,
		});
		expect(required.altcha_challenge?.signature).toBeTruthy();
	});

	it('accepts a solved challenge once and answers a replay with a fresh challenge', async () => {
		const required = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		const token = await solveCaptchaChallenge(required);

		await forgot(harness).header('X-Captcha-Token', token).expect(HTTP_STATUS.NO_CONTENT).execute();

		const replayed = await rejectWith(forgot(harness).header('X-Captcha-Token', token), APIErrorCodes.INVALID_CAPTCHA);
		expect(replayed.captcha_provider).toBe('altcha');
		expect(replayed.altcha_challenge?.signature).toBeTruthy();
		expect(replayed.altcha_challenge?.signature).not.toBe(required.altcha_challenge?.signature);
	});

	it('rejects a payload whose derived key does not match the challenge', async () => {
		const required = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		const challenge = required.altcha_challenge;
		const forged = Buffer.from(
			JSON.stringify({
				challenge: {parameters: challenge?.parameters, signature: challenge?.signature},
				solution: {counter: 1, derivedKey: '00'.repeat(32)},
			}),
			'utf8',
		).toString('base64');

		const rejected = await rejectWith(forgot(harness).header('X-Captcha-Token', forged), APIErrorCodes.INVALID_CAPTCHA);
		expect(rejected.altcha_challenge?.signature).toBeTruthy();
	});

	it('skips the check once an admin turns it off', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

		const updated = await createBuilder<{captcha: CaptchaConfigResponse}>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({captcha: {enabled: false}})
			.execute();
		expect(updated.captcha).toEqual({enabled: false, cost: 1000, max_counter: 100});

		await forgot(harness).expect(HTTP_STATUS.NO_CONTENT).execute();
	});
});
