// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	type AltchaCaptchaConfig,
	DEFAULT_ALTCHA_CAPTCHA_CONFIG,
} from '@fluxer/schema/src/domains/admin/AltchaCaptchaSchemas';
import {solveChallenge} from 'altcha-lib';
import {deriveKey} from 'altcha-lib/algorithms/pbkdf2';
import type {Challenge} from 'altcha-lib/types';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface CaptchaErrorBody {
	code: string;
	captcha_provider?: string;
	altcha_challenge?: Challenge;
}

const FORGOT_PATH = '/auth/forgot';
const FORGOT_BODY = {email: 'altcha-nobody@example.com'};

async function setAltchaConfig(overrides: Partial<AltchaCaptchaConfig>): Promise<void> {
	await getInstanceConfigRepository().setAltchaCaptchaConfig({
		...DEFAULT_ALTCHA_CAPTCHA_CONFIG,
		enabled: true,
		cost: 1000,
		max_counter: 100,
		...overrides,
	});
}

async function solve(challenge: Challenge): Promise<string> {
	const solution = await solveChallenge({challenge, deriveKey, timeout: 0});
	if (!solution) throw new Error('ALTCHA challenge was not solved');
	return Buffer.from(JSON.stringify({challenge, solution}), 'utf8').toString('base64');
}

async function rejectWith(builder: TestRequestBuilder<CaptchaErrorBody>, code: string): Promise<CaptchaErrorBody> {
	const {json} = await builder.expect(HTTP_STATUS.BAD_REQUEST, code).executeWithResponse();
	expect(json.code).toBe(code);
	return json;
}

function forgot(harness: ApiTestHarness): TestRequestBuilder<CaptchaErrorBody> {
	return createBuilderWithoutAuth<CaptchaErrorBody>(harness).post(FORGOT_PATH).body(FORGOT_BODY);
}

describe('ALTCHA captcha experiment', () => {
	let harness: ApiTestHarness;
	let previousCaptchaEnabled: boolean;
	let previousTestModeEnabled: boolean;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		previousCaptchaEnabled = Config.captcha.enabled;
		previousTestModeEnabled = Config.dev.testModeEnabled;
		Config.captcha.enabled = true;
		Config.dev.testModeEnabled = true;
	});

	afterEach(() => {
		Config.captcha.enabled = previousCaptchaEnabled;
		Config.dev.testModeEnabled = previousTestModeEnabled;
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('keeps the configured provider while the experiment is off', async () => {
		const body = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		expect(body).not.toHaveProperty('captcha_provider');
		expect(body).not.toHaveProperty('altcha_challenge');
	});

	it('leaves anonymous requests on the configured provider unless anonymous_enabled is set', async () => {
		await setAltchaConfig({rollout_basis_points: 10000});
		const body = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		expect(body).not.toHaveProperty('altcha_challenge');
	});

	it('serves anonymous requests a challenge and accepts the solved payload once', async () => {
		await setAltchaConfig({anonymous_enabled: true});
		const required = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		expect(required.captcha_provider).toBe('altcha');
		expect(required.altcha_challenge?.parameters).toMatchObject({algorithm: 'PBKDF2/SHA-256', cost: 1000});
		const token = await solve(required.altcha_challenge as Challenge);

		await forgot(harness)
			.header('X-Captcha-Token', token)
			.header('X-Captcha-Type', 'altcha')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const replayed = await rejectWith(
			forgot(harness).header('X-Captcha-Token', token).header('X-Captcha-Type', 'altcha'),
			APIErrorCodes.INVALID_CAPTCHA,
		);
		expect(replayed.captcha_provider).toBe('altcha');
		expect(replayed.altcha_challenge?.signature).not.toBe(required.altcha_challenge?.signature);
	});

	it('rejects a payload whose derived key does not match the challenge', async () => {
		await setAltchaConfig({anonymous_enabled: true});
		const required = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		const challenge = required.altcha_challenge as Challenge;
		const forged = Buffer.from(
			JSON.stringify({challenge, solution: {counter: 1, derivedKey: '00'.repeat(32)}}),
			'utf8',
		).toString('base64');

		await rejectWith(
			forgot(harness).header('X-Captcha-Token', forged).header('X-Captcha-Type', 'altcha'),
			APIErrorCodes.INVALID_CAPTCHA,
		);
	});

	it('rejects an ALTCHA payload from a requester outside the experiment', async () => {
		await setAltchaConfig({anonymous_enabled: true});
		const required = await rejectWith(forgot(harness), APIErrorCodes.CAPTCHA_REQUIRED);
		const token = await solve(required.altcha_challenge as Challenge);
		await setAltchaConfig({anonymous_enabled: false});

		const rejected = await rejectWith(
			forgot(harness).header('X-Captcha-Token', token).header('X-Captcha-Type', 'altcha'),
			APIErrorCodes.INVALID_CAPTCHA,
		);
		expect(rejected).not.toHaveProperty('altcha_challenge');
	});

	it('buckets signed-in users by their own rollout and still accepts the configured provider', async () => {
		Config.captcha.enabled = false;
		const included = await createTestAccount(harness);
		const excluded = await createTestAccount(harness);
		Config.captcha.enabled = true;
		await setAltchaConfig({
			anonymous_enabled: true,
			included_user_ids: [included.userId],
			excluded_user_ids: [excluded.userId],
		});
		const redeemPath = '/gifts/altcha-gift-code/redeem';

		const excludedBody = await rejectWith(
			createBuilder<CaptchaErrorBody>(harness, excluded.token).post(redeemPath),
			APIErrorCodes.CAPTCHA_REQUIRED,
		);
		expect(excludedBody).not.toHaveProperty('altcha_challenge');

		const includedBody = await rejectWith(
			createBuilder<CaptchaErrorBody>(harness, included.token).post(redeemPath),
			APIErrorCodes.CAPTCHA_REQUIRED,
		);
		const token = await solve(includedBody.altcha_challenge as Challenge);
		const solved = await createBuilder<CaptchaErrorBody>(harness, included.token)
			.post(redeemPath)
			.header('X-Captcha-Token', token)
			.header('X-Captcha-Type', 'altcha')
			.executeRaw();
		expect([APIErrorCodes.CAPTCHA_REQUIRED, APIErrorCodes.INVALID_CAPTCHA]).not.toContain(solved.json?.code);

		const classic = await createBuilder<CaptchaErrorBody>(harness, included.token)
			.post(redeemPath)
			.header('X-Captcha-Token', 'hcaptcha-token')
			.header('X-Captcha-Type', 'hcaptcha')
			.executeRaw();
		expect([APIErrorCodes.CAPTCHA_REQUIRED, APIErrorCodes.INVALID_CAPTCHA]).not.toContain(classic.json?.code);
	});
});
