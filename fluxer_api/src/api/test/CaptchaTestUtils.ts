// SPDX-License-Identifier: AGPL-3.0-or-later

import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {solveChallenge} from 'altcha-lib';
import {deriveKey} from 'altcha-lib/algorithms/pbkdf2';
import type {Challenge} from 'altcha-lib/types';

export const CAPTCHA_TEST_HEADER = 'x-fluxer-test-enable-captcha';

export interface CaptchaErrorBody {
	code: string;
	captcha_provider?: string;
	altcha_challenge?: Challenge;
}

export async function useCheapCaptcha(): Promise<void> {
	await getInstanceConfigRepository().updateCaptchaConfig({enabled: true, cost: 1000, max_counter: 100});
}

export async function solveCaptchaChallenge(body: CaptchaErrorBody): Promise<string> {
	const challenge = body.altcha_challenge;
	if (!challenge) throw new Error('The response carried no ALTCHA challenge');
	const solution = await solveChallenge({challenge, deriveKey, timeout: 0});
	if (!solution) throw new Error('The ALTCHA challenge was not solved');
	const payload = {challenge: {parameters: challenge.parameters, signature: challenge.signature}, solution};
	return Buffer.from(JSON.stringify(payload), 'utf8').toString('base64');
}

export async function issueSolvedCaptchaToken(harness: ApiTestHarness): Promise<string> {
	const {json} = await createBuilderWithoutAuth<CaptchaErrorBody>(harness)
		.post('/auth/forgot')
		.header(CAPTCHA_TEST_HEADER, 'true')
		.body({email: 'captcha-token-source@example.com'})
		.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CAPTCHA_REQUIRED)
		.executeWithResponse();
	return await solveCaptchaChallenge(json);
}
