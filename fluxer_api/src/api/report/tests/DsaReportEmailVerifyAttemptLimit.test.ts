// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createUniqueEmail, findLastTestEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const WRONG_CODE = 'AAAA-AAAA';

async function requestCode(
	harness: ApiTestHarness,
	email = createUniqueEmail('dsa-attempts'),
): Promise<{
	email: string;
	code: string;
}> {
	await clearTestEmails(harness);
	await createBuilderWithoutAuth(harness)
		.post('/reports/dsa/email/send')
		.body({email})
		.expect(HTTP_STATUS.OK)
		.execute();
	const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
	if (typeof code !== 'string' || code === WRONG_CODE) {
		throw new Error('No usable DSA verification code was sent');
	}
	return {email, code};
}

function verify(harness: ApiTestHarness, email: string, code: string) {
	return createBuilderWithoutAuth<{ticket: string}>(harness).post('/reports/dsa/email/verify').body({email, code});
}

async function enterWrongCode(harness: ApiTestHarness, email: string, times: number): Promise<void> {
	for (let attempt = 0; attempt < times; attempt++) {
		await verify(harness, email, WRONG_CODE)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_VERIFICATION_CODE)
			.execute();
	}
}

async function storedCode(email: string) {
	return new ReportRepository().getDsaEmailVerification(email.toLowerCase());
}

describe('DSA verification code attempts per address', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('four wrong codes leave the sent code usable', async () => {
		const {email, code} = await requestCode(harness);
		await enterWrongCode(harness, email, 4);
		expect(await storedCode(email)).not.toBeNull();
		await verify(harness, email, code).expect(HTTP_STATUS.OK).execute();
	});

	test('after five wrong codes the sent code no longer verifies and is removed', async () => {
		const {email, code} = await requestCode(harness);
		await enterWrongCode(harness, email, 5);
		expect(await storedCode(email)).not.toBeNull();
		const wrong = await verify(harness, email, WRONG_CODE).executeRaw();
		const {response, json} = await verify(harness, email, code).executeRaw();
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(json).toEqual(wrong.json);
		expect((json as {code?: string}).code).toBe(APIErrorCodes.INVALID_DSA_VERIFICATION_CODE);
		expect(await storedCode(email)).toBeNull();
	});

	test('attempts are counted per address', async () => {
		const first = await requestCode(harness);
		await enterWrongCode(harness, first.email, 6);
		expect(await storedCode(first.email)).toBeNull();
		const second = await requestCode(harness);
		await enterWrongCode(harness, second.email, 1);
		await verify(harness, second.email, second.code).expect(HTTP_STATUS.OK).execute();
	});

	test('letter case variants of an address share one count', async () => {
		const {email, code} = await requestCode(harness);
		await enterWrongCode(harness, email, 3);
		await enterWrongCode(harness, email.toUpperCase(), 2);
		await verify(harness, email, code)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_VERIFICATION_CODE)
			.execute();
		expect(await storedCode(email)).toBeNull();
	});

	test('a newly sent code starts a new count', async () => {
		const first = await requestCode(harness);
		await enterWrongCode(harness, first.email, 6);
		const second = await requestCode(harness, first.email);
		await enterWrongCode(harness, second.email, 4);
		await verify(harness, second.email, second.code).expect(HTTP_STATUS.OK).execute();
	});
});
