// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createUniqueEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function countVerificationEmailsTo(harness: ApiTestHarness, email: string): Promise<number> {
	const emails = await listTestEmails(harness);
	return emails.filter((sent) => sent.type === 'dsa_report_verification' && sent.to === email.toLowerCase()).length;
}

describe('DSA report verification email recipient limit', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('limits verification emails per address regardless of letter case', async () => {
		await clearTestEmails(harness);
		const email = createUniqueEmail('dsa-recipient');
		for (let attempt = 0; attempt < 3; attempt++) {
			await createBuilderWithoutAuth(harness)
				.post('/reports/dsa/email/send')
				.body({email})
				.expect(HTTP_STATUS.OK)
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email: email.toUpperCase()})
			.expect(429)
			.execute();
		expect(await countVerificationEmailsTo(harness, email)).toBe(3);
	});

	test('keeps sending to other addresses after one address reaches its limit', async () => {
		await clearTestEmails(harness);
		const limited = createUniqueEmail('dsa-recipient');
		for (let attempt = 0; attempt < 3; attempt++) {
			await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email: limited}).execute();
		}
		const other = createUniqueEmail('dsa-recipient');
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email: other})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await countVerificationEmailsTo(harness, other)).toBe(1);
	});
});
