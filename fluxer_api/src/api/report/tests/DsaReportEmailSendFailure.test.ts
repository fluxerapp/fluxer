// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createUniqueEmail, findLastTestEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

describe('DSA verification email delivery result on the route', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	test('the route answers 503 and the next send goes through right away', async () => {
		await clearTestEmails(harness);
		const email = createUniqueEmail('dsa-send-failure');
		const send = vi.spyOn(TestEmailService.prototype, 'sendDsaReportVerificationCode').mockResolvedValueOnce(false);
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email})
			.expect(HTTP_STATUS.SERVICE_UNAVAILABLE, APIErrorCodes.SERVICE_UNAVAILABLE)
			.execute();
		expect(send).toHaveBeenCalledTimes(1);
		expect(await new ReportRepository().getDsaEmailVerification(email.toLowerCase())).toBeNull();
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email})
			.expect(HTTP_STATUS.OK)
			.execute();
		const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/verify')
			.body({email, code})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
});
