// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ReportResponse {
	report_id: string;
}

interface Me {
	username: string;
	discriminator: string;
}

async function issueTicket(harness: ApiTestHarness): Promise<string> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-tag');
	await createBuilderWithoutAuth(harness)
		.post('/reports/dsa/email/send')
		.body({email})
		.expect(HTTP_STATUS.OK)
		.execute();
	const code = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification')?.metadata.code;
	const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
		.post('/reports/dsa/email/verify')
		.body({email, code})
		.expect(HTTP_STATUS.OK)
		.execute();
	return ticket;
}

function submitUserReport(harness: ApiTestHarness, ticket: string, userTag: string) {
	return createBuilderWithoutAuth<ReportResponse>(harness).post('/reports/dsa').body({
		ticket,
		report_type: 'user',
		category: 'harassment',
		user_tag: userTag,
		reporter_full_legal_name: 'John Doe',
		reporter_country_of_residence: 'DE',
	});
}

async function reportedUserIdOf(reportId: string): Promise<string | undefined> {
	const report = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
	return report?.reportedUserId?.toString();
}

async function readMe(harness: ApiTestHarness, token: string): Promise<Me> {
	return createBuilder<Me>(harness, token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
}

describe('DSA user lookup by tag', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('an instance with random tags finds the user by username#1234 and rejects a bare username', async () => {
		const target = await createTestAccount(harness);
		const me = await readMe(harness, target.token);
		const ticket = await issueTicket(harness);
		await submitUserReport(harness, ticket, me.username)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_DSA_REPORT_TARGET)
			.execute();
		const result = await submitUserReport(harness, ticket, `${me.username}#${me.discriminator}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await reportedUserIdOf(result.report_id)).toBe(target.userId);
	});

	test('an instance without tags finds the user by bare username and by username#0000', async () => {
		const config = getConfig();
		const originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup', TagStyles.NONE);
			const target = await createTestAccount(harness);
			const me = await readMe(harness, target.token);
			expect(me.discriminator).toBe('0000');
			for (const userTag of [me.username, `  ${me.username.toUpperCase()}  `, `${me.username}#0000`]) {
				const result = await submitUserReport(harness, await issueTicket(harness), userTag)
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(await reportedUserIdOf(result.report_id)).toBe(target.userId);
			}
			await submitUserReport(harness, await issueTicket(harness), 'nobody_has_this_name')
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_USER)
				.execute();
		} finally {
			config.instance.selfHosted = originalSelfHosted;
			getInstanceConfigRepository().clearCacheForTesting();
		}
	});
});
