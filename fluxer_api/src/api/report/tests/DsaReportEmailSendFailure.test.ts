// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createUniqueEmail, findLastTestEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IEmailDnsValidationService} from '@app/api/infrastructure/IEmailDnsValidationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IInviteRepository} from '@app/api/invite/IInviteRepository';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {ReportService} from '@app/api/report/ReportService';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import type {IRateLimitService, RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface SentCode {
	email: string;
	code: string;
}

function allowedLimit(): RateLimitResult {
	return {allowed: true, limit: 5, remaining: 4, resetTime: new Date(Date.now() + 1000), resetAfterDecimal: 1};
}

function createService(sendResult: boolean): {service: ReportService; sent: Array<SentCode>} {
	const sent: Array<SentCode> = [];
	const emailService = {
		sendDsaReportVerificationCode: async (email: string, code: string) => {
			sent.push({email, code});
			return sendResult;
		},
	} as unknown as IEmailService;
	const rateLimitService = {
		checkLimit: async () => allowedLimit(),
		peekLimit: async () => allowedLimit(),
		resetLimit: async () => {},
	} as unknown as IRateLimitService;
	const emailDnsValidationService: IEmailDnsValidationService = {hasValidDnsRecords: async () => true};
	const service = new ReportService(
		new ReportRepository(),
		{} as IChannelRepository,
		{} as IGuildRepositoryAggregate,
		{} as IUserRepository,
		{} as IInviteRepository,
		emailService,
		emailDnsValidationService,
		{} as ISnowflakeService,
		{} as IStorageService,
		{} as IGatewayService,
		rateLimitService,
		{} as IWebhookRepository,
	);
	return {service, sent};
}

describe('DSA verification email delivery result in the service', () => {
	let executor: InMemoryCassandraQueryExecutor;

	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	test('an email that was not sent is answered with 503 and leaves no code behind', async () => {
		const {service, sent} = createService(false);
		const email = createUniqueEmail('dsa-send-failure');
		const failure = await service.sendDsaReportVerificationCode(email).catch((error: unknown) => error);
		expect(failure).toBeInstanceOf(ServiceUnavailableError);
		expect((failure as ServiceUnavailableError).status).toBe(503);
		expect((failure as ServiceUnavailableError).code).toBe(APIErrorCodes.SERVICE_UNAVAILABLE);
		expect(sent).toHaveLength(1);
		expect(await new ReportRepository().getDsaEmailVerification(email.toLowerCase())).toBeNull();
	});

	test('an email that was sent resolves and keeps its code', async () => {
		const {service, sent} = createService(true);
		const email = createUniqueEmail('dsa-send-failure');
		await expect(service.sendDsaReportVerificationCode(email)).resolves.toBeUndefined();
		expect(sent).toEqual([{email: email.toLowerCase(), code: expect.stringMatching(/^[A-Z0-9]{4}-[A-Z0-9]{4}$/)}]);
		expect(await new ReportRepository().getDsaEmailVerification(email.toLowerCase())).not.toBeNull();
		await expect(service.verifyDsaReportEmail(email, sent[0].code)).resolves.toMatch(/^[0-9a-f]{64}$/);
	});
});

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
