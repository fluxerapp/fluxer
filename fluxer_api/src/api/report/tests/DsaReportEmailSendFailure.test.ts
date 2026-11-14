// SPDX-License-Identifier: AGPL-3.0-or-later

import {createReportID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {getEmailService, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {getReportFlowResponse} from '@app/api/report/flows/ReportFlowRegistry';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createRecordingSmtpServer} from '@app/api/test/RecordingSmtpServer';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {EmailDeliveryModes} from '@fluxer/constants/src/AccountConfigConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {SmtpTlsModes} from '@fluxer/constants/src/SmtpConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const TEST_LOCALE = 'en-US';

interface ReportTarget {
	guildId: string;
	channelId: string;
	messageId: string;
	authorId: string;
}

async function configureSmtp(port: number) {
	await getInstanceConfigRepository().setInstanceIntegrationsConfig({
		email: {
			delivery: EmailDeliveryModes.SMTP,
			from_email: 'reports@example.com',
			from_name: 'Report Team',
			smtp: {host: '127.0.0.1', port, username: '', password: '', tls_mode: SmtpTlsModes.NONE},
		},
	});
}

function verificationCode(body: string): string {
	const match = /(?:^|\r\n)([A-Z0-9]{4}-[A-Z0-9]{4})(?:\r\n|$)/.exec(body);
	if (!match) throw new Error('The delivered DSA email has no verification code');
	return match[1];
}

describe('DSA email delivery through SMTP', () => {
	let harness: ApiTestHarness;
	const servers = new Set<Awaited<ReturnType<typeof createRecordingSmtpServer>>>();

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
		const config = getConfig();
		const previousTestMode = config.dev.testModeEnabled;
		try {
			config.dev.testModeEnabled = false;
			getEmailService();
		} finally {
			config.dev.testModeEnabled = previousTestMode;
		}
	});

	afterEach(async () => {
		try {
			await Promise.all([...servers].map((smtp) => smtp.close()));
		} finally {
			servers.clear();
			await harness?.shutdown();
		}
	});

	test('the route answers 503 and the next send goes through right away', async () => {
		const smtp = await createRecordingSmtpServer({firstDelivery: 'reject'});
		servers.add(smtp);
		await configureSmtp(smtp.port);
		const email = 'dsa-send-failure@example.com';
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email, locale: TEST_LOCALE})
			.expect(HTTP_STATUS.SERVICE_UNAVAILABLE, APIErrorCodes.SERVICE_UNAVAILABLE)
			.execute();
		expect(smtp.attempts).toHaveLength(1);
		expect(smtp.attempts[0]).toMatchObject({from: '<reports@example.com>', to: '<dsa-send-failure@example.com>'});
		expect(smtp.attempts[0].body).toContain('Use this code to send your Digital Services Act report to Fluxer.');
		expect(smtp.delivered).toEqual([]);
		expect(await new ReportRepository().getDsaEmailVerification(email)).toBeNull();
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email, locale: TEST_LOCALE})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(smtp.attempts).toHaveLength(2);
		expect(smtp.delivered).toHaveLength(1);
		expect(smtp.delivered[0]).toMatchObject({from: '<reports@example.com>', to: '<dsa-send-failure@example.com>'});
		expect(smtp.delivered[0].body).toContain('This code expires in 10 minutes.');
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/verify')
			.body({email, code: verificationCode(smtp.delivered[0].body)})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await new ReportRepository().getDsaEmailVerification(email)).toBeNull();
	});

	test.each<{
		kind: 'message' | 'guild';
		receipt: string;
		request: (target: ReportTarget) => Record<string, unknown>;
		reportedUserId: (target: ReportTarget) => bigint | null;
	}>([
		{
			kind: 'message',
			receipt: 'We received your Digital Services Act report about a message on Fluxer.',
			request: (target) => ({
				message_link: `https://web.fluxer.app/channels/${target.guildId}/${target.channelId}/${target.messageId}`,
				steps: [
					{screen_id: 'root_message', option_id: 'abuse'},
					{screen_id: 'abuse', option_id: 'hate'},
					{screen_id: 'hate', option_id: 'hate_incitement'},
				],
				additional_info: 'This message calls for violence against a group of people.',
			}),
			reportedUserId: (target) => BigInt(target.authorId),
		},
		{
			kind: 'guild',
			receipt: 'We received your Digital Services Act report about a community on Fluxer.',
			request: (target) => ({
				guild_id: target.guildId,
				steps: [
					{screen_id: 'community_parts', item_ids: ['activity']},
					{screen_id: 'root_guild', option_id: 'abuse'},
					{screen_id: 'abuse_guild', option_id: 'raid'},
				],
				additional_info: 'This community organizes raids on other communities.',
			}),
			reportedUserId: () => null,
		},
	])('a $kind notice is filed when SMTP refuses its receipt', async ({kind, receipt, request, reportedUserId}) => {
		const verificationSmtp = await createRecordingSmtpServer({firstDelivery: 'hold'});
		servers.add(verificationSmtp);
		verificationSmtp.release();
		await configureSmtp(verificationSmtp.port);
		const {members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const message = await sendChannelMessage(harness, members[0].token, systemChannel.id, 'Reported on the DSA form');
		const email = 'dsa-receipt-failure@example.com';
		await createBuilderWithoutAuth(harness)
			.post('/reports/dsa/email/send')
			.body({email, locale: TEST_LOCALE})
			.expect(HTTP_STATUS.OK)
			.execute();
		const verificationEmails = verificationSmtp.delivered.filter(
			(mail) => mail.to === '<dsa-receipt-failure@example.com>',
		);
		expect(verificationEmails).toHaveLength(1);
		const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
			.post('/reports/dsa/email/verify')
			.body({email, code: verificationCode(verificationEmails[0].body)})
			.expect(HTTP_STATUS.OK)
			.execute();
		const receiptSmtp = await createRecordingSmtpServer({firstDelivery: 'reject'});
		servers.add(receiptSmtp);
		await configureSmtp(receiptSmtp.port);
		const target = {
			guildId: guild.id,
			channelId: systemChannel.id,
			messageId: message.id,
			authorId: members[0].userId,
		};
		const body = {
			ticket,
			report_type: kind,
			revision_hash: getReportFlowResponse(kind, 'dsa', TEST_LOCALE).revision_hash,
			good_faith_confirmed: true,
			reporter_full_legal_name: 'Jane Doe',
			reporter_country_of_residence: 'DE',
			...request(target),
		};
		const result = await createBuilderWithoutAuth<{report_id: string; status: string}>(harness)
			.post('/reports/dsa')
			.body(body)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.status).toBe('pending');
		const stored = await new ReportRepository().getReport(createReportID(BigInt(result.report_id)));
		expect(stored?.reporterEmail).toBe('dsa-receipt-failure@example.com');
		expect(stored?.reportedGuildId?.toString()).toBe(guild.id);
		expect(stored?.reportedUserId).toBe(reportedUserId(target));
		expect(receiptSmtp.attempts).toHaveLength(1);
		expect(receiptSmtp.attempts[0]).toMatchObject({
			from: '<reports@example.com>',
			to: '<dsa-receipt-failure@example.com>',
		});
		expect(receiptSmtp.attempts[0].body).toContain(receipt);
		expect(receiptSmtp.attempts[0].body).toContain(result.report_id);
		expect(receiptSmtp.delivered).toEqual([]);
	});
});
