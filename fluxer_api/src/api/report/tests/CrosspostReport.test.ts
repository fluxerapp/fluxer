// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID, createReportID} from '@app/api/BrandedTypes';
import {type AnnouncementWorld, announcementWorld} from '@app/api/channel/tests/AnnouncementTestUtils';
import {sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {readMessageRow, writeMessageRow} from '@app/api/channel/tests/CrosspostTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, executeWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageFlags, MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface ReportResponse {
	report_id: string;
	status: string;
}

interface CopyFixture {
	world: AnnouncementWorld;
	sourceId: string;
	copyId: string;
}

async function writeCopy(params: {
	harness: ApiTestHarness;
	owner: TestAccount;
	targetChannelId: string;
	sourceGuildId: string;
	sourceChannelId: string;
	sourceMessageId: string;
	content: string;
	extraFlags?: number;
}): Promise<string> {
	const webhook = await createWebhook(params.harness, params.targetChannelId, params.owner.token, 'Follower');
	const {json} = await executeWebhook(
		params.harness,
		webhook.id,
		webhook.token,
		{content: params.content, wait: true},
		200,
	);
	const copyId = json!.id;
	const row = await readMessageRow(params.targetChannelId, copyId);
	await writeMessageRow(row!, {
		flags: MessageFlags.IS_CROSSPOST | (params.extraFlags ?? 0),
		message_reference: {
			channel_id: createChannelID(BigInt(params.sourceChannelId)),
			guild_id: createGuildID(BigInt(params.sourceGuildId)),
			message_id: createMessageID(BigInt(params.sourceMessageId)),
			type: MessageReferenceTypes.DEFAULT,
		},
	});
	return copyId;
}

async function createCopyOfMemberMessage(harness: ApiTestHarness, extraFlags?: number): Promise<CopyFixture> {
	const world = await announcementWorld(harness);
	const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'published update');
	const copyId = await writeCopy({
		harness,
		owner: world.b.owner,
		targetChannelId: world.b.t1.id,
		sourceGuildId: world.a.guild.id,
		sourceChannelId: world.a.ann.id,
		sourceMessageId: source.id,
		content: 'published update',
		extraFlags,
	});
	return {world, sourceId: source.id, copyId};
}

function reportMessage(harness: ApiTestHarness, token: string, channelId: string, messageId: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/message')
		.body({channel_id: channelId, message_id: messageId, category: 'harassment'});
}

async function readReport(reportId: string) {
	const report = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
	expect(report).not.toBeNull();
	return report!;
}

async function createDsaTicket(harness: ApiTestHarness): Promise<string> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-crosspost');
	await createBuilderWithoutAuth(harness).post('/reports/dsa/email/send').body({email}).execute();
	const dsaEmail = findLastTestEmail(await listTestEmails(harness), 'dsa_report_verification');
	const {ticket} = await createBuilder<{ticket: string}>(harness, '')
		.post('/reports/dsa/email/verify')
		.body({email, code: dsaEmail!.metadata.code})
		.expect(HTTP_STATUS.OK)
		.execute();
	return ticket;
}

function dsaMessageReport(harness: ApiTestHarness, ticket: string, channelId: string, messageId: string) {
	return createBuilderWithoutAuth<ReportResponse>(harness)
		.post('/reports/dsa')
		.body({
			ticket,
			report_type: 'message',
			category: 'harassment',
			message_link: `https://fluxer.test/channels/0/${channelId}/${messageId}`,
			reporter_full_legal_name: 'Jane Doe',
			reporter_country_of_residence: 'DE',
		});
}

describe('Reporting published copies', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('a member of the target reports a copy and the source author is reported', async () => {
		const {world, copyId} = await createCopyOfMemberMessage(harness);
		const result = await reportMessage(harness, world.b.webhookManager.token, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.reportedUserId?.toString()).toBe(world.a.member.userId);
		expect(report.reportedMessageId?.toString()).toBe(copyId);
		expect(report.reportedChannelId?.toString()).toBe(world.b.t1.id);
		expect(report.reportedGuildId?.toString()).toBe(world.b.guild.id);
		const reportedContext = report.messageContext?.find((entry) => entry.messageId.toString() === copyId);
		expect(reportedContext?.authorId.toString()).toBe(world.a.member.userId);
		expect(reportedContext?.content).toBe('published update');
	});

	test('the source author cannot report a copy of their own message', async () => {
		const world = await announcementWorld(harness);
		const source = await sendChannelMessage(harness, world.b.owner.token, world.a.ann.id, 'my own update');
		const copyId = await writeCopy({
			harness,
			owner: world.b.owner,
			targetChannelId: world.b.t1.id,
			sourceGuildId: world.a.guild.id,
			sourceChannelId: world.a.ann.id,
			sourceMessageId: source.id,
			content: 'my own update',
		});
		await reportMessage(harness, world.b.owner.token, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CANNOT_REPORT_OWN_MESSAGE)
			.execute();
	});

	test('the message-link report on a copy reports the source author', async () => {
		const {world, copyId} = await createCopyOfMemberMessage(harness);
		const ticket = await createDsaTicket(harness);
		const result = await dsaMessageReport(harness, ticket, world.b.t1.id, copyId).expect(HTTP_STATUS.OK).execute();
		const report = await readReport(result.report_id);
		expect(report.reportedUserId?.toString()).toBe(world.a.member.userId);
		expect(report.reportedMessageId?.toString()).toBe(copyId);
		expect(report.reportedChannelId?.toString()).toBe(world.b.t1.id);
	});

	test('a copy of a webhook-authored source keeps the existing errors', async () => {
		const world = await announcementWorld(harness);
		const sourceWebhook = await createWebhook(harness, world.a.ann.id, world.a.owner.token, 'Source hook');
		const {json: source} = await executeWebhook(
			harness,
			sourceWebhook.id,
			sourceWebhook.token,
			{content: 'from a webhook', wait: true},
			200,
		);
		const copyId = await writeCopy({
			harness,
			owner: world.b.owner,
			targetChannelId: world.b.t1.id,
			sourceGuildId: world.a.guild.id,
			sourceChannelId: world.a.ann.id,
			sourceMessageId: source!.id,
			content: 'from a webhook',
		});
		await reportMessage(harness, world.b.webhookManager.token, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		const ticket = await createDsaTicket(harness);
		await dsaMessageReport(harness, ticket, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_USER)
			.execute();
	});

	test('a source-deleted copy keeps the existing errors', async () => {
		const {world, copyId} = await createCopyOfMemberMessage(harness, MessageFlags.SOURCE_MESSAGE_DELETED);
		await reportMessage(harness, world.b.webhookManager.token, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		const ticket = await createDsaTicket(harness);
		await dsaMessageReport(harness, ticket, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_USER)
			.execute();
	});

	test('a copy whose source is gone keeps the existing error', async () => {
		const {world, sourceId, copyId} = await createCopyOfMemberMessage(harness);
		await createBuilder(harness, world.a.member.token)
			.delete(`/channels/${world.a.ann.id}/messages/${sourceId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await reportMessage(harness, world.b.webhookManager.token, world.b.t1.id, copyId)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
	});

	test('reports on ordinary webhook messages are unchanged', async () => {
		const world = await announcementWorld(harness);
		const webhook = await createWebhook(harness, world.b.t1.id, world.b.owner.token, 'Plain hook');
		const {json} = await executeWebhook(harness, webhook.id, webhook.token, {content: 'plain', wait: true}, 200);
		await reportMessage(harness, world.b.webhookManager.token, world.b.t1.id, json!.id)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
	});
});
