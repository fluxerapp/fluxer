// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	createChannel,
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	createPermissionOverwrite,
	getChannel,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {
	getReportFlowVariant,
	type ReportFlowStepInput,
	resolveReportFlowAnswers,
} from '@app/api/report/flows/ReportFlowRegistry';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ReportFlowResponse, ReportFlowTargetType} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface ReportResponse {
	report_id: string;
	status: string;
	reported_at: string;
}

interface ErrorResponse {
	code: string;
	step_index?: number;
	errors?: Array<{path: string; code: string}>;
}

interface Ticket {
	email: string;
	ticket: string;
}

const HATE_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'abuse'},
	{screen_id: 'abuse', option_id: 'hate'},
	{screen_id: 'hate', option_id: 'hate_incitement'},
];

const HARASSMENT_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'abuse'},
	{screen_id: 'abuse', option_id: 'harassment'},
	{screen_id: 'harassment', option_id: 'harassment_direct'},
];

const CSAM_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'abuse'},
	{screen_id: 'abuse', option_id: 'sexual'},
	{screen_id: 'sexual', option_id: 'minor_sexual'},
	{screen_id: 'minor_sexual', option_id: 'csam'},
];

const RAID_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'community_parts', item_ids: ['activity']},
	{screen_id: 'root_guild', option_id: 'abuse'},
	{screen_id: 'abuse_guild', option_id: 'raid'},
];

class CapturingPublisher implements ActivityPublisher {
	readonly payloads: Array<string> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.payloads.push(payload);
	}
}

function dsaHash(target: ReportFlowTargetType): string {
	return getReportFlowVariant(target, 'dsa').revisionHash;
}

function dsaSteps(target: ReportFlowTargetType, steps: ReadonlyArray<ReportFlowStepInput>) {
	return resolveReportFlowAnswers({target, surface: 'dsa', revisionHash: dsaHash(target), steps}).steps;
}

async function readReport(reportId: string) {
	const report = await new ReportRepository().getReport(createReportID(BigInt(reportId)));
	if (!report) {
		throw new Error(`Report ${reportId} was not stored`);
	}
	return report;
}

async function countReports(): Promise<number> {
	return (await new ReportRepository().listAllReportsPaginated(100)).length;
}

async function issueTicket(harness: ApiTestHarness): Promise<Ticket> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-flow');
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
	return {email, ticket};
}

async function setupMessageLink(harness: ApiTestHarness): Promise<{link: string; authorId: string; guildId: string}> {
	const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
	const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
	const message = await sendChannelMessage(harness, members[0].token, channel.id, 'Reported on the DSA form');
	return {
		link: `https://web.fluxer.app/channels/${guild.id}/${channel.id}/${message.id}`,
		authorId: members[0].userId,
		guildId: guild.id,
	};
}

function messageFlowBody(ticket: string, link: string, steps: ReadonlyArray<ReportFlowStepInput>, extra = {}) {
	return {
		ticket,
		report_type: 'message',
		message_link: link,
		revision_hash: dsaHash('message'),
		steps,
		good_faith_confirmed: true,
		additional_info: 'This message calls for violence against a group of people.',
		reporter_full_legal_name: 'Jane Doe',
		reporter_country_of_residence: 'DE',
		...extra,
	};
}

function submitDsa<T = ReportResponse>(harness: ApiTestHarness, body: unknown) {
	return createBuilderWithoutAuth<T>(harness).post('/reports/dsa').body(body);
}

function guildFlowBody(ticket: string, guildId: string) {
	return {
		ticket,
		report_type: 'guild',
		guild_id: guildId,
		revision_hash: dsaHash('guild'),
		steps: RAID_WALK,
		good_faith_confirmed: true,
		additional_info: 'This community organizes raids on other communities.',
		reporter_full_legal_name: 'Jane Doe',
		reporter_country_of_residence: 'NL',
	};
}

async function listReceipts(harness: ApiTestHarness) {
	return (await listTestEmails(harness)).filter((sent) => sent.type === 'report_received');
}

async function fetchTag(harness: ApiTestHarness, token: string): Promise<string> {
	const me = await createBuilder<{username: string; discriminator: string}>(harness, token)
		.get('/users/@me')
		.expect(HTTP_STATUS.OK)
		.execute();
	return `${me.username}#${me.discriminator}`;
}

async function sendThree(harness: ApiTestHarness, token: string, channelId: string): Promise<Array<string>> {
	const ids: Array<string> = [];
	for (const content of ['Before the reported message', 'The reported message', 'After the reported message']) {
		ids.push((await sendChannelMessage(harness, token, channelId, content)).id);
	}
	return ids;
}

async function storedContextIds(reportId: string): Promise<Array<string>> {
	const report = await readReport(reportId);
	return (report.messageContext ?? []).map((entry) => entry.messageId.toString());
}

describe('DSA report flow', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		vi.restoreAllMocks();
		resetActivityEventsForTests();
		await harness?.shutdown();
	});

	test('a verified reporter files a message notice from the DSA flow', async () => {
		const {email, ticket} = await issueTicket(harness);
		const flow = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
			.get('/reports/flows/message?surface=dsa&locale=de')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(flow.surface).toBe('dsa');
		expect(flow.revision_hash).toBe(dsaHash('message'));
		const target = await setupMessageLink(harness);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, target.link, HATE_WALK, {revision_hash: flow.revision_hash, locale: 'de'}),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.status).toBe('pending');
		const report = await readReport(result.report_id);
		expect(report.reason).toBe('hate_incitement');
		expect(report.category).toBe('hate_speech');
		expect(report.flowSteps).toEqual(dsaSteps('message', HATE_WALK));
		expect(report.flowRevision).toBe(flow.revision_hash);
		expect(report.flowLocale).toBe('de');
		expect(report.flowSurface).toBe('dsa');
		expect(report.reporterGoodFaithConfirmed).toBe(true);
		expect(report.reporterFullLegalName).toBe('Jane Doe');
		expect(report.reporterEmail).toBe(email);
		expect(report.reporterId).toBeNull();
		expect(report.reportedUserId?.toString()).toBe(target.authorId);
	});

	test('steps need the good-faith statement and an explanation', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const valid = messageFlowBody(ticket, target.link, HATE_WALK);
		const {good_faith_confirmed: _goodFaith, ...withoutGoodFaith} = valid;
		const {revision_hash: _hash, ...withoutHash} = valid;
		const cases: Array<[unknown, string]> = [
			[withoutGoodFaith, 'good_faith_confirmed'],
			[{...valid, good_faith_confirmed: false}, 'good_faith_confirmed'],
			[{...valid, additional_info: ''}, 'additional_info'],
			[{...valid, additional_info: '   '}, 'additional_info'],
			[withoutHash, 'revision_hash'],
		];
		for (const [body, path] of cases) {
			const error = await submitDsa<ErrorResponse>(harness, body)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			expect(
				error.errors?.map((entry) => entry.path),
				path,
			).toContain(path);
		}
		await submitDsa(harness, valid).expect(HTTP_STATUS.OK).execute();
	});

	test('the legal name is required unless the walk is about child sexual abuse', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const error = await submitDsa<ErrorResponse>(
			harness,
			messageFlowBody(ticket, target.link, HARASSMENT_WALK, {reporter_full_legal_name: undefined}),
		)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		expect(error.errors?.map((entry) => entry.path)).toEqual(['reporter_full_legal_name']);
		expect(await countReports()).toBe(0);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, target.link, CSAM_WALK, {reporter_full_legal_name: undefined}),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.reporterFullLegalName).toBeNull();
		expect(report.reason).toBe('csam');
		expect(report.category).toBe('child_safety');
	});

	test('a legacy body is stored as before', async () => {
		const {ticket} = await issueTicket(harness);
		const user = await createTestAccount(harness);
		const result = await submitDsa(harness, {
			ticket,
			report_type: 'user',
			category: 'harassment',
			user_id: user.userId,
			reporter_full_legal_name: 'John Doe',
			reporter_country_of_residence: 'SE',
			additional_info: 'Legacy notice',
		})
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.category).toBe('harassment');
		expect(report.additionalInfo).toBe('Legacy notice');
		expect(report.reporterFullLegalName).toBe('John Doe');
		expect(report.reason).toBeNull();
		expect(report.flowRevision).toBeNull();
		expect(report.flowSteps).toBeNull();
		expect(report.flowLocale).toBeNull();
		expect(report.flowSurface).toBeNull();
		expect(report.reporterGoodFaithConfirmed).toBeNull();
	});

	test('a legacy body still needs its category and legal name', async () => {
		const {ticket} = await issueTicket(harness);
		const user = await createTestAccount(harness);
		const legacy = {
			ticket,
			report_type: 'user',
			category: 'harassment',
			user_id: user.userId,
			reporter_full_legal_name: 'John Doe',
			reporter_country_of_residence: 'SE',
		};
		const {category: _category, ...withoutCategory} = legacy;
		const {reporter_full_legal_name: _name, ...withoutName} = legacy;
		for (const [body, path] of [
			[withoutCategory, 'category'],
			[withoutName, 'reporter_full_legal_name'],
		] as const) {
			const error = await submitDsa<ErrorResponse>(harness, body)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			expect(error.errors?.map((entry) => entry.path)).toContain(path);
		}
		await submitDsa(harness, legacy).expect(HTTP_STATUS.OK).execute();
	});

	test('a category sent next to the steps is ignored', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const result = await submitDsa(harness, messageFlowBody(ticket, target.link, HATE_WALK, {category: 'spam'}))
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.category).toBe('hate_speech');
		expect(report.reason).toBe('hate_incitement');
	});

	test('a bad walk keeps the ticket', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const inAppOnly = await submitDsa<ErrorResponse>(
			harness,
			messageFlowBody(ticket, target.link, [{screen_id: 'root_message', option_id: 'dislike'}]),
		)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
			.execute();
		expect(inAppOnly.step_index).toBe(0);
		const welfare = await submitDsa<ErrorResponse>(
			harness,
			messageFlowBody(ticket, target.link, [
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'self_harm'},
				{screen_id: 'self_harm', option_id: 'worried_self_harm'},
			]),
		)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
			.execute();
		expect(welfare.step_index).toBe(2);
		const inAppHash = getReportFlowVariant('message', 'in_app').revisionHash;
		await submitDsa(
			harness,
			messageFlowBody(ticket, target.link, [{screen_id: 'root_message', option_id: 'dislike'}], {
				revision_hash: inAppHash,
			}),
		)
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.REPORT_FLOW_OUTDATED)
			.execute();
		expect(await countReports()).toBe(0);
		await submitDsa(harness, messageFlowBody(ticket, target.link, HATE_WALK))
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	test('a community notice stores the raid reason', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const result = await submitDsa(harness, guildFlowBody(ticket, target.guildId)).expect(HTTP_STATUS.OK).execute();
		const report = await readReport(result.report_id);
		expect(report.reason).toBe('raid');
		expect(report.category).toBe('raid_coordination');
		expect(report.flowSteps).toEqual(dsaSteps('guild', RAID_WALK));
		expect(report.reportedGuildId?.toString()).toBe(target.guildId);
	});

	test('a community notice publishes report_filed under the zero key', async () => {
		const publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const result = await submitDsa(harness, guildFlowBody(ticket, target.guildId)).expect(HTTP_STATUS.OK).execute();
		const filed = () =>
			publisher.payloads
				.map((payload) => JSON.parse(payload) as {kind: string; key: string; data: Record<string, unknown>})
				.filter((event) => event.kind === 'report_filed');
		await vi.waitFor(() => expect(filed()).toHaveLength(1));
		const [event] = filed();
		expect(event.key).toBe('0');
		expect(event.data).toEqual({
			report_id: result.report_id,
			reporter_id: '0',
			category: 'raid_coordination',
			target_type: 'dsa',
			reported_user_id: null,
			guild_id: target.guildId,
			message_id: null,
			channel_id: null,
		});
	});

	test('a reporter username sent by an older form is ignored', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const account = await createTestAccount(harness);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, target.link, HATE_WALK, {reporter_fluxer_tag: await fetchTag(harness, account.token)}),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.reporterId).toBeNull();
		expect(report.reportedUserId?.toString()).toBe(target.authorId);
	});

	test('a filed notice sends one receipt to the verified address', async () => {
		const account = await createTestAccount(harness);
		const target = await setupMessageLink(harness);
		const bodies: Array<[string, (ticket: string) => unknown]> = [
			['message', (ticket) => messageFlowBody(ticket, target.link, HATE_WALK)],
			['guild', (ticket) => guildFlowBody(ticket, target.guildId)],
			[
				'user',
				(ticket) => ({
					ticket,
					report_type: 'user',
					category: 'harassment',
					user_id: account.userId,
					reporter_full_legal_name: 'John Doe',
					reporter_country_of_residence: 'SE',
				}),
			],
		];
		for (const [kind, body] of bodies) {
			const {email, ticket} = await issueTicket(harness);
			expect(await listReceipts(harness)).toEqual([]);
			const result = await submitDsa(harness, body(ticket)).expect(HTTP_STATUS.OK).execute();
			const receipts = await listReceipts(harness);
			expect(receipts).toHaveLength(1);
			expect(receipts[0].to).toBe(email.toLowerCase());
			expect(receipts[0].metadata).toEqual({report_id: result.report_id, target_kind: kind});
		}
	});

	test('a refused notice sends no receipt', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		await submitDsa(harness, messageFlowBody(ticket, target.link, [{screen_id: 'root_message', option_id: 'dislike'}]))
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
			.execute();
		expect(await listReceipts(harness)).toEqual([]);
	});

	test('a notice is filed even when its receipt cannot be sent', async () => {
		const target = await setupMessageLink(harness);
		const send = vi.spyOn(TestEmailService.prototype, 'sendReportReceivedEmail');
		send.mockRejectedValueOnce(new Error('mail transport is down'));
		const first = await issueTicket(harness);
		const thrown = await submitDsa(harness, messageFlowBody(first.ticket, target.link, HATE_WALK))
			.expect(HTTP_STATUS.OK)
			.execute();
		send.mockResolvedValueOnce(false);
		const second = await issueTicket(harness);
		const refused = await submitDsa(harness, guildFlowBody(second.ticket, target.guildId))
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(send).toHaveBeenCalledTimes(2);
		expect((await readReport(thrown.report_id)).reporterEmail).toBe(first.email);
		expect((await readReport(refused.report_id)).reporterEmail).toBe(second.email);
		expect(await listReceipts(harness)).toEqual([]);
	});

	test('every message link that does not resolve gets the same answer and keeps the ticket', async () => {
		const {ticket} = await issueTicket(harness);
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const other = await setupTestGuildWithMembers(harness, 0);
		const sideChannel = await createChannel(harness, owner.token, guild.id, 'side');
		const message = await sendChannelMessage(harness, members[0].token, systemChannel.id, 'Reported on the DSA form');
		const otherMessage = await sendChannelMessage(harness, other.owner.token, other.systemChannel.id, 'Elsewhere');
		await createFriendship(harness, members[0], owner);
		const dm = await createDmChannel(harness, members[0].token, owner.userId);
		const dmMessage = await sendChannelMessage(harness, members[0].token, dm.id, 'A direct message');
		const link = (guildSegment: string, channelId: string, messageId: string) =>
			`https://web.fluxer.app/channels/${guildSegment}/${channelId}/${messageId}`;
		const valid = link(guild.id, systemChannel.id, message.id);
		const cases: Array<[string, Record<string, unknown>]> = [
			['unknown channel', {message_link: link(guild.id, TEST_IDS.NONEXISTENT_CHANNEL, message.id)}],
			['unknown message', {message_link: link(guild.id, systemChannel.id, TEST_IDS.NONEXISTENT_MESSAGE)}],
			['message of another channel', {message_link: link(guild.id, sideChannel.id, message.id)}],
			['message of another community', {message_link: link(guild.id, systemChannel.id, otherMessage.id)}],
			['another community in the link', {message_link: link(other.guild.id, systemChannel.id, message.id)}],
			['a direct message link for a community message', {message_link: link('@me', systemChannel.id, message.id)}],
			['a community link for a direct message', {message_link: link(guild.id, dm.id, dmMessage.id)}],
			['a placeholder community in the link', {message_link: link('0', systemChannel.id, message.id)}],
			['an unknown author name', {message_link: valid, reported_user_tag: 'nobody-by-this-name#0001'}],
			['another author name', {message_link: valid, reported_user_tag: await fetchTag(harness, owner.token)}],
		];
		const answers = new Set<string>();
		for (const [name, extra] of cases) {
			const {response, text} = await submitDsa(harness, messageFlowBody(ticket, valid, HATE_WALK, extra)).executeRaw();
			expect(response.status, name).toBe(HTTP_STATUS.NOT_FOUND);
			expect((JSON.parse(text) as ErrorResponse).code, name).toBe(APIErrorCodes.UNKNOWN_MESSAGE);
			answers.add(text);
		}
		expect(answers.size).toBe(1);
		expect(await countReports()).toBe(0);
		expect(await listReceipts(harness)).toEqual([]);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, valid, HATE_WALK, {reported_user_tag: await fetchTag(harness, members[0].token)}),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect((await readReport(result.report_id)).reportedUserId?.toString()).toBe(members[0].userId);
	});

	test('a message whose author account cannot be loaded gets the same answer and keeps the ticket', async () => {
		const {ticket} = await issueTicket(harness);
		const target = await setupMessageLink(harness);
		const missing = await submitDsa(
			harness,
			messageFlowBody(ticket, target.link.replace(/\d+$/, TEST_IDS.NONEXISTENT_MESSAGE), HATE_WALK),
		).executeRaw();
		const findUnique = vi.spyOn(UserRepository.prototype, 'findUnique').mockResolvedValueOnce(null);
		const {response, text} = await submitDsa(harness, messageFlowBody(ticket, target.link, HATE_WALK)).executeRaw();
		expect(findUnique).toHaveBeenCalledTimes(1);
		expect(response.status).toBe(HTTP_STATUS.NOT_FOUND);
		expect((JSON.parse(text) as ErrorResponse).code).toBe(APIErrorCodes.UNKNOWN_MESSAGE);
		expect(text).toBe(missing.text);
		expect(await countReports()).toBe(0);
		await submitDsa(harness, messageFlowBody(ticket, target.link, HATE_WALK))
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	test('a notice about a public channel keeps the surrounding messages', async () => {
		const {ticket} = await issueTicket(harness);
		const {members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const ids = await sendThree(harness, members[0].token, systemChannel.id);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, `https://web.fluxer.app/channels/${guild.id}/${systemChannel.id}/${ids[1]}`, HATE_WALK),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect((await storedContextIds(result.report_id)).slice(-3)).toEqual(ids);
	});

	test('a notice about a channel that is not open to every member keeps the reported message only', async () => {
		const {owner, guild, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const restrictions: Array<[string, bigint]> = [
			['hidden', Permissions.VIEW_CHANNEL],
			['no-history', Permissions.READ_MESSAGE_HISTORY],
		];
		for (const [name, denied] of restrictions) {
			const channel = await createChannel(harness, owner.token, guild.id, name);
			await createPermissionOverwrite(harness, owner.token, channel.id, guild.id, {
				type: 0,
				allow: '0',
				deny: denied.toString(),
			});
			const [, target] = await sendThree(harness, owner.token, channel.id);
			const {ticket} = await issueTicket(harness);
			const result = await submitDsa(
				harness,
				messageFlowBody(ticket, `https://web.fluxer.app/channels/${guild.id}/${channel.id}/${target}`, HATE_WALK),
			)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(await storedContextIds(result.report_id), name).toEqual([target]);
		}
		const open = await sendThree(harness, owner.token, systemChannel.id);
		const {ticket} = await issueTicket(harness);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, `https://web.fluxer.app/channels/${guild.id}/${systemChannel.id}/${open[1]}`, HATE_WALK),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await storedContextIds(result.report_id)).toEqual(open);
	});

	test('a notice about a direct message uses the @me link and keeps the reported message only', async () => {
		const {owner, members} = await setupTestGuildWithMembers(harness, 1);
		await createFriendship(harness, members[0], owner);
		const dm = await createDmChannel(harness, members[0].token, owner.userId);
		const [, target] = await sendThree(harness, members[0].token, dm.id);
		const {ticket} = await issueTicket(harness);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, `https://web.fluxer.app/channels/@me/${dm.id}/${target}`, HATE_WALK),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.reportedGuildId).toBeNull();
		expect(report.reportedChannelId?.toString()).toBe(dm.id);
		expect(report.reportedUserId?.toString()).toBe(members[0].userId);
		expect(report.messageContext?.map((entry) => entry.messageId.toString())).toEqual([target]);
	});

	test('a notice about a group direct message uses the @me link and keeps the reported message only', async () => {
		const {owner, members} = await setupTestGuildWithMembers(harness, 2);
		await createFriendship(harness, owner, members[0]);
		await createFriendship(harness, owner, members[1]);
		const group = await createGroupDmChannel(harness, owner.token, [members[0].userId, members[1].userId]);
		const [, target] = await sendThree(harness, members[0].token, group.id);
		const {ticket} = await issueTicket(harness);
		await submitDsa(
			harness,
			messageFlowBody(ticket, `https://web.fluxer.app/channels/${group.id}/${group.id}/${target}`, HATE_WALK),
		)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, `https://web.fluxer.app/channels/@me/${group.id}/${target}`, HATE_WALK),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.reportedGuildId).toBeNull();
		expect(report.reportedChannelId?.toString()).toBe(group.id);
		expect(report.reportedUserId?.toString()).toBe(members[0].userId);
		expect(report.messageContext?.map((entry) => entry.messageId.toString())).toEqual([target]);
	});

	test('the reported message keeps its attachments when the surrounding messages are left out', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'hidden');
		await createPermissionOverwrite(harness, owner.token, channel.id, guild.id, {
			type: 0,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await sendChannelMessage(harness, owner.token, channel.id, 'Before the reported message');
		const {json: message} = await sendMessageWithAttachments(
			harness,
			owner.token,
			channel.id,
			{content: 'The reported message', attachments: [{id: 0, filename: 'evidence.png'}]},
			[{index: 0, filename: 'evidence.png', data: loadFixture('yeah.png')}],
		);
		const {ticket} = await issueTicket(harness);
		const result = await submitDsa(
			harness,
			messageFlowBody(ticket, `https://web.fluxer.app/channels/${guild.id}/${channel.id}/${message.id}`, HATE_WALK),
		)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.messageContext).toHaveLength(1);
		expect(report.messageContext![0].attachments.map((entry) => entry.filename)).toEqual(['evidence.png']);
		const key = `attachments/${channel.id}/${message.attachments![0]!.id}/evidence.png`;
		expect(harness.storageService.hasObject(Config.s3.buckets.reports, key)).toBe(true);
	});

	test('the notice text is not content screened', async () => {
		phraseBlocklistCache.add('blockedslur');
		urlBlocklistCache.addDomain('blocked-phish.example');
		try {
			const {ticket} = await issueTicket(harness);
			const target = await setupMessageLink(harness);
			const text = 'They wrote blockedslur and posted https://blocked-phish.example/login to steal accounts.';
			const result = await submitDsa(harness, messageFlowBody(ticket, target.link, HATE_WALK, {additional_info: text}))
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await readReport(result.report_id)).additionalInfo).toBe(text);
		} finally {
			phraseBlocklistCache.remove('blockedslur');
			urlBlocklistCache.removeDomain('blocked-phish.example');
		}
	});
});
