// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createReportID, createUserID} from '@app/api/BrandedTypes';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {Config, getConfig} from '@app/api/Config';
import {loadFixture} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannelInvite,
	createDmChannel,
	createPermissionOverwrite,
	createRole,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {GuildMemberRepository} from '@app/api/guild/repositories/GuildMemberRepository';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import {getInstanceConfigRepository, getRateLimitService} from '@app/api/middleware/ServiceSingletons';
import {getReportFlowVariant, type ReportFlowStepInput} from '@app/api/report/flows/ReportFlowRegistry';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {
	deleteWebhook,
	deleteWebhookMessageByToken,
	executeWebhook,
	executeWebhookWithAttachments,
} from '@app/api/webhook/tests/WebhookTestUtils';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ReportAdminResponseSchema} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {ReportFlowSurface, ReportFlowTargetType} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import type {WebhookCreateResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';
import type {z} from 'zod';

type AdminReport = z.infer<typeof ReportAdminResponseSchema>;

interface ReportResponse {
	report_id: string;
}

interface AdminReportList {
	reports: Array<AdminReport>;
	total: number;
}

type AuthorKind = 'user' | 'bot' | 'webhook';

interface CreatorProfile {
	id: string;
	username: string;
	discriminator: string;
	global_name: string | null;
	avatar: string | null;
}

interface World {
	owner: TestAccount;
	creator: CreatorProfile;
	reporter: TestAccount;
	author: TestAccount;
	outsider: TestAccount;
	guildId: string;
	channelId: string;
	webhook: WebhookCreateResponse;
	botUserId: string;
	botAppId: string;
	botToken: string;
	messages: Record<AuthorKind, string>;
}

const WEBHOOK_NAME = 'Harbor Bulletin';
const WEBHOOK_USERNAME = 'Night Desk';
const CREATOR_GLOBAL_NAME = 'Harbor Keeper';
const KINDS: ReadonlyArray<AuthorKind> = ['user', 'bot', 'webhook'];
const SPAM_WALK: ReadonlyArray<ReportFlowStepInput> = [{screen_id: 'root_message', option_id: 'spam'}];
const CSAM_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'abuse'},
	{screen_id: 'abuse', option_id: 'sexual'},
	{screen_id: 'sexual', option_id: 'minor_sexual'},
	{screen_id: 'minor_sexual', option_id: 'csam'},
];
const HARASSMENT_WALK: ReadonlyArray<ReportFlowStepInput> = [
	{screen_id: 'root_message', option_id: 'abuse'},
	{screen_id: 'abuse', option_id: 'harassment'},
	{screen_id: 'harassment', option_id: 'harassment_direct'},
];

class CapturingPublisher implements ActivityPublisher {
	readonly payloads: Array<string> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.payloads.push(payload);
	}
}

function currentHash(target: ReportFlowTargetType, surface: ReportFlowSurface = 'in_app'): string {
	return getReportFlowVariant(target, surface).revisionHash;
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

async function setJoinedAt(guildId: string, userId: string, joinedAt: Date): Promise<void> {
	const repository = new GuildMemberRepository();
	const member = await repository.getMember(createGuildID(BigInt(guildId)), createUserID(BigInt(userId)));
	if (!member) {
		throw new Error(`Member ${userId} of guild ${guildId} was not stored`);
	}
	await repository.upsertMember({...member.toRow(), joined_at: joinedAt});
}

async function createWebhookWithAvatar(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<WebhookCreateResponse> {
	return createBuilder<WebhookCreateResponse>(harness, token)
		.post(`/channels/${channelId}/webhooks`)
		.body({name: WEBHOOK_NAME, avatar: getPngDataUrl()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function sendWebhookMessage(harness: ApiTestHarness, webhook: WebhookCreateResponse, content: string) {
	const {json} = await executeWebhook(
		harness,
		webhook.id,
		webhook.token,
		{content, username: WEBHOOK_USERNAME, wait: true},
		200,
	);
	return json!.id;
}

async function setupWorld(harness: ApiTestHarness): Promise<World> {
	const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 2);
	const [reporter, author] = members;
	const channelId = systemChannel.id;
	const creator = await createBuilder<CreatorProfile>(harness, owner.token)
		.patch('/users/@me')
		.body({avatar: getPngDataUrl(), global_name: CREATOR_GLOBAL_NAME})
		.expect(HTTP_STATUS.OK)
		.execute();
	const bot = await createTestBotAccount(harness, {appName: 'Harbor Helper'});
	await authorizeBot(harness, owner.token, bot.appId, ['bot'], guild.id, '0');
	const webhook = await createWebhookWithAvatar(harness, owner.token, channelId);
	const user = await sendChannelMessage(harness, author.token, channelId, 'A message from a member');
	const botMessage = await sendChannelMessage(harness, `Bot ${bot.botToken}`, channelId, 'A message from a bot');
	const webhookMessage = await sendWebhookMessage(harness, webhook, 'A message from a webhook');
	return {
		owner,
		creator,
		reporter,
		author,
		outsider: await createTestAccount(harness),
		guildId: guild.id,
		channelId,
		webhook,
		botUserId: bot.botUserId,
		botAppId: bot.appId,
		botToken: bot.botToken,
		messages: {user: user.id, bot: botMessage.id, webhook: webhookMessage},
	};
}

async function grantManageWebhooks(harness: ApiTestHarness, world: World, userId: string): Promise<void> {
	const role = await createRole(harness, world.owner.token, world.guildId, {
		name: 'Webhook Keepers',
		permissions: Permissions.MANAGE_WEBHOOKS.toString(),
	});
	await addMemberRole(harness, world.owner.token, world.guildId, userId, role.id);
}

function submitFlow(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	steps: ReadonlyArray<ReportFlowStepInput> = SPAM_WALK,
) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/flows/message/submissions')
		.body({channel_id: channelId, message_id: messageId, revision_hash: currentHash('message'), steps});
}

function submitLegacy(harness: ApiTestHarness, token: string, channelId: string, messageId: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/message')
		.body({channel_id: channelId, message_id: messageId, category: 'spam'});
}

async function issueTicket(harness: ApiTestHarness): Promise<string> {
	await clearTestEmails(harness);
	const email = createUniqueEmail('dsa-webhook');
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

function submitDsa(harness: ApiTestHarness, ticket: string, world: World, messageId: string, extra = {}) {
	return createBuilderWithoutAuth<ReportResponse>(harness)
		.post('/reports/dsa')
		.body({
			ticket,
			report_type: 'message',
			message_link: `https://web.fluxer.app/channels/${world.guildId}/${world.channelId}/${messageId}`,
			revision_hash: currentHash('message', 'dsa'),
			steps: HARASSMENT_WALK,
			good_faith_confirmed: true,
			additional_info: 'This message targets a member of the community.',
			reporter_full_legal_name: 'Jane Doe',
			reporter_country_of_residence: 'DE',
			...extra,
		});
}

const NO_WEBHOOK = {
	reportedWebhookId: null,
	reportedWebhookName: null,
	reportedWebhookAvatarHash: null,
	reportedWebhookDefaultName: null,
	reportedWebhookDefaultAvatarHash: null,
	reportedWebhookType: null,
	reportedWebhookApplicationId: null,
	reportedWebhookChannelId: null,
	reportedWebhookGuildId: null,
	reportedWebhookCreatedAt: null,
	reportedWebhookCreatorId: null,
	reportedWebhookCreatorUsername: null,
	reportedWebhookCreatorDiscriminator: null,
	reportedWebhookCreatorGlobalName: null,
	reportedWebhookCreatorAvatarHash: null,
};

function webhookCreatedAt(webhookId: string): string {
	return snowflakeToDate(BigInt(webhookId)).toISOString();
}

function expectedAuthor(world: World, kind: AuthorKind) {
	if (kind === 'webhook') {
		return {
			reportedUserId: null,
			reportedWebhookId: world.webhook.id,
			reportedWebhookName: WEBHOOK_USERNAME,
			reportedWebhookAvatarHash: world.webhook.avatar,
			reportedWebhookDefaultName: WEBHOOK_NAME,
			reportedWebhookDefaultAvatarHash: world.webhook.avatar,
			reportedWebhookType: 1,
			reportedWebhookApplicationId: null,
			reportedWebhookChannelId: world.channelId,
			reportedWebhookGuildId: world.guildId,
			reportedWebhookCreatedAt: webhookCreatedAt(world.webhook.id),
			reportedWebhookCreatorId: world.owner.userId,
			reportedWebhookCreatorUsername: world.creator.username,
			reportedWebhookCreatorDiscriminator: Number(world.creator.discriminator),
			reportedWebhookCreatorGlobalName: CREATOR_GLOBAL_NAME,
			reportedWebhookCreatorAvatarHash: world.creator.avatar,
		};
	}
	return {
		...NO_WEBHOOK,
		reportedUserId: kind === 'user' ? world.author.userId : world.botUserId,
	};
}

function storedAuthor(report: Awaited<ReturnType<typeof readReport>>) {
	return {
		reportedUserId: report.reportedUserId?.toString() ?? null,
		reportedWebhookId: report.reportedWebhookId?.toString() ?? null,
		reportedWebhookName: report.reportedWebhookName,
		reportedWebhookAvatarHash: report.reportedWebhookAvatarHash,
		reportedWebhookDefaultName: report.reportedWebhookDefaultName,
		reportedWebhookDefaultAvatarHash: report.reportedWebhookDefaultAvatarHash,
		reportedWebhookType: report.reportedWebhookType,
		reportedWebhookApplicationId: report.reportedWebhookApplicationId?.toString() ?? null,
		reportedWebhookChannelId: report.reportedWebhookChannelId?.toString() ?? null,
		reportedWebhookGuildId: report.reportedWebhookGuildId?.toString() ?? null,
		reportedWebhookCreatedAt: report.reportedWebhookCreatedAt?.toISOString() ?? null,
		reportedWebhookCreatorId: report.reportedWebhookCreatorId?.toString() ?? null,
		reportedWebhookCreatorUsername: report.reportedWebhookCreatorUsername,
		reportedWebhookCreatorDiscriminator: report.reportedWebhookCreatorDiscriminator,
		reportedWebhookCreatorGlobalName: report.reportedWebhookCreatorGlobalName,
		reportedWebhookCreatorAvatarHash: report.reportedWebhookCreatorAvatarHash,
	};
}

async function expectStoredAuthor(reportId: string, world: World, kind: AuthorKind) {
	const report = await readReport(reportId);
	expect(storedAuthor(report)).toEqual(expectedAuthor(world, kind));
	expect(report.reportedMessageId?.toString()).toBe(world.messages[kind]);
	expect(report.reportedChannelId?.toString()).toBe(world.channelId);
	expect(report.reportedGuildId?.toString()).toBe(world.guildId);
	return report;
}

async function createAdmin(harness: ApiTestHarness): Promise<TestAccount> {
	return setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE, AdminACLs.REPORT_VIEW]);
}

describe('Reports of webhook and bot messages', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		resetActivityEventsForTests();
		await harness?.shutdown();
	});

	test('the message flow files user, bot and webhook messages', async () => {
		const world = await setupWorld(harness);
		for (const kind of KINDS) {
			const result = await submitFlow(harness, world.reporter.token, world.channelId, world.messages[kind])
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await expectStoredAuthor(result.report_id, world, kind);
			expect(report.category).toBe('spam');
			expect(report.reason).toBe('spam');
			expect(report.flowSurface).toBe('in_app');
		}
		expect(await countReports()).toBe(3);
	});

	test('the legacy route files user, bot and webhook messages', async () => {
		const world = await setupWorld(harness);
		for (const kind of KINDS) {
			const result = await submitLegacy(harness, world.reporter.token, world.channelId, world.messages[kind])
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await expectStoredAuthor(result.report_id, world, kind);
			expect(report.category).toBe('spam');
			expect(report.reason).toBeNull();
		}
	});

	test('the DSA form files user, bot and webhook messages from a message link', async () => {
		const world = await setupWorld(harness);
		for (const kind of KINDS) {
			const result = await submitDsa(harness, await issueTicket(harness), world, world.messages[kind])
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await expectStoredAuthor(result.report_id, world, kind);
			expect(report.reporterId).toBeNull();
			expect(report.category).toBe('harassment');
			expect(report.flowSurface).toBe('dsa');
		}
	});

	test('a DSA report naming a user tag for a webhook message is not found', async () => {
		const world = await setupWorld(harness);
		const ticket = await issueTicket(harness);
		const {json} = await createBuilder<{username: string; discriminator: string}>(harness, world.author.token)
			.get('/users/@me')
			.executeWithResponse();
		await submitDsa(harness, ticket, world, world.messages.webhook, {
			reported_user_tag: `${json.username}#${json.discriminator}`,
		})
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		expect(await countReports()).toBe(0);
		await submitDsa(harness, ticket, world, world.messages.webhook).expect(HTTP_STATUS.OK).execute();
	});

	test('the snapshot keeps the webhook message and its neighbors', async () => {
		const world = await setupWorld(harness);
		const result = await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		const context = new Map(report.messageContext!.map((entry) => [entry.messageId.toString(), entry]));
		const reported = context.get(world.messages.webhook)!;
		expect(reported.authorId).toBeNull();
		expect(reported.webhookId?.toString()).toBe(world.webhook.id);
		expect(reported.authorUsername).toBe(WEBHOOK_USERNAME);
		expect(reported.authorAvatarHash).toBe(world.webhook.avatar);
		expect(reported.content).toBe('A message from a webhook');
		expect(context.get(world.messages.user)?.authorId?.toString()).toBe(world.author.userId);
		expect(context.get(world.messages.bot)?.authorId?.toString()).toBe(world.botUserId);
		expect(context.get(world.messages.bot)?.webhookId).toBeNull();
		expect(report.reportedProfileSnapshot).toBeNull();
	});

	test('a bot direct message can be reported', async () => {
		const world = await setupWorld(harness);
		const dm = await createDmChannel(harness, `Bot ${world.botToken}`, world.reporter.userId);
		const message = await sendChannelMessage(harness, `Bot ${world.botToken}`, dm.id, 'Hello from a bot');
		const result = await submitFlow(harness, world.reporter.token, dm.id, message.id).expect(HTTP_STATUS.OK).execute();
		const report = await readReport(result.report_id);
		expect(report.reportedUserId?.toString()).toBe(world.botUserId);
		expect(report.reportedWebhookId).toBeNull();
		expect(report.reportedGuildId).toBeNull();
	});

	test('channel access applies to webhook messages as to user messages', async () => {
		const world = await setupWorld(harness);
		const lateJoiner = await createTestAccount(harness);
		const invite = await createChannelInvite(harness, world.owner.token, world.channelId);
		await acceptInvite(harness, lateJoiner.token, invite.code);
		await setJoinedAt(world.guildId, world.reporter.userId, snowflakeToDate(BigInt(world.guildId)));
		await setJoinedAt(world.guildId, lateJoiner.userId, new Date(Date.now() + 60_000));
		for (const account of [world.reporter, lateJoiner]) {
			await createPermissionOverwrite(harness, world.owner.token, world.channelId, account.userId, {
				type: 1,
				allow: '0',
				deny: Permissions.READ_MESSAGE_HISTORY.toString(),
			});
		}
		for (const kind of ['user', 'webhook'] as const) {
			await submitFlow(harness, lateJoiner.token, world.channelId, world.messages[kind])
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			await submitLegacy(harness, lateJoiner.token, world.channelId, world.messages[kind])
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			await submitFlow(harness, world.outsider.token, world.channelId, world.messages[kind])
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
		}
		expect(await countReports()).toBe(0);
		await submitFlow(harness, world.reporter.token, world.channelId, world.messages.user)
			.expect(HTTP_STATUS.OK)
			.execute();
		await submitLegacy(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createPermissionOverwrite(harness, world.owner.token, world.channelId, world.reporter.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await submitFlow(harness, world.reporter.token, world.channelId, world.messages.bot)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		expect(await countReports()).toBe(2);
	});

	test('webhook reports dedupe across the flow and the legacy route', async () => {
		const world = await setupWorld(harness);
		await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
			.execute();
		await submitLegacy(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
			.execute();
		const parallel = await Promise.all(
			[0, 1, 2].map(() =>
				submitFlow(harness, world.author.token, world.channelId, world.messages.webhook).executeRaw(),
			),
		);
		expect(parallel.map(({response}) => response.status).sort()).toEqual([200, 409, 409]);
		expect(await countReports()).toBe(2);
	});

	test('a rate limited webhook report releases its reservation', async () => {
		const world = await setupWorld(harness);
		for (const kind of ['user', 'bot'] as const) {
			await submitFlow(harness, world.reporter.token, world.channelId, world.messages[kind])
				.expect(HTTP_STATUS.OK)
				.execute();
		}
		const extra = await sendWebhookMessage(harness, world.webhook, 'Another webhook message');
		await submitFlow(harness, world.reporter.token, world.channelId, extra).expect(HTTP_STATUS.OK).execute();
		for (let attempt = 0; attempt < 2; attempt++) {
			await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
		}
		await getRateLimitService().resetLimit(`report:message:channel:user:${world.reporter.userId}:${world.channelId}`);
		await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await countReports()).toBe(4);
	});

	test('report_filed keeps its contract with no reported user', async () => {
		const publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		const world = await setupWorld(harness);
		const result = await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		const filed = () =>
			publisher.payloads
				.map((payload) => JSON.parse(payload) as {kind: string; data: Record<string, unknown>})
				.filter((event) => event.kind === 'report_filed');
		await vi.waitFor(() => expect(filed()).toHaveLength(1));
		const [event] = filed();
		expect(event.data).toEqual({
			report_id: result.report_id,
			reporter_id: world.reporter.userId,
			category: 'spam',
			target_type: 'message',
			reported_user_id: null,
			guild_id: world.guildId,
			message_id: world.messages.webhook,
			channel_id: world.channelId,
		});
	});

	test('a DSA report of a webhook message publishes report_filed under the zero key', async () => {
		const publisher = new CapturingPublisher();
		await startActivityEvents({publisher, kv: new MockKVProvider()});
		const world = await setupWorld(harness);
		const result = await submitDsa(harness, await issueTicket(harness), world, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
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
			category: 'harassment',
			target_type: 'dsa',
			reported_user_id: null,
			guild_id: world.guildId,
			message_id: world.messages.webhook,
			channel_id: world.channelId,
		});
	});

	test('the webhook creator tag follows the tag style of the instance', async () => {
		const config = getConfig();
		const originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			await getInstanceConfigRepository().setAccountIdentityMode(AccountIdentityModes.EMAIL, 'setup', TagStyles.NONE);
			const world = await setupWorld(harness);
			const report = await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
				.expect(HTTP_STATUS.OK)
				.execute();
			const admin = await createAdmin(harness);
			const detail = await createBuilder<AdminReport>(harness, admin.token)
				.get(`/admin/reports/${report.report_id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(detail).toMatchObject({
				reported_webhook_creator_id: world.owner.userId,
				reported_webhook_creator_tag: world.creator.username,
				reported_webhook_creator_username: world.creator.username,
				reported_webhook_creator_discriminator: '0000',
			});
		} finally {
			config.instance.selfHosted = originalSelfHosted;
			getInstanceConfigRepository().clearCacheForTesting();
		}
	});

	test('the admin API shows webhook fields and search filters by webhook', async () => {
		const world = await setupWorld(harness);
		const webhookReport = await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		const botReport = await submitFlow(harness, world.reporter.token, world.channelId, world.messages.bot)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, world.owner.token)
			.patch('/users/@me')
			.body({global_name: 'Renamed Keeper'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const admin = await createAdmin(harness);
		const detail = await createBuilder<AdminReport>(harness, admin.token)
			.get(`/admin/reports/${webhookReport.report_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(ReportAdminResponseSchema.safeParse(detail).success).toBe(true);
		expect(detail).toMatchObject({
			reported_user_id: null,
			reported_user_tag: null,
			reported_webhook_id: world.webhook.id,
			reported_webhook_name: WEBHOOK_USERNAME,
			reported_webhook_avatar_hash: world.webhook.avatar,
			reported_webhook_default_name: WEBHOOK_NAME,
			reported_webhook_default_avatar_hash: world.webhook.avatar,
			reported_webhook_type: 1,
			reported_webhook_application_id: null,
			reported_webhook_channel_id: world.channelId,
			reported_webhook_guild_id: world.guildId,
			reported_webhook_created_at: webhookCreatedAt(world.webhook.id),
			reported_webhook_creator_id: world.owner.userId,
			reported_webhook_creator_tag: `${world.creator.username}#${world.creator.discriminator.padStart(4, '0')}`,
			reported_webhook_creator_username: world.creator.username,
			reported_webhook_creator_global_name: CREATOR_GLOBAL_NAME,
			reported_webhook_creator_discriminator: world.creator.discriminator.padStart(4, '0'),
			reported_webhook_creator_avatar_hash: world.creator.avatar,
			reported_message_id: world.messages.webhook,
			reported_channel_id: world.channelId,
			reported_guild_id: world.guildId,
		});
		const reported = detail.message_context!.find((entry) => entry.id === world.messages.webhook)!;
		expect(reported).toMatchObject({
			author_id: world.webhook.id,
			author_username: WEBHOOK_USERNAME,
			webhook_id: world.webhook.id,
		});
		const neighbor = detail.message_context!.find((entry) => entry.id === world.messages.user)!;
		expect(neighbor).toMatchObject({author_id: world.author.userId, webhook_id: null});
		const bot = await createBuilder<AdminReport>(harness, admin.token)
			.get(`/admin/reports/${botReport.report_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(bot).toMatchObject({
			reported_user_id: world.botUserId,
			reported_webhook_id: null,
			reported_webhook_name: null,
			reported_webhook_default_name: null,
			reported_webhook_type: null,
			reported_webhook_created_at: null,
			reported_webhook_creator_id: null,
			reported_webhook_creator_tag: null,
		});
		const list = await createBuilder<AdminReportList>(harness, admin.token)
			.get(`/admin/reports?reported_webhook_id=${world.webhook.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(list.reports.map((report) => report.report_id)).toEqual([webhookReport.report_id]);
		expect(list.reports[0]).toMatchObject({
			reported_webhook_id: world.webhook.id,
			reported_user_id: null,
			reported_webhook_creator_id: world.owner.userId,
			reported_webhook_creator_global_name: CREATOR_GLOBAL_NAME,
		});
		const free = await createBuilder<AdminReportList>(harness, admin.token)
			.get(`/admin/reports?q=${world.webhook.id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(free.reports.map((report) => report.report_id)).toContain(webhookReport.report_id);
	});

	test('a webhook whose creator deleted their account keeps the creator id without a profile', async () => {
		const world = await setupWorld(harness);
		await grantManageWebhooks(harness, world, world.author.userId);
		const orphan = await createBuilder<WebhookCreateResponse>(harness, world.author.token)
			.post(`/channels/${world.channelId}/webhooks`)
			.body({name: 'Ghost Relay'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const messageId = await sendWebhookMessage(harness, orphan, 'A message from an orphaned webhook');
		await deleteAccount(harness, world.author.token, world.author.password);
		await setPendingDeletionAt(harness, world.author.userId, new Date(Date.now() - 60_000));
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, world.author.userId);
		const flow = await submitFlow(harness, world.reporter.token, world.channelId, messageId)
			.expect(HTTP_STATUS.OK)
			.execute();
		const dsa = await submitDsa(harness, await issueTicket(harness), world, messageId)
			.expect(HTTP_STATUS.OK)
			.execute();
		const expected = {
			...NO_WEBHOOK,
			reportedUserId: null,
			reportedWebhookId: orphan.id,
			reportedWebhookName: WEBHOOK_USERNAME,
			reportedWebhookDefaultName: 'Ghost Relay',
			reportedWebhookType: 1,
			reportedWebhookChannelId: world.channelId,
			reportedWebhookGuildId: world.guildId,
			reportedWebhookCreatedAt: webhookCreatedAt(orphan.id),
			reportedWebhookCreatorId: world.author.userId,
		};
		for (const reportId of [flow.report_id, dsa.report_id]) {
			expect(storedAuthor(await readReport(reportId))).toEqual(expected);
		}
		const admin = await createAdmin(harness);
		const detail = await createBuilder<AdminReport>(harness, admin.token)
			.get(`/admin/reports/${flow.report_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(ReportAdminResponseSchema.safeParse(detail).success).toBe(true);
		expect(detail).toMatchObject({
			reported_user_id: null,
			reported_webhook_id: orphan.id,
			reported_webhook_creator_id: world.author.userId,
			reported_webhook_creator_tag: null,
			reported_webhook_creator_username: null,
			reported_webhook_creator_global_name: null,
			reported_webhook_creator_discriminator: null,
			reported_webhook_creator_avatar_hash: null,
		});
	}, 120_000);

	test('a webhook created by a bot records the bot application', async () => {
		const world = await setupWorld(harness);
		await grantManageWebhooks(harness, world, world.botUserId);
		const botWebhook = await createBuilder<WebhookCreateResponse>(harness, `Bot ${world.botToken}`)
			.post(`/channels/${world.channelId}/webhooks`)
			.body({name: 'Helper Relay'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const messageId = await sendWebhookMessage(harness, botWebhook, 'A message from a bot webhook');
		const result = await submitLegacy(harness, world.reporter.token, world.channelId, messageId)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(storedAuthor(report)).toMatchObject({
			reportedUserId: null,
			reportedWebhookId: botWebhook.id,
			reportedWebhookDefaultName: 'Helper Relay',
			reportedWebhookCreatorId: world.botUserId,
			reportedWebhookCreatorUsername: expect.any(String),
			reportedWebhookApplicationId: world.botAppId,
		});
	});

	test('a deleted webhook keeps the message snapshot and drops the webhook context', async () => {
		const world = await setupWorld(harness);
		await deleteWebhook(harness, world.webhook.id, world.owner.token);
		const result = await submitFlow(harness, world.reporter.token, world.channelId, world.messages.webhook)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(storedAuthor(await readReport(result.report_id))).toEqual({
			...NO_WEBHOOK,
			reportedUserId: null,
			reportedWebhookId: world.webhook.id,
			reportedWebhookName: WEBHOOK_USERNAME,
			reportedWebhookAvatarHash: world.webhook.avatar,
			reportedWebhookCreatedAt: webhookCreatedAt(world.webhook.id),
		});
	});

	test('webhook attachments are kept as evidence after the message is deleted', async () => {
		const world = await setupWorld(harness);
		const {json: message} = await executeWebhookWithAttachments(harness, {
			webhookId: world.webhook.id,
			webhookToken: world.webhook.token,
			payload: {content: 'webhook evidence', attachments: [{id: 0, filename: 'evidence.png'}]},
			files: [{index: 0, filename: 'evidence.png', data: loadFixture('yeah.png')}],
		});
		const attachment = message!.attachments![0]!;
		const result = await submitFlow(harness, world.reporter.token, world.channelId, message!.id, CSAM_WALK)
			.expect(HTTP_STATUS.OK)
			.execute();
		const report = await readReport(result.report_id);
		expect(report.category).toBe('child_safety');
		const reported = report.messageContext!.find((entry) => entry.messageId.toString() === message!.id)!;
		expect(reported.attachments.map((entry) => entry.filename)).toEqual(['evidence.png']);
		const key = `attachments/${world.channelId}/${attachment.id}/evidence.png`;
		expect(harness.storageService.hasObject(Config.s3.buckets.reports, key)).toBe(true);
		await deleteWebhookMessageByToken(harness, world.webhook.id, world.webhook.token, message!.id);
		expect(harness.storageService.hasObject(Config.s3.buckets.reports, key)).toBe(true);
		const admin = await createAdmin(harness);
		const detail = await createBuilder<AdminReport>(harness, admin.token)
			.get(`/admin/reports/${result.report_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const evidence = detail.message_context!.find((entry) => entry.id === message!.id)!;
		expect(evidence.attachments.map((entry) => ({id: entry.id, filename: entry.filename}))).toEqual([
			{id: attachment.id, filename: 'evidence.png'},
		]);
	});
});
