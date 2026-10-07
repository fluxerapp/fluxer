// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID} from '@app/api/BrandedTypes';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {getConfig} from '@app/api/Config';
import {
	createChannel,
	createDmChannel,
	createFriendship,
	getChannel,
	leaveGuild,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {resetActivityEventsForTests, startActivityEvents} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ActivityPublisher} from '@app/api/infrastructure/activity/ActivitySpool';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {getRateLimitService} from '@app/api/middleware/ServiceSingletons';
import {
	getReportFlowResponse,
	getReportFlowVariant,
	type ReportFlowStepInput,
	resolveReportFlowAnswers,
} from '@app/api/report/flows/ReportFlowRegistry';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {
	ReportFlowResponse,
	type ReportFlowSurface,
	type ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
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

interface MessageTarget {
	reporter: TestAccount;
	author: TestAccount;
	guildId: string;
	channelId: string;
	messageId: string;
}

interface Walk {
	name: string;
	steps: ReadonlyArray<ReportFlowStepInput>;
	reason: string;
	category: string;
}

const HAS_SESSION_STARTED = 1n << 39n;
const RATE_LIMIT_HEADER = 'x-fluxer-test-enable-rate-limits';

const VARIANTS: ReadonlyArray<readonly [ReportFlowTargetType, ReportFlowSurface]> = [
	['message', 'in_app'],
	['user', 'in_app'],
	['message', 'dsa'],
	['user', 'dsa'],
	['guild', 'dsa'],
];

const MESSAGE_WALKS: ReadonlyArray<Walk> = [
	{name: 'S3 spam', steps: [{screen_id: 'root_message', option_id: 'spam'}], reason: 'spam', category: 'spam'},
	{
		name: 'S6 private information',
		steps: [
			{screen_id: 'root_message', option_id: 'private_info'},
			{screen_id: 'private_info', item_ids: ['phone', 'email']},
		],
		reason: 'doxxing',
		category: 'doxxing',
	},
	{
		name: 'S6 intimate photo override',
		steps: [
			{screen_id: 'root_message', option_id: 'private_info'},
			{screen_id: 'private_info', item_ids: ['face_photo', 'intimate_photo']},
		],
		reason: 'intimate_image_abuse',
		category: 'doxxing',
	},
	{
		name: 'S8 S9 age stated',
		steps: [
			{screen_id: 'root_message', option_id: 'something_else'},
			{screen_id: 'something_else_message', option_id: 'too_young'},
			{screen_id: 'age_stated_message', option_id: 'age_yes'},
		],
		reason: 'underage',
		category: 'underage_user',
	},
	{
		name: 'S11 S12 self-harm worry',
		steps: [
			{screen_id: 'root_message', option_id: 'something_else'},
			{screen_id: 'something_else_message', option_id: 'self_harm'},
			{screen_id: 'self_harm', option_id: 'worried_self_harm'},
		],
		reason: 'wellbeing_concern',
		category: 'self_harm',
	},
	{
		name: 'S14 harmful false claims',
		steps: [
			{screen_id: 'root_message', option_id: 'violence_misinfo'},
			{screen_id: 'violence_misinfo', option_id: 'false_info'},
			{screen_id: 'false_info', option_id: 'harmful_false_claims'},
		],
		reason: 'harmful_false_claims',
		category: 'other',
	},
	{
		name: 'S15 fraud',
		steps: [
			{screen_id: 'root_message', option_id: 'something_else'},
			{screen_id: 'something_else_message', option_id: 'impersonation'},
			{screen_id: 'impersonation', option_id: 'fraud'},
		],
		reason: 'fraud',
		category: 'illegal_activity',
	},
	{
		name: 'S18 to S21 csam',
		steps: [
			{screen_id: 'root_message', option_id: 'abuse'},
			{screen_id: 'abuse', option_id: 'sexual'},
			{screen_id: 'sexual', option_id: 'minor_sexual'},
			{screen_id: 'minor_sexual', option_id: 'csam'},
		],
		reason: 'csam',
		category: 'child_safety',
	},
	{
		name: 'S1 to S5 terrorism',
		steps: [
			{screen_id: 'root_message', option_id: 'violence_misinfo'},
			{screen_id: 'violence_misinfo', option_id: 'terrorism'},
		],
		reason: 'terrorism_extremism',
		category: 'violent_content',
	},
];

const USER_WALKS: ReadonlyArray<Walk> = [
	{
		name: 'P1 to P3 harassment',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['photo', 'profile_text']},
			{screen_id: 'root_user', option_id: 'abuse'},
			{screen_id: 'profile_abuse', option_id: 'harassment'},
		],
		reason: 'harassment',
		category: 'harassment',
	},
	{
		name: 'P5 private information',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['profile_text']},
			{screen_id: 'root_user', option_id: 'something_else'},
			{screen_id: 'something_else_user', option_id: 'private_info'},
			{screen_id: 'profile_private_info', item_ids: ['phone', 'address']},
		],
		reason: 'doxxing',
		category: 'inappropriate_profile',
	},
	{
		name: 'P4 self-harm worry',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['profile_text']},
			{screen_id: 'root_user', option_id: 'something_else'},
			{screen_id: 'something_else_user', option_id: 'self_harm'},
			{screen_id: 'crisis_support'},
			{screen_id: 'self_harm_profile', option_id: 'worried'},
		],
		reason: 'wellbeing_concern',
		category: 'other',
	},
	{
		name: 'P4 age stated',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['profile_text']},
			{screen_id: 'root_user', option_id: 'something_else'},
			{screen_id: 'something_else_user', option_id: 'too_young'},
			{screen_id: 'age_stated_profile', option_id: 'age_yes'},
		],
		reason: 'underage',
		category: 'underage_user',
	},
	{
		name: 'spam profile',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['name']},
			{screen_id: 'root_user', option_id: 'spam'},
			{screen_id: 'spam_profile', option_id: 'spam_profile'},
		],
		reason: 'spam',
		category: 'spam_account',
	},
	{
		name: 'staff impersonation',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['name', 'photo']},
			{screen_id: 'root_user', option_id: 'impersonation'},
			{screen_id: 'impersonation', option_id: 'impersonation_staff'},
		],
		reason: 'impersonation_staff',
		category: 'impersonation',
	},
	{
		name: 'hateful profile',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['profile_text']},
			{screen_id: 'root_user', option_id: 'hate_violence'},
			{screen_id: 'profile_hate_violence', option_id: 'hate'},
			{screen_id: 'hate', option_id: 'hate_slurs'},
		],
		reason: 'hate_slurs',
		category: 'hate_speech',
	},
	{
		name: 'csam in a profile',
		steps: [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['photo']},
			{screen_id: 'root_user', option_id: 'abuse'},
			{screen_id: 'profile_abuse', option_id: 'sexual'},
			{screen_id: 'profile_sexual', option_id: 'minor_sexual'},
			{screen_id: 'profile_minor_sexual', option_id: 'csam'},
		],
		reason: 'csam',
		category: 'child_safety',
	},
];

const SPAM_WALK: ReadonlyArray<ReportFlowStepInput> = [{screen_id: 'root_message', option_id: 'spam'}];
const USER_SPAM_WALK: ReadonlyArray<ReportFlowStepInput> = USER_WALKS[4].steps;

class CapturingPublisher implements ActivityPublisher {
	readonly payloads: Array<string> = [];

	async publish(_subject: string, payload: string): Promise<void> {
		this.payloads.push(payload);
	}
}

function currentHash(target: ReportFlowTargetType, surface: ReportFlowSurface = 'in_app'): string {
	return getReportFlowVariant(target, surface).revisionHash;
}

function expectedSteps(target: ReportFlowTargetType, steps: ReadonlyArray<ReportFlowStepInput>) {
	return resolveReportFlowAnswers({target, surface: 'in_app', revisionHash: currentHash(target), steps}).steps;
}

function screenKind(screen: ReportFlowResponse['screens'][number]): string {
	if (screen.checklist) return 'checklist';
	if (screen.next_screen_id) return 'info';
	return 'choice';
}

function optionIds(flow: ReportFlowResponse): Array<string> {
	return flow.screens.flatMap((screen) => screen.options.map((option) => option.id));
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

async function setupMessage(harness: ApiTestHarness): Promise<MessageTarget> {
	const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
	const author = members[0];
	const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
	const message = await sendChannelMessage(harness, author.token, channel.id, 'Reported message');
	return {reporter: owner, author, guildId: guild.id, channelId: channel.id, messageId: message.id};
}

async function setupDmMessages(
	harness: ApiTestHarness,
	reporter: TestAccount,
	count: number,
): Promise<{channelId: string; messageIds: Array<string>}> {
	const author = await createTestAccount(harness);
	await createFriendship(harness, reporter, author);
	const channel = await createDmChannel(harness, author.token, reporter.userId);
	const messageIds: Array<string> = [];
	for (let index = 0; index < count; index++) {
		messageIds.push((await sendChannelMessage(harness, author.token, channel.id, `DM message ${index}`)).id);
	}
	return {channelId: channel.id, messageIds};
}

function messageBody(target: {channelId: string; messageId: string}, steps = SPAM_WALK, extra = {}) {
	return {
		channel_id: target.channelId,
		message_id: target.messageId,
		revision_hash: currentHash('message'),
		steps,
		...extra,
	};
}

function userBody(userId: string, steps = USER_SPAM_WALK, extra = {}) {
	return {user_id: userId, revision_hash: currentHash('user'), steps, ...extra};
}

function submitMessage(harness: ApiTestHarness, token: string, body: unknown) {
	return createBuilder<ReportResponse>(harness, token).post('/reports/flows/message/submissions').body(body);
}

function submitUser(harness: ApiTestHarness, token: string, body: unknown) {
	return createBuilder<ReportResponse>(harness, token).post('/reports/flows/user/submissions').body(body);
}

function legacyMessage(harness: ApiTestHarness, token: string, target: {channelId: string; messageId: string}) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/message')
		.body({channel_id: target.channelId, message_id: target.messageId, category: 'spam'});
}

function legacyUser(harness: ApiTestHarness, token: string, userId: string) {
	return createBuilder<ReportResponse>(harness, token)
		.post('/reports/user')
		.body({user_id: userId, category: 'spam_account'});
}

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '').patch(`/test/users/${userId}/flags`).body({flags: flags.toString()}).execute();
}

describe('Report flows', () => {
	let harness: ApiTestHarness;
	const originalSelfHosted = getConfig().instance.selfHosted;
	const originalProductName = getConfig().instance.branding.productName;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	afterEach(async () => {
		getConfig().instance.selfHosted = originalSelfHosted;
		getConfig().instance.branding.productName = originalProductName;
		resetActivityEventsForTests();
		await harness?.shutdown();
	});

	describe('Fetch', () => {
		test.each(VARIANTS)('%s on %s serves every reachable screen', async (target, surface) => {
			const account = await createTestAccount(harness);
			const flow = await createBuilder<ReportFlowResponse>(harness, account.token)
				.get(`/reports/flows/${target}?surface=${surface}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(ReportFlowResponse.safeParse(flow).success).toBe(true);
			const variant = getReportFlowVariant(target, surface);
			expect(flow.target_type).toBe(target);
			expect(flow.surface).toBe(surface);
			expect(flow.start_screen_id).toBe(variant.startScreenId);
			expect(flow.screens[0].id).toBe(variant.startScreenId);
			expect(flow.revision_hash).toBe(variant.revisionHash);
			expect(flow.screens.map((screen) => screen.id).sort()).toEqual([...variant.screens.keys()].sort());
			for (const screen of flow.screens) {
				expect(screenKind(screen), screen.id).toBe(variant.screens.get(screen.id)?.kind);
			}
		});

		test('the default surface is in app and the hash is locale independent', async () => {
			const english = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/message?locale=en-US')
				.expect(HTTP_STATUS.OK)
				.execute();
			const german = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/message?locale=de')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(english.surface).toBe('in_app');
			expect(english.revision_hash).toBe(german.revision_hash);
			expect(english.revision_hash).toMatch(/^[0-9a-f]{16}$/);
		});

		test('works logged out and sends no-cache headers', async () => {
			const {response, json} = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/user')
				.expect(HTTP_STATUS.OK)
				.executeWithResponse();
			expect(json.start_screen_id).toBe('profile_intro');
			expect(response.headers.get('cache-control')).toBe('private, no-cache');
			expect(response.headers.get('vary')).toContain('Accept-Language');
		});

		test('the DSA message flow drops the app-only rows and adds the copyright notice', async () => {
			const inApp = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/message')
				.expect(HTTP_STATUS.OK)
				.execute();
			const dsa = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/message?surface=dsa')
				.expect(HTTP_STATUS.OK)
				.execute();
			const appOnly = ['dislike', 'rude_language', 'dsa', 'copyright', 'worried_self_harm', 'worried_suicide'];
			for (const id of appOnly) {
				expect(optionIds(inApp), id).toContain(id);
				expect(optionIds(dsa), id).not.toContain(id);
			}
			expect(optionIds(inApp)).not.toContain('copyright_notice');
			expect(optionIds(dsa)).toContain('copyright_notice');
			expect(dsa.revision_hash).not.toBe(inApp.revision_hash);
		});

		test('the DSA user flow starts at the profile parts', async () => {
			const flow = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/user?surface=dsa')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.start_screen_id).toBe('profile_parts');
			expect(flow.screens.some((screen) => screen.id === 'profile_intro')).toBe(false);
		});

		test('the guild flow exists only on the DSA surface', async () => {
			for (const path of ['/reports/flows/guild', '/reports/flows/guild?surface=in_app']) {
				const error = await createBuilderWithoutAuth<ErrorResponse>(harness)
					.get(path)
					.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
					.execute();
				expect(error.errors?.map((entry) => entry.path)).toEqual(['surface']);
			}
			const flow = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/guild?surface=dsa')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.start_screen_id).toBe('community_parts');
		});

		test('a self-hosted instance has no guidelines link', async () => {
			const hosted = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/user')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(hosted.guidelines_url).not.toBeNull();
			expect(hosted.screens[0].options.map((option) => option.id)).toEqual(['learn_more']);
			getConfig().instance.selfHosted = true;
			const selfHosted = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/user')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(selfHosted.guidelines_url).toBeNull();
			expect(selfHosted.screens[0].options).toEqual([]);
			expect(selfHosted.revision_hash).not.toBe(hosted.revision_hash);
		});
	});

	describe('Locale', () => {
		test('the user locale picks the catalog', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me/settings')
				.body({locale: 'de'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const flow = await createBuilder<ReportFlowResponse>(harness, account.token)
				.get('/reports/flows/message')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.locale).toBe('de');
			expect(flow).toEqual(getReportFlowResponse('message', 'in_app', 'de'));
		});

		test.each([
			['sv', 'sv-SE'],
			['nb', 'no'],
			['pt', 'pt-BR'],
			['xx-YY', 'en-US'],
		])('?locale=%s resolves to %s', async (raw, resolved) => {
			const flow = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get(`/reports/flows/message?locale=${raw}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.locale).toBe(resolved);
			expect(flow).toEqual(getReportFlowResponse('message', 'in_app', resolved));
		});

		test('the query locale wins over the user locale', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me/settings')
				.body({locale: 'de'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const flow = await createBuilder<ReportFlowResponse>(harness, account.token)
				.get('/reports/flows/message?locale=ja')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.locale).toBe('ja');
		});

		test('Accept-Language is used when logged out', async () => {
			const flow = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/user')
				.header('Accept-Language', 'fr-FR,fr;q=0.9')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.locale).toBe('fr');
		});

		test('the product name comes from the instance config', async () => {
			getConfig().instance.branding.productName = 'Harbor Chat';
			const flow = await createBuilderWithoutAuth<ReportFlowResponse>(harness)
				.get('/reports/flows/user?locale=en-US')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(flow.screens[0].options[0].label).toBe('Read the Harbor Chat Community Guidelines');
		});
	});

	describe('Unknown', () => {
		test('an unknown target type is a validation error', async () => {
			const error = await createBuilderWithoutAuth<ErrorResponse>(harness)
				.get('/reports/flows/webhook')
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			expect(error.errors?.map((entry) => entry.path)).toEqual(['target_type']);
		});

		test('an unknown surface is a validation error', async () => {
			const error = await createBuilderWithoutAuth<ErrorResponse>(harness)
				.get('/reports/flows/message?surface=nope')
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			expect(error.errors?.map((entry) => entry.path)).toEqual(['surface']);
		});
	});

	describe('Submit, message', () => {
		test.each(MESSAGE_WALKS)('$name stores $reason', async ({steps, reason, category}) => {
			const target = await setupMessage(harness);
			const result = await submitMessage(harness, target.reporter.token, messageBody(target, steps, {locale: 'de'}))
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.status).toBe('pending');
			const report = await readReport(result.report_id);
			expect(report.reason).toBe(reason);
			expect(report.category).toBe(category);
			expect(report.flowSteps).toEqual(expectedSteps('message', steps));
			expect(report.flowRevision).toBe(currentHash('message'));
			expect(report.flowLocale).toBe('de');
			expect(report.flowSurface).toBe('in_app');
			expect(report.reporterGoodFaithConfirmed).toBeNull();
			expect(report.reportedMessageId?.toString()).toBe(target.messageId);
			expect(report.reportedUserId?.toString()).toBe(target.author.userId);
		});

		test('checklist items are stored in definition order', async () => {
			const target = await setupMessage(harness);
			const result = await submitMessage(harness, target.reporter.token, messageBody(target, MESSAGE_WALKS[1].steps))
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await readReport(result.report_id);
			expect(report.flowSteps?.[1]).toEqual({screen_id: 'private_info', item_ids: ['email', 'phone']});
		});

		test('the locale falls back to the request locale and is resolved', async () => {
			const target = await setupMessage(harness);
			const first = await submitMessage(harness, target.reporter.token, messageBody(target, SPAM_WALK, {locale: 'nb'}))
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await readReport(first.report_id)).flowLocale).toBe('no');
			const other = await setupMessage(harness);
			await createBuilder(harness, other.reporter.token)
				.patch('/users/@me/settings')
				.body({locale: 'ja'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const second = await submitMessage(harness, other.reporter.token, messageBody(other))
				.expect(HTTP_STATUS.OK)
				.execute();
			expect((await readReport(second.report_id)).flowLocale).toBe('ja');
		});
	});

	describe('Submit, user', () => {
		test.each(USER_WALKS)('$name stores $reason as $category', async ({steps, reason, category}) => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const result = await submitUser(harness, reporter.token, userBody(target.userId, steps, {locale: 'sv'}))
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await readReport(result.report_id);
			expect(report.reason).toBe(reason);
			expect(report.category).toBe(category);
			expect(report.flowSteps).toEqual(expectedSteps('user', steps));
			expect(report.flowRevision).toBe(currentHash('user'));
			expect(report.flowLocale).toBe('sv-SE');
			expect(report.flowSurface).toBe('in_app');
			expect(report.reportedUserId?.toString()).toBe(target.userId);
		});

		test('the guild context is kept', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const result = await submitUser(
				harness,
				owner.token,
				userBody(members[0].userId, USER_SPAM_WALK, {guild_id: guild.id}),
			)
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await readReport(result.report_id);
			expect(report.reportedGuildId?.toString()).toBe(guild.id);
			expect(report.reason).toBe('spam');
		});

		test('a guild the reporter has not joined is answered like an unknown guild', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 2);
			const [target, formerMember] = members;
			const outsider = await createTestAccount(harness);
			const unknown = await submitUser(
				harness,
				outsider.token,
				userBody(target.userId, USER_SPAM_WALK, {guild_id: TEST_IDS.NONEXISTENT_GUILD}),
			)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.executeRaw();
			const notJoined = await submitUser(
				harness,
				outsider.token,
				userBody(target.userId, USER_SPAM_WALK, {guild_id: guild.id}),
			)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.executeRaw();
			expect(notJoined.text).toBe(unknown.text);
			await leaveGuild(harness, formerMember.token, guild.id);
			const left = await submitUser(
				harness,
				formerMember.token,
				userBody(target.userId, USER_SPAM_WALK, {guild_id: guild.id}),
			)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
				.executeRaw();
			expect(left.text).toBe(unknown.text);
			expect(await countReports()).toBe(0);
			await submitUser(harness, outsider.token, userBody(target.userId)).expect(HTTP_STATUS.OK).execute();
			expect(await countReports()).toBe(1);
		});
	});

	describe('Invalid walks', () => {
		const messageCases: ReadonlyArray<{name: string; steps: ReadonlyArray<ReportFlowStepInput>; stepIndex: number}> = [
			{name: 'wrong start', steps: [{screen_id: 'abuse', option_id: 'harassment'}], stepIndex: 0},
			{
				name: 'unknown screen',
				steps: [
					{screen_id: 'root_message', option_id: 'abuse'},
					{screen_id: 'nope', option_id: 'harassment'},
				],
				stepIndex: 1,
			},
			{name: 'option not on the screen', steps: [{screen_id: 'root_message', option_id: 'csam'}], stepIndex: 0},
			{
				name: 'a DSA-only option in app',
				steps: [
					{screen_id: 'root_message', option_id: 'something_else'},
					{screen_id: 'something_else_message', option_id: 'copyright_notice'},
				],
				stepIndex: 1,
			},
			{
				name: 'skipped screen',
				steps: [
					{screen_id: 'root_message', option_id: 'abuse'},
					{screen_id: 'harassment', option_id: 'harassment_direct'},
				],
				stepIndex: 1,
			},
			{name: 'walk ending on a screen', steps: [{screen_id: 'root_message', option_id: 'abuse'}], stepIndex: 0},
			{name: 'walk through dislike', steps: [{screen_id: 'root_message', option_id: 'dislike'}], stepIndex: 0},
			{
				name: 'walk through rude language',
				steps: [
					{screen_id: 'root_message', option_id: 'abuse'},
					{screen_id: 'abuse', option_id: 'rude_language'},
				],
				stepIndex: 1,
			},
			{
				name: 'walk through a link',
				steps: [
					{screen_id: 'root_message', option_id: 'something_else'},
					{screen_id: 'something_else_message', option_id: 'dsa'},
				],
				stepIndex: 1,
			},
			{
				name: 'unknown item',
				steps: [
					{screen_id: 'root_message', option_id: 'private_info'},
					{screen_id: 'private_info', item_ids: ['nope']},
				],
				stepIndex: 1,
			},
			{
				name: 'duplicate items',
				steps: [
					{screen_id: 'root_message', option_id: 'private_info'},
					{screen_id: 'private_info', item_ids: ['email', 'email']},
				],
				stepIndex: 1,
			},
			{
				name: 'option on a checklist',
				steps: [
					{screen_id: 'root_message', option_id: 'private_info'},
					{screen_id: 'private_info', option_id: 'email'},
				],
				stepIndex: 1,
			},
			{
				name: 'step after a submit',
				steps: [
					{screen_id: 'root_message', option_id: 'spam'},
					{screen_id: 'root_message', option_id: 'spam'},
				],
				stepIndex: 1,
			},
		];

		test.each(messageCases)('message: $name fails at step $stepIndex', async ({steps, stepIndex}) => {
			const target = await setupMessage(harness);
			const error = await submitMessage(harness, target.reporter.token, messageBody(target, steps))
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
				.execute();
			expect(error).toMatchObject({step_index: stepIndex});
			expect(await countReports()).toBe(0);
		});

		const userCases: ReadonlyArray<{name: string; steps: ReadonlyArray<ReportFlowStepInput>; stepIndex: number}> = [
			{
				name: 'missing info step',
				steps: [
					{screen_id: 'profile_parts', item_ids: ['photo']},
					{screen_id: 'root_user', option_id: 'spam'},
					{screen_id: 'spam_profile', option_id: 'spam_profile'},
				],
				stepIndex: 0,
			},
			{
				name: 'option on an info step',
				steps: [
					{screen_id: 'profile_intro', option_id: 'learn_more'},
					{screen_id: 'profile_parts', item_ids: ['photo']},
					{screen_id: 'root_user', option_id: 'spam'},
					{screen_id: 'spam_profile', option_id: 'spam_profile'},
				],
				stepIndex: 0,
			},
			{
				name: 'missing crisis support step',
				steps: [
					{screen_id: 'profile_intro'},
					{screen_id: 'profile_parts', item_ids: ['profile_text']},
					{screen_id: 'root_user', option_id: 'something_else'},
					{screen_id: 'something_else_user', option_id: 'self_harm'},
					{screen_id: 'self_harm_profile', option_id: 'worried'},
				],
				stepIndex: 4,
			},
			{
				name: 'age not stated',
				steps: [
					{screen_id: 'profile_intro'},
					{screen_id: 'profile_parts', item_ids: ['profile_text']},
					{screen_id: 'root_user', option_id: 'something_else'},
					{screen_id: 'something_else_user', option_id: 'too_young'},
					{screen_id: 'age_stated_profile', option_id: 'age_no'},
				],
				stepIndex: 4,
			},
		];

		test.each(userCases)('user: $name fails at step $stepIndex', async ({steps, stepIndex}) => {
			const reporter = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			const error = await submitUser(harness, reporter.token, userBody(target.userId, steps))
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
				.execute();
			expect(error).toMatchObject({step_index: stepIndex});
			expect(await countReports()).toBe(0);
		});

		test('shape errors are standard validation errors', async () => {
			const target = await setupMessage(harness);
			const seventeen = Array.from({length: 17}, () => ({screen_id: 'root_message', option_id: 'spam'}));
			const shapes = [
				messageBody(target, seventeen),
				messageBody(target, []),
				messageBody(target, [
					{screen_id: 'root_message', option_id: 'private_info'},
					{screen_id: 'private_info', item_ids: []},
				]),
				messageBody(target, [{screen_id: 'x'.repeat(49), option_id: 'spam'}]),
				{...messageBody(target), revision_hash: undefined},
			];
			for (const body of shapes) {
				await submitMessage(harness, target.reporter.token, body)
					.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
					.execute();
			}
			expect(await countReports()).toBe(0);
		});

		test('invalid walks leave no row, spend no service allowance and leave no reservation', async () => {
			const target = await setupMessage(harness);
			for (let attempt = 0; attempt < 6; attempt++) {
				await submitMessage(
					harness,
					target.reporter.token,
					messageBody(target, [{screen_id: 'root_message', option_id: 'dislike'}]),
				)
					.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
					.execute();
			}
			expect(await countReports()).toBe(0);
			await submitMessage(harness, target.reporter.token, messageBody(target)).expect(HTTP_STATUS.OK).execute();
			expect(await countReports()).toBe(1);
		});
	});

	describe('Revision', () => {
		test('a stale hash with a valid walk is accepted and stored', async () => {
			const target = await setupMessage(harness);
			const result = await submitMessage(
				harness,
				target.reporter.token,
				messageBody(target, SPAM_WALK, {revision_hash: '0000000000000000'}),
			)
				.expect(HTTP_STATUS.OK)
				.execute();
			const report = await readReport(result.report_id);
			expect(report.flowRevision).toBe('0000000000000000');
			expect(report.reason).toBe('spam');
		});

		test('a stale hash with an invalid walk is outdated', async () => {
			const target = await setupMessage(harness);
			await submitMessage(
				harness,
				target.reporter.token,
				messageBody(target, [{screen_id: 'root_message', option_id: 'retired_option'}], {
					revision_hash: '0000000000000000',
				}),
			)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.REPORT_FLOW_OUTDATED)
				.execute();
			const reporter = await createTestAccount(harness);
			const user = await createTestAccount(harness);
			await submitUser(
				harness,
				reporter.token,
				userBody(user.userId, [{screen_id: 'profile_parts', item_ids: ['photo']}], {
					revision_hash: '0000000000000000',
				}),
			)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.REPORT_FLOW_OUTDATED)
				.execute();
			expect(await countReports()).toBe(0);
		});

		test('the current hash with an invalid walk is a 400', async () => {
			const target = await setupMessage(harness);
			await submitMessage(
				harness,
				target.reporter.token,
				messageBody(target, [{screen_id: 'root_message', option_id: 'retired_option'}]),
			)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
				.execute();
		});
	});

	describe('Legacy rules', () => {
		test('the author cannot report their own message', async () => {
			const target = await setupMessage(harness);
			await submitMessage(harness, target.author.token, messageBody(target))
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CANNOT_REPORT_OWN_MESSAGE)
				.execute();
		});

		test('a user cannot report their own profile', async () => {
			const reporter = await createTestAccount(harness);
			await submitUser(harness, reporter.token, userBody(reporter.userId))
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CANNOT_REPORT_YOURSELF)
				.execute();
		});

		test('an unverified reporter is refused before the walk is checked', async () => {
			const target = await setupMessage(harness);
			const unverified = await createTestAccount(harness, {skipEmailVerification: true});
			await submitMessage(harness, unverified.token, messageBody(target, [{screen_id: 'nope', option_id: 'x'}]))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED)
				.execute();
			await submitUser(harness, unverified.token, userBody(target.author.userId))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED)
				.execute();
		});

		test('an unclaimed reporter is refused', async () => {
			const target = await setupMessage(harness);
			await unclaimAccount(harness, target.reporter.userId);
			await submitMessage(harness, target.reporter.token, messageBody(target))
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS)
				.execute();
		});

		test('a report-banned reporter is refused', async () => {
			const target = await setupMessage(harness);
			await setUserFlags(harness, target.reporter.userId, HAS_SESSION_STARTED | UserFlags.REPORT_BANNED);
			await submitMessage(harness, target.reporter.token, messageBody(target))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REPORT_BANNED)
				.execute();
			await submitUser(harness, target.reporter.token, userBody(target.author.userId))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.REPORT_BANNED)
				.execute();
		});

		test('bots and logged out callers cannot submit', async () => {
			const target = await setupMessage(harness);
			const bot = await createTestBotAccount(harness);
			await submitMessage(harness, `Bot ${bot.botToken}`, messageBody(target))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
			await submitUser(harness, `Bot ${bot.botToken}`, userBody(target.author.userId))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
			await submitMessage(harness, '', messageBody(target)).expect(HTTP_STATUS.UNAUTHORIZED).execute();
		});

		test('channel access is checked as on the legacy route', async () => {
			const target = await setupMessage(harness);
			const outsider = await createTestAccount(harness);
			await submitMessage(harness, outsider.token, messageBody(target))
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
			await legacyMessage(harness, outsider.token, target)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
			await submitMessage(harness, target.reporter.token, {
				...messageBody(target),
				channel_id: TEST_IDS.NONEXISTENT_CHANNEL,
			})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
			await submitUser(harness, target.reporter.token, userBody(TEST_IDS.NONEXISTENT_USER))
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_USER)
				.execute();
			expect(await countReports()).toBe(0);
		});
	});

	describe('Dedupe', () => {
		test('v2 and legacy message reports share the reservation', async () => {
			const first = await setupMessage(harness);
			await legacyMessage(harness, first.reporter.token, first).expect(HTTP_STATUS.OK).execute();
			await submitMessage(harness, first.reporter.token, messageBody(first))
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			const second = await setupMessage(harness);
			await submitMessage(harness, second.reporter.token, messageBody(second)).expect(HTTP_STATUS.OK).execute();
			await legacyMessage(harness, second.reporter.token, second)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			expect(await countReports()).toBe(2);
		});

		test('v2 and legacy user reports share a 24 hour reservation', async () => {
			const reporter = await createTestAccount(harness);
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			await legacyUser(harness, reporter.token, first.userId).expect(HTTP_STATUS.OK).execute();
			await submitUser(harness, reporter.token, userBody(first.userId))
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			await submitUser(harness, reporter.token, userBody(second.userId)).expect(HTTP_STATUS.OK).execute();
			await legacyUser(harness, reporter.token, second.userId)
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			const other = await createTestAccount(harness);
			await submitUser(harness, other.token, userBody(first.userId)).expect(HTTP_STATUS.OK).execute();
			expect(await countReports()).toBe(3);
		});

		test('parallel message submissions file one report', async () => {
			const reporter = await createTestAccount(harness);
			const {channelId, messageIds} = await setupDmMessages(harness, reporter, 2);
			const v2 = await Promise.all(
				[0, 1, 2].map(() =>
					submitMessage(harness, reporter.token, messageBody({channelId, messageId: messageIds[0]})).executeRaw(),
				),
			);
			expect(v2.map(({response}) => response.status).sort()).toEqual([200, 409, 409]);
			const legacy = await Promise.all(
				[0, 1, 2].map(() => legacyMessage(harness, reporter.token, {channelId, messageId: messageIds[1]}).executeRaw()),
			);
			expect(legacy.map(({response}) => response.status).sort()).toEqual([200, 409, 409]);
			expect(await countReports()).toBe(2);
		});

		test('parallel user submissions file one report', async () => {
			const reporter = await createTestAccount(harness);
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			const v2 = await Promise.all(
				[0, 1, 2].map(() => submitUser(harness, reporter.token, userBody(first.userId)).executeRaw()),
			);
			expect(v2.map(({response}) => response.status).sort()).toEqual([200, 409, 409]);
			const legacy = await Promise.all(
				[0, 1, 2].map(() => legacyUser(harness, reporter.token, second.userId).executeRaw()),
			);
			expect(legacy.map(({response}) => response.status).sort()).toEqual([200, 409, 409]);
			expect(await countReports()).toBe(2);
		});

		test('a rate limited message report releases its reservation', async () => {
			const reporter = await createTestAccount(harness);
			const {channelId, messageIds} = await setupDmMessages(harness, reporter, 4);
			for (const messageId of messageIds.slice(0, 3)) {
				await submitMessage(harness, reporter.token, messageBody({channelId, messageId}))
					.expect(HTTP_STATUS.OK)
					.execute();
			}
			const limited = {channelId, messageId: messageIds[3]};
			for (let attempt = 0; attempt < 2; attempt++) {
				await submitMessage(harness, reporter.token, messageBody(limited))
					.expect(429, APIErrorCodes.RATE_LIMITED)
					.execute();
			}
			await getRateLimitService().resetLimit(`report:message:channel:user:${reporter.userId}:${channelId}`);
			await submitMessage(harness, reporter.token, messageBody(limited)).expect(HTTP_STATUS.OK).execute();
			expect(await countReports()).toBe(4);
		});

		test('a repeat user submission past the reporter allowance is rate limited', async () => {
			const reporter = await createTestAccount(harness);
			const users = await Promise.all([0, 1, 2, 3, 4].map(() => createTestAccount(harness)));
			for (const user of users) {
				await submitUser(harness, reporter.token, userBody(user.userId)).expect(HTTP_STATUS.OK).execute();
			}
			await submitUser(harness, reporter.token, userBody(users[0].userId))
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
			await getRateLimitService().resetLimit(`report:create:user:${reporter.userId}`);
			await submitUser(harness, reporter.token, userBody(users[0].userId))
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			expect(await countReports()).toBe(5);
		});

		test('a rate limited user report releases its reservation', async () => {
			const reporter = await createTestAccount(harness);
			for (let index = 0; index < 5; index++) {
				const user = await createTestAccount(harness);
				await submitUser(harness, reporter.token, userBody(user.userId)).expect(HTTP_STATUS.OK).execute();
			}
			const limited = await createTestAccount(harness);
			for (let attempt = 0; attempt < 2; attempt++) {
				await submitUser(harness, reporter.token, userBody(limited.userId))
					.expect(429, APIErrorCodes.RATE_LIMITED)
					.execute();
			}
			await getRateLimitService().resetLimit(`report:create:user:${reporter.userId}`);
			await submitUser(harness, reporter.token, userBody(limited.userId)).expect(HTTP_STATUS.OK).execute();
			await submitUser(harness, reporter.token, userBody(limited.userId))
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONFLICT)
				.execute();
			expect(await countReports()).toBe(6);
		});
	});

	describe('Limits', () => {
		test('a message report refused by the guild limit spends no other allowance', async () => {
			const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const author = members[0];
			const second = await createChannel(harness, owner.token, guild.id, 'second-channel');
			const first: Array<string> = [];
			for (let index = 0; index < 3; index++) {
				first.push((await sendChannelMessage(harness, author.token, systemChannel.id, `First ${index}`)).id);
			}
			const other: Array<string> = [];
			for (let index = 0; index < 2; index++) {
				other.push((await sendChannelMessage(harness, author.token, second.id, `Second ${index}`)).id);
			}
			for (const messageId of first) {
				await submitMessage(harness, owner.token, messageBody({channelId: systemChannel.id, messageId}))
					.expect(HTTP_STATUS.OK)
					.execute();
			}
			await submitMessage(harness, owner.token, messageBody({channelId: second.id, messageId: other[0]}))
				.expect(HTTP_STATUS.OK)
				.execute();
			const limited = {channelId: second.id, messageId: other[1]};
			const remaining = async (identifier: string, maxAttempts: number) =>
				(await getRateLimitService().peekLimit({identifier, maxAttempts, windowMs: 3_600_000})).remaining;
			const budgets = async () => ({
				reporter: await remaining(`report:create:user:${owner.userId}`, 5),
				channel: await remaining(`report:message:channel:user:${owner.userId}:${second.id}`, 3),
				target: await remaining(`report:message:target:${limited.messageId}`, 20),
				guild: await remaining(`report:message:guild:user:${owner.userId}:${guild.id}`, 4),
			});
			expect(await budgets()).toEqual({reporter: 1, channel: 2, target: 20, guild: 0});
			for (let attempt = 0; attempt < 2; attempt++) {
				await submitMessage(harness, owner.token, messageBody(limited))
					.expect(429, APIErrorCodes.RATE_LIMITED)
					.execute();
			}
			expect(await budgets()).toEqual({reporter: 1, channel: 2, target: 20, guild: 0});
			await getRateLimitService().resetLimit(`report:message:guild:user:${owner.userId}:${guild.id}`);
			await submitMessage(harness, owner.token, messageBody(limited)).expect(HTTP_STATUS.OK).execute();
			expect(await budgets()).toEqual({reporter: 0, channel: 1, target: 19, guild: 3});
			expect(await countReports()).toBe(5);
		});

		test('the service limit is shared with the legacy routes', async () => {
			const reporter = await createTestAccount(harness);
			const dmA = await setupDmMessages(harness, reporter, 2);
			const dmB = await setupDmMessages(harness, reporter, 1);
			await legacyMessage(harness, reporter.token, {channelId: dmA.channelId, messageId: dmA.messageIds[0]})
				.expect(HTTP_STATUS.OK)
				.execute();
			await submitMessage(
				harness,
				reporter.token,
				messageBody({channelId: dmA.channelId, messageId: dmA.messageIds[1]}),
			)
				.expect(HTTP_STATUS.OK)
				.execute();
			await submitMessage(
				harness,
				reporter.token,
				messageBody({channelId: dmB.channelId, messageId: dmB.messageIds[0]}),
			)
				.expect(HTTP_STATUS.OK)
				.execute();
			const users = await Promise.all([0, 1, 2].map(() => createTestAccount(harness)));
			await legacyUser(harness, reporter.token, users[0].userId).expect(HTTP_STATUS.OK).execute();
			await submitUser(harness, reporter.token, userBody(users[1].userId)).expect(HTTP_STATUS.OK).execute();
			await submitUser(harness, reporter.token, userBody(users[2].userId))
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
			expect(await countReports()).toBe(5);
		});

		test('the route bucket is shared and invalid walks spend a route token but no service token', async () => {
			const reporter = await createTestAccount(harness);
			const dm = await setupDmMessages(harness, reporter, 3);
			for (let attempt = 0; attempt < 6; attempt++) {
				await submitMessage(
					harness,
					reporter.token,
					messageBody({channelId: dm.channelId, messageId: dm.messageIds[0]}, [
						{screen_id: 'root_message', option_id: 'dislike'},
					]),
				)
					.header(RATE_LIMIT_HEADER, 'true')
					.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS)
					.execute();
			}
			await legacyMessage(harness, reporter.token, {channelId: dm.channelId, messageId: dm.messageIds[0]})
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(HTTP_STATUS.OK)
				.execute();
			await submitMessage(harness, reporter.token, messageBody({channelId: dm.channelId, messageId: dm.messageIds[1]}))
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(HTTP_STATUS.OK)
				.execute();
			const users = await Promise.all([0, 1, 2].map(() => createTestAccount(harness)));
			await legacyUser(harness, reporter.token, users[0].userId)
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(HTTP_STATUS.OK)
				.execute();
			await submitUser(harness, reporter.token, userBody(users[1].userId))
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(HTTP_STATUS.OK)
				.execute();
			await submitUser(harness, reporter.token, userBody(users[2].userId))
				.header(RATE_LIMIT_HEADER, 'true')
				.expect(429, APIErrorCodes.RATE_LIMITED)
				.execute();
			expect(await countReports()).toBe(4);
			await submitUser(harness, reporter.token, userBody(users[2].userId)).expect(HTTP_STATUS.OK).execute();
		});
	});

	describe('Content filter', () => {
		test('a blocked phrase equal to an item id does not block a submission', async () => {
			const target = await setupMessage(harness);
			const phrases = ['email', 'phone', 'private_info', 'root_message'];
			for (const phrase of phrases) phraseBlocklistCache.add(phrase);
			try {
				const result = await submitMessage(harness, target.reporter.token, messageBody(target, MESSAGE_WALKS[1].steps))
					.expect(HTTP_STATUS.OK)
					.execute();
				expect((await readReport(result.report_id)).reason).toBe('doxxing');
			} finally {
				for (const phrase of phrases) phraseBlocklistCache.remove(phrase);
			}
		});
	});

	describe('Events', () => {
		test('report_filed keeps the legacy category and target type', async () => {
			const publisher = new CapturingPublisher();
			await startActivityEvents({publisher, kv: new MockKVProvider()});
			const target = await setupMessage(harness);
			await submitMessage(harness, target.reporter.token, messageBody(target, MESSAGE_WALKS[7].steps))
				.expect(HTTP_STATUS.OK)
				.execute();
			const reporter = await createTestAccount(harness);
			await submitUser(harness, reporter.token, userBody(target.author.userId, USER_WALKS[1].steps))
				.expect(HTTP_STATUS.OK)
				.execute();
			const filed = () =>
				publisher.payloads
					.map((payload) => JSON.parse(payload) as {kind: string; data: Record<string, unknown>})
					.filter((event) => event.kind === 'report_filed');
			await vi.waitFor(() => expect(filed()).toHaveLength(2));
			const events = filed().map((event) => event.data);
			expect(events).toEqual(
				expect.arrayContaining([
					expect.objectContaining({
						category: 'child_safety',
						target_type: 'message',
						message_id: target.messageId,
						reported_user_id: target.author.userId,
					}),
					expect.objectContaining({
						category: 'inappropriate_profile',
						target_type: 'user',
						reported_user_id: target.author.userId,
					}),
				]),
			);
			for (const event of events) {
				expect(Object.keys(event).sort()).toEqual(
					[
						'category',
						'channel_id',
						'guild_id',
						'message_id',
						'report_id',
						'reported_user_id',
						'reporter_id',
						'target_type',
					].sort(),
				);
			}
		});
	});

	describe('Legacy routes', () => {
		test('the legacy message, user and guild routes still file reports with empty flow fields', async () => {
			const target = await setupMessage(harness);
			const message = await legacyMessage(harness, target.reporter.token, target).expect(HTTP_STATUS.OK).execute();
			const user = await createBuilder<ReportResponse>(harness, target.reporter.token)
				.post('/reports/user')
				.body({user_id: target.author.userId, category: 'inappropriate_profile', guild_id: target.guildId})
				.expect(HTTP_STATUS.OK)
				.execute();
			const guild = await createBuilder<ReportResponse>(harness, target.author.token)
				.post('/reports/guild')
				.body({guild_id: target.guildId, category: 'raid_coordination'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const reports = await Promise.all([message, user, guild].map((result) => readReport(result.report_id)));
			expect(reports.map((report) => report.category)).toEqual(['spam', 'inappropriate_profile', 'raid_coordination']);
			for (const report of reports) {
				expect(report.reason).toBeNull();
				expect(report.flowRevision).toBeNull();
				expect(report.flowSteps).toBeNull();
				expect(report.flowLocale).toBeNull();
				expect(report.flowSurface).toBeNull();
				expect(report.reporterGoodFaithConfirmed).toBeNull();
			}
		});
	});
});
