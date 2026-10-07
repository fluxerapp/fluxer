// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID} from '@app/api/BrandedTypes';
import {MessageContentService} from '@app/api/channel/services/message/MessageContentService';
import {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {MessageSendService} from '@app/api/channel/services/message/MessageSendService';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	updateChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {EmbedService} from '@app/api/infrastructure/EmbedService';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {StartForumThreadResponse} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	sfwChannelId: string;
	nsfwChannelId: string;
}

describe('thread nsfw scope', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'nsfw');
		const sfw = await createChannel(harness, owner.token, guild.id, 'sfw');
		const nsfw = await createChannel(harness, owner.token, guild.id, 'nsfw');
		await updateChannel(harness, owner.token, nsfw.id, {nsfw: true});
		const invite = await createChannelInvite(harness, owner.token, sfw.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		return {owner, member, guildId: guild.id, sfwChannelId: sfw.id, nsfwChannelId: nsfw.id};
	}

	function startThread(token: string, channelId: string) {
		return threadsRequest<ThreadChannelResponse>(harness, token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
	}

	function post(token: string, channelId: string, body: Record<string, unknown>, status = 200, code?: string) {
		return threadsRequest<MessageResponse>(harness, token)
			.post(`/channels/${channelId}/messages`)
			.body(body)
			.expect(status, code)
			.execute();
	}

	it('allows nsfw attachments and unfurls in a thread under an nsfw parent', async () => {
		const s = await setup();
		const nsfwThread = await startThread(s.owner.token, s.nsfwChannelId);
		const sfwThread = await startThread(s.owner.token, s.sfwChannelId);
		const attachments = vi.spyOn(
			MessagePersistenceService.prototype as unknown as {processAttachments: (...args: Array<unknown>) => unknown},
			'processAttachments',
		);
		const embeds = vi.spyOn(EmbedService.prototype, 'getInitialEmbeds');
		const scope = vi.spyOn(MessageContentService.prototype, 'isNSFWContentAllowed');
		await post(s.owner.token, nsfwThread.id, {content: 'see https://example.com/a'});
		expect(scope.mock.calls.at(-1)?.[0].channel?.id.toString()).toBe(s.nsfwChannelId);
		expect(scope.mock.results.at(-1)?.value).toBe(true);
		expect(attachments.mock.calls.at(-1)?.[1]).toBe('allow');
		expect(embeds.mock.calls.at(-1)?.[0].nsfwMode).toBe('allow');
		await post(s.owner.token, sfwThread.id, {content: 'see https://example.com/b'});
		expect(scope.mock.calls.at(-1)?.[0].channel?.id.toString()).toBe(s.sfwChannelId);
		expect(scope.mock.calls.at(-1)?.[0].channel?.isNsfw).toBe(false);
	});

	it('accepts nsfw forwards into a thread only under an nsfw parent', async () => {
		const s = await setup();
		const nsfwThread = await startThread(s.owner.token, s.nsfwChannelId);
		const sfwThread = await startThread(s.owner.token, s.sfwChannelId);
		const source = await sendMessage(harness, s.owner.token, s.sfwChannelId, 'forward me');
		vi.spyOn(
			MessageSendService.prototype as unknown as {snapshotsContainNsfwContent: () => boolean},
			'snapshotsContainNsfwContent',
		).mockReturnValue(true);
		const reference = {type: 1, channel_id: s.sfwChannelId, message_id: source.id};
		await post(s.owner.token, nsfwThread.id, {message_reference: reference});
		await post(
			s.owner.token,
			sfwThread.id,
			{message_reference: reference},
			403,
			APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED,
		);
	});

	it('requires age verification for posts and post data under an nsfw forum', async () => {
		const s = await setup();
		const minor = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
		const invite = await createChannelInvite(harness, s.owner.token, s.sfwChannelId);
		await acceptInvite(harness, minor.token, invite.code);
		await ensureSessionStarted(harness, minor.token);
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
			.post(`/guilds/${s.guildId}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM, nsfw: true})
			.execute();
		const created = await threadsRequest<StartForumThreadResponse>(harness, s.owner.token)
			.post(`/channels/${forum.id}/threads`)
			.body({name: 'post', message: {content: 'hello'}})
			.expect(201)
			.execute();
		const restricted = (method: 'get' | 'post', route: string, body?: Record<string, unknown>) => {
			const request = threadsRequest(harness, minor.token)[method](route);
			return (body ? request.body(body) : request).expect(403, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED).execute();
		};
		await restricted('get', `/channels/${forum.id}`);
		await restricted('get', `/channels/${created.id}/messages`);
		await restricted('post', `/channels/${forum.id}/post-data`, {thread_ids: [created.id]});
		await restricted('get', `/channels/${forum.id}/threads/search`);
		await threadsRequest(harness, s.member.token).get(`/channels/${created.id}/messages`).expect(200).execute();
	});

	it('records the effective nsfw of the parent on a thread message report', async () => {
		const s = await setup();
		const invite = await createChannelInvite(harness, s.owner.token, s.nsfwChannelId);
		await acceptInvite(harness, s.member.token, invite.code);
		const thread = await startThread(s.owner.token, s.nsfwChannelId);
		const message = await post(s.owner.token, thread.id, {content: 'reported'});
		const report = await threadsRequest<{report_id: string}>(harness, s.member.token)
			.post('/reports/message')
			.body({channel_id: thread.id, message_id: message.id, category: 'harassment'})
			.expect(200)
			.execute();
		const stored = await new ReportRepository().getReport(createReportID(BigInt(report.report_id)));
		expect(stored?.reportedChannelEffectiveNsfw).toBe(true);
	});
});
