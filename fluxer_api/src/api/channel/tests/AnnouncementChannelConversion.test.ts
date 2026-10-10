// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID, createWebhookID, createWebhookToken} from '@app/api/BrandedTypes';
import {disableCrosspostWorker, followRequest, publishRequest} from '@app/api/channel/tests/AnnouncementTestUtils';
import {
	createChannel,
	createFriendship,
	createGroupDmChannel,
	createGuild,
	deleteChannel,
	getChannel,
	setupTestGuildWithMembers,
	updateChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	copiesOf,
	editRequest,
	type FanoutWorld,
	followInto,
	postAndPublish,
	sendMessage,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, getChannelWebhooks} from '@app/api/webhook/tests/WebhookTestUtils';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

interface RecordedJob {
	taskType: string;
	payload: WorkerJobPayload;
	options: WorkerJobOptions | undefined;
}

class RecordingWorkerService implements IWorkerService {
	readonly jobs: Array<RecordedJob> = [];
	private nextJobId = 1n;

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		this.jobs.push({taskType, payload, options});
		return this.nextJobId++;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	followerRemovals(): Array<RecordedJob> {
		return this.jobs.filter((job) => job.taskType === 'removeChannelFollowers');
	}
}

interface AuditLogResponse {
	audit_log_entries: Array<{
		action_type: number;
		target_id: string | null;
		changes?: Array<{key: string; old_value?: unknown; new_value?: unknown}>;
	}>;
}

async function insertFollowerWebhook(params: {guildId: string; channelId: string; creatorId: string}): Promise<string> {
	const webhookId = createWebhookID(BigInt(Date.now()) * 1000n + BigInt(Math.floor(Math.random() * 1000)));
	await new WebhookRepository().create({
		webhookId,
		token: createWebhookToken('f'.repeat(64)),
		type: WebhookTypes.CHANNEL_FOLLOWER,
		guildId: createGuildID(BigInt(params.guildId)),
		channelId: createChannelID(BigInt(params.channelId)),
		creatorId: createUserID(BigInt(params.creatorId)),
		name: 'Source Guild #news',
		avatarHash: null,
	});
	return webhookId.toString();
}

describe('Announcement channel conversion', () => {
	let harness: ApiTestHarness;
	let worker: RecordingWorkerService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
	});

	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('converts a text channel to an announcement channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Convert Guild');
		const text = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_TEXT);
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const converted = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(converted.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect((await getChannel(harness, owner.token, text.id)).type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		const channelUpdates = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'CHANNEL_UPDATE');
		expect(channelUpdates).toHaveLength(1);
		expect(channelUpdates[0]?.data).toMatchObject({type: ChannelTypes.GUILD_ANNOUNCEMENT});
		const auditLog = await createBuilder<AuditLogResponse>(harness, owner.token)
			.get(`/guilds/${guild.id}/audit-logs?action_type=${AuditLogActionType.CHANNEL_UPDATE}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const entry = auditLog.audit_log_entries.find((candidate) => candidate.target_id === text.id);
		const typeChange = entry?.changes?.find((change) => change.key === 'type');
		expect(typeChange?.old_value).toBe(ChannelTypes.GUILD_TEXT);
		expect(typeChange?.new_value).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(worker.followerRemovals()).toHaveLength(0);
	});

	test('converts an announcement channel back to text and schedules follower removal', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Revert Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const converted = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${announcement.id}`)
			.body({type: ChannelTypes.GUILD_TEXT})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(converted.type).toBe(ChannelTypes.GUILD_TEXT);
		const removals = worker.followerRemovals();
		expect(removals).toHaveLength(1);
		expect(removals[0]?.payload).toEqual({sourceChannelId: announcement.id, reason: 'converted'});
		expect(removals[0]?.options?.jobKey).toMatch(new RegExp(`^remove-followers:${announcement.id}:converted:\\d+$`));
		expect(removals[0]?.options?.skipLedger).toBeUndefined();
	});

	test('keeps the type when the body has no type or repeats the current type', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Keep Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const renamed = await updateChannel(harness, owner.token, announcement.id, {topic: 'release notes'});
		expect(renamed.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(renamed.topic).toBe('release notes');
		const repeated = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${announcement.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT, topic: 'still news'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(repeated.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		const nulled = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${announcement.id}`)
			.body({type: null, topic: 'null type'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(nulled.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(worker.followerRemovals()).toHaveLength(0);
	});

	test('rejects conversions outside text and announcement', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Reject Guild');
		const text = await createChannel(harness, owner.token, guild.id, 'text', ChannelTypes.GUILD_TEXT);
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const attempts: Array<{channelId: string; type: number | string}> = [
			{channelId: text.id, type: ChannelTypes.GUILD_VOICE},
			{channelId: text.id, type: ChannelTypes.GUILD_CATEGORY},
			{channelId: text.id, type: 'announcement'},
			{channelId: announcement.id, type: ChannelTypes.GUILD_VOICE},
			{channelId: announcement.id, type: ChannelTypes.GROUP_DM},
		];
		for (const attempt of attempts) {
			await createBuilder(harness, owner.token)
				.patch(`/channels/${attempt.channelId}`)
				.body({type: attempt.type})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CHANNEL_TYPE_CONVERSION_NOT_SUPPORTED)
				.execute();
		}
		expect((await getChannel(harness, owner.token, text.id)).type).toBe(ChannelTypes.GUILD_TEXT);
		expect((await getChannel(harness, owner.token, announcement.id)).type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('ignores a body type on channels that cannot be converted', async () => {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		await createFriendship(harness, owner, friend);
		const guild = await createGuild(harness, owner.token, 'Ignore Guild');
		const voice = await createChannel(harness, owner.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);
		const category = await createChannel(harness, owner.token, guild.id, 'category', ChannelTypes.GUILD_CATEGORY);
		const link = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'link', type: ChannelTypes.GUILD_LINK, url: 'https://example.com'})
			.execute();
		const groupDm = await createGroupDmChannel(harness, owner.token, [friend.userId]);
		const attempts: Array<{channelId: string; type: number; expected: number}> = [
			{channelId: voice.id, type: ChannelTypes.GUILD_TEXT, expected: ChannelTypes.GUILD_VOICE},
			{channelId: voice.id, type: ChannelTypes.GUILD_ANNOUNCEMENT, expected: ChannelTypes.GUILD_VOICE},
			{channelId: category.id, type: ChannelTypes.GUILD_ANNOUNCEMENT, expected: ChannelTypes.GUILD_CATEGORY},
			{channelId: link.id, type: ChannelTypes.GUILD_TEXT, expected: ChannelTypes.GUILD_LINK},
			{channelId: groupDm.id, type: ChannelTypes.GUILD_ANNOUNCEMENT, expected: ChannelTypes.GROUP_DM},
		];
		for (const [index, attempt] of attempts.entries()) {
			const name = `renamed-${index}`;
			const updated = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${attempt.channelId}`)
				.body({type: attempt.type, name})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(updated.type).toBe(attempt.expected);
			expect(updated.name).toBe(name);
			const stored = await getChannel(harness, owner.token, attempt.channelId);
			expect(stored.type).toBe(attempt.expected);
			expect(stored.name).toBe(name);
		}
	});

	test('requires MANAGE_CHANNELS', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const text = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_TEXT);
		const announcement = await createChannel(harness, owner.token, guild.id, 'ann', ChannelTypes.GUILD_ANNOUNCEMENT);
		await createBuilder(harness, member.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/channels/${announcement.id}`)
			.body({type: ChannelTypes.GUILD_TEXT})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		expect((await getChannel(harness, owner.token, text.id)).type).toBe(ChannelTypes.GUILD_TEXT);
		expect((await getChannel(harness, owner.token, announcement.id)).type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(worker.followerRemovals()).toHaveLength(0);
	});

	test('refuses to convert a channel that receives follows until the follow is removed', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Followed Guild');
		const text = await createChannel(harness, owner.token, guild.id, 'feed', ChannelTypes.GUILD_TEXT);
		await createWebhook(harness, text.id, owner.token, 'Incoming Bot');
		const followerWebhookId = await insertFollowerWebhook({
			guildId: guild.id,
			channelId: text.id,
			creatorId: owner.userId,
		});
		const channelWebhooks = await getChannelWebhooks(harness, text.id, owner.token);
		expect(channelWebhooks.some((webhook) => webhook.id === followerWebhookId)).toBe(true);
		await createBuilder(harness, owner.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CHANNEL_HAS_FOLLOWED_CHANNELS)
			.execute();
		expect((await getChannel(harness, owner.token, text.id)).type).toBe(ChannelTypes.GUILD_TEXT);
		await createBuilder(harness, owner.token)
			.delete(`/webhooks/${followerWebhookId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const converted = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(converted.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('an incoming webhook does not block conversion', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Incoming Guild');
		const text = await createChannel(harness, owner.token, guild.id, 'feed', ChannelTypes.GUILD_TEXT);
		await createWebhook(harness, text.id, owner.token, 'Incoming Bot');
		const converted = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(converted.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('records a distinct job for every conversion back to text', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Flip Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		for (const type of [ChannelTypes.GUILD_TEXT, ChannelTypes.GUILD_ANNOUNCEMENT, ChannelTypes.GUILD_TEXT]) {
			await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${announcement.id}`)
				.body({type})
				.expect(HTTP_STATUS.OK)
				.execute();
		}
		const removals = worker.followerRemovals();
		expect(removals).toHaveLength(2);
		const jobKeys = new Set(removals.map((job) => job.options?.jobKey));
		expect(jobKeys.size).toBe(2);
	});

	test('deleting an announcement channel schedules follower removal with source-deleted copies', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Delete Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const text = await createChannel(harness, owner.token, guild.id, 'text', ChannelTypes.GUILD_TEXT);
		await deleteChannel(harness, owner.token, text.id);
		expect(worker.followerRemovals()).toHaveLength(0);
		await deleteChannel(harness, owner.token, announcement.id);
		const removals = worker.followerRemovals();
		expect(removals).toHaveLength(1);
		expect(removals[0]?.payload).toEqual({
			sourceChannelId: announcement.id,
			reason: 'deleted',
			copyMode: 'source_deleted',
		});
		expect(removals[0]?.options?.jobKey).toMatch(new RegExp(`^remove-followers:${announcement.id}:deleted:\\d+$`));
	});

	test('deleting a guild schedules follower removal for each announcement channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Owner Delete Guild');
		const first = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const second = await createChannel(harness, owner.token, guild.id, 'updates', ChannelTypes.GUILD_ANNOUNCEMENT);
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/delete`)
			.body({password: owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		const removals = worker.followerRemovals();
		expect(removals.map((job) => job.payload.sourceChannelId).sort()).toEqual([first.id, second.id].sort());
		for (const job of removals) {
			expect(job.payload.reason).toBe('deleted');
			expect(job.payload.copyMode).toBe('source_deleted');
		}
	});

	test('admin guild deletion purges copies', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup', 'guild:delete']);
		const guild = await createGuild(harness, admin.token, 'Admin Delete Guild');
		const announcement = await createChannel(harness, admin.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${guild.id}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		const removals = worker.followerRemovals();
		expect(removals).toHaveLength(1);
		expect(removals[0]?.payload).toEqual({
			sourceChannelId: announcement.id,
			reason: 'deleted',
			copyMode: 'purge',
		});
	});
});

describe('Announcement channel conversion with the crosspost worker', () => {
	let harness: ApiTestHarness;
	let world: FanoutWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		world = await setupFanoutWorld(harness);
	});

	afterEach(() => {
		disableCrosspostWorker();
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function convert(type: number): Promise<void> {
		await createBuilder<ChannelResponse>(harness, world.a.owner.token)
			.patch(`/channels/${world.a.ann.id}`)
			.body({type})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function followerWebhooks(channelId: string) {
		const webhooks = await new WebhookRepository().listByChannel(createChannelID(BigInt(channelId)));
		return webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER);
	}

	test('converting to text removes every follower after the drain and leaves copies propagating', async () => {
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		const source = await postAndPublish(harness, world, {content: 'before conversion'});
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await convert(ChannelTypes.GUILD_TEXT);
		expect(world.worker.byTask('removeChannelFollowers')).toHaveLength(1);
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(1);
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(0);
		expect(await followerWebhooks(world.b.t2.id)).toHaveLength(0);
		const updates = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'WEBHOOKS_UPDATE' && params.guildId.toString() === world.b.guild.id);
		expect(updates.map((params) => (params.data as {channel_id: string}).channel_id).sort()).toEqual(
			[world.b.t1.id, world.b.t2.id].sort(),
		);
		const audit = await createBuilder<AuditLogResponse>(harness, world.b.owner.token)
			.get(`/guilds/${world.b.guild.id}/audit-logs?action_type=${AuditLogActionType.WEBHOOK_DELETE}`)
			.execute();
		expect(audit.audit_log_entries).toHaveLength(0);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy!.content).toBe('before conversion');
		await editRequest(harness, world.a.owner.token, world.a.ann.id, source.id, {content: 'after conversion'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await world.worker.drain();
		for (const channelId of [world.b.t1.id, world.b.t2.id]) {
			const [updated] = await copiesOf(harness, world.b.owner.token, channelId, source.id);
			expect(updated!.content).toBe('after conversion');
		}
	});

	test('converting back before the drain keeps the followers', async () => {
		await followInto(harness, world, world.b.t1.id);
		await convert(ChannelTypes.GUILD_TEXT);
		await convert(ChannelTypes.GUILD_ANNOUNCEMENT);
		await world.worker.drain();
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(1);
		const source = await postAndPublish(harness, world, {content: 'still followed'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(1);
	});

	test('flipping twice records two removals and ends with no followers', async () => {
		await followInto(harness, world, world.b.t1.id);
		await convert(ChannelTypes.GUILD_TEXT);
		await convert(ChannelTypes.GUILD_ANNOUNCEMENT);
		await convert(ChannelTypes.GUILD_TEXT);
		const removals = world.worker.byTask('removeChannelFollowers');
		expect(new Set(removals.map((job) => job.jobKey)).size).toBe(2);
		await world.worker.drain();
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(0);
	});

	test('publishing or following after converting to text is refused', async () => {
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'late'});
		await convert(ChannelTypes.GUILD_TEXT);
		await publishRequest(harness, world.a.owner.token, world.a.ann.id, message.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
		await followRequest(harness, world.b.owner.token, world.a.ann.id, world.b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
	});
});
