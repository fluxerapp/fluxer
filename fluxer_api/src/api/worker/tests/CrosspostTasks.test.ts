// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createMessageID, createWebhookID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {CrosspostTaskNames} from '@app/api/channel/services/message/CrosspostPropagation';
import {disableCrosspostWorker, follow} from '@app/api/channel/tests/AnnouncementTestUtils';
import {
	type FanoutWorld,
	followInto,
	patchChannelRow,
	postAndPublish,
	setupFanoutWorld,
	sourceIndex,
	workerHelpers,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import crosspostMessage from '@app/api/worker/tasks/CrosspostMessage';
import removeChannelFollowers from '@app/api/worker/tasks/RemoveChannelFollowers';
import syncCrosspostedMessage from '@app/api/worker/tasks/SyncCrosspostedMessage';
import {ChannelTypes, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const pageSizes = vi.hoisted(() => ({chunk: null as number | null, page: null as number | null}));

vi.mock('@fluxer/constants/src/AnnouncementConstants', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@fluxer/constants/src/AnnouncementConstants')>();
	return {
		...actual,
		get CROSSPOST_FANOUT_CHUNK_SIZE() {
			return pageSizes.chunk ?? actual.CROSSPOST_FANOUT_CHUNK_SIZE;
		},
		get CROSSPOST_FANOUT_PAGE_SIZE() {
			return pageSizes.page ?? actual.CROSSPOST_FANOUT_PAGE_SIZE;
		},
	};
});

describe('crosspost tasks against storage', () => {
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
		pageSizes.chunk = null;
		pageSizes.page = null;
		disableCrosspostWorker();
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function followerIds(): Promise<Array<string>> {
		const entries = await new WebhookRepository().listIdsBySourceChannel(createChannelID(BigInt(world.a.ann.id)), {
			limit: 100,
		});
		return entries.map((entry) => entry.webhookId.toString());
	}

	test('removeChannelFollowers removes every follower once and announces each target', async () => {
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		await patchChannelRow(world.a.ann.id, {type: ChannelTypes.GUILD_TEXT});
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const payload = {sourceChannelId: world.a.ann.id, reason: 'converted'};
		await removeChannelFollowers(payload, workerHelpers(world.worker));
		expect(await followerIds()).toHaveLength(0);
		const updates = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'WEBHOOKS_UPDATE');
		expect(updates.map((params) => (params.data as {channel_id: string}).channel_id).sort()).toEqual(
			[world.b.t1.id, world.b.t2.id].sort(),
		);
		expect(updates.every((params) => params.guildId.toString() === world.b.guild.id)).toBe(true);
		dispatchSpy.mockClear();
		await removeChannelFollowers(payload, workerHelpers(world.worker));
		expect(dispatchSpy.mock.calls.filter(([params]) => params.event === 'WEBHOOKS_UPDATE')).toHaveLength(0);
		const webhooks = await new WebhookRepository().listByChannel(createChannelID(BigInt(world.b.t1.id)));
		expect(webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER)).toHaveLength(0);
	});

	test('removeChannelFollowers keeps followers when the channel is an announcement channel again', async () => {
		await followInto(harness, world, world.b.t1.id);
		await removeChannelFollowers({sourceChannelId: world.a.ann.id, reason: 'converted'}, workerHelpers(world.worker));
		expect(await followerIds()).toHaveLength(1);
	});

	test('removeChannelFollowers with a copy mode queues one sync per published message, across pages', async () => {
		await followInto(harness, world, world.b.t1.id);
		const published = [];
		for (const content of ['one', 'two', 'three']) {
			published.push(await postAndPublish(harness, world, {content}));
		}
		expect((await sourceIndex(world.a.ann.id)).sort()).toEqual(published.map((message) => message.id).sort());
		pageSizes.page = 2;
		const before = world.worker.recorded.length;
		await removeChannelFollowers(
			{sourceChannelId: world.a.ann.id, reason: 'deleted', copyMode: 'purge'},
			workerHelpers(world.worker),
		);
		const queued = world.worker.recorded.slice(before);
		expect(queued.every((job) => job.taskType === CrosspostTaskNames.SYNC_CROSSPOSTED_MESSAGE)).toBe(true);
		expect(queued.map((job) => (job.payload as {messageId: string}).messageId).sort()).toEqual(
			published.map((message) => message.id).sort(),
		);
		expect(queued.map((job) => job.payload.mode)).toEqual(['purge', 'purge', 'purge']);
		expect(queued.map((job) => job.jobKey).sort()).toEqual(
			published.map((message) => `crosspost-sync:${message.id}:purge`).sort(),
		);
		expect(await followerIds()).toHaveLength(0);
	});

	test('syncCrosspostedMessage pages through every mapping row', async () => {
		const source = await postAndPublish(harness, world, {content: 'many followers'});
		const repository = new ChannelRepository();
		const webhookIds = [
			'900000000000000001',
			'900000000000000002',
			'900000000000000003',
			'900000000000000004',
			'900000000000000005',
		];
		for (const webhookId of webhookIds) {
			await repository.crossposts.insertPending({
				source_message_id: createMessageID(BigInt(source.id)),
				webhook_id: createWebhookID(BigInt(webhookId)),
				source_channel_id: createChannelID(BigInt(world.a.ann.id)),
				target_guild_id: createGuildID(BigInt(world.b.guild.id)),
				target_channel_id: createChannelID(BigInt(world.b.t1.id)),
				target_message_id: createMessageID(BigInt(webhookId)),
				state: 'pending',
				reserved_at: new Date(Date.now() - 120_000),
				source_fingerprint: null,
				created_at: new Date(),
			});
		}
		pageSizes.page = 2;
		pageSizes.chunk = 1;
		const before = world.worker.recorded.length;
		await syncCrosspostedMessage(
			{channelId: world.a.ann.id, messageId: source.id, mode: 'source_deleted'},
			workerHelpers(world.worker),
		);
		const chunks = world.worker.recorded.slice(before);
		expect(chunks.map((job) => job.taskType)).toEqual(Array(5).fill(CrosspostTaskNames.SYNC_CROSSPOST_COPIES));
		expect(chunks.map((job) => (job.payload as {webhookIds: Array<string>}).webhookIds)).toEqual(
			webhookIds.map((webhookId) => [webhookId]),
		);
		expect(chunks.map((job) => job.jobKey)).toEqual(
			webhookIds.map((webhookId) => `crosspost-sync-chunk:${source.id}:source_deleted:source_deleted:${webhookId}`),
		);
		expect(await sourceIndex(world.a.ann.id)).not.toContain(source.id);
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		const remaining = await repository.crossposts.listBySourceMessage(createMessageID(BigInt(source.id)), {
			limit: 100,
		});
		expect(remaining).toHaveLength(0);
	});

	test('crosspostMessage does nothing for a message that is not published', async () => {
		await follow(harness, world.b.owner.token, world.a.ann.id, world.b.t1.id);
		await world.worker.drain();
		const before = world.worker.recorded.length;
		await crosspostMessage({channelId: world.a.ann.id, messageId: '123456789012345678'}, workerHelpers(world.worker));
		expect(world.worker.recorded.length).toBe(before);
	});
});
