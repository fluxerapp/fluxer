// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageRepository} from '@app/api/channel/repositories/MessageRepository';
import {disableCrosspostWorker} from '@app/api/channel/tests/AnnouncementTestUtils';
import {pinMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {
	type AnnouncementWorld,
	createAnnouncementWorld,
	createStallGate,
	editMessage,
	postMessage,
	publish,
	publishedEditBudgetRemaining,
	type RecordedJob,
	RecordingWorkerService,
	readMessageRow,
} from '@app/api/channel/tests/CrosspostTestUtils';
import {
	copiesOf,
	deleteMessageRequest,
	editRequest,
	type FanoutWorld,
	followInto,
	sendMessage,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {
	CROSSPOST_SOURCE_DELETED_CONTENT,
	CROSSPOST_SYNC_COALESCE_MS,
} from '@fluxer/constants/src/AnnouncementConstants';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

function stallFirstReadOf(messageId: string) {
	const gate = createStallGate();
	const original = MessageRepository.prototype.getMessage;
	let armed = true;
	vi.spyOn(MessageRepository.prototype, 'getMessage').mockImplementation(async function (
		this: MessageRepository,
		channelId,
		id,
	) {
		const result = await original.call(this, channelId, id);
		if (armed && id.toString() === messageId) {
			armed = false;
			await gate.hit();
		}
		return result;
	});
	return gate;
}

function expectUpdateSyncJob(job: RecordedJob | undefined, channelId: string, messageId: string): void {
	expect(job?.payload).toEqual({channelId, messageId, mode: 'update'});
	const runAt = job?.options?.runAt;
	expect(runAt).toBeInstanceOf(Date);
	const bucket = Math.floor((runAt!.getTime() - 1000) / CROSSPOST_SYNC_COALESCE_MS) - 1;
	expect(runAt!.getTime()).toBe((bucket + 1) * CROSSPOST_SYNC_COALESCE_MS + 1000);
	expect(job?.options?.jobKey).toBe(`crosspost-sync:${messageId}:update:${bucket}`);
	expect(job?.options?.skipLedger).toBe(true);
}

describe('Publishing races with other message writers', () => {
	let harness: ApiTestHarness;
	let worker: RecordingWorkerService;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
		world = await createAnnouncementWorld(harness);
	});

	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('a stale author edit that sets flags keeps the published flag, counts toward the cap and propagates', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Draft');
		const gate = stallFirstReadOf(message.id);
		const pendingEdit = editMessage(harness, world.author.token, world.announcement.id, message.id, {
			content: 'Edited while publishing',
			flags: MessageFlags.SUPPRESS_EMBEDS,
		})
			.expect(200)
			.execute();
		await gate.reached;
		await publish(harness, world.author.token, world.announcement.id, message.id);
		gate.release();
		const edited = await pendingEdit;
		expect(edited.flags).toBe(MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS);
		const row = await readMessageRow(world.announcement.id, message.id);
		expect(row?.flags).toBe(MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS);
		expect(row?.content).toBe('Edited while publishing');
		expect(await publishedEditBudgetRemaining(message.id)).toBe(2);
		const syncs = worker.syncJobsFor(message.id);
		expect(syncs).toHaveLength(1);
		expectUpdateSyncJob(syncs[0], world.announcement.id, message.id);
		await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: 'Second edit'})
			.expect(200)
			.execute();
		expect(await publishedEditBudgetRemaining(message.id)).toBe(1);
		expect(worker.syncJobsFor(message.id)).toHaveLength(2);
		await createBuilder(harness, world.author.token)
			.delete(`/channels/${world.announcement.id}/messages/${message.id}`)
			.expect(204)
			.execute();
		const removal = worker.syncJobsFor(message.id).find((job) => job.payload.mode === 'source_deleted');
		expect(removal?.payload).toEqual({
			channelId: world.announcement.id,
			messageId: message.id,
			mode: 'source_deleted',
		});
		expect(removal?.options?.jobKey).toBe(`crosspost-sync:${message.id}:source_deleted`);
		expect(removal?.options?.runAt).toBeUndefined();
	});

	test('a stale moderator suppress-embeds edit keeps the published flag without using the cap', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'See https://example.com');
		const gate = stallFirstReadOf(message.id);
		const pendingEdit = editMessage(harness, world.mod.token, world.announcement.id, message.id, {
			flags: MessageFlags.SUPPRESS_EMBEDS,
		})
			.expect(200)
			.execute();
		await gate.reached;
		await publish(harness, world.author.token, world.announcement.id, message.id);
		gate.release();
		const edited = await pendingEdit;
		expect(edited.flags).toBe(MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS);
		expect((await readMessageRow(world.announcement.id, message.id))?.flags).toBe(
			MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS,
		);
		expect(await publishedEditBudgetRemaining(message.id)).toBe(3);
		const syncs = worker.syncJobsFor(message.id);
		expect(syncs).toHaveLength(1);
		expectUpdateSyncJob(syncs[0], world.announcement.id, message.id);
	});

	test('a stale webhook token edit keeps the published flag and counts toward the cap', async () => {
		const webhook = await createWebhook(harness, world.announcement.id, world.owner.token, 'Release Bot');
		const executed = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({content: 'Build 42 shipped'})
			.expect(200)
			.execute();
		const gate = stallFirstReadOf(executed.id);
		const pendingEdit = createBuilderWithoutAuth<MessageResponse>(harness)
			.patch(`/webhooks/${webhook.id}/${webhook.token}/messages/${executed.id}`)
			.body({content: 'Build 42 shipped (edited)', flags: MessageFlags.SUPPRESS_EMBEDS})
			.expect(200)
			.execute();
		await gate.reached;
		await publish(harness, world.mod.token, world.announcement.id, executed.id);
		gate.release();
		await pendingEdit;
		const row = await readMessageRow(world.announcement.id, executed.id);
		expect(row?.flags).toBe(MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS);
		expect(row?.content).toBe('Build 42 shipped (edited)');
		expect(await publishedEditBudgetRemaining(executed.id)).toBe(2);
		expect(worker.syncJobsFor(executed.id)).toHaveLength(1);
	});

	test('a pin concurrent with a publish keeps both', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Pin and publish');
		await Promise.all([
			pinMessage(harness, world.owner.token, world.announcement.id, message.id),
			publish(harness, world.author.token, world.announcement.id, message.id),
		]);
		const row = await readMessageRow(world.announcement.id, message.id);
		expect(row?.flags).toBe(MessageFlags.CROSSPOSTED);
		expect(row?.pinnedTimestamp).not.toBeNull();
	});
});

describe('Publishing races with a follower attached', () => {
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

	test('a stale flag edit during a publish reaches the copy and a later delete marks it', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'Draft'});
		const gate = stallFirstReadOf(message.id);
		const pendingEdit = editRequest(harness, world.a.owner.token, world.a.ann.id, message.id, {
			content: 'Edited while publishing',
			flags: MessageFlags.SUPPRESS_EMBEDS,
		})
			.expect(200)
			.execute();
		await gate.reached;
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		gate.release();
		await pendingEdit;
		await world.worker.drain();
		let [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('Edited while publishing');
		expect(copy!.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SUPPRESS_EMBEDS);
		await editRequest(harness, world.a.owner.token, world.a.ann.id, message.id, {content: 'Second edit'})
			.expect(200)
			.execute();
		await world.worker.drain();
		[copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('Second edit');
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, message.id);
		await world.worker.drain();
		[copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED);
		expect(copy!.content).toBe(CROSSPOST_SOURCE_DELETED_CONTENT);
		expect(world.worker.deadLetters).toHaveLength(0);
	});
});
