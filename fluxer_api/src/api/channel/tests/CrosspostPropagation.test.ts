// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {crosspostSyncBucket, enqueueCrosspostSync} from '@app/api/channel/services/message/CrosspostPropagation';
import {
	addGuildMember,
	createAnnouncementTargetGuild,
	disableCrosspostWorker,
	enableCrosspostWorker,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {addGuildFeature, createTestChannelService} from '@app/api/channel/tests/CrosspostTestUtils';
import {
	copiesOf,
	deleteMessageRequest,
	editRequest,
	type FanoutWorld,
	followInto,
	listCopies,
	mappingRow,
	mappingRows,
	patchChannel,
	postAndPublish,
	publishAndDrain,
	readRow,
	sendMessage,
	sendWithImage,
	setupFanoutWorld,
	sourceIndex,
	workerHelpers,
	writeRow,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import type {EmbedService} from '@app/api/infrastructure/EmbedService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import type {Embed} from '@app/api/models/Embed';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {getExpiryBucket} from '@app/api/utils/AttachmentDecay';
import {createWebhook, executeWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import deleteUserMessagesInGuildByTime from '@app/api/worker/tasks/DeleteUserMessagesInGuildByTime';
import {processExpiredAttachments} from '@app/api/worker/tasks/ExpireAttachments';
import extractEmbeds from '@app/api/worker/tasks/ExtractEmbeds';
import messageShred from '@app/api/worker/tasks/MessageShred';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	CROSSPOST_SOURCE_DELETED_CONTENT,
	CROSSPOST_SYNC_COALESCE_MS,
} from '@fluxer/constants/src/AnnouncementConstants';
import {
	ChannelTypes,
	MessageAttachmentFlags,
	MessageFlags,
	MessageReferenceTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const DELETED_COPY_FLAGS = MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED;

async function awaitFreshBucket(): Promise<void> {
	const intoBucket = Date.now() % CROSSPOST_SYNC_COALESCE_MS;
	if (intoBucket > CROSSPOST_SYNC_COALESCE_MS - 2000) {
		await new Promise((resolve) => setTimeout(resolve, CROSSPOST_SYNC_COALESCE_MS - intoBucket + 50));
	}
}

describe('Crosspost propagation', () => {
	let harness: ApiTestHarness;
	let world: FanoutWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		harness.storageService.reset();
		world = await setupFanoutWorld(harness);
	});

	afterEach(() => {
		disableCrosspostWorker();
		resetChannelThreadsConfig();
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function onlyCopy(channelId: string, sourceId: string, token = world.b.owner.token): Promise<MessageResponse> {
		const copies = await copiesOf(harness, token, channelId, sourceId);
		expect(copies).toHaveLength(1);
		return copies[0]!;
	}

	async function edit(messageId: string, body: Record<string, unknown>, token = world.a.owner.token) {
		await editRequest(harness, token, world.a.ann.id, messageId, body).expect(200).execute();
	}

	async function expectDeletedCopy(channelId: string, copyId: string): Promise<void> {
		const row = (await readRow(channelId, copyId))!;
		expect(row.flags).toBe(DELETED_COPY_FLAGS);
		expect(row.content).toBe(CROSSPOST_SOURCE_DELETED_CONTENT);
		expect(row.attachments).toHaveLength(0);
		expect(row.embeds).toHaveLength(0);
		expect(row.stickers).toHaveLength(0);
		expect(row.editedTimestamp).not.toBeNull();
	}

	test('a content edit updates every copy through one coalesced sync job', async () => {
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		const source = await postAndPublish(harness, world, {content: 'draft'});
		const copyT1 = await onlyCopy(world.b.t1.id, source.id);
		const copyT2 = await onlyCopy(world.b.t2.id, source.id);
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const before = crosspostSyncBucket(Date.now());
		await edit(source.id, {content: 'final'});
		const after = crosspostSyncBucket(Date.now());
		const [job] = world.worker
			.byTask('syncCrosspostedMessage')
			.filter((entry) => (entry.payload as {messageId: string}).messageId === source.id);
		const bucket = Number(job!.jobKey!.split(':').at(-1));
		expect(job!.jobKey).toBe(`crosspost-sync:${source.id}:update:${bucket}`);
		expect(bucket).toBeGreaterThanOrEqual(before);
		expect(bucket).toBeLessThanOrEqual(after);
		expect(job!.runAt!.getTime()).toBe((bucket + 1) * CROSSPOST_SYNC_COALESCE_MS + 1000);
		expect(job!.options?.skipLedger).toBe(true);
		await world.worker.drain();
		for (const [channelId, copyId] of [
			[world.b.t1.id, copyT1.id],
			[world.b.t2.id, copyT2.id],
		] as const) {
			const updated = await onlyCopy(channelId, source.id);
			expect(updated.content).toBe('final');
			expect(updated.edited_timestamp).toBeTruthy();
			const updates = dispatchSpy.mock.calls
				.map(([params]) => params)
				.filter(
					(params) =>
						params.event === 'MESSAGE_UPDATE' &&
						params.guildId.toString() === world.b.guild.id &&
						(params.data as {id: string}).id === copyId,
				);
			expect(updates).toHaveLength(1);
		}
	});

	test('two edits in one bucket run a single sync and the copy ends with the second', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'first'});
		await awaitFreshBucket();
		await edit(source.id, {content: 'second'});
		await edit(source.id, {content: 'third'});
		const syncJobs = world.worker
			.byTask('syncCrosspostedMessage')
			.filter((entry) => (entry.payload as {messageId: string}).messageId === source.id);
		expect(syncJobs).toHaveLength(1);
		expect(world.worker.deduped.filter((entry) => entry.taskType === 'syncCrosspostedMessage')).toHaveLength(1);
		await world.worker.drain();
		expect((await onlyCopy(world.b.t1.id, source.id)).content).toBe('third');
	});

	test('embed edits and suppress-embeds toggles propagate, including flag-only edits', async () => {
		await followInto(harness, world, world.b.t1.id);
		const embedded = await postAndPublish(harness, world, {content: 'card', embeds: [{title: 'Old title'}]});
		await edit(embedded.id, {embeds: [{title: 'New title', description: 'Details'}]});
		await world.worker.drain();
		let copyRow = (await readRow(world.b.t1.id, (await onlyCopy(world.b.t1.id, embedded.id)).id))!;
		expect(copyRow.embeds.map((embed) => [embed.title, embed.description])).toEqual([['New title', 'Details']]);
		await edit(embedded.id, {flags: MessageFlags.SUPPRESS_EMBEDS});
		await world.worker.drain();
		copyRow = (await readRow(world.b.t1.id, copyRow.id.toString()))!;
		expect(copyRow.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SUPPRESS_EMBEDS);
		await edit(embedded.id, {flags: 0}, world.a.moderator.token);
		await world.worker.drain();
		copyRow = (await readRow(world.b.t1.id, copyRow.id.toString()))!;
		expect(copyRow.flags).toBe(MessageFlags.IS_CROSSPOST);
		const plain = await postAndPublish(harness, world, {content: 'plain'});
		await awaitFreshBucket();
		await edit(plain.id, {content: 'plain edited'});
		await world.worker.drain();
		const plainCopy = await onlyCopy(world.b.t1.id, plain.id);
		const editedAt = (await readRow(world.a.ann.id, plain.id))!.editedTimestamp;
		await edit(plain.id, {flags: MessageFlags.SUPPRESS_EMBEDS});
		expect((await readRow(world.a.ann.id, plain.id))!.editedTimestamp).toEqual(editedAt);
		await world.worker.drain();
		const plainRow = (await readRow(world.b.t1.id, plainCopy.id))!;
		expect(plainRow.content).toBe('plain edited');
		expect(plainRow.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SUPPRESS_EMBEDS);
	});

	test('removing one attachment by an edit drops it from the copy and deletes no copy objects', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'two files'}, [
			'keep.png',
			'drop.gif',
		]);
		await publishAndDrain(harness, world, source.id);
		const copy = await onlyCopy(world.b.t1.id, source.id);
		const before = (await readRow(world.b.t1.id, copy.id))!.attachments;
		expect(before).toHaveLength(2);
		const sourceRow = (await readRow(world.a.ann.id, source.id))!;
		await edit(source.id, {attachments: [{id: sourceRow.attachments[0]!.id.toString()}]});
		await world.worker.drain();
		expect(before.map((attachment) => attachment.id)).toEqual(sourceRow.attachments.map((attachment) => attachment.id));
		const after = (await readRow(world.b.t1.id, copy.id))!.attachments;
		expect(after.map((attachment) => attachment.id)).toEqual([before[0]!.id]);
		const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
		expect(deleted.some((key) => key.startsWith(`attachments/${world.b.t1.id}/`))).toBe(false);
		expect(deleted).not.toContain(`attachments/${world.a.ann.id}/${before[0]!.id}/${before[0]!.filename}`);
		const [rendered] = (await listCopies(harness, world.b.owner.token, world.b.t1.id)).filter(
			(message) => message.id === copy.id,
		);
		expect(rendered!.attachments?.map((attachment) => attachment.url?.split('?')[0])).toEqual([
			expect.stringContaining(`/attachments/${world.a.ann.id}/${before[0]!.id}/${before[0]!.filename}`),
		]);
	});

	test('forwarding a copy clones the shared file into the forward channel', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'forward the file'});
		await publishAndDrain(harness, world, source.id);
		const copy = await onlyCopy(world.b.t1.id, source.id);
		const [attachment] = (await readRow(world.a.ann.id, source.id))!.attachments;
		const sourceKey = `attachments/${world.a.ann.id}/${attachment!.id}/${attachment!.filename}`;
		const copiedBefore = harness.storageService.getCopiedObjects().length;
		const forwarded = await createBuilder<MessageResponse>(harness, world.b.owner.token)
			.post(`/channels/${world.b.t2.id}/messages`)
			.body({
				message_reference: {
					type: MessageReferenceTypes.FORWARD,
					message_id: copy.id,
					channel_id: world.b.t1.id,
					guild_id: world.b.guild.id,
				},
			})
			.expect(200)
			.execute();
		const clones = harness.storageService.getCopiedObjects().slice(copiedBefore);
		expect(clones).toHaveLength(1);
		expect(clones[0]!.sourceKey).toBe(sourceKey);
		expect(clones[0]!.destinationKey.startsWith(`attachments/${world.b.t2.id}/`)).toBe(true);
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, source.id);
		await world.worker.drain();
		const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
		expect(deleted).toContain(sourceKey);
		expect(deleted).not.toContain(clones[0]!.destinationKey);
		expect(forwarded.message_snapshots?.[0]?.attachments).toHaveLength(1);
	});

	test('attachment metadata edits reach the copy under the same attachment id', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'alt text'});
		await publishAndDrain(harness, world, source.id);
		const copy = await onlyCopy(world.b.t1.id, source.id);
		const sourceAttachment = (await readRow(world.a.ann.id, source.id))!.attachments[0]!;
		const attachmentId = (await readRow(world.b.t1.id, copy.id))!.attachments[0]!.id;
		expect(attachmentId).toBe(sourceAttachment.id);
		const copiedBefore = harness.storageService.getCopiedObjects().length;
		await edit(source.id, {attachments: [{id: sourceAttachment.id.toString(), description: 'A cat'}]});
		await world.worker.drain();
		let copied = (await readRow(world.b.t1.id, copy.id))!.attachments[0]!;
		expect(copied.id).toBe(attachmentId);
		expect(copied.description).toBe('A cat');
		const edited = (await readRow(world.a.ann.id, source.id))!;
		await writeRow(edited, {
			attachments: edited.attachments.map((attachment) => ({
				...attachment.toMessageAttachment(),
				flags: attachment.flags | MessageAttachmentFlags.IS_SPOILER,
			})),
		});
		await enqueueCrosspostSync(world.worker, {
			channelId: createChannelID(BigInt(world.a.ann.id)),
			messageId: createMessageID(BigInt(source.id)),
			mode: 'update',
		});
		await world.worker.drain();
		copied = (await readRow(world.b.t1.id, copy.id))!.attachments[0]!;
		expect(copied.id).toBe(attachmentId);
		expect(copied.flags & MessageAttachmentFlags.IS_SPOILER).toBe(MessageAttachmentFlags.IS_SPOILER);
		expect(harness.storageService.getCopiedObjects()).toHaveLength(copiedBefore);
	});

	test('a late unfurl on a published source propagates its embeds', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'read https://example.com/post'});
		const embedService = {
			processUrlWithCachePolicy: async (url: string) => ({
				embeds: [
					{
						toMessageEmbed: () => ({
							type: 'link',
							url,
							title: 'Example post',
							description: null,
							timestamp: null,
							color: null,
							author: null,
							provider: null,
							thumbnail: null,
							image: null,
							video: null,
							footer: null,
							fields: null,
							nsfw: null,
						}),
					} as unknown as Embed,
				],
				cacheTtlSeconds: 0,
			}),
			cacheEmbeds: async () => {},
		} as unknown as EmbedService;
		disableCrosspostWorker();
		world.worker = enableCrosspostWorker({}, {embedService});
		await extractEmbeds(
			{channelId: world.a.ann.id, messageId: source.id, guildId: world.a.guild.id, nsfwMode: 'allow'},
			workerHelpers(world.worker),
		);
		expect(world.worker.byTask('syncCrosspostedMessage')).toHaveLength(1);
		await world.worker.drain();
		const copyRow = (await readRow(world.b.t1.id, (await onlyCopy(world.b.t1.id, source.id)).id))!;
		expect(copyRow.embeds.map((embed) => embed.title)).toEqual(['Example post']);
	});

	test('a sync with nothing new writes nothing', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'steady'});
		const copy = await onlyCopy(world.b.t1.id, source.id);
		const before = await readRow(world.b.t1.id, copy.id);
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await enqueueCrosspostSync(world.worker, {
			channelId: createChannelID(BigInt(world.a.ann.id)),
			messageId: createMessageID(BigInt(source.id)),
			mode: 'update',
		});
		await world.worker.drain();
		expect((await readRow(world.b.t1.id, copy.id))?.version).toBe(before?.version);
		expect(dispatchSpy.mock.calls.filter(([params]) => params.event === 'MESSAGE_UPDATE')).toHaveLength(0);
	});

	test('deleting the source marks every copy and cleans up the mapping and index rows', async () => {
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {
			content: 'goodbye',
			embeds: [{title: 'Card'}],
		});
		await publishAndDrain(harness, world, source.id);
		const copyT1 = await onlyCopy(world.b.t1.id, source.id);
		const copyT2 = await onlyCopy(world.b.t2.id, source.id);
		const sourceKeys = (await readRow(world.a.ann.id, source.id))!.attachments.map(
			(attachment) => `attachments/${world.a.ann.id}/${attachment.id}/${attachment.filename}`,
		);
		expect(sourceKeys).toHaveLength(1);
		for (const [channelId, copyId] of [
			[world.b.t1.id, copyT1.id],
			[world.b.t2.id, copyT2.id],
		] as const) {
			const copyKeys = (await readRow(channelId, copyId))!.attachments.map(
				(attachment) => `attachments/${world.a.ann.id}/${attachment.id}/${attachment.filename}`,
			);
			expect(copyKeys).toEqual(sourceKeys);
		}
		expect(await sourceIndex(world.a.ann.id)).toContain(source.id);
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, source.id);
		await world.worker.drain();
		await expectDeletedCopy(world.b.t1.id, copyT1.id);
		await expectDeletedCopy(world.b.t2.id, copyT2.id);
		const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
		for (const key of sourceKeys) {
			expect(deleted.filter((entry) => entry === key)).toHaveLength(1);
		}
		expect(deleted.some((key) => key.startsWith(`attachments/${world.b.t1.id}/`))).toBe(false);
		expect(deleted.some((key) => key.startsWith(`attachments/${world.b.t2.id}/`))).toBe(false);
		expect(await mappingRows(source.id)).toHaveLength(0);
		expect(await sourceIndex(world.a.ann.id)).not.toContain(source.id);
	});

	test('deleting the source keeps the thread a follower started from its copy', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'thread me'});
		const copy = await onlyCopy(world.b.t1.id, source.id);
		await threadsRequest(harness, world.b.owner.token)
			.post(`/channels/${world.b.t1.id}/messages/${copy.id}/threads`)
			.body({name: 'discussion'})
			.expect(201)
			.execute();
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, source.id);
		await world.worker.drain();
		const row = (await readRow(world.b.t1.id, copy.id))!;
		expect(row.flags).toBe(DELETED_COPY_FLAGS | ServerMessageFlags.HAS_THREAD);
		expect(row.content).toBe(CROSSPOST_SOURCE_DELETED_CONTENT);
		const rendered = await threadsRequest<MessageResponse & {thread?: {id: string}}>(harness, world.b.owner.token)
			.get(`/channels/${world.b.t1.id}/messages/${copy.id}`)
			.execute();
		expect(rendered.thread?.id).toBe(copy.id);
	});

	describe('other delete paths mark copies too', () => {
		test('bulk delete', async () => {
			await followInto(harness, world, world.b.t1.id);
			const first = await postAndPublish(harness, world, {content: 'one'});
			const second = await postAndPublish(harness, world, {content: 'two'});
			await createBuilder(harness, world.a.owner.token)
				.post(`/channels/${world.a.ann.id}/messages/bulk-delete`)
				.body({message_ids: [first.id, second.id]})
				.expect(204)
				.execute();
			await world.worker.drain();
			for (const source of [first, second]) {
				await expectDeletedCopy(world.b.t1.id, (await onlyCopy(world.b.t1.id, source.id)).id);
			}
		});

		test('a ban that deletes recent messages', async () => {
			disableCrosspostWorker();
			world.worker = enableCrosspostWorker({deleteUserMessagesInGuildByTime});
			await followInto(harness, world, world.b.t1.id);
			const source = await sendMessage(harness, world.a.member.token, world.a.ann.id, {content: 'soon banned'});
			await publishAndDrain(harness, world, source.id, world.a.member.token);
			const copy = await onlyCopy(world.b.t1.id, source.id);
			await createBuilder(harness, world.a.owner.token)
				.put(`/guilds/${world.a.guild.id}/bans/${world.a.member.userId}`)
				.body({delete_message_seconds: 3600})
				.expect(204)
				.execute();
			await world.worker.drain();
			expect(await readRow(world.a.ann.id, source.id)).toBeNull();
			await expectDeletedCopy(world.b.t1.id, copy.id);
		});

		test('a self bulk delete', async () => {
			await followInto(harness, world, world.b.t1.id);
			const source = await sendMessage(harness, world.a.member.token, world.a.ann.id, {content: 'mine'});
			await publishAndDrain(harness, world, source.id, world.a.member.token);
			const copy = await onlyCopy(world.b.t1.id, source.id);
			await createTestChannelService().userMessageDeletion.deleteUserMessagesInScope(
				createUserID(BigInt(world.a.member.userId)),
				{channelIds: [createChannelID(BigInt(world.a.ann.id))]},
			);
			await world.worker.drain();
			expect(await readRow(world.a.ann.id, source.id)).toBeNull();
			await expectDeletedCopy(world.b.t1.id, copy.id);
		});

		test('a webhook token delete of a published webhook message', async () => {
			await followInto(harness, world, world.b.t1.id);
			const incoming = await createWebhook(harness, world.a.ann.id, world.a.owner.token, 'Releases');
			const {json} = await executeWebhook(harness, incoming.id, incoming.token, {content: 'bot post', wait: true}, 200);
			await publishAndDrain(harness, world, json!.id);
			const copy = await onlyCopy(world.b.t1.id, json!.id);
			await createBuilderWithoutAuth(harness)
				.delete(`/webhooks/${incoming.id}/${incoming.token}/messages/${json!.id}`)
				.expect(204)
				.execute();
			await world.worker.drain();
			await expectDeletedCopy(world.b.t1.id, copy.id);
		});
	});

	describe('takedowns delete copies', () => {
		test('an admin delete of the source', async () => {
			await followInto(harness, world, world.b.t1.id);
			const source = await postAndPublish(harness, world, {content: 'illegal'});
			const copy = await onlyCopy(world.b.t1.id, source.id);
			const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const admin = await setUserACLs(harness, world.a.member, [AdminACLs.AUTHENTICATE, AdminACLs.MESSAGE_DELETE]);
			await createBuilder(harness, admin.token)
				.delete(`/admin/channels/${world.a.ann.id}/messages/${source.id}`)
				.expect(200)
				.execute();
			await world.worker.drain();
			expect(await readRow(world.b.t1.id, copy.id)).toBeNull();
			expect(await listCopies(harness, world.b.owner.token, world.b.t1.id)).toHaveLength(0);
			expect(
				dispatchSpy.mock.calls.some(
					([params]) =>
						params.event === 'MESSAGE_DELETE' &&
						params.guildId.toString() === world.b.guild.id &&
						(params.data as {id: string}).id === copy.id,
				),
			).toBe(true);
			expect(await mappingRows(source.id)).toHaveLength(0);
		});

		test('a message shred of the source', async () => {
			disableCrosspostWorker();
			world.worker = enableCrosspostWorker({}, {kvClient: getKVClient()});
			await followInto(harness, world, world.b.t1.id);
			const source = await sendMessage(harness, world.a.member.token, world.a.ann.id, {content: 'shred me'});
			await publishAndDrain(harness, world, source.id, world.a.member.token);
			const copy = await onlyCopy(world.b.t1.id, source.id);
			await messageShred(
				{
					job_id: 'crosspost-propagation-shred',
					admin_user_id: '1',
					target_user_id: world.a.member.userId,
					entries: [{channel_id: world.a.ann.id, message_id: source.id}],
				},
				workerHelpers(world.worker),
			);
			await world.worker.drain();
			expect(await readRow(world.a.ann.id, source.id)).toBeNull();
			expect(await readRow(world.b.t1.id, copy.id)).toBeNull();
		});
	});

	describe('removing a copy keeps the source files', () => {
		async function publishedCopyWithFile(): Promise<{sourceId: string; copyId: string; sourceKey: string}> {
			await followInto(harness, world, world.b.t1.id);
			await followInto(harness, world, world.b.t2.id);
			const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'shared file'});
			await publishAndDrain(harness, world, source.id);
			const [attachment] = (await readRow(world.a.ann.id, source.id))!.attachments;
			const sourceKey = `attachments/${world.a.ann.id}/${attachment!.id}/${attachment!.filename}`;
			const copy = await onlyCopy(world.b.t1.id, source.id);
			expect((await readRow(world.b.t1.id, copy.id))!.attachments.map((entry) => entry.id)).toEqual([attachment!.id]);
			return {sourceId: source.id, copyId: copy.id, sourceKey};
		}

		async function expectSourceFileKept(sourceId: string, sourceKey: string): Promise<void> {
			await world.worker.drain();
			const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
			expect(deleted).not.toContain(sourceKey);
			expect(deleted.some((key) => key.startsWith('attachments/'))).toBe(false);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, sourceKey)).toBe(true);
			expect((await readRow(world.a.ann.id, sourceId))!.attachments).toHaveLength(1);
			const sibling = await onlyCopy(world.b.t2.id, sourceId);
			expect(sibling.attachments?.[0]?.url).toContain(`/${sourceKey}`);
		}

		test('a moderator delete in the following channel', async () => {
			const {sourceId, copyId, sourceKey} = await publishedCopyWithFile();
			await deleteMessageRequest(harness, world.b.owner.token, world.b.t1.id, copyId);
			expect(await readRow(world.b.t1.id, copyId)).toBeNull();
			await expectSourceFileKept(sourceId, sourceKey);
		});

		test('a bulk delete in the following channel', async () => {
			const {sourceId, copyId, sourceKey} = await publishedCopyWithFile();
			const other = await sendMessage(harness, world.b.owner.token, world.b.t1.id, {content: 'local'});
			await createBuilder(harness, world.b.owner.token)
				.post(`/channels/${world.b.t1.id}/messages/bulk-delete`)
				.body({message_ids: [copyId, other.id]})
				.expect(204)
				.execute();
			expect(await readRow(world.b.t1.id, copyId)).toBeNull();
			await expectSourceFileKept(sourceId, sourceKey);
		});

		test('deleting the following channel', async () => {
			const {sourceId, sourceKey} = await publishedCopyWithFile();
			await createBuilder(harness, world.b.owner.token).delete(`/channels/${world.b.t1.id}`).expect(204).execute();
			await expectSourceFileKept(sourceId, sourceKey);
		});

		test('deleting the following guild', async () => {
			const {sourceId, sourceKey} = await publishedCopyWithFile();
			await createBuilder(harness, world.b.owner.token)
				.post(`/guilds/${world.b.guild.id}/delete`)
				.body({password: world.b.owner.password})
				.expect(204)
				.execute();
			await world.worker.drain();
			const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
			expect(deleted).not.toContain(sourceKey);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, sourceKey)).toBe(true);
			expect((await readRow(world.a.ann.id, sourceId))!.attachments).toHaveLength(1);
		});
	});

	describe('deleting the source channel after it published', () => {
		async function publishedWithFile(): Promise<{sourceId: string; copyId: string; sourceKey: string}> {
			await followInto(harness, world, world.b.t1.id);
			const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'channel file'});
			await publishAndDrain(harness, world, source.id);
			const [attachment] = (await readRow(world.a.ann.id, source.id))!.attachments;
			const copy = await onlyCopy(world.b.t1.id, source.id);
			return {
				sourceId: source.id,
				copyId: copy.id,
				sourceKey: `attachments/${world.a.ann.id}/${attachment!.id}/${attachment!.filename}`,
			};
		}

		async function convertToText(): Promise<void> {
			await patchChannel(harness, world.a.owner.token, world.a.ann.id, {type: ChannelTypes.GUILD_TEXT})
				.expect(200)
				.execute();
			await world.worker.drain();
		}

		async function expectCopyMarkedAndSourcePurged(sourceId: string, copyId: string, sourceKey: string) {
			await world.worker.drain();
			await expectDeletedCopy(world.b.t1.id, copyId);
			expect(await mappingRows(sourceId)).toHaveLength(0);
			expect(await sourceIndex(world.a.ann.id)).toEqual([]);
			const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
			expect(deleted.filter((key) => key === sourceKey)).toHaveLength(1);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, sourceKey)).toBe(false);
		}

		test('a channel converted to text and then deleted marks its copies', async () => {
			const {sourceId, copyId, sourceKey} = await publishedWithFile();
			await convertToText();
			expect((await readRow(world.b.t1.id, copyId))!.content).toBe('channel file');
			await createBuilder(harness, world.a.owner.token).delete(`/channels/${world.a.ann.id}`).expect(204).execute();
			await expectCopyMarkedAndSourcePurged(sourceId, copyId, sourceKey);
		});

		test('a guild deleted after its channel was converted to text marks the copies and purges the files', async () => {
			const {sourceId, copyId, sourceKey} = await publishedWithFile();
			await convertToText();
			await createBuilder(harness, world.a.owner.token)
				.post(`/guilds/${world.a.guild.id}/delete`)
				.body({password: world.a.owner.password})
				.expect(204)
				.execute();
			await expectCopyMarkedAndSourcePurged(sourceId, copyId, sourceKey);
		});

		test('deleting the source guild purges the source files', async () => {
			const {sourceId, copyId, sourceKey} = await publishedWithFile();
			await createBuilder(harness, world.a.owner.token)
				.post(`/guilds/${world.a.guild.id}/delete`)
				.body({password: world.a.owner.password})
				.expect(204)
				.execute();
			await expectCopyMarkedAndSourcePurged(sourceId, copyId, sourceKey);
		});

		test('a failed follower removal enqueue fails the delete before anything is purged', async () => {
			const {sourceId, copyId, sourceKey} = await publishedWithFile();
			world.worker.failure = (taskType) => (taskType === 'removeChannelFollowers' ? new Error('queue down') : null);
			try {
				await createBuilder(harness, world.a.owner.token).delete(`/channels/${world.a.ann.id}`).expect(500).execute();
			} finally {
				world.worker.failure = null;
			}
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, sourceKey)).toBe(true);
			expect(await readRow(world.a.ann.id, sourceId)).not.toBeNull();
			expect((await readRow(world.b.t1.id, copyId))!.content).toBe('channel file');
			await createBuilder(harness, world.a.owner.token).delete(`/channels/${world.a.ann.id}`).expect(204).execute();
			await expectCopyMarkedAndSourcePurged(sourceId, copyId, sourceKey);
		});
	});

	test('a delete that read the source before it was flagged still marks the copies', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'raced'});
		const copy = await onlyCopy(world.b.t1.id, source.id);
		const row = (await readRow(world.a.ann.id, source.id))!;
		await writeRow(row, {flags: row.flags & ~MessageFlags.CROSSPOSTED});
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, source.id);
		await world.worker.drain();
		await expectDeletedCopy(world.b.t1.id, copy.id);
		expect(await mappingRows(source.id)).toHaveLength(0);
	});

	test('deleting unpublished messages in a text channel enqueues no sync jobs', async () => {
		const message = await sendMessage(harness, world.b.owner.token, world.b.t1.id, {content: 'local only'});
		await deleteMessageRequest(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(world.worker.recorded.filter((job) => job.taskType === 'syncCrosspostedMessage')).toHaveLength(0);
	});

	test('removing a source attachment while publishing is disabled drops it from the copies', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'frozen'}, [
			'keep.png',
			'drop.gif',
		]);
		await publishAndDrain(harness, world, source.id);
		const copy = await onlyCopy(world.b.t1.id, source.id);
		const [kept, dropped] = (await readRow(world.a.ann.id, source.id))!.attachments;
		await addGuildFeature(world.a.guild.id, GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED);
		await createBuilder(harness, world.a.owner.token)
			.delete(`/channels/${world.a.ann.id}/messages/${source.id}/attachments/${dropped!.id}`)
			.expect(204)
			.execute();
		await edit(source.id, {content: 'frozen, edited'});
		await world.worker.drain();
		const copyRow = (await readRow(world.b.t1.id, copy.id))!;
		expect(copyRow.attachments.map((attachment) => attachment.id)).toEqual([kept!.id]);
		expect(copyRow.content).toBe('frozen');
		const droppedKey = `attachments/${world.a.ann.id}/${dropped!.id}/${dropped!.filename}`;
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, droppedKey)).toBe(false);
	});

	test('a copy owns no decay record and its expiry purges only the source file', async () => {
		const queued: Array<{s3Key: string}> = [];
		disableCrosspostWorker();
		world.worker = enableCrosspostWorker(
			{},
			{
				assetDeletionQueue: {
					async queueDeletion(item: {s3Key: string}) {
						queued.push(item);
					},
				} as unknown as IAssetDeletionQueue,
				instanceConfigRepository: {
					async getEffectiveAttachmentDecayConfig() {
						return {enabled: true};
					},
				} as unknown as InstanceConfigRepository,
			},
		);
		await followInto(harness, world, world.b.t1.id);
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'decaying'});
		await publishAndDrain(harness, world, source.id);
		const [attachment] = (await readRow(world.a.ann.id, source.id))!.attachments;
		const copy = await onlyCopy(world.b.t1.id, source.id);
		await createBuilder(harness, world.b.owner.token).get(`/channels/${world.b.t1.id}/messages/${copy.id}`).execute();
		const repository = new AttachmentDecayRepository();
		const record = await repository.fetchById(attachment!.id);
		expect(record?.channel_id.toString()).toBe(world.a.ann.id);
		expect(record?.message_id.toString()).toBe(source.id);
		const expiresAt = new Date(Date.now() - 60_000);
		await repository.upsert({...record!, expires_at: expiresAt, expiry_bucket: getExpiryBucket(expiresAt)});
		await processExpiredAttachments();
		expect(queued.map((item) => item.s3Key)).toEqual([
			`attachments/${world.a.ann.id}/${attachment!.id}/${attachment!.filename}`,
		]);
	});

	test('a copy deleted in the target is cleaned up lazily on the next edit', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'moderated'});
		const copy = await onlyCopy(world.b.t1.id, source.id);
		await deleteMessageRequest(harness, world.b.owner.token, world.b.t1.id, copy.id);
		expect(await mappingRow(source.id, webhookId)).not.toBeNull();
		await edit(source.id, {content: 'edited after removal'});
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		expect(await mappingRow(source.id, webhookId)).toBeNull();
	});

	test('syncs after the source is gone are no-ops', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'twice'});
		const copy = await onlyCopy(world.b.t1.id, source.id);
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, source.id);
		await world.worker.drain();
		const marked = await readRow(world.b.t1.id, copy.id);
		for (const mode of ['update', 'source_deleted'] as const) {
			await enqueueCrosspostSync(world.worker, {
				channelId: createChannelID(BigInt(world.a.ann.id)),
				messageId: createMessageID(BigInt(source.id)),
				mode,
			});
			await world.worker.drain();
		}
		expect(world.worker.deadLetters).toHaveLength(0);
		expect((await readRow(world.b.t1.id, copy.id))?.version).toBe(marked?.version);
	});

	test('a copy rewritten as source deleted never takes later updates', async () => {
		await followInto(harness, world, world.b.ageRestricted.id);
		const source = await postAndPublish(harness, world, {content: 'rule change'});
		const copy = await onlyCopy(world.b.ageRestricted.id, source.id);
		await patchChannel(harness, world.a.owner.token, world.a.ann.id, {nsfw_override: true}).expect(200).execute();
		await patchChannel(harness, world.b.owner.token, world.b.ageRestricted.id, {nsfw_override: false})
			.expect(200)
			.execute();
		await edit(source.id, {content: 'second version'});
		await world.worker.drain();
		await expectDeletedCopy(world.b.ageRestricted.id, copy.id);
		await patchChannel(harness, world.b.owner.token, world.b.ageRestricted.id, {nsfw_override: true})
			.expect(200)
			.execute();
		await edit(source.id, {content: 'third version'});
		await world.worker.drain();
		await expectDeletedCopy(world.b.ageRestricted.id, copy.id);
	});

	test('a delivered copy keeps updating after an unfollow', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'before unfollow'});
		await createBuilder(harness, world.b.owner.token).delete(`/webhooks/${webhookId}`).expect(204).execute();
		await edit(source.id, {content: 'after unfollow'});
		await world.worker.drain();
		expect((await onlyCopy(world.b.t1.id, source.id)).content).toBe('after unfollow');
	});

	test('old copies keep updating after the follower webhook moves', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'in t1'});
		await createBuilder(harness, world.b.owner.token)
			.patch(`/webhooks/${webhookId}`)
			.body({channel_id: world.b.t2.id})
			.expect(200)
			.execute();
		await edit(source.id, {content: 'still in t1'});
		await world.worker.drain();
		expect((await onlyCopy(world.b.t1.id, source.id)).content).toBe('still in t1');
		expect(await copiesOf(harness, world.b.owner.token, world.b.t2.id, source.id)).toHaveLength(0);
		const next = await postAndPublish(harness, world, {content: 'lands in t2'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t2.id, next.id)).toHaveLength(1);
	});

	test('unpublished messages enqueue no sync jobs on edit and a no-op one on delete', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'private note'});
		await edit(message.id, {content: 'edited'});
		expect(world.worker.byTask('syncCrosspostedMessage')).toHaveLength(0);
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, message.id);
		expect(world.worker.byTask('syncCrosspostedMessage')).toHaveLength(1);
		await world.worker.drain();
		expect(world.worker.byTask('syncCrosspostCopies')).toHaveLength(0);
		expect(world.worker.deadLetters).toHaveLength(0);
	});

	test('a target that loses its age restriction gets the copy rewritten on the next edit', async () => {
		await patchChannel(harness, world.a.owner.token, world.a.ann.id, {nsfw_override: true}).expect(200).execute();
		await followInto(harness, world, world.b.ageRestricted.id);
		const source = await postAndPublish(harness, world, {content: 'restricted news'});
		const copy = await onlyCopy(world.b.ageRestricted.id, source.id);
		await patchChannel(harness, world.b.owner.token, world.b.ageRestricted.id, {nsfw_override: false})
			.expect(200)
			.execute();
		await edit(source.id, {content: 'restricted news, edited'});
		await world.worker.drain();
		await expectDeletedCopy(world.b.ageRestricted.id, copy.id);
	});

	test('webhook token edits and attachment deletes propagate', async () => {
		await followInto(harness, world, world.b.t1.id);
		const incoming = await createWebhook(harness, world.a.ann.id, world.a.owner.token, 'Releases');
		const {json} = await executeWebhook(harness, incoming.id, incoming.token, {content: 'v1', wait: true}, 200);
		await publishAndDrain(harness, world, json!.id);
		await createBuilderWithoutAuth(harness)
			.patch(`/webhooks/${incoming.id}/${incoming.token}/messages/${json!.id}`)
			.body({content: 'v2'})
			.expect(200)
			.execute();
		await world.worker.drain();
		expect((await onlyCopy(world.b.t1.id, json!.id)).content).toBe('v2');
		const withFile = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'file'});
		await publishAndDrain(harness, world, withFile.id);
		const copy = await onlyCopy(world.b.t1.id, withFile.id);
		const sourceAttachment = (await readRow(world.a.ann.id, withFile.id))!.attachments[0]!;
		const attachmentId = sourceAttachment.id;
		expect((await readRow(world.b.t1.id, copy.id))!.attachments[0]!.id).toBe(attachmentId);
		await createBuilder(harness, world.a.owner.token)
			.delete(`/channels/${world.a.ann.id}/messages/${withFile.id}/attachments/${attachmentId}`)
			.expect(204)
			.execute();
		await world.worker.drain();
		expect((await readRow(world.b.t1.id, copy.id))!.attachments).toHaveLength(0);
		const deleted = harness.storageService.getDeletedObjects().map((entry) => entry.key);
		expect(
			deleted.filter((key) => key === `attachments/${world.a.ann.id}/${attachmentId}/${sourceAttachment.filename}`),
		).toHaveLength(1);
		expect(deleted.some((key) => key.startsWith(`attachments/${world.b.t1.id}/`))).toBe(false);
	});

	test('copies in several guilds follow edits of the source', async () => {
		const c = await createAnnouncementTargetGuild(harness, 'Third Community');
		await addGuildMember(harness, world.a.owner, world.a.guild, c.owner);
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, c.t1.id, c.owner.token);
		const source = await postAndPublish(harness, world, {content: 'shared'});
		await edit(source.id, {content: 'shared, edited'});
		await world.worker.drain();
		expect((await onlyCopy(world.b.t1.id, source.id)).content).toBe('shared, edited');
		expect((await onlyCopy(c.t1.id, source.id, c.owner.token)).content).toBe('shared, edited');
	});
});
