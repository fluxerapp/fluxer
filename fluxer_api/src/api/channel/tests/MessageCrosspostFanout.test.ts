// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAttachmentID,
	createChannelID,
	createGuildID,
	createMessageID,
	createStickerID,
	createWebhookID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {enqueueCrosspostSync} from '@app/api/channel/services/message/CrosspostPropagation';
import {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import {
	addGuildMember,
	createAnnouncementTargetGuild,
	createGuildChannel,
	disableCrosspostWorker,
	findWebhookRow,
	follow,
	publish,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {loadFixture} from '@app/api/channel/tests/AttachmentTestUtils';
import {createPermissionOverwrite, updateGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {addGuildFeature, createStallGate, patchGuildRow} from '@app/api/channel/tests/CrosspostTestUtils';
import {
	copiesOf,
	deleteMessageRequest,
	editRequest,
	type FanoutWorld,
	followInto,
	listCopies,
	mappingRow,
	patchChannel,
	patchChannelRow,
	postAndPublish,
	publishAndDrain,
	readRow,
	sendMessage,
	sendWithImage,
	setupFanoutWorld,
	workerHelpers,
	writeRow,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {Logger} from '@app/api/Logger';
import {getMessages} from '@app/api/message/tests/MessageTestUtils';
import {startContentBlocklistCaches, stopContentBlocklistCaches} from '@app/api/middleware/ContentBlocklistCaches';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import crosspostMessageChunk from '@app/api/worker/tasks/CrosspostMessageChunk';
import syncCrosspostCopies from '@app/api/worker/tasks/SyncCrosspostCopies';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {CROSSPOST_SOURCE_DELETED_CONTENT} from '@fluxer/constants/src/AnnouncementConstants';
import {
	ChannelTypes,
	MessageAttachmentFlags,
	MessageFlags,
	MessageTypes,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {
	ContentWarningLevel,
	GuildExplicitContentFilterTypes,
	GuildFeatures,
	GuildOperations,
} from '@fluxer/constants/src/GuildConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import sharp from 'sharp';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

function createBarrier(parties: number): () => Promise<void> {
	let arrived = 0;
	let open: () => void = () => {};
	const opened = new Promise<void>((resolve) => {
		open = resolve;
	});
	return async () => {
		arrived++;
		if (arrived >= parties) open();
		await opened;
	};
}

const fanoutSizes = vi.hoisted(() => ({chunk: null as number | null, page: null as number | null}));

vi.mock('@fluxer/constants/src/AnnouncementConstants', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@fluxer/constants/src/AnnouncementConstants')>();
	return {
		...actual,
		get CROSSPOST_FANOUT_CHUNK_SIZE() {
			return fanoutSizes.chunk ?? actual.CROSSPOST_FANOUT_CHUNK_SIZE;
		},
		get CROSSPOST_FANOUT_PAGE_SIZE() {
			return fanoutSizes.page ?? actual.CROSSPOST_FANOUT_PAGE_SIZE;
		},
	};
});

describe('Crosspost fan-out', () => {
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
		fanoutSizes.chunk = null;
		fanoutSizes.page = null;
		disableCrosspostWorker();
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createTextChannel(name: string): Promise<ChannelResponse> {
		return createGuildChannel(harness, world.b.owner.token, world.b.guild.id, {name, type: ChannelTypes.GUILD_TEXT});
	}

	async function setSourceIcon(icon: string | null): Promise<string | null> {
		const updated = await updateGuild(harness, world.a.owner.token, world.a.guild.id, {icon});
		return updated.icon ?? null;
	}

	async function solidPngDataUrl(background: string): Promise<string> {
		const png = await sharp({create: {width: 32, height: 32, channels: 3, background}})
			.png()
			.toBuffer();
		return `data:image/png;base64,${png.toString('base64')}`;
	}

	function webhookAvatarCopies(webhookId: string): Array<{sourceKey: string; destinationKey: string}> {
		return harness.storageService
			.getCopiedObjects()
			.filter((copy) => copy.destinationKey.startsWith(`avatars/${webhookId}/`));
	}

	function attachmentObjectCopies(): Array<string> {
		return harness.storageService
			.getCopiedObjects()
			.filter((entry) => entry.sourceBucket === Config.s3.buckets.cdn && entry.sourceKey.startsWith('attachments/'))
			.map((entry) => entry.destinationKey);
	}

	function deletedKeys(): Array<string> {
		return harness.storageService.getDeletedObjects().map((entry) => entry.key);
	}

	function failNextCopyCreate(webhookId?: string): void {
		const original = MessagePersistenceService.prototype.createMessage;
		let failed = false;
		vi.spyOn(MessagePersistenceService.prototype, 'createMessage').mockImplementation(async function (
			this: MessagePersistenceService,
			params,
		) {
			const matches = webhookId ? params.webhookId?.toString() === webhookId : params.webhookId != null;
			if (!failed && matches) {
				failed = true;
				throw new Error('database hiccup');
			}
			return original.call(this, params);
		});
	}

	function stallNextCopyCreate(): ReturnType<typeof createStallGate> {
		const gate = createStallGate();
		const original = MessagePersistenceService.prototype.createMessage;
		let stalled = false;
		vi.spyOn(MessagePersistenceService.prototype, 'createMessage').mockImplementation(async function (
			this: MessagePersistenceService,
			params,
		) {
			if (!stalled && params.webhookId != null) {
				stalled = true;
				await gate.hit();
			}
			return original.call(this, params);
		});
		return gate;
	}

	async function expectNoCopies(channelId: string): Promise<void> {
		expect(await listCopies(harness, world.b.owner.token, channelId)).toHaveLength(0);
		expect(world.worker.deadLetters).toHaveLength(0);
	}

	test('one follower receives a copy with the published wire shape', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const source = await postAndPublish(harness, world, {content: 'Version 2 is out'});
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy).toBeDefined();
		expect(copy!.type).toBe(MessageTypes.DEFAULT);
		expect(copy!.webhook_id).toBe(webhookId);
		expect(copy!.author.id).toBe(webhookId);
		expect(copy!.author.username).toBe(`${world.a.guild.name} #${world.a.ann.name}`);
		expect(copy!.author.bot).toBe(true);
		expect(copy!.flags).toBe(MessageFlags.IS_CROSSPOST);
		expect(copy!.content).toBe('Version 2 is out');
		expect(copy!.message_reference).toEqual({
			type: 0,
			guild_id: world.a.guild.id,
			channel_id: world.a.ann.id,
			message_id: source.id,
		});
		expect('referenced_message' in copy!).toBe(false);
		const row = await mappingRow(source.id, webhookId);
		expect(row?.state).toBe('delivered');
		expect(row?.target_message_id.toString()).toBe(copy!.id);
		expect(row?.target_guild_id.toString()).toBe(world.b.guild.id);
	});

	test('copies store no mentions and reading them does not repair any', async () => {
		await followInto(harness, world, world.b.t1.id);
		const content = `@everyone <@&${world.a.guild.id}> <@${world.a.member.userId}> hello`;
		const source = await postAndPublish(harness, world, {content});
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy!.content).toBe(content);
		expect(copy!.mention_everyone).toBe(false);
		expect(copy!.mentions).toEqual([]);
		expect(copy!.mention_roles).toEqual([]);
		const before = await readRow(world.b.t1.id, copy!.id);
		for (let index = 0; index < 3; index++) {
			await createBuilder(harness, world.b.owner.token)
				.get(`/channels/${world.b.t1.id}/messages/${copy!.id}`)
				.execute();
			await getMessages(harness, world.b.owner.token, world.b.t1.id);
		}
		const after = await readRow(world.b.t1.id, copy!.id);
		expect(after?.version).toBe(before?.version);
		expect(after?.mentionEveryone).toBe(false);
		expect(after?.mentionedUserIds.size).toBe(0);
		expect(after?.mentionedRoleIds.size).toBe(0);
	});

	test('sendable flags carry over to the copy', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'quiet'});
		const sendable = MessageFlags.SUPPRESS_EMBEDS | MessageFlags.SUPPRESS_NOTIFICATIONS | MessageFlags.VOICE_MESSAGE;
		await writeRow((await readRow(world.a.ann.id, message.id))!, {flags: sendable});
		await publishAndDrain(harness, world, message.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.flags).toBe(MessageFlags.IS_CROSSPOST | sendable);
	});

	test('copies carry the source attachments and resolve to the source channel', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'files'}, [
			'first.png',
			'second.gif',
		]);
		const row = (await readRow(world.a.ann.id, message.id))!;
		await writeRow(row, {
			attachments: row.attachments.map((attachment, index) => ({
				...attachment.toMessageAttachment(),
				flags: index === 0 ? MessageAttachmentFlags.IS_SPOILER : 0,
			})),
		});
		await publishAndDrain(harness, world, message.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		const sourceAttachments = (await readRow(world.a.ann.id, message.id))!.attachments;
		const copyRow = (await readRow(world.b.t1.id, copy!.id))!;
		expect(copyRow.attachments.map((attachment) => attachment.toMessageAttachment())).toEqual(
			sourceAttachments.map((attachment) => attachment.toMessageAttachment()),
		);
		expect(copy!.attachments).toHaveLength(2);
		copy!.attachments!.forEach((attachment, index) => {
			const original = sourceAttachments[index]!;
			expect(attachment.id).toBe(original.id.toString());
			expect(attachment.url).toContain(`/attachments/${world.a.ann.id}/${original.id}/${original.filename}`);
			expect(attachment.url).not.toContain(`/attachments/${world.b.t1.id}/`);
			expect(attachment.proxy_url).toBe(attachment.url);
		});
		expect(attachmentObjectCopies()).toEqual([]);
	});

	test('rich embeds are copied with their source attachment urls', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {
			content: 'card',
			embeds: [{title: 'Release card', description: 'All the details', image: {url: 'attachment://yeah.png'}}],
		});
		const sourceRow = (await readRow(world.a.ann.id, message.id))!;
		expect(sourceRow.attachments).toHaveLength(0);
		const sourceImageUrl = sourceRow.embeds[0]!.image!.url!;
		expect(sourceImageUrl.startsWith(`${Config.endpoints.media}/attachments/${world.a.ann.id}/`)).toBe(true);
		const sourceKey = sourceImageUrl.slice(`${Config.endpoints.media}/`.length);
		await publishAndDrain(harness, world, message.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		const copyRow = (await readRow(world.b.t1.id, copy!.id))!;
		expect(copyRow.embeds[0]!.title).toBe('Release card');
		expect(copyRow.embeds[0]!.description).toBe('All the details');
		expect(copyRow.embeds[0]!.image!.url).toBe(sourceImageUrl);
		expect(copy!.embeds?.[0]?.image?.url).toContain(`/${sourceKey}`);
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, sourceKey)).toBe(true);
		expect(attachmentObjectCopies()).toEqual([]);
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, message.id);
		await world.worker.drain();
		expect(deletedKeys().some((key) => key.startsWith(`attachments/${world.b.t1.id}/`))).toBe(false);
		const marked = (await readRow(world.b.t1.id, copy!.id))!;
		expect(marked.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED);
	});

	test('favoriting the embed image of a copy by embed index copies the source object', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {
			content: 'card',
			embeds: [{title: 'Release card', image: {url: 'attachment://yeah.png'}}],
		});
		const sourceRow = (await readRow(world.a.ann.id, message.id))!;
		const sourceKey = sourceRow.embeds[0]!.image!.url!.slice(`${Config.endpoints.media}/`.length);
		await publishAndDrain(harness, world, message.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		await createBuilder(harness, world.b.owner.token)
			.post(`/channels/${world.b.t1.id}/messages/${copy!.id}/memes`)
			.body({embed_index: 0, name: 'Release card'})
			.expect(201)
			.execute();
		const fromSource = harness.storageService
			.getCopiedObjects()
			.filter((entry) => entry.sourceBucket === Config.s3.buckets.cdn && entry.sourceKey === sourceKey);
		expect(fromSource).toHaveLength(1);
	});

	test('stickers are copied', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'sticker'});
		const stickerId = createStickerID(123456789012345678n);
		await writeRow((await readRow(world.a.ann.id, message.id))!, {
			sticker_items: [{sticker_id: stickerId, name: 'party', animated: false}],
		});
		await publishAndDrain(harness, world, message.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		const copyRow = (await readRow(world.b.t1.id, copy!.id))!;
		expect(copyRow.stickers.map((sticker) => [sticker.id, sticker.name])).toEqual([[stickerId, 'party']]);
	});

	test('several followers in several guilds each get one copy', async () => {
		const c = await createAnnouncementTargetGuild(harness, 'Third Community');
		await addGuildMember(harness, world.a.owner, world.a.guild, c.owner);
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		await followInto(harness, world, c.t1.id, c.owner.token);
		const source = await postAndPublish(harness, world, {content: 'everyone gets this'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(1);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t2.id, source.id)).toHaveLength(1);
		expect(await copiesOf(harness, c.owner.token, c.t1.id, source.id)).toHaveLength(1);
	});

	test('small chunks and pages still give every follower exactly one copy', async () => {
		const targets = [world.b.t1, world.b.t2];
		for (const name of ['t3', 't4', 't5']) {
			targets.push(await createTextChannel(name));
		}
		for (const target of targets) {
			await followInto(harness, world, target.id);
		}
		fanoutSizes.chunk = 1;
		fanoutSizes.page = 2;
		const source = await postAndPublish(harness, world, {content: 'paged'});
		for (const target of targets) {
			expect(await copiesOf(harness, world.b.owner.token, target.id, source.id)).toHaveLength(1);
		}
		expect(world.worker.byTask('crosspostMessage')).toHaveLength(3);
		expect(world.worker.byTask('crosspostMessageChunk')).toHaveLength(5);
	});

	test('running a chunk twice for one follower creates one copy', async () => {
		const source = await postAndPublish(harness, world, {content: 'once'});
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const payload = {channelId: world.a.ann.id, messageId: source.id, webhookIds: [webhookId], attempt: 0};
		await crosspostMessageChunk(payload, workerHelpers(world.worker));
		await crosspostMessageChunk(payload, workerHelpers(world.worker));
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(1);
	});

	test('a stale pending reservation is reclaimed with a newer id, a fresh one waits', async () => {
		const staleSource = await postAndPublish(harness, world, {content: 'stale'});
		const freshSource = await postAndPublish(harness, world, {content: 'fresh'});
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const repository = new ChannelRepository();
		const staleTargetId = createMessageID(BigInt(staleSource.id));
		const base = {
			webhook_id: createWebhookID(BigInt(webhookId)),
			source_channel_id: createChannelID(BigInt(world.a.ann.id)),
			target_guild_id: createGuildID(BigInt(world.b.guild.id)),
			target_channel_id: createChannelID(BigInt(world.b.t1.id)),
			state: 'pending' as const,
			source_fingerprint: null,
			created_at: new Date(),
		};
		await repository.crossposts.insertPending({
			...base,
			source_message_id: createMessageID(BigInt(staleSource.id)),
			target_message_id: staleTargetId,
			reserved_at: new Date(Date.now() - 120_000),
		});
		await repository.crossposts.insertPending({
			...base,
			source_message_id: createMessageID(BigInt(freshSource.id)),
			target_message_id: createMessageID(BigInt(freshSource.id)),
			reserved_at: new Date(),
		});
		await crosspostMessageChunk(
			{channelId: world.a.ann.id, messageId: staleSource.id, webhookIds: [webhookId], attempt: 0},
			workerHelpers(world.worker),
		);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, staleSource.id);
		expect(BigInt(copy!.id)).toBeGreaterThan(staleTargetId);
		expect((await mappingRow(staleSource.id, webhookId))?.state).toBe('delivered');
		const recordedBefore = world.worker.recorded.length;
		await crosspostMessageChunk(
			{channelId: world.a.ann.id, messageId: freshSource.id, webhookIds: [webhookId], attempt: 0},
			workerHelpers(world.worker),
		);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, freshSource.id)).toHaveLength(0);
		const retries = world.worker.recorded.slice(recordedBefore);
		expect(retries).toHaveLength(1);
		expect(retries[0]!.taskType).toBe('crosspostMessageChunk');
		expect(retries[0]!.payload).toMatchObject({attempt: 1, webhookIds: [webhookId]});
		expect(retries[0]!.runAt!.getTime()).toBeGreaterThan(Date.now());
		expect(retries[0]!.jobKey).toBe(`crosspost-chunk:${freshSource.id}:${webhookId}:1`);
	});

	test('a copy delivered on retry still lands after newer target messages', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'late'});
		failNextCopyCreate();
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		await world.worker.drain({maxJobs: 2});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id)).toHaveLength(0);
		const newer = await sendMessage(harness, world.b.owner.token, world.b.t1.id, {content: 'meanwhile'});
		await world.worker.drain();
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(BigInt(copy!.id)).toBeGreaterThan(BigInt(newer.id));
		const channel = await createBuilder<ChannelResponse>(harness, world.b.owner.token)
			.get(`/channels/${world.b.t1.id}`)
			.execute();
		expect(channel.last_message_id).toBe(copy!.id);
	});

	describe('skips without errors', () => {
		test('a deleted follower webhook', async () => {
			const webhookId = await followInto(harness, world, world.b.t1.id);
			await createBuilder(harness, world.b.owner.token).delete(`/webhooks/${webhookId}`).expect(204).execute();
			await postAndPublish(harness, world, {content: 'nobody'});
			await expectNoCopies(world.b.t1.id);
		});

		test('a follower webhook removed while the chunk is queued', async () => {
			const webhookId = await followInto(harness, world, world.b.t1.id);
			const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'raced'});
			await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
			await world.worker.drain({maxJobs: 1});
			await createBuilder(harness, world.b.owner.token).delete(`/webhooks/${webhookId}`).expect(204).execute();
			await world.worker.drain();
			await expectNoCopies(world.b.t1.id);
		});

		test('a deleted target channel', async () => {
			await followInto(harness, world, world.b.t1.id);
			const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'gone'});
			await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
			await world.worker.drain({maxJobs: 1});
			await createBuilder(harness, world.b.owner.token).delete(`/channels/${world.b.t1.id}`).expect(204).execute();
			await world.worker.drain();
			expect(world.worker.deadLetters).toHaveLength(0);
		});

		test('a target converted to a non-text type', async () => {
			await followInto(harness, world, world.b.t1.id);
			await patchChannelRow(world.b.t1.id, {type: ChannelTypes.GUILD_ANNOUNCEMENT});
			await postAndPublish(harness, world, {content: 'wrong type'});
			await expectNoCopies(world.b.t1.id);
		});

		test('an age-restricted source into a target that lost its restriction', async () => {
			await patchChannel(harness, world.a.owner.token, world.a.ann.id, {nsfw_override: true}).execute();
			await followInto(harness, world, world.b.ageRestricted.id);
			await patchChannel(harness, world.b.owner.token, world.b.ageRestricted.id, {nsfw_override: false}).execute();
			await postAndPublish(harness, world, {content: 'restricted'});
			await expectNoCopies(world.b.ageRestricted.id);
		});

		test('a content-warning source into a target that lost its warning', async () => {
			await patchChannel(harness, world.a.owner.token, world.a.ann.id, {
				content_warning_level: ContentWarningLevel.CONTENT_WARNING,
			}).execute();
			await followInto(harness, world, world.b.contentWarning.id);
			await patchChannel(harness, world.b.owner.token, world.b.contentWarning.id, {
				content_warning_level: ContentWarningLevel.INHERIT,
			}).execute();
			await postAndPublish(harness, world, {content: 'warned'});
			await expectNoCopies(world.b.contentWarning.id);
		});

		test('a target guild with sending disabled', async () => {
			await followInto(harness, world, world.b.t1.id);
			await patchGuildRow(world.b.guild.id, {disabled_operations: GuildOperations.SEND_MESSAGE});
			await postAndPublish(harness, world, {content: 'muted target'});
			await patchGuildRow(world.b.guild.id, {disabled_operations: 0});
			await expectNoCopies(world.b.t1.id);
		});

		test('a source guild that disables announcement channels after publishing', async () => {
			await followInto(harness, world, world.b.t1.id);
			const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'switched off'});
			await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
			await addGuildFeature(world.a.guild.id, GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED);
			await world.worker.drain();
			await expectNoCopies(world.b.t1.id);
		});

		test('a follower creator who can no longer view the source', async () => {
			await followInto(harness, world, world.b.t1.id);
			await createPermissionOverwrite(harness, world.a.owner.token, world.a.ann.id, world.b.owner.userId, {
				type: 1,
				allow: '0',
				deny: Permissions.VIEW_CHANNEL.toString(),
			});
			const infoSpy = vi.spyOn(Logger.child({}), 'info');
			await postAndPublish(harness, world, {content: 'private'});
			await expectNoCopies(world.b.t1.id);
			expect(infoSpy.mock.calls.some(([, message]) => String(message).includes('cannot view'))).toBe(true);
		});
	});

	test('flagged media is stripped for targets that do not allow it', async () => {
		const c = await createAnnouncementTargetGuild(harness, 'Filtered Community');
		await addGuildMember(harness, world.a.owner, world.a.guild, c.owner);
		await patchGuildRow(world.a.guild.id, {explicit_content_filter: GuildExplicitContentFilterTypes.DISABLED});
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.ageRestricted.id);
		await followInto(harness, world, c.t1.id, c.owner.token);
		await addGuildFeature(world.b.guild.id, GuildFeatures.DISCOVERABLE);
		await patchGuildRow(c.guild.id, {explicit_content_filter: GuildExplicitContentFilterTypes.ALL_MEMBERS});
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {
			content: 'mixed',
			embeds: [{title: 'Safe card'}, {title: 'Flagged card'}],
		});
		const row = (await readRow(world.a.ann.id, message.id))!;
		await writeRow(row, {
			attachments: row.attachments.map((attachment) => ({...attachment.toMessageAttachment(), nsfw: true})),
			embeds: row.embeds.map((embed, index) => ({...embed.toMessageEmbed(), nsfw: index === 1})),
		});
		await publishAndDrain(harness, world, message.id);
		for (const [token, channelId] of [
			[world.b.owner.token, world.b.t1.id],
			[c.owner.token, c.t1.id],
		] as const) {
			const [copy] = await copiesOf(harness, token, channelId, message.id);
			const copyRow = (await readRow(channelId, copy!.id))!;
			expect(copyRow.attachments).toHaveLength(0);
			expect(copyRow.embeds.map((embed) => embed.title)).toEqual(['Safe card']);
		}
		const [restricted] = await copiesOf(harness, world.b.owner.token, world.b.ageRestricted.id, message.id);
		const restrictedRow = (await readRow(world.b.ageRestricted.id, restricted!.id))!;
		expect(restrictedRow.attachments).toHaveLength(1);
		expect(restrictedRow.embeds.map((embed) => embed.title)).toEqual(['Safe card', 'Flagged card']);
	});

	test('a sha blocklisted after publishing stops the fan-out', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'image'});
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		const blockedSha = 'cd'.repeat(32);
		const row = (await readRow(world.a.ann.id, message.id))!;
		await writeRow(row, {
			attachments: row.attachments.map((attachment) => ({
				...attachment.toMessageAttachment(),
				content_hash: blockedSha,
			})),
		});
		fileShaCache.add(blockedSha);
		const warnSpy = vi.spyOn(Logger.child({}), 'warn');
		try {
			await world.worker.drain();
		} finally {
			fileShaCache.remove(blockedSha);
		}
		await expectNoCopies(world.b.t1.id);
		expect(warnSpy.mock.calls.some(([, text]) => String(text).includes('blocked content'))).toBe(true);
	});

	test('an edit that removes and adds attachments reaches the copies', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'v1'}, [
			'first.png',
			'second.png',
		]);
		await publishAndDrain(harness, world, message.id);
		const row = (await readRow(world.a.ann.id, message.id))!;
		const [first, second] = row.attachments;
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.attachments?.map((attachment) => attachment.id)).toEqual([
			first!.id.toString(),
			second!.id.toString(),
		]);
		await createBuilder(harness, world.a.owner.token)
			.delete(`/channels/${world.a.ann.id}/messages/${message.id}/attachments/${second!.id}`)
			.expect(204)
			.execute();
		await world.worker.drain();
		let copyRow = (await readRow(world.b.t1.id, copy!.id))!;
		expect(copyRow.attachments.map((attachment) => attachment.id)).toEqual([first!.id]);
		const secondKey = `attachments/${world.a.ann.id}/${second!.id}/${second!.filename}`;
		expect(deletedKeys().filter((key) => key === secondKey)).toHaveLength(1);
		const extraId = first!.id + 1000n;
		const edited = (await readRow(world.a.ann.id, message.id))!;
		await writeRow(edited, {
			content: 'v2',
			attachments: [
				first!.toMessageAttachment(),
				{...first!.toMessageAttachment(), attachment_id: createAttachmentID(extraId), filename: 'extra.png'},
			],
		});
		await enqueueCrosspostSync(world.worker, {
			channelId: createChannelID(BigInt(world.a.ann.id)),
			messageId: createMessageID(BigInt(message.id)),
			mode: 'update',
		});
		await world.worker.drain();
		copyRow = (await readRow(world.b.t1.id, copy!.id))!;
		expect(copyRow.content).toBe('v2');
		expect(copyRow.attachments.map((attachment) => [attachment.id, attachment.filename])).toEqual([
			[first!.id, first!.filename],
			[extraId, 'extra.png'],
		]);
		const [synced] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(synced!.attachments?.[1]?.url).toContain(`/attachments/${world.a.ann.id}/${extraId}/extra.png`);
		expect(attachmentObjectCopies()).toEqual([]);
		expect(deletedKeys().some((key) => key.startsWith(`attachments/${world.b.t1.id}/`))).toBe(false);
	});

	test('a published message with large attachments reaches every copy without copying bytes', async () => {
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'big'});
		const row = (await readRow(world.a.ann.id, message.id))!;
		const large = 500n * 1024n * 1024n;
		await writeRow(row, {
			attachments: row.attachments.map((attachment) => ({...attachment.toMessageAttachment(), size: large})),
		});
		await publishAndDrain(harness, world, message.id);
		for (const target of [world.b.t1, world.b.t2]) {
			const [copy] = await copiesOf(harness, world.b.owner.token, target.id, message.id);
			expect(copy!.attachments).toHaveLength(1);
			expect(copy!.attachments![0]!.size).toBe(Number(large));
			expect(copy!.attachments![0]!.id).toBe(row.attachments[0]!.id.toString());
		}
		expect(attachmentObjectCopies()).toEqual([]);
	});

	test('a failed target is retried with backoff and only the missing copy is created', async () => {
		const webhookT1 = await followInto(harness, world, world.b.t1.id);
		const webhookT2 = await followInto(harness, world, world.b.t2.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'retry'});
		failNextCopyCreate();
		await publishAndDrain(harness, world, message.id);
		const retries = world.worker
			.byTask('crosspostMessageChunk')
			.filter((job) => (job.payload as {attempt: number}).attempt === 1);
		expect(retries).toHaveLength(1);
		expect(retries[0]!.runAt).toBeInstanceOf(Date);
		expect((retries[0]!.payload as {webhookIds: Array<string>}).webhookIds).toHaveLength(1);
		expect([webhookT1, webhookT2]).toContain((retries[0]!.payload as {webhookIds: Array<string>}).webhookIds[0]);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id)).toHaveLength(1);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t2.id, message.id)).toHaveLength(1);
	});

	test('an edit racing the delivery ends in the copy', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'draft'});
		const gate = stallNextCopyCreate();
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		const draining = world.worker.drain();
		await gate.reached;
		await editRequest(harness, world.a.owner.token, world.a.ann.id, message.id, {content: 'final'})
			.expect(200)
			.execute();
		gate.release();
		await draining;
		await world.worker.drain();
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('final');
	});

	test('a delete racing the delivery leaves the copy marked as source deleted', async () => {
		await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'short lived'});
		const gate = stallNextCopyCreate();
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		const draining = world.worker.drain();
		await gate.reached;
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, message.id);
		gate.release();
		await draining;
		await world.worker.drain();
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.flags).toBe(MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED);
		expect(copy!.content).toBe(CROSSPOST_SOURCE_DELETED_CONTENT);
		expect(copy!.attachments ?? []).toHaveLength(0);
	});

	test('a sync between reserve and create keeps the pending row and the copy converges', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'one'});
		const gate = createStallGate();
		const original = MessagePersistenceService.prototype.createMessage;
		vi.spyOn(MessagePersistenceService.prototype, 'createMessage').mockImplementation(async function (
			this: MessagePersistenceService,
			params,
		) {
			if (params.webhookId?.toString() === webhookId) {
				await gate.hit();
			}
			return original.call(this, params);
		});
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		const draining = world.worker.drain();
		await gate.reached;
		expect((await mappingRow(message.id, webhookId))?.state).toBe('pending');
		await editRequest(harness, world.a.owner.token, world.a.ann.id, message.id, {content: 'two'}).expect(200).execute();
		const syncCopies = (await import('@app/api/worker/tasks/SyncCrosspostCopies')).default;
		await syncCopies(
			{channelId: world.a.ann.id, messageId: message.id, mode: 'update', webhookIds: [webhookId]},
			workerHelpers(world.worker),
		);
		expect((await mappingRow(message.id, webhookId))?.state).toBe('pending');
		gate.release();
		await draining;
		await world.worker.drain();
		let [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('two');
		await editRequest(harness, world.a.owner.token, world.a.ann.id, message.id, {content: 'three'})
			.expect(200)
			.execute();
		await world.worker.drain();
		[copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('three');
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, message.id);
		await world.worker.drain();
		[copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.flags & MessageFlags.SOURCE_MESSAGE_DELETED).toBe(MessageFlags.SOURCE_MESSAGE_DELETED);
	});

	test('a copy moves the target last message id, is indexed and is dispatched to the target guild', async () => {
		await followInto(harness, world, world.b.t1.id);
		await patchChannelRow(world.b.t1.id, {indexed_at: new Date()});
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const indexSpy = vi.spyOn(MessageSearchService.prototype, 'indexMessage');
		const source = await postAndPublish(harness, world, {content: 'indexed'});
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		const channel = await createBuilder<ChannelResponse>(harness, world.b.owner.token)
			.get(`/channels/${world.b.t1.id}`)
			.execute();
		expect(channel.last_message_id).toBe(copy!.id);
		expect(indexSpy.mock.calls.some(([message]) => message.id.toString() === copy!.id)).toBe(true);
		const created = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'MESSAGE_CREATE' && params.guildId.toString() === world.b.guild.id);
		expect(created.some((params) => (params.data as {id: string}).id === copy!.id)).toBe(true);
	});

	test('messages published before a follow are not backfilled', async () => {
		const source = await postAndPublish(harness, world, {content: 'before'});
		await followInto(harness, world, world.b.t1.id);
		await world.worker.drain();
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(0);
		await follow(harness, world.b.owner.token, world.a.ann.id, world.b.t2.id);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t2.id, source.id)).toHaveLength(0);
	});
	async function sendEmbeddedImage(content: string): Promise<{id: string; imageUrl: string}> {
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {
			content,
			embeds: [{title: 'Card', image: {url: 'attachment://yeah.png'}}],
		});
		const row = (await readRow(world.a.ann.id, message.id))!;
		expect(row.attachments).toHaveLength(0);
		return {id: message.id, imageUrl: row.embeds[0]!.toMessageEmbed().image!.url!};
	}

	test('a sha blocklisted on embed media after publishing stops the fan-out', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendEmbeddedImage('blocked card');
		await publish(harness, world.a.owner.token, world.a.ann.id, source.id);
		const blockedSha = 'ef'.repeat(32);
		const row = (await readRow(world.a.ann.id, source.id))!;
		await writeRow(row, {
			embeds: row.embeds.map((embed) => {
				const value = embed.toMessageEmbed();
				return {...value, image: value.image ? {...value.image, content_hash: blockedSha} : value.image};
			}),
		});
		fileShaCache.add(blockedSha);
		try {
			await world.worker.drain();
		} finally {
			fileShaCache.remove(blockedSha);
		}
		await expectNoCopies(world.b.t1.id);
		expect(attachmentObjectCopies()).toEqual([]);
	});

	test('the worker blocklist caches load entries added after publishing', async () => {
		await followInto(harness, world, world.b.t1.id);
		const source = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {
			content: 'news with lateblockedphrase inside',
		});
		await publish(harness, world.a.owner.token, world.a.ann.id, source.id);
		await new AdminRepository().banPhrase('lateblockedphrase');
		await stopContentBlocklistCaches();
		await startContentBlocklistCaches({kvClient: getKVClient(), storageService: harness.storageService});
		try {
			await world.worker.drain();
		} finally {
			await stopContentBlocklistCaches();
		}
		await expectNoCopies(world.b.t1.id);
	});

	test('removing a copy never deletes target objects that a source embed points at', async () => {
		await followInto(harness, world, world.b.t1.id);
		const victim = await sendWithImage(harness, world.b.owner.token, world.b.t1.id, {content: 'mine'});
		const victimAttachment = (await readRow(world.b.t1.id, victim.id))!.attachments[0]!;
		const victimKey = `attachments/${world.b.t1.id}/${victimAttachment.id}/${victimAttachment.filename}`;
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, victimKey)).toBe(true);
		const source = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {
			content: 'look',
			embeds: [{title: 'Pointer', image: {url: 'https://example.com/pointer.png'}}],
		});
		const sourceRow = (await readRow(world.a.ann.id, source.id))!;
		const pointerUrl = `${Config.endpoints.media}/attachments/${world.b.t1.id}/${victimAttachment.id}/x.png`;
		await writeRow(sourceRow, {
			embeds: sourceRow.embeds.map((embed) => {
				const value = embed.toMessageEmbed();
				return {...value, image: {...value.image!, url: pointerUrl}};
			}),
		});
		await publishAndDrain(harness, world, source.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect((await readRow(world.b.t1.id, copy!.id))!.embeds[0]!.toMessageEmbed().image?.url).toBe(pointerUrl);
		await deleteMessageRequest(harness, world.a.owner.token, world.a.ann.id, source.id);
		await world.worker.drain();
		expect((await readRow(world.b.t1.id, copy!.id))!.flags).toBe(
			MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED,
		);
		expect(deletedKeys().some((key) => key.startsWith(`attachments/${world.b.t1.id}/${victimAttachment.id}/`))).toBe(
			false,
		);
		expect(harness.storageService.hasObject(Config.s3.buckets.cdn, victimKey)).toBe(true);
	});

	test('a delivered copy whose race check failed catches up on the retry', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'one'});
		const original = MessagePersistenceService.prototype.createMessage;
		vi.spyOn(MessagePersistenceService.prototype, 'createMessage').mockImplementation(async function (
			this: MessagePersistenceService,
			params,
		) {
			if (params.webhookId?.toString() !== webhookId) {
				return original.call(this, params);
			}
			await writeRow((await readRow(world.a.ann.id, message.id))!, {content: 'two'});
			const created = await original.call(this, params);
			vi.spyOn(MessageWriteLock.prototype, 'withFreshMessage').mockRejectedValueOnce(new Error('lock hiccup'));
			return created;
		});
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		await world.worker.drain();
		expect(world.worker.byTask('crosspostMessageChunk').some((job) => job.payload.attempt === 1)).toBe(true);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('two');
	});

	test('an admin takedown racing the delivery deletes the copy instead of marking it', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'illegal'});
		const gate = createStallGate();
		const original = MessagePersistenceService.prototype.createMessage;
		vi.spyOn(MessagePersistenceService.prototype, 'createMessage').mockImplementation(async function (
			this: MessagePersistenceService,
			params,
		) {
			if (params.webhookId?.toString() === webhookId) {
				await gate.hit();
			}
			return original.call(this, params);
		});
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		const draining = world.worker.drain();
		await gate.reached;
		const admin = await setUserACLs(harness, world.a.member, [AdminACLs.AUTHENTICATE, AdminACLs.MESSAGE_DELETE]);
		await createBuilder(harness, admin.token)
			.delete(`/admin/channels/${world.a.ann.id}/messages/${message.id}`)
			.expect(200)
			.execute();
		gate.release();
		await draining;
		await world.worker.drain();
		await expectNoCopies(world.b.t1.id);
		expect(await mappingRow(message.id, webhookId)).toBeNull();
	});

	test('a failed copy create leaves no copy behind and the retry delivers once', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'files'});
		failNextCopyCreate(webhookId);
		await publish(harness, world.a.owner.token, world.a.ann.id, message.id);
		await crosspostMessageChunk(
			{channelId: world.a.ann.id, messageId: message.id, webhookIds: [webhookId], attempt: 0},
			workerHelpers(world.worker),
		);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id)).toHaveLength(0);
		await world.worker.drain();
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id)).toHaveLength(1);
		expect(attachmentObjectCopies()).toEqual([]);
		expect(deletedKeys().filter((key) => key.startsWith('attachments/'))).toEqual([]);
	});

	test('two syncs of one edit converge on the source attachments', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		const message = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'v1'});
		await publishAndDrain(harness, world, message.id);
		const row = (await readRow(world.a.ann.id, message.id))!;
		const first = row.attachments[0]!;
		const extraId = first.id + 1n;
		await harness.storageService.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: `attachments/${world.a.ann.id}/${extraId}/extra.png`,
			body: loadFixture('yeah.png'),
			contentType: 'image/png',
		});
		await writeRow(row, {
			content: 'v2',
			attachments: [
				first.toMessageAttachment(),
				{...first.toMessageAttachment(), attachment_id: createAttachmentID(extraId), filename: 'extra.png'},
			],
		});
		const arrive = createBarrier(2);
		const originalLock = MessageWriteLock.prototype.withFreshMessage;
		vi.spyOn(MessageWriteLock.prototype, 'withFreshMessage').mockImplementation(async function (
			this: MessageWriteLock,
			...args: Parameters<MessageWriteLock['withFreshMessage']>
		) {
			await arrive();
			return originalLock.apply(this, args);
		} as MessageWriteLock['withFreshMessage']);
		const payload = {
			channelId: world.a.ann.id,
			messageId: message.id,
			mode: 'update' as const,
			webhookIds: [webhookId],
		};
		const results = await Promise.allSettled([
			syncCrosspostCopies(payload, workerHelpers(world.worker)),
			syncCrosspostCopies(payload, workerHelpers(world.worker)),
		]);
		vi.restoreAllMocks();
		expect(results.some((result) => result.status === 'fulfilled')).toBe(true);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
		expect(copy!.content).toBe('v2');
		const copyRow = (await readRow(world.b.t1.id, copy!.id))!;
		expect(copyRow.attachments.map((attachment) => attachment.id)).toEqual([first.id, extraId]);
		expect(attachmentObjectCopies()).toEqual([]);
		expect(deletedKeys().filter((key) => key.startsWith('attachments/'))).toEqual([]);
	});

	describe('copy author avatar', () => {
		test('a follow captures the source icon and the copy uses it', async () => {
			const icon = await setSourceIcon(getPngDataUrl());
			expect(icon).toBeTruthy();
			const webhookId = await followInto(harness, world, world.b.t1.id);
			expect((await findWebhookRow(webhookId))?.avatarHash).toBe(icon);
			expect(webhookAvatarCopies(webhookId)).toEqual([
				{
					sourceBucket: Config.s3.buckets.cdn,
					sourceKey: `icons/${world.a.guild.id}/${icon!.replace(/^a_/, '')}`,
					destinationBucket: Config.s3.buckets.cdn,
					destinationKey: `avatars/${webhookId}/${icon!.replace(/^a_/, '')}`,
				},
			]);
			const source = await postAndPublish(harness, world, {content: 'with icon'});
			const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
			expect(copy!.author.avatar).toBe(icon);
			expect(webhookAvatarCopies(webhookId)).toHaveLength(1);
		});

		test('a changed source icon reaches new copies while the webhook keeps its snapshot', async () => {
			const oldIcon = await setSourceIcon(getPngDataUrl());
			const webhookId = await followInto(harness, world, world.b.t1.id);
			const before = await postAndPublish(harness, world, {content: 'before'});
			const newIcon = await setSourceIcon(await solidPngDataUrl('#ff0000'));
			expect(newIcon).toBeTruthy();
			expect(newIcon).not.toBe(oldIcon);
			const first = await postAndPublish(harness, world, {content: 'after one'});
			const second = await postAndPublish(harness, world, {content: 'after two'});
			expect((await findWebhookRow(webhookId))?.avatarHash).toBe(oldIcon);
			const newKey = newIcon!.replace(/^a_/, '');
			const [firstCopy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, first.id);
			const [secondCopy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, second.id);
			expect(firstCopy!.author.avatar).toBe(newIcon);
			expect(secondCopy!.author.avatar).toBe(newIcon);
			expect((await readRow(world.b.t1.id, firstCopy!.id))?.webhookAvatarHash).toBe(newIcon);
			const newIconCopies = webhookAvatarCopies(webhookId).filter(
				(copy) => copy.destinationKey === `avatars/${webhookId}/${newKey}`,
			);
			expect(newIconCopies).toEqual([expect.objectContaining({sourceKey: `icons/${world.a.guild.id}/${newKey}`})]);
			expect(
				await harness.storageService.getObjectMetadata(Config.s3.buckets.cdn, `avatars/${webhookId}/${newKey}`),
			).not.toBeNull();
			const [beforeCopy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, before.id);
			expect(beforeCopy!.author.avatar).toBe(oldIcon);
			await editRequest(harness, world.a.owner.token, world.a.ann.id, before.id, {content: 'before edited'})
				.expect(200)
				.execute();
			await world.worker.drain();
			const [editedCopy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, before.id);
			expect(editedCopy!.content).toBe('before edited');
			expect(editedCopy!.author.avatar).toBe(oldIcon);
		});

		test('a source guild without an icon gives a null avatar on the webhook and the copy', async () => {
			const webhookId = await followInto(harness, world, world.b.t1.id);
			expect((await findWebhookRow(webhookId))?.avatarHash ?? null).toBeNull();
			const source = await postAndPublish(harness, world, {content: 'no icon'});
			const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
			expect(copy!.author.avatar ?? null).toBeNull();
			expect(webhookAvatarCopies(webhookId)).toHaveLength(0);
		});

		test('removing the source icon after following gives new copies a null avatar', async () => {
			const icon = await setSourceIcon(getPngDataUrl());
			const webhookId = await followInto(harness, world, world.b.t1.id);
			expect(await setSourceIcon(null)).toBeNull();
			const source = await postAndPublish(harness, world, {content: 'icon removed'});
			const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
			expect(copy!.author.avatar ?? null).toBeNull();
			expect((await findWebhookRow(webhookId))?.avatarHash).toBe(icon);
		});
	});
});
