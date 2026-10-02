// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAttachmentID, createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {enqueueCrosspostFamilyPurgeFromCopies} from '@app/api/channel/services/message/CrosspostPropagation';
import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	createChannel,
	createPermissionOverwrite as createChannelPermissionOverwrite,
	pinMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	type AnnouncementWorld,
	addGuildFeature,
	channelBudgetRemaining,
	createAnnouncementWorld,
	createTestChannelService,
	crosspostRequest,
	editMessage,
	listCrosspostSources,
	patchGuildRow,
	postMessage,
	publish,
	RecordingWorkerService,
	readMessageRow,
	writeMessageRow,
} from '@app/api/channel/tests/CrosspostTestUtils';
import {Logger} from '@app/api/Logger';
import {getMessages} from '@app/api/message/tests/MessageTestUtils';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {phraseBlocklistCache} from '@app/api/middleware/PhraseBlocklistCache';
import {getSnowflakeService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {MockSnowflakeService} from '@app/api/test/mocks/MockSnowflakeService';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import {CROSSPOST_CHANNEL_RATE_LIMIT} from '@fluxer/constants/src/AnnouncementConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageFlags, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const SERVER_BITS = MessageFlags.CROSSPOSTED | MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED;

describe('Publishing messages in announcement channels', () => {
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
		phraseBlocklistCache.remove('forbiddenword');
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('the author publishes their own message', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Release 1.0 is out');
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const infoSpy = vi.spyOn(Logger.child({}), 'info');
		const published = await publish(harness, world.author.token, world.announcement.id, message.id);
		expect(published.id).toBe(message.id);
		expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
		expect(published.edited_timestamp ?? null).toBeNull();
		const row = await readMessageRow(world.announcement.id, message.id);
		expect(row?.flags).toBe(MessageFlags.CROSSPOSTED);
		expect(row?.editedTimestamp).toBeNull();
		const updates = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'MESSAGE_UPDATE');
		expect(updates).toHaveLength(1);
		expect(updates[0]?.data).toMatchObject({id: message.id, flags: MessageFlags.CROSSPOSTED});
		expect(infoSpy).toHaveBeenCalledWith(
			{
				actorId: world.author.userId,
				guildId: world.guild.id,
				channelId: world.announcement.id,
				messageId: message.id,
			},
			'message published',
		);
	});

	test('publishing another member message needs Manage Messages and Send Messages', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Weekly update');
		await crosspostRequest(harness, world.member.token, world.announcement.id, message.id)
			.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await createChannelPermissionOverwrite(harness, world.owner.token, world.announcement.id, world.modRoleId, {
			type: 0,
			allow: '0',
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		await crosspostRequest(harness, world.mod.token, world.announcement.id, message.id)
			.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await createChannelPermissionOverwrite(harness, world.owner.token, world.announcement.id, world.modRoleId, {
			type: 0,
			allow: '0',
			deny: '0',
		});
		const published = await publish(harness, world.mod.token, world.announcement.id, message.id);
		expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
	});

	test('the author cannot publish without Send Messages', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Muted soon');
		await createChannelPermissionOverwrite(harness, world.owner.token, world.announcement.id, world.author.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		await crosspostRequest(harness, world.author.token, world.announcement.id, message.id)
			.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	test('publishing an incoming webhook message needs Manage Messages', async () => {
		const webhook = await createWebhook(harness, world.announcement.id, world.owner.token, 'Status Bot');
		const executed = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({content: 'Status: all systems normal'})
			.expect(200)
			.execute();
		await crosspostRequest(harness, world.member.token, world.announcement.id, executed.id)
			.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		const published = await publish(harness, world.mod.token, world.announcement.id, executed.id);
		expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
	});

	test('a message can only be published once', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Once only');
		await publish(harness, world.author.token, world.announcement.id, message.id);
		await crosspostRequest(harness, world.author.token, world.announcement.id, message.id)
			.expect(400, APIErrorCodes.MESSAGE_ALREADY_CROSSPOSTED)
			.execute();
		expect(worker.byTask('crosspostMessage')).toHaveLength(1);
	});

	test('publishing in a text channel is rejected', async () => {
		const message = await postMessage(harness, world.author.token, world.text.id, 'Not news');
		await crosspostRequest(harness, world.author.token, world.text.id, message.id)
			.expect(400, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
		const row = await readMessageRow(world.text.id, message.id);
		expect(row?.flags).toBe(0);
	});

	test('replies, forwards, follow notices and pin notices cannot be published', async () => {
		const original = await postMessage(harness, world.author.token, world.announcement.id, 'Original');
		const reply = await createBuilder<MessageResponse>(harness, world.author.token)
			.post(`/channels/${world.announcement.id}/messages`)
			.body({content: 'A reply', message_reference: {message_id: original.id}})
			.expect(200)
			.execute();
		expect(reply.type).toBe(MessageTypes.REPLY);
		const forward = await createBuilder<MessageResponse>(harness, world.author.token)
			.post(`/channels/${world.announcement.id}/messages`)
			.body({
				message_reference: {
					type: 1,
					message_id: original.id,
					channel_id: world.announcement.id,
					guild_id: world.guild.id,
				},
			})
			.expect(200)
			.execute();
		expect(forward.message_snapshots?.length ?? 0).toBeGreaterThan(0);
		const followNotice = await postMessage(harness, world.author.token, world.announcement.id, 'Follow notice');
		const followRow = await readMessageRow(world.announcement.id, followNotice.id);
		await writeMessageRow(followRow!, {type: MessageTypes.CHANNEL_FOLLOW_ADD});
		await pinMessage(harness, world.owner.token, world.announcement.id, original.id);
		const messages = await getMessages(harness, world.owner.token, world.announcement.id);
		const pinNotice = messages.find((candidate) => candidate.type === MessageTypes.CHANNEL_PINNED_MESSAGE);
		expect(pinNotice).toBeDefined();
		for (const id of [reply.id, forward.id, followNotice.id, pinNotice!.id]) {
			await crosspostRequest(harness, world.owner.token, world.announcement.id, id)
				.expect(400, APIErrorCodes.MESSAGE_NOT_CROSSPOSTABLE)
				.execute();
		}
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
	});

	test('a copy cannot be published again', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Copied text');
		const row = await readMessageRow(world.announcement.id, message.id);
		await writeMessageRow(row!, {flags: MessageFlags.IS_CROSSPOST});
		await crosspostRequest(harness, world.owner.token, world.announcement.id, message.id)
			.expect(400, APIErrorCodes.MESSAGE_NOT_CROSSPOSTABLE)
			.execute();
	});

	test('unknown messages and channels return 404', async () => {
		await crosspostRequest(harness, world.author.token, world.announcement.id, '123456789012345678')
			.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		await crosspostRequest(harness, world.author.token, '123456789012345678', '123456789012345679')
			.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
			.execute();
	});

	test('a community with sending disabled or the kill switch cannot publish', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Blocked update');
		await patchGuildRow(world.guild.id, {disabled_operations: GuildOperations.SEND_MESSAGE});
		await crosspostRequest(harness, world.author.token, world.announcement.id, message.id)
			.expect(403, APIErrorCodes.FEATURE_TEMPORARILY_DISABLED)
			.execute();
		await patchGuildRow(world.guild.id, {disabled_operations: 0});
		await addGuildFeature(world.guild.id, GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED);
		await crosspostRequest(harness, world.author.token, world.announcement.id, message.id)
			.expect(403, APIErrorCodes.FEATURE_TEMPORARILY_DISABLED)
			.execute();
		expect((await readMessageRow(world.announcement.id, message.id))?.flags).toBe(0);
	});

	test('clients cannot set server-owned flags, and a suppress-embeds toggle keeps the published flag', async () => {
		const sent = await createBuilder<MessageResponse>(harness, world.author.token)
			.post(`/channels/${world.announcement.id}/messages`)
			.body({content: 'Flag probe', flags: SERVER_BITS})
			.expect(200)
			.execute();
		expect(sent.flags & SERVER_BITS).toBe(0);
		expect((await readMessageRow(world.announcement.id, sent.id))?.flags).toBe(0);
		const edited = await editMessage(harness, world.author.token, world.announcement.id, sent.id, {
			flags: SERVER_BITS | MessageFlags.SUPPRESS_EMBEDS,
		})
			.expect(200)
			.execute();
		expect(edited.flags).toBe(MessageFlags.SUPPRESS_EMBEDS);
		await publish(harness, world.author.token, world.announcement.id, sent.id);
		const toggled = await editMessage(harness, world.author.token, world.announcement.id, sent.id, {flags: 0})
			.expect(200)
			.execute();
		expect(toggled.flags).toBe(MessageFlags.CROSSPOSTED);
		const moderated = await editMessage(harness, world.mod.token, world.announcement.id, sent.id, {
			flags: MessageFlags.SUPPRESS_EMBEDS | SERVER_BITS,
		})
			.expect(200)
			.execute();
		expect(moderated.flags).toBe(MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS);
	});

	test('publishing enqueues the fan-out job and indexes the source', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Fan out');
		await publish(harness, world.author.token, world.announcement.id, message.id);
		const jobs = worker.byTask('crosspostMessage');
		expect(jobs).toHaveLength(1);
		expect(jobs[0]?.payload).toEqual({channelId: world.announcement.id, messageId: message.id});
		expect(jobs[0]?.options?.jobKey).toBe(`crosspost:${message.id}`);
		expect(jobs[0]?.options?.skipLedger).toBe(true);
		expect(await listCrosspostSources(world.announcement.id)).toEqual([message.id]);
	});

	test('repeated queue failures never use up the channel allowance', async () => {
		worker.failure = (taskType) =>
			taskType === 'crosspostMessage' ? new WorkerQueueOverflowError(taskType, 'stream full') : null;
		const attempts = CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts + 2;
		for (let index = 0; index < attempts; index++) {
			const message = await postMessage(harness, world.author.token, world.announcement.id, `Overflow ${index}`);
			await crosspostRequest(harness, world.author.token, world.announcement.id, message.id)
				.expect(503, APIErrorCodes.SERVICE_UNAVAILABLE)
				.execute();
		}
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
		worker.failure = null;
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'After the overflow');
		await publish(harness, world.author.token, world.announcement.id, message.id);
	});

	test('a full queue returns 503 and leaves the message unpublished', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Overflow');
		worker.failure = (taskType) =>
			taskType === 'crosspostMessage' ? new WorkerQueueOverflowError(taskType, 'stream full') : null;
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await crosspostRequest(harness, world.author.token, world.announcement.id, message.id)
			.expect(503, APIErrorCodes.SERVICE_UNAVAILABLE)
			.execute();
		expect((await readMessageRow(world.announcement.id, message.id))?.flags).toBe(0);
		expect(await listCrosspostSources(world.announcement.id)).toEqual([]);
		const updates = dispatchSpy.mock.calls
			.map(([params]) => params)
			.filter((params) => params.event === 'MESSAGE_UPDATE');
		expect(updates.map((update) => (update.data as {flags: number}).flags)).toEqual([MessageFlags.CROSSPOSTED, 0]);
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
		worker.failure = null;
		const published = await publish(harness, world.author.token, world.announcement.id, message.id);
		expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
		expect(worker.byTask('crosspostMessage')).toHaveLength(1);
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts - 1);
	});

	test('a message with large attachments can be published', async () => {
		const {json} = await sendMessageWithAttachments(
			harness,
			world.author.token,
			world.announcement.id,
			{content: 'Big file', attachments: [{id: 0, filename: 'yeah.png'}]},
			[{index: 0, filename: 'yeah.png', data: loadFixture('yeah.png')}],
		);
		const row = (await readMessageRow(world.announcement.id, json.id))!;
		const large = 2n * 1024n * 1024n * 1024n;
		await writeMessageRow(row, {
			attachments: [
				{...row.attachments[0]!.toMessageAttachment(), size: large},
				{
					...row.attachments[0]!.toMessageAttachment(),
					attachment_id: createAttachmentID(row.attachments[0]!.id + 1n),
					filename: 'second.png',
					size: large,
				},
			],
		});
		const published = await publish(harness, world.author.token, world.announcement.id, json.id);
		expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
		expect(published.attachments).toHaveLength(2);
		expect(worker.byTask('crosspostMessage')).toHaveLength(1);
	});

	test('blocklisted content cannot be published and does not consume budget', async () => {
		const plain = await postMessage(harness, world.author.token, world.announcement.id, 'says forbiddenword here');
		const embedded = await createBuilder<MessageResponse>(harness, world.author.token)
			.post(`/channels/${world.announcement.id}/messages`)
			.body({embeds: [{title: 'Weekly', fields: [{name: 'Notes', value: 'contains forbiddenword'}]}]})
			.expect(200)
			.execute();
		const {json: withFile} = await sendMessageWithAttachments(
			harness,
			world.author.token,
			world.announcement.id,
			{content: 'Picture', attachments: [{id: 0, filename: 'yeah.png'}]},
			[{index: 0, filename: 'yeah.png', data: loadFixture('yeah.png')}],
		);
		const fileRow = await readMessageRow(world.announcement.id, withFile.id);
		const blockedSha = 'ab'.repeat(32);
		await writeMessageRow(fileRow!, {
			attachments: fileRow!.attachments.map((attachment) => ({
				...attachment.toMessageAttachment(),
				content_hash: blockedSha,
			})),
		});
		phraseBlocklistCache.add('forbiddenword');
		fileShaCache.add(blockedSha);
		try {
			for (const id of [plain.id, embedded.id, withFile.id]) {
				await crosspostRequest(harness, world.author.token, world.announcement.id, id)
					.expect(403, APIErrorCodes.CONTENT_BLOCKED)
					.execute();
			}
		} finally {
			fileShaCache.remove(blockedSha);
		}
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
	});

	test('files shown only through embeds count toward the hash blocklist', async () => {
		const {json} = await sendMessageWithAttachments(
			harness,
			world.owner.token,
			world.announcement.id,
			{
				content: 'Card',
				embeds: [{title: 'Card', image: {url: 'attachment://yeah.png'}}],
				attachments: [{id: 0, filename: 'yeah.png'}],
			},
			[{index: 0, filename: 'yeah.png', data: loadFixture('yeah.png')}],
		);
		const row = (await readMessageRow(world.announcement.id, json.id))!;
		expect(row.attachments).toHaveLength(0);
		const embedHash = row.embeds[0]!.toMessageEmbed().image?.content_hash;
		expect(embedHash).toBeTruthy();
		fileShaCache.add(embedHash!);
		try {
			await crosspostRequest(harness, world.owner.token, world.announcement.id, json.id)
				.expect(403, APIErrorCodes.CONTENT_BLOCKED)
				.execute();
		} finally {
			fileShaCache.remove(embedHash!);
		}
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
		await publish(harness, world.owner.token, world.announcement.id, json.id);
	});

	test('the history cutoff applies to moderators without Read Message History', async () => {
		const older = await postMessage(harness, world.author.token, world.announcement.id, 'Old news');
		const olderTimestamp = snowflakeToDate(BigInt(older.id)).getTime();
		const snowflakes = getSnowflakeService() as MockSnowflakeService;
		snowflakes.configure({startTimestampMs: olderTimestamp + 20_000});
		const newer = await postMessage(harness, world.author.token, world.announcement.id, 'Fresh news');
		await patchGuildRow(world.guild.id, {message_history_cutoff: new Date(olderTimestamp + 10_000)});
		await createChannelPermissionOverwrite(harness, world.owner.token, world.announcement.id, world.mod.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		await crosspostRequest(harness, world.mod.token, world.announcement.id, older.id)
			.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		await publish(harness, world.mod.token, world.announcement.id, newer.id);
	});

	test('two concurrent publishes of one message succeed once and consume one slot', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Race');
		const attempt = () => crosspostRequest(harness, world.author.token, world.announcement.id, message.id).executeRaw();
		const results = await Promise.all([attempt(), attempt()]);
		const statuses = results.map((result) => result.response.status).sort();
		expect(statuses).toEqual([200, 400]);
		const rejected = results.find((result) => result.response.status === 400);
		expect((rejected?.json as {code?: string} | undefined)?.code).toBe(APIErrorCodes.MESSAGE_ALREADY_CROSSPOSTED);
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts - 1);
		expect(worker.byTask('crosspostMessage')).toHaveLength(1);
	});

	test('a second announcement channel publishes independently', async () => {
		const second = await createChannel(
			harness,
			world.owner.token,
			world.guild.id,
			'news-two',
			ChannelTypes.GUILD_ANNOUNCEMENT,
		);
		const message = await postMessage(harness, world.author.token, second.id, 'Second channel');
		await publish(harness, world.author.token, second.id, message.id);
		expect(await listCrosspostSources(second.id)).toEqual([message.id]);
		expect(await listCrosspostSources(world.announcement.id)).toEqual([]);
	});

	test('every delete path of an announcement message enqueues a source-deleted sync', async () => {
		const sources = [];
		for (let index = 0; index < 5; index++) {
			const message = await postMessage(harness, world.author.token, world.announcement.id, `Source ${index}`);
			await publish(harness, world.author.token, world.announcement.id, message.id);
			sources.push(message.id);
		}
		const unpublished = await postMessage(harness, world.author.token, world.announcement.id, 'Never published');
		const textMessage = await postMessage(harness, world.author.token, world.text.id, 'Plain text channel');
		const webhook = await createWebhook(harness, world.announcement.id, world.owner.token, 'Digest');
		const webhookMessage = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({content: 'Digest'})
			.expect(200)
			.execute();
		await publish(harness, world.mod.token, world.announcement.id, webhookMessage.id);
		await createBuilder(harness, world.mod.token)
			.delete(`/channels/${world.announcement.id}/messages/${sources[0]}`)
			.expect(204)
			.execute();
		await createBuilder(harness, world.mod.token)
			.post(`/channels/${world.announcement.id}/messages/bulk-delete`)
			.body({message_ids: [sources[1], unpublished.id]})
			.expect(204)
			.execute();
		await createBuilder(harness, world.mod.token)
			.post(`/channels/${world.text.id}/messages/bulk-delete`)
			.body({message_ids: [textMessage.id]})
			.expect(204)
			.execute();
		await createBuilderWithoutAuth(harness)
			.delete(`/webhooks/${webhook.id}/${webhook.token}/messages/${webhookMessage.id}`)
			.expect(204)
			.execute();
		const channelService = createTestChannelService();
		await channelService.messages.deletion.deleteUserMessagesInGuild({
			userId: createUserID(BigInt(world.author.userId)),
			guildId: createGuildID(BigInt(world.guild.id)),
			seconds: 3600,
		});
		const removalsFor = (messageId: string) =>
			worker.syncJobsFor(messageId).filter((job) => job.payload.mode === 'source_deleted');
		for (const messageId of [...sources, webhookMessage.id, unpublished.id]) {
			const removals = removalsFor(messageId);
			expect(removals.length).toBeGreaterThanOrEqual(1);
			expect(removals[0]?.payload).toEqual({
				channelId: world.announcement.id,
				messageId,
				mode: 'source_deleted',
			});
			expect(removals[0]?.options?.jobKey).toBe(`crosspost-sync:${messageId}:source_deleted`);
			expect(removals[0]?.options?.skipLedger).toBe(true);
		}
		expect(worker.syncJobsFor(textMessage.id)).toHaveLength(0);
	});

	test('bulk deleting your own messages enqueues a source-deleted sync for announcement ones', async () => {
		const published = await postMessage(harness, world.author.token, world.announcement.id, 'Mine, published');
		await publish(harness, world.author.token, world.announcement.id, published.id);
		const unpublished = await postMessage(harness, world.author.token, world.announcement.id, 'Mine, unpublished');
		const plain = await postMessage(harness, world.author.token, world.text.id, 'Mine, plain');
		const deleted = await createTestChannelService().userMessageDeletion.deleteUserMessagesBulk(
			createUserID(BigInt(world.author.userId)),
		);
		expect(deleted).toBeGreaterThanOrEqual(3);
		expect(await readMessageRow(world.announcement.id, published.id)).toBeNull();
		expect(worker.syncJobsFor(published.id).map((job) => job.payload.mode)).toEqual(['source_deleted']);
		expect(worker.syncJobsFor(unpublished.id).map((job) => job.payload.mode)).toEqual(['source_deleted']);
		expect(worker.syncJobsFor(plain.id)).toHaveLength(0);
	});

	test('the reverse cascade helper purges each copied family once', async () => {
		const source = await postMessage(harness, world.author.token, world.announcement.id, 'Source');
		const copy = await postMessage(harness, world.author.token, world.text.id, 'Copy');
		const plain = await postMessage(harness, world.author.token, world.text.id, 'Plain');
		const copyRow = await readMessageRow(world.text.id, copy.id);
		await writeMessageRow(copyRow!, {
			flags: MessageFlags.IS_CROSSPOST,
			message_reference: {
				channel_id: createChannelID(BigInt(world.announcement.id)),
				message_id: createMessageID(BigInt(source.id)),
				guild_id: createGuildID(BigInt(world.guild.id)),
				type: 0,
			},
		});
		const rows = await Promise.all([
			readMessageRow(world.text.id, copy.id),
			readMessageRow(world.text.id, copy.id),
			readMessageRow(world.text.id, plain.id),
		]);
		await enqueueCrosspostFamilyPurgeFromCopies(worker, {messages: rows.filter((row) => row !== null)});
		const syncs = worker.byTask('syncCrosspostedMessage');
		expect(syncs).toHaveLength(1);
		expect(syncs[0]?.payload).toEqual({
			channelId: world.announcement.id,
			messageId: source.id,
			mode: 'purge',
			deleteSource: true,
		});
		expect(syncs[0]?.options?.jobKey).toBe(`crosspost-sync:${source.id}:purge`);
	});
});
