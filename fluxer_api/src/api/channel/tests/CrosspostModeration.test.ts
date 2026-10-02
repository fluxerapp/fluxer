// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	type ChannelID,
	createAttachmentID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {CrosspostTaskNames} from '@app/api/channel/services/message/CrosspostPropagation';
import {
	type AnnouncementTargetGuild,
	type AnnouncementWorld,
	addGuildMember,
	announcementWorld,
	createAnnouncementTargetGuild,
	follow,
	publish,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {readMessageRow, writeMessageRow} from '@app/api/channel/tests/CrosspostTestUtils';
import {NcmecRepository} from '@app/api/csam/NcmecRepository';
import type {MessageEmbed} from '@app/api/database/types/MessageTypes';
import type {EmbedService} from '@app/api/infrastructure/EmbedService';
import {
	getGatewayService,
	getKVClient,
	getSnowflakeService,
	setInjectedWorkerService,
} from '@app/api/middleware/ServiceRegistry';
import {
	getCacheService,
	getChannelRepository,
	getNcmecSubmissionService,
	getPurgeQueue,
	getStorageService,
} from '@app/api/middleware/ServiceSingletons';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, executeWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import extractEmbeds from '@app/api/worker/tasks/ExtractEmbeds';
import messageShred from '@app/api/worker/tasks/MessageShred';
import {
	clearWorkerDependencies,
	setWorkerDependencies,
	setWorkerDependenciesForTest,
} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {workerTasks} from '@app/api/worker/WorkerTaskRegistry';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageFlags, MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const SERVER_BITS = MessageFlags.CROSSPOSTED | MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED;
const CROSSPOST_TASKS = new Set<string>(Object.values(CrosspostTaskNames));
const BLOCKED_DOMAIN = 'crosspost-blocked.example';

interface QueuedJob {
	taskType: string;
	payload: WorkerJobPayload;
	options: WorkerJobOptions | undefined;
}

class CrosspostQueueWorker implements IWorkerService {
	readonly recorded: Array<QueuedJob> = [];
	private queue: Array<QueuedJob> = [];
	private seenKeys = new Set<string>();
	private nextJobId = 1n;

	clear(): void {
		this.recorded.length = 0;
		this.queue = [];
		this.seenKeys.clear();
	}

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		const jobKey = options?.jobKey;
		if (jobKey && this.seenKeys.has(jobKey)) {
			return 0n;
		}
		if (jobKey) {
			this.seenKeys.add(jobKey);
		}
		const job = {taskType, payload, options};
		this.recorded.push(job);
		if (CROSSPOST_TASKS.has(taskType)) {
			this.queue.push(job);
		}
		return this.nextJobId++;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	syncJobs(): Array<QueuedJob> {
		return this.recorded.filter((job) => job.taskType === CrosspostTaskNames.SYNC_CROSSPOSTED_MESSAGE);
	}

	async drain(): Promise<void> {
		const handlers = workerTasks as Partial<Record<string, WorkerTaskHandler>>;
		const attempts = new Map<QueuedJob, number>();
		let processed = 0;
		while (this.queue.length > 0) {
			processed += 1;
			if (processed > 5_000) {
				throw new Error('crosspost queue did not settle');
			}
			const job = this.queue.shift()!;
			const handler = handlers[job.taskType];
			if (!handler) {
				throw new Error(`no worker handler registered for ${job.taskType}`);
			}
			const helpers: WorkerTaskHelpers = {
				logger: new NoopLogger(),
				jobId: 0n,
				addJob: (taskType, payload, options) => this.addJob(taskType, payload, options),
				reportProgress: async () => {},
				shouldCancel: async () => false,
				setContextLink: async () => {},
			};
			try {
				await handler(job.payload, helpers);
			} catch (error) {
				const count = (attempts.get(job) ?? 0) + 1;
				attempts.set(job, count);
				if (count >= 3) {
					throw error;
				}
				this.queue.push(job);
			}
		}
	}
}

interface FabricatedCopy {
	copyId: string;
	attachment: {id: string; filename: string} | null;
}

async function fabricateCopy(params: {
	harness: ApiTestHarness;
	owner: TestAccount;
	target: string;
	world: AnnouncementWorld;
	sourceId: string;
	extraFlags?: number;
	withImage?: boolean;
}): Promise<FabricatedCopy> {
	const webhook = await createWebhook(params.harness, params.target, params.owner.token, 'Follower');
	const {json: message} = await executeWebhook(
		params.harness,
		webhook.id,
		webhook.token,
		{content: 'copied update', wait: true},
		200,
	);
	const sourceAttachments = params.withImage
		? (await readMessageRow(params.world.a.ann.id, params.sourceId))!.attachments
		: [];
	const row = await readMessageRow(params.target, message!.id);
	await writeMessageRow(row!, {
		flags: MessageFlags.IS_CROSSPOST | (params.extraFlags ?? 0),
		attachments:
			sourceAttachments.length > 0 ? sourceAttachments.map((attachment) => attachment.toMessageAttachment()) : null,
		message_reference: {
			channel_id: createChannelID(BigInt(params.world.a.ann.id)),
			guild_id: createGuildID(BigInt(params.world.a.guild.id)),
			message_id: createMessageID(BigInt(params.sourceId)),
			type: MessageReferenceTypes.DEFAULT,
		},
	});
	const attachment = sourceAttachments[0];
	return {
		copyId: message!.id,
		attachment: attachment ? {id: attachment.id.toString(), filename: attachment.filename} : null,
	};
}

async function createAdmin(harness: ApiTestHarness): Promise<TestAccount> {
	return setUserACLs(harness, await createTestAccount(harness), [
		AdminACLs.AUTHENTICATE,
		AdminACLs.MESSAGE_DELETE,
		AdminACLs.MESSAGE_SHRED,
		AdminACLs.CSAM_SUBMIT_NCMEC,
		AdminACLs.USER_DELETE,
		AdminACLs.ARCHIVE_TRIGGER_USER,
	]);
}

async function adminDeleteMessage(harness: ApiTestHarness, admin: TestAccount, channelId: string, messageId: string) {
	await createBuilder(harness, admin.token)
		.delete(`/admin/channels/${channelId}/messages/${messageId}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function deleteMessageSilently(channelId: string, messageId: string, fallbackUserId: string): Promise<void> {
	const service = getNcmecSubmissionService() as unknown as {
		deleteMessageSilently(channelId: ChannelID, messageId: MessageID, fallbackUserId: UserID): Promise<void>;
	};
	await service.deleteMessageSilently(
		createChannelID(BigInt(channelId)),
		createMessageID(BigInt(messageId)),
		createUserID(BigInt(fallbackUserId)),
	);
}

function blockedEmbedService(): EmbedService {
	return {
		processUrlWithCachePolicy: async (url: string) => ({
			embeds: [{toMessageEmbed: (): MessageEmbed => ({type: 'link', url}) as MessageEmbed}],
			cacheTtlSeconds: 0,
		}),
		cacheEmbeds: async () => {},
	} as unknown as EmbedService;
}

function helpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

async function listCopies(channelId: string, sourceId: string): Promise<Array<string>> {
	const messages = await new ChannelRepository().listMessages(createChannelID(BigInt(channelId)), undefined, 100);
	return messages
		.filter(
			(message) =>
				(message.flags & MessageFlags.IS_CROSSPOST) !== 0 && message.reference?.messageId?.toString() === sourceId,
		)
		.map((message) => message.id.toString());
}

describe('Crosspost moderation', () => {
	let harness: ApiTestHarness;
	let worker: CrosspostQueueWorker;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		worker = new CrosspostQueueWorker();
		setInjectedWorkerService(worker);
	});

	beforeEach(async () => {
		await harness.reset();
		harness.storageService.reset();
		worker.clear();
		world = await announcementWorld(harness);
	});

	afterEach(() => {
		clearWorkerDependencies();
		urlBlocklistCache.removeDomain(BLOCKED_DOMAIN);
	});

	afterAll(async () => {
		setInjectedWorkerService(new NoopWorkerService());
		await harness?.shutdown();
	});

	function deletedKeys(): Array<string> {
		return harness.storageService.getDeletedObjects().map((entry) => entry.key);
	}

	describe('takedown hooks', () => {
		test('an admin delete of a copy enqueues a purge of the whole family', async () => {
			const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'update');
			const {copyId} = await fabricateCopy({
				harness,
				owner: world.b.owner,
				target: world.b.t1.id,
				world,
				sourceId: source.id,
			});
			const admin = await createAdmin(harness);
			await adminDeleteMessage(harness, admin, world.b.t1.id, copyId);
			expect(await readMessageRow(world.b.t1.id, copyId)).toBeNull();
			const jobs = worker.syncJobs();
			expect(jobs).toHaveLength(1);
			expect(jobs[0]!.payload).toEqual({
				channelId: world.a.ann.id,
				messageId: source.id,
				mode: 'purge',
				deleteSource: true,
			});
			expect(jobs[0]!.options?.jobKey).toBe(`crosspost-sync:${source.id}:purge`);
			expect(jobs[0]!.options?.skipLedger).toBe(true);
		});

		test('an admin delete of a published source purges its copies and leaves others alone', async () => {
			const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'update');
			await publish(harness, world.a.member.token, world.a.ann.id, source.id);
			const plain = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'not published');
			const admin = await createAdmin(harness);
			await adminDeleteMessage(harness, admin, world.a.ann.id, plain.id);
			expect(worker.syncJobs()).toHaveLength(0);
			await adminDeleteMessage(harness, admin, world.a.ann.id, source.id);
			const jobs = worker.syncJobs();
			expect(jobs).toHaveLength(1);
			expect(jobs[0]!.payload).toEqual({channelId: world.a.ann.id, messageId: source.id, mode: 'purge'});
		});

		test('an NCMEC report on a copy records the source author and its takedown purges the family', async () => {
			const {json: source} = await sendMessageWithAttachments(
				harness,
				world.a.member.token,
				world.a.ann.id,
				{content: 'update', attachments: [{id: 0, filename: 'yeah.png'}]},
				[{index: 0, filename: 'yeah.png', data: loadFixture('yeah.png')}],
			);
			const {copyId, attachment} = await fabricateCopy({
				harness,
				owner: world.b.owner,
				target: world.b.t1.id,
				world,
				sourceId: source.id,
				withImage: true,
			});
			expect(attachment).not.toBeNull();
			const admin = await createAdmin(harness);
			await createBuilder(harness, admin.token)
				.post('/admin/messages/ncmec-reports')
				.body({
					channel_id: world.b.t1.id,
					message_id: copyId,
					attachment_id: attachment!.id,
					filename: attachment!.filename,
					reporter_full_name: 'Crosspost Reporter',
					confirmed_viewed: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const submission = await new NcmecRepository().getAttachmentSubmission(
				createAttachmentID(BigInt(attachment!.id)),
			);
			expect(submission?.user_id?.toString()).toBe(world.a.member.userId);
			expect(submission?.message_id.toString()).toBe(copyId);
			expect(submission?.channel_id.toString()).toBe(world.b.t1.id);
			worker.clear();
			const deletedBefore = deletedKeys().length;
			await deleteMessageSilently(world.b.t1.id, copyId, world.a.member.userId);
			expect(await readMessageRow(world.b.t1.id, copyId)).toBeNull();
			expect(deletedKeys().slice(deletedBefore)).toEqual([]);
			expect(
				harness.storageService.hasObject(
					Config.s3.buckets.cdn,
					`attachments/${world.a.ann.id}/${attachment!.id}/${attachment!.filename}`,
				),
			).toBe(true);
			const jobs = worker.syncJobs();
			expect(jobs).toHaveLength(1);
			expect(jobs[0]!.payload).toEqual({
				channelId: world.a.ann.id,
				messageId: source.id,
				mode: 'purge',
				deleteSource: true,
			});
		});

		test('an NCMEC takedown of a published source purges its copies', async () => {
			const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'update');
			await publish(harness, world.a.member.token, world.a.ann.id, source.id);
			worker.clear();
			await deleteMessageSilently(world.a.ann.id, source.id, world.a.member.userId);
			const jobs = worker.syncJobs();
			expect(jobs).toHaveLength(1);
			expect(jobs[0]!.payload).toEqual({channelId: world.a.ann.id, messageId: source.id, mode: 'purge'});
		});

		test('a message shred of a published source purges its copies', async () => {
			const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'update');
			await publish(harness, world.a.member.token, world.a.ann.id, source.id);
			const plain = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'plain');
			worker.clear();
			setWorkerDependenciesForTest({
				kvClient: getKVClient(),
				channelRepository: getChannelRepository(),
				gatewayService: getGatewayService(),
				storageService: getStorageService(),
				purgeQueue: getPurgeQueue(),
				workerService: worker,
			});
			await messageShred(
				{
					job_id: 'crosspost-shred',
					admin_user_id: '1',
					target_user_id: world.a.member.userId,
					entries: [
						{channel_id: world.a.ann.id, message_id: source.id},
						{channel_id: world.a.ann.id, message_id: plain.id},
					],
				},
				helpers(),
			);
			expect(await readMessageRow(world.a.ann.id, source.id)).toBeNull();
			const jobs = worker.syncJobs();
			expect(jobs).toHaveLength(1);
			expect(jobs[0]!.payload).toEqual({channelId: world.a.ann.id, messageId: source.id, mode: 'purge'});
		});

		test('a blocked unfurl on a published source deletes it and purges its copies', async () => {
			const source = await sendChannelMessage(
				harness,
				world.a.member.token,
				world.a.ann.id,
				`see https://${BLOCKED_DOMAIN}/page`,
			);
			await publish(harness, world.a.member.token, world.a.ann.id, source.id);
			urlBlocklistCache.addDomain(BLOCKED_DOMAIN);
			worker.clear();
			setWorkerDependenciesForTest({
				channelRepository: getChannelRepository(),
				gatewayService: getGatewayService(),
				embedService: blockedEmbedService(),
				cacheService: getCacheService(),
				workerService: worker,
			});
			await extractEmbeds(
				{channelId: world.a.ann.id, messageId: source.id, guildId: world.a.guild.id, nsfwMode: 'allow'},
				helpers(),
			);
			expect(await readMessageRow(world.a.ann.id, source.id)).toBeNull();
			const jobs = worker.syncJobs();
			expect(jobs).toHaveLength(1);
			expect(jobs[0]!.payload).toEqual({channelId: world.a.ann.id, messageId: source.id, mode: 'purge'});
		});
	});

	describe('forwarding', () => {
		function forward(token: string, targetChannelId: string, channelId: string, guildId: string, messageId: string) {
			return createBuilder<MessageResponse>(harness, token)
				.post(`/channels/${targetChannelId}/messages`)
				.body({
					message_reference: {
						type: MessageReferenceTypes.FORWARD,
						message_id: messageId,
						channel_id: channelId,
						guild_id: guildId,
					},
				});
		}

		test('forwarding a published source carries none of the server bits', async () => {
			const source = await sendChannelMessage(harness, world.b.owner.token, world.a.ann.id, 'forward me');
			await publish(harness, world.b.owner.token, world.a.ann.id, source.id);
			const forwarded = await forward(world.b.owner.token, world.b.t2.id, world.a.ann.id, world.a.guild.id, source.id)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(forwarded.message_snapshots?.[0]?.flags ?? 0).toBe(0);
		});

		test('forwarding a copy carries none of the server bits', async () => {
			const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'update');
			const {copyId} = await fabricateCopy({
				harness,
				owner: world.b.owner,
				target: world.b.t1.id,
				world,
				sourceId: source.id,
				extraFlags: MessageFlags.SUPPRESS_NOTIFICATIONS,
			});
			const forwarded = await forward(world.b.owner.token, world.b.t2.id, world.b.t1.id, world.b.guild.id, copyId)
				.expect(HTTP_STATUS.OK)
				.execute();
			const flags = forwarded.message_snapshots?.[0]?.flags ?? 0;
			expect(flags & SERVER_BITS).toBe(0);
			expect(flags & MessageFlags.SUPPRESS_NOTIFICATIONS).toBe(MessageFlags.SUPPRESS_NOTIFICATIONS);
		});

		test('forwarding a source-deleted copy is refused', async () => {
			const source = await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, 'update');
			const {copyId} = await fabricateCopy({
				harness,
				owner: world.b.owner,
				target: world.b.t1.id,
				world,
				sourceId: source.id,
				extraFlags: MessageFlags.SOURCE_MESSAGE_DELETED,
			});
			await forward(world.b.owner.token, world.b.t2.id, world.b.t1.id, world.b.guild.id, copyId)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
		});
	});

	describe('takedown cascade after delivery', () => {
		let c: AnnouncementTargetGuild;

		beforeEach(async () => {
			const deps = await initializeWorkerDependencies(getSnowflakeService());
			deps.workerService = worker;
			setWorkerDependencies(deps);
			c = await createAnnouncementTargetGuild(harness, 'Announcement Target C');
			await addGuildMember(harness, world.a.owner, world.a.guild, c.owner);
			await follow(harness, world.b.owner.token, world.a.ann.id, world.b.t1.id);
			await follow(harness, world.b.owner.token, world.a.ann.id, world.b.t2.id);
			await follow(harness, c.owner.token, world.a.ann.id, c.t1.id);
		});

		async function publishAndDeliver(content: string, withImage = false): Promise<string> {
			let sourceId: string;
			if (withImage) {
				const filename = 'yeah.png';
				const {json} = await sendMessageWithAttachments(
					harness,
					world.a.member.token,
					world.a.ann.id,
					{content, attachments: [{id: 0, filename}]},
					[{index: 0, filename, data: loadFixture(filename)}],
				);
				sourceId = json.id;
			} else {
				sourceId = (await sendChannelMessage(harness, world.a.member.token, world.a.ann.id, content)).id;
			}
			await publish(harness, world.a.member.token, world.a.ann.id, sourceId);
			await worker.drain();
			return sourceId;
		}

		async function sourceAttachmentKey(sourceId: string): Promise<string> {
			const [attachment] = (await readMessageRow(world.a.ann.id, sourceId))!.attachments;
			const key = `attachments/${world.a.ann.id}/${attachment!.id}/${attachment!.filename}`;
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, key)).toBe(true);
			return key;
		}

		function expectNoTargetObjectsDeleted(): void {
			for (const channelId of [world.b.t1.id, world.b.t2.id, c.t1.id]) {
				expect(deletedKeys().some((key) => key.startsWith(`attachments/${channelId}/`))).toBe(false);
			}
		}

		async function expectFamilyGone(sourceId: string): Promise<void> {
			expect(await readMessageRow(world.a.ann.id, sourceId)).toBeNull();
			expect(await listCopies(world.b.t1.id, sourceId)).toEqual([]);
			expect(await listCopies(world.b.t2.id, sourceId)).toEqual([]);
			expect(await listCopies(c.t1.id, sourceId)).toEqual([]);
		}

		test('an admin delete of one copy removes the source and every sibling copy', async () => {
			const sourceId = await publishAndDeliver('family update', true);
			const sourceKey = await sourceAttachmentKey(sourceId);
			const [copyId] = await listCopies(world.b.t1.id, sourceId);
			expect(copyId).toBeDefined();
			expect(await listCopies(world.b.t2.id, sourceId)).toHaveLength(1);
			expect(await listCopies(c.t1.id, sourceId)).toHaveLength(1);
			const admin = await createAdmin(harness);
			await adminDeleteMessage(harness, admin, world.b.t1.id, copyId!);
			expect(deletedKeys()).not.toContain(sourceKey);
			await worker.drain();
			await expectFamilyGone(sourceId);
			expect(deletedKeys().filter((key) => key === sourceKey)).toHaveLength(1);
			expect(harness.storageService.hasObject(Config.s3.buckets.cdn, sourceKey)).toBe(false);
			expectNoTargetObjectsDeleted();
		});

		test('an NCMEC takedown of a delivered copy records the source author and removes the family', async () => {
			const sourceId = await publishAndDeliver('image update', true);
			const sourceKey = await sourceAttachmentKey(sourceId);
			const [copyId] = await listCopies(c.t1.id, sourceId);
			const copy = await readMessageRow(c.t1.id, copyId!);
			const attachment = copy!.attachments[0]!;
			expect(`attachments/${world.a.ann.id}/${attachment.id}/${attachment.filename}`).toBe(sourceKey);
			const admin = await createAdmin(harness);
			await createBuilder(harness, admin.token)
				.post('/admin/messages/ncmec-reports')
				.body({
					channel_id: c.t1.id,
					message_id: copyId,
					attachment_id: attachment.id.toString(),
					filename: attachment.filename,
					reporter_full_name: 'Crosspost Reporter',
					confirmed_viewed: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const submission = await new NcmecRepository().getAttachmentSubmission(attachment.id);
			expect(submission?.user_id?.toString()).toBe(world.a.member.userId);
			await deleteMessageSilently(c.t1.id, copyId!, world.a.member.userId);
			await worker.drain();
			await expectFamilyGone(sourceId);
			expect(deletedKeys().filter((key) => key === sourceKey)).toHaveLength(1);
			expectNoTargetObjectsDeleted();
		});

		test('a blocked unfurl of a published source deletes every copy', async () => {
			const sourceId = await publishAndDeliver(`see https://${BLOCKED_DOMAIN}/page`);
			expect(await listCopies(c.t1.id, sourceId)).toHaveLength(1);
			urlBlocklistCache.addDomain(BLOCKED_DOMAIN);
			const deps = await initializeWorkerDependencies(getSnowflakeService());
			deps.workerService = worker;
			deps.embedService = blockedEmbedService();
			setWorkerDependencies(deps);
			await extractEmbeds(
				{channelId: world.a.ann.id, messageId: sourceId, guildId: world.a.guild.id, nsfwMode: 'allow'},
				helpers(),
			);
			await worker.drain();
			await expectFamilyGone(sourceId);
		});
	});
});
