// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID} from '@app/api/BrandedTypes';
import {createApiContext} from '@app/api/CreateApiContext';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {publishedEditRateLimitIdentifier} from '@app/api/channel/services/message/CrosspostPropagation';
import {crosspostChannelRateLimitIdentifier} from '@app/api/channel/services/message/MessageCrosspostService';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {DisabledLiveKitService} from '@app/api/infrastructure/DisabledLiveKitService';
import {InMemoryVoiceRoomStore} from '@app/api/infrastructure/InMemoryVoiceRoomStore';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {createGuildStackServices} from '@app/api/middleware/GuildStackServiceFactory';
import {getVoiceAvailabilityService} from '@app/api/middleware/ServiceRegistry';
import {
	getAssetDeletionQueue,
	getAttachmentUploadTraceRepository,
	getAvatarService,
	getChannelRepository,
	getEmbedService,
	getEntityAssetService,
	getFavoriteMemeRepository,
	getGuildAuditLogService,
	getGuildRepository,
	getInviteRepository,
	getLimitConfigService,
	getPurgeQueue,
	getRateLimitService,
	getReadStateService,
	getStorageService,
	getUserCacheService,
	getUserRepository,
	getVirusScanServiceInstance,
	getWebhookRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {Message} from '@app/api/models/Message';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	CROSSPOST_CHANNEL_RATE_LIMIT,
	PUBLISHED_MESSAGE_EDIT_RATE_LIMIT,
} from '@fluxer/constants/src/AnnouncementConstants';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

export interface RecordedJob {
	taskType: string;
	payload: WorkerJobPayload;
	options: WorkerJobOptions | undefined;
}

export class RecordingWorkerService implements IWorkerService {
	readonly jobs: Array<RecordedJob> = [];
	failure: ((taskType: string) => Error | null) | null = null;
	private nextJobId = 1n;

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		const error = this.failure?.(taskType) ?? null;
		if (error) {
			throw error;
		}
		this.jobs.push({taskType, payload, options});
		return this.nextJobId++;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	byTask(taskType: string): Array<RecordedJob> {
		return this.jobs.filter((job) => job.taskType === taskType);
	}

	syncJobsFor(messageId: string): Array<RecordedJob> {
		return this.byTask('syncCrosspostedMessage').filter((job) => job.payload.messageId === messageId);
	}
}

export interface AnnouncementWorld {
	owner: TestAccount;
	author: TestAccount;
	mod: TestAccount;
	member: TestAccount;
	guild: GuildResponse;
	announcement: ChannelResponse;
	text: ChannelResponse;
	modRoleId: string;
}

export async function createAnnouncementWorld(harness: ApiTestHarness): Promise<AnnouncementWorld> {
	const owner = await createTestAccount(harness);
	const author = await createTestAccount(harness);
	const mod = await createTestAccount(harness);
	const member = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, 'Announcements Source');
	const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
	const text = await createChannel(harness, owner.token, guild.id, 'chat', ChannelTypes.GUILD_TEXT);
	const invite = await createChannelInvite(harness, owner.token, announcement.id);
	for (const account of [author, mod, member]) {
		await acceptInvite(harness, account.token, invite.code);
		await ensureSessionStarted(harness, account.token);
	}
	await ensureSessionStarted(harness, owner.token);
	const modRole = await createRole(harness, owner.token, guild.id, {
		name: 'Moderator',
		permissions: Permissions.MANAGE_MESSAGES.toString(),
	});
	await addMemberRole(harness, owner.token, guild.id, mod.userId, modRole.id);
	return {owner, author, mod, member, guild, announcement, text, modRoleId: modRole.id};
}

export async function createAnnouncementChannel(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	name: string,
): Promise<ChannelResponse> {
	return createChannel(harness, token, guildId, name, ChannelTypes.GUILD_ANNOUNCEMENT);
}

export async function postMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	content: string,
): Promise<MessageResponse> {
	return sendChannelMessage(harness, token, channelId, content);
}

export function crosspostRequest(harness: ApiTestHarness, token: string, channelId: string, messageId: string) {
	return createBuilder<MessageResponse>(harness, token).post(`/channels/${channelId}/messages/${messageId}/crosspost`);
}

export async function publish(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<MessageResponse> {
	return crosspostRequest(harness, token, channelId, messageId).expect(200).execute();
}

export function editMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	body: Record<string, unknown>,
) {
	return createBuilder<MessageResponse>(harness, token)
		.patch(`/channels/${channelId}/messages/${messageId}`)
		.body(body);
}

export async function readMessageRow(channelId: string, messageId: string): Promise<Message | null> {
	return new ChannelRepository().messages.getMessage(
		createChannelID(BigInt(channelId)),
		createMessageID(BigInt(messageId)),
	);
}

export async function writeMessageRow(message: Message, patch: Partial<ReturnType<Message['toRow']>>): Promise<void> {
	await new ChannelRepository().messages.upsertMessage({...message.toRow(), ...patch}, message.toRow());
}

export async function listCrosspostSources(channelId: string): Promise<Array<string>> {
	const ids = await new ChannelRepository().crossposts.listSourcesByChannel(createChannelID(BigInt(channelId)), {
		limit: 100,
	});
	return ids.map((id) => id.toString());
}

export async function patchGuildRow(guildId: string, patch: Partial<GuildRow>): Promise<void> {
	const repository = new GuildRepository();
	const id = createGuildID(BigInt(guildId));
	const existing = await repository.findUnique(id);
	if (!existing) {
		throw new Error(`guild ${guildId} not found`);
	}
	await repository.upsertPartial(id, patch, existing.toRow());
}

export async function addGuildFeature(guildId: string, feature: string): Promise<void> {
	const existing = await new GuildRepository().findUnique(createGuildID(BigInt(guildId)));
	const features = new Set(existing?.features ?? []);
	features.add(feature);
	await patchGuildRow(guildId, {features});
}

export async function channelBudgetRemaining(channelId: string): Promise<number> {
	const result = await getRateLimitService().peekLimit({
		identifier: crosspostChannelRateLimitIdentifier(createChannelID(BigInt(channelId))),
		...CROSSPOST_CHANNEL_RATE_LIMIT,
	});
	return result.remaining;
}

export async function publishedEditBudgetRemaining(messageId: string): Promise<number> {
	const result = await getRateLimitService().peekLimit({
		identifier: publishedEditRateLimitIdentifier(createMessageID(BigInt(messageId))),
		...PUBLISHED_MESSAGE_EDIT_RATE_LIMIT,
	});
	return result.remaining;
}

export interface StallGate {
	reached: Promise<void>;
	release: () => void;
}

export function createStallGate(): StallGate & {hit: () => Promise<void>} {
	let markReached: () => void = () => {};
	let release: () => void = () => {};
	const reached = new Promise<void>((resolve) => {
		markReached = resolve;
	});
	const released = new Promise<void>((resolve) => {
		release = resolve;
	});
	return {
		reached,
		release,
		hit: async () => {
			markReached();
			await released;
		},
	};
}

export function createTestChannelService(): ChannelService {
	return createGuildStackServices({
		apiContext: createApiContext(),
		channelRepository: getChannelRepository(),
		userRepository: getUserRepository(),
		guildRepository: getGuildRepository(),
		inviteRepository: getInviteRepository(),
		webhookRepository: getWebhookRepository(),
		favoriteMemeRepository: getFavoriteMemeRepository(),
		avatarService: getAvatarService(),
		entityAssetService: getEntityAssetService(),
		assetDeletionQueue: getAssetDeletionQueue(),
		userCacheService: getUserCacheService(),
		limitConfigService: getLimitConfigService(),
		embedService: getEmbedService(),
		readStateService: getReadStateService(),
		storageService: getStorageService(),
		attachmentUploadTraceRepository: getAttachmentUploadTraceRepository(),
		virusScanService: getVirusScanServiceInstance(),
		purgeQueue: getPurgeQueue(),
		guildAuditLogService: getGuildAuditLogService(),
		voiceRoomStore: new InMemoryVoiceRoomStore(),
		liveKitService: new DisabledLiveKitService(),
		voiceAvailabilityService: getVoiceAvailabilityService(),
	}).channelService;
}
