// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {isCrosspostCopy} from '@app/api/channel/services/message/MessageHelpers';
import {Logger} from '@app/api/Logger';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {
	CROSSPOST_SYNC_COALESCE_MS,
	PUBLISHED_MESSAGE_EDIT_RATE_LIMIT,
} from '@fluxer/constants/src/AnnouncementConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import type {IRateLimitService, RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {z} from 'zod';

export const CrosspostTaskNames = {
	CROSSPOST_MESSAGE: 'crosspostMessage',
	CROSSPOST_MESSAGE_CHUNK: 'crosspostMessageChunk',
	SYNC_CROSSPOSTED_MESSAGE: 'syncCrosspostedMessage',
	SYNC_CROSSPOST_COPIES: 'syncCrosspostCopies',
	REMOVE_CHANNEL_FOLLOWERS: 'removeChannelFollowers',
} as const;

type CrosspostTaskName = (typeof CrosspostTaskNames)[keyof typeof CrosspostTaskNames];

export type CrosspostWorkerService = IWorkerService<WorkerTaskName | CrosspostTaskName>;

const CrosspostSyncModeSchema = z.enum(['update', 'source_deleted', 'purge']);
export type CrosspostSyncMode = z.infer<typeof CrosspostSyncModeSchema>;
type CrosspostRemovalMode = Exclude<CrosspostSyncMode, 'update'>;

export const CrosspostMessagePayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	afterWebhookId: z.string().optional(),
});
type CrosspostMessagePayload = z.infer<typeof CrosspostMessagePayloadSchema>;

export const CrosspostMessageChunkPayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	webhookIds: z.array(z.string()).min(1),
	attempt: z.number().int().min(0),
});

export const SyncCrosspostedMessagePayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	mode: CrosspostSyncModeSchema,
	deleteSource: z.boolean().optional(),
});
type SyncCrosspostedMessagePayload = z.infer<typeof SyncCrosspostedMessagePayloadSchema>;

export const SyncCrosspostCopiesPayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	mode: CrosspostSyncModeSchema,
	webhookIds: z.array(z.string()).min(1),
});

export const RemoveChannelFollowersPayloadSchema = z.object({
	sourceChannelId: z.string(),
	reason: z.enum(['deleted', 'converted']),
	copyMode: z.enum(['source_deleted', 'purge']).optional(),
});

export type PublishedEditActor = 'author' | 'webhook' | 'moderator';

const CROSSPOST_PURGE_MARKER_TTL_SECONDS = 86_400;

type CrosspostMessageLike = Pick<Message, 'id' | 'channelId' | 'flags' | 'reference'>;

export function isCrosspostedMessage(message: Pick<Message, 'flags'>): boolean {
	return (message.flags & MessageFlags.CROSSPOSTED) !== 0;
}

export function crosspostFanoutJobKey(messageId: string, afterWebhookId?: string): string {
	return afterWebhookId ? `crosspost:${messageId}:${afterWebhookId}` : `crosspost:${messageId}`;
}

export function crosspostChunkJobKey(messageId: string, firstWebhookId: string, attempt: number): string {
	return `crosspost-chunk:${messageId}:${firstWebhookId}:${attempt}`;
}

export function crosspostSyncBucket(nowMs: number): number {
	return Math.floor(nowMs / CROSSPOST_SYNC_COALESCE_MS);
}

export function crosspostSyncJobKey(messageId: string, mode: CrosspostSyncMode, bucket?: number): string {
	return mode === 'update' ? `crosspost-sync:${messageId}:update:${bucket}` : `crosspost-sync:${messageId}:${mode}`;
}

export function crosspostSyncChunkJobKey(params: {
	messageId: string;
	mode: CrosspostSyncMode;
	bucketOrMode: string;
	firstWebhookId: string;
}): string {
	return `crosspost-sync-chunk:${params.messageId}:${params.mode}:${params.bucketOrMode}:${params.firstWebhookId}`;
}

export function publishedEditRateLimitIdentifier(messageId: MessageID): string {
	return `crosspost:edit:${messageId}`;
}

function crosspostPurgeMarkerKey(messageId: MessageID | string): string {
	return `crosspost:purged:${messageId}`;
}

export async function isCrosspostSourcePurged(messageId: MessageID): Promise<boolean> {
	return (await getKVClient().exists(crosspostPurgeMarkerKey(messageId))) > 0;
}

export function createCrosspostRateLimitError(code: string, result: RateLimitResult): RateLimitError {
	return new RateLimitError({
		code,
		scope: 'shared',
		retryAfter: result.retryAfter,
		retryAfterDecimal: result.retryAfterDecimal,
		limit: result.limit,
		resetTime: result.resetTime,
	});
}

export async function enqueueCrosspostSync(
	workerService: CrosspostWorkerService,
	{
		channelId,
		messageId,
		mode,
		deleteSource,
	}: {
		channelId: ChannelID;
		messageId: MessageID;
		mode: CrosspostSyncMode;
		deleteSource?: boolean;
	},
): Promise<void> {
	const payload: SyncCrosspostedMessagePayload = {
		channelId: channelId.toString(),
		messageId: messageId.toString(),
		mode,
		...(deleteSource ? {deleteSource: true} : {}),
	};
	if (mode === 'purge') {
		try {
			await getKVClient().setex(crosspostPurgeMarkerKey(messageId), CROSSPOST_PURGE_MARKER_TTL_SECONDS, '1');
		} catch (error) {
			Logger.error(
				{error, channelId: payload.channelId, messageId: payload.messageId},
				'Failed to mark crosspost purge',
			);
		}
	}
	let jobKey: string;
	let runAt: Date | undefined;
	if (mode === 'update') {
		const bucket = crosspostSyncBucket(Date.now());
		jobKey = crosspostSyncJobKey(payload.messageId, mode, bucket);
		runAt = new Date((bucket + 1) * CROSSPOST_SYNC_COALESCE_MS + 1000);
	} else {
		jobKey = crosspostSyncJobKey(payload.messageId, mode);
	}
	try {
		await workerService.addJob(CrosspostTaskNames.SYNC_CROSSPOSTED_MESSAGE, payload, {
			jobKey,
			runAt,
			skipLedger: true,
		});
	} catch (error) {
		Logger.error(
			{error, channelId: payload.channelId, messageId: payload.messageId, mode},
			'Failed to enqueue crosspost sync',
		);
	}
}

export interface CrosspostSourceRemovalParams {
	messages: ReadonlyArray<CrosspostMessageLike>;
	mode: CrosspostRemovalMode;
	channel?: Pick<Channel, 'type'> | null;
}

function mayHaveCrosspostCopies(message: CrosspostMessageLike, channel: Pick<Channel, 'type'> | null): boolean {
	if (isCrosspostedMessage(message)) return true;
	return channel?.type === ChannelTypes.GUILD_ANNOUNCEMENT && !isCrosspostCopy(message);
}

export async function enqueueCrosspostSourceRemoval(
	workerService: CrosspostWorkerService,
	{messages, mode, channel = null}: CrosspostSourceRemovalParams,
): Promise<void> {
	for (const message of messages) {
		if (!mayHaveCrosspostCopies(message, channel)) continue;
		await enqueueCrosspostSync(workerService, {channelId: message.channelId, messageId: message.id, mode});
	}
}

export async function enqueueCrosspostFamilyPurgeFromCopies(
	workerService: CrosspostWorkerService,
	{messages}: {messages: ReadonlyArray<CrosspostMessageLike>},
): Promise<void> {
	const seen = new Set<string>();
	for (const message of messages) {
		if (!isCrosspostCopy(message)) continue;
		const reference = message.reference;
		if (!reference?.messageId) continue;
		const key = `${reference.channelId}:${reference.messageId}`;
		if (seen.has(key)) continue;
		seen.add(key);
		await enqueueCrosspostSync(workerService, {
			channelId: reference.channelId,
			messageId: reference.messageId,
			mode: 'purge',
			deleteSource: true,
		});
	}
}

interface CrosspostPropagationDeps {
	rateLimitService: IRateLimitService;
	workerService: CrosspostWorkerService;
}

export class CrosspostPropagation {
	constructor(private readonly deps: CrosspostPropagationDeps) {}

	async withPublishedEditBudget<T>(
		{fresh, actor}: {fresh: Message; actor: PublishedEditActor},
		write: () => Promise<T>,
	): Promise<T> {
		if (!isCrosspostedMessage(fresh) || actor === 'moderator') {
			return write();
		}
		const config = {identifier: publishedEditRateLimitIdentifier(fresh.id), ...PUBLISHED_MESSAGE_EDIT_RATE_LIMIT};
		const peek = await this.deps.rateLimitService.peekLimit(config);
		if (!peek.allowed) {
			throw createCrosspostRateLimitError(APIErrorCodes.PUBLISHED_MESSAGE_EDIT_RATE_LIMITED, peek);
		}
		const result = await write();
		await this.deps.rateLimitService.checkLimit(config);
		return result;
	}

	async enqueueCrosspostFanout({channelId, messageId}: {channelId: ChannelID; messageId: MessageID}): Promise<void> {
		const payload: CrosspostMessagePayload = {channelId: channelId.toString(), messageId: messageId.toString()};
		await this.deps.workerService.addJob(CrosspostTaskNames.CROSSPOST_MESSAGE, payload, {
			jobKey: crosspostFanoutJobKey(payload.messageId),
			skipLedger: true,
		});
	}

	async propagateEdit(message: Message): Promise<void> {
		if (!isCrosspostedMessage(message)) {
			return;
		}
		await enqueueCrosspostSync(this.deps.workerService, {
			channelId: message.channelId,
			messageId: message.id,
			mode: 'update',
		});
	}

	async enqueueCrosspostSourceRemoval(params: CrosspostSourceRemovalParams): Promise<void> {
		await enqueueCrosspostSourceRemoval(this.deps.workerService, params);
	}
}
