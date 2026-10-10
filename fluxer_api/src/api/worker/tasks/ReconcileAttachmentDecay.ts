// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	attachmentDecayReconcileJobKey,
	finishAttachmentDecayReconcileRun,
	holdAttachmentDecayReconcileRun,
} from '@app/api/attachment/AttachmentDecayReconcileRun';
import {AttachmentDecayRepository} from '@app/api/attachment/AttachmentDecayRepository';
import {type AttachmentDecayPayload, AttachmentDecayService} from '@app/api/attachment/AttachmentDecayService';
import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	attachmentDecayPayloadsForMessage,
	makeAttachmentCdnKey,
} from '@app/api/channel/services/message/MessageHelpers';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const CHANNEL_PAGE_SIZE = 1000;

const CursorSchema = z.object({
	pageState: z.string().nullable(),
	channelId: z.string().nullable(),
	beforeMessageId: z.string().nullable(),
});

const PayloadSchema = z.object({
	runId: z.string(),
	chunk: z.number().int().nonnegative(),
	cursor: CursorSchema.optional(),
});

type ReconcileCursor = z.infer<typeof CursorSchema>;
export type ReconcileAttachmentDecayPayload = z.infer<typeof PayloadSchema>;

interface ReconcileChunkLimits {
	messagePageSize: number;
	messagePages: number;
}

const DEFAULT_CHUNK_LIMITS: ReconcileChunkLimits = {messagePageSize: 500, messagePages: 20};

interface ChunkProgress {
	messagePageSize: number;
	messagePagesLeft: number;
	channels: number;
	changed: number;
}

async function attachmentFileExists(attachment: AttachmentDecayPayload): Promise<boolean> {
	const {storageService} = getWorkerDependencies();
	const key = makeAttachmentCdnKey(attachment.channelId, attachment.attachmentId, attachment.filename);
	return (await storageService.getObjectMetadata(Config.s3.buckets.cdn, key)) !== null;
}

async function reconcileChannel(
	service: AttachmentDecayService,
	channelId: ChannelID,
	beforeMessageId: MessageID | undefined,
	progress: ChunkProgress,
): Promise<MessageID | null> {
	const {channelRepository} = getWorkerDependencies();
	let cursor = beforeMessageId;
	while (true) {
		if (progress.messagePagesLeft <= 0 && cursor !== undefined) return cursor;
		const messages = await channelRepository.messages.listMessages(channelId, cursor, progress.messagePageSize);
		progress.messagePagesLeft--;
		if (messages.length === 0) break;
		progress.changed += await service.reconcile(
			messages.flatMap(attachmentDecayPayloadsForMessage),
			attachmentFileExists,
		);
		if (messages.length < progress.messagePageSize) break;
		cursor = messages[messages.length - 1]!.id;
	}
	progress.channels++;
	return null;
}

async function reconcileFromCursor(
	service: AttachmentDecayService,
	repo: AttachmentDecayRepository,
	cursor: ReconcileCursor,
	progress: ChunkProgress,
): Promise<ReconcileCursor | null> {
	let {pageState, channelId} = cursor;
	if (channelId !== null && cursor.beforeMessageId !== null) {
		const beforeMessageId = await reconcileChannel(
			service,
			createChannelID(BigInt(channelId)),
			createMessageID(BigInt(cursor.beforeMessageId)),
			progress,
		);
		if (beforeMessageId !== null) return {pageState, channelId, beforeMessageId: beforeMessageId.toString()};
	}
	while (true) {
		const page = await repo.scanChannelsWithMessagesPage(CHANNEL_PAGE_SIZE, pageState);
		const handled = channelId === null ? -1 : page.channelIds.findIndex((id) => id.toString() === channelId);
		for (const nextChannelId of page.channelIds.slice(handled + 1)) {
			if (progress.messagePagesLeft <= 0) return {pageState, channelId, beforeMessageId: null};
			channelId = nextChannelId.toString();
			const beforeMessageId = await reconcileChannel(service, nextChannelId, undefined, progress);
			if (beforeMessageId !== null) return {pageState, channelId, beforeMessageId: beforeMessageId.toString()};
		}
		if (page.pageState === null) return null;
		pageState = page.pageState;
	}
}

export async function reconcileAttachmentDecayChunk(
	payload: ReconcileAttachmentDecayPayload,
	limits = DEFAULT_CHUNK_LIMITS,
): Promise<ReconcileAttachmentDecayPayload | null> {
	const {runId, chunk} = payload;
	if (!Config.instance.selfHosted) {
		Logger.info({runId, chunk}, 'Attachment decay reconcile only runs on self-hosted instances; stopping');
		return null;
	}
	const {instanceConfigRepository, kvClient} = getWorkerDependencies();
	if (!(await holdAttachmentDecayReconcileRun(kvClient, runId))) {
		Logger.info({runId, chunk}, 'Attachment decay reconcile superseded by a newer run; stopping');
		return null;
	}
	const config = await instanceConfigRepository.fetchStoredEffectiveAttachmentDecayConfig();
	if (!config.enabled) {
		await finishAttachmentDecayReconcileRun(kvClient, runId);
		Logger.info({runId, chunk}, 'Attachment decay disabled; stopping reconcileAttachmentDecay task');
		return null;
	}
	const repo = new AttachmentDecayRepository();
	const service = new AttachmentDecayService(repo, async () => config);
	const progress: ChunkProgress = {
		messagePageSize: limits.messagePageSize,
		messagePagesLeft: limits.messagePages,
		channels: 0,
		changed: 0,
	};
	const cursor = await reconcileFromCursor(
		service,
		repo,
		payload.cursor ?? {pageState: null, channelId: null, beforeMessageId: null},
		progress,
	);
	const summary = {runId, chunk, channels: progress.channels, changed: progress.changed};
	if (cursor !== null) {
		Logger.info(summary, 'Reconciled a chunk of attachment decay records');
		return {runId, chunk: chunk + 1, cursor};
	}
	await finishAttachmentDecayReconcileRun(kvClient, runId);
	Logger.info(summary, 'Reconciled attachment decay records with the instance settings');
	return null;
}

const reconcileAttachmentDecay: WorkerTaskHandler = async (payload, helpers) => {
	const next = await reconcileAttachmentDecayChunk(PayloadSchema.parse(payload));
	if (next === null) return;
	await helpers.addJob('reconcileAttachmentDecay', next, {
		jobKey: attachmentDecayReconcileJobKey(next.runId, next.chunk),
		skipLedger: true,
	});
};

export default reconcileAttachmentDecay;
