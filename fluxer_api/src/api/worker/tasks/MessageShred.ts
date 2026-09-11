// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {seconds} from 'itty-time';
import {z} from 'zod';
import type {ChannelID, MessageID} from '../../BrandedTypes';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import {purgeMessageAttachments} from '../../channel/services/message/MessageHelpers';
import type {Message} from '../../models/Message';
import {deleteMessageSearchDocuments} from '../../search/MessageSearchIndexCleanup';
import {chunkArray} from '../../utils/ArrayUtils';
import {getWorkerDependencies} from '../WorkerContext';
import {createBulkDeleteDispatcher} from './utils/MessageDeletion';

const PayloadSchema = z.object({
	job_id: z.string().min(1),
	admin_user_id: z.string().min(1),
	target_user_id: z.string().min(1),
	entries: z.array(
		z.object({
			channel_id: z.string(),
			message_id: z.string(),
		}),
	),
});
const INPUT_SLICE_SIZE = 500;
const VALIDATION_CHUNK_SIZE = 25;
const DELETION_CHUNK_SIZE = 10;
const STATUS_TTL_SECONDS = seconds('1 hour');
const messageShredTask: WorkerTaskHandler = async (payload, helpers) => {
	const data = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: data}, 'Processing messageShred task');
	const {kvClient, channelRepository, gatewayService, storageService, purgeQueue} = getWorkerDependencies();
	const progressKey = `message_shred_status:${data.job_id}`;
	const requestedEntries = data.entries.length;
	const startedAt = new Date().toISOString();
	let skippedCount = 0;
	let processedCount = 0;
	let totalValidCount = 0;
	const persistStatus = async (
		status: 'in_progress' | 'completed' | 'failed',
		extra?: {
			completed_at?: string;
			failed_at?: string;
			error?: string;
		},
	) => {
		await kvClient.set(
			progressKey,
			JSON.stringify({
				status,
				requested: requestedEntries,
				total: totalValidCount,
				processed: processedCount,
				skipped: skippedCount,
				started_at: startedAt,
				...extra,
			}),
			'EX',
			STATUS_TTL_SECONDS,
		);
	};
	await persistStatus('in_progress');
	const authorId = createUserID(BigInt(data.target_user_id));
	const seen = new Set<string>();
	const bulkDeleteDispatcher = createBulkDeleteDispatcher({
		channelRepository,
		gatewayService,
		batchSize: DELETION_CHUNK_SIZE,
	});
	const processSlice = async (
		entriesSlice: Array<{
			channel_id: string;
			message_id: string;
		}>,
	) => {
		const typedSlice: Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}> = [];
		for (const entry of entriesSlice) {
			const key = `${entry.channel_id}:${entry.message_id}`;
			if (seen.has(key)) {
				skippedCount += 1;
				continue;
			}
			seen.add(key);
			try {
				typedSlice.push({
					channelId: createChannelID(BigInt(entry.channel_id)),
					messageId: createMessageID(BigInt(entry.message_id)),
				});
			} catch (error) {
				skippedCount += 1;
				helpers.logger.warn({error, entry}, 'Skipping malformed entry in message shred job');
			}
		}
		if (typedSlice.length === 0) {
			return;
		}
		for (const validationChunk of chunkArray(typedSlice, VALIDATION_CHUNK_SIZE)) {
			const messageFetches = validationChunk.map(
				({channelId, messageId}: {channelId: ChannelID; messageId: MessageID}) =>
					channelRepository.getMessage(channelId, messageId),
			);
			const fetchedMessages = await Promise.all(messageFetches);
			const deletableChunk: Array<{
				channelId: ChannelID;
				messageId: MessageID;
				message: Message;
			}> = [];
			for (let i = 0; i < validationChunk.length; i++) {
				const message = fetchedMessages[i];
				if (message && String(message.authorId) === data.target_user_id) {
					deletableChunk.push({...validationChunk[i]!, message});
				} else {
					skippedCount += 1;
				}
			}
			if (deletableChunk.length === 0) {
				await persistStatus('in_progress');
				continue;
			}
			totalValidCount += deletableChunk.length;
			await persistStatus('in_progress');
			for (const deletionChunk of chunkArray(deletableChunk, DELETION_CHUNK_SIZE)) {
				await Promise.all(
					deletionChunk.map(
						async ({channelId, messageId, message}: {channelId: ChannelID; messageId: MessageID; message: Message}) => {
							if (message.attachments.length > 0) {
								await purgeMessageAttachments(message, storageService, purgeQueue);
							}
							return channelRepository.deleteMessage(channelId, messageId, authorId);
						},
					),
				);
				processedCount += deletionChunk.length;
				await deleteMessageSearchDocuments(
					deletionChunk.map(({messageId}) => messageId),
					{context: {source: 'message_shred', jobId: data.job_id}},
				);
				await persistStatus('in_progress');
				for (const {channelId, messageId} of deletionChunk) {
					bulkDeleteDispatcher.track(channelId, messageId);
				}
				await bulkDeleteDispatcher.flush(true);
			}
		}
	};
	for (const entriesSlice of chunkArray(data.entries, INPUT_SLICE_SIZE)) {
		await processSlice(entriesSlice);
	}
	await persistStatus('completed', {
		completed_at: new Date().toISOString(),
	});
	await bulkDeleteDispatcher.flush(true);
};

export default messageShredTask;
