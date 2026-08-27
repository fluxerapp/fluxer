// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createMessageID,
	createRoleID,
	createUserID,
	type GuildID,
	type MessageID,
	type RoleID,
	type UserID,
} from '../../BrandedTypes';
import type {GatewayMentionSourceEntry, IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import {getWorkerDependencies} from '../WorkerContext';

const MENTION_CHUNK_SIZE = 250;
const MENTION_CHUNK_KEY_DIGEST_LENGTH = 32;
const MENTION_CHUNK_ENQUEUE_CONCURRENCY = 16;
const MENTION_SOURCE_PAGE_SIZE = 5000;
const PayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	authorId: z.string(),
	guildId: z.string().optional(),
	mentionHere: z.boolean().optional(),
	mentionEveryone: z.boolean().optional(),
	mentionUserIds: z.array(z.string()).optional(),
	mentionRoleIds: z.array(z.string()).optional(),
});

interface MentionSources {
	direct: Map<string, UserID>;
	role: Map<string, UserID>;
	everyone: Map<string, UserID>;
}

interface MentionChunkEntry {
	userId: string;
	direct: boolean;
	role: boolean;
	everyone: boolean;
}

function createMentionSources(): MentionSources {
	return {
		direct: new Map(),
		role: new Map(),
		everyone: new Map(),
	};
}

function addMentionSource(target: Map<string, UserID>, userIds: Array<UserID>): void {
	for (const userId of userIds) {
		target.set(userId.toString(), userId);
	}
}

function getMentionChunkEntries(sources: MentionSources): Array<MentionChunkEntry> {
	const entries = new Map<string, MentionChunkEntry>();
	for (const [sourceName, source] of [
		['direct', sources.direct],
		['role', sources.role],
		['everyone', sources.everyone],
	] as const) {
		for (const [key] of source) {
			let entry = entries.get(key);
			if (!entry) {
				entry = {
					userId: key,
					direct: false,
					role: false,
					everyone: false,
				};
				entries.set(key, entry);
			}
			entry[sourceName] = true;
		}
	}
	return Array.from(entries.values());
}

function chunkEntries(entries: Array<MentionChunkEntry>, chunkSize: number): Array<Array<MentionChunkEntry>> {
	const chunks: Array<Array<MentionChunkEntry>> = [];
	for (let index = 0; index < entries.length; index += chunkSize) {
		chunks.push(entries.slice(index, index + chunkSize));
	}
	return chunks;
}

function toMentionChunkEntry(entry: GatewayMentionSourceEntry): MentionChunkEntry {
	return {
		userId: entry.userId.toString(),
		direct: entry.direct,
		role: entry.role,
		everyone: entry.everyone,
	};
}

function mentionChunkJobKey(messageId: string, chunk: Array<MentionChunkEntry>): string {
	const digest = createHash('sha256');
	for (const entry of chunk) {
		digest.update(`${entry.userId}:${entry.direct ? 1 : 0}:${entry.role ? 1 : 0}:${entry.everyone ? 1 : 0}\n`);
	}
	return `mention-chunk:${messageId}:${digest.digest('hex').slice(0, MENTION_CHUNK_KEY_DIGEST_LENGTH)}`;
}

async function enqueueMentionChunks({
	chunks,
	channelId,
	messageId,
	guildId,
	addJob,
	firstChunkIndex = 0,
	chunkCount,
}: {
	chunks: Array<Array<MentionChunkEntry>>;
	channelId: string;
	messageId: string;
	guildId?: string;
	addJob: WorkerTaskHelpers['addJob'];
	firstChunkIndex?: number;
	chunkCount?: number;
}): Promise<number> {
	for (let offset = 0; offset < chunks.length; offset += MENTION_CHUNK_ENQUEUE_CONCURRENCY) {
		const chunkSlice = chunks.slice(offset, offset + MENTION_CHUNK_ENQUEUE_CONCURRENCY);
		await Promise.all(
			chunkSlice.map((chunk, localIndex) => {
				const index = firstChunkIndex + offset + localIndex;
				return addJob(
					'handleMentionChunk',
					{
						channelId,
						messageId,
						guildId,
						chunkIndex: index,
						...(chunkCount !== undefined && {chunkCount}),
						mentions: chunk,
					},
					{
						jobKey: mentionChunkJobKey(messageId, chunk),
						skipLedger: true,
					},
				);
			}),
		);
	}
	return firstChunkIndex + chunks.length;
}

async function enqueueGuildMentionSourcePages({
	gatewayService,
	guildId,
	channelId,
	authorId,
	messageId,
	mentionEveryone,
	mentionHere,
	roleIds,
	userIds,
	addJob,
}: {
	gatewayService: IGatewayService;
	guildId: GuildID;
	channelId: ChannelID;
	authorId: UserID;
	messageId: MessageID;
	mentionEveryone: boolean;
	mentionHere: boolean;
	roleIds: Array<RoleID>;
	userIds: Array<UserID>;
	addJob: WorkerTaskHelpers['addJob'];
}): Promise<{totalMentioned: number; chunkCount: number; pageCount: number}> {
	let cursor: string | undefined;
	let chunkIndex = 0;
	let pageCount = 0;
	let totalMentioned = 0;
	do {
		const page = await gatewayService.resolveMentionSourcesPage({
			guildId,
			channelId,
			authorId,
			mentionEveryone,
			mentionHere,
			roleIds,
			userIds,
			limit: MENTION_SOURCE_PAGE_SIZE,
			...(cursor !== undefined && {cursor}),
		});
		pageCount++;
		totalMentioned += page.mentions.length;
		const chunks = chunkEntries(page.mentions.map(toMentionChunkEntry), MENTION_CHUNK_SIZE);
		chunkIndex = await enqueueMentionChunks({
			chunks,
			channelId: channelId.toString(),
			messageId: messageId.toString(),
			guildId: guildId.toString(),
			addJob,
			firstChunkIndex: chunkIndex,
		});
		const nextCursor = page.nextCursor ?? undefined;
		if (nextCursor !== undefined && nextCursor === cursor) {
			throw new Error('Gateway mention source page cursor did not advance');
		}
		cursor = nextCursor;
	} while (cursor !== undefined);
	return {
		totalMentioned,
		chunkCount: chunkIndex,
		pageCount,
	};
}

const handleMentions: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing handleMentions task');
	const {channelRepository, gatewayService} = getWorkerDependencies();
	const authorId = createUserID(BigInt(validated.authorId));
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	const payloadGuildId = validated.guildId ? createGuildID(BigInt(validated.guildId)) : null;
	const mentionHere = validated.mentionHere ?? false;
	const message = await channelRepository.getMessage(channelId, messageId);
	if (!message) {
		Logger.debug({messageId}, 'handleMentions: Message not found, skipping');
		return;
	}
	const channel = await channelRepository.findUnique(channelId);
	if (!channel) {
		Logger.debug({channelId}, 'handleMentions: Channel not found, skipping');
		return;
	}
	const mentionEveryone = validated.mentionEveryone ?? (message.mentionEveryone && !mentionHere);
	const roleIds =
		validated.mentionRoleIds?.map((roleId) => createRoleID(BigInt(roleId))) ?? Array.from(message.mentionedRoleIds);
	const userIds =
		validated.mentionUserIds?.map((userId) => createUserID(BigInt(userId))) ?? Array.from(message.mentionedUserIds);
	if (channel.guildId) {
		const result = await enqueueGuildMentionSourcePages({
			gatewayService,
			guildId: channel.guildId,
			channelId,
			authorId,
			mentionEveryone,
			mentionHere,
			roleIds,
			userIds,
			messageId,
			addJob: helpers.addJob,
		});
		if (result.totalMentioned === 0) {
			Logger.debug({channelId, guildId: channel.guildId}, 'No users to mention, skipping read state updates');
			return;
		}
		Logger.debug(
			{
				channelId,
				guildId: channel.guildId,
				totalMentioned: result.totalMentioned,
				chunkCount: result.chunkCount,
				pageCount: result.pageCount,
				everyoneMention: mentionEveryone,
				hereMention: mentionHere,
				roleCount: roleIds.length,
				userCount: userIds.length,
			},
			'Enqueued paged mention source chunks',
		);
		return;
	}
	const sources = createMentionSources();
	const dmMentionedUserIds = userIds.filter((userId) => userId !== authorId);
	addMentionSource(sources.direct, dmMentionedUserIds);
	Logger.debug({channelId, userMentionCount: dmMentionedUserIds.length}, 'Handled DM user mentions');
	const mentionEntries = getMentionChunkEntries(sources);
	if (mentionEntries.length === 0) {
		Logger.debug(
			{channelId, guildId: channel.guildId ?? payloadGuildId},
			'No users to mention, skipping read state updates',
		);
		return;
	}
	const effectiveGuildId = channel.guildId ?? payloadGuildId;
	const chunks = chunkEntries(mentionEntries, MENTION_CHUNK_SIZE);
	await enqueueMentionChunks({
		chunks,
		channelId: channelId.toString(),
		messageId: messageId.toString(),
		guildId: effectiveGuildId?.toString(),
		addJob: helpers.addJob,
		chunkCount: chunks.length,
	});
	Logger.debug(
		{
			channelId,
			guildId: effectiveGuildId,
			totalMentioned: mentionEntries.length,
			chunkCount: chunks.length,
			everyoneMentions: mentionEveryone ? 1 : 0,
			roleMentions: roleIds.length,
			userMentions: userIds.length,
		},
		'Enqueued mention chunks',
	);
};

export default handleMentions;
