// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {MAX_EMBEDS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import type {ChannelID, GuildID, MessageID} from '../../BrandedTypes';
import {createChannelID, createGuildID, createMessageID, createUserID} from '../../BrandedTypes';
import type {ChannelRepository} from '../../channel/ChannelRepository';
import {buildBroadcastMessageData} from '../../channel/services/message/MessageGatewayDispatch';
import type {MessageEmbed, MessageEmbedChild} from '../../database/types/MessageTypes';
import type {ModerationContext} from '../../infrastructure/ContentModerationService';
import {contentModerationService} from '../../infrastructure/ContentModerationService';
import type {EmbedService} from '../../infrastructure/EmbedService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {MediaProxyNsfwMode} from '../../infrastructure/IMediaService';
import {Logger} from '../../Logger';
import type {Channel} from '../../models/Channel';
import {Message} from '../../models/Message';
import {deleteMessageSearchDocuments} from '../../search/MessageSearchIndexCleanup';
import * as UnfurlerUtils from '../../utils/UnfurlerUtils';
import {ChannelEventDispatcher} from '../services/ChannelEventDispatcher';
import {getWorkerDependencies} from '../WorkerContext';

const PayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	guildId: z.string().nullable().optional(),
	nsfwMode: z.enum(['block', 'flag', 'allow']).default('block'),
	bypassUnfurlCache: z.boolean().optional().default(false),
	expectedContentHash: z.string().optional(),
});

interface NormalizedEmbedAuthor {
	name: string | null;
	url: string | null;
	icon_url: string | null;
}

interface NormalizedEmbedField {
	name: string | null;
	value: string | null;
	inline: boolean;
}

interface NormalizedEmbedMedia {
	url: string | null;
	content_type: string | null;
	content_hash: string | null;
	width: number | null;
	height: number | null;
	description: string | null;
	placeholder: string | null;
	duration: number | null;
	flags: number;
}

interface NormalizedEmbedChild {
	type: string | null;
	title: string | null;
	description: string | null;
	url: string | null;
	timestamp: string | null;
	color: number | null;
	author: NormalizedEmbedAuthor | null;
	provider: NormalizedEmbedAuthor | null;
	thumbnail: NormalizedEmbedMedia | null;
	image: NormalizedEmbedMedia | null;
	video: NormalizedEmbedMedia | null;
	audio: NormalizedEmbedMedia | null;
	footer: {
		text: string | null;
		icon_url: string | null;
	} | null;
	fields: Array<NormalizedEmbedField>;
	html: string | null;
	html_width: number | null;
	html_height: number | null;
	nsfw: boolean | null;
}

interface NormalizedEmbed extends NormalizedEmbedChild {
	children: Array<NormalizedEmbedChild>;
}

function normalizeEmbedAuthor(
	author?: MessageEmbed['author'] | MessageEmbed['provider'],
): NormalizedEmbedAuthor | null {
	if (!author) {
		return null;
	}
	const iconUrl = 'icon_url' in author ? (author.icon_url ?? null) : null;
	return {
		name: author.name ?? null,
		url: author.url ?? null,
		icon_url: iconUrl,
	};
}

function normalizeEmbedMedia(media?: MessageEmbed['image']): NormalizedEmbedMedia | null {
	if (!media) {
		return null;
	}
	return {
		url: media.url ?? null,
		content_type: media.content_type ?? null,
		content_hash: media.content_hash ?? null,
		width: media.width ?? null,
		height: media.height ?? null,
		description: media.description ?? null,
		placeholder: media.placeholder ?? null,
		duration: media.duration ?? null,
		flags: media.flags ?? 0,
	};
}

function normalizeEmbedChildForComparison(embed: MessageEmbed | MessageEmbedChild): NormalizedEmbedChild {
	return {
		type: embed.type ?? null,
		title: embed.title ?? null,
		description: embed.description ?? null,
		url: embed.url ?? null,
		timestamp: embed.timestamp ? new Date(embed.timestamp).toISOString() : null,
		color: embed.color ?? null,
		author: normalizeEmbedAuthor(embed.author),
		provider: normalizeEmbedAuthor(embed.provider),
		thumbnail: normalizeEmbedMedia(embed.thumbnail ?? undefined),
		image: normalizeEmbedMedia(embed.image ?? undefined),
		video: normalizeEmbedMedia(embed.video ?? undefined),
		audio: normalizeEmbedMedia(embed.audio ?? undefined),
		footer: embed.footer
			? {
					text: embed.footer.text ?? null,
					icon_url: embed.footer.icon_url ?? null,
				}
			: null,
		fields: (embed.fields ?? []).map((field) => ({
			name: field.name ?? null,
			value: field.value ?? null,
			inline: field.inline ?? false,
		})),
		html: embed.html ?? null,
		html_width: embed.html_width ?? null,
		html_height: embed.html_height ?? null,
		nsfw: embed.nsfw ?? null,
	};
}

function normalizeEmbedForComparison(embed: MessageEmbed): NormalizedEmbed {
	return {
		...normalizeEmbedChildForComparison(embed),
		children: (embed.children ?? []).map((child) => normalizeEmbedChildForComparison(child)),
	};
}

function areEmbedsEquivalent(existingEmbeds: Array<MessageEmbed>, newEmbeds: Array<MessageEmbed>): boolean {
	if (existingEmbeds.length !== newEmbeds.length) {
		return false;
	}
	const normalizedExistingEmbeds = existingEmbeds.map((embed) => normalizeEmbedForComparison(embed));
	const normalizedNewEmbeds = newEmbeds.map((embed) => normalizeEmbedForComparison(embed));
	return JSON.stringify(normalizedExistingEmbeds) === JSON.stringify(normalizedNewEmbeds);
}

function isEmbedNsfw(embed: MessageEmbed): boolean {
	if (embed.nsfw) return true;
	for (const child of embed.children ?? []) {
		if (child.nsfw) return true;
	}
	return false;
}

const UNFURL_BATCH_TIMEOUT_MS = 20000;
const MAX_CONCURRENT_UNFURL_URLS = 2;
const MAX_UNFURL_URLS_PER_MESSAGE = MAX_EMBEDS_PER_MESSAGE;
const MESSAGE_READ_RETRY_ATTEMPTS = 8;
const MESSAGE_READ_RETRY_DELAY_MS = 100;
const MESSAGE_LOCK_TTL_SECONDS = 5;
const MESSAGE_LOCK_ACQUIRE_ATTEMPTS = 8;
const MESSAGE_LOCK_RETRY_DELAY_MS = 75;

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

function messageMatchesExpectedContent(message: Message, expectedContentHash?: string): boolean {
	return expectedContentHash === undefined || UnfurlerUtils.hashUnfurlContent(message.content) === expectedContentHash;
}

async function readMessageForExpectedContent(
	channelRepository: ChannelRepository,
	channelId: ChannelID,
	messageId: MessageID,
	expectedContentHash?: string,
): Promise<Message | null> {
	let lastMessage: Message | null = null;
	for (let attempt = 0; attempt < MESSAGE_READ_RETRY_ATTEMPTS; attempt++) {
		const message = await channelRepository.getMessage(channelId, messageId);
		if (message && messageMatchesExpectedContent(message, expectedContentHash)) {
			return message;
		}
		lastMessage = message;
		await sleep(MESSAGE_READ_RETRY_DELAY_MS * (attempt + 1));
	}
	if (!lastMessage) {
		Logger.info({messageId: messageId.toString()}, 'Skipping extractEmbeds: message not visible after retry');
	} else {
		Logger.info({messageId: messageId.toString()}, 'Skipping extractEmbeds: message content changed before processing');
	}
	return null;
}

async function withMessageWriteLock<T>(
	cacheService: ICacheService,
	channelId: ChannelID,
	messageId: MessageID,
	fn: () => Promise<T>,
): Promise<T> {
	const lockKey = `message:${channelId}:${messageId}:write`;
	let lockToken: string | null = null;
	for (let attempt = 0; attempt < MESSAGE_LOCK_ACQUIRE_ATTEMPTS; attempt++) {
		lockToken = await cacheService.acquireLock(lockKey, MESSAGE_LOCK_TTL_SECONDS);
		if (lockToken) break;
		await sleep(MESSAGE_LOCK_RETRY_DELAY_MS * (attempt + 1));
	}
	if (!lockToken) {
		throw new Error(`Timed out acquiring message write lock for ${channelId}:${messageId}`);
	}
	try {
		return await fn();
	} finally {
		await cacheService.releaseLock(lockKey, lockToken).catch((error: unknown) => {
			Logger.warn({error, lockKey}, 'Failed to release message write lock');
		});
	}
}

async function processUnfurlUrl(
	url: string,
	embedService: EmbedService,
	nsfwMode: MediaProxyNsfwMode,
	signal: AbortSignal,
	bypassCache: boolean,
	unfurledEmbedsByUrl: Map<string, Array<MessageEmbed>>,
): Promise<void> {
	if (signal.aborted) {
		throw new Error('Unfurl batch aborted');
	}
	const {embeds, cacheTtlSeconds} = await embedService.processUrlWithCachePolicy(url, nsfwMode, {
		signal,
		bypassCache,
	});
	if (embeds.length > 0) {
		const messageEmbeds = embeds.map((e) => e.toMessageEmbed());
		const safeEmbeds = nsfwMode === 'allow' ? messageEmbeds : messageEmbeds.filter((e) => !isEmbedNsfw(e));
		if (safeEmbeds.length > 0) {
			unfurledEmbedsByUrl.set(url, safeEmbeds);
			await embedService.cacheEmbeds(url, embeds, cacheTtlSeconds);
		}
	}
}

async function runUnfurlWorker(
	pendingUrls: Array<string>,
	embedService: EmbedService,
	nsfwMode: MediaProxyNsfwMode,
	signal: AbortSignal,
	bypassCache: boolean,
	unfurledEmbedsByUrl: Map<string, Array<MessageEmbed>>,
): Promise<void> {
	while (!signal.aborted) {
		const url = pendingUrls.shift();
		if (!url) {
			return;
		}
		await processUnfurlUrl(url, embedService, nsfwMode, signal, bypassCache, unfurledEmbedsByUrl);
	}
}

async function unfurlUrls(
	urlsToUnfurl: Array<string>,
	embedService: EmbedService,
	nsfwMode: MediaProxyNsfwMode,
	bypassCache: boolean,
): Promise<Map<string, Array<MessageEmbed>>> {
	const unfurledEmbedsByUrl = new Map<string, Array<MessageEmbed>>();
	const pendingUrls = [...urlsToUnfurl];
	const abortController = new AbortController();
	const workerCount = Math.min(MAX_CONCURRENT_UNFURL_URLS, pendingUrls.length);
	const workers = Array.from({length: workerCount}, () =>
		runUnfurlWorker(pendingUrls, embedService, nsfwMode, abortController.signal, bypassCache, unfurledEmbedsByUrl),
	);
	const workersDone = Promise.all(workers).then(() => undefined);
	let timeoutHandle: NodeJS.Timeout | undefined;
	const timeout = new Promise<void>((_, reject) => {
		timeoutHandle = setTimeout(() => {
			abortController.abort('Unfurl batch timed out');
			reject(
				new Error(
					`Unfurl batch exceeded total timeout (${UNFURL_BATCH_TIMEOUT_MS}ms) for ${urlsToUnfurl.length} URL(s)`,
				),
			);
		}, UNFURL_BATCH_TIMEOUT_MS);
	});
	try {
		await Promise.race([workersDone, timeout]);
		await workersDone;
	} finally {
		if (timeoutHandle) clearTimeout(timeoutHandle);
	}
	return unfurledEmbedsByUrl;
}

async function scanEmbedsForBannedContent(
	urls: Array<string>,
	unfurledEmbedsByUrl: Map<string, Array<MessageEmbed>>,
	ctx: ModerationContext,
): Promise<void> {
	for (const url of urls) {
		contentModerationService.scanUrl(url, ctx);
		const embeds = unfurledEmbedsByUrl.get(url);
		if (!embeds) {
			continue;
		}
		for (const embed of embeds) {
			const imageUrls = [embed.thumbnail?.url, embed.image?.url, embed.video?.url, embed.audio?.url].filter(
				(u): u is string => u != null,
			);
			for (const imageUrl of imageUrls) {
				contentModerationService.scanUrl(imageUrl, ctx);
			}
			const children = embed.children ?? [];
			for (const child of children) {
				const childImageUrls = [child.thumbnail?.url, child.image?.url, child.video?.url, child.audio?.url].filter(
					(u): u is string => u != null,
				);
				for (const childImageUrl of childImageUrls) {
					contentModerationService.scanUrl(childImageUrl, ctx);
				}
			}
		}
	}
}

function buildOrderedEmbeds(
	urls: Array<string>,
	unfurledEmbedsByUrl: Map<string, Array<MessageEmbed>>,
): Array<MessageEmbed> {
	const orderedEmbeds: Array<MessageEmbed> = [];
	for (const url of urls) {
		const embeds = unfurledEmbedsByUrl.get(url);
		if (embeds) {
			orderedEmbeds.push(...embeds);
		}
		if (orderedEmbeds.length >= MAX_EMBEDS_PER_MESSAGE) {
			return orderedEmbeds.slice(0, MAX_EMBEDS_PER_MESSAGE);
		}
	}
	return orderedEmbeds;
}

async function updateMessageEmbeds(
	channelRepository: ChannelRepository,
	freshMessage: Message,
	orderedEmbeds: Array<MessageEmbed>,
): Promise<Message | null> {
	const existingEmbeds = (freshMessage.embeds ?? []).map((embed) => embed.toMessageEmbed());
	const preservedEmbeds = existingEmbeds.filter((embed) => embed.type === 'rich');
	const nextEmbeds = [...preservedEmbeds, ...orderedEmbeds];
	if (areEmbedsEquivalent(existingEmbeds, nextEmbeds)) {
		Logger.debug({messageId: freshMessage.id.toString()}, 'Embeds unchanged, skipping update');
		return freshMessage;
	}
	const messageWithEmbeds = new Message({
		...freshMessage.toRow(),
		embeds: nextEmbeds.length > 0 ? nextEmbeds : null,
	});
	await channelRepository.updateEmbeds(messageWithEmbeds);
	return messageWithEmbeds;
}

interface DispatchEmbedUpdateParams {
	latestMessage: Message;
	channel: Channel;
	guildId: GuildID | null;
	gatewayService: IGatewayService;
}

async function dispatchEmbedUpdate({
	latestMessage,
	channel,
	guildId,
	gatewayService,
}: DispatchEmbedUpdateParams): Promise<void> {
	const messageWithUpdatedEmbeds = new Message({
		...latestMessage.toRow(),
		embeds: latestMessage.embeds.map((e) => e.toMessageEmbed()),
	});
	const messageData = await buildBroadcastMessageData({
		channel,
		message: messageWithUpdatedEmbeds,
		sourceGuildId: guildId ?? channel.guildId,
	});
	const eventDispatcher = new ChannelEventDispatcher({gatewayService});
	if (guildId && !channel.guildId) {
		await gatewayService.dispatchGuild({
			guildId,
			event: 'MESSAGE_UPDATE',
			data: messageData,
		});
	} else {
		await eventDispatcher.dispatchMessageUpdate(channel, messageData);
	}
	Logger.debug({messageId: latestMessage.id.toString()}, 'Dispatched MESSAGE_UPDATE after embed processing');
}

const extractEmbeds: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing extractEmbeds task');
	const {channelRepository, gatewayService, embedService, cacheService} = getWorkerDependencies();
	const messageId = createMessageID(BigInt(validated.messageId));
	const channelId = createChannelID(BigInt(validated.channelId));
	const message = await readMessageForExpectedContent(
		channelRepository,
		channelId,
		messageId,
		validated.expectedContentHash,
	);
	if (!message || !message.content) {
		Logger.info({messageId: messageId.toString()}, 'Skipping extractEmbeds: message not found or no content');
		return;
	}
	const channel = await channelRepository.findUnique(channelId);
	if (!channel) {
		Logger.info({channelId: channelId.toString()}, 'Skipping extractEmbeds: channel not found');
		return;
	}
	const guildId =
		validated.guildId && validated.guildId !== 'null' ? createGuildID(BigInt(validated.guildId)) : channel.guildId;
	const extractedUrls = UnfurlerUtils.extractURLs(message.content);
	const urls = extractedUrls.slice(0, MAX_UNFURL_URLS_PER_MESSAGE);
	if (urls.length === 0) {
		Logger.info({messageId: messageId.toString()}, 'Skipping extractEmbeds: no URLs found in content');
		return;
	}
	try {
		const nsfwMode = validated.nsfwMode;
		Logger.info(
			{messageId: messageId.toString(), urlCount: urls.length, skippedUrlCount: extractedUrls.length - urls.length},
			'Unfurling URLs',
		);
		const unfurledEmbedsByUrl = await unfurlUrls(urls, embedService, nsfwMode, validated.bypassUnfurlCache);
		if (unfurledEmbedsByUrl.size === 0) {
			Logger.info({messageId: messageId.toString(), urlsAttempted: urls.length}, 'No URLs were successfully unfurled');
			return;
		}
		const orderedEmbeds = buildOrderedEmbeds(urls, unfurledEmbedsByUrl);
		const handled = await withMessageWriteLock(cacheService, channelId, messageId, async () => {
			const latestExpectedMessage = await channelRepository.getMessage(channelId, messageId);
			if (!latestExpectedMessage) {
				Logger.debug({messageId: messageId.toString()}, 'Message no longer exists, skipping embed update');
				return false;
			}
			if (!messageMatchesExpectedContent(latestExpectedMessage, validated.expectedContentHash)) {
				Logger.info({messageId: messageId.toString()}, 'Skipping extractEmbeds: message content changed after unfurl');
				return false;
			}
			const moderationCtx: ModerationContext = {
				userId: latestExpectedMessage.authorId,
				guildId: guildId,
				channelId: channelId,
				messageId: messageId,
				surface: 'message_embed_unfurl',
			};
			try {
				await scanEmbedsForBannedContent(urls, unfurledEmbedsByUrl, moderationCtx);
			} catch (moderationError) {
				if (moderationError instanceof ContentBlockedError) {
					Logger.warn(
						{
							messageId: messageId.toString(),
							channelId: channelId.toString(),
							guildId: guildId?.toString() ?? null,
							userId: latestExpectedMessage.authorId?.toString() ?? null,
						},
						'Content moderation blocked embed unfurl, deleting parent message',
					);
					await channelRepository.deleteMessage(
						channelId,
						messageId,
						latestExpectedMessage.authorId || createUserID(0n),
						latestExpectedMessage.pinnedTimestamp || undefined,
					);
					await deleteMessageSearchDocuments([messageId], {context: {source: 'blocked_embed_unfurl'}});
					const eventDispatcher = new ChannelEventDispatcher({gatewayService});
					await eventDispatcher.dispatchMessageDelete(channel, messageId);
					return true;
				}
				Logger.error(
					{error: moderationError, messageId: messageId.toString()},
					'Content moderation check failed with non-blocking error, proceeding with embed',
				);
			}
			const latestMessage = await updateMessageEmbeds(channelRepository, latestExpectedMessage, orderedEmbeds);
			if (!latestMessage) {
				return false;
			}
			if (!(latestMessage.flags & MessageFlags.SUPPRESS_EMBEDS)) {
				await dispatchEmbedUpdate({
					latestMessage,
					channel,
					guildId,
					gatewayService,
				});
			} else {
				Logger.info({messageId: messageId.toString()}, 'Skipping MESSAGE_UPDATE dispatch due to SUPPRESS_EMBEDS flag');
			}
			return true;
		});
		if (!handled) {
			return;
		}
		Logger.info(
			{messageId: messageId.toString(), embedCount: orderedEmbeds.length},
			'Handled extractEmbeds successfully',
		);
	} catch (error) {
		Logger.error({error, messageId: messageId.toString()}, 'Failed to process embeds');
		throw error;
	}
};

export default extractEmbeds;
