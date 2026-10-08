// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {ChannelID, GuildID, MessageID, UserID, WebhookID} from '@app/api/BrandedTypes';
import {createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {CrosspostedMessageKey} from '@app/api/channel/repositories/ICrosspostedMessageRepository';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {
	collectEmbedContentHashes,
	type EmbedMediaField,
	forEachEmbedMedia,
	parseAttachmentUrl,
} from '@app/api/channel/services/message/CrosspostEmbedObjects';
import {isCrosspostedMessage, isCrosspostSourcePurged} from '@app/api/channel/services/message/CrosspostPropagation';
import {MessageContentService} from '@app/api/channel/services/message/MessageContentService';
import {
	dispatchMessageCreateBroadcast,
	dispatchMessageUpdateBroadcast,
} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {isOperationDisabled, purgeMessageAttachments} from '@app/api/channel/services/message/MessageHelpers';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import {checkCrosspostContentRules} from '@app/api/channel/utils/CrosspostContentRules';
import {
	type ContentWarningChannelLike,
	channelToContentWarningView,
	computeEffectiveChannelNsfw,
	guildResponseToContentWarningView,
} from '@app/api/channel/utils/EffectiveContentWarning';
import type {CrosspostedMessageRow} from '@app/api/database/types/ChannelTypes';
import type {
	MessageAttachment,
	MessageEmbed,
	MessageEmbedChild,
	MessageStickerItem,
} from '@app/api/database/types/MessageTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {contentModerationService, type ModerationContext} from '@app/api/infrastructure/ContentModerationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {Webhook} from '@app/api/models/Webhook';
import {deleteMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {
	CROSSPOST_PENDING_RECLAIM_AFTER_MS,
	CROSSPOST_SOURCE_DELETED_CONTENT,
} from '@fluxer/constants/src/AnnouncementConstants';
import {
	CHANNEL_FOLLOW_TARGET_TYPES,
	ChannelTypes,
	MessageFlags,
	MessageReferenceTypes,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
	WebhookTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {THREAD_MESSAGE_FLAG_MASK} from '@fluxer/constants/src/ThreadConstants';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

export type CrosspostCopySyncMode = 'update' | 'source_deleted' | 'purge';

export type CrosspostDeliveryOutcome = 'delivered' | 'skipped';

export interface CrosspostSourceContext {
	message: Message;
	channel: Channel;
	parent: Channel | null;
	guild: GuildResponse;
	fingerprint: string;
	accessCache: Map<string, Promise<boolean>>;
	authorAvatarCache: Map<string, Promise<string | null>>;
}

export type CrosspostSourceLoadResult =
	| {kind: 'missing'}
	| {kind: 'inactive'}
	| {kind: 'ready'; context: CrosspostSourceContext};

interface CrosspostTarget {
	channel: Channel;
	parent: Channel | null;
	guild: GuildResponse;
}

interface CrosspostCopyPayload {
	content: string | null;
	flags: number;
	attachments: Array<MessageAttachment>;
	embeds: Array<MessageEmbed>;
	stickerItems: Array<MessageStickerItem>;
}

type CrosspostCopyPayloadResult = {kind: 'blocked'} | {kind: 'ready'; payload: CrosspostCopyPayload};

export interface CrosspostDeliveryDeps {
	channelRepository: ChannelRepository;
	webhookRepository: IWebhookRepository;
	userRepository: IUserRepository;
	guildRepository: IGuildRepositoryAggregate;
	gatewayService: IGatewayService;
	storageService: IStorageService;
	avatarService: AvatarService;
	purgeQueue: IPurgeQueue;
	snowflakeService: ISnowflakeService;
	cacheService: ICacheService;
	limitConfigService: LimitConfigService;
	persistenceService: MessagePersistenceService;
	searchService: MessageSearchService;
}

const UNAVAILABLE_GUILD_FEATURES: ReadonlyArray<string> = [
	GuildFeatures.UNAVAILABLE_FOR_EVERYONE,
	GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF,
	GuildFeatures.UNAVAILABLE_HIDDEN,
];

export class CrosspostDeliveryPendingError extends Error {
	constructor(sourceMessageId: MessageID, webhookId: WebhookID) {
		super(`Crosspost delivery for ${sourceMessageId} to webhook ${webhookId} is still pending`);
		this.name = 'CrosspostDeliveryPendingError';
	}
}

export class CrosspostSyncConflictError extends Error {
	constructor(sourceMessageId: MessageID, webhookId: WebhookID) {
		super(`Crosspost copy for ${sourceMessageId} via webhook ${webhookId} changed during sync`);
		this.name = 'CrosspostSyncConflictError';
	}
}

function toStableValue(value: unknown): unknown {
	if (value === null || value === undefined) return null;
	if (typeof value === 'bigint') return value.toString();
	if (value instanceof Date) return value.toISOString();
	if (value instanceof Set) return [...value].map(toStableValue).sort();
	if (value instanceof Map) {
		return [...value.entries()]
			.map(([key, entry]) => [String(key), toStableValue(entry)] as const)
			.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
	}
	if (Array.isArray(value)) return value.map(toStableValue);
	if (typeof value === 'object') {
		const result: Record<string, unknown> = {};
		for (const key of Object.keys(value as Record<string, unknown>).sort()) {
			const entry = (value as Record<string, unknown>)[key];
			if (entry === undefined || entry === null) continue;
			result[key] = toStableValue(entry);
		}
		return result;
	}
	return value;
}

export function crosspostSourceFingerprint(source: Message): string {
	const state = {
		content: source.content ?? null,
		flags: source.flags & SENDABLE_MESSAGE_FLAGS,
		stickers: source.stickers.map((sticker) => sticker.id.toString()),
		embeds: source.embeds.map((embed) => embed.toMessageEmbed()),
		attachments: source.attachments.map((attachment) => ({
			id: attachment.id,
			filename: attachment.filename,
			title: attachment.title,
			description: attachment.description,
			flags: attachment.flags,
			nsfw: attachment.nsfw,
		})),
	};
	return createHash('sha256')
		.update(JSON.stringify(toStableValue(state)))
		.digest('hex');
}

function isGuildUnavailable(guild: GuildResponse): boolean {
	return guild.features.some((feature) => UNAVAILABLE_GUILD_FEATURES.includes(feature));
}

function withoutNsfwChildren(embed: MessageEmbed): MessageEmbed {
	if (!embed.children || embed.children.length === 0) return embed;
	const children = embed.children.filter((child) => !child.nsfw);
	return {...embed, children: children.length > 0 ? children : null};
}

function cloneEmbed(embed: MessageEmbed): MessageEmbed {
	const cloneChild = (child: MessageEmbedChild): MessageEmbedChild => ({
		...child,
		thumbnail: child.thumbnail ? {...child.thumbnail} : child.thumbnail,
		image: child.image ? {...child.image} : child.image,
		video: child.video ? {...child.video} : child.video,
		audio: child.audio ? {...child.audio} : child.audio,
	});
	return {
		...cloneChild(embed),
		children: embed.children ? embed.children.map(cloneChild) : embed.children,
	};
}

export class CrosspostDeliveryService {
	private readonly writeLock: MessageWriteLock;
	private readonly contentService: MessageContentService;

	constructor(private readonly deps: CrosspostDeliveryDeps) {
		this.writeLock = new MessageWriteLock(deps.cacheService, deps.channelRepository.messages);
		this.contentService = new MessageContentService(deps.userRepository, deps.guildRepository, deps.limitConfigService);
	}

	async loadSource(channelId: ChannelID, messageId: MessageID): Promise<CrosspostSourceContext | null> {
		const message = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) return null;
		const channel = await this.deps.channelRepository.findUnique(channelId);
		if (!channel?.guildId) return null;
		const guild = await this.loadGuild(channel.guildId);
		if (!guild) return null;
		const parent = await this.loadParent(channel);
		return {
			message,
			channel,
			parent,
			guild,
			fingerprint: crosspostSourceFingerprint(message),
			accessCache: new Map(),
			authorAvatarCache: new Map(),
		};
	}

	async loadSourceForDelivery(channelId: ChannelID, messageId: MessageID): Promise<CrosspostSourceLoadResult> {
		const context = await this.loadSource(channelId, messageId);
		if (!context) return {kind: 'missing'};
		if (
			!isCrosspostedMessage(context.message) ||
			context.channel.type !== ChannelTypes.GUILD_ANNOUNCEMENT ||
			!this.isSourceGuildActive(context.guild)
		) {
			return {kind: 'inactive'};
		}
		return {kind: 'ready', context};
	}

	isSourceGuildActive(guild: GuildResponse): boolean {
		return (
			!isGuildUnavailable(guild) &&
			!isOperationDisabled(guild, GuildOperations.SEND_MESSAGE) &&
			!guild.features.includes(GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED)
		);
	}

	async deliverToWebhook(context: CrosspostSourceContext, webhookId: WebhookID): Promise<CrosspostDeliveryOutcome> {
		const logContext = {
			sourceMessageId: context.message.id.toString(),
			webhookId: webhookId.toString(),
		};
		const webhook = await this.deps.webhookRepository.findUnique(webhookId);
		if (
			!webhook ||
			webhook.type !== WebhookTypes.CHANNEL_FOLLOWER ||
			webhook.sourceChannelId !== context.channel.id ||
			!webhook.channelId
		) {
			return 'skipped';
		}
		if (!(await this.creatorCanViewSource(context, webhook.creatorId))) {
			Logger.info(logContext, 'Skipping crosspost delivery: follower creator cannot view the source channel');
			return 'skipped';
		}
		const target = await this.loadDeliveryTarget(webhook.channelId);
		if (!target) {
			Logger.info(logContext, 'Skipping crosspost delivery: target channel cannot receive copies');
			return 'skipped';
		}
		if (this.checkContentRules(context, target) !== 'ok') {
			Logger.info(logContext, 'Skipping crosspost delivery: target does not meet the content rules');
			return 'skipped';
		}
		return this.deliverToTarget(context, webhook, target, true);
	}

	private async deliverToTarget(
		context: CrosspostSourceContext,
		webhook: Webhook,
		target: CrosspostTarget,
		allowRestart: boolean,
	): Promise<CrosspostDeliveryOutcome> {
		const {crossposts} = this.deps.channelRepository;
		const key: CrosspostedMessageKey = {sourceMessageId: context.message.id, webhookId: webhook.id};
		const existing = await crossposts.get(key.sourceMessageId, key.webhookId);
		if (existing?.state === 'delivered') {
			await this.closeDeliveryRace(context, key);
			return 'skipped';
		}
		if (existing?.state === 'pending') {
			const copy = await this.deps.channelRepository.messages.getMessage(
				existing.target_channel_id,
				existing.target_message_id,
			);
			if (copy) {
				const marked = await crossposts.markDelivered(key, {
					targetMessageId: existing.target_message_id,
					sourceFingerprint: null,
				});
				if (!marked) return 'skipped';
				const copyChannel =
					existing.target_channel_id === target.channel.id
						? target.channel
						: await this.deps.channelRepository.findUnique(existing.target_channel_id);
				if (copyChannel) {
					await this.announceCopy(copyChannel, copy);
				}
				await this.closeDeliveryRace(context, key);
				return 'delivered';
			}
			if (Date.now() - existing.reserved_at.getTime() < CROSSPOST_PENDING_RECLAIM_AFTER_MS) {
				throw new CrosspostDeliveryPendingError(key.sourceMessageId, key.webhookId);
			}
		}
		const authorAvatar = await this.resolveCopyAuthorAvatar(context, webhook);
		const built = this.buildCopyPayload(context, target);
		if (built.kind === 'blocked') {
			Logger.warn(
				{sourceMessageId: context.message.id.toString(), webhookId: webhook.id.toString()},
				'Skipping crosspost delivery: blocked content',
			);
			return 'skipped';
		}
		const {payload} = built;
		const messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(target.channel.id));
		const now = new Date();
		const reserved =
			existing?.state === 'pending'
				? await crossposts.reclaimPending(key, {
						fromTargetMessageId: existing.target_message_id,
						toTargetMessageId: messageId,
						reservedAt: now,
					})
				: await crossposts.insertPending({
						source_message_id: key.sourceMessageId,
						webhook_id: key.webhookId,
						source_channel_id: context.channel.id,
						target_guild_id: target.channel.guildId!,
						target_channel_id: target.channel.id,
						target_message_id: messageId,
						state: 'pending',
						reserved_at: now,
						source_fingerprint: null,
						created_at: now,
					});
		if (!reserved) {
			if (allowRestart) {
				return this.deliverToTarget(context, webhook, target, false);
			}
			throw new CrosspostDeliveryPendingError(key.sourceMessageId, key.webhookId);
		}
		const copy = await this.createCopy(context, webhook, target, messageId, payload, authorAvatar);
		const marked = await crossposts.markDelivered(key, {
			targetMessageId: messageId,
			sourceFingerprint: context.fingerprint,
		});
		if (!marked) {
			await this.deps.channelRepository.deleteMessage(target.channel.id, messageId, createUserID(0n));
			return 'skipped';
		}
		await this.announceCopy(target.channel, copy);
		await this.closeDeliveryRace(context, key);
		return 'delivered';
	}

	private async createCopy(
		context: CrosspostSourceContext,
		webhook: Webhook,
		target: CrosspostTarget,
		messageId: MessageID,
		payload: CrosspostCopyPayload,
		authorAvatar: string | null,
	): Promise<Message> {
		try {
			const {message} = await this.deps.persistenceService.createMessage({
				messageId,
				channelId: target.channel.id,
				webhookId: webhook.id,
				webhookName: webhook.name,
				webhookAvatar: authorAvatar,
				type: MessageTypes.DEFAULT,
				content: payload.content,
				flags: payload.flags,
				processedAttachments: payload.attachments,
				processedEmbeds: payload.embeds,
				processedStickerItems: payload.stickerItems,
				messageReference: {
					guild_id: context.channel.guildId,
					channel_id: context.channel.id,
					message_id: context.message.id,
					type: MessageReferenceTypes.DEFAULT,
				},
				mentionData: {
					flags: payload.flags,
					mentionUserIds: [],
					mentionRoleIds: [],
					mentionChannelIds: [],
					mentionEveryone: false,
				},
				guildId: target.channel.guildId,
				skipDeferredEmbeds: true,
			});
			return message;
		} catch (error) {
			await this.deps.channelRepository.deleteMessage(target.channel.id, messageId, createUserID(0n));
			await this.deps.channelRepository.crossposts.delete(
				{sourceMessageId: context.message.id, webhookId: webhook.id},
				{state: 'pending', target_message_id: messageId},
			);
			throw error;
		}
	}

	private resolveCopyAuthorAvatar(context: CrosspostSourceContext, webhook: Webhook): Promise<string | null> {
		const iconHash = context.guild.icon ?? null;
		if (!iconHash) return Promise.resolve(null);
		if (iconHash === webhook.avatarHash) return Promise.resolve(iconHash);
		const cacheKey = `${webhook.id}:${iconHash}`;
		const cached = context.authorAvatarCache.get(cacheKey);
		if (cached) return cached;
		const pending = this.deps.avatarService.ensureWebhookAvatarFromGuildIcon({
			guildId: context.channel.guildId!,
			iconHash,
			webhookId: webhook.id,
		});
		context.authorAvatarCache.set(cacheKey, pending);
		pending.catch(() => context.authorAvatarCache.delete(cacheKey));
		return pending;
	}

	private async announceCopy(channel: Channel, copy: Message): Promise<void> {
		await dispatchMessageCreateBroadcast({gatewayService: this.deps.gatewayService, channel, message: copy});
		if (channel.indexedAt != null) {
			void this.deps.searchService.indexMessage(copy, false, {includeDefault: true});
		}
	}

	private async closeDeliveryRace(context: CrosspostSourceContext, key: CrosspostedMessageKey): Promise<void> {
		const row = await this.deps.channelRepository.crossposts.get(key.sourceMessageId, key.webhookId);
		if (!row) return;
		const latest = await this.deps.channelRepository.messages.getMessage(context.channel.id, context.message.id);
		if (!latest) {
			await this.syncCopy(row, await this.removalModeFor(context.message.id), null);
			return;
		}
		if (crosspostSourceFingerprint(latest) !== row.source_fingerprint) {
			await this.syncCopy(row, 'update', await this.loadSource(context.channel.id, context.message.id));
		}
	}

	async removalModeFor(sourceMessageId: MessageID): Promise<'source_deleted' | 'purge'> {
		return (await isCrosspostSourcePurged(sourceMessageId)) ? 'purge' : 'source_deleted';
	}

	async syncCopy(
		row: CrosspostedMessageRow,
		mode: CrosspostCopySyncMode,
		source: CrosspostSourceContext | null,
	): Promise<void> {
		if (mode === 'update') {
			await this.syncUpdate(row, source);
			return;
		}
		if (mode === 'source_deleted') {
			await this.markCopySourceDeleted(row);
			return;
		}
		await this.purgeCopy(row);
	}

	private keyOf(row: CrosspostedMessageRow): CrosspostedMessageKey {
		return {sourceMessageId: row.source_message_id, webhookId: row.webhook_id};
	}

	private async syncUpdate(initialRow: CrosspostedMessageRow, source: CrosspostSourceContext | null): Promise<void> {
		const {crossposts} = this.deps.channelRepository;
		const key = this.keyOf(initialRow);
		const row = await crossposts.get(key.sourceMessageId, key.webhookId);
		if (row?.state !== 'delivered') return;
		if (!source) return;
		if (source.guild.features.includes(GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED)) {
			await this.dropRemovedSourceAttachments(row, source);
			return;
		}
		const fingerprint = source.fingerprint;
		if (fingerprint === row.source_fingerprint) return;
		const copy = await this.deps.channelRepository.messages.getMessage(row.target_channel_id, row.target_message_id);
		if (!copy) {
			await crossposts.delete(key, {state: 'delivered', target_message_id: row.target_message_id});
			return;
		}
		if ((copy.flags & MessageFlags.SOURCE_MESSAGE_DELETED) !== 0) return;
		const target = await this.loadSyncTarget(row.target_channel_id);
		if (!target) return;
		if (!(await this.syncStillAllowed(source, row, target))) {
			await this.markCopySourceDeleted(row);
			return;
		}
		const built = this.buildCopyPayload(source, target);
		if (built.kind === 'blocked') {
			Logger.warn(
				{sourceMessageId: row.source_message_id.toString(), webhookId: row.webhook_id.toString()},
				'Crosspost sync found blocked content, marking the copy as source deleted',
			);
			await this.markCopySourceDeleted(row);
			return;
		}
		const {payload} = built;
		const result = await this.writeLock.withFreshMessage(
			row.target_channel_id,
			row.target_message_id,
			async (fresh) => {
				const current = await crossposts.get(key.sourceMessageId, key.webhookId);
				const latestSource = await this.deps.channelRepository.messages.getMessage(
					source.channel.id,
					source.message.id,
				);
				if (current?.state === 'delivered' && current.source_fingerprint === fingerprint) {
					return {kind: 'current' as const};
				}
				if (
					current?.state !== 'delivered' ||
					current.target_message_id !== row.target_message_id ||
					!fresh ||
					(fresh.flags & MessageFlags.SOURCE_MESSAGE_DELETED) !== 0 ||
					!latestSource ||
					crosspostSourceFingerprint(latestSource) !== fingerprint
				) {
					return {kind: 'conflict' as const};
				}
				const updated = await this.deps.channelRepository.messages.upsertMessage(
					{
						...fresh.toRow(),
						content: payload.content,
						embeds: payload.embeds.length > 0 ? payload.embeds : null,
						attachments: payload.attachments.length > 0 ? payload.attachments : null,
						sticker_items: payload.stickerItems.length > 0 ? payload.stickerItems : null,
						flags: (fresh.flags & ~SENDABLE_MESSAGE_FLAGS) | payload.flags,
						edited_timestamp: new Date(),
					},
					fresh.toRow(),
				);
				await crossposts.updateSynced(key, {
					targetMessageId: row.target_message_id,
					sourceFingerprint: fingerprint,
				});
				return {kind: 'synced' as const, updated};
			},
		);
		if (result.kind === 'current') return;
		if (result.kind === 'conflict') {
			throw new CrosspostSyncConflictError(key.sourceMessageId, key.webhookId);
		}
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.deps.gatewayService,
			channel: target.channel,
			message: result.updated,
		});
		if (target.channel.indexedAt != null) {
			void this.deps.searchService.updateMessageIndex(result.updated, {includeDefault: true});
		}
	}

	private async dropRemovedSourceAttachments(
		row: CrosspostedMessageRow,
		source: CrosspostSourceContext,
	): Promise<void> {
		const updated = await this.writeLock.withFreshMessage(
			row.target_channel_id,
			row.target_message_id,
			async (fresh) => {
				if (!fresh || (fresh.flags & MessageFlags.SOURCE_MESSAGE_DELETED) !== 0) return null;
				const latestSource = await this.deps.channelRepository.messages.getMessage(
					source.channel.id,
					source.message.id,
				);
				if (!latestSource) return null;
				const liveIds = new Set(latestSource.attachments.map((attachment) => attachment.id));
				const kept = fresh.attachments.filter((attachment) => liveIds.has(attachment.id));
				if (kept.length === fresh.attachments.length) return null;
				return this.deps.channelRepository.messages.upsertMessage(
					{
						...fresh.toRow(),
						attachments: kept.length > 0 ? kept.map((attachment) => attachment.toMessageAttachment()) : null,
						edited_timestamp: new Date(),
					},
					fresh.toRow(),
				);
			},
		);
		if (!updated) return;
		const channel = await this.deps.channelRepository.findUnique(row.target_channel_id);
		if (!channel) return;
		await dispatchMessageUpdateBroadcast({gatewayService: this.deps.gatewayService, channel, message: updated});
		if (channel.indexedAt != null) {
			void this.deps.searchService.updateMessageIndex(updated, {includeDefault: true});
		}
	}

	private async syncStillAllowed(
		source: CrosspostSourceContext,
		row: CrosspostedMessageRow,
		target: CrosspostTarget,
	): Promise<boolean> {
		if (this.checkContentRules(source, target) !== 'ok') return false;
		const webhook = await this.deps.webhookRepository.findUnique(row.webhook_id);
		if (!webhook || webhook.sourceChannelId !== source.channel.id) return true;
		return this.creatorCanViewSource(source, webhook.creatorId);
	}

	private isStaleRow(row: CrosspostedMessageRow): boolean {
		return row.state === 'delivered' || Date.now() - row.reserved_at.getTime() >= CROSSPOST_PENDING_RECLAIM_AFTER_MS;
	}

	private async markCopySourceDeleted(row: CrosspostedMessageRow): Promise<void> {
		const {crossposts} = this.deps.channelRepository;
		const key = this.keyOf(row);
		const outcome = await this.writeLock.withFreshMessage(
			row.target_channel_id,
			row.target_message_id,
			async (fresh) => {
				if (!fresh) return {kind: 'missing' as const};
				if ((fresh.flags & MessageFlags.SOURCE_MESSAGE_DELETED) !== 0) {
					return {kind: 'already' as const};
				}
				const updated = await this.deps.channelRepository.messages.upsertMessage(
					{
						...fresh.toRow(),
						content: CROSSPOST_SOURCE_DELETED_CONTENT,
						attachments: null,
						embeds: null,
						sticker_items: null,
						flags:
							MessageFlags.IS_CROSSPOST |
							MessageFlags.SOURCE_MESSAGE_DELETED |
							(fresh.flags & THREAD_MESSAGE_FLAG_MASK),
						edited_timestamp: new Date(),
					},
					fresh.toRow(),
				);
				return {kind: 'marked' as const, updated};
			},
		);
		if (outcome.kind === 'missing') {
			if (this.isStaleRow(row)) {
				await crossposts.delete(key);
			}
			return;
		}
		if (outcome.kind === 'marked') {
			const channel = await this.deps.channelRepository.findUnique(row.target_channel_id);
			if (channel) {
				await dispatchMessageUpdateBroadcast({
					gatewayService: this.deps.gatewayService,
					channel,
					message: outcome.updated,
				});
				if (channel.indexedAt != null) {
					void this.deps.searchService.updateMessageIndex(outcome.updated, {includeDefault: true});
				}
			}
		}
		await crossposts.delete(key);
	}

	private async purgeCopy(row: CrosspostedMessageRow): Promise<void> {
		const {crossposts} = this.deps.channelRepository;
		const key = this.keyOf(row);
		const removed = await this.writeLock.withFreshMessage(
			row.target_channel_id,
			row.target_message_id,
			async (fresh) => {
				if (!fresh) return null;
				await this.deps.channelRepository.deleteMessage(
					fresh.channelId,
					fresh.id,
					fresh.authorId ?? createUserID(0n),
					fresh.pinnedTimestamp ?? undefined,
				);
				return fresh;
			},
		);
		if (!removed) {
			if (this.isStaleRow(row)) {
				await crossposts.delete(key);
			}
			return;
		}
		const channel = await this.deps.channelRepository.findUnique(row.target_channel_id);
		if (channel) {
			await dispatchChannelEvent({
				gatewayService: this.deps.gatewayService,
				channel,
				event: 'MESSAGE_DELETE',
				data: {channel_id: channel.id.toString(), id: removed.id.toString()},
			});
		}
		await deleteMessageSearchDocuments([removed.id], {context: {source: 'crosspost_purge'}});
		await crossposts.delete(key);
	}

	async deleteSourceMessage(channelId: ChannelID, messageId: MessageID): Promise<void> {
		const removed = await this.writeLock.withFreshMessage(channelId, messageId, async (fresh) => {
			if (!fresh) return null;
			await this.deps.channelRepository.deleteMessage(
				channelId,
				messageId,
				fresh.authorId ?? createUserID(0n),
				fresh.pinnedTimestamp ?? undefined,
			);
			return fresh;
		});
		if (!removed) return;
		await purgeMessageAttachments(removed, this.deps.storageService, this.deps.purgeQueue);
		const channel = await this.deps.channelRepository.findUnique(channelId);
		if (channel) {
			await dispatchChannelEvent({
				gatewayService: this.deps.gatewayService,
				channel,
				event: 'MESSAGE_DELETE',
				data: {channel_id: channelId.toString(), id: messageId.toString()},
			});
		}
		await deleteMessageSearchDocuments([messageId], {context: {source: 'crosspost_family_purge'}});
	}

	private buildCopyPayload(source: CrosspostSourceContext, target: CrosspostTarget): CrosspostCopyPayloadResult {
		const message = source.message;
		if (this.isBlocked(source)) {
			return {kind: 'blocked'};
		}
		const nsfwAllowed = this.targetAllowsNsfw(target);
		const excludedAttachmentIds = new Set<string>();
		const attachments: Array<MessageAttachment> = [];
		for (const attachment of message.attachments) {
			if (!nsfwAllowed && attachment.nsfw) {
				excludedAttachmentIds.add(attachment.id.toString());
				continue;
			}
			attachments.push(attachment.toMessageAttachment());
		}
		let embeds = message.embeds.map((embed) => cloneEmbed(embed.toMessageEmbed()));
		if (!nsfwAllowed) {
			embeds = embeds.filter((embed) => !embed.nsfw).map(withoutNsfwChildren);
		}
		const removedMedia: Array<{owner: MessageEmbedChild; field: EmbedMediaField}> = [];
		forEachEmbedMedia(embeds, (media, owner, field) => {
			const parsed = parseAttachmentUrl(media.url, source.channel.id);
			if (parsed && excludedAttachmentIds.has(parsed.id)) {
				removedMedia.push({owner, field});
			}
		});
		for (const {owner, field} of removedMedia) {
			owner[field] = null;
		}
		return {
			kind: 'ready',
			payload: {
				content: message.content,
				flags: MessageFlags.IS_CROSSPOST | (message.flags & SENDABLE_MESSAGE_FLAGS),
				attachments,
				embeds,
				stickerItems: message.stickers.map((sticker) => sticker.toMessageStickerItem()),
			},
		};
	}

	private isBlocked(source: CrosspostSourceContext): boolean {
		const message = source.message;
		const context: Omit<ModerationContext, 'surface'> = {
			userId: message.authorId,
			guildId: source.channel.guildId,
			channelId: message.channelId,
			messageId: message.id,
		};
		try {
			const textContext: ModerationContext = {...context, surface: 'message_content'};
			contentModerationService.scanText(message.content, textContext);
			for (const embed of message.embeds) {
				if (embed.type !== 'rich') continue;
				contentModerationService.scanText(embed.title, textContext);
				contentModerationService.scanText(embed.description, textContext);
				for (const field of embed.fields) {
					contentModerationService.scanText(field.name, textContext);
					contentModerationService.scanText(field.value, textContext);
				}
				contentModerationService.scanText(embed.footer?.text, textContext);
				contentModerationService.scanText(embed.author?.name, textContext);
			}
			for (const attachment of message.attachments) {
				if (attachment.contentHash) {
					contentModerationService.scanSha256(attachment.contentHash, {...context, surface: 'message_attachment'});
				}
			}
			for (const contentHash of collectEmbedContentHashes(message)) {
				contentModerationService.scanSha256(contentHash, {...context, surface: 'message_attachment'});
			}
		} catch (error) {
			if (error instanceof ContentBlockedError) return true;
			throw error;
		}
		return false;
	}

	private targetAllowsNsfw(target: CrosspostTarget): boolean {
		if (
			computeEffectiveChannelNsfw(
				channelToContentWarningView(target.channel),
				target.parent ? channelToContentWarningView(target.parent) : null,
				guildResponseToContentWarningView(target.guild),
			)
		) {
			return true;
		}
		return this.contentService.isNSFWContentAllowed({
			channel: target.channel,
			guild: target.guild,
			member: null,
			isBot: false,
		});
	}

	private checkContentRules(source: CrosspostSourceContext, target: CrosspostTarget) {
		return checkCrosspostContentRules({
			source: channelToContentWarningView(source.channel),
			sourceParent: this.parentView(source.parent),
			sourceGuild: guildResponseToContentWarningView(source.guild),
			target: channelToContentWarningView(target.channel),
			targetParent: this.parentView(target.parent),
			targetGuild: guildResponseToContentWarningView(target.guild),
		});
	}

	private parentView(parent: Channel | null): ContentWarningChannelLike | null {
		return parent ? channelToContentWarningView(parent) : null;
	}

	private async creatorCanViewSource(source: CrosspostSourceContext, creatorId: UserID | null): Promise<boolean> {
		if (!creatorId || !source.channel.guildId) return false;
		const cacheKey = creatorId.toString();
		let cached = source.accessCache.get(cacheKey);
		if (!cached) {
			cached = this.deps.gatewayService.checkPermission({
				guildId: source.channel.guildId,
				userId: creatorId,
				permission: Permissions.VIEW_CHANNEL,
				channelId: source.channel.id,
			});
			source.accessCache.set(cacheKey, cached);
		}
		return cached;
	}

	private async loadDeliveryTarget(channelId: ChannelID): Promise<CrosspostTarget | null> {
		const target = await this.loadSyncTarget(channelId);
		if (!target) return null;
		if (!CHANNEL_FOLLOW_TARGET_TYPES.has(target.channel.type)) return null;
		if (isGuildUnavailable(target.guild) || isOperationDisabled(target.guild, GuildOperations.SEND_MESSAGE)) {
			return null;
		}
		return target;
	}

	private async loadSyncTarget(channelId: ChannelID): Promise<CrosspostTarget | null> {
		const channel = await this.deps.channelRepository.findUnique(channelId);
		if (!channel?.guildId) return null;
		const guild = await this.loadGuild(channel.guildId);
		if (!guild) return null;
		return {channel, parent: await this.loadParent(channel), guild};
	}

	private async loadParent(channel: Channel): Promise<Channel | null> {
		if (!channel.parentId || channel.type === ChannelTypes.GUILD_CATEGORY) return null;
		return this.deps.channelRepository.findUnique(channel.parentId);
	}

	private async loadGuild(guildId: GuildID): Promise<GuildResponse | null> {
		try {
			return await this.deps.gatewayService.getGuildData({
				guildId,
				userId: createUserID(0n),
				skipMembershipCheck: true,
			});
		} catch (error) {
			if (error instanceof UnknownGuildError) {
				return null;
			}
			throw error;
		}
	}
}
