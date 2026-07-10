// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageFlags, Permissions, SENDABLE_MESSAGE_FLAGS} from '@fluxer/constants/src/ChannelConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {NsfwEmojiStickerBlockedError} from '@fluxer/errors/src/domains/moderation/NsfwEmojiStickerBlockedError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import {AttachmentDecayService} from '../../../attachment/AttachmentDecayService';
import type {ChannelID, EmojiID, GuildID, MessageID, RoleID, StickerID, UserID, WebhookID} from '../../../BrandedTypes';
import {createAttachmentID, createEmojiID, createGuildID} from '../../../BrandedTypes';
import {getContentMessage} from '../../../content_i18n/ContentI18n';
import type {
	MessageAttachment,
	MessageEmbed,
	MessageReference,
	MessageStickerItem,
} from '../../../database/types/MessageTypes';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import type {EmbedService} from '../../../infrastructure/EmbedService';
import type {IMediaService, MediaProxyNsfwMode} from '../../../infrastructure/IMediaService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {MessageSnapshot} from '../../../models/MessageSnapshot';
import type {User} from '../../../models/User';
import type {PackService} from '../../../pack/PackService';
import type {ReadStateService} from '../../../read_state/ReadStateService';
import type {IUserRepository} from '../../../user/IUserRepository';
import {hasVisibleContent} from '../../../utils/StringUtils';
import type {AttachmentToProcess} from '../../AttachmentDTOs';
import type {MessageUpdateRequest} from '../../MessageTypes';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AttachmentUploadTraceRepository} from '../../repositories/message/AttachmentUploadTraceRepository';
import {AttachmentProcessingService} from './AttachmentProcessingService';
import {type DmNsfwContext, MessageContentService} from './MessageContentService';
import {MessageEmbedAttachmentResolver} from './MessageEmbedAttachmentResolver';
import {collectMessageAttachments} from './MessageHelpers';
import {MessageStickerService} from './MessageStickerService';

function mapAttachmentForEmbedResolution(att: MessageAttachment) {
	return {
		attachment_id: att.attachment_id,
		filename: att.filename,
		width: att.width ?? null,
		height: att.height ?? null,
		content_type: att.content_type,
		content_hash: att.content_hash ?? null,
		placeholder: att.placeholder ?? null,
		flags: att.flags ?? 0,
		duration: att.duration ?? null,
		nsfw: att.nsfw ?? null,
	};
}

interface CreateMessageResult {
	message: Message;
	enqueueDeferredEmbeds: () => Promise<void>;
}

interface UpdateMessageResult {
	message: Message;
	enqueueDeferredEmbeds: () => Promise<void>;
}

interface CreateMessageParams {
	messageId: MessageID;
	channelId: ChannelID;
	user?: User;
	userId?: UserID;
	webhookId?: WebhookID;
	webhookName?: string;
	webhookAvatar?: string | null;
	type: number;
	content: string | null | undefined;
	flags: number;
	embeds?: Array<RichEmbedRequest>;
	attachments?: Array<AttachmentToProcess>;
	processedAttachments?: Array<MessageAttachment>;
	stickerIds?: Array<StickerID>;
	messageReference?: MessageReference;
	messageSnapshots?: Array<MessageSnapshot>;
	guildId: GuildID | null;
	channel?: Channel;
	referencedMessage?: Message | null;
	allowedMentions?: AllowedMentionsRequest | null;
	guild?: GuildResponse | null;
	member?: GuildMemberResponse | null;
	hasPermission?: (permission: bigint) => Promise<boolean>;
	mentionData?: {
		flags: number;
		mentionUserIds: Array<UserID>;
		mentionRoleIds: Array<RoleID>;
		mentionChannelIds: Array<ChannelID>;
		mentionEveryone: boolean;
	};
	allowEmbeds?: boolean;
	dmNsfwContext?: DmNsfwContext;
}

export class MessagePersistenceService {
	private readonly attachmentService: AttachmentProcessingService;
	private readonly contentService: MessageContentService;
	private readonly stickerService: MessageStickerService;
	private readonly embedAttachmentResolver: MessageEmbedAttachmentResolver;
	private readonly attachmentDecayService: AttachmentDecayService;

	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private packService: PackService,
		private embedService: EmbedService,
		storageService: IStorageService,
		attachmentUploadTraceRepository: AttachmentUploadTraceRepository,
		mediaService: IMediaService,
		virusScanService: IVirusScanService,
		snowflakeService: ISnowflakeService,
		private readStateService: ReadStateService,
		limitConfigService: LimitConfigService,
	) {
		this.attachmentService = new AttachmentProcessingService(
			storageService,
			attachmentUploadTraceRepository,
			mediaService,
			virusScanService,
			snowflakeService,
		);
		this.contentService = new MessageContentService(
			this.userRepository,
			guildRepository,
			this.packService,
			limitConfigService,
		);
		this.stickerService = new MessageStickerService(
			this.userRepository,
			guildRepository,
			this.packService,
			limitConfigService,
		);
		this.embedAttachmentResolver = new MessageEmbedAttachmentResolver();
		this.attachmentDecayService = new AttachmentDecayService();
	}

	getEmbedAttachmentResolver(): MessageEmbedAttachmentResolver {
		return this.embedAttachmentResolver;
	}

	async createMessage(params: CreateMessageParams): Promise<CreateMessageResult> {
		const authorId = params.user?.id ?? params.userId ?? null;
		const mentionData =
			params.mentionData ??
			({
				flags: params.flags,
				mentionUserIds: [],
				mentionRoleIds: [],
				mentionChannelIds: [],
				mentionEveryone: false,
			} as const);
		const isBot = params.user?.isBot ?? false;
		const isBugHunterBot = isBot && ((params.user?.flags ?? 0n) & UserFlags.BUG_HUNTER) !== 0n;
		const isNSFWAllowed = this.contentService.isNSFWContentAllowed({
			channel: params.channel,
			guild: params.guild,
			member: params.member,
			isBot,
			dmNsfwContext: params.dmNsfwContext,
		});
		let nsfwEmojiIds = new Set<EmojiID>();
		if (params.content) {
			if (!isNSFWAllowed) {
				await this.enforceNsfwEmojiRestrictions(params.content);
			} else {
				nsfwEmojiIds = await this.collectNsfwEmojiIds(params.content);
			}
		}
		const [sanitizedContent, attachmentResult, processedStickers] = await Promise.all([
			this.sanitizeContentIfNeeded(params, authorId),
			this.processAttachments(params, isNSFWAllowed ? 'allow' : 'block'),
			this.processStickers(params, authorId, isNSFWAllowed),
		]);
		let messageContent = sanitizedContent;
		let processedAttachments: Array<MessageAttachment> = params.processedAttachments
			? [...params.processedAttachments]
			: [];
		if (attachmentResult) {
			if (attachmentResult.hasVirusDetected) {
				messageContent = getContentMessage('content.virus_detected', params.user?.locale);
				processedAttachments = [];
			} else {
				processedAttachments = [...processedAttachments, ...attachmentResult.attachments];
			}
		}
		const allowEmbeds = params.allowEmbeds ?? true;
		let initialEmbeds: Array<MessageEmbed> | null = null;
		let hasUncachedUrls = false;
		const referencedFilenames = this.embedAttachmentResolver.collectReferencedAttachmentFilenames(params.embeds);
		if (allowEmbeds) {
			const resolvedEmbeds = this.embedAttachmentResolver.resolveEmbedAttachmentUrls({
				embeds: params.embeds,
				attachments: processedAttachments.map(mapAttachmentForEmbedResolution),
				channelId: params.channelId,
			});
			const embedResult = await this.embedService.getInitialEmbeds({
				content: messageContent,
				customEmbeds: resolvedEmbeds,
				nsfwMode: isNSFWAllowed ? 'allow' : 'block',
				isBugHunterBot,
			});
			initialEmbeds = embedResult.embeds;
			hasUncachedUrls = embedResult.hasUncachedUrls;
		}
		const messageAttachments =
			referencedFilenames.size > 0
				? processedAttachments.filter((att) => !referencedFilenames.has(att.filename))
				: processedAttachments;
		const messageRowData = {
			channel_id: params.channelId,
			bucket: BucketUtils.makeBucket(params.messageId),
			message_id: params.messageId,
			author_id: authorId,
			type: params.type,
			webhook_id: params.webhookId || null,
			webhook_name: params.webhookName || null,
			webhook_avatar_hash: params.webhookAvatar || null,
			content: messageContent,
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: mentionData.flags,
			mention_everyone: mentionData.mentionEveryone,
			mention_users: mentionData.mentionUserIds.length > 0 ? new Set(mentionData.mentionUserIds) : null,
			mention_roles: mentionData.mentionRoleIds.length > 0 ? new Set(mentionData.mentionRoleIds) : null,
			mention_channels: mentionData.mentionChannelIds.length > 0 ? new Set(mentionData.mentionChannelIds) : null,
			attachments: messageAttachments.length > 0 ? messageAttachments : null,
			embeds: allowEmbeds ? initialEmbeds : null,
			sticker_items: processedStickers.length > 0 ? processedStickers : null,
			message_reference: params.messageReference || null,
			message_snapshots:
				params.messageSnapshots && params.messageSnapshots.length > 0
					? params.messageSnapshots.map((snapshot) => snapshot.toMessageSnapshot())
					: null,
			call: null,
			nsfw_emojis: nsfwEmojiIds.size > 0 ? nsfwEmojiIds : null,
			has_reaction: false,
			version: 1,
		};
		const message = await this.channelRepository.messages.upsertMessage(messageRowData);
		const enqueueDeferredEmbeds = await this.runPostPersistenceOperations({
			message,
			params,
			authorId,
			allowEmbeds,
			hasUncachedUrls,
			isNSFWAllowed,
		});
		return {message, enqueueDeferredEmbeds};
	}

	private async sanitizeContentIfNeeded(params: CreateMessageParams, authorId: UserID | null): Promise<string | null> {
		const messageContent = params.content ?? null;
		if (!messageContent || !hasVisibleContent(messageContent)) {
			return null;
		}
		if (!params.channel) {
			return messageContent;
		}
		const sanitizedContent = await this.contentService.sanitizeCustomEmojis({
			content: messageContent,
			userId: authorId,
			webhookId: params.webhookId ?? null,
			guildId: params.guildId,
			hasPermission: params.guildId ? params.hasPermission : undefined,
		});
		return hasVisibleContent(sanitizedContent) ? sanitizedContent : null;
	}

	private async processAttachments(
		params: CreateMessageParams,
		nsfwMode: MediaProxyNsfwMode,
	): Promise<{
		attachments: Array<MessageAttachment>;
		hasVirusDetected: boolean;
	} | null> {
		if (!params.attachments || params.attachments.length === 0) {
			return null;
		}
		return this.attachmentService.computeAttachments({
			message: {
				id: params.messageId,
				channelId: params.channelId,
			} as Message,
			attachments: params.attachments,
			channel: params.channel,
			guild: params.guild,
			member: params.member,
			nsfwMode,
		});
	}

	private async processStickers(
		params: CreateMessageParams,
		authorId: UserID | null,
		isNSFWAllowed?: boolean,
	): Promise<Array<MessageStickerItem>> {
		if (!params.stickerIds || params.stickerIds.length === 0) {
			return [];
		}
		return this.stickerService.computeStickerIds({
			stickerIds: params.stickerIds,
			userId: authorId,
			guildId: params.guildId,
			hasPermission: params.hasPermission,
			isNSFWAllowed: isNSFWAllowed ?? true,
		});
	}

	private async enforceNsfwEmojiRestrictions(content: string): Promise<void> {
		const nsfwIds = await this.collectNsfwEmojiIds(content);
		if (nsfwIds.size > 0) {
			throw new NsfwEmojiStickerBlockedError();
		}
	}

	private async collectNsfwEmojiIds(content: string): Promise<Set<EmojiID>> {
		const CUSTOM_EMOJI_REGEX = /<a?:[^:]+:(\d+)>/g;
		const emojiIds = new Set<EmojiID>();
		let match: RegExpExecArray | null;
		while ((match = CUSTOM_EMOJI_REGEX.exec(content)) !== null) {
			emojiIds.add(createEmojiID(BigInt(match[1])));
		}
		if (emojiIds.size === 0) {
			return new Set();
		}
		const lookups = await Promise.all([...emojiIds].map((id) => this.guildRepository.getEmojiById(id)));
		const nsfwEmojiIds = new Set<EmojiID>();
		for (const emoji of lookups) {
			if (emoji?.isNsfw) {
				nsfwEmojiIds.add(emoji.id);
			}
		}
		return nsfwEmojiIds;
	}

	private async runPostPersistenceOperations(context: {
		message: Message;
		params: CreateMessageParams;
		authorId: UserID | null;
		allowEmbeds: boolean;
		hasUncachedUrls: boolean;
		isNSFWAllowed: boolean;
	}): Promise<() => Promise<void>> {
		const {message, params, authorId, allowEmbeds, hasUncachedUrls, isNSFWAllowed} = context;
		const operations: Array<Promise<unknown>> = [];
		const trackedAttachments = collectMessageAttachments(message);
		if (trackedAttachments.length > 0) {
			const uploadedAt = snowflakeToDate(params.messageId);
			const decayPayloads = trackedAttachments.map((att) => ({
				attachmentId: att.id,
				channelId: params.channelId,
				messageId: params.messageId,
				filename: att.filename,
				sizeBytes: att.size,
				uploadedAt,
			}));
			operations.push(this.attachmentDecayService.upsertMany(decayPayloads));
		}
		let enqueueDeferredEmbeds: () => Promise<void> = () => Promise.resolve();
		if (allowEmbeds && hasUncachedUrls) {
			enqueueDeferredEmbeds = () =>
				this.embedService.enqueueUrlEmbedExtraction(
					params.channelId,
					params.messageId,
					params.guildId,
					isNSFWAllowed ? 'allow' : 'block',
					{content: message.content},
				);
		}
		if (authorId) {
			const isBot = params.user?.isBot ?? false;
			if (!isBot) {
				operations.push(
					this.readStateService.ackMessage({
						userId: authorId,
						channelId: params.channelId,
						messageId: params.messageId,
						mentionCount: 0,
						silent: true,
						emitGateway: false,
					}),
				);
			}
		}
		if (operations.length > 0) {
			await Promise.all(operations);
		}
		return enqueueDeferredEmbeds;
	}

	async updateMessage(params: {
		message: Message;
		messageId: MessageID;
		data: MessageUpdateRequest;
		channel: Channel;
		guild: GuildResponse | null;
		member?: GuildMemberResponse | null;
		allowEmbeds?: boolean;
		isBot?: boolean;
		isBugHunterBot?: boolean;
		locale?: string | null;
	}): Promise<UpdateMessageResult> {
		const {message, messageId, data, channel, guild, member} = params;
		if (message.messageSnapshots && message.messageSnapshots.length > 0) {
			throw InputValidationError.fromCode('message', ValidationErrorCodes.MESSAGES_WITH_SNAPSHOTS_CANNOT_BE_EDITED);
		}
		const isNSFWAllowed = this.contentService.isNSFWContentAllowed({
			channel,
			guild,
			member,
			isBot: params.isBot,
		});
		const updatedRowData = {...message.toRow()};
		let hasChanges = false;
		const allowEmbeds = params.allowEmbeds ?? true;
		let hasUncachedUrls = false;
		if (data.content !== undefined && data.content !== message.content) {
			let sanitizedContent = data.content && hasVisibleContent(data.content) ? data.content : '';
			if (sanitizedContent) {
				if (!isNSFWAllowed) {
					await this.enforceNsfwEmojiRestrictions(sanitizedContent);
				}
				sanitizedContent = await this.contentService.sanitizeCustomEmojis({
					content: sanitizedContent,
					userId: message.authorId ?? null,
					webhookId: message.webhookId ?? null,
					guildId: channel.guildId,
				});
				if (!hasVisibleContent(sanitizedContent)) {
					sanitizedContent = '';
				}
			}
			updatedRowData.content = sanitizedContent;
			updatedRowData.edited_timestamp = new Date();
			hasChanges = true;
		}
		if (data.flags !== undefined) {
			const preservedFlags = message.flags & ~SENDABLE_MESSAGE_FLAGS;
			const newFlags = data.flags & SENDABLE_MESSAGE_FLAGS;
			updatedRowData.flags = preservedFlags | newFlags;
			hasChanges = true;
		}
		if (data.attachments !== undefined) {
			if (data.attachments.length > 0) {
				type EditNewAttachment = AttachmentToProcess & {
					upload_filename: string;
				};
				type EditExistingAttachment = {
					id: bigint;
					title?: string | null;
					description?: string | null;
				};
				type EditAttachment = EditNewAttachment | EditExistingAttachment;
				const newAttachments: Array<AttachmentToProcess> = [];
				const existingAttachments: Array<MessageAttachment> = [];
				for (const att of data.attachments as Array<EditAttachment>) {
					if ('upload_filename' in att && att.upload_filename) {
						newAttachments.push(att as AttachmentToProcess);
					} else {
						const existingAtt = att as EditExistingAttachment;
						const refId = createAttachmentID(existingAtt.id);
						let found = message.attachments.find((existing) => existing.id === refId);
						if (!found && refId < BigInt(message.attachments.length)) {
							found = message.attachments[Number(refId)];
						}
						if (found) {
							const updated = found.toMessageAttachment();
							if ('title' in existingAtt && existingAtt.title !== undefined) {
								updated.title = existingAtt.title;
							}
							if ('description' in existingAtt && existingAtt.description !== undefined) {
								updated.description = existingAtt.description;
							}
							existingAttachments.push(updated);
						}
					}
				}
				let processedNewAttachments: Array<MessageAttachment> = [];
				if (newAttachments.length > 0) {
					const attachmentResult = await this.attachmentService.computeAttachments({
						message,
						attachments: newAttachments,
						channel,
						guild,
						member,
						nsfwMode: isNSFWAllowed ? 'allow' : 'block',
					});
					processedNewAttachments = attachmentResult.attachments;
					if (attachmentResult.hasVirusDetected) {
						updatedRowData.content = getContentMessage('content.virus_detected', params.locale);
					}
				}
				const allAttachments = [...existingAttachments, ...processedNewAttachments];
				updatedRowData.attachments = allAttachments.length > 0 ? allAttachments : null;
			} else {
				updatedRowData.attachments = null;
			}
			hasChanges = true;
		}
		if (allowEmbeds && (data.embeds !== undefined || (data.content !== undefined && message.embeds.length === 0))) {
			const attachmentsForResolution = updatedRowData.attachments || [];
			const resolvedEmbeds = this.embedAttachmentResolver.resolveEmbedAttachmentUrls({
				embeds: data.embeds,
				attachments: attachmentsForResolution.map(mapAttachmentForEmbedResolution),
				channelId: channel.id,
			});
			const referencedFilenames = this.embedAttachmentResolver.collectReferencedAttachmentFilenames(data.embeds);
			if (referencedFilenames.size > 0 && updatedRowData.attachments) {
				const filtered = updatedRowData.attachments.filter((att) => !referencedFilenames.has(att.filename));
				updatedRowData.attachments = filtered.length > 0 ? filtered : null;
			}
			const {embeds: initialEmbeds, hasUncachedUrls: embedUrls} = await this.embedService.getInitialEmbeds({
				content: updatedRowData.content ?? null,
				customEmbeds: resolvedEmbeds,
				nsfwMode: isNSFWAllowed ? 'allow' : 'block',
				isBugHunterBot: params.isBugHunterBot,
			});
			updatedRowData.embeds = initialEmbeds;
			hasUncachedUrls = embedUrls;
			hasChanges = true;
		}
		let updatedMessage = message;
		if (hasChanges) {
			updatedMessage = await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
		}
		const enqueueDeferredEmbeds =
			allowEmbeds && hasUncachedUrls
				? () =>
						this.embedService.enqueueUrlEmbedExtraction(
							channel.id,
							messageId,
							guild?.id ? createGuildID(BigInt(guild.id)) : null,
							isNSFWAllowed ? 'allow' : 'block',
							{content: updatedMessage.content},
						)
				: () => Promise.resolve();
		return {message: updatedMessage, enqueueDeferredEmbeds};
	}

	async updateSnapshotAttachments(params: {
		message: Message;
		snapshotEdits: ReadonlyArray<{
			attachments?: ReadonlyArray<{
				id: bigint | number;
				title?: string | null;
				description?: string | null;
			}>;
		}>;
	}): Promise<Message> {
		const {message, snapshotEdits} = params;
		if (!message.messageSnapshots || message.messageSnapshots.length === 0) {
			throw InputValidationError.fromCode('message_snapshots', ValidationErrorCodes.INVALID_MESSAGE_DATA);
		}
		const updatedSnapshots = message.messageSnapshots.map((snapshot, index) => {
			const edit = snapshotEdits[index];
			if (!edit || !edit.attachments || edit.attachments.length === 0) {
				return snapshot.toMessageSnapshot();
			}
			const snapshotRow = snapshot.toMessageSnapshot();
			const existingAttachments = snapshotRow.attachments ?? [];
			const editsByRefId = new Map<
				bigint,
				{
					title?: string | null;
					description?: string | null;
				}
			>();
			for (const att of edit.attachments) {
				const refId = createAttachmentID(typeof att.id === 'number' ? BigInt(att.id) : att.id);
				editsByRefId.set(refId, {title: att.title, description: att.description});
			}
			const newAttachments = existingAttachments.map((existing, attIndex) => {
				const matchById = editsByRefId.get(existing.attachment_id);
				const fallback = matchById === undefined ? editsByRefId.get(createAttachmentID(BigInt(attIndex))) : undefined;
				const patch = matchById ?? fallback;
				if (!patch) return existing;
				return {
					...existing,
					title: patch.title !== undefined ? patch.title : existing.title,
					description: patch.description !== undefined ? patch.description : existing.description,
				};
			});
			return {...snapshotRow, attachments: newAttachments.length > 0 ? newAttachments : null};
		});
		const updatedRowData = {...message.toRow(), message_snapshots: updatedSnapshots};
		return await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
	}

	async handleNonAuthorEdit(params: {
		message: Message;
		data: MessageUpdateRequest;
		guild: GuildResponse | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
	}): Promise<{
		canEdit: boolean;
		updatedFlags?: number;
		updatedAttachments?: Array<MessageAttachment>;
	}> {
		const {message, data, guild, hasPermission} = params;
		if (!guild) {
			return {canEdit: false};
		}
		const hasEditableFields = data.flags != null || data.attachments !== undefined;
		if (!hasEditableFields) {
			return {canEdit: false};
		}
		const canManage = await hasPermission(Permissions.MANAGE_MESSAGES);
		if (!canManage) {
			return {canEdit: false};
		}
		let updatedFlags: number | undefined;
		let updatedAttachments: Array<MessageAttachment> | undefined;
		if (data.flags != null) {
			if (data.flags & MessageFlags.SUPPRESS_EMBEDS) {
				updatedFlags = message.flags | MessageFlags.SUPPRESS_EMBEDS;
			} else {
				updatedFlags = message.flags & ~MessageFlags.SUPPRESS_EMBEDS;
			}
		}
		if (data.attachments !== undefined) {
			type EditExistingAttachment = {
				id: bigint;
				title?: string | null;
				description?: string | null;
			};
			type EditAttachment =
				| (AttachmentToProcess & {
						upload_filename: string;
				  })
				| EditExistingAttachment;
			for (const att of data.attachments as Array<EditAttachment>) {
				if (!('upload_filename' in att)) {
					const allowedKeys = new Set(['id', 'title', 'description', 'flags']);
					const disallowedEditKeys = new Set(['filename', 'duration', 'waveform']);
					const actualKeys = Object.keys(att);
					const hasDisallowedKeys =
						actualKeys.some((key) => !allowedKeys.has(key)) || actualKeys.some((key) => disallowedEditKeys.has(key));
					if (hasDisallowedKeys) {
						throw InputValidationError.fromCode('attachments', ValidationErrorCodes.CANNOT_EDIT_ATTACHMENT_METADATA);
					}
				}
			}
			const processedAttachments: Array<MessageAttachment> = [];
			for (const att of data.attachments as Array<EditAttachment>) {
				if (!('upload_filename' in att)) {
					const existingAtt = att as EditExistingAttachment;
					const refId = createAttachmentID(existingAtt.id);
					let found = message.attachments.find((existing) => existing.id === refId);
					if (!found && refId < BigInt(message.attachments.length)) {
						found = message.attachments[Number(refId)];
					}
					if (found) {
						const updated = found.toMessageAttachment();
						if ('title' in existingAtt && existingAtt.title !== undefined) {
							updated.title = existingAtt.title;
						}
						if ('description' in existingAtt && existingAtt.description !== undefined) {
							updated.description = existingAtt.description;
						}
						processedAttachments.push(updated);
					}
				}
			}
			updatedAttachments = processedAttachments;
		}
		return {canEdit: true, updatedFlags, updatedAttachments};
	}

	async createSystemMessage(params: {
		messageId: MessageID;
		channelId: ChannelID;
		userId: UserID;
		type: number;
		content?: string | null;
		guildId?: GuildID | null;
		mentionUserIds?: Array<UserID>;
		messageReference?: MessageReference;
	}): Promise<Message> {
		const {message} = await this.createMessage({
			messageId: params.messageId,
			channelId: params.channelId,
			userId: params.userId,
			type: params.type,
			content: params.content ?? null,
			flags: 0,
			guildId: params.guildId ?? null,
			allowEmbeds: false,
			messageReference: params.messageReference,
			mentionData: {
				flags: 0,
				mentionUserIds: params.mentionUserIds ?? [],
				mentionRoleIds: [],
				mentionChannelIds: [],
				mentionEveryone: false,
			},
		});
		return message;
	}
}
