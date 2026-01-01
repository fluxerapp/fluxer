/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {AttachmentDecayService} from '~/attachment/AttachmentDecayService';
import {
	type AttachmentID,
	type ChannelID,
	createGuildID,
	type GuildID,
	type MessageID,
	type RoleID,
	type StickerID,
	type UserID,
	type WebhookID,
} from '~/BrandedTypes';
import {MessageFlags, Permissions, SENDABLE_MESSAGE_FLAGS} from '~/Constants';
import type {AttachmentToProcess} from '~/channel/AttachmentDTOs';
import type {AllowedMentionsRequest, MessageUpdateRequest, RichEmbedRequest} from '~/channel/ChannelModel';
import type {IChannelRepositoryAggregate} from '~/channel/repositories/IChannelRepositoryAggregate';
import type {
	MessageAttachment,
	MessageEmbed,
	MessageReference,
	MessageSnapshot,
	MessageStickerItem,
} from '~/database/CassandraTypes';
import {InputValidationError} from '~/Errors';
import type {GuildMemberResponse, GuildResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {EmbedService} from '~/infrastructure/EmbedService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {IVirusScanService} from '~/infrastructure/IVirusScanService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {Channel, Message, User} from '~/Models';
import type {PackService} from '~/pack/PackService';
import type {ReadStateService} from '~/read_state/ReadStateService';
import type {IUserRepository} from '~/user/IUserRepository';
import * as BucketUtils from '~/utils/BucketUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {AttachmentProcessingService} from './AttachmentProcessingService';
import {MessageContentService} from './MessageContentService';
import {MessageEmbedAttachmentResolver} from './MessageEmbedAttachmentResolver';
import {VIRUS_MESSAGE_PREFIX, VIRUS_RECIPE_SUGGESTIONS} from './MessageHelpers';
import {MessageStickerService} from './MessageStickerService';

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
	attachmentDecayExcludedIds?: Array<AttachmentID>;
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
		mentionEveryone: boolean;
	};
	allowEmbeds?: boolean;
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
		guildRepository: IGuildRepository,
		private packService: PackService,
		private embedService: EmbedService,
		storageService: IStorageService,
		mediaService: IMediaService,
		virusScanService: IVirusScanService,
		snowflakeService: SnowflakeService,
		private readStateService: ReadStateService,
	) {
		this.attachmentService = new AttachmentProcessingService(
			storageService,
			mediaService,
			virusScanService,
			snowflakeService,
		);
		this.contentService = new MessageContentService(this.userRepository, guildRepository, this.packService);
		this.stickerService = new MessageStickerService(this.userRepository, guildRepository, this.packService);
		this.embedAttachmentResolver = new MessageEmbedAttachmentResolver();
		this.attachmentDecayService = new AttachmentDecayService();
	}

	getEmbedAttachmentResolver(): MessageEmbedAttachmentResolver {
		return this.embedAttachmentResolver;
	}

	async createMessage(params: CreateMessageParams): Promise<Message> {
		const authorId = params.user?.id ?? params.userId ?? null;

		const mentionData =
			params.mentionData ??
			({
				flags: params.flags,
				mentionUserIds: [],
				mentionRoleIds: [],
				mentionEveryone: false,
			} as const);

		const isNSFWAllowed = this.contentService.isNSFWContentAllowed({
			channel: params.channel,
			guild: params.guild,
			member: params.member,
		});

		const [sanitizedContent, attachmentResult, processedStickers] = await Promise.all([
			this.sanitizeContentIfNeeded(params, authorId),
			this.processAttachments(params, isNSFWAllowed),
			this.processStickers(params, authorId),
		]);

		let messageContent = sanitizedContent;
		let processedAttachments: Array<MessageAttachment> = params.processedAttachments
			? [...params.processedAttachments]
			: [];

		if (attachmentResult) {
			if (attachmentResult.hasVirusDetected) {
				const randomIndex = Math.floor(Math.random() * VIRUS_RECIPE_SUGGESTIONS.length);
				messageContent = VIRUS_MESSAGE_PREFIX + VIRUS_RECIPE_SUGGESTIONS[randomIndex];
				processedAttachments = [];
			} else {
				processedAttachments = [...processedAttachments, ...attachmentResult.attachments];
			}
		}

		const allowEmbeds = params.allowEmbeds ?? true;
		let initialEmbeds: Array<MessageEmbed> | null = null;
		let hasUncachedUrls = false;

		if (allowEmbeds) {
			const resolvedEmbeds = this.embedAttachmentResolver.resolveEmbedAttachmentUrls({
				embeds: params.embeds,
				attachments: processedAttachments.map((att) => ({
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
				})),
				channelId: params.channelId,
			});

			const embedResult = await this.embedService.getInitialEmbeds({
				content: messageContent,
				customEmbeds: resolvedEmbeds,
				isNSFWAllowed,
			});

			initialEmbeds = embedResult.embeds;
			hasUncachedUrls = embedResult.hasUncachedUrls;
		}

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
			mention_channels: null,
			attachments: processedAttachments.length > 0 ? processedAttachments : null,
			embeds: allowEmbeds ? initialEmbeds : null,
			sticker_items: processedStickers.length > 0 ? processedStickers : null,
			message_reference: params.messageReference || null,
			message_snapshots: params.messageSnapshots || null,
			call: null,
			has_reaction: false,
			version: 1,
		};

		const message = await this.channelRepository.messages.upsertMessage(messageRowData);

		await this.runPostPersistenceOperations({
			params,
			authorId,
			processedAttachments,
			allowEmbeds,
			hasUncachedUrls,
			isNSFWAllowed,
		});

		return message;
	}

	private async sanitizeContentIfNeeded(params: CreateMessageParams, authorId: UserID | null): Promise<string | null> {
		const messageContent = params.content ?? null;
		if (!messageContent || !params.channel) {
			return messageContent;
		}

		return this.contentService.sanitizeCustomEmojis({
			content: messageContent,
			userId: authorId,
			webhookId: params.webhookId ?? null,
			guildId: params.guildId,
			hasPermission: params.guildId ? params.hasPermission : undefined,
		});
	}

	private async processAttachments(
		params: CreateMessageParams,
		isNSFWAllowed: boolean,
	): Promise<{attachments: Array<MessageAttachment>; hasVirusDetected: boolean} | null> {
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
			isNSFWAllowed,
		});
	}

	private async processStickers(
		params: CreateMessageParams,
		authorId: UserID | null,
	): Promise<Array<MessageStickerItem>> {
		if (!params.stickerIds || params.stickerIds.length === 0) {
			return [];
		}

		return this.stickerService.computeStickerIds({
			stickerIds: params.stickerIds,
			userId: authorId,
			guildId: params.guildId,
			hasPermission: params.hasPermission,
		});
	}

	private async runPostPersistenceOperations(context: {
		params: CreateMessageParams;
		authorId: UserID | null;
		processedAttachments: Array<MessageAttachment>;
		allowEmbeds: boolean;
		hasUncachedUrls: boolean;
		isNSFWAllowed: boolean;
	}): Promise<void> {
		const {params, authorId, processedAttachments, allowEmbeds, hasUncachedUrls, isNSFWAllowed} = context;

		const operations: Array<Promise<void>> = [];

		const excludedAttachmentIds = new Set(params.attachmentDecayExcludedIds ?? []);
		if (processedAttachments.length > 0) {
			const attachmentsForDecay = processedAttachments.filter((att) => !excludedAttachmentIds.has(att.attachment_id));
			if (attachmentsForDecay.length > 0) {
				const uploadedAt = new Date(SnowflakeUtils.extractTimestamp(params.messageId));
				const decayPayloads = attachmentsForDecay.map((att) => ({
					attachmentId: att.attachment_id,
					channelId: params.channelId,
					messageId: params.messageId,
					filename: att.filename,
					sizeBytes: att.size ?? 0n,
					uploadedAt,
				}));
				operations.push(this.attachmentDecayService.upsertMany(decayPayloads));
			}
		}

		if (allowEmbeds && hasUncachedUrls) {
			operations.push(
				this.embedService.enqueueUrlEmbedExtraction(params.channelId, params.messageId, params.guildId, isNSFWAllowed),
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
					}),
				);
			}
		}

		if (operations.length > 0) {
			await Promise.all(operations);
		}
	}

	async updateMessage(params: {
		message: Message;
		messageId: MessageID;
		data: MessageUpdateRequest;
		channel: Channel;
		guild: GuildResponse | null;
		member?: GuildMemberResponse | null;
		allowEmbeds?: boolean;
	}): Promise<Message> {
		const {message, messageId, data, channel, guild, member} = params;

		if (message.messageSnapshots && message.messageSnapshots.length > 0) {
			throw InputValidationError.create('message', 'Messages with snapshots cannot be edited');
		}

		const isNSFWAllowed = this.contentService.isNSFWContentAllowed({
			channel,
			guild,
			member,
		});

		const updatedRowData = {...message.toRow()};
		let hasChanges = false;
		const allowEmbeds = params.allowEmbeds ?? true;

		if (data.content !== undefined && data.content !== message.content) {
			let sanitizedContent = data.content;
			if (sanitizedContent) {
				sanitizedContent = await this.contentService.sanitizeCustomEmojis({
					content: sanitizedContent,
					userId: message.authorId ?? null,
					webhookId: message.webhookId ?? null,
					guildId: channel.guildId,
				});
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
				type EditAttachment =
					| (AttachmentToProcess & {upload_filename: string})
					| {id: number; title?: string | null; description?: string | null};

				const newAttachments: Array<AttachmentToProcess> = [];
				const existingAttachments: Array<MessageAttachment> = [];

				for (const att of data.attachments as Array<EditAttachment>) {
					if ('upload_filename' in att && att.upload_filename) {
						newAttachments.push(att as AttachmentToProcess);
					} else {
						const refId = BigInt(att.id);
						let found = message.attachments.find((existing) => existing.id === refId);
						if (!found && refId < BigInt(message.attachments.length)) {
							found = message.attachments[Number(refId)];
						}
						if (found) {
							const updated = found.toMessageAttachment();
							if ('title' in att && att.title !== undefined) {
								updated.title = att.title;
							}
							if ('description' in att && att.description !== undefined) {
								updated.description = att.description;
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
						isNSFWAllowed,
					});
					processedNewAttachments = attachmentResult.attachments;

					if (attachmentResult.hasVirusDetected) {
						const randomIndex = Math.floor(Math.random() * VIRUS_RECIPE_SUGGESTIONS.length);
						updatedRowData.content = VIRUS_MESSAGE_PREFIX + VIRUS_RECIPE_SUGGESTIONS[randomIndex];
					}
				}

				const allAttachments = [...existingAttachments, ...processedNewAttachments];
				updatedRowData.attachments = allAttachments.length > 0 ? allAttachments : null;
			} else {
				updatedRowData.attachments = null;
			}
			hasChanges = true;
		}

		if (allowEmbeds && (data.content !== undefined || data.embeds !== undefined)) {
			const attachmentsForResolution = updatedRowData.attachments || [];
			const resolvedEmbeds = this.embedAttachmentResolver.resolveEmbedAttachmentUrls({
				embeds: data.embeds,
				attachments: attachmentsForResolution.map((att) => ({
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
				})),
				channelId: channel.id,
			});

			const {embeds: initialEmbeds, hasUncachedUrls: embedUrls} = await this.embedService.getInitialEmbeds({
				content: updatedRowData.content ?? null,
				customEmbeds: resolvedEmbeds,
				isNSFWAllowed,
			});
			updatedRowData.embeds = initialEmbeds;
			hasChanges = true;

			if (embedUrls) {
				await this.embedService.enqueueUrlEmbedExtraction(
					channel.id,
					messageId,
					guild?.id ? createGuildID(BigInt(guild.id)) : null,
					isNSFWAllowed,
				);
			}
		}

		let updatedMessage = message;
		if (hasChanges) {
			updatedMessage = await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
		}

		return updatedMessage;
	}

	async handleNonAuthorEdit(params: {
		message: Message;
		data: MessageUpdateRequest;
		guild: GuildResponse | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
	}): Promise<{canEdit: boolean; updatedFlags?: number}> {
		const {message, data, guild, hasPermission} = params;

		if (guild && data.flags != null) {
			const canManage = await hasPermission(Permissions.MANAGE_MESSAGES);
			if (canManage) {
				let updatedFlags: number;
				if (data.flags & MessageFlags.SUPPRESS_EMBEDS) {
					updatedFlags = message.flags | MessageFlags.SUPPRESS_EMBEDS;
				} else {
					updatedFlags = message.flags & ~MessageFlags.SUPPRESS_EMBEDS;
				}

				return {canEdit: true, updatedFlags};
			}
		}

		return {canEdit: false};
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
		return this.createMessage({
			messageId: params.messageId,
			channelId: params.channelId,
			userId: params.userId,
			type: params.type,
			content: params.content ?? null,
			flags: 0,
			guildId: params.guildId ?? null,
			messageReference: params.messageReference,
			mentionData: {
				flags: 0,
				mentionUserIds: params.mentionUserIds ?? [],
				mentionRoleIds: [],
				mentionEveryone: false,
			},
		});
	}
}
