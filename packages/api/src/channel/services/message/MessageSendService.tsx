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

import type {ChannelID, GuildID, RoleID, UserID} from '@fluxer/api/src/BrandedTypes';
import {
	createChannelID,
	createGuildID,
	createMessageID,
	createStickerID,
	createUserID,
} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {AttachmentRequestData, AttachmentToProcess} from '@fluxer/api/src/channel/AttachmentDTOs';
import type {MessageRequest} from '@fluxer/api/src/channel/MessageTypes';
import type {IChannelRepositoryAggregate} from '@fluxer/api/src/channel/repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '@fluxer/api/src/channel/services/AuthenticatedChannel';
import type {MessageChannelAuthService} from '@fluxer/api/src/channel/services/message/MessageChannelAuthService';
import type {MessageDispatchService} from '@fluxer/api/src/channel/services/message/MessageDispatchService';
import type {MessageEmbedAttachmentResolver} from '@fluxer/api/src/channel/services/message/MessageEmbedAttachmentResolver';
import {
	createMessageSnapshotsForForward,
	isOperationDisabled,
	isPersonalNotesChannel,
} from '@fluxer/api/src/channel/services/message/MessageHelpers';
import type {MessageMentionService} from '@fluxer/api/src/channel/services/message/MessageMentionService';
import type {MessageOperationsHelpers} from '@fluxer/api/src/channel/services/message/MessageOperationsHelpers';
import type {MessagePersistenceService} from '@fluxer/api/src/channel/services/message/MessagePersistenceService';
import type {MessageProcessingService} from '@fluxer/api/src/channel/services/message/MessageProcessingService';
import type {MessageSearchService} from '@fluxer/api/src/channel/services/message/MessageSearchService';
import type {MessageValidationService} from '@fluxer/api/src/channel/services/message/MessageValidationService';
import {SYSTEM_USER_ID} from '@fluxer/api/src/constants/Core';
import type {MessageAttachment, MessageReference} from '@fluxer/api/src/database/types/MessageTypes';
import type {IFavoriteMemeRepository} from '@fluxer/api/src/favorite_meme/IFavoriteMemeRepository';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {Channel} from '@fluxer/api/src/models/Channel';
import type {Message} from '@fluxer/api/src/models/Message';
import type {MessageSnapshot} from '@fluxer/api/src/models/MessageSnapshot';
import type {User} from '@fluxer/api/src/models/User';
import type {Webhook} from '@fluxer/api/src/models/Webhook';
import {withBusinessSpan} from '@fluxer/api/src/telemetry/BusinessSpans';
import {recordMessageSendDuration, recordMessageSent} from '@fluxer/api/src/telemetry/MessageTelemetry';
import {
	ChannelTypes,
	MessageReferenceTypes,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {CannotExecuteOnDmError} from '@fluxer/errors/src/domains/core/CannotExecuteOnDmError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {SlowmodeRateLimitError} from '@fluxer/errors/src/domains/core/SlowmodeRateLimitError';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

interface MessageSendServiceDeps {
	channelRepository: IChannelRepositoryAggregate;
	storageService: IStorageService;
	gatewayService: IGatewayService;
	snowflakeService: SnowflakeService;
	rateLimitService: IRateLimitService;
	favoriteMemeRepository: IFavoriteMemeRepository;
	validationService: MessageValidationService;
	mentionService: MessageMentionService;
	searchService: MessageSearchService;
	persistenceService: MessagePersistenceService;
	channelAuthService: MessageChannelAuthService;
	processingService: MessageProcessingService;
	dispatchService: MessageDispatchService;
	operationsHelpers: MessageOperationsHelpers;
	embedAttachmentResolver: MessageEmbedAttachmentResolver;
	limitConfigService: LimitConfigService;
}

export class MessageSendService {
	constructor(private readonly deps: MessageSendServiceDeps) {}

	private attachmentsToProcess(attachments?: Array<AttachmentRequestData>): Array<AttachmentToProcess> | undefined {
		if (!attachments) return undefined;
		const processed = attachments.filter(
			(att): att is AttachmentToProcess =>
				'upload_filename' in att && typeof att.upload_filename === 'string' && att.upload_filename.length > 0,
		);
		return processed.length > 0 ? processed : undefined;
	}

	private async checkMessageSendPermissions({
		guild,
		member,
		channel,
		data,
		user,
		checkPermission,
		hasPermission,
		channelId,
	}: {
		guild: GuildResponse | null;
		member: GuildMemberResponse | null;
		channel: Channel;
		data: MessageRequest;
		user: User;
		checkPermission: (permission: bigint) => Promise<void>;
		hasPermission: (permission: bigint) => Promise<boolean>;
		channelId: ChannelID;
	}): Promise<{canEmbedLinks: boolean; canMentionEveryone: boolean}> {
		const [canEmbedLinks, canMentionEveryone, canAttachFiles] = await Promise.all([
			hasPermission(Permissions.EMBED_LINKS),
			hasPermission(Permissions.MENTION_EVERYONE),
			hasPermission(Permissions.ATTACH_FILES),
		]);

		const hasFavoriteMeme = data.favorite_meme_id != null;

		if (data.embeds && data.embeds.length > 0 && !canEmbedLinks) {
			throw new MissingPermissionsError();
		}

		if (hasFavoriteMeme && (!canEmbedLinks || !canAttachFiles)) {
			throw new MissingPermissionsError();
		}

		if (guild) {
			if (!member) {
				throw new UnknownChannelError();
			}

			if (isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
				throw new FeatureTemporarilyDisabledError();
			}

			await checkPermission(Permissions.SEND_MESSAGES);

			if (data.tts) {
				const hasTtsPermission = await hasPermission(Permissions.SEND_TTS_MESSAGES);
				if (!hasTtsPermission) {
					data.tts = false;
				}
			}

			await this.deps.channelAuthService.checkGuildVerification({user, guild, member});
		} else if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
			await this.deps.channelAuthService.validateDMSendPermissions({channelId, userId: user.id});
		}

		return {canEmbedLinks, canMentionEveryone};
	}

	async validateMessageCanBeSent({
		user,
		channelId,
		data,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
	}): Promise<void> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			channelId,
		});

		if (user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
		}

		if (isPersonalNotesChannel({userId: user.id, channelId})) {
			await this.validatePersonalNoteMessage({user, channelId, data});
			return;
		}

		const {channel, guild, checkPermission, hasPermission, member} = authChannel;

		const {canMentionEveryone} = await this.checkMessageSendPermissions({
			guild,
			member,
			channel,
			data,
			user,
			checkPermission,
			hasPermission,
			channelId,
		});

		this.deps.validationService.ensureTextChannel(channel);

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data, guildFeatures: guild?.features ?? null});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const {referencedMessage, referencedChannelGuildId} = await this.fetchReferencedMessageForValidation({
			data,
			channelId,
			isForwardMessage,
			user,
		});

		if (data.message_reference && referencedMessage && !isForwardMessage) {
			const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
			if (!replyableTypes.has(referencedMessage.type)) {
				throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
			}
		}

		this.ensureForwardGuildMatches({data, referencedChannelGuildId});

		if (data.message_reference && guild && !isForwardMessage) {
			const hasReadHistory = await hasPermission(Permissions.READ_MESSAGE_HISTORY);
			if (!hasReadHistory) {
				this.assertReferencedMessageWithinCutoff({
					referencedMessage,
					guild,
				});
			}
		} else if (data.message_reference && guild && isForwardMessage) {
			await checkPermission(Permissions.READ_MESSAGE_HISTORY);
		}

		if (channel && !isForwardMessage && (data.content !== undefined || data.message_reference != null)) {
			const mentionContent = data.content ?? '';
			const mentions = this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage: referencedMessage || null,
				message: {
					id: createMessageID(await this.deps.snowflakeService.generate()),
					channelId,
					authorId: user.id,
					content: mentionContent,
					flags: this.deps.validationService.calculateMessageFlags(data),
				} as Message,
				channelType: channel.type,
				allowedMentions: data.allowed_mentions || null,
				guild,
				canMentionEveryone,
			});

			await this.deps.mentionService.validateMentions({
				userMentions: mentions.userMentions,
				roleMentions: mentions.roleMentions,
				channel,
				canMentionRoles: canMentionEveryone,
			});
		}

		await this.ensureAttachmentsExist(data.attachments);
	}

	private async validatePersonalNoteMessage({
		user,
		channelId,
		data,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
	}): Promise<void> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			channelId,
		});
		const {channel} = authChannel;

		this.deps.validationService.ensureTextChannel(channel);

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data, guildFeatures: null});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const {referencedMessage, referencedChannelGuildId} = await this.fetchReferencedMessageForValidation({
			data,
			channelId,
			isForwardMessage,
			user,
		});

		if (data.message_reference && referencedMessage && !isForwardMessage) {
			const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
			if (!replyableTypes.has(referencedMessage.type)) {
				throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
			}
		}

		this.ensureForwardGuildMatches({data, referencedChannelGuildId});

		if (channel && !isForwardMessage && (data.content !== undefined || data.message_reference != null)) {
			const mentionContent = data.content ?? '';
			const mentions = this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage: referencedMessage || null,
				message: {
					id: createMessageID(await this.deps.snowflakeService.generate()),
					channelId,
					authorId: user.id,
					content: mentionContent,
					flags: this.deps.validationService.calculateMessageFlags(data),
				} as Message,
				channelType: channel.type,
				allowedMentions: data.allowed_mentions || null,
				guild: null,
				canMentionEveryone: true,
			});

			await this.deps.mentionService.validateMentions({
				userMentions: mentions.userMentions,
				roleMentions: mentions.roleMentions,
				channel,
			});
		}

		await this.ensureAttachmentsExist(data.attachments);
	}

	private async fetchReferencedMessageForValidation({
		data,
		channelId,
		isForwardMessage,
		user,
	}: {
		data: MessageRequest;
		channelId: ChannelID;
		isForwardMessage: boolean;
		user: User;
	}): Promise<{referencedMessage: Message | null; referencedChannelGuildId?: GuildID | null}> {
		if (!data.message_reference) {
			return {referencedMessage: null};
		}

		let referenceChannelId = channelId;
		let forwardReferenceAuthChannel: AuthenticatedChannel | null = null;
		let referencedChannelGuildId: GuildID | null | undefined;
		if (isForwardMessage) {
			forwardReferenceAuthChannel = await this.deps.channelAuthService.getChannelAuthenticated({
				userId: user.id,
				channelId: createChannelID(data.message_reference.channel_id!),
			});
			await this.ensureForwardSourceAccess(forwardReferenceAuthChannel);
			referenceChannelId = forwardReferenceAuthChannel.channel.id;
			referencedChannelGuildId = forwardReferenceAuthChannel.channel.guildId ?? null;
		}

		const referencedMessage = await this.deps.channelRepository.messages.getMessage(
			referenceChannelId,
			createMessageID(data.message_reference.message_id),
		);

		if (!referencedMessage) {
			throw new UnknownMessageError();
		}

		if (forwardReferenceAuthChannel) {
			await this.ensureReferencedMessageIsVisible({
				referencedMessage,
				authChannel: forwardReferenceAuthChannel,
			});
		}

		return {referencedMessage, referencedChannelGuildId};
	}

	private async ensureAttachmentsExist(attachments?: Array<AttachmentRequestData>): Promise<void> {
		if (!attachments || attachments.length === 0) return;

		for (let index = 0; index < attachments.length; index++) {
			const attachment = attachments[index];
			if (!('upload_filename' in attachment) || !attachment.upload_filename) continue;
			const metadata = await this.deps.storageService.getObjectMetadata(
				Config.s3.buckets.uploads,
				attachment.upload_filename,
			);
			if (!metadata) {
				throw InputValidationError.fromCode(
					`attachments.${index}.upload_filename`,
					ValidationErrorCodes.UPLOADED_ATTACHMENT_NOT_FOUND,
					{filename: attachment.filename},
				);
			}
		}
	}

	private ensureMessageRequestIsValid({
		user,
		data,
		guildFeatures,
	}: {
		user: User;
		data: MessageRequest;
		guildFeatures: Iterable<string> | null;
	}): boolean {
		const isForwardMessage = data.message_reference?.type === MessageReferenceTypes.FORWARD;

		if (isForwardMessage) {
			if (!data.message_reference?.channel_id || !data.message_reference?.message_id) {
				throw InputValidationError.fromCode(
					'message_reference',
					ValidationErrorCodes.FORWARD_REFERENCE_REQUIRES_CHANNEL_AND_MESSAGE,
				);
			}

			if (
				data.content ||
				(data.embeds && data.embeds.length > 0) ||
				(data.attachments && data.attachments.length > 0) ||
				(data.sticker_ids && data.sticker_ids.length > 0)
			) {
				throw InputValidationError.fromCode(
					'message_reference',
					ValidationErrorCodes.FORWARD_MESSAGES_CANNOT_CONTAIN_CONTENT,
				);
			}
		} else {
			this.deps.validationService.validateMessageContent(data, user, {guildFeatures});
		}

		return isForwardMessage;
	}

	private async resolveReferenceContext({
		data,
		channelId,
		isForwardMessage,
		user,
	}: {
		data: MessageRequest;
		channelId: ChannelID;
		isForwardMessage: boolean;
		user: User;
	}): Promise<{
		referencedMessage: Message | null;
		referencedChannelGuildId?: GuildID | null;
		messageSnapshots?: Array<MessageSnapshot>;
	}> {
		let referenceChannelId = channelId;
		let forwardReferenceAuthChannel: AuthenticatedChannel | null = null;
		let referencedChannelGuildId: GuildID | null | undefined;
		if (isForwardMessage) {
			forwardReferenceAuthChannel = await this.deps.channelAuthService.getChannelAuthenticated({
				userId: user.id,
				channelId: createChannelID(data.message_reference!.channel_id!),
			});
			await this.ensureForwardSourceAccess(forwardReferenceAuthChannel);
			referenceChannelId = forwardReferenceAuthChannel.channel.id;
			referencedChannelGuildId = forwardReferenceAuthChannel.channel.guildId ?? null;
		}

		const referencedMessage = data.message_reference
			? await this.deps.channelRepository.messages.getMessage(
					referenceChannelId,
					createMessageID(data.message_reference.message_id),
				)
			: null;

		if (data.message_reference && !referencedMessage) {
			throw new UnknownMessageError();
		}

		if (forwardReferenceAuthChannel && referencedMessage) {
			await this.ensureReferencedMessageIsVisible({
				referencedMessage,
				authChannel: forwardReferenceAuthChannel,
			});
		}

		let messageSnapshots: Array<MessageSnapshot> | undefined;
		if (isForwardMessage && referencedMessage) {
			messageSnapshots = await createMessageSnapshotsForForward(
				referencedMessage,
				user,
				channelId,
				this.deps.storageService,
				this.deps.snowflakeService,
				this.deps.limitConfigService,
			);
		}

		return {referencedMessage, referencedChannelGuildId, messageSnapshots};
	}

	private async ensureForwardSourceAccess(authChannel: AuthenticatedChannel): Promise<void> {
		if (authChannel.guild) {
			await authChannel.checkPermission(Permissions.VIEW_CHANNEL);
			await authChannel.checkPermission(Permissions.READ_MESSAGE_HISTORY);
		}
	}

	private async ensureReferencedMessageIsVisible({
		referencedMessage,
		authChannel,
	}: {
		referencedMessage: Message;
		authChannel: AuthenticatedChannel;
	}): Promise<void> {
		if (!authChannel.guild) {
			return;
		}

		const hasReadHistory = await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY);
		if (hasReadHistory) {
			return;
		}

		this.assertReferencedMessageWithinCutoff({
			referencedMessage,
			guild: authChannel.guild,
		});
	}

	private ensureForwardGuildMatches({
		data,
		referencedChannelGuildId,
	}: {
		data: MessageRequest;
		referencedChannelGuildId?: GuildID | null;
	}): void {
		if (data.message_reference?.type !== MessageReferenceTypes.FORWARD) {
			return;
		}

		if (referencedChannelGuildId === undefined || data.message_reference?.guild_id === undefined) {
			return;
		}

		const providedGuildId = createGuildID(data.message_reference.guild_id);
		if (providedGuildId !== referencedChannelGuildId) {
			throw InputValidationError.fromCode(
				'message_reference.guild_id',
				ValidationErrorCodes.GUILD_ID_MUST_MATCH_REFERENCED_MESSAGE,
			);
		}
	}

	private async prepareMessageAttachments({
		user,
		channelId,
		data,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
	}): Promise<{
		attachmentsToProcess?: Array<AttachmentToProcess>;
		favoriteMemeAttachment?: MessageAttachment;
	}> {
		const attachmentsToProcess = this.attachmentsToProcess(data.attachments);
		let favoriteMemeAttachment: MessageAttachment | undefined;
		if (data.favorite_meme_id) {
			favoriteMemeAttachment = await this.deps.operationsHelpers.processFavoriteMeme({
				user,
				channelId,
				favoriteMemeId: data.favorite_meme_id,
			});
		}

		return {attachmentsToProcess, favoriteMemeAttachment};
	}

	private assertReferencedMessageWithinCutoff({
		referencedMessage,
		guild,
	}: {
		referencedMessage: Message | null;
		guild: GuildResponse;
	}): void {
		if (!referencedMessage) {
			throw new UnknownMessageError();
		}

		const cutoff = guild.message_history_cutoff;
		if (!cutoff) {
			throw new UnknownMessageError();
		}

		const messageTimestamp = snowflakeToDate(referencedMessage.id).getTime();
		const cutoffTimestamp = new Date(cutoff).getTime();
		if (messageTimestamp < cutoffTimestamp) {
			throw new UnknownMessageError();
		}
	}

	private getMessageTypeForRequest(data: MessageRequest): number {
		if (!data.message_reference) {
			return MessageTypes.DEFAULT;
		}
		const referenceType = data.message_reference.type ?? MessageReferenceTypes.DEFAULT;
		return referenceType === MessageReferenceTypes.FORWARD ? MessageTypes.DEFAULT : MessageTypes.REPLY;
	}

	private buildMessageReferencePayload({
		data,
		referencedMessage,
		guild,
		isForwardMessage,
		referencedChannelGuildId,
	}: {
		data: MessageRequest;
		referencedMessage: Message | null;
		guild: GuildResponse | null;
		isForwardMessage: boolean;
		referencedChannelGuildId?: GuildID | null;
	}): MessageReference | undefined {
		if (!data.message_reference) {
			return undefined;
		}

		const channelId = referencedMessage
			? referencedMessage.channelId
			: createChannelID(data.message_reference.channel_id!);
		const guildId = isForwardMessage
			? (referencedChannelGuildId ?? null)
			: guild?.id
				? createGuildID(BigInt(guild.id))
				: null;

		return {
			message_id: createMessageID(data.message_reference.message_id),
			channel_id: channelId,
			guild_id: guildId,
			type: data.message_reference.type ?? MessageReferenceTypes.DEFAULT,
		};
	}

	async sendMessage({
		user,
		channelId,
		data,
		requestCache,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		return await withBusinessSpan(
			'fluxer.message.send',
			'fluxer.messages.sent',
			{channel_id: channelId.toString()},
			() => this.performSendMessage({user, channelId, data, requestCache}),
		);
	}

	private async performSendMessage({
		user,
		channelId,
		data,
		requestCache,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const startTime = Date.now();
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			channelId,
		});

		if (user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
		}

		if (isPersonalNotesChannel({userId: user.id, channelId})) {
			return this.sendPersonalNoteMessage({user, channelId, data, requestCache});
		}

		const {channel, guild, checkPermission, hasPermission, member} = authChannel;

		const {canEmbedLinks, canMentionEveryone} = await this.checkMessageSendPermissions({
			guild,
			member,
			channel,
			data,
			user,
			checkPermission,
			hasPermission,
			channelId,
		});

		if (guild) {
			if (channel.rateLimitPerUser && channel.rateLimitPerUser > 0 && !user.isBot) {
				const hasBypassSlowmode = await hasPermission(Permissions.BYPASS_SLOWMODE);

				if (!hasBypassSlowmode) {
					const rateLimitKey = `slowmode:${channelId}:${user.id}`;
					const result = await this.deps.rateLimitService.checkLimit({
						identifier: rateLimitKey,
						maxAttempts: 1,
						windowMs: channel.rateLimitPerUser * 1000,
					});

					if (!result.allowed) {
						throw new SlowmodeRateLimitError({
							retryAfter: result.retryAfter!,
						});
					}
				}
			}
		}

		this.deps.validationService.ensureTextChannel(channel);

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data, guildFeatures: guild?.features ?? null});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const existingMessage = await this.deps.operationsHelpers.findExistingMessage({
			userId: user.id,
			nonce: data.nonce,
			expectedChannelId: channelId,
		});
		if (existingMessage) {
			return existingMessage;
		}

		const referenceContext = await this.resolveReferenceContext({
			data,
			channelId,
			isForwardMessage,
			user,
		});

		const {referencedMessage, referencedChannelGuildId, messageSnapshots} = referenceContext;

		if (data.message_reference && guild && !isForwardMessage) {
			const hasReadHistory = await hasPermission(Permissions.READ_MESSAGE_HISTORY);
			if (!hasReadHistory) {
				this.assertReferencedMessageWithinCutoff({
					referencedMessage,
					guild,
				});
			}
		} else if (data.message_reference && guild && isForwardMessage) {
			await checkPermission(Permissions.READ_MESSAGE_HISTORY);
		}

		if (data.message_reference && referencedMessage && !isForwardMessage) {
			const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
			if (!replyableTypes.has(referencedMessage.type)) {
				throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
			}
		}

		this.ensureForwardGuildMatches({data, referencedChannelGuildId});

		const {attachmentsToProcess, favoriteMemeAttachment} = await this.prepareMessageAttachments({
			user,
			channelId,
			data,
		});

		const messageId = createMessageID(await this.deps.snowflakeService.generate());

		let mentionData:
			| {
					flags: number;
					mentionUserIds: Array<UserID>;
					mentionRoleIds: Array<RoleID>;
					mentionEveryone: boolean;
					mentionHere: boolean;
			  }
			| undefined;
		if (channel && !isForwardMessage && (data.content !== undefined || data.message_reference != null)) {
			const mentionContent = data.content ?? '';
			const mentions = this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage: referencedMessage || null,
				message: {
					id: messageId,
					channelId,
					authorId: user.id,
					content: mentionContent,
					flags: this.deps.validationService.calculateMessageFlags(data),
				} as Message,
				channelType: channel.type,
				allowedMentions: data.allowed_mentions || null,
				guild,
				canMentionEveryone,
			});

			const {validUserIds, validRoleIds} = await this.deps.mentionService.validateMentions({
				userMentions: mentions.userMentions,
				roleMentions: mentions.roleMentions,
				channel,
				canMentionRoles: canMentionEveryone,
			});

			mentionData = {
				flags: mentions.flags,
				mentionUserIds: validUserIds,
				mentionRoleIds: validRoleIds,
				mentionEveryone: mentions.mentionsEveryone || mentions.mentionsHere,
				mentionHere: mentions.mentionsHere,
			};
		}

		const messageReference = this.buildMessageReferencePayload({
			data,
			referencedMessage,
			guild,
			isForwardMessage,
			referencedChannelGuildId,
		});

		const message = await this.deps.persistenceService.createMessage({
			messageId,
			channelId,
			user,
			type: this.getMessageTypeForRequest(data),
			content: data.content,
			flags: this.deps.validationService.calculateMessageFlags(data),
			embeds: data.embeds,
			attachments: attachmentsToProcess,
			processedAttachments: favoriteMemeAttachment ? [favoriteMemeAttachment] : undefined,
			attachmentDecayExcludedIds: favoriteMemeAttachment ? [favoriteMemeAttachment.attachment_id] : undefined,
			stickerIds: data.sticker_ids ? data.sticker_ids.flatMap((stickerId) => createStickerID(stickerId)) : undefined,
			messageReference,
			messageSnapshots,
			guildId: guild?.id ? createGuildID(BigInt(guild.id)) : null,
			channel,
			referencedMessage,
			allowedMentions: data.allowed_mentions,
			guild,
			member,
			hasPermission: guild ? hasPermission : undefined,
			mentionData,
			allowEmbeds: canEmbedLinks,
		});

		const messageType = this.getMessageTypeForRequest(data);
		const hasAttachments = (attachmentsToProcess?.length ?? 0) > 0 || favoriteMemeAttachment !== undefined;
		const hasEmbeds = (data.embeds?.length ?? 0) > 0;
		const durationMs = Date.now() - startTime;

		recordMessageSent({
			channelType: channel.type.toString(),
			hasAttachments: hasAttachments.toString(),
			hasEmbeds: hasEmbeds.toString(),
			messageType: messageType.toString(),
		});

		recordMessageSendDuration({
			channelType: channel.type.toString(),
			durationMs,
		});

		await Promise.all([
			this.deps.processingService.updateDMRecipients({channel, channelId, requestCache}),
			this.deps.processingService.processMessageAfterCreation({
				message,
				channel,
				guild,
				user,
				data,
				referencedMessage,
				mentionHere: mentionData?.mentionHere ?? false,
			}),
			this.deps.processingService.updateReadStates({user, guild, channel, channelId, messageId}),
		]);

		await this.deps.dispatchService.dispatchMessageCreate({
			channel,
			message,
			requestCache,
			currentUserId: user.id,
			nonce: data.nonce,
			tts: data.tts,
		});
		if (data.nonce) {
			await this.deps.validationService.cacheMessageNonce({userId: user.id, nonce: data.nonce, channelId, messageId});
		}
		if (channel.indexedAt) {
			void this.deps.searchService.indexMessage(message, user.isBot);
		}

		return message;
	}

	async sendWebhookMessage({
		webhook,
		data,
		username,
		avatar,
		requestCache,
	}: {
		webhook: Webhook;
		data: MessageRequest;
		username?: string | null;
		avatar?: string | null;
		requestCache: RequestCache;
	}): Promise<Message> {
		const startTime = Date.now();
		const channelId = webhook.channelId!;
		const channel = await this.deps.channelRepository.channelData.findUnique(channelId);

		if (!channel || !channel.guildId) {
			throw new CannotExecuteOnDmError();
		}

		const guild = await this.deps.gatewayService.getGuildData({
			guildId: channel.guildId,
			userId: createUserID(0n),
			skipMembershipCheck: true,
		});

		this.deps.validationService.validateMessageContent(data, null, {guildFeatures: guild?.features ?? null});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const messageId = createMessageID(await this.deps.snowflakeService.generate());

		let mentionData:
			| {
					flags: number;
					mentionUserIds: Array<UserID>;
					mentionRoleIds: Array<RoleID>;
					mentionEveryone: boolean;
					mentionHere: boolean;
			  }
			| undefined;
		if (data.content && channel) {
			const mentions = this.deps.mentionService.extractMentions({
				content: data.content,
				referencedMessage: null,
				message: {
					id: messageId,
					channelId,
					webhookId: webhook.id,
					content: data.content,
					flags: this.deps.validationService.calculateMessageFlags(data),
				} as Message,
				channelType: channel.type,
				allowedMentions: data.allowed_mentions || null,
				guild,
			});

			const {validUserIds, validRoleIds} = await this.deps.mentionService.validateMentions({
				userMentions: mentions.userMentions,
				roleMentions: mentions.roleMentions,
				channel,
			});

			mentionData = {
				flags: mentions.flags,
				mentionUserIds: validUserIds,
				mentionRoleIds: validRoleIds,
				mentionEveryone: mentions.mentionsEveryone || mentions.mentionsHere,
				mentionHere: mentions.mentionsHere,
			};
		}

		const message = await this.deps.persistenceService.createMessage({
			messageId,
			channelId,
			webhookId: webhook.id,
			webhookName: username ?? webhook.name!,
			webhookAvatar: avatar ?? webhook.avatarHash,
			type: MessageTypes.DEFAULT,
			content: data.content,
			flags: this.deps.validationService.calculateMessageFlags(data),
			embeds: data.embeds,
			attachments: this.attachmentsToProcess(data.attachments),
			guildId: channel.guildId,
			channel,
			guild,
			mentionData,
			allowEmbeds: true,
		});

		const hasAttachments = (this.attachmentsToProcess(data.attachments)?.length ?? 0) > 0;
		const hasEmbeds = (data.embeds?.length ?? 0) > 0;
		const durationMs = Date.now() - startTime;

		recordMessageSent({
			channelType: channel.type.toString(),
			hasAttachments: hasAttachments.toString(),
			hasEmbeds: hasEmbeds.toString(),
			messageType: MessageTypes.DEFAULT.toString(),
		});

		recordMessageSendDuration({
			channelType: channel.type.toString(),
			durationMs,
		});

		await this.deps.mentionService.handleMentionTasks({
			guildId: channel.guildId,
			message,
			authorId: createUserID(0n),
			mentionHere: mentionData?.mentionHere ?? false,
		});

		await this.deps.dispatchService.dispatchMessageCreate({channel, message, requestCache});

		if (channel.indexedAt) {
			void this.deps.searchService.indexMessage(message, false);
		}

		return message;
	}

	private async sendPersonalNoteMessage({
		user,
		channelId,
		data,
		requestCache,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const {channel} = await this.deps.channelAuthService.getChannelAuthenticated({userId: user.id, channelId});

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data, guildFeatures: null});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const existingMessage = await this.deps.operationsHelpers.findExistingMessage({
			userId: user.id,
			nonce: data.nonce,
			expectedChannelId: channelId,
		});

		if (existingMessage) {
			return existingMessage;
		}

		const {referencedMessage, referencedChannelGuildId, messageSnapshots} = await this.resolveReferenceContext({
			data,
			channelId,
			isForwardMessage,
			user,
		});

		this.ensureForwardGuildMatches({data, referencedChannelGuildId});

		const {attachmentsToProcess, favoriteMemeAttachment} = await this.prepareMessageAttachments({
			user,
			channelId,
			data,
		});
		const messageId = createMessageID(await this.deps.snowflakeService.generate());
		const messageReference = this.buildMessageReferencePayload({
			data,
			referencedMessage,
			guild: null,
			isForwardMessage,
			referencedChannelGuildId,
		});

		const message = await this.deps.persistenceService.createMessage({
			messageId,
			channelId,
			user,
			type: this.getMessageTypeForRequest(data),
			content: data.content,
			flags: data.flags ? data.flags & SENDABLE_MESSAGE_FLAGS : 0,
			embeds: data.embeds,
			attachments: attachmentsToProcess,
			processedAttachments: favoriteMemeAttachment ? [favoriteMemeAttachment] : undefined,
			attachmentDecayExcludedIds: favoriteMemeAttachment ? [favoriteMemeAttachment.attachment_id] : undefined,
			messageReference,
			messageSnapshots,
			guildId: null,
			channel,
		});

		await this.deps.dispatchService.dispatchMessageCreate({
			channel,
			message,
			requestCache,
			currentUserId: user.id,
			nonce: data.nonce,
			tts: data.tts,
		});

		if (data.nonce) {
			await this.deps.validationService.cacheMessageNonce({userId: user.id, nonce: data.nonce, channelId, messageId});
		}

		if (channel.indexedAt) {
			void this.deps.searchService.indexMessage(message, user.isBot);
		}

		return message;
	}
}
