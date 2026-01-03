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

import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createMessageID,
	createStickerID,
	createUserID,
	type GuildID,
	type RoleID,
	type UserID,
} from '~/BrandedTypes';
import {Config} from '~/Config';
import {
	ChannelTypes,
	GuildOperations,
	MessageReferenceTypes,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
	UserFlags,
} from '~/Constants';
import type {AttachmentRequestData, AttachmentToProcess} from '~/channel/AttachmentDTOs';
import type {MessageRequest} from '~/channel/ChannelModel';
import type {MessageAttachment, MessageReference, MessageSnapshot} from '~/database/CassandraTypes';
import {
	CannotExecuteOnDmError,
	FeatureTemporarilyDisabledError,
	InputValidationError,
	MissingPermissionsError,
	SlowmodeRateLimitError,
	UnknownChannelError,
	UnknownMessageError,
} from '~/Errors';
import type {IFavoriteMemeRepository} from '~/favorite_meme/IFavoriteMemeRepository';
import type {GuildResponse} from '~/guild/GuildModel';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {Message, User, Webhook} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {MessageDispatchService} from './MessageDispatchService';
import type {MessageEmbedAttachmentResolver} from './MessageEmbedAttachmentResolver';
import {createMessageSnapshotsForForward, isOperationDisabled, isPersonalNotesChannel} from './MessageHelpers';
import type {MessageMentionService} from './MessageMentionService';
import type {MessageOperationsHelpers} from './MessageOperationsHelpers';
import type {MessagePersistenceService} from './MessagePersistenceService';
import type {MessageProcessingService} from './MessageProcessingService';
import type {MessageSearchService} from './MessageSearchService';
import type {MessageValidationService} from './MessageValidationService';

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

		if (!(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.create('content', 'You must start a session before sending messages');
		}

		if (isPersonalNotesChannel({userId: user.id, channelId})) {
			await this.validatePersonalNoteMessage({user, channelId, data});
			return;
		}

		const {channel, guild, checkPermission, hasPermission, member} = authChannel;

		const [canEmbedLinks, canMentionEveryone] = await Promise.all([
			hasPermission(Permissions.EMBED_LINKS),
			hasPermission(Permissions.MENTION_EVERYONE),
		]);

		if (data.embeds && data.embeds.length > 0 && !canEmbedLinks) {
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
				await checkPermission(Permissions.SEND_TTS_MESSAGES);
			}

			await this.deps.channelAuthService.checkGuildVerification({user, guild, member});
		} else if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
			await this.deps.channelAuthService.validateDMSendPermissions({channelId, userId: user.id});
		}

		this.deps.validationService.ensureTextChannel(channel);

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const {referencedMessage, referencedChannelGuildId} = await this.fetchReferencedMessageForValidation({
			data,
			channelId,
			isForwardMessage,
		});

		if (data.message_reference && referencedMessage && !isForwardMessage) {
			const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
			if (!replyableTypes.has(referencedMessage.type)) {
				throw InputValidationError.create('message_reference', 'Cannot reply to system message');
			}
		}

		this.ensureForwardGuildMatches({data, referencedChannelGuildId});

		if (data.message_reference && guild) {
			await checkPermission(Permissions.READ_MESSAGE_HISTORY);
		}

		if (data.content && channel && !isForwardMessage) {
			const mentions = this.deps.mentionService.extractMentions({
				content: data.content,
				referencedMessage: referencedMessage || null,
				message: {
					id: createMessageID(this.deps.snowflakeService.generate()),
					channelId,
					authorId: user.id,
					content: data.content,
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

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data});

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const {referencedMessage, referencedChannelGuildId} = await this.fetchReferencedMessageForValidation({
			data,
			channelId,
			isForwardMessage,
		});

		if (data.message_reference && referencedMessage && !isForwardMessage) {
			const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
			if (!replyableTypes.has(referencedMessage.type)) {
				throw InputValidationError.create('message_reference', 'Cannot reply to system message');
			}
		}

		this.ensureForwardGuildMatches({data, referencedChannelGuildId});

		if (data.content && channel && !isForwardMessage) {
			const mentions = this.deps.mentionService.extractMentions({
				content: data.content,
				referencedMessage: referencedMessage || null,
				message: {
					id: createMessageID(this.deps.snowflakeService.generate()),
					channelId,
					authorId: user.id,
					content: data.content,
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
	}: {
		data: MessageRequest;
		channelId: ChannelID;
		isForwardMessage: boolean;
	}): Promise<{referencedMessage: Message | null; referencedChannelGuildId?: GuildID | null}> {
		if (!data.message_reference) {
			return {referencedMessage: null};
		}

		const referenceChannelId = isForwardMessage ? createChannelID(data.message_reference.channel_id!) : channelId;

		const referencedMessage = await this.deps.channelRepository.messages.getMessage(
			referenceChannelId,
			createMessageID(data.message_reference.message_id),
		);

		if (!referencedMessage) {
			throw new UnknownMessageError();
		}

		let referencedChannelGuildId: GuildID | null | undefined;
		if (isForwardMessage) {
			const referencedChannel = await this.deps.channelRepository.channelData.findUnique(referencedMessage.channelId);
			if (!referencedChannel) {
				throw new UnknownChannelError();
			}
			referencedChannelGuildId = referencedChannel.guildId ?? null;
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
				throw InputValidationError.create(
					`attachments.${index}.upload_filename`,
					`Uploaded attachment ${attachment.filename} was not found`,
				);
			}
		}
	}

	private ensureMessageRequestIsValid({user, data}: {user: User; data: MessageRequest}): boolean {
		const isForwardMessage = data.message_reference?.type === MessageReferenceTypes.FORWARD;

		if (isForwardMessage) {
			if (!data.message_reference?.channel_id || !data.message_reference?.message_id) {
				throw InputValidationError.create(
					'message_reference',
					'Forward message reference must include channel_id and message_id',
				);
			}

			if (
				data.content ||
				(data.embeds && data.embeds.length > 0) ||
				(data.attachments && data.attachments.length > 0) ||
				(data.sticker_ids && data.sticker_ids.length > 0)
			) {
				throw InputValidationError.create(
					'message_reference',
					'Forward messages cannot contain content, embeds, attachments, or stickers',
				);
			}
		} else {
			this.deps.validationService.validateMessageContent(data, user);
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
		const referenceChannelId = isForwardMessage ? createChannelID(data.message_reference!.channel_id!) : channelId;

		const referencedMessage = data.message_reference
			? await this.deps.channelRepository.messages.getMessage(
					referenceChannelId,
					createMessageID(data.message_reference.message_id),
				)
			: null;

		if (data.message_reference && !referencedMessage) {
			throw new UnknownMessageError();
		}

		let referencedChannelGuildId: GuildID | null | undefined;
		if (isForwardMessage && referencedMessage) {
			const referencedChannel = await this.deps.channelRepository.channelData.findUnique(referencedMessage.channelId);
			if (!referencedChannel) {
				throw new UnknownChannelError();
			}
			referencedChannelGuildId = referencedChannel.guildId;
		}

		let messageSnapshots: Array<MessageSnapshot> | undefined;
		if (isForwardMessage && referencedMessage) {
			messageSnapshots = await createMessageSnapshotsForForward(
				referencedMessage,
				user,
				channelId,
				this.deps.storageService,
				this.deps.snowflakeService,
				this.deps.validationService.validateAttachmentSizes.bind(this.deps.validationService),
			);
		}

		return {referencedMessage, referencedChannelGuildId, messageSnapshots};
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
			throw InputValidationError.create(
				'message_reference.guild_id',
				'Guild id must match the channel the referenced message was fetched from',
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

		const channel_id = referencedMessage
			? referencedMessage.channelId
			: createChannelID(data.message_reference.channel_id!);
		const guild_id = isForwardMessage
			? (referencedChannelGuildId ?? null)
			: guild?.id
				? createGuildID(BigInt(guild.id))
				: null;

		return {
			message_id: createMessageID(data.message_reference.message_id),
			channel_id,
			guild_id,
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
		try {
			const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
				userId: user.id,
				channelId,
			});

			if (!(user.flags & UserFlags.HAS_SESSION_STARTED)) {
				throw InputValidationError.create('content', 'You must start a session before sending messages');
			}

			if (isPersonalNotesChannel({userId: user.id, channelId})) {
				return this.sendPersonalNoteMessage({user, channelId, data, requestCache});
			}

			const {channel, guild, checkPermission, hasPermission, member} = authChannel;

			const [canEmbedLinks, canMentionEveryone] = await Promise.all([
				hasPermission(Permissions.EMBED_LINKS),
				hasPermission(Permissions.MENTION_EVERYONE),
			]);

			if (data.embeds && data.embeds.length > 0 && !canEmbedLinks) {
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
					await checkPermission(Permissions.SEND_TTS_MESSAGES);
				}

				await this.deps.channelAuthService.checkGuildVerification({user, guild, member});

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
								message: 'You are sending messages too quickly. Please slow down.',
								retryAfter: result.retryAfter!,
							});
						}
					}
				}
			} else if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
				await this.deps.channelAuthService.validateDMSendPermissions({channelId, userId: user.id});
			}

			this.deps.validationService.ensureTextChannel(channel);

			const isForwardMessage = this.ensureMessageRequestIsValid({user, data});

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

			if (data.message_reference && guild) {
				await checkPermission(Permissions.READ_MESSAGE_HISTORY);
			}

			const referenceContext = await this.resolveReferenceContext({
				data,
				channelId,
				isForwardMessage,
				user,
			});

			const {referencedMessage, referencedChannelGuildId, messageSnapshots} = referenceContext;

			if (data.message_reference && referencedMessage && !isForwardMessage) {
				const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
				if (!replyableTypes.has(referencedMessage.type)) {
					throw InputValidationError.create('message_reference', 'Cannot reply to system message');
				}
			}

			this.ensureForwardGuildMatches({data, referencedChannelGuildId});

			const {attachmentsToProcess, favoriteMemeAttachment} = await this.prepareMessageAttachments({
				user,
				channelId,
				data,
			});

			const messageId = createMessageID(this.deps.snowflakeService.generate());

			let mentionData:
				| {
						flags: number;
						mentionUserIds: Array<UserID>;
						mentionRoleIds: Array<RoleID>;
						mentionEveryone: boolean;
						mentionHere: boolean;
				  }
				| undefined;
			if (data.content && channel && !isForwardMessage) {
				const mentions = this.deps.mentionService.extractMentions({
					content: data.content,
					referencedMessage: referencedMessage || null,
					message: {
						id: messageId,
						channelId,
						authorId: user.id,
						content: data.content,
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

			getMetricsService().counter({name: 'message.send'});

			return message;
		} catch (error) {
			getMetricsService().counter({name: 'message.send.error'});
			throw error;
		}
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

		this.deps.validationService.validateMessageContent(data, null);

		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});

		const messageId = createMessageID(this.deps.snowflakeService.generate());

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

		const isForwardMessage = this.ensureMessageRequestIsValid({user, data});

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
		const messageId = createMessageID(this.deps.snowflakeService.generate());
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
			type: MessageTypes.DEFAULT,
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

		return message;
	}
}
