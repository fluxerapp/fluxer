// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ChannelTypes,
	MessageFlags,
	MessageReferenceTypes,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {RelationshipTypes, SensitiveMediaFilterLevel, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {CannotExecuteOnDmError} from '@fluxer/errors/src/domains/core/CannotExecuteOnDmError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {SlowmodeRateLimitError} from '@fluxer/errors/src/domains/core/SlowmodeRateLimitError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {AttachmentID, ChannelID, GuildID, MessageID, RoleID, UserID} from '../../../BrandedTypes';
import {
	createAttachmentID,
	createChannelID,
	createGuildID,
	createMessageID,
	createStickerID,
	createUserID,
} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import {SYSTEM_USER_ID} from '../../../constants/Core';
import type {MessageAttachment, MessageReference} from '../../../database/types/MessageTypes';
import type {IFavoriteMemeRepository} from '../../../favorite_meme/IFavoriteMemeRepository';
import type {GatewayChannelMention, IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import {Logger} from '../../../Logger';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {MessageSnapshot} from '../../../models/MessageSnapshot';
import type {User} from '../../../models/User';
import type {Webhook} from '../../../models/Webhook';
import type {IUserRepository} from '../../../user/IUserRepository';
import type {DirectMessageSpamMitigationService} from '../../../user/services/DirectMessageSpamMitigationService';
import {assertGuildMemberCanCommunicate} from '../../../utils/GuildCommunicationUtils';
import type {AttachmentRequestData, AttachmentToProcess} from '../../AttachmentDTOs';
import type {MessageRequest, MessageUpdateRequest} from '../../MessageTypes';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AttachmentUploadTraceRepository} from '../../repositories/message/AttachmentUploadTraceRepository';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {DmNsfwContext} from './MessageContentService';
import type {MessageDispatchService} from './MessageDispatchService';
import type {MessageEmbedAttachmentResolver} from './MessageEmbedAttachmentResolver';
import {
	createMessageSnapshotsForForward,
	type ForwardMediaSelection,
	isOperationDisabled,
	isPersonalNotesChannel,
} from './MessageHelpers';
import type {MessageMentionService} from './MessageMentionService';
import type {MessageOperationsHelpers} from './MessageOperationsHelpers';
import type {MessagePersistenceService} from './MessagePersistenceService';
import type {MessageProcessingService} from './MessageProcessingService';
import type {MessageSearchService} from './MessageSearchService';
import type {MessageValidationService} from './MessageValidationService';

interface MessageSendServiceDeps {
	channelRepository: IChannelRepositoryAggregate;
	userRepository: IUserRepository;
	storageService: IStorageService;
	gatewayService: IGatewayService;
	snowflakeService: ISnowflakeService;
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
	attachmentUploadTraceRepository: AttachmentUploadTraceRepository;
	limitConfigService: LimitConfigService;
	directMessageSpamMitigationService: DirectMessageSpamMitigationService;
}

interface SendMessageResult {
	message: Message;
	authChannel: AuthenticatedChannel;
}

interface SendMentionData {
	flags: number;
	mentionUserIds: Array<UserID>;
	mentionRoleIds: Array<RoleID>;
	mentionChannelIds: Array<ChannelID>;
	mentionChannels: Array<GatewayChannelMention>;
	mentionEveryone: boolean;
	mentionHere: boolean;
}

export class MessageSendService {
	constructor(private readonly deps: MessageSendServiceDeps) {}

	private cacheMentionChannels(params: {
		requestCache: RequestCache;
		messageId: MessageID;
		mentionChannels?: Array<GatewayChannelMention>;
	}): void {
		if (params.mentionChannels && params.mentionChannels.length > 0) {
			params.requestCache.messageMentionChannels.set(params.messageId.toString(), params.mentionChannels);
		}
	}

	private async cacheMessageNonceIfPresent(params: {
		userId: UserID;
		nonce?: string;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		if (!params.nonce) {
			return;
		}
		try {
			await this.deps.validationService.cacheMessageNonce({
				userId: params.userId,
				nonce: params.nonce,
				channelId: params.channelId,
				messageId: params.messageId,
			});
		} catch (error) {
			Logger.warn(
				{
					userId: params.userId.toString(),
					channelId: params.channelId.toString(),
					messageId: params.messageId.toString(),
					error,
				},
				'Message was persisted but nonce cache failed',
			);
		}
	}

	private logPostCreateFailure(params: {messageId: MessageID; step: string; error: unknown}): void {
		Logger.warn(
			{messageId: params.messageId.toString(), step: params.step, error: params.error},
			'Message was persisted but post-create work failed',
		);
	}

	private async settlePostCreateWork(
		messageId: MessageID,
		work: Array<{step: string; promise: Promise<void>}>,
	): Promise<void> {
		const results = await Promise.allSettled(work.map((item) => item.promise));
		for (const [index, result] of results.entries()) {
			if (result.status === 'rejected') {
				this.logPostCreateFailure({messageId, step: work[index]!.step, error: result.reason});
			}
		}
	}

	private getSearchIndexOptions(channel: Channel) {
		const includeDefault = channel.indexedAt != null;
		return includeDefault ? {includeDefault} : null;
	}

	private async buildDmNsfwContext(channel: Channel, senderId: UserID): Promise<DmNsfwContext | undefined> {
		if (channel.type === ChannelTypes.GROUP_DM || channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return undefined;
		}
		if (channel.type !== ChannelTypes.DM) {
			return undefined;
		}
		const recipientIds = Array.from(channel.recipientIds).filter((id) => id !== senderId);
		if (recipientIds.length !== 1) {
			return undefined;
		}
		const recipientId = recipientIds[0];
		const [senderSettings, recipientSettings, friendship] = await Promise.all([
			this.deps.userRepository.findSettings(senderId),
			this.deps.userRepository.findSettings(recipientId),
			this.deps.userRepository.getRelationship(senderId, recipientId, RelationshipTypes.FRIEND),
		]);
		const areFriends = friendship != null;
		const senderFilterLevel = areFriends
			? (senderSettings?.sensitiveContentFriendDmFilter ?? SensitiveMediaFilterLevel.SHOW)
			: (senderSettings?.sensitiveContentNonFriendDmFilter ?? SensitiveMediaFilterLevel.BLOCK);
		const recipientFilterLevel = areFriends
			? (recipientSettings?.sensitiveContentFriendDmFilter ?? SensitiveMediaFilterLevel.SHOW)
			: (recipientSettings?.sensitiveContentNonFriendDmFilter ?? SensitiveMediaFilterLevel.BLOCK);
		return {senderFilterLevel, recipientFilterLevel};
	}

	private attachmentsToProcess(attachments?: Array<AttachmentRequestData>): Array<AttachmentToProcess> | undefined {
		if (!attachments) return undefined;
		const processed = attachments.filter(
			(att): att is AttachmentToProcess =>
				'upload_filename' in att && typeof att.upload_filename === 'string' && att.upload_filename.length > 0,
		);
		return processed.length > 0 ? processed : undefined;
	}

	private resolveWebhookAttachmentUploadUserId(
		webhook: Webhook,
		attachments?: Array<AttachmentRequestData>,
	): UserID | undefined {
		const uploadUserId = webhook.creatorId ?? undefined;
		if (uploadUserId === undefined && this.attachmentsToProcess(attachments) !== undefined) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.INVALID_MESSAGE_DATA);
		}
		return uploadUserId;
	}

	private getOneToOneDmRecipientId(channel: Channel, senderId: UserID): UserID | null {
		if (channel.guildId || channel.type !== ChannelTypes.DM) {
			return null;
		}
		const recipientIds = Array.from(channel.recipientIds).filter((id) => id !== senderId);
		return recipientIds.length === 1 ? recipientIds[0]! : null;
	}

	private async checkMessageSendPermissions({
		guild,
		member,
		channel,
		data,
		user,
		checkPermission,
		hasPermission,
	}: {
		guild: GuildResponse | null;
		member: GuildMemberResponse | null;
		channel: Channel;
		data: MessageRequest;
		user: User;
		checkPermission: (permission: bigint) => Promise<void>;
		hasPermission: (permission: bigint) => Promise<boolean>;
	}): Promise<{
		canEmbedLinks: boolean;
		canMentionEveryone: boolean;
		canAttachFiles: boolean;
	}> {
		const [canEmbedLinks, canMentionEveryone, canAttachFiles] = await Promise.all([
			hasPermission(Permissions.EMBED_LINKS),
			hasPermission(Permissions.MENTION_EVERYONE),
			hasPermission(Permissions.ATTACH_FILES),
		]);
		const hasFavoriteMeme = data.favorite_meme_id != null;
		const hasUploadedAttachments = this.attachmentsToProcess(data.attachments) !== undefined;
		if (data.embeds && data.embeds.length > 0 && !canEmbedLinks) {
			throw new MissingPermissionsError();
		}
		if (hasFavoriteMeme && !canEmbedLinks) {
			throw new MissingPermissionsError();
		}
		if ((hasFavoriteMeme || hasUploadedAttachments) && !canAttachFiles) {
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
			assertGuildMemberCanCommunicate(member);
			if (data.tts) {
				const hasTtsPermission = await hasPermission(Permissions.SEND_TTS_MESSAGES);
				if (!hasTtsPermission) {
					data.tts = false;
				}
			}
			await this.deps.channelAuthService.checkGuildVerification({user, guild, member});
		} else if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
			await this.deps.channelAuthService.validateDMSendPermissions({channel, userId: user.id});
		}
		return {canEmbedLinks, canMentionEveryone, canAttachFiles};
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
		if (!user.isBot && user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
		}
		if (isPersonalNotesChannel({userId: user.id, channelId})) {
			await this.validatePersonalNoteMessage({user, channelId, data});
			return;
		}
		const {channel, guild, checkPermission, hasPermission, member} = authChannel;
		const {canMentionEveryone, canEmbedLinks, canAttachFiles} = await this.checkMessageSendPermissions({
			guild,
			member,
			channel,
			data,
			user,
			checkPermission,
			hasPermission,
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
		if (isForwardMessage && referencedMessage && guild) {
			const hasEmbeds =
				(referencedMessage.flags & MessageFlags.SUPPRESS_EMBEDS) === 0 && referencedMessage.embeds.length > 0;
			const hasAttachments = referencedMessage.attachments.length > 0;
			if (hasEmbeds && !canEmbedLinks) {
				throw new MissingPermissionsError();
			}
			if (hasAttachments && !canAttachFiles) {
				throw new MissingPermissionsError();
			}
		}
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
		}
		if (channel && !isForwardMessage && (data.content !== undefined || data.message_reference != null)) {
			const mentionContent = data.content ?? '';
			const mentions = await this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage: referencedMessage || null,
				message: {
					id: createMessageID(await this.deps.snowflakeService.generateForChannel(channelId)),
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
				channelMentions: mentions.channelMentions,
				channel,
				message: {authorId: user.id, webhookId: null},
				guild,
				canMentionRoles: canMentionEveryone,
			});
		}
		await this.ensureAttachmentsExist({
			attachments: data.attachments,
			user,
			channelId,
			guildFeatures: guild?.features ?? null,
		});
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
			const mentions = await this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage: referencedMessage || null,
				message: {
					id: createMessageID(await this.deps.snowflakeService.generateForChannel(channelId)),
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
				channelMentions: mentions.channelMentions,
				channel,
				message: {authorId: user.id, webhookId: null},
				guild: null,
			});
		}
		await this.ensureAttachmentsExist({
			attachments: data.attachments,
			user,
			channelId,
			guildFeatures: null,
		});
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
	}): Promise<{
		referencedMessage: Message | null;
		referencedChannelGuildId?: GuildID | null;
	}> {
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
		return {referencedMessage, referencedChannelGuildId};
	}

	private async ensureAttachmentsExist({
		attachments,
		user,
		channelId,
		guildFeatures,
	}: {
		attachments?: Array<AttachmentRequestData>;
		user: User;
		channelId: ChannelID;
		guildFeatures: Iterable<string> | null;
	}): Promise<void> {
		if (!attachments || attachments.length === 0) return;
		const uploadedAttachmentSizes: Array<{
			size: number | bigint;
		}> = [];
		for (let index = 0; index < attachments.length; index++) {
			const attachment = attachments[index];
			if (!('upload_filename' in attachment) || !attachment.upload_filename) continue;
			const pendingUpload = await this.deps.attachmentUploadTraceRepository.getPendingUpload({
				uploadKey: attachment.upload_filename,
				userId: user.id,
				channelId,
			});
			if (!pendingUpload) {
				throw InputValidationError.fromCode(
					`attachments.${index}.upload_filename`,
					ValidationErrorCodes.UPLOADED_ATTACHMENT_NOT_FOUND,
					{filename: attachment.filename},
				);
			}
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
			uploadedAttachmentSizes.push({size: metadata.contentLength});
		}
		if (uploadedAttachmentSizes.length > 0) {
			this.deps.validationService.validateTotalAttachmentSize(uploadedAttachmentSizes, user, guildFeatures);
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
		let messageSnapshots: Array<MessageSnapshot> | undefined;
		if (isForwardMessage && referencedMessage) {
			messageSnapshots = await createMessageSnapshotsForForward(
				referencedMessage,
				user,
				channelId,
				this.deps.storageService,
				this.deps.snowflakeService,
				this.deps.limitConfigService,
				this.getForwardMediaSelection(data),
			);
		}
		return {referencedMessage, referencedChannelGuildId, messageSnapshots};
	}

	private getForwardMediaSelection(data: MessageRequest): ForwardMediaSelection | undefined {
		const reference = data.message_reference;
		if (reference?.type !== MessageReferenceTypes.FORWARD) {
			return undefined;
		}
		const attachmentIds = reference.attachment_ids?.length
			? new Set<AttachmentID>(reference.attachment_ids.map((id) => createAttachmentID(id)))
			: undefined;
		const embedIndices = reference.embed_indices?.length ? new Set(reference.embed_indices) : undefined;
		if (!attachmentIds && !embedIndices) {
			return undefined;
		}
		return {attachmentIds, embedIndices};
	}

	private snapshotsContainNsfwContent(snapshots: Array<MessageSnapshot>): boolean {
		for (const snapshot of snapshots) {
			for (const att of snapshot.attachments) {
				if (att.nsfw) return true;
			}
			for (const embed of snapshot.embeds) {
				if (embed.nsfw) return true;
				for (const child of embed.children ?? []) {
					if (child.nsfw) return true;
				}
			}
		}
		return false;
	}

	private async ensureForwardSourceAccess(authChannel: AuthenticatedChannel): Promise<void> {
		if (authChannel.guild) {
			await authChannel.checkPermission(Permissions.VIEW_CHANNEL);
		}
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
	}): Promise<SendMessageResult> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			channelId,
		});
		if (!user.isBot && user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
		}
		if (isPersonalNotesChannel({userId: user.id, channelId})) {
			const message = await this.sendPersonalNoteMessage({authChannel, user, channelId, data, requestCache});
			return {message, authChannel};
		}
		const {channel, guild, checkPermission, hasPermission, member} = authChannel;
		const {canEmbedLinks, canMentionEveryone, canAttachFiles} = await this.checkMessageSendPermissions({
			guild,
			member,
			channel,
			data,
			user,
			checkPermission,
			hasPermission,
		});
		const needsSlowmodeCheck = guild && channel.rateLimitPerUser && channel.rateLimitPerUser > 0 && !user.isBot;
		const slowmodeBypass = needsSlowmodeCheck ? await hasPermission(Permissions.BYPASS_SLOWMODE) : false;
		const slowmodeKey = needsSlowmodeCheck && !slowmodeBypass ? `slowmode:${channelId}:${user.id}` : null;
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
			return {message: existingMessage, authChannel};
		}
		const referenceContext = await this.resolveReferenceContext({
			data,
			channelId,
			isForwardMessage,
			user,
		});
		const {referencedMessage, referencedChannelGuildId, messageSnapshots} = referenceContext;
		if (isForwardMessage && messageSnapshots && guild) {
			const snapshotHasEmbeds = messageSnapshots.some((s) => s.embeds.length > 0);
			const snapshotHasAttachments = messageSnapshots.some((s) => s.attachments.length > 0);
			if (snapshotHasEmbeds && !canEmbedLinks) {
				throw new MissingPermissionsError();
			}
			if (snapshotHasAttachments && !canAttachFiles) {
				throw new MissingPermissionsError();
			}
		}
		if (isForwardMessage && messageSnapshots && this.snapshotsContainNsfwContent(messageSnapshots)) {
			const guildNsfw = guild != null && guild.nsfw_level === GuildNSFWLevel.AGE_RESTRICTED;
			const destAllowsNsfw = channel.isNsfw || guildNsfw;
			if (!destAllowsNsfw) {
				throw new NsfwContentRequiresAgeVerificationError();
			}
		}
		if (data.message_reference && guild && !isForwardMessage) {
			const hasReadHistory = await hasPermission(Permissions.READ_MESSAGE_HISTORY);
			if (!hasReadHistory) {
				this.assertReferencedMessageWithinCutoff({
					referencedMessage,
					guild,
				});
			}
		}
		if (data.message_reference && referencedMessage && !isForwardMessage) {
			const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
			if (!replyableTypes.has(referencedMessage.type)) {
				throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
			}
		}
		this.ensureForwardGuildMatches({data, referencedChannelGuildId});
		await this.ensureAttachmentsExist({
			attachments: data.attachments,
			user,
			channelId,
			guildFeatures: guild?.features ?? null,
		});
		const {attachmentsToProcess, favoriteMemeAttachment} = await this.prepareMessageAttachments({
			user,
			channelId,
			data,
		});
		const dmNsfwContext = guild ? undefined : await this.buildDmNsfwContext(channel, user.id);
		const messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(channelId));
		let mentionData: SendMentionData | undefined;
		const shouldExtractMentions =
			channel && !isForwardMessage && (data.content !== undefined || data.message_reference != null);
		if (shouldExtractMentions) {
			const mentionContent = data.content ?? '';
			const mentions = await this.deps.mentionService.extractMentions({
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
			const {validUserIds, validRoleIds, validChannelMentions} = await this.deps.mentionService.validateMentions({
				userMentions: mentions.userMentions,
				roleMentions: mentions.roleMentions,
				channelMentions: mentions.channelMentions,
				channel,
				message: {authorId: user.id, webhookId: null},
				guild,
				canMentionRoles: canMentionEveryone,
			});
			mentionData = {
				flags: mentions.flags,
				mentionUserIds: validUserIds,
				mentionRoleIds: validRoleIds,
				mentionChannelIds: validChannelMentions.map((mentionedChannel) => createChannelID(BigInt(mentionedChannel.id))),
				mentionChannels: validChannelMentions,
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
		if (slowmodeKey) {
			const slowmodeResult = await this.deps.rateLimitService.checkLimit({
				identifier: slowmodeKey,
				maxAttempts: 1,
				windowMs: channel.rateLimitPerUser! * 1000,
				algorithm: 'leaky_bucket',
			});
			if (!slowmodeResult.allowed) {
				throw new SlowmodeRateLimitError({
					retryAfter: slowmodeResult.retryAfter,
					retryAfterDecimal: slowmodeResult.retryAfterDecimal,
				});
			}
		}
		const dmRecipientId = this.getOneToOneDmRecipientId(channel, user.id);
		let suppressDmRecipientDelivery = false;
		if (dmRecipientId && !user.isBot) {
			const spamDecision = await this.deps.directMessageSpamMitigationService.recordOneToOneDmSend({
				sender: user,
				recipientId: dmRecipientId,
			});
			suppressDmRecipientDelivery = spamDecision.shouldSuppressRecipientDelivery;
		}
		const {message, enqueueDeferredEmbeds} = await this.deps.persistenceService.createMessage({
			messageId,
			channelId,
			user,
			type: this.getMessageTypeForRequest(data),
			content: data.content,
			flags: this.deps.validationService.calculateMessageFlags(data),
			embeds: data.embeds,
			attachments: attachmentsToProcess,
			attachmentUploadUserId: user.id,
			processedAttachments: favoriteMemeAttachment ? [favoriteMemeAttachment] : undefined,
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
			dmNsfwContext,
		});
		this.cacheMentionChannels({
			requestCache,
			messageId,
			mentionChannels: mentionData?.mentionChannels,
		});
		if (!suppressDmRecipientDelivery) {
			await this.settlePostCreateWork(messageId, [
				{
					step: 'update_dm_recipients',
					promise: this.deps.processingService.updateDMRecipients({channel, channelId, requestCache}),
				},
				{
					step: 'process_message_after_creation',
					promise: this.deps.processingService.processMessageAfterCreation({
						message,
						channel,
						guild,
						user,
						data,
						referencedMessage,
						mentionHere: mentionData?.mentionHere ?? false,
					}),
				},
				{
					step: 'update_read_states',
					promise: this.deps.processingService.updateReadStates({user, guild, channel, channelId, messageId}),
				},
			]);
		}
		await this.settlePostCreateWork(messageId, [
			{
				step: 'dispatch',
				promise: suppressDmRecipientDelivery
					? this.deps.dispatchService.dispatchMessageCreateToUser({
							channel,
							message,
							userId: user.id,
							requestCache,
							currentUserId: user.id,
							nonce: data.nonce,
							tts: data.tts,
							mentionHere: mentionData?.mentionHere ?? false,
						})
					: this.deps.dispatchService.dispatchMessageCreate({
							channel,
							message,
							requestCache,
							currentUserId: user.id,
							nonce: data.nonce,
							tts: data.tts,
							mentionHere: mentionData?.mentionHere ?? false,
						}),
			},
		]);
		await this.cacheMessageNonceIfPresent({userId: user.id, nonce: data.nonce, channelId, messageId});
		void enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction');
		});
		const searchIndexOptions = this.getSearchIndexOptions(channel);
		if (searchIndexOptions && !suppressDmRecipientDelivery) {
			void this.deps.searchService.indexMessage(message, user.isBot, searchIndexOptions);
		}
		return {message, authChannel};
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
			this.deps.validationService.validateMessageContent(data, null, {
				guildFeatures: guild?.features ?? null,
				messageAuthorType: 'webhook',
			});
		}
		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});
		const webhookActorId = createUserID(BigInt(webhook.id));
		const existingMessage = await this.deps.operationsHelpers.findExistingMessage({
			userId: webhookActorId,
			nonce: data.nonce,
			expectedChannelId: channelId,
		});
		if (existingMessage) {
			return existingMessage;
		}
		let referencedMessage: Message | null = null;
		let messageSnapshots: Array<MessageSnapshot> | undefined;
		if (data.message_reference) {
			const referenceChannelId = isForwardMessage ? createChannelID(data.message_reference.channel_id!) : channelId;
			if (referenceChannelId !== channelId) {
				throw new UnknownMessageError();
			}
			referencedMessage = await this.deps.channelRepository.messages.getMessage(
				referenceChannelId,
				createMessageID(data.message_reference.message_id),
			);
			if (!referencedMessage) {
				throw new UnknownMessageError();
			}
			if (isForwardMessage) {
				messageSnapshots = await createMessageSnapshotsForForward(
					referencedMessage,
					null,
					channelId,
					this.deps.storageService,
					this.deps.snowflakeService,
					this.deps.limitConfigService,
					this.getForwardMediaSelection(data),
				);
			} else {
				const replyableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
				if (!replyableTypes.has(referencedMessage.type)) {
					throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
				}
			}
		}
		const messageReference = this.buildMessageReferencePayload({
			data,
			referencedMessage,
			guild,
			isForwardMessage,
			referencedChannelGuildId: channel.guildId,
		});
		const messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(channelId));
		let mentionData: SendMentionData | undefined;
		const shouldExtractWebhookMentions =
			channel && !isForwardMessage && (data.content !== undefined || data.message_reference != null);
		if (shouldExtractWebhookMentions) {
			const mentionContent = data.content ?? '';
			const mentions = await this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage: referencedMessage,
				message: {
					id: messageId,
					channelId,
					webhookId: webhook.id,
					content: mentionContent,
					flags: this.deps.validationService.calculateMessageFlags(data),
				} as Message,
				channelType: channel.type,
				allowedMentions: data.allowed_mentions || null,
				guild,
			});
			const {validUserIds, validRoleIds, validChannelMentions} = await this.deps.mentionService.validateMentions({
				userMentions: mentions.userMentions,
				roleMentions: mentions.roleMentions,
				channelMentions: mentions.channelMentions,
				channel,
				message: {authorId: null, webhookId: webhook.id},
				guild,
			});
			mentionData = {
				flags: mentions.flags,
				mentionUserIds: validUserIds,
				mentionRoleIds: validRoleIds,
				mentionChannelIds: validChannelMentions.map((mentionedChannel) => createChannelID(BigInt(mentionedChannel.id))),
				mentionChannels: validChannelMentions,
				mentionEveryone: mentions.mentionsEveryone || mentions.mentionsHere,
				mentionHere: mentions.mentionsHere,
			};
		}
		const {message, enqueueDeferredEmbeds} = await this.deps.persistenceService.createMessage({
			messageId,
			channelId,
			webhookId: webhook.id,
			webhookName: username ?? webhook.name!,
			webhookAvatar: avatar ?? webhook.avatarHash,
			type: this.getMessageTypeForRequest(data),
			content: data.content,
			flags: this.deps.validationService.calculateMessageFlags(data),
			embeds: data.embeds,
			attachments: this.attachmentsToProcess(data.attachments),
			attachmentUploadUserId: this.resolveWebhookAttachmentUploadUserId(webhook, data.attachments),
			stickerIds: data.sticker_ids ? data.sticker_ids.flatMap((stickerId) => createStickerID(stickerId)) : undefined,
			messageReference,
			messageSnapshots,
			guildId: channel.guildId,
			channel,
			guild,
			referencedMessage,
			mentionData,
			allowEmbeds: true,
		});
		this.cacheMentionChannels({
			requestCache,
			messageId,
			mentionChannels: mentionData?.mentionChannels,
		});
		await this.deps.mentionService.handleMentionTasks({
			guildId: channel.guildId,
			message,
			authorId: webhookActorId,
			mentionHere: mentionData?.mentionHere ?? false,
		});
		await this.deps.dispatchService.dispatchMessageCreate({
			channel,
			message,
			requestCache,
			nonce: data.nonce,
			mentionHere: mentionData?.mentionHere ?? false,
		});
		await this.cacheMessageNonceIfPresent({userId: webhookActorId, nonce: data.nonce, channelId, messageId});
		void enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction');
		});
		const searchIndexOptions = this.getSearchIndexOptions(channel);
		if (searchIndexOptions) {
			void this.deps.searchService.indexMessage(message, false, searchIndexOptions);
		}
		return message;
	}

	async editWebhookMessage({
		webhook,
		messageId,
		data,
		requestCache,
	}: {
		webhook: Webhook;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const channelId = webhook.channelId!;
		const channel = await this.deps.channelRepository.channelData.findUnique(channelId);
		if (!channel || !channel.guildId) {
			throw new CannotExecuteOnDmError();
		}
		const existingMessage = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!existingMessage) throw new UnknownMessageError();
		if (existingMessage.webhookId !== webhook.id) {
			throw new MissingPermissionsError();
		}
		const guild = await this.deps.gatewayService.getGuildData({
			guildId: channel.guildId,
			userId: createUserID(0n),
			skipMembershipCheck: true,
		});
		this.deps.validationService.validateMessageEditable(existingMessage);
		this.deps.validationService.validateMessageContent(data, null, {
			isUpdate: true,
			guildFeatures: guild?.features ?? null,
			messageAuthorType: 'webhook',
		});
		if (data.embeds) {
			this.deps.embedAttachmentResolver.validateAttachmentReferences({
				embeds: data.embeds,
				attachments: data.attachments,
				existingAttachments: existingMessage.attachments.map((att) => ({filename: att.filename})),
			});
		}
		const {message: updatedMessage, enqueueDeferredEmbeds} = await this.deps.persistenceService.updateMessage({
			message: existingMessage,
			messageId,
			data,
			channel,
			guild,
			attachmentUploadUserId: this.resolveWebhookAttachmentUploadUserId(webhook, data.attachments),
			allowEmbeds: true,
		});
		await this.deps.dispatchService.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
		void enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction after edit');
		});
		const searchIndexOptions = this.getSearchIndexOptions(channel);
		if (searchIndexOptions) {
			void this.deps.searchService.updateMessageIndex(updatedMessage, searchIndexOptions);
		}
		return updatedMessage;
	}

	private async sendPersonalNoteMessage({
		authChannel,
		user,
		channelId,
		data,
		requestCache,
	}: {
		authChannel: AuthenticatedChannel;
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const {channel} = authChannel;
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
		await this.ensureAttachmentsExist({
			attachments: data.attachments,
			user,
			channelId,
			guildFeatures: null,
		});
		const {attachmentsToProcess, favoriteMemeAttachment} = await this.prepareMessageAttachments({
			user,
			channelId,
			data,
		});
		const messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(channelId));
		const messageReference = this.buildMessageReferencePayload({
			data,
			referencedMessage,
			guild: null,
			isForwardMessage,
			referencedChannelGuildId,
		});
		const {message, enqueueDeferredEmbeds} = await this.deps.persistenceService.createMessage({
			messageId,
			channelId,
			user,
			type: this.getMessageTypeForRequest(data),
			content: data.content,
			flags: data.flags ? data.flags & SENDABLE_MESSAGE_FLAGS : 0,
			embeds: data.embeds,
			attachments: attachmentsToProcess,
			attachmentUploadUserId: user.id,
			processedAttachments: favoriteMemeAttachment ? [favoriteMemeAttachment] : undefined,
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
		void enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction');
		});
		if (data.nonce) {
			await this.deps.validationService.cacheMessageNonce({userId: user.id, nonce: data.nonce, channelId, messageId});
		}
		const searchIndexOptions = this.getSearchIndexOptions(channel);
		if (searchIndexOptions) {
			void this.deps.searchService.indexMessage(message, user.isBot, searchIndexOptions);
		}
		return message;
	}
}
