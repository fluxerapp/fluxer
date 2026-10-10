// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, GuildID, MessageID, RoleID, UserID} from '@app/api/BrandedTypes';
import {
	channelIdToMessageId,
	createAttachmentID,
	createChannelID,
	createGuildID,
	createMessageID,
	createStickerID,
	createUserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {AttachmentRequestData, AttachmentToProcess} from '@app/api/channel/AttachmentDTOs';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import type {AttachmentUploadTraceRepository} from '@app/api/channel/repositories/message/AttachmentUploadTraceRepository';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import type {CrosspostPropagation} from '@app/api/channel/services/message/CrosspostPropagation';
import {emitMessageCreated} from '@app/api/channel/services/message/MessageActivity';
import type {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import type {MessageDispatchService} from '@app/api/channel/services/message/MessageDispatchService';
import type {MessageEmbedAttachmentResolver} from '@app/api/channel/services/message/MessageEmbedAttachmentResolver';
import {
	createMessageSnapshotsForForward,
	type ForwardMediaSelection,
	isOperationDisabled,
	isPersonalNotesChannel,
} from '@app/api/channel/services/message/MessageHelpers';
import {assertMessageWithinHistoryCutoff} from '@app/api/channel/services/message/MessageHistoryCutoff';
import type {MessageMentionService} from '@app/api/channel/services/message/MessageMentionService';
import type {MessageOperationsHelpers} from '@app/api/channel/services/message/MessageOperationsHelpers';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {MessageProcessingService} from '@app/api/channel/services/message/MessageProcessingService';
import type {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import type {MessageValidationService} from '@app/api/channel/services/message/MessageValidationService';
import type {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import type {ThreadMessageActivity} from '@app/api/channel/services/message/ThreadMessageActivity';
import {assertThreadAllowed} from '@app/api/channel/services/thread/ThreadDenials';
import {enqueueThreadSearchSync} from '@app/api/channel/threads/ThreadJobs';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {MessageAttachment, MessageReference} from '@app/api/database/types/MessageTypes';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {IFavoriteMemeRepository} from '@app/api/favorite_meme/IFavoriteMemeRepository';
import type {GatewayChannelMention, IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {MessageSnapshot} from '@app/api/models/MessageSnapshot';
import type {User} from '@app/api/models/User';
import type {Webhook} from '@app/api/models/Webhook';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {assertMayStartConversation} from '@app/api/user/NewConversationLimit';
import {isContentHidden, isDirectDeliverySuppressed} from '@app/api/user/UserHelpers';
import {assertGuildMemberCanCommunicate} from '@app/api/utils/GuildCommunicationUtils';
import {
	ChannelTypes,
	MessageReferenceTypes,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {threadWriteBlock} from '@fluxer/constants/src/ThreadPermissionUtils';
import {DELETED_USER_ID, UserFlags} from '@fluxer/constants/src/UserConstants';
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
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

interface MessageSendServiceDeps {
	threadActivity: ThreadMessageActivity;
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
	messageWriteLock: MessageWriteLock;
	crosspostPropagation: CrosspostPropagation;
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

	private attachmentsToProcess(attachments?: Array<AttachmentRequestData>): Array<AttachmentToProcess> | undefined {
		if (!attachments) return undefined;
		const processed = attachments.filter(
			(att): att is AttachmentToProcess =>
				'upload_filename' in att && typeof att.upload_filename === 'string' && att.upload_filename.length > 0,
		);
		return processed.length > 0 ? processed : undefined;
	}

	private async resolveWebhookAttachmentUploadUserId(
		webhook: Webhook,
		attachments?: Array<AttachmentRequestData>,
	): Promise<UserID | undefined> {
		if (this.attachmentsToProcess(attachments) === undefined) {
			return webhook.creatorId ?? undefined;
		}
		if (!webhook.creatorId) {
			return createUserID(DELETED_USER_ID);
		}
		const creator = await this.deps.userRepository.findUnique(webhook.creatorId);
		return creator ? webhook.creatorId : createUserID(DELETED_USER_ID);
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

	private assertThreadSendAllowed(authChannel: AuthenticatedChannel): void {
		if (authChannel.thread) {
			assertThreadAllowed(threadWriteBlock('send', authChannel.thread.actor));
		}
	}

	private assertForwardableReference(isForwardMessage: boolean, referencedMessage: Message | null): void {
		if (isForwardMessage && referencedMessage?.type === MessageTypes.THREAD_CREATED) {
			throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
		}
	}

	private async generateMessageId(channel: Channel): Promise<MessageID> {
		let messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(channel.id));
		for (let attempt = 0; channel.isThread() && BigInt(messageId) <= BigInt(channel.id) && attempt < 3; attempt++) {
			messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(channel.id));
		}
		if (channel.isThread() && BigInt(messageId) <= BigInt(channel.id)) {
			throw new Error('Thread message id must be greater than the thread id');
		}
		return messageId;
	}

	async validateForumStarter({
		user,
		parentAuth,
		data,
	}: {
		user: User;
		parentAuth: AuthenticatedChannel;
		data: MessageRequest;
	}): Promise<void> {
		if (!user.isBot && user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
		}
		const {channel, guild, member, checkPermission, hasPermission} = parentAuth;
		await this.checkMessageSendPermissions({guild, member, channel, data, user, checkPermission, hasPermission});
		this.ensureMessageRequestIsValid({user, data, guildFeatures: guild?.features ?? null});
		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});
		await this.ensureAttachmentsExist({
			attachments: data.attachments,
			user,
			channelId: channel.id,
			guildFeatures: guild?.features ?? null,
		});
	}

	async validateWebhookForumStarter({parent, data}: {parent: Channel; data: MessageRequest}): Promise<void> {
		if (!parent.guildId) throw new CannotExecuteOnDmError();
		const guild = await this.deps.gatewayService.getGuildData({
			guildId: parent.guildId,
			userId: createUserID(0n),
			skipMembershipCheck: true,
		});
		this.ensureWebhookMessageRequestIsValid(data, guild?.features ?? null);
		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
		});
	}

	private ensureWebhookMessageRequestIsValid(data: MessageRequest, guildFeatures: Iterable<string> | null): boolean {
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
				guildFeatures,
				messageAuthorType: 'webhook',
			});
		}
		return isForwardMessage;
	}

	private assertNotThreadStarterReference(
		referenceIsThread: boolean,
		referenceChannelId: ChannelID,
		messageId: bigint | string,
	): void {
		if (referenceIsThread && String(messageId) === referenceChannelId.toString()) {
			throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.CANNOT_REPLY_TO_SYSTEM_MESSAGE);
		}
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
		channelIsThread,
		isForwardMessage,
		user,
		viewer,
	}: {
		data: MessageRequest;
		channelId: ChannelID;
		channelIsThread: boolean;
		isForwardMessage: boolean;
		user: User;
		viewer: ThreadViewer;
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
				viewer,
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
			this.assertNotThreadStarterReference(
				forwardReferenceAuthChannel?.channel.isThread() ?? channelIsThread,
				referenceChannelId,
				data.message_reference.message_id,
			);
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
		viewer,
		channelId,
		data,
		requestCache,
		forumStarter,
	}: {
		user: User;
		viewer: ThreadViewer;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
		forumStarter?: {parentAuth: AuthenticatedChannel};
	}): Promise<SendMessageResult> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			viewer,
			channelId,
		});
		if (!user.isBot && user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			throw InputValidationError.fromCode('content', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
		}
		if (isPersonalNotesChannel({userId: user.id, channelId})) {
			const message = await this.sendPersonalNoteMessage({authChannel, user, viewer, channelId, data, requestCache});
			return {message, authChannel};
		}
		assertAccountNotLimited(user);
		const {channel, guild, member} = authChannel;
		const {checkPermission, hasPermission} = forumStarter?.parentAuth ?? authChannel;
		const uploadChannelId = forumStarter?.parentAuth.channel.id ?? channelId;
		const {canEmbedLinks, canMentionEveryone, canAttachFiles} = await this.checkMessageSendPermissions({
			guild,
			member,
			channel,
			data,
			user,
			checkPermission,
			hasPermission,
		});
		const needsSlowmodeCheck =
			!forumStarter && guild && channel.rateLimitPerUser && channel.rateLimitPerUser > 0 && !user.isBot;
		const slowmodeBypass = needsSlowmodeCheck ? await hasPermission(Permissions.BYPASS_SLOWMODE) : false;
		const slowmodeKey = needsSlowmodeCheck && !slowmodeBypass ? `slowmode:${channelId}:${user.id}` : null;
		this.deps.validationService.ensureTextChannel(channel);
		this.assertThreadSendAllowed(authChannel);
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
			channelIsThread: channel.isThread(),
			isForwardMessage,
			user,
			viewer,
		});
		const {referencedMessage, referencedChannelGuildId, messageSnapshots} = referenceContext;
		this.assertForwardableReference(isForwardMessage, referencedMessage);
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
			const destAllowsNsfw = (authChannel.thread?.parent ?? channel).isNsfw || guildNsfw;
			if (!destAllowsNsfw) {
				throw new NsfwContentRequiresAgeVerificationError();
			}
		}
		if (data.message_reference && guild && !isForwardMessage) {
			const hasReadHistory = await hasPermission(Permissions.READ_MESSAGE_HISTORY);
			if (!hasReadHistory) {
				assertMessageWithinHistoryCutoff({
					message: referencedMessage,
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
		const dmRecipientId = this.getOneToOneDmRecipientId(channel, user.id);
		if (dmRecipientId !== null) {
			await assertMayStartConversation({
				user,
				targetId: dmRecipientId,
				users: this.deps.userRepository,
				messages: this.deps.channelRepository.messages,
				channel,
			});
		}
		await this.ensureAttachmentsExist({
			attachments: data.attachments,
			user,
			channelId: uploadChannelId,
			guildFeatures: guild?.features ?? null,
		});
		const {attachmentsToProcess, favoriteMemeAttachment} = await this.prepareMessageAttachments({
			user,
			channelId,
			data,
		});
		const dmNsfwContext = guild ? undefined : await this.deps.persistenceService.resolveDmNsfwContext(channel, user.id);
		const messageId = forumStarter ? channelIdToMessageId(channel.id) : await this.generateMessageId(channel);
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
		if (authChannel.thread && !forumStarter) {
			await this.deps.threadActivity.beforeUserSend({
				channel,
				parent: authChannel.thread.parent,
				state: authChannel.thread.state,
				member: authChannel.thread.member,
				userId: user.id,
				isBot: user.isBot,
			});
		}
		const suppressDmRecipientDelivery = dmRecipientId !== null && isDirectDeliverySuppressed(user);
		const suppressDelivery = suppressDmRecipientDelivery || isContentHidden(user, messageId);
		const channelHadMessages = channel.lastMessageId !== null;
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
			uploadChannelId,
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
			threadInsert: authChannel.thread !== undefined,
		});
		this.cacheMentionChannels({
			requestCache,
			messageId,
			mentionChannels: mentionData?.mentionChannels,
		});
		if (!suppressDelivery) {
			await this.settlePostCreateWork(messageId, [
				{
					step: 'update_dm_recipients',
					promise: this.deps.processingService.updateDMRecipients({channel, channelId, messageId, requestCache}),
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
				...(authChannel.thread && mentionData && mentionData.mentionUserIds.length > 0
					? [
							{
								step: 'thread_mention_members',
								promise: this.deps.threadActivity.addMentionedUsers({
									channel,
									parent: authChannel.thread.parent,
									isModerator: authChannel.thread.isModerator,
									authorId: user.id,
									mentionUserIds: mentionData.mentionUserIds,
								}),
							},
						]
					: []),
			]);
		}
		await this.settlePostCreateWork(messageId, [
			{
				step: 'dispatch',
				promise: suppressDelivery
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
		emitMessageCreated({
			user,
			message,
			channel,
			guildId: guild?.id ? createGuildID(BigInt(guild.id)) : null,
			guildOwnerId: guild?.owner_id ? createUserID(BigInt(guild.owner_id)) : null,
			dmRecipientId,
			channelHadMessages,
			delivered: !suppressDelivery,
			userRepository: this.deps.userRepository,
		});
		void enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction');
		});
		if (authChannel.thread) enqueueThreadSearchSync(channel.id, {activity: true});
		const searchIndexOptions = this.getSearchIndexOptions(channel);
		if (searchIndexOptions && !suppressDmRecipientDelivery) {
			void this.deps.searchService.indexMessage(message, user.isBot, searchIndexOptions);
		}
		return {message, authChannel};
	}

	async sendWebhookMessage({
		webhook,
		thread,
		data,
		username,
		avatar,
		requestCache,
		forumStarter,
	}: {
		webhook: Webhook;
		thread?: Channel | null;
		data: MessageRequest;
		username?: string | null;
		avatar?: string | null;
		requestCache: RequestCache;
		forumStarter?: boolean;
	}): Promise<Message> {
		const channelId = thread?.id ?? webhook.channelId!;
		const channel = thread ?? (await this.deps.channelRepository.channelData.findUnique(channelId));
		if (!channel?.guildId) {
			throw new CannotExecuteOnDmError();
		}
		const guild = await this.deps.gatewayService.getGuildData({
			guildId: channel.guildId,
			userId: createUserID(0n),
			skipMembershipCheck: true,
		});
		const isForwardMessage = this.ensureWebhookMessageRequestIsValid(data, guild?.features ?? null);
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
			this.assertForwardableReference(isForwardMessage, referencedMessage);
		}
		const messageReference = this.buildMessageReferencePayload({
			data,
			referencedMessage,
			guild,
			isForwardMessage,
			referencedChannelGuildId: channel.guildId,
		});
		if (thread && !forumStarter) {
			await this.deps.threadActivity.beforeWebhookSend(thread);
		}
		const messageId = thread && forumStarter ? channelIdToMessageId(thread.id) : await this.generateMessageId(channel);
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
			attachmentUploadUserId: await this.resolveWebhookAttachmentUploadUserId(webhook, data.attachments),
			uploadChannelId: thread ? (webhook.channelId ?? undefined) : undefined,
			stickerIds: data.sticker_ids ? data.sticker_ids.flatMap((stickerId) => createStickerID(stickerId)) : undefined,
			messageReference,
			messageSnapshots,
			guildId: channel.guildId,
			channel,
			guild,
			referencedMessage,
			mentionData,
			allowEmbeds: true,
			threadInsert: thread != null,
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
		if (thread) enqueueThreadSearchSync(thread.id, {activity: true});
		const searchIndexOptions = this.getSearchIndexOptions(channel);
		if (searchIndexOptions) {
			void this.deps.searchService.indexMessage(message, false, searchIndexOptions);
		}
		return message;
	}

	async editWebhookMessage({
		webhook,
		thread,
		messageId,
		data,
		requestCache,
	}: {
		webhook: Webhook;
		thread?: Channel | null;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const channelId = thread?.id ?? webhook.channelId!;
		const channel = thread ?? (await this.deps.channelRepository.channelData.findUnique(channelId));
		if (!channel?.guildId) {
			throw new CannotExecuteOnDmError();
		}
		const existingMessage = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!existingMessage) throw new UnknownMessageError();
		if (existingMessage.webhookId !== webhook.id) {
			throw new MissingPermissionsError();
		}
		if (thread) {
			await this.deps.threadActivity.assertWebhookCanEdit(thread);
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
		const attachmentUploadUserId = await this.resolveWebhookAttachmentUploadUserId(webhook, data.attachments);
		const {message: updatedMessage, enqueueDeferredEmbeds} = await this.deps.messageWriteLock.withFreshMessage(
			channelId,
			messageId,
			async (fresh) => {
				if (!fresh) throw new UnknownMessageError();
				if (fresh.webhookId !== webhook.id) {
					throw new MissingPermissionsError();
				}
				return this.deps.crosspostPropagation.withPublishedEditBudget({fresh, actor: 'webhook'}, () =>
					this.deps.persistenceService.updateMessage({
						message: fresh,
						messageId,
						data,
						channel,
						guild,
						attachmentUploadUserId,
						allowEmbeds: true,
					}),
				);
			},
		);
		await this.deps.dispatchService.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
		await this.deps.crosspostPropagation.propagateEdit(updatedMessage);
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
		viewer,
		channelId,
		data,
		requestCache,
	}: {
		authChannel: AuthenticatedChannel;
		user: User;
		viewer: ThreadViewer;
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
			channelIsThread: channel.isThread(),
			isForwardMessage,
			user,
			viewer,
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
			stickerIds: data.sticker_ids ? data.sticker_ids.flatMap((stickerId) => createStickerID(stickerId)) : undefined,
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
