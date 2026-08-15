// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {ChannelID, MessageID, UserID} from '../../../BrandedTypes';
import {Logger} from '../../../Logger';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Message} from '../../../models/Message';
import type {IUserRepository} from '../../../user/IUserRepository';
import {assertGuildMemberCanCommunicate} from '../../../utils/GuildCommunicationUtils';
import type {MessageUpdateRequest} from '../../MessageTypes';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {MessageDispatchService} from './MessageDispatchService';
import type {MessageEmbedAttachmentResolver} from './MessageEmbedAttachmentResolver';
import {isOperationDisabled} from './MessageHelpers';
import type {MessageMentionService} from './MessageMentionService';
import type {MessagePersistenceService} from './MessagePersistenceService';
import type {MessageProcessingService} from './MessageProcessingService';
import type {MessageSearchService} from './MessageSearchService';
import type {MessageValidationService} from './MessageValidationService';

const MESSAGE_LOCK_TTL_SECONDS = 5;
const MESSAGE_LOCK_ACQUIRE_ATTEMPTS = 6;
const MESSAGE_LOCK_RETRY_DELAY_MS = 50;

interface MessageEditServiceDeps {
	channelRepository: IChannelRepositoryAggregate;
	userRepository: IUserRepository;
	cacheService: ICacheService;
	validationService: MessageValidationService;
	persistenceService: MessagePersistenceService;
	channelAuthService: MessageChannelAuthService;
	processingService: MessageProcessingService;
	dispatchService: MessageDispatchService;
	searchService: MessageSearchService;
	embedAttachmentResolver: MessageEmbedAttachmentResolver;
	mentionService: MessageMentionService;
}

export class MessageEditService {
	constructor(private readonly deps: MessageEditServiceDeps) {}

	async editMessage({
		userId,
		channelId,
		messageId,
		data,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const {channel, guild, hasPermission, member} = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
		});
		const hasNewAttachments =
			data.attachments?.some(
				(attachment) =>
					'upload_filename' in attachment &&
					typeof attachment.upload_filename === 'string' &&
					attachment.upload_filename.length > 0,
			) ?? false;
		const [canEmbedLinks, canMentionEveryone, canAttachFiles] = await Promise.all([
			hasPermission(Permissions.EMBED_LINKS),
			hasPermission(Permissions.MENTION_EVERYONE),
			hasPermission(Permissions.ATTACH_FILES),
		]);
		if (data.embeds && data.embeds.length > 0 && !canEmbedLinks) {
			throw new MissingPermissionsError();
		}
		if (hasNewAttachments && !canAttachFiles) {
			throw new MissingPermissionsError();
		}
		if (isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
			throw new FeatureTemporarilyDisabledError();
		}
		const message = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) throw new UnknownMessageError();
		if (message.authorId === userId) {
			assertGuildMemberCanCommunicate(member);
		}
		if (data.message_snapshots !== undefined) {
			const isAuthor = message.authorId === userId;
			const canManage = isAuthor ? true : await hasPermission(Permissions.MANAGE_MESSAGES);
			if (!isAuthor && !canManage) {
				throw new MissingPermissionsError();
			}
			const updatedMessage = await this.withMessageLock(channelId, messageId, () =>
				this.deps.persistenceService.updateSnapshotAttachments({
					message,
					snapshotEdits: data.message_snapshots ?? [],
				}),
			);
			await this.deps.dispatchService.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
			return updatedMessage;
		}
		const user = await this.deps.userRepository.findUnique(userId);
		this.deps.validationService.validateMessageEditable(message);
		this.deps.validationService.validateMessageContent(data, user, {
			isUpdate: true,
			guildFeatures: guild?.features ?? null,
		});
		this.deps.embedAttachmentResolver.validateAttachmentReferences({
			embeds: data.embeds,
			attachments: data.attachments,
			existingAttachments: message.attachments.map((att) => ({filename: att.filename})),
		});
		const referencedMessage = message.reference
			? await this.deps.channelRepository.messages.getMessage(channelId, message.reference.messageId)
			: null;
		const effectiveAllowedMentions = this.getEffectiveAllowedMentionsForEdit({message, referencedMessage, data});
		const hasMentionContentChanges =
			data.content !== undefined || data.allowed_mentions !== undefined || data.embeds !== undefined;
		if (hasMentionContentChanges) {
			const mentionContent = data.content ?? message.content ?? '';
			await this.deps.mentionService.extractMentions({
				content: mentionContent,
				referencedMessage,
				message: {
					id: message.id,
					channelId: message.channelId,
					authorId: message.authorId ?? userId,
					content: mentionContent,
					flags: data.flags ?? message.flags,
				} as Message,
				channelType: channel.type,
				allowedMentions: effectiveAllowedMentions,
				guild,
				canMentionEveryone,
			});
		}
		if (message.authorId !== userId) {
			return await this.withMessageLock(channelId, messageId, () =>
				this.deps.processingService.handleNonAuthorEdit({
					message,
					messageId,
					data,
					guild,
					hasPermission,
					channel,
					requestCache,
					persistenceService: this.deps.persistenceService,
					dispatchService: this.deps.dispatchService,
				}),
			);
		}
		const isBugHunterBot = !!user?.isBot && (user.flags & UserFlags.BUG_HUNTER) !== 0n;
		const updateResult = await this.withMessageLock(channelId, messageId, () =>
			this.deps.persistenceService.updateMessage({
				message,
				messageId,
				data,
				channel,
				guild,
				member,
				attachmentUploadUserId: userId,
				allowEmbeds: canEmbedLinks,
				isBot: user?.isBot,
				isBugHunterBot,
				locale: user?.locale,
			}),
		);
		let updatedMessage = updateResult.message;
		if (data.content !== undefined || data.allowed_mentions !== undefined || data.embeds !== undefined) {
			const mentionResult = await this.deps.processingService.handleMentions({
				channel,
				message: updatedMessage,
				referencedMessageOnSend: referencedMessage,
				allowedMentions: effectiveAllowedMentions,
				guild,
				canMentionEveryone,
				canMentionRoles: canMentionEveryone,
			});
			updatedMessage = mentionResult.message;
			if (mentionResult.mentionChannels.length > 0) {
				requestCache.messageMentionChannels.set(updatedMessage.id.toString(), mentionResult.mentionChannels);
			}
		}
		await this.deps.dispatchService.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
		void updateResult.enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction after edit');
		});
		if (channel.indexedAt != null) {
			void this.deps.searchService.updateMessageIndex(updatedMessage);
		}
		return updatedMessage;
	}

	private getEffectiveAllowedMentionsForEdit({
		message,
		referencedMessage,
		data,
	}: {
		message: Message;
		referencedMessage: Message | null;
		data: MessageUpdateRequest;
	}): AllowedMentionsRequest | null {
		if (data.allowed_mentions !== undefined) {
			return data.allowed_mentions;
		}
		const referencedAuthorId = referencedMessage?.authorId;
		if (
			referencedAuthorId == null ||
			message.authorId == null ||
			referencedAuthorId === message.authorId ||
			message.mentionedUserIds.has(referencedAuthorId)
		) {
			return null;
		}
		return {replied_user: false};
	}

	private async withMessageLock<T>(channelId: ChannelID, messageId: MessageID, fn: () => Promise<T>): Promise<T> {
		const lockKey = `message:${channelId}:${messageId}:write`;
		let lockToken: string | null = null;
		for (let attempt = 0; attempt < MESSAGE_LOCK_ACQUIRE_ATTEMPTS; attempt++) {
			lockToken = await this.deps.cacheService.acquireLock(lockKey, MESSAGE_LOCK_TTL_SECONDS);
			if (lockToken) break;
			await new Promise((resolve) => setTimeout(resolve, MESSAGE_LOCK_RETRY_DELAY_MS * (attempt + 1)));
		}
		if (!lockToken) {
			throw new ThrottledError({
				code: APIErrorCodes.RESOURCE_LOCKED,
				headers: {'Retry-After': '1'},
				data: {retry_after: 1},
			});
		}
		try {
			return await fn();
		} finally {
			await this.deps.cacheService.releaseLock(lockKey, lockToken).catch(() => {});
		}
	}
}
