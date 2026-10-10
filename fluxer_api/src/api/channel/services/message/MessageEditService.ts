// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createGuildID, createUserID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import type {CrosspostPropagation} from '@app/api/channel/services/message/CrosspostPropagation';
import {emitMessageUpdated} from '@app/api/channel/services/message/MessageActivity';
import type {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import type {MessageDispatchService} from '@app/api/channel/services/message/MessageDispatchService';
import type {MessageEmbedAttachmentResolver} from '@app/api/channel/services/message/MessageEmbedAttachmentResolver';
import {isOperationDisabled, isPersonalNotesChannel} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageMentionService} from '@app/api/channel/services/message/MessageMentionService';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {MessageProcessingService} from '@app/api/channel/services/message/MessageProcessingService';
import type {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import type {MessageValidationService} from '@app/api/channel/services/message/MessageValidationService';
import type {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import {assertThreadInteractionAllowed} from '@app/api/channel/services/thread/ThreadInteractionGuards';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Message} from '@app/api/models/Message';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import {assertMayStartConversation, oneToOneDmRecipient} from '@app/api/user/NewConversationLimit';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {isDirectDeliverySuppressed} from '@app/api/user/UserHelpers';
import {assertGuildMemberCanCommunicate} from '@app/api/utils/GuildCommunicationUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';

interface EditMessageResult {
	message: Message;
	authChannel: AuthenticatedChannel;
}

interface MessageEditServiceDeps {
	channelRepository: ChannelRepository;
	userRepository: UserRepository;
	validationService: MessageValidationService;
	persistenceService: MessagePersistenceService;
	channelAuthService: MessageChannelAuthService;
	processingService: MessageProcessingService;
	dispatchService: MessageDispatchService;
	searchService: MessageSearchService;
	embedAttachmentResolver: MessageEmbedAttachmentResolver;
	mentionService: MessageMentionService;
	messageWriteLock: MessageWriteLock;
	crosspostPropagation: CrosspostPropagation;
}

export class MessageEditService {
	constructor(private readonly deps: MessageEditServiceDeps) {}

	async editMessage({
		userId,
		viewer,
		channelId,
		messageId,
		data,
		requestCache,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<EditMessageResult> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
		});
		const {channel, guild, hasPermission, member} = authChannel;
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
		assertThreadInteractionAllowed(authChannel, 'edit', {ignoreTimeout: message.authorId !== userId});
		if (data.message_snapshots !== undefined) {
			throw new MissingPermissionsError();
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
		const referencedMessage = message.reference?.messageId
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
			const editedMessage = await this.deps.messageWriteLock.withFreshMessage(channelId, messageId, (fresh) => {
				if (!fresh) throw new UnknownMessageError();
				return this.deps.processingService.handleNonAuthorEdit({
					message: fresh,
					messageId,
					data,
					guild,
					hasPermission,
					channel,
					requestCache,
					persistenceService: this.deps.persistenceService,
					dispatchService: this.deps.dispatchService,
				});
			});
			await this.deps.crosspostPropagation.propagateEdit(editedMessage);
			return {message: editedMessage, authChannel};
		}
		if (user && !isPersonalNotesChannel({userId, channelId})) {
			assertAccountNotLimited(user);
		}
		const dmRecipientId = oneToOneDmRecipient(channel, userId);
		if (user && dmRecipientId !== null) {
			await assertMayStartConversation({
				user,
				targetId: dmRecipientId,
				users: this.deps.userRepository,
				messages: this.deps.channelRepository.messages,
				channel,
			});
		}
		const isBugHunterBot = !!user?.isBot && (user.flags & UserFlags.BUG_HUNTER) !== 0n;
		const dmNsfwContext = guild ? undefined : await this.deps.persistenceService.resolveDmNsfwContext(channel, userId);
		const updateResult = await this.deps.messageWriteLock.withFreshMessage(channelId, messageId, async (fresh) => {
			if (!fresh) throw new UnknownMessageError();
			return this.deps.crosspostPropagation.withPublishedEditBudget({fresh, actor: 'author'}, () =>
				this.deps.persistenceService.updateMessage({
					message: fresh,
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
					dmNsfwContext,
				}),
			);
		});
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
		await this.deps.crosspostPropagation.propagateEdit(updatedMessage);
		if (user && ((data.content !== undefined && data.content !== message.content) || hasNewAttachments)) {
			emitMessageUpdated({
				user,
				message: updatedMessage,
				channel,
				guildId: guild?.id ? createGuildID(BigInt(guild.id)) : null,
				guildOwnerId: guild?.owner_id ? createUserID(BigInt(guild.owner_id)) : null,
				dmRecipientId,
				channelHadMessages: true,
				delivered: !(dmRecipientId !== null && isDirectDeliverySuppressed(user)),
				userRepository: this.deps.userRepository,
			});
		}
		void updateResult.enqueueDeferredEmbeds().catch((error) => {
			Logger.warn({error, messageId: messageId.toString()}, 'Failed to enqueue deferred embed extraction after edit');
		});
		if (channel.indexedAt != null) {
			void this.deps.searchService.updateMessageIndex(updatedMessage);
		}
		return {message: updatedMessage, authChannel};
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
}
