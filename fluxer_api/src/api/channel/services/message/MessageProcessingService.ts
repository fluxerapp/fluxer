// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {CannotEditOtherUserMessageError} from '@fluxer/errors/src/domains/channel/CannotEditOtherUserMessageError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import {type ChannelID, createChannelID, createGuildID, type MessageID, type UserID} from '../../../BrandedTypes';
import type {GatewayChannelMention, IGatewayService} from '../../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../../infrastructure/UserCacheService';
import {Logger} from '../../../Logger';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {User} from '../../../models/User';
import type {ReadStateService} from '../../../read_state/ReadStateService';
import type {IUserRepository} from '../../../user/IUserRepository';
import {mapChannelToResponse} from '../../ChannelMappers';
import type {MessageRequest, MessageUpdateRequest} from '../../MessageTypes';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageDispatchService} from './MessageDispatchService';
import {isPersonalNotesChannel} from './MessageHelpers';
import type {MessageMentionService} from './MessageMentionService';
import type {MessagePersistenceService} from './MessagePersistenceService';
import {incrementDmMentionCounts} from './ReadStateHelpers';

interface RecipientOpenState {
	recipientId: UserID;
	isOpen: boolean;
}

interface MentionProcessingResult {
	message: Message;
	mentionChannels: Array<GatewayChannelMention>;
}

function channelWithLastMessageId(channel: Channel, messageId: MessageID): Channel {
	if (channel.lastMessageId != null && channel.lastMessageId >= messageId) {
		return channel;
	}
	return new Channel({...channel.toRow(), last_message_id: messageId});
}

export class MessageProcessingService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private userCacheService: UserCacheService,
		private gatewayService: IGatewayService,
		private readStateService: ReadStateService,
		private mentionService: MessageMentionService,
	) {}

	async processMessageAfterCreation(params: {
		message: Message;
		channel: Channel;
		guild: GuildResponse | null;
		user: User;
		data: MessageRequest;
		referencedMessage: Message | null;
		mentionHere?: boolean;
	}): Promise<void> {
		const {message, guild, user, mentionHere = false} = params;
		await this.mentionService.handleMentionTasks({
			guildId: guild ? createGuildID(BigInt(guild.id)) : null,
			message,
			authorId: user.id,
			mentionHere,
		});
	}

	async updateDMRecipients({
		channel,
		channelId,
		messageId,
		requestCache,
	}: {
		channel: Channel;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		if (channel.guildId || channel.type !== ChannelTypes.DM) return;
		if (!channel.recipientIds || channel.recipientIds.size !== 2) return;
		const recipientIds = Array.from(channel.recipientIds);
		const openStates = await this.batchCheckDmChannelOpen(recipientIds, channelId);
		const closedRecipients = openStates.filter((state) => !state.isOpen);
		if (closedRecipients.length === 0) return;
		const snapshotChannel = channelWithLastMessageId(channel, messageId);
		await Promise.all(
			closedRecipients.map((state) =>
				this.openDmAndDispatch({
					recipientId: state.recipientId,
					channel: snapshotChannel,
					requestCache,
				}),
			),
		);
	}

	private async batchCheckDmChannelOpen(
		recipientIds: Array<UserID>,
		channelId: ChannelID,
	): Promise<Array<RecipientOpenState>> {
		return Promise.all(
			recipientIds.map(async (recipientId) => ({
				recipientId,
				isOpen: await this.userRepository.isDmChannelOpen(recipientId, channelId),
			})),
		);
	}

	private async openDmAndDispatch(params: {
		recipientId: UserID;
		channel: Channel;
		requestCache: RequestCache;
	}): Promise<void> {
		const {recipientId, channel, requestCache} = params;
		await this.userRepository.openPrivateChannelForUser(recipientId, channel);
		const channelResponse = await mapChannelToResponse({
			channel,
			currentUserId: recipientId,
			userCacheService: this.userCacheService,
			requestCache,
		});
		await this.gatewayService.dispatchPresence({
			userId: recipientId,
			event: 'CHANNEL_CREATE',
			data: channelResponse,
		});
	}

	async updateReadStates({
		user,
		guild,
		channel,
		channelId,
		messageId,
	}: {
		user: User;
		guild: GuildResponse | null;
		channel: Channel;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		if (!guild) {
			const recipients = await this.userRepository.listUsers(Array.from(channel.recipientIds));
			await incrementDmMentionCounts({
				readStateService: this.readStateService,
				userRepository: this.userRepository,
				user,
				recipients,
				channelId,
				messageId,
			});
		}
	}

	async handleMentions(params: {
		channel: Channel;
		message: Message;
		referencedMessageOnSend: Message | null;
		allowedMentions: AllowedMentionsRequest | null;
		guild?: GuildResponse | null;
		canMentionEveryone: boolean;
		canMentionRoles: boolean;
	}): Promise<MentionProcessingResult> {
		const {channel, message, referencedMessageOnSend, allowedMentions, guild, canMentionEveryone, canMentionRoles} =
			params;
		if (message.authorId != null && isPersonalNotesChannel({userId: message.authorId, channelId: channel.id})) {
			return {message, mentionChannels: []};
		}
		const content = message.content ?? '';
		const mentions = await this.mentionService.extractMentions({
			content,
			referencedMessage: referencedMessageOnSend,
			message,
			channelType: channel.type,
			allowedMentions,
			guild,
			canMentionEveryone,
		});
		const {validUserIds, validRoleIds, validChannelMentions} = await this.mentionService.validateMentions({
			userMentions: mentions.userMentions,
			roleMentions: mentions.roleMentions,
			channelMentions: mentions.channelMentions,
			channel,
			message,
			guild,
			canMentionRoles,
		});
		const updatedMessageData = {
			...message.toRow(),
			flags: mentions.flags,
			mention_users: validUserIds.length > 0 ? new Set(validUserIds) : null,
			mention_roles: validRoleIds.length > 0 ? new Set(validRoleIds) : null,
			mention_channels:
				validChannelMentions.length > 0
					? new Set(validChannelMentions.map((mentionedChannel) => createChannelID(BigInt(mentionedChannel.id))))
					: null,
			mention_everyone: mentions.mentionsEveryone,
		};
		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
		return {message: updatedMessage, mentionChannels: validChannelMentions};
	}

	async repairMentionsOnRead(message: Message, sourceChannel?: Channel): Promise<Message> {
		if (
			message.mentionedUserIds.size === 0 &&
			message.mentionedRoleIds.size === 0 &&
			message.mentionedChannelIds.size === 0 &&
			!message.mentionEveryone
		) {
			const hasChannelMentionSyntax = (message.content ?? '').includes('<#');
			if (!hasChannelMentionSyntax) {
				return message;
			}
		}
		const referencedMessage =
			message.reference && message.mentionedUserIds.size > 0
				? await this.channelRepository.messages.getMessage(message.reference.channelId, message.reference.messageId)
				: null;
		const repair = await this.mentionService.buildReadRepairMentionData({message, referencedMessage});
		if (!repair.changed) {
			return message;
		}
		const resolvedSourceChannel =
			sourceChannel ??
			(repair.channelMentions.size > 0 || message.mentionedChannelIds.size > 0
				? await this.channelRepository.channelData.findUnique(message.channelId)
				: null);
		const validChannelMentions = resolvedSourceChannel
			? await this.mentionService.validateChannelMentions({
					channelMentions: repair.channelMentions,
					channel: resolvedSourceChannel,
				})
			: [];
		const updatedMessageData = {
			...message.toRow(),
			mention_users: repair.userMentions.size > 0 ? repair.userMentions : null,
			mention_roles: repair.roleMentions.size > 0 ? repair.roleMentions : null,
			mention_channels:
				validChannelMentions.length > 0
					? new Set(validChannelMentions.map((mentionedChannel) => createChannelID(BigInt(mentionedChannel.id))))
					: null,
			mention_everyone: repair.mentionsEveryone,
		};
		try {
			return await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
		} catch (error) {
			Logger.warn(
				{
					error,
					channelId: message.channelId.toString(),
					messageId: message.id.toString(),
				},
				'Failed to repair message mentions during read',
			);
			return message;
		}
	}

	async handleNonAuthorEdit(params: {
		message: Message;
		messageId: MessageID;
		data: MessageUpdateRequest;
		guild: GuildResponse | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
		channel: Channel;
		requestCache: RequestCache;
		persistenceService: MessagePersistenceService;
		dispatchService: MessageDispatchService;
	}): Promise<Message> {
		const {message, data, guild, hasPermission, channel, requestCache, persistenceService, dispatchService} = params;
		const editResult = await persistenceService.handleNonAuthorEdit({
			message,
			data,
			guild,
			hasPermission,
		});
		if (editResult.canEdit && (editResult.updatedFlags !== undefined || editResult.updatedAttachments !== undefined)) {
			const updatedRowData = {...message.toRow()};
			if (editResult.updatedFlags !== undefined) {
				updatedRowData.flags = editResult.updatedFlags;
			}
			if (editResult.updatedAttachments !== undefined) {
				updatedRowData.attachments = editResult.updatedAttachments;
			}
			const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
			await dispatchService.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
			return updatedMessage;
		}
		throw new CannotEditOtherUserMessageError();
	}
}
