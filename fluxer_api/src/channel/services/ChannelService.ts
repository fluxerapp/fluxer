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

import type {ChannelID} from '~/BrandedTypes';
import {TEXT_BASED_CHANNEL_TYPES} from '~/Constants';
import type {MessageRequest} from '~/channel/ChannelModel';
import {CannotSendMessageToNonTextChannelError} from '~/Errors';
import type {IFavoriteMemeRepository} from '~/favorite_meme/IFavoriteMemeRepository';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {EmbedService} from '~/infrastructure/EmbedService';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {ILiveKitService} from '~/infrastructure/ILiveKitService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {IVirusScanService} from '~/infrastructure/IVirusScanService';
import type {IVoiceRoomStore} from '~/infrastructure/IVoiceRoomStore';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {IInviteRepository} from '~/invite/IInviteRepository';
import type {User} from '~/Models';
import type {PackService} from '~/pack/PackService';
import type {ReadStateService} from '~/read_state/ReadStateService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {VoiceAvailabilityService} from '~/voice/VoiceAvailabilityService';
import type {IWebhookRepository} from '~/webhook/IWebhookRepository';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {IChannelRepository} from '../IChannelRepository';
import {AttachmentUploadService} from './AttachmentUploadService';
import {CallService} from './CallService';
import {ChannelDataService} from './ChannelDataService';
import {GroupDmService} from './GroupDmService';
import {MessageInteractionService} from './MessageInteractionService';
import {MessageService} from './MessageService';
import {MessagePersistenceService} from './message/MessagePersistenceService';

export class ChannelService {
	public readonly channelData: ChannelDataService;
	public readonly messages: MessageService;
	public readonly interactions: MessageInteractionService;
	public readonly attachments: AttachmentUploadService;
	public readonly groupDms: GroupDmService;
	public readonly calls: CallService;

	constructor(
		channelRepository: IChannelRepository,
		userRepository: IUserRepository,
		guildRepository: IGuildRepository,
		packService: PackService,
		userCacheService: UserCacheService,
		embedService: EmbedService,
		readStateService: ReadStateService,
		cacheService: ICacheService,
		storageService: IStorageService,
		gatewayService: IGatewayService,
		mediaService: IMediaService,
		avatarService: AvatarService,
		workerService: IWorkerService,
		virusScanService: IVirusScanService,
		snowflakeService: SnowflakeService,
		rateLimitService: IRateLimitService,
		cloudflarePurgeQueue: ICloudflarePurgeQueue,
		favoriteMemeRepository: IFavoriteMemeRepository,
		guildAuditLogService: GuildAuditLogService,
		voiceRoomStore: IVoiceRoomStore,
		liveKitService: ILiveKitService,
		inviteRepository: IInviteRepository,
		webhookRepository: IWebhookRepository,
		voiceAvailabilityService?: VoiceAvailabilityService,
	) {
		const messagePersistenceService = new MessagePersistenceService(
			channelRepository,
			userRepository,
			guildRepository,
			packService,
			embedService,
			storageService,
			mediaService,
			virusScanService,
			snowflakeService,
			readStateService,
		);

		this.channelData = new ChannelDataService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			storageService,
			gatewayService,
			mediaService,
			avatarService,
			snowflakeService,
			cloudflarePurgeQueue,
			voiceRoomStore,
			liveKitService,
			voiceAvailabilityService,
			messagePersistenceService,
			guildAuditLogService,
			inviteRepository,
			webhookRepository,
		);

		this.messages = new MessageService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			readStateService,
			cacheService,
			storageService,
			gatewayService,
			mediaService,
			workerService,
			snowflakeService,
			rateLimitService,
			cloudflarePurgeQueue,
			favoriteMemeRepository,
			guildAuditLogService,
			messagePersistenceService,
		);

		this.interactions = new MessageInteractionService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			readStateService,
			gatewayService,
			mediaService,
			snowflakeService,
			messagePersistenceService,
			guildAuditLogService,
		);

		this.attachments = new AttachmentUploadService(
			channelRepository,
			userRepository,
			storageService,
			cloudflarePurgeQueue,
			this.interactions.getChannelAuthenticated.bind(this.interactions),
			(channel) => {
				if (!channel) throw new Error('Channel is required');
				if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
					throw new CannotSendMessageToNonTextChannelError();
				}
			},
			this.interactions.dispatchMessageUpdate.bind(this.interactions),
			this.messages.deleteMessage.bind(this.messages),
		);

		this.groupDms = new GroupDmService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			gatewayService,
			mediaService,
			snowflakeService,
			this.channelData.groupDmUpdateService,
			this.messages.getMessagePersistenceService(),
		);

		this.calls = new CallService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
			userCacheService,
			mediaService,
			snowflakeService,
			readStateService,
			voiceAvailabilityService,
		);
	}

	async getChannel(...args: Parameters<ChannelDataService['getChannel']>) {
		return this.channelData.getChannel(...args);
	}

	async getChannelAuthenticated(...args: Parameters<ChannelDataService['getChannelAuthenticated']>) {
		return this.channelData.getChannelAuthenticated(...args);
	}

	async getPublicChannelData(...args: Parameters<ChannelDataService['getPublicChannelData']>) {
		return this.channelData.getPublicChannelData(...args);
	}

	async getChannelMemberCount(...args: Parameters<ChannelDataService['getChannelMemberCount']>) {
		return this.channelData.getChannelMemberCount(...args);
	}

	async getChannelSystem(...args: Parameters<ChannelDataService['getChannelSystem']>) {
		return this.channelData.getChannelSystem(...args);
	}

	async editChannel(...args: Parameters<ChannelDataService['editChannel']>) {
		return this.channelData.editChannel(...args);
	}

	async deleteChannel(...args: Parameters<ChannelDataService['deleteChannel']>) {
		return this.channelData.deleteChannel(...args);
	}

	async getAvailableRtcRegions(...args: Parameters<ChannelDataService['getAvailableRtcRegions']>) {
		return this.channelData.getAvailableRtcRegions(...args);
	}

	async sendMessage(...args: Parameters<MessageService['sendMessage']>) {
		return this.messages.sendMessage(...args);
	}

	async sendWebhookMessage(...args: Parameters<MessageService['sendWebhookMessage']>) {
		return this.messages.sendWebhookMessage(...args);
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
		return this.messages.validateMessageCanBeSent({user, channelId, data});
	}

	async editMessage(...args: Parameters<MessageService['editMessage']>) {
		return this.messages.editMessage(...args);
	}

	async deleteMessage(...args: Parameters<MessageService['deleteMessage']>) {
		return this.messages.deleteMessage(...args);
	}

	async getMessage(...args: Parameters<MessageService['getMessage']>) {
		return this.messages.getMessage(...args);
	}

	async getMessages(...args: Parameters<MessageService['getMessages']>) {
		return this.messages.getMessages(...args);
	}

	async bulkDeleteMessages(...args: Parameters<MessageService['bulkDeleteMessages']>) {
		return this.messages.bulkDeleteMessages(...args);
	}

	async deleteUserMessagesInGuild(...args: Parameters<MessageService['deleteUserMessagesInGuild']>) {
		return this.messages.deleteUserMessagesInGuild(...args);
	}

	async sendJoinSystemMessage(...args: Parameters<MessageService['sendJoinSystemMessage']>) {
		return this.messages.sendJoinSystemMessage(...args);
	}

	async searchMessages(...args: Parameters<MessageService['searchMessages']>) {
		return this.messages.searchMessages(...args);
	}

	async anonymizeMessagesByAuthor(...args: Parameters<MessageService['anonymizeMessagesByAuthor']>) {
		return this.messages.anonymizeMessagesByAuthor(...args);
	}

	async addReaction(...args: Parameters<MessageInteractionService['addReaction']>) {
		return this.interactions.addReaction(...args);
	}

	async removeReaction(...args: Parameters<MessageInteractionService['removeReaction']>) {
		return this.interactions.removeReaction(...args);
	}

	async removeOwnReaction(...args: Parameters<MessageInteractionService['removeOwnReaction']>) {
		return this.interactions.removeOwnReaction(...args);
	}

	async removeAllReactions(...args: Parameters<MessageInteractionService['removeAllReactions']>) {
		return this.interactions.removeAllReactions(...args);
	}

	async removeAllReactionsForEmoji(...args: Parameters<MessageInteractionService['removeAllReactionsForEmoji']>) {
		return this.interactions.removeAllReactionsForEmoji(...args);
	}

	async getUsersForReaction(...args: Parameters<MessageInteractionService['getUsersForReaction']>) {
		return this.interactions.getUsersForReaction(...args);
	}

	async getMessageReactions(...args: Parameters<MessageInteractionService['getMessageReactions']>) {
		return this.interactions.getMessageReactions(...args);
	}

	async setHasReaction(...args: Parameters<MessageInteractionService['setHasReaction']>) {
		return this.interactions.setHasReaction(...args);
	}

	async pinMessage(...args: Parameters<MessageInteractionService['pinMessage']>) {
		return this.interactions.pinMessage(...args);
	}

	async unpinMessage(...args: Parameters<MessageInteractionService['unpinMessage']>) {
		return this.interactions.unpinMessage(...args);
	}

	async getChannelPins(...args: Parameters<MessageInteractionService['getChannelPins']>) {
		return this.interactions.getChannelPins(...args);
	}

	async startTyping(...args: Parameters<MessageInteractionService['startTyping']>) {
		return this.interactions.startTyping(...args);
	}

	async ackMessage(...args: Parameters<MessageInteractionService['ackMessage']>) {
		return this.interactions.ackMessage(...args);
	}

	async deleteReadState(...args: Parameters<MessageInteractionService['deleteReadState']>) {
		return this.interactions.deleteReadState(...args);
	}

	async ackPins(...args: Parameters<MessageInteractionService['ackPins']>) {
		return this.interactions.ackPins(...args);
	}

	async uploadFormDataAttachments(...args: Parameters<AttachmentUploadService['uploadFormDataAttachments']>) {
		return this.attachments.uploadFormDataAttachments(...args);
	}

	async deleteAttachment(...args: Parameters<AttachmentUploadService['deleteAttachment']>) {
		return this.attachments.deleteAttachment(...args);
	}

	async purgeChannelAttachments(...args: Parameters<AttachmentUploadService['purgeChannelAttachments']>) {
		return this.attachments.purgeChannelAttachments(...args);
	}

	async removeRecipientFromChannel(...args: Parameters<GroupDmService['removeRecipientFromChannel']>) {
		return this.groupDms.removeRecipientFromChannel(...args);
	}

	async updateGroupDmChannel(...args: Parameters<GroupDmService['updateGroupDmChannel']>) {
		return this.groupDms.updateGroupDmChannel(...args);
	}

	async checkCallEligibility(...args: Parameters<CallService['checkCallEligibility']>) {
		return this.calls.checkCallEligibility(...args);
	}

	async createOrGetCall(...args: Parameters<CallService['createOrGetCall']>) {
		return this.calls.createOrGetCall(...args);
	}

	async updateCall(...args: Parameters<CallService['updateCall']>) {
		return this.calls.updateCall(...args);
	}

	async ringCallRecipients(...args: Parameters<CallService['ringCallRecipients']>) {
		return this.calls.ringCallRecipients(...args);
	}

	async stopRingingCallRecipients(...args: Parameters<CallService['stopRingingCallRecipients']>) {
		return this.calls.stopRingingCallRecipients(...args);
	}

	async setChannelPermissionOverwrite(params: Parameters<ChannelDataService['setChannelPermissionOverwrite']>[0]) {
		return this.channelData.setChannelPermissionOverwrite(params);
	}

	async deleteChannelPermissionOverwrite(
		params: Parameters<ChannelDataService['deleteChannelPermissionOverwrite']>[0],
	) {
		return this.channelData.deleteChannelPermissionOverwrite(params);
	}
}
