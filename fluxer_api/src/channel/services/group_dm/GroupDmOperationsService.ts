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

import {randomInt} from 'node:crypto';
import {type ChannelID, createMessageID, type UserID} from '~/BrandedTypes';
import {ChannelTypes, MAX_GROUP_DM_RECIPIENTS, MessageTypes, RelationshipTypes} from '~/Constants';
import {mapChannelToResponse, mapMessageToResponse} from '~/channel/ChannelModel';
import {
	CannotRemoveOtherRecipientsError,
	InputValidationError,
	InvalidChannelTypeError,
	MaxGroupDmRecipientsError,
	MissingAccessError,
	NotFriendsWithUserError,
	UnknownChannelError,
} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import {UserPermissionUtils} from '~/utils/UserPermissionUtils';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {GroupDmUpdateService} from '../channel_data/GroupDmUpdateService';
import type {MessagePersistenceService} from '../message/MessagePersistenceService';
import {dispatchChannelDelete} from './GroupDmHelpers';

export class GroupDmOperationsService {
	private readonly userPermissionUtils: UserPermissionUtils;

	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		guildRepository: IGuildRepository,
		private userCacheService: UserCacheService,
		private gatewayService: IGatewayService,
		private mediaService: IMediaService,
		private snowflakeService: SnowflakeService,
		private groupDmUpdateService: GroupDmUpdateService,
		private messagePersistenceService: MessagePersistenceService,
	) {
		this.userPermissionUtils = new UserPermissionUtils(userRepository, guildRepository);
	}

	async addRecipientToChannel({
		userId,
		channelId,
		recipientId,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		recipientId: UserID;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeError();
		}

		if (!channel.recipientIds.has(userId)) {
			throw new MissingAccessError();
		}

		const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
		if (!friendship) {
			throw new NotFriendsWithUserError();
		}

		await this.userPermissionUtils.validateGroupDmAddPermissions({
			userId,
			targetId: recipientId,
		});

		return this.addRecipientViaInvite({
			channelId,
			recipientId,
			inviterId: userId,
			requestCache,
		});
	}

	async addRecipientViaInvite({
		channelId,
		recipientId,
		inviterId,
		requestCache,
	}: {
		channelId: ChannelID;
		recipientId: UserID;
		inviterId?: UserID | null;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeError();
		}

		if (channel.recipientIds.has(recipientId)) {
			return channel;
		}

		if (channel.recipientIds.size >= MAX_GROUP_DM_RECIPIENTS) {
			throw new MaxGroupDmRecipientsError();
		}

		const updatedRecipientIds = new Set([...channel.recipientIds, recipientId]);
		const updatedChannel = await this.channelRepository.channelData.upsert({
			...channel.toRow(),
			recipient_ids: updatedRecipientIds,
		});

		await this.userRepository.openDmForUser(recipientId, channelId);

		const recipientUserResponse = await this.userCacheService.getUserPartialResponse(recipientId, requestCache);

		const messageId = createMessageID(this.snowflakeService.generate());
		const systemMessage = await this.messagePersistenceService.createSystemMessage({
			messageId,
			channelId,
			userId: inviterId ?? channel.ownerId ?? recipientId,
			type: MessageTypes.RECIPIENT_ADD,
			mentionUserIds: [recipientId],
		});

		const channelResponse = await mapChannelToResponse({
			channel: updatedChannel,
			currentUserId: recipientId,
			userCacheService: this.userCacheService,
			requestCache,
		});

		await this.gatewayService.dispatchPresence({
			userId: recipientId,
			event: 'CHANNEL_CREATE',
			data: channelResponse,
		});

		for (const recId of channel.recipientIds) {
			await this.gatewayService.dispatchPresence({
				userId: recId,
				event: 'CHANNEL_RECIPIENT_ADD',
				data: {
					channel_id: channelId.toString(),
					user: recipientUserResponse,
				},
			});
		}

		const messageResponse = await mapMessageToResponse({
			message: systemMessage,
			userCacheService: this.userCacheService,
			requestCache,
			mediaService: this.mediaService,
			getReferencedMessage: (channelId, messageId) => this.channelRepository.messages.getMessage(channelId, messageId),
		});

		for (const recipientId of updatedRecipientIds) {
			await this.gatewayService.dispatchPresence({
				userId: recipientId,
				event: 'MESSAGE_CREATE',
				data: {...messageResponse, channel_type: updatedChannel.type},
			});
		}

		await Promise.all(
			Array.from(updatedRecipientIds).map(async (recId) => {
				await this.syncGroupDmRecipientsForUser(recId);
			}),
		);

		return updatedChannel;
	}

	async removeRecipientFromChannel({
		userId,
		channelId,
		recipientId,
		requestCache,
		silent,
	}: {
		userId: UserID;
		channelId: ChannelID;
		recipientId: UserID;
		requestCache: RequestCache;
		silent?: boolean;
	}): Promise<void> {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeError();
		}

		if (!channel.recipientIds.has(userId)) {
			throw new MissingAccessError();
		}

		if (!channel.recipientIds.has(recipientId)) {
			throw InputValidationError.create('user_id', 'User is not in this channel');
		}

		if (recipientId !== userId && channel.ownerId !== userId) {
			throw new CannotRemoveOtherRecipientsError();
		}

		const updatedRecipientIds = new Set(channel.recipientIds);
		updatedRecipientIds.delete(recipientId);

		let newOwnerId = channel.ownerId;
		if (recipientId === channel.ownerId && updatedRecipientIds.size > 0) {
			const remaining = Array.from(updatedRecipientIds);
			newOwnerId = remaining[randomInt(remaining.length)] as UserID;
		}

		if (updatedRecipientIds.size === 0) {
			await this.channelRepository.messages.deleteAllChannelMessages(channelId);
			await this.channelRepository.channelData.delete(channelId);
			await this.userRepository.closeDmForUser(recipientId, channelId);

			await dispatchChannelDelete({
				channel,
				requestCache,
				userCacheService: this.userCacheService,
				gatewayService: this.gatewayService,
			});
			return;
		}

		const updatedNicknames = new Map(channel.nicknames);
		updatedNicknames.delete(recipientId.toString());

		const updatedChannel = await this.channelRepository.channelData.upsert({
			...channel.toRow(),
			owner_id: newOwnerId,
			recipient_ids: updatedRecipientIds,
			nicks: updatedNicknames.size > 0 ? updatedNicknames : null,
		});

		await this.userRepository.closeDmForUser(recipientId, channelId);

		const recipientUserResponse = await this.userCacheService.getUserPartialResponse(recipientId, requestCache);
		for (const recId of updatedRecipientIds) {
			await this.gatewayService.dispatchPresence({
				userId: recId,
				event: 'CHANNEL_RECIPIENT_REMOVE',
				data: {
					channel_id: channelId.toString(),
					user: recipientUserResponse,
				},
			});
		}

		if (!silent) {
			const messageId = createMessageID(this.snowflakeService.generate());
			const systemMessage = await this.messagePersistenceService.createSystemMessage({
				messageId,
				channelId,
				userId,
				type: MessageTypes.RECIPIENT_REMOVE,
				mentionUserIds: [recipientId],
			});

			const messageResponse = await mapMessageToResponse({
				message: systemMessage,
				userCacheService: this.userCacheService,
				requestCache,
				mediaService: this.mediaService,
				getReferencedMessage: (channelId, messageId) =>
					this.channelRepository.messages.getMessage(channelId, messageId),
			});

			for (const remainingRecipientId of updatedRecipientIds) {
				await this.gatewayService.dispatchPresence({
					userId: remainingRecipientId,
					event: 'MESSAGE_CREATE',
					data: {...messageResponse, channel_type: updatedChannel.type},
				});
			}
		}

		const channelResponse = await mapChannelToResponse({
			channel,
			currentUserId: null,
			userCacheService: this.userCacheService,
			requestCache,
		});
		await this.gatewayService.dispatchPresence({
			userId: recipientId,
			event: 'CHANNEL_DELETE',
			data: channelResponse,
		});

		await Promise.all(
			[...Array.from(updatedRecipientIds), recipientId].map(async (recId) => {
				await this.syncGroupDmRecipientsForUser(recId);
			}),
		);
	}

	async updateGroupDmChannel(params: {
		userId: UserID;
		channelId: ChannelID;
		name?: string;
		icon?: string | null;
		ownerId?: UserID;
		nicks?: Record<string, string | null> | null;
		requestCache: RequestCache;
	}): Promise<Channel> {
		return this.groupDmUpdateService.updateGroupDmChannel(params);
	}

	private async syncGroupDmRecipientsForUser(userId: UserID): Promise<void> {
		const channels = await this.userRepository.listPrivateChannels(userId);
		const groupDmChannels = channels.filter((ch) => ch.type === ChannelTypes.GROUP_DM);

		const recipientsByChannel: Record<string, Array<string>> = {};
		for (const channel of groupDmChannels) {
			const otherRecipients = Array.from(channel.recipientIds)
				.filter((recId) => recId !== userId)
				.map((recId) => recId.toString());

			if (otherRecipients.length > 0) {
				recipientsByChannel[channel.id.toString()] = otherRecipients;
			}
		}

		await this.gatewayService.syncGroupDmRecipients({
			userId,
			recipientsByChannel,
		});
	}
}
