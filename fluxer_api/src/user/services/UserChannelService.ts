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

import {type ChannelID, createChannelID, createMessageID, createUserID, type UserID} from '~/BrandedTypes';
import {ChannelTypes, MAX_GROUP_DMS_PER_USER, MessageTypes, RelationshipTypes} from '~/Constants';
import {mapChannelToResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import {dispatchMessageCreate} from '~/channel/services/group_dm/GroupDmHelpers';
import {
	CannotSendMessagesToUserError,
	InputValidationError,
	MaxGroupDmRecipientsError,
	MaxGroupDmsError,
	MissingAccessError,
	NotFriendsWithUserError,
	UnclaimedAccountRestrictedError,
	UnknownUserError,
} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel, Message, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {CreatePrivateChannelRequest} from '~/user/UserModel';
import * as BucketUtils from '~/utils/BucketUtils';
import type {UserPermissionUtils} from '~/utils/UserPermissionUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';

export class UserChannelService {
	constructor(
		private userAccountRepository: IUserAccountRepository,
		private userChannelRepository: IUserChannelRepository,
		private userRelationshipRepository: IUserRelationshipRepository,
		private channelService: ChannelService,
		private channelRepository: IChannelRepository,
		private gatewayService: IGatewayService,
		private mediaService: IMediaService,
		private snowflakeService: SnowflakeService,
		private userPermissionUtils: UserPermissionUtils,
	) {}

	async getPrivateChannels(userId: UserID): Promise<Array<Channel>> {
		return await this.userChannelRepository.listPrivateChannels(userId);
	}

	async createOrOpenDMChannel({
		userId,
		data,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		data: CreatePrivateChannelRequest;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		if (data.recipients !== undefined) {
			return await this.createGroupDMChannel({
				userId,
				recipients: data.recipients,
				userCacheService,
				requestCache,
			});
		}

		const recipientId = createUserID(data.recipient_id!);
		if (userId === recipientId) {
			throw InputValidationError.create('recipient_id', 'Cannot DM yourself');
		}
		const targetUser = await this.userAccountRepository.findUnique(recipientId);
		if (!targetUser) throw new UnknownUserError();

		await this.validateDmPermission(userId, recipientId, targetUser);

		const existingChannel = await this.userChannelRepository.findExistingDmState(userId, recipientId);
		if (existingChannel) {
			return await this.reopenExistingDMChannel({userId, existingChannel, userCacheService, requestCache});
		}
		const channel = await this.createNewDMChannel({userId, recipientId, userCacheService, requestCache});
		return channel;
	}

	async pinDmChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		const channel = await this.channelService.getChannel({userId, channelId});
		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw InputValidationError.create('channel_id', 'Channel must be a DM or group DM');
		}
		if (!channel.recipientIds.has(userId)) {
			throw new MissingAccessError();
		}
		const newPinnedDMs = await this.userChannelRepository.addPinnedDm(userId, channelId);
		await this.gatewayService.dispatchPresence({
			userId: userId,
			event: 'USER_PINNED_DMS_UPDATE',
			data: newPinnedDMs.map(String),
		});
	}

	async unpinDmChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		const channel = await this.channelService.getChannel({userId, channelId});
		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw InputValidationError.create('channel_id', 'Channel must be a DM or group DM');
		}
		if (!channel.recipientIds.has(userId)) {
			throw new MissingAccessError();
		}
		const newPinnedDMs = await this.userChannelRepository.removePinnedDm(userId, channelId);
		await this.gatewayService.dispatchPresence({
			userId: userId,
			event: 'USER_PINNED_DMS_UPDATE',
			data: newPinnedDMs.map(String),
		});
	}

	async preloadDMMessages(params: {
		userId: UserID;
		channelIds: Array<ChannelID>;
	}): Promise<Record<string, Message | null>> {
		const {userId, channelIds} = params;
		if (channelIds.length > 100) {
			throw InputValidationError.create('channels', 'Cannot preload more than 100 channels at once');
		}

		const results: Record<string, Message | null> = {};
		const fetchPromises = channelIds.map(async (channelId) => {
			try {
				const channel = await this.channelService.getChannel({userId, channelId});
				if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
					return;
				}
				if (!channel.recipientIds.has(userId)) {
					return;
				}
				const messages = await this.channelService.getMessages({
					userId,
					channelId,
					limit: 1,
					before: undefined,
					after: undefined,
					around: undefined,
				});
				results[channelId.toString()] = messages[0] ?? null;
			} catch {
				results[channelId.toString()] = null;
			}
		});

		await Promise.all(fetchPromises);
		return results;
	}

	async getExistingDmForUsers(userId: UserID, recipientId: UserID): Promise<Channel | null> {
		return await this.userChannelRepository.findExistingDmState(userId, recipientId);
	}

	async ensureDmOpenForBothUsers({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const existingChannel = await this.userChannelRepository.findExistingDmState(userId, recipientId);

		if (existingChannel) {
			const [isUserOpen, isRecipientOpen] = await Promise.all([
				this.userChannelRepository.isDmChannelOpen(userId, existingChannel.id),
				this.userChannelRepository.isDmChannelOpen(recipientId, existingChannel.id),
			]);

			if (!isUserOpen) {
				await this.userChannelRepository.openDmForUser(userId, existingChannel.id);
				await this.dispatchChannelCreate({userId, channel: existingChannel, userCacheService, requestCache});
			}

			if (!isRecipientOpen) {
				await this.userChannelRepository.openDmForUser(recipientId, existingChannel.id);
				await this.dispatchChannelCreate({
					userId: recipientId,
					channel: existingChannel,
					userCacheService,
					requestCache,
				});
			}

			return existingChannel;
		}

		return await this.createNewDmForBothUsers({userId, recipientId, userCacheService, requestCache});
	}

	async reopenDmForBothUsers({
		userId,
		recipientId,
		existingChannel,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		existingChannel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.reopenExistingDMChannel({userId, existingChannel, userCacheService, requestCache});
		await this.reopenExistingDMChannel({
			userId: recipientId,
			existingChannel,
			userCacheService,
			requestCache,
		});
	}

	async createNewDmForBothUsers({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const newChannel = await this.createNewDMChannel({
			userId,
			recipientId,
			userCacheService,
			requestCache,
		});
		await this.userChannelRepository.openDmForUser(recipientId, newChannel.id);
		await this.dispatchChannelCreate({userId: recipientId, channel: newChannel, userCacheService, requestCache});
		return newChannel;
	}

	private async reopenExistingDMChannel({
		userId,
		existingChannel,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		existingChannel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		await this.userChannelRepository.openDmForUser(userId, existingChannel.id);
		await this.dispatchChannelCreate({userId, channel: existingChannel, userCacheService, requestCache});
		return existingChannel;
	}

	private async createNewDMChannel({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const channelId = createChannelID(this.snowflakeService.generate());
		const newChannel = await this.userChannelRepository.createDmChannelAndState(userId, recipientId, channelId);
		await this.userChannelRepository.openDmForUser(userId, channelId);
		await this.dispatchChannelCreate({userId, channel: newChannel, userCacheService, requestCache});
		return newChannel;
	}

	private async createGroupDMChannel({
		userId,
		recipients,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipients: Array<bigint>;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		if (recipients.length > 9) {
			throw new MaxGroupDmRecipientsError();
		}

		const recipientIds = recipients.map(createUserID);
		const uniqueRecipientIds = new Set(recipientIds);
		if (uniqueRecipientIds.size !== recipientIds.length) {
			throw InputValidationError.create('recipients', 'Duplicate recipients are not allowed');
		}

		if (uniqueRecipientIds.has(userId)) {
			throw InputValidationError.create('recipients', 'Cannot add yourself to a group DM');
		}

		const usersToCheck = new Set<UserID>([userId, ...recipientIds]);
		await this.ensureUsersWithinGroupDmLimit(usersToCheck);

		for (const recipientId of recipientIds) {
			const targetUser = await this.userAccountRepository.findUnique(recipientId);
			if (!targetUser) {
				throw new UnknownUserError();
			}

			const friendship = await this.userRelationshipRepository.getRelationship(
				userId,
				recipientId,
				RelationshipTypes.FRIEND,
			);
			if (!friendship) {
				throw new NotFriendsWithUserError();
			}

			await this.userPermissionUtils.validateGroupDmAddPermissions({userId, targetId: recipientId});
		}

		const channelId = createChannelID(this.snowflakeService.generate());
		const allRecipients = new Set([userId, ...recipientIds]);

		const channelData = {
			channel_id: channelId,
			guild_id: null,
			type: ChannelTypes.GROUP_DM,
			name: null,
			topic: null,
			icon_hash: null,
			url: null,
			parent_id: null,
			position: 0,
			owner_id: userId,
			recipient_ids: allRecipients,
			nsfw: false,
			rate_limit_per_user: 0,
			bitrate: null,
			user_limit: null,
			rtc_region: null,
			last_message_id: null,
			last_pin_timestamp: null,
			permission_overwrites: null,
			nicks: null,
			soft_deleted: false,
			indexed_at: null,
			version: 1,
		};

		const newChannel = await this.channelRepository.upsert(channelData);

		for (const recipientId of allRecipients) {
			await this.userChannelRepository.openDmForUser(recipientId, channelId);
		}

		const systemMessages: Array<Message> = [];
		for (const recipientId of recipientIds) {
			const messageId = createMessageID(this.snowflakeService.generate());
			const message = await this.channelRepository.upsertMessage({
				channel_id: channelId,
				bucket: BucketUtils.makeBucket(messageId),
				message_id: messageId,
				author_id: userId,
				type: MessageTypes.RECIPIENT_ADD,
				webhook_id: null,
				webhook_name: null,
				webhook_avatar_hash: null,
				content: null,
				edited_timestamp: null,
				pinned_timestamp: null,
				flags: 0,
				mention_everyone: false,
				mention_users: new Set([recipientId]),
				mention_roles: null,
				mention_channels: null,
				attachments: null,
				embeds: null,
				sticker_items: null,
				message_reference: null,
				message_snapshots: null,
				call: null,
				has_reaction: false,
				version: 1,
			});
			systemMessages.push(message);
		}

		for (const recipientId of allRecipients) {
			await this.dispatchChannelCreate({userId: recipientId, channel: newChannel, userCacheService, requestCache});
		}

		for (const message of systemMessages) {
			await this.dispatchSystemMessage({
				channel: newChannel,
				message,
				userCacheService,
				requestCache,
			});
		}

		return newChannel;
	}

	private async dispatchSystemMessage({
		channel,
		message,
		userCacheService,
		requestCache,
	}: {
		channel: Channel;
		message: Message;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		await dispatchMessageCreate({
			channel,
			message,
			requestCache,
			userCacheService,
			gatewayService: this.gatewayService,
			mediaService: this.mediaService,
			getReferencedMessage: (channelId, messageId) => this.channelRepository.getMessage(channelId, messageId),
		});
	}

	private async dispatchChannelCreate({
		userId,
		channel,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		channel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const channelResponse = await mapChannelToResponse({
			channel,
			currentUserId: userId,
			userCacheService,
			requestCache,
		});
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'CHANNEL_CREATE',
			data: channelResponse,
		});
	}

	private async ensureUsersWithinGroupDmLimit(userIds: Iterable<UserID>): Promise<void> {
		for (const userId of userIds) {
			await this.ensureUserWithinGroupDmLimit(userId);
		}
	}

	private async ensureUserWithinGroupDmLimit(userId: UserID): Promise<void> {
		const summaries = await this.userChannelRepository.listPrivateChannelSummaries(userId);
		const openGroupDms = summaries.filter((summary) => summary.open && summary.isGroupDm).length;
		if (openGroupDms >= MAX_GROUP_DMS_PER_USER) {
			throw new MaxGroupDmsError();
		}
	}

	private async validateDmPermission(userId: UserID, recipientId: UserID, _recipientUser?: User | null): Promise<void> {
		const senderUser = await this.userAccountRepository.findUnique(userId);
		if (senderUser && senderUser.isUnclaimedAccount()) {
			throw new UnclaimedAccountRestrictedError('send direct messages');
		}

		const userBlockedRecipient = await this.userRelationshipRepository.getRelationship(
			userId,
			recipientId,
			RelationshipTypes.BLOCKED,
		);
		if (userBlockedRecipient) {
			throw new CannotSendMessagesToUserError();
		}

		const recipientBlockedUser = await this.userRelationshipRepository.getRelationship(
			recipientId,
			userId,
			RelationshipTypes.BLOCKED,
		);
		if (recipientBlockedUser) {
			throw new CannotSendMessagesToUserError();
		}

		const friendship = await this.userRelationshipRepository.getRelationship(
			userId,
			recipientId,
			RelationshipTypes.FRIEND,
		);
		if (friendship) return;

		const hasMutualGuilds = await this.userPermissionUtils.checkMutualGuildsAsync({
			userId,
			targetId: recipientId,
		});
		if (hasMutualGuilds) return;

		throw new CannotSendMessagesToUserError();
	}
}
