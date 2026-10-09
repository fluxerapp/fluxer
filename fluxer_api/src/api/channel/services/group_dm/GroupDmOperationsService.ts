// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {createMessageID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import {dispatchChannelDelete} from '@app/api/channel/services/group_dm/GroupDmHelpers';
import {dispatchMessageCreateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {User} from '@app/api/models/User';
import {deleteChannelMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import {UserPermissionUtils} from '@app/api/utils/UserPermissionUtils';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GROUP_DM_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {MaxGroupDmRecipientsError} from '@fluxer/errors/src/domains/channel/MaxGroupDmRecipientsError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {CannotRemoveOtherRecipientsError} from '@fluxer/errors/src/domains/core/CannotRemoveOtherRecipientsError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import {NotFriendsWithUserError} from '@fluxer/errors/src/domains/user/NotFriendsWithUserError';

export class GroupDmOperationsService {
	private readonly userPermissionUtils: UserPermissionUtils;

	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		guildRepository: IGuildRepositoryAggregate,
		private userCacheService: UserCacheService,
		private gatewayService: IGatewayService,
		private snowflakeService: ISnowflakeService,
		private messagePersistenceService: MessagePersistenceService,
		private readonly limitConfigService: LimitConfigService,
		private readonly voiceRoomStore: IVoiceRoomStore,
		private readonly liveKitService: ILiveKitService,
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
		await this.assertInviterNotLimited(userId);
		const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
		if (!friendship) {
			throw new NotFriendsWithUserError();
		}
		await this.userPermissionUtils.validateGroupDmAddPermissions({
			userId,
			targetId: recipientId,
		});
		if (channel.isNsfw && !(await this.canJoinMatureGroup(recipientId))) {
			throw new MissingAccessError();
		}
		const {channel: updatedChannel} = await this.addRecipientViaInviteWithResult({
			channelId,
			recipientId,
			inviterId: userId,
			requestCache,
		});
		return updatedChannel;
	}

	async addBotRecipientToChannel({
		userId,
		channelId,
		botUserId,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		botUserId: UserID;
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
		await this.assertInviterNotLimited(userId);
		const {channel: updatedChannel} = await this.addRecipientViaInviteWithResult({
			channelId,
			recipientId: botUserId,
			inviterId: userId,
			requestCache,
		});
		return updatedChannel;
	}

	private async canJoinMatureGroup(userId: UserID): Promise<boolean> {
		const user = await this.userRepository.findUnique(userId);
		return user != null && canUserAccessNsfwContent(user);
	}

	private async assertInviterNotLimited(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (user) assertAccountNotLimited(user);
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
		const {channel} = await this.addRecipientViaInviteWithResult({
			channelId,
			recipientId,
			inviterId,
			requestCache,
		});
		return channel;
	}

	private async addRecipientViaInviteWithResult({
		channelId,
		recipientId,
		inviterId,
		requestCache,
	}: {
		channelId: ChannelID;
		recipientId: UserID;
		inviterId?: UserID | null;
		requestCache: RequestCache;
	}): Promise<{
		channel: Channel;
		recipientAdded: boolean;
	}> {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();
		if (channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeError();
		}
		if (channel.recipientIds.has(recipientId)) {
			return {channel, recipientAdded: false};
		}
		if (channel.isNsfw && !(await this.canJoinMatureGroup(recipientId))) {
			throw new NsfwContentRequiresAgeVerificationError();
		}
		const inviterUser = inviterId ? await this.userRepository.findUnique(inviterId) : null;
		const fallbackLimit = MAX_GROUP_DM_RECIPIENTS;
		const recipientLimit = this.resolveLimitForUser(inviterUser ?? null, 'max_group_dm_recipients', fallbackLimit);
		if (channel.recipientIds.size >= recipientLimit) {
			throw new MaxGroupDmRecipientsError(recipientLimit);
		}
		const updatedRecipientIds = new Set([...channel.recipientIds, recipientId]);
		const updatedChannel = await this.channelRepository.channelData.upsert({
			...channel.toRow(),
			recipient_ids: updatedRecipientIds,
		});
		await this.userRepository.openPrivateChannelForUser(recipientId, updatedChannel);
		const recipientUserResponse = await this.userCacheService.getUserPartialResponse(recipientId, requestCache);
		const messageId = createMessageID(await this.snowflakeService.generateForChannel(channelId));
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
		await dispatchMessageCreateBroadcast({
			gatewayService: this.gatewayService,
			channel: updatedChannel,
			message: systemMessage,
		});
		await Promise.all(
			Array.from(updatedRecipientIds).map(async (recId) => {
				await this.syncGroupDmRecipientsForUser(recId);
			}),
		);
		return {channel: updatedChannel, recipientAdded: true};
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
			throw InputValidationError.fromCode('user_id', ValidationErrorCodes.USER_NOT_IN_CHANNEL);
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
			await deleteChannelMessageSearchDocuments(channelId, {context: {source: 'group_dm_delete'}});
			await this.channelRepository.channelData.delete(channelId);
			await this.userRepository.closeDmForUser(recipientId, channelId);
			await this.disconnectRemovedRecipientFromCall(channelId, recipientId);
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
		await this.disconnectRemovedRecipientFromCall(channelId, recipientId);
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
			const messageId = createMessageID(await this.snowflakeService.generateForChannel(channelId));
			const systemMessage = await this.messagePersistenceService.createSystemMessage({
				messageId,
				channelId,
				userId,
				type: MessageTypes.RECIPIENT_REMOVE,
				mentionUserIds: [recipientId],
			});
			await dispatchMessageCreateBroadcast({
				gatewayService: this.gatewayService,
				channel: updatedChannel,
				message: systemMessage,
			});
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

	private async disconnectRemovedRecipientFromCall(channelId: ChannelID, recipientId: UserID): Promise<void> {
		try {
			const {voiceStates} = await this.gatewayService.getVoiceStatesForChannel({channelId});
			await this.gatewayService.disconnectVoiceUserIfInChannel({channelId, userId: recipientId});
			const recipientVoiceStates = voiceStates.filter((voiceState) => voiceState.userId === recipientId.toString());
			if (recipientVoiceStates.length === 0) return;
			const pinnedServer = await this.voiceRoomStore.getPinnedRoomServer(undefined, channelId);
			if (!pinnedServer) return;
			for (const voiceState of recipientVoiceStates) {
				await this.liveKitService.disconnectParticipant({
					userId: recipientId,
					channelId,
					connectionId: voiceState.connectionId,
					regionId: pinnedServer.regionId,
					serverId: pinnedServer.serverId,
				});
			}
		} catch (error) {
			Logger.error(
				{error, channelId: channelId.toString(), userId: recipientId.toString()},
				'Failed to disconnect removed group DM recipient from call',
			);
		}
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

	private resolveLimitForUser(user: User | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
