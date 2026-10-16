// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {createMessageID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelUtilsService} from '@app/api/channel/services/channel_data/ChannelUtilsService';
import {requireGroupDmMember} from '@app/api/channel/services/group_dm/GroupDmHelpers';
import {dispatchMessageCreateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {GroupDmMatureContentIneligibleError} from '@fluxer/errors/src/domains/channel/GroupDmMatureContentIneligibleError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {CannotTransferOwnershipToBotError} from '@fluxer/errors/src/domains/guild/CannotTransferOwnershipToBotError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';

export class GroupDmUpdateService {
	constructor(
		private channelRepository: ChannelRepository,
		private userRepository: UserRepository,
		private avatarService: AvatarService,
		private snowflakeService: ISnowflakeService,
		private channelUtilsService: ChannelUtilsService,
		private messagePersistenceService: MessagePersistenceService,
		private gatewayService: IGatewayService,
	) {}

	async updateGroupDmChannel({
		userId,
		channelId,
		name,
		icon,
		ownerId,
		nicks,
		nsfw,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		name?: string | null;
		icon?: string | null;
		ownerId?: UserID;
		nicks?: Record<string, string | null> | null;
		nsfw?: boolean;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const channel = await requireGroupDmMember(this.channelRepository, channelId, userId);
		const dmModCtx = {
			userId,
			guildId: null,
			channelId,
			messageId: null,
			surface: 'profile_field' as const,
		};
		contentModerationService.scanText(name ?? null, dmModCtx);
		if (nicks && typeof nicks === 'object') {
			for (const nickValue of Object.values(nicks)) {
				contentModerationService.scanText(nickValue, dmModCtx);
			}
		}
		const updates: Partial<ChannelRow> = {};
		if (ownerId !== undefined) {
			if (channel.ownerId !== userId) {
				throw new MissingPermissionsError();
			}
			if (!channel.recipientIds.has(ownerId)) {
				throw new UnknownUserError();
			}
			const newOwnerUser = await this.userRepository.findUnique(ownerId);
			if (newOwnerUser?.isBot) {
				throw new CannotTransferOwnershipToBotError();
			}
			updates.owner_id = ownerId;
		}
		if (name !== undefined) {
			updates.name = name;
		}
		if (nicks !== undefined) {
			if (nicks === null) {
				if (channel.ownerId !== userId) {
					throw new MissingPermissionsError();
				}
				updates.nicks = null;
			} else {
				const isOwner = channel.ownerId === userId;
				for (const targetUserId of Object.keys(nicks)) {
					const targetUserIdBigInt = BigInt(targetUserId) as UserID;
					if (!channel.recipientIds.has(targetUserIdBigInt) && targetUserIdBigInt !== userId) {
						throw new UnknownUserError();
					}
					if (!isOwner && targetUserId !== userId.toString()) {
						throw new MissingPermissionsError();
					}
				}
				const updatedNicknames = new Map(channel.nicknames);
				for (const [targetUserId, nickname] of Object.entries(nicks)) {
					if (nickname === null || nickname.trim() === '') {
						updatedNicknames.delete(targetUserId);
					} else {
						updatedNicknames.set(targetUserId, nickname.trim());
					}
				}
				updates.nicks = updatedNicknames.size > 0 ? (updatedNicknames as Map<string, string>) : null;
			}
		}
		if (nsfw !== undefined && nsfw !== channel.isNsfw) {
			if (channel.ownerId !== userId) {
				throw new MissingPermissionsError();
			}
			if (nsfw) {
				const recipients = await Promise.all(
					Array.from(channel.recipientIds).map((id) => this.userRepository.findUnique(id)),
				);
				if (recipients.some((recipient) => !recipient || !canUserAccessNsfwContent(recipient))) {
					throw new GroupDmMatureContentIneligibleError();
				}
			}
			updates.nsfw = nsfw;
		}
		let iconHash: string | null = null;
		if (icon !== undefined) {
			iconHash = await this.avatarService.uploadAvatar({
				prefix: 'icons',
				entityId: channelId,
				errorPath: 'icon',
				previousKey: channel.iconHash,
				base64Image: icon,
			});
			updates.icon_hash = iconHash;
		}
		const updatedChannel = await this.channelRepository.channelData.upsert({
			...channel.toRow(),
			...updates,
		});
		if (name !== undefined && name !== channel.name) {
			const messageId = createMessageID(await this.snowflakeService.generateForChannel(channelId));
			const message = await this.messagePersistenceService.createSystemMessage({
				messageId,
				channelId,
				userId,
				type: MessageTypes.CHANNEL_NAME_CHANGE,
				content: name,
			});
			await dispatchMessageCreateBroadcast({gatewayService: this.gatewayService, channel: updatedChannel, message});
		}
		if (icon !== undefined && iconHash !== channel.iconHash) {
			const messageId = createMessageID(await this.snowflakeService.generateForChannel(channelId));
			const message = await this.messagePersistenceService.createSystemMessage({
				messageId,
				channelId,
				userId,
				type: MessageTypes.CHANNEL_ICON_CHANGE,
				content: iconHash,
			});
			await dispatchMessageCreateBroadcast({gatewayService: this.gatewayService, channel: updatedChannel, message});
		}
		await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChannel, requestCache});
		return updatedChannel;
	}
}
