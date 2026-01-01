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

import {type ChannelID, createMessageID, type UserID} from '~/BrandedTypes';
import {ChannelTypes, MessageTypes} from '~/Constants';
import {
	InvalidChannelTypeError,
	MissingAccessError,
	MissingPermissionsError,
	UnknownChannelError,
	UnknownUserError,
} from '~/Errors';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {Channel} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessagePersistenceService} from '../message/MessagePersistenceService';
import type {ChannelUtilsService} from './ChannelUtilsService';

export class GroupDmUpdateService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private avatarService: AvatarService,
		private snowflakeService: SnowflakeService,
		private channelUtilsService: ChannelUtilsService,
		private messagePersistenceService: MessagePersistenceService,
	) {}

	async updateGroupDmChannel({
		userId,
		channelId,
		name,
		icon,
		ownerId,
		nicks,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		name?: string | null;
		icon?: string | null;
		ownerId?: UserID;
		nicks?: Record<string, string | null> | null;
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

		const updates: Partial<ReturnType<Channel['toRow']>> = {};

		if (ownerId !== undefined) {
			if (channel.ownerId !== userId) {
				throw new MissingPermissionsError();
			}

			if (!channel.recipientIds.has(ownerId)) {
				throw new UnknownUserError();
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
			const messageId = createMessageID(this.snowflakeService.generate());
			const message = await this.messagePersistenceService.createSystemMessage({
				messageId,
				channelId,
				userId,
				type: MessageTypes.CHANNEL_NAME_CHANGE,
				content: name,
			});

			await this.channelUtilsService.dispatchMessageCreate({
				channel: updatedChannel,
				message,
				requestCache,
			});
		}

		if (icon !== undefined && iconHash !== channel.iconHash) {
			const messageId = createMessageID(this.snowflakeService.generate());
			const message = await this.messagePersistenceService.createSystemMessage({
				messageId,
				channelId,
				userId,
				type: MessageTypes.CHANNEL_ICON_CHANGE,
				content: iconHash,
			});

			await this.channelUtilsService.dispatchMessageCreate({
				channel: updatedChannel,
				message,
				requestCache,
			});
		}

		await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChannel, requestCache});

		return updatedChannel;
	}
}
