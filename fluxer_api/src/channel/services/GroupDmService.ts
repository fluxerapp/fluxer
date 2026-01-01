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

import type {ChannelID, UserID} from '~/BrandedTypes';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IChannelRepository} from '../IChannelRepository';
import type {GroupDmUpdateService} from './channel_data/GroupDmUpdateService';
import {GroupDmOperationsService} from './group_dm/GroupDmOperationsService';
import type {MessagePersistenceService} from './message/MessagePersistenceService';

export class GroupDmService {
	private operationsService: GroupDmOperationsService;

	constructor(
		channelRepository: IChannelRepository,
		userRepository: IUserRepository,
		guildRepository: IGuildRepository,
		userCacheService: UserCacheService,
		gatewayService: IGatewayService,
		mediaService: IMediaService,
		snowflakeService: SnowflakeService,
		groupDmUpdateService: GroupDmUpdateService,
		messagePersistenceService: MessagePersistenceService,
	) {
		this.operationsService = new GroupDmOperationsService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			gatewayService,
			mediaService,
			snowflakeService,
			groupDmUpdateService,
			messagePersistenceService,
		);
	}

	async addRecipientToChannel(params: {
		userId: UserID;
		channelId: ChannelID;
		recipientId: UserID;
		requestCache: RequestCache;
	}): Promise<Channel> {
		return this.operationsService.addRecipientToChannel(params);
	}

	async addRecipientViaInvite(params: {
		channelId: ChannelID;
		recipientId: UserID;
		inviterId?: UserID | null;
		requestCache: RequestCache;
	}): Promise<Channel> {
		return this.operationsService.addRecipientViaInvite(params);
	}

	async removeRecipientFromChannel(params: {
		userId: UserID;
		channelId: ChannelID;
		recipientId: UserID;
		requestCache: RequestCache;
		silent?: boolean;
	}): Promise<void> {
		return this.operationsService.removeRecipientFromChannel(params);
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
		return this.operationsService.updateGroupDmChannel(params);
	}
}
