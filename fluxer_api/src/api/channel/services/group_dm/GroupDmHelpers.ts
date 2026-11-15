// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';

export async function dispatchChannelDelete({
	channel,
	requestCache,
	userCacheService,
	gatewayService,
}: {
	channel: Channel;
	requestCache: RequestCache;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
}): Promise<void> {
	const channelResponse = await mapChannelToResponse({
		channel,
		currentUserId: null,
		userCacheService,
		requestCache,
	});
	await dispatchChannelEvent({
		channel,
		event: 'CHANNEL_DELETE',
		data: channelResponse,
		gatewayService,
	});
}

export async function dispatchDmChannelDelete({
	gatewayService,
	userCacheService,
	channel,
	userId,
	requestCache,
}: {
	gatewayService: IGatewayService;
	userCacheService: UserCacheService;
	channel: Channel;
	userId: UserID;
	requestCache: RequestCache;
}): Promise<void> {
	await gatewayService.dispatchPresence({
		userId,
		event: 'CHANNEL_DELETE',
		data: await mapChannelToResponse({
			channel,
			currentUserId: null,
			userCacheService: userCacheService,
			requestCache,
		}),
	});
}

export async function requireGroupDmMember(
	channelRepository: ChannelRepository,
	channelId: ChannelID,
	userId: UserID,
): Promise<Channel> {
	const channel = await channelRepository.channelData.findUnique(channelId);
	if (!channel) throw new UnknownChannelError();
	if (channel.type !== ChannelTypes.GROUP_DM) throw new InvalidChannelTypeError();
	if (!channel.recipientIds.has(userId)) throw new MissingAccessError();
	return channel;
}
