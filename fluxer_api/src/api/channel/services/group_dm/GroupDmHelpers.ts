// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';

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
