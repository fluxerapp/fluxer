// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {SYSTEM_THREAD_VIEWER} from '@app/api/experiment/ChannelThreadsGate';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import type {UserChannelService} from '@app/api/user/services/UserChannelService';

export async function sendSystemDmMessage(params: {
	systemUser: User;
	recipientId: UserID;
	content: string;
	userChannelService: Pick<UserChannelService, 'ensureDmOpenForBothUsers'>;
	channelService: Pick<ChannelService, 'messages'>;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
}): Promise<void> {
	const {systemUser, recipientId, content, userChannelService, channelService, userCacheService, requestCache} = params;
	const channel = await userChannelService.ensureDmOpenForBothUsers({
		userId: SYSTEM_USER_ID,
		recipientId,
		userCacheService,
		requestCache,
	});
	await channelService.messages.send.sendMessage({
		user: systemUser,
		viewer: SYSTEM_THREAD_VIEWER,
		channelId: channel.id,
		data: {content},
		requestCache,
	});
}
