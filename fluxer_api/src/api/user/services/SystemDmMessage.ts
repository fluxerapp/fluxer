// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {SYSTEM_THREAD_VIEWER} from '@app/api/experiment/ChannelThreadsGate';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import {createRequestCache, type RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {UserChannelService} from '@app/api/user/services/UserChannelService';

export interface SystemDmDeps {
	channelService: ChannelService;
	userChannelService: UserChannelService;
	userCacheService: UserCacheService;
}

export async function systemDmsHidden(
	config: Pick<InstanceConfigRepository, 'getInstancePolicyConfig'> = getInstanceConfigRepository(),
): Promise<boolean> {
	return (await config.getInstancePolicyConfig()).direct_messages_disabled;
}

export async function noticeEmailAddress(
	user: Pick<User, 'email'>,
	config: Pick<InstanceConfigRepository, 'isEmailEnabled'> = getInstanceConfigRepository(),
): Promise<string | null> {
	return user.email && (await config.isEmailEnabled()) ? user.email : null;
}

export function systemDmBody(emailBody: string): string {
	return emailBody.split('\n\n').slice(0, -1).join('\n\n');
}

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

export async function trySendSystemDm(
	deps: SystemDmDeps,
	users: Pick<UserRepository, 'findUniqueAssert'>,
	recipientId: UserID,
	content: string,
): Promise<boolean> {
	if (await systemDmsHidden()) {
		return false;
	}
	const requestCache = createRequestCache();
	try {
		await sendSystemDmMessage({
			systemUser: await users.findUniqueAssert(SYSTEM_USER_ID),
			recipientId,
			content,
			userChannelService: deps.userChannelService,
			channelService: deps.channelService,
			userCacheService: deps.userCacheService,
			requestCache,
		});
		return true;
	} catch (error) {
		Logger.warn({recipientId: recipientId.toString(), error}, 'Failed to send a system DM');
		return false;
	} finally {
		requestCache.clear();
	}
}
