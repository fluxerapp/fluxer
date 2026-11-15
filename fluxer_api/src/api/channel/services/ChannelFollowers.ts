// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID} from '@app/api/BrandedTypes';
import type {CrosspostedMessageRepository} from '@app/api/channel/repositories/CrosspostedMessageRepository';
import {withRetryingLock} from '@app/api/channel/utils/RetryingLock';
import {Logger} from '@app/api/Logger';
import {getSnowflakeService, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {Channel} from '@app/api/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

type ChannelFollowerRemovalReason = 'deleted' | 'converted';
export type ChannelFollowerRemovalCopyMode = 'source_deleted' | 'purge';

export async function withChannelFollowLock<T>(
	cacheService: ICacheService,
	channelId: ChannelID,
	fn: () => Promise<T>,
	keepAliveTtlSeconds?: number,
): Promise<T> {
	return withRetryingLock(cacheService, `channel-follow:${channelId}`, fn, keepAliveTtlSeconds);
}

interface ChannelFollowerRemovalParams {
	sourceChannelId: ChannelID;
	reason: ChannelFollowerRemovalReason;
	copyMode?: ChannelFollowerRemovalCopyMode;
}

async function addChannelFollowerRemovalJob(params: ChannelFollowerRemovalParams): Promise<void> {
	const {sourceChannelId, reason, copyMode} = params;
	const uniqueSuffix = await getSnowflakeService().generate();
	await getWorkerService().addJob(
		'removeChannelFollowers',
		{
			sourceChannelId: sourceChannelId.toString(),
			reason,
			...(copyMode ? {copyMode} : {}),
		},
		{jobKey: `remove-followers:${sourceChannelId}:${reason}:${uniqueSuffix}`},
	);
}

export async function enqueueChannelFollowerRemoval(params: ChannelFollowerRemovalParams): Promise<void> {
	try {
		await addChannelFollowerRemovalJob(params);
	} catch (error) {
		Logger.error(
			{error, sourceChannelId: params.sourceChannelId.toString(), reason: params.reason, copyMode: params.copyMode},
			'Failed to enqueue channel follower removal',
		);
	}
}

async function channelMayHaveFollowerCopies(
	channel: Pick<Channel, 'id' | 'type'>,
	crossposts: Pick<CrosspostedMessageRepository, 'listSourcesByChannel'>,
): Promise<boolean> {
	if (channel.type === ChannelTypes.GUILD_ANNOUNCEMENT) return true;
	const sources = await crossposts.listSourcesByChannel(channel.id, {limit: 1});
	return sources.length > 0;
}

export async function scheduleDeletedChannelFollowerRemoval(params: {
	channel: Pick<Channel, 'id' | 'type'>;
	crossposts: Pick<CrosspostedMessageRepository, 'listSourcesByChannel'>;
	copyMode: ChannelFollowerRemovalCopyMode;
}): Promise<void> {
	if (!(await channelMayHaveFollowerCopies(params.channel, params.crossposts))) return;
	await addChannelFollowerRemovalJob({
		sourceChannelId: params.channel.id,
		reason: 'deleted',
		copyMode: params.copyMode,
	});
}
