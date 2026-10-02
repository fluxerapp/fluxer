// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID} from '@app/api/BrandedTypes';
import type {ICrosspostedMessageRepository} from '@app/api/channel/repositories/ICrosspostedMessageRepository';
import {Logger} from '@app/api/Logger';
import {getSnowflakeService, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {Channel} from '@app/api/models/Channel';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const CHANNEL_FOLLOW_LOCK_TTL_SECONDS = 5;
const CHANNEL_FOLLOW_LOCK_ACQUIRE_ATTEMPTS = 6;
const CHANNEL_FOLLOW_LOCK_RETRY_DELAY_MS = 50;

export type ChannelFollowerRemovalReason = 'deleted' | 'converted';
export type ChannelFollowerRemovalCopyMode = 'source_deleted' | 'purge';

export async function withChannelFollowLock<T>(
	cacheService: ICacheService,
	channelId: ChannelID,
	fn: () => Promise<T>,
): Promise<T> {
	const lockKey = `channel-follow:${channelId}`;
	let lockToken: string | null = null;
	for (let attempt = 0; attempt < CHANNEL_FOLLOW_LOCK_ACQUIRE_ATTEMPTS; attempt++) {
		lockToken = await cacheService.acquireLock(lockKey, CHANNEL_FOLLOW_LOCK_TTL_SECONDS);
		if (lockToken) break;
		await new Promise((resolve) => setTimeout(resolve, CHANNEL_FOLLOW_LOCK_RETRY_DELAY_MS * (attempt + 1)));
	}
	if (!lockToken) {
		throw new ThrottledError({
			code: APIErrorCodes.RESOURCE_LOCKED,
			retryAfterSeconds: 1,
			data: {retry_after: 1},
		});
	}
	try {
		return await fn();
	} finally {
		await cacheService.releaseLock(lockKey, lockToken).catch(() => {});
	}
}

interface ChannelFollowerRemovalParams {
	sourceChannelId: ChannelID;
	reason: ChannelFollowerRemovalReason;
	copyMode?: ChannelFollowerRemovalCopyMode;
}

export async function addChannelFollowerRemovalJob(params: ChannelFollowerRemovalParams): Promise<void> {
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

export async function channelMayHaveFollowerCopies(
	channel: Pick<Channel, 'id' | 'type'>,
	crossposts: Pick<ICrosspostedMessageRepository, 'listSourcesByChannel'>,
): Promise<boolean> {
	if (channel.type === ChannelTypes.GUILD_ANNOUNCEMENT) return true;
	const sources = await crossposts.listSourcesByChannel(channel.id, {limit: 1});
	return sources.length > 0;
}

export async function scheduleDeletedChannelFollowerRemoval(params: {
	channel: Pick<Channel, 'id' | 'type'>;
	crossposts: Pick<ICrosspostedMessageRepository, 'listSourcesByChannel'>;
	copyMode: ChannelFollowerRemovalCopyMode;
}): Promise<void> {
	if (!(await channelMayHaveFollowerCopies(params.channel, params.crossposts))) return;
	await addChannelFollowerRemovalJob({
		sourceChannelId: params.channel.id,
		reason: 'deleted',
		copyMode: params.copyMode,
	});
}
