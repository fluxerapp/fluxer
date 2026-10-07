// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import type {Message} from '@app/api/models/Message';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const MESSAGE_WRITE_LOCK_TTL_SECONDS = 5;
const MESSAGE_WRITE_LOCK_ACQUIRE_ATTEMPTS = 6;
const MESSAGE_WRITE_LOCK_RETRY_DELAY_MS = 50;

export interface MessageWriteLockReader {
	getMessage(channelId: ChannelID, messageId: MessageID): Promise<Message | null>;
}

function messageWriteLockKey(channelId: ChannelID, messageId: MessageID): string {
	return `message:${channelId}:${messageId}:write`;
}

export class MessageWriteLock {
	constructor(
		private readonly cacheService: ICacheService,
		private readonly messages: MessageWriteLockReader,
	) {}

	async withLock<T>(channelId: ChannelID, messageId: MessageID, fn: () => Promise<T>): Promise<T> {
		const lockKey = messageWriteLockKey(channelId, messageId);
		let lockToken: string | null = null;
		for (let attempt = 0; attempt < MESSAGE_WRITE_LOCK_ACQUIRE_ATTEMPTS; attempt++) {
			lockToken = await this.cacheService.acquireLock(lockKey, MESSAGE_WRITE_LOCK_TTL_SECONDS);
			if (lockToken) break;
			await new Promise((resolve) => setTimeout(resolve, MESSAGE_WRITE_LOCK_RETRY_DELAY_MS * (attempt + 1)));
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
			await this.cacheService.releaseLock(lockKey, lockToken).catch(() => {});
		}
	}

	async withFreshMessage<T>(
		channelId: ChannelID,
		messageId: MessageID,
		fn: (fresh: Message | null) => Promise<T>,
	): Promise<T> {
		return this.withLock(channelId, messageId, async () => {
			const fresh = await this.messages.getMessage(channelId, messageId);
			return fn(fresh);
		});
	}
}
