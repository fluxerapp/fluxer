// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {withRetryingLock} from '@app/api/channel/utils/RetryingLock';
import type {Message} from '@app/api/models/Message';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

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
		return withRetryingLock(this.cacheService, messageWriteLockKey(channelId, messageId), fn);
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
