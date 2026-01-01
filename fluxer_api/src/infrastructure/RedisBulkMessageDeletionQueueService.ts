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

import type {Redis} from 'ioredis';
import type {UserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';

interface QueuedBulkMessageDeletion {
	userId: bigint;
	scheduledAt: number;
}

const QUEUE_KEY = 'bulk_message_deletion_queue';
const SECONDARY_KEY_PREFIX = 'bulk_message_deletion_queue:';

export class RedisBulkMessageDeletionQueueService {
	constructor(private readonly redis: Redis) {}

	private getSecondaryKey(userId: UserID): string {
		return `${SECONDARY_KEY_PREFIX}${userId}`;
	}

	private serializeQueueItem(item: QueuedBulkMessageDeletion): string {
		return `${item.userId}|${item.scheduledAt}`;
	}

	private deserializeQueueItem(value: string): QueuedBulkMessageDeletion {
		const [userIdStr, scheduledAtStr] = value.split('|');
		return {
			userId: BigInt(userIdStr),
			scheduledAt: Number.parseInt(scheduledAtStr, 10),
		};
	}

	async scheduleDeletion(userId: UserID, scheduledAt: Date): Promise<void> {
		try {
			const entry: QueuedBulkMessageDeletion = {
				userId,
				scheduledAt: scheduledAt.getTime(),
			};
			const value = this.serializeQueueItem(entry);
			const secondaryKey = this.getSecondaryKey(userId);
			await this.redis.pipeline().zadd(QUEUE_KEY, entry.scheduledAt, value).set(secondaryKey, value).exec();
			Logger.debug({userId: userId.toString(), scheduledAt}, 'Scheduled bulk message deletion');
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to schedule bulk message deletion');
			throw error;
		}
	}

	async removeFromQueue(userId: UserID): Promise<void> {
		try {
			const secondaryKey = this.getSecondaryKey(userId);
			const value = await this.redis.get(secondaryKey);
			if (!value) {
				Logger.debug({userId: userId.toString()}, 'User not in bulk message deletion queue');
				return;
			}
			await this.redis.pipeline().zrem(QUEUE_KEY, value).del(secondaryKey).exec();
			Logger.debug({userId: userId.toString()}, 'Removed bulk message deletion from queue');
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to remove bulk message deletion from queue');
			throw error;
		}
	}

	async getReadyDeletions(nowMs: number, limit: number): Promise<Array<QueuedBulkMessageDeletion>> {
		try {
			const results = await this.redis.zrangebyscore(QUEUE_KEY, '-inf', nowMs, 'LIMIT', 0, limit);
			const deletions: Array<QueuedBulkMessageDeletion> = [];
			for (const result of results) {
				try {
					const deletion = this.deserializeQueueItem(result);
					deletions.push(deletion);
				} catch (error) {
					Logger.error({error, result}, 'Failed to parse queued bulk message deletion entry');
				}
			}
			return deletions;
		} catch (error) {
			Logger.error({error, nowMs, limit}, 'Failed to fetch ready bulk message deletions');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.redis.zcard(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get bulk message deletion queue size');
			throw error;
		}
	}
}
