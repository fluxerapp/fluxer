// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {UserID} from '@app/api/BrandedTypes';
import {parseDeletionQueueMember, parseDeletionQueueUserId} from '@app/api/infrastructure/DeletionQueueMember';
import {Logger} from '@app/api/Logger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import {generateLockToken} from '@pkgs/cache/src/CacheLockValidation';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

interface QueuedBulkMessageDeletion {
	userId: bigint;
	scheduledAt: number;
}

const QUEUE_KEY = 'bulk_message_deletion_queue';
const SECONDARY_KEY_PREFIX = 'bulk_message_deletion_queue:';
const STATE_VERSION_KEY = 'bulk_message_deletion_queue:state_version';
const REBUILD_LOCK_KEY = 'bulk_message_deletion_queue:rebuild_lock';
const REBUILD_LOCK_TTL = seconds('5 minutes');

export class KVBulkMessageDeletionQueueService {
	constructor(
		private readonly kvClient: IKVProvider,
		private readonly userRepository: UserRepository,
	) {}

	private getSecondaryKey(userId: UserID): string {
		return `${SECONDARY_KEY_PREFIX}${userId}`;
	}

	private serializeQueueItem(item: QueuedBulkMessageDeletion): string {
		const userId = parseDeletionQueueUserId(item.userId.toString());
		return `${userId}|${item.scheduledAt}`;
	}

	private deserializeQueueItem(value: string): QueuedBulkMessageDeletion {
		const {userId, payload} = parseDeletionQueueMember(value);
		const scheduledAt = parseStoredTimestamp(payload, 'Bulk message deletion queue timestamp');
		assert(scheduledAt !== null, 'A non-null bulk message deletion timestamp must not parse as null');
		return {
			userId,
			scheduledAt,
		};
	}

	async needsRebuild(): Promise<boolean> {
		try {
			const stateVersion = parseStoredTimestamp(
				await this.kvClient.get(STATE_VERSION_KEY),
				'Bulk message deletion queue state version',
			);
			if (stateVersion === null) {
				Logger.debug('Bulk message deletion queue needs rebuild: no state version');
				return true;
			}
			const ageMs = Date.now() - stateVersion;
			if (ageMs > ms('1 day')) {
				Logger.debug({ageMs, maxAgeMs: ms('1 day')}, 'Bulk message deletion queue needs rebuild: state too old');
				return true;
			}
			return false;
		} catch (error) {
			Logger.error({error}, 'Failed to check if bulk message deletion queue needs rebuild');
			throw error;
		}
	}

	async rebuildState(): Promise<void> {
		Logger.info('Starting bulk message deletion queue rebuild from primary database');
		try {
			await this.kvClient.del(QUEUE_KEY);
			await this.kvClient.del(STATE_VERSION_KEY);
			let pageState: string | null = null;
			let totalProcessed = 0;
			let totalQueued = 0;
			const batchSize = 1000;
			do {
				const page = await this.userRepository.scanAllUsersPage(batchSize, pageState);
				pageState = page.pageState;
				const users = page.users;
				if (users.length === 0) continue;
				for (const user of users) {
					if (!user.pendingBulkMessageDeletionAt) continue;
					await this.scheduleDeletion(user.id, user.pendingBulkMessageDeletionAt);
					totalQueued++;
				}
				totalProcessed += users.length;
				if (totalProcessed % 10000 === 0) {
					Logger.debug({totalProcessed, totalQueued}, 'Bulk message deletion queue rebuild progress');
				}
			} while (pageState);
			await this.kvClient.set(STATE_VERSION_KEY, Date.now().toString());
			Logger.info({totalProcessed, totalQueued}, 'Bulk message deletion queue rebuild completed');
		} catch (error) {
			Logger.error({error}, 'Failed to rebuild bulk message deletion queue state');
			throw error;
		}
	}

	async scheduleDeletion(userId: UserID, scheduledAt: Date): Promise<void> {
		try {
			const entry: QueuedBulkMessageDeletion = {
				userId,
				scheduledAt: getValidTimestamp(scheduledAt, `Pending bulk message deletion timestamp for user ${userId}`),
			};
			const value = this.serializeQueueItem(entry);
			const secondaryKey = this.getSecondaryKey(userId);
			await this.kvClient.scheduleBulkDeletion(QUEUE_KEY, secondaryKey, entry.scheduledAt, value);
			Logger.debug({userId: userId.toString(), scheduledAt}, 'Scheduled bulk message deletion');
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to schedule bulk message deletion');
			throw error;
		}
	}

	async removeFromQueue(userId: UserID): Promise<void> {
		try {
			const secondaryKey = this.getSecondaryKey(userId);
			const removed = await this.kvClient.removeBulkDeletion(QUEUE_KEY, secondaryKey);
			if (!removed) {
				Logger.debug({userId: userId.toString()}, 'User not in bulk message deletion queue');
				return;
			}
			Logger.debug({userId: userId.toString()}, 'Removed bulk message deletion from queue');
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to remove bulk message deletion from queue');
			throw error;
		}
	}

	async getReadyDeletions(nowMs: number, limit: number): Promise<Array<QueuedBulkMessageDeletion>> {
		try {
			const results = await this.kvClient.zrangebyscore(QUEUE_KEY, '-inf', nowMs, 'LIMIT', 0, limit);
			const deletions: Array<QueuedBulkMessageDeletion> = [];
			const unparseable: Array<string> = [];
			for (const result of results) {
				try {
					deletions.push(this.deserializeQueueItem(result));
				} catch {
					unparseable.push(result);
				}
			}
			if (unparseable.length > 0) {
				Logger.error({unparseableCount: unparseable.length}, 'Dropped unparseable bulk message deletion queue members');
				try {
					await this.kvClient.zrem(QUEUE_KEY, ...unparseable);
				} catch (error) {
					Logger.error(
						{error, unparseableCount: unparseable.length},
						'Failed to remove unparseable bulk message deletion queue members',
					);
				}
			}
			return deletions;
		} catch (error) {
			Logger.error({error, nowMs, limit}, 'Failed to fetch ready bulk message deletions');
			throw error;
		}
	}

	async acquireRebuildLock(): Promise<string | null> {
		try {
			const token = generateLockToken();
			const acquired = await this.kvClient.acquireLock(REBUILD_LOCK_KEY, token, REBUILD_LOCK_TTL);
			if (acquired) {
				Logger.debug({token}, 'Acquired bulk message deletion rebuild lock');
				return token;
			}
			return null;
		} catch (error) {
			Logger.error({error}, 'Failed to acquire bulk message deletion rebuild lock');
			throw error;
		}
	}

	async releaseRebuildLock(token: string): Promise<boolean> {
		try {
			const released = await this.kvClient.releaseLock(REBUILD_LOCK_KEY, token);
			if (released) {
				Logger.debug({token}, 'Released bulk message deletion rebuild lock');
			}
			return released;
		} catch (error) {
			Logger.error({error, token}, 'Failed to release bulk message deletion rebuild lock');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.kvClient.zcard(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get bulk message deletion queue size');
			throw error;
		}
	}
}
