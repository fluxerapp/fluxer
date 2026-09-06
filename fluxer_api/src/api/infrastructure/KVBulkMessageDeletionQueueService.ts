// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateLockToken} from '@pkgs/cache/src/CacheLockValidation';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';
import type {UserID} from '../BrandedTypes';
import {Logger} from '../Logger';
import type {UserRepository} from '../user/repositories/UserRepository';

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
		return `${item.userId}|${item.scheduledAt}`;
	}

	private deserializeQueueItem(value: string): QueuedBulkMessageDeletion {
		const [userIdStr, scheduledAtStr] = value.split('|');
		return {
			userId: BigInt(userIdStr),
			scheduledAt: Number.parseInt(scheduledAtStr, 10),
		};
	}

	async needsRebuild(): Promise<boolean> {
		try {
			const versionExists = await this.kvClient.exists(STATE_VERSION_KEY);
			if (!versionExists) {
				Logger.debug('Bulk message deletion queue needs rebuild: no state version');
				return true;
			}
			const stateVersionStr = await this.kvClient.get(STATE_VERSION_KEY);
			if (stateVersionStr) {
				const stateVersion = Number.parseInt(stateVersionStr, 10);
				const ageMs = Date.now() - stateVersion;
				if (ageMs > ms('1 day')) {
					Logger.debug({ageMs, maxAgeMs: ms('1 day')}, 'Bulk message deletion queue needs rebuild: state too old');
					return true;
				}
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
			while (true) {
				const page = await this.userRepository.scanAllUsersPage(batchSize, pageState);
				const users = page.users;
				if (users.length === 0) {
					break;
				}
				for (const user of users) {
					if (user.pendingBulkMessageDeletionAt) {
						await this.scheduleDeletion(user.id, user.pendingBulkMessageDeletionAt);
						totalQueued++;
					}
				}
				totalProcessed += users.length;
				pageState = page.pageState;
				if (totalProcessed % 10000 === 0) {
					Logger.debug({totalProcessed, totalQueued}, 'Bulk message deletion queue rebuild progress');
				}
				if (!pageState) {
					break;
				}
			}
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
				scheduledAt: scheduledAt.getTime(),
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
