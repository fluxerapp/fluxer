// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {parseDeletionQueueMember, parseDeletionQueueUserId} from '@app/api/infrastructure/DeletionQueueMember';
import {Logger} from '@app/api/Logger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	isPendingDeletionBlocked,
	resolvePendingDeletionReasonCode,
} from '@app/api/user/services/PendingDeletionCoordinator';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import {Int32Type} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {generateLockToken} from '@pkgs/cache/src/CacheLockValidation';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

interface QueuedDeletion {
	userId: bigint;
	deletionReasonCode: number;
}

const QUEUE_KEY = 'deletion_queue';
const STATE_VERSION_KEY = 'deletion_queue:state_version';
const REBUILD_LOCK_KEY = 'deletion_queue:rebuild_lock';
const REBUILD_LOCK_TTL = seconds('5 minutes');

export class KVAccountDeletionQueueService {
	constructor(
		private readonly kvClient: IKVProvider,
		private readonly userRepository: UserRepository,
	) {}

	private serializeQueueItem(item: QueuedDeletion): string {
		const userId = parseDeletionQueueUserId(item.userId.toString());
		const reasonCode = this.validateReasonCode(item.deletionReasonCode);
		return `${userId}|${reasonCode}`;
	}

	private deserializeQueueItem(value: string): QueuedDeletion {
		const {userId, payload} = parseDeletionQueueMember(value);
		if (payload.length > 10 || /\D/.test(payload)) {
			throw new TypeError('Deletion queue reason code must be a decimal nonnegative 32-bit integer');
		}
		return {
			userId,
			deletionReasonCode: this.validateReasonCode(Number(payload)),
		};
	}

	private validateReasonCode(value: number): number {
		const parsed = Int32Type.safeParse(value);
		if (!parsed.success) {
			throw new TypeError('Deletion queue reason code must be a decimal nonnegative 32-bit integer');
		}
		return parsed.data;
	}

	async needsRebuild(): Promise<boolean> {
		try {
			const stateVersion = parseStoredTimestamp(
				await this.kvClient.get(STATE_VERSION_KEY),
				'Deletion queue state version',
			);
			if (stateVersion === null) {
				Logger.debug('Deletion queue needs rebuild: no state version');
				return true;
			}
			const ageMs = Date.now() - stateVersion;
			if (ageMs > ms('1 day')) {
				Logger.debug({ageMs, maxAgeMs: ms('1 day')}, 'Deletion queue needs rebuild: state too old');
				return true;
			}
			return false;
		} catch (error) {
			Logger.error({error}, 'Failed to check if deletion queue needs rebuild');
			throw error;
		}
	}

	async rebuildState(lockToken: string | null = null): Promise<void> {
		Logger.info('Starting deletion queue rebuild from primary database');
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
				for (const user of users) {
					if (!user.pendingDeletionAt || (!user.deletionStartedAt && isPendingDeletionBlocked(user))) continue;
					const queueItem: QueuedDeletion = {
						userId: user.id,
						deletionReasonCode: resolvePendingDeletionReasonCode(user, 0),
					};
					const score = getValidTimestamp(user.pendingDeletionAt, `Pending deletion timestamp for user ${user.id}`);
					const value = this.serializeQueueItem(queueItem);
					const secondaryKey = this.getSecondaryKey(user.id);
					await this.kvClient.scheduleBulkDeletion(QUEUE_KEY, secondaryKey, score, value);
					totalQueued++;
				}
				totalProcessed += users.length;
				if (lockToken !== null) {
					await this.renewRebuildLock(lockToken);
				}
				if (users.length > 0 && totalProcessed % 10000 === 0) {
					Logger.debug({totalProcessed, totalQueued}, 'Deletion queue rebuild progress');
				}
			} while (pageState);
			await this.kvClient.set(STATE_VERSION_KEY, Date.now().toString());
			Logger.info({totalProcessed, totalQueued}, 'Deletion queue rebuild completed');
		} catch (error) {
			Logger.error({error}, 'Failed to rebuild deletion queue state');
			throw error;
		}
	}

	async scheduleDeletion(userId: UserID, pendingAt: Date, reasonCode: number): Promise<void> {
		try {
			const queueItem: QueuedDeletion = {
				userId,
				deletionReasonCode: reasonCode,
			};
			const score = getValidTimestamp(pendingAt, `Pending deletion timestamp for user ${userId}`);
			const value = this.serializeQueueItem(queueItem);
			const secondaryKey = this.getSecondaryKey(userId);
			await this.kvClient.removeBulkDeletion(QUEUE_KEY, secondaryKey);
			await this.kvClient.scheduleBulkDeletion(QUEUE_KEY, secondaryKey, score, value);
			Logger.debug({userId: userId.toString(), pendingAt, reasonCode}, 'Scheduled user deletion');
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to schedule deletion');
			throw error;
		}
	}

	async removeFromQueue(userId: UserID): Promise<void> {
		try {
			const secondaryKey = this.getSecondaryKey(userId);
			const value = await this.kvClient.get(secondaryKey);
			if (!value) {
				Logger.debug({userId: userId.toString()}, 'User not in deletion queue');
				return;
			}
			await this.kvClient.removeBulkDeletion(QUEUE_KEY, secondaryKey);
			Logger.debug({userId: userId.toString()}, 'Removed user from deletion queue');
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to remove user from deletion queue');
			throw error;
		}
	}

	async getReadyDeletions(nowMs: number, limit: number): Promise<Array<QueuedDeletion>> {
		try {
			const results = await this.kvClient.zrangebyscore(QUEUE_KEY, '-inf', nowMs, 'LIMIT', 0, limit);
			const deletions: Array<QueuedDeletion> = [];
			const unparseable: Array<string> = [];
			for (const result of results) {
				try {
					deletions.push(this.deserializeQueueItem(result));
				} catch {
					unparseable.push(result);
				}
			}
			if (unparseable.length > 0) {
				Logger.error({unparseableCount: unparseable.length}, 'Dropped unparseable deletion queue members');
				try {
					await this.kvClient.zrem(QUEUE_KEY, ...unparseable);
				} catch (error) {
					Logger.error(
						{error, unparseableCount: unparseable.length},
						'Failed to remove unparseable deletion queue members',
					);
				}
			}
			return deletions;
		} catch (error) {
			Logger.error({error, nowMs, limit}, 'Failed to get ready deletions');
			throw error;
		}
	}

	async acquireRebuildLock(): Promise<string | null> {
		try {
			const token = generateLockToken();
			const acquired = await this.kvClient.acquireLock(REBUILD_LOCK_KEY, token, REBUILD_LOCK_TTL);
			if (acquired) {
				Logger.debug({token}, 'Acquired rebuild lock');
				return token;
			}
			return null;
		} catch (error) {
			Logger.error({error}, 'Failed to acquire rebuild lock');
			throw error;
		}
	}

	private async renewRebuildLock(token: string): Promise<void> {
		try {
			const renewed = await this.kvClient.extendLock(REBUILD_LOCK_KEY, token, REBUILD_LOCK_TTL);
			if (!renewed) {
				Logger.warn({token}, 'Deletion queue rebuild lock was no longer held on renewal');
			}
		} catch (error) {
			Logger.error({error, token}, 'Failed to renew deletion queue rebuild lock');
		}
	}

	async releaseRebuildLock(token: string): Promise<boolean> {
		try {
			const released = await this.kvClient.releaseLock(REBUILD_LOCK_KEY, token);
			if (released) {
				Logger.debug({token}, 'Released rebuild lock');
			}
			return released;
		} catch (error) {
			Logger.error({error, token}, 'Failed to release rebuild lock');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.kvClient.zcard(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get queue size');
			throw error;
		}
	}

	async getStateVersion(): Promise<number | null> {
		try {
			return parseStoredTimestamp(await this.kvClient.get(STATE_VERSION_KEY), 'Deletion queue state version');
		} catch (error) {
			Logger.error({error}, 'Failed to get state version');
			throw error;
		}
	}

	private getSecondaryKey(userId: UserID): string {
		return `deletion_queue_by_user:${userId.toString()}`;
	}
}
