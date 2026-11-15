// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {parseDeletionQueueMember, parseDeletionQueueUserId} from '@app/api/infrastructure/DeletionQueueMember';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {generateLockToken} from '@pkgs/cache/src/CacheLockValidation';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

interface UserScheduledKvQueueOptions<Item, Args extends Array<unknown>> {
	queueKey: string;
	stateVersionKey: string;
	rebuildLockKey: string;
	secondaryKey: (userId: UserID) => string;
	label: string;
	timestampLabel: string;
	encodePayload: (item: Item) => string;
	decodePayload: (userId: bigint, payload: string) => Item;
	scheduledItem: (userId: UserID, score: number, ...args: Args) => Item;
	rebuildEntry: (user: User) => {score: number; item: Item} | null;
}

const REBUILD_LOCK_TTL = seconds('5 minutes');

export class UserScheduledKvQueue<Item extends {userId: bigint}, Args extends Array<unknown>> {
	constructor(
		private readonly kvClient: IKVProvider,
		private readonly userRepository: UserRepository,
		private readonly options: UserScheduledKvQueueOptions<Item, Args>,
	) {}

	private serializeQueueItem(item: Item): string {
		const userId = parseDeletionQueueUserId(item.userId.toString());
		return `${userId.toString()}|${this.options.encodePayload(item)}`;
	}

	private deserializeQueueItem(value: string): Item {
		const {userId, payload} = parseDeletionQueueMember(value);
		return this.options.decodePayload(userId, payload);
	}

	async needsRebuild(): Promise<boolean> {
		try {
			const stateVersion = parseStoredTimestamp(
				await this.kvClient.get(this.options.stateVersionKey),
				`${this.options.label} state version`,
			);
			if (stateVersion === null) {
				Logger.debug(`${this.options.label} needs rebuild: no state version`);
				return true;
			}
			const ageMs = Date.now() - stateVersion;
			if (ageMs > ms('1 day')) {
				Logger.debug({ageMs, maxAgeMs: ms('1 day')}, `${this.options.label} needs rebuild: state too old`);
				return true;
			}
			return false;
		} catch (error) {
			Logger.error({error}, `Failed to check if ${this.options.label} needs rebuild`);
			throw error;
		}
	}

	async rebuildState(lockToken: string | null = null): Promise<void> {
		Logger.info(`Starting ${this.options.label} rebuild from primary database`);
		try {
			await this.kvClient.del(this.options.queueKey);
			await this.kvClient.del(this.options.stateVersionKey);
			let pageState: string | null = null;
			let totalProcessed = 0;
			let totalQueued = 0;
			const batchSize = 1000;
			do {
				const page = await this.userRepository.scanAllUsersPage(batchSize, pageState);
				pageState = page.pageState;
				const users = page.users;
				for (const user of users) {
					const entry = this.options.rebuildEntry(user);
					if (!entry) continue;
					const value = this.serializeQueueItem(entry.item);
					const secondaryKey = this.options.secondaryKey(user.id);
					await this.kvClient.removeBulkDeletion(this.options.queueKey, secondaryKey);
					await this.kvClient.scheduleBulkDeletion(this.options.queueKey, secondaryKey, entry.score, value);
					totalQueued++;
				}
				totalProcessed += users.length;
				if (lockToken !== null) {
					await this.renewRebuildLock(lockToken);
				}
				if (users.length > 0 && totalProcessed % 10000 === 0) {
					Logger.debug({totalProcessed, totalQueued}, `${this.options.label} rebuild progress`);
				}
			} while (pageState);
			await this.kvClient.set(this.options.stateVersionKey, Date.now().toString());
			Logger.info({totalProcessed, totalQueued}, `${this.options.label} rebuild completed`);
		} catch (error) {
			Logger.error({error}, `Failed to rebuild ${this.options.label} state`);
			throw error;
		}
	}

	async getReadyDeletions(nowMs: number, limit: number): Promise<Array<Item>> {
		try {
			const results = await this.kvClient.zrangebyscore(this.options.queueKey, '-inf', nowMs, 'LIMIT', 0, limit);
			const deletions: Array<Item> = [];
			const unparseable: Array<string> = [];
			for (const result of results) {
				try {
					deletions.push(this.deserializeQueueItem(result));
				} catch {
					unparseable.push(result);
				}
			}
			if (unparseable.length > 0) {
				Logger.error({unparseableCount: unparseable.length}, `Dropped unparseable ${this.options.label} members`);
				try {
					await this.kvClient.zrem(this.options.queueKey, ...unparseable);
				} catch (error) {
					Logger.error(
						{error, unparseableCount: unparseable.length},
						`Failed to remove unparseable ${this.options.label} members`,
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
			const acquired = await this.kvClient.acquireLock(this.options.rebuildLockKey, token, REBUILD_LOCK_TTL);
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
			const renewed = await this.kvClient.extendLock(this.options.rebuildLockKey, token, REBUILD_LOCK_TTL);
			if (!renewed) {
				Logger.warn({token}, `${this.options.label} rebuild lock was no longer held on renewal`);
			}
		} catch (error) {
			Logger.error({error, token}, `Failed to renew ${this.options.label} rebuild lock`);
		}
	}

	async releaseRebuildLock(token: string): Promise<boolean> {
		try {
			const released = await this.kvClient.releaseLock(this.options.rebuildLockKey, token);
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
			return await this.kvClient.zcard(this.options.queueKey);
		} catch (error) {
			Logger.error({error}, 'Failed to get queue size');
			throw error;
		}
	}

	async scheduleDeletion(userId: UserID, pendingAt: Date, ...args: Args): Promise<void> {
		try {
			const score = getValidTimestamp(pendingAt, `${this.options.timestampLabel} timestamp for user ${userId}`);
			const queueItem = this.options.scheduledItem(userId, score, ...args);
			const value = this.serializeQueueItem(queueItem);
			const secondaryKey = this.options.secondaryKey(userId);
			await this.kvClient.removeBulkDeletion(this.options.queueKey, secondaryKey);
			await this.kvClient.scheduleBulkDeletion(this.options.queueKey, secondaryKey, score, value);
			Logger.debug({userId: userId.toString(), pendingAt}, `Scheduled ${this.options.label}`);
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to schedule deletion');
			throw error;
		}
	}

	async removeFromQueue(userId: UserID): Promise<void> {
		try {
			const secondaryKey = this.options.secondaryKey(userId);
			const removed = await this.kvClient.removeBulkDeletion(this.options.queueKey, secondaryKey);
			if (!removed) {
				Logger.debug({userId: userId.toString()}, `User not in ${this.options.label}`);
				return;
			}
			Logger.debug({userId: userId.toString()}, `Removed user from ${this.options.label}`);
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, `Failed to remove user from ${this.options.label}`);
			throw error;
		}
	}
}

export async function rebuildIfStale(
	queue: Pick<
		UserScheduledKvQueue<{userId: bigint}, []>,
		'needsRebuild' | 'acquireRebuildLock' | 'rebuildState' | 'releaseRebuildLock'
	>,
	logger: LoggerInterface,
	label: string,
): Promise<boolean> {
	if (!(await queue.needsRebuild())) return true;
	logger.info(`${label} needs rebuild, acquiring lock`);
	const lockToken = await queue.acquireRebuildLock();
	if (!lockToken) {
		logger.info(`Another worker is rebuilding the ${label.toLowerCase()}, skipping this run`);
		return false;
	}
	try {
		await queue.rebuildState(lockToken);
	} catch (error) {
		try {
			await queue.releaseRebuildLock(lockToken);
		} catch (releaseError) {
			logger.error({error: releaseError}, `Failed to release ${label.toLowerCase()} lock after rebuild failure`);
		}
		throw error;
	}
	await queue.releaseRebuildLock(lockToken);
	return true;
}
