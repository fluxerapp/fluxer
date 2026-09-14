// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {runSlotBatches, splitIntoSlotBatches} from '@pkgs/kv_client/src/KVHashSlots';
import {seconds} from 'itty-time';

const TTL_SECONDS = seconds('90 days');
const STATE_VERSION_KEY = 'activity_tracker:state_version';
const STATE_VERSION_TTL_SECONDS = seconds('1 day');
const REBUILD_BATCH_SIZE = 100;

function parseActivityValue(value: string | null, userId: UserID): Date | null {
	const timestamp = parseStoredTimestamp(value, `Activity timestamp for user ${userId}`);
	return timestamp === null ? null : new Date(timestamp);
}

export class KVActivityTracker {
	private isShuttingDown = false;

	constructor(private readonly kvClient: IKVProvider) {}

	shutdown(): void {
		this.isShuttingDown = true;
	}

	private getActivityKey(userId: UserID): string {
		return `user_activity:${userId}`;
	}

	async updateActivity(userId: UserID, timestamp: Date): Promise<void> {
		const key = this.getActivityKey(userId);
		const value = getValidTimestamp(timestamp, `Activity timestamp for user ${userId}`).toString();
		await this.kvClient.setex(key, TTL_SECONDS, value);
	}

	async getActivity(userId: UserID): Promise<Date | null> {
		const key = this.getActivityKey(userId);
		return parseActivityValue(await this.kvClient.get(key), userId);
	}

	async getActivities(userIds: ReadonlyArray<UserID>): Promise<Map<UserID, Date | null>> {
		const activities = new Map<UserID, Date | null>();
		if (userIds.length === 0) {
			return activities;
		}
		const values = await this.kvClient.mget(...userIds.map((userId) => this.getActivityKey(userId)));
		for (const [index, userId] of userIds.entries()) {
			activities.set(userId, parseActivityValue(values[index], userId));
		}
		return activities;
	}

	async needsRebuild(): Promise<boolean> {
		return (await this.kvClient.ttl(STATE_VERSION_KEY)) < 0;
	}

	private async writeActivityBatch(entries: ReadonlyArray<{key: string; value: string}>): Promise<void> {
		const batches = splitIntoSlotBatches(entries, (entry) => entry.key, this.kvClient.isClustered());
		await runSlotBatches(batches, async (batch) => {
			const pipeline = this.kvClient.pipeline();
			for (const entry of batch) {
				pipeline.setex(entry.key, TTL_SECONDS, entry.value);
			}
			for (const [error] of await pipeline.exec()) {
				if (error) {
					throw error;
				}
			}
		});
	}

	async rebuildActivities(): Promise<void> {
		Logger.info('Starting activity tracker rebuild from Cassandra');
		const userRepository = new UserRepository();
		try {
			const kvBatchSize = 1000;
			let processedCount = 0;
			let usersWithActivity = 0;
			let batch: Array<{key: string; value: string}> = [];
			let pageState: string | null = null;
			let iterationCount = 0;
			while (!this.isShuttingDown) {
				const page = await userRepository.scanAllUsersPage(REBUILD_BATCH_SIZE, pageState);
				const users = page.users;
				for (const user of users) {
					if (!user.lastActiveAt) continue;
					const timestamp = getValidTimestamp(user.lastActiveAt, `Last activity timestamp for user ${user.id}`);
					batch.push({key: this.getActivityKey(user.id), value: timestamp.toString()});
					usersWithActivity++;
					if (batch.length >= kvBatchSize) {
						await this.writeActivityBatch(batch);
						batch = [];
					}
				}
				processedCount += users.length;
				if (users.length > 0 && processedCount % 10000 === 0) {
					Logger.debug({processedCount, usersWithActivity}, 'Activity tracker rebuild progress');
				}
				pageState = page.pageState;
				iterationCount++;
				if (iterationCount % 10 === 0) {
					await new Promise((resolve) => setTimeout(resolve, 100));
				}
				if (!pageState) {
					break;
				}
			}
			if (this.isShuttingDown) {
				Logger.warn({processedCount, usersWithActivity}, 'Activity tracker rebuild interrupted by shutdown');
				return;
			}
			if (batch.length > 0) {
				await this.writeActivityBatch(batch);
			}
			await this.kvClient.setex(STATE_VERSION_KEY, STATE_VERSION_TTL_SECONDS, Date.now().toString());
			Logger.info({processedCount, usersWithActivity}, 'Activity tracker rebuild completed');
		} catch (error) {
			Logger.error({error}, 'Activity tracker rebuild failed');
			throw error;
		}
	}
}
