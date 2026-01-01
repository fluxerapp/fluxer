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
import {UserRepository} from '~/user/UserRepository';

const TTL_SECONDS = 90 * 24 * 60 * 60;
const STATE_VERSION_KEY = 'activity_tracker:state_version';
const STATE_VERSION_TTL_SECONDS = 24 * 60 * 60;
const REBUILD_BATCH_SIZE = 100;

export class RedisActivityTracker {
	private redis: Redis;

	constructor(redis: Redis) {
		this.redis = redis;
	}

	private getActivityKey(userId: UserID): string {
		return `user_activity:${userId}`;
	}

	async updateActivity(userId: UserID, timestamp: Date): Promise<void> {
		const key = this.getActivityKey(userId);
		const value = timestamp.getTime().toString();
		await this.redis.setex(key, TTL_SECONDS, value);
	}

	async getActivity(userId: UserID): Promise<Date | null> {
		const key = this.getActivityKey(userId);
		const value = await this.redis.get(key);

		if (!value) {
			return null;
		}

		const timestamp = parseInt(value, 10);
		if (Number.isNaN(timestamp)) {
			return null;
		}

		return new Date(timestamp);
	}

	async needsRebuild(): Promise<boolean> {
		const exists = await this.redis.exists(STATE_VERSION_KEY);
		if (exists === 0) {
			return true;
		}

		const ttl = await this.redis.ttl(STATE_VERSION_KEY);

		if (ttl < 0) {
			return true;
		}

		const age = STATE_VERSION_TTL_SECONDS - ttl;
		return age > STATE_VERSION_TTL_SECONDS;
	}

	async rebuildActivities(): Promise<void> {
		Logger.info('Starting activity tracker rebuild from Cassandra');

		const userRepository = new UserRepository();

		try {
			const redisBatchSize = 1000;
			let processedCount = 0;
			let usersWithActivity = 0;
			let pipeline = this.redis.pipeline();
			let pipelineCount = 0;
			let lastUserId: UserID | undefined;

			while (true) {
				const users = await userRepository.listAllUsersPaginated(REBUILD_BATCH_SIZE, lastUserId);

				if (users.length === 0) {
					break;
				}

				for (const user of users) {
					if (user.lastActiveAt) {
						const key = this.getActivityKey(user.id);
						const value = user.lastActiveAt.getTime().toString();
						pipeline.setex(key, TTL_SECONDS, value);
						pipelineCount++;
						usersWithActivity++;

						if (pipelineCount >= redisBatchSize) {
							await pipeline.exec();
							pipeline = this.redis.pipeline();
							pipelineCount = 0;
						}
					}

					processedCount++;
				}

				if (processedCount % 10000 === 0) {
					Logger.info({processedCount, usersWithActivity}, 'Activity tracker rebuild progress');
				}

				lastUserId = users[users.length - 1].id;
			}

			if (pipelineCount > 0) {
				await pipeline.exec();
			}

			await this.redis.setex(STATE_VERSION_KEY, STATE_VERSION_TTL_SECONDS, Date.now().toString());

			Logger.info({processedCount, usersWithActivity}, 'Activity tracker rebuild completed');
		} catch (error) {
			Logger.error({error}, 'Activity tracker rebuild failed');
			throw error;
		}
	}
}
