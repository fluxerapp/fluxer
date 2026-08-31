// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {createUserID, type UserID} from '../BrandedTypes';
import {Logger} from '../Logger';

const QUEUE_KEY = 'premium:reconcile:queue';
const SECONDARY_KEY_PREFIX = 'premium:reconcile:queue:user:';

export class PremiumStateReconciliationQueueService {
	constructor(private readonly kvClient: IKVProvider) {}

	private getSecondaryKey(userId: UserID): string {
		return `${SECONDARY_KEY_PREFIX}${userId.toString()}`;
	}

	private serializeQueueValue(userId: UserID): string {
		return userId.toString();
	}

	async enqueueUser(userId: UserID, scheduledAt: Date = new Date()): Promise<void> {
		try {
			const secondaryKey = this.getSecondaryKey(userId);
			const value = this.serializeQueueValue(userId);
			await this.kvClient.scheduleBulkDeletion(QUEUE_KEY, secondaryKey, scheduledAt.getTime(), value);
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to enqueue user for premium state reconciliation');
			throw error;
		}
	}

	async claimUser(userId: UserID, nowMs: number, leaseUntilMs: number): Promise<boolean> {
		try {
			const value = this.serializeQueueValue(userId);
			return await this.kvClient.claimBulkDeletion(QUEUE_KEY, value, nowMs, leaseUntilMs);
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to claim user for premium state reconciliation');
			throw error;
		}
	}

	async removeUser(userId: UserID): Promise<boolean> {
		try {
			const secondaryKey = this.getSecondaryKey(userId);
			return await this.kvClient.removeBulkDeletion(QUEUE_KEY, secondaryKey);
		} catch (error) {
			Logger.error({error, userId: userId.toString()}, 'Failed to remove user from premium reconciliation queue');
			throw error;
		}
	}

	async getReadyUserIds(nowMs: number, limit: number): Promise<Array<UserID>> {
		try {
			const values = await this.kvClient.zrangebyscore(QUEUE_KEY, '-inf', nowMs, 'LIMIT', 0, limit);
			const userIds: Array<UserID> = [];
			for (const value of values) {
				try {
					userIds.push(createUserID(BigInt(value)));
				} catch (error) {
					Logger.warn({error, value}, 'Skipping invalid premium reconciliation queue entry');
				}
			}
			return userIds;
		} catch (error) {
			Logger.error({error, nowMs, limit}, 'Failed to fetch ready premium reconciliation queue entries');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.kvClient.zcard(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get premium reconciliation queue size');
			throw error;
		}
	}
}
