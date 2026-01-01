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

import {randomUUID} from 'node:crypto';
import type {Redis} from 'ioredis';
import {FLUXER_EPOCH} from '~/Constants';

const MAX_SEQ = 4095;
const MAX_NODE_ID = 1023;
const TIMESTAMP_SHIFT = 22n;
const NODE_ID_SHIFT = 12n;
const NODE_ID_TTL = 300;
const NODE_ID_RENEWAL_INTERVAL = 240;

class SnowflakeService {
	private epoch: bigint;
	private nodeId: number | null = null;
	private seq: number;
	private lastSeqExhaustion: bigint;
	private redis: Redis | null = null;
	private instanceId: string;
	private renewalInterval: NodeJS.Timeout | null = null;
	private initializationPromise: Promise<void> | null = null;

	constructor(redis?: Redis) {
		this.epoch = BigInt(FLUXER_EPOCH);
		this.seq = 0;
		this.lastSeqExhaustion = 0n;
		this.instanceId = randomUUID();

		if (redis) {
			this.redis = redis;
		}
	}

	async initialize(): Promise<void> {
		if (this.nodeId != null) {
			return;
		}

		if (!this.initializationPromise) {
			this.initializationPromise = (async () => {
				if (this.redis) {
					this.nodeId = await this.acquireNodeId();
					this.startNodeIdRenewal();
				} else {
					this.nodeId = 0;
				}
			})().finally(() => {
				this.initializationPromise = null;
			});
		}

		await this.initializationPromise;
	}

	private async acquireNodeId(): Promise<number> {
		if (!this.redis) {
			throw new Error('Redis not available for node ID allocation');
		}

		const nodeIdKey = 'snowflake:node_counter';
		const nodeRegistryKey = 'snowflake:nodes';

		for (let attempt = 0; attempt < MAX_NODE_ID; attempt++) {
			const candidateId = await this.redis.incr(nodeIdKey);
			const normalizedId = (candidateId - 1) % (MAX_NODE_ID + 1);

			const lockKey = `snowflake:node:${normalizedId}`;
			const acquired = await this.redis.set(lockKey, this.instanceId, 'EX', NODE_ID_TTL, 'NX');

			if (acquired === 'OK') {
				await this.redis.hset(nodeRegistryKey, normalizedId, this.instanceId);
				return normalizedId;
			}
		}

		throw new Error('Unable to acquire unique node ID - all nodes in use');
	}

	private startNodeIdRenewal(): void {
		if (this.renewalInterval) {
			return;
		}

		this.renewalInterval = setInterval(async () => {
			await this.renewNodeId();
		}, NODE_ID_RENEWAL_INTERVAL * 1000);
	}

	private async renewNodeId(): Promise<void> {
		if (!this.redis || this.nodeId == null) return;

		const lockKey = `snowflake:node:${this.nodeId}`;
		await this.redis.expire(lockKey, NODE_ID_TTL);
	}

	async shutdown(): Promise<void> {
		if (this.renewalInterval) {
			clearInterval(this.renewalInterval);
			this.renewalInterval = null;
		}

		const nodeId = this.nodeId;

		if (this.redis && nodeId != null) {
			const lockKey = `snowflake:node:${nodeId}`;
			const nodeRegistryKey = 'snowflake:nodes';

			this.nodeId = null;

			try {
				await this.redis.del(lockKey);
				await this.redis.hdel(nodeRegistryKey, nodeId.toString());
			} catch (_err) {}
		} else {
			this.nodeId = null;
		}
	}

	public generate(): bigint {
		if (this.nodeId == null) {
			throw new Error('SnowflakeService not initialized - call initialize() first');
		}

		const currentTime = BigInt(Date.now());
		return this.generateWithTimestamp(currentTime);
	}

	private generateWithTimestamp(timestamp: bigint): bigint {
		if (this.nodeId == null) {
			throw new Error('SnowflakeService not initialized - call initialize() first');
		}

		while (this.seq === 0 && timestamp <= this.lastSeqExhaustion) {
			this.sleep(1);
			timestamp = BigInt(Date.now());
		}

		const epochDiff = timestamp - this.epoch;
		const snowflakeId = (epochDiff << TIMESTAMP_SHIFT) | (BigInt(this.nodeId) << NODE_ID_SHIFT) | BigInt(this.seq);

		if (this.seq >= MAX_SEQ) {
			this.seq = 0;
			this.lastSeqExhaustion = timestamp;
		} else {
			this.seq += 1;
		}

		return snowflakeId;
	}

	private sleep(milliseconds: number): void {
		const start = Date.now();
		while (Date.now() - start < milliseconds) {}
	}
}

export {SnowflakeService};
