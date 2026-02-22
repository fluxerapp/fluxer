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

import type {IKVSubscription} from '@fluxer/kv_client/src/IKVProvider';
import type {IKVLogger, KVClientMode, KVClusterNode} from '@fluxer/kv_client/src/KVClientConfig';
import Redis from 'ioredis';

interface KVSubscriptionConfig {
	url: string;
	mode?: KVClientMode;
	clusterNodes?: Array<KVClusterNode>;
	timeoutMs: number;
	logger: IKVLogger;
}

export class KVSubscription implements IKVSubscription {
	private readonly url: string;
	private readonly mode: KVClientMode;
	private readonly clusterNodes: Array<KVClusterNode>;
	private readonly timeoutMs: number;
	private readonly logger: IKVLogger;
	private readonly channels: Set<string> = new Set();
	private readonly messageCallbacks: Set<(channel: string, message: string) => void> = new Set();
	private readonly errorCallbacks: Set<(error: Error) => void> = new Set();
	private client: Redis | null = null;

	constructor(config: KVSubscriptionConfig) {
		this.url = config.url;
		this.mode = config.mode ?? 'standalone';
		this.clusterNodes = config.clusterNodes ?? [];
		this.timeoutMs = config.timeoutMs;
		this.logger = config.logger;
	}

	async connect(): Promise<void> {
		if (this.client !== null) {
			return;
		}

		const connectionUrl = this.resolveSubscriptionUrl();
		const client = new Redis(connectionUrl, {
			autoResubscribe: true,
			connectTimeout: this.timeoutMs,
			commandTimeout: this.timeoutMs,
			maxRetriesPerRequest: 1,
			retryStrategy: createRetryStrategy(),
		});

		client.on('message', (channel: string, message: string) => {
			for (const callback of this.messageCallbacks) {
				callback(channel, message);
			}
		});

		client.on('error', (error: Error) => {
			this.logger.error({error}, 'KV subscription error');
			for (const callback of this.errorCallbacks) {
				callback(error);
			}
		});

		this.client = client;

		if (this.channels.size > 0) {
			await this.client.subscribe(...Array.from(this.channels));
		}
	}

	on(event: 'message', callback: (channel: string, message: string) => void): void;
	on(event: 'error', callback: (error: Error) => void): void;
	on(
		event: 'message' | 'error',
		callback: ((channel: string, message: string) => void) | ((error: Error) => void),
	): void {
		if (event === 'message') {
			this.messageCallbacks.add(callback as (channel: string, message: string) => void);
			return;
		}

		this.errorCallbacks.add(callback as (error: Error) => void);
	}

	async subscribe(...channels: Array<string>): Promise<void> {
		const newChannels = channels.filter((channel) => {
			if (this.channels.has(channel)) {
				return false;
			}
			this.channels.add(channel);
			return true;
		});

		if (newChannels.length === 0 || this.client === null) {
			return;
		}

		await this.client.subscribe(...newChannels);
	}

	async unsubscribe(...channels: Array<string>): Promise<void> {
		const removedChannels = channels.filter((channel) => this.channels.delete(channel));

		if (removedChannels.length === 0 || this.client === null) {
			return;
		}

		await this.client.unsubscribe(...removedChannels);
	}

	async quit(): Promise<void> {
		const client = this.client;
		this.client = null;
		if (client === null) {
			return;
		}

		await client.quit();
	}

	async disconnect(): Promise<void> {
		const client = this.client;
		this.client = null;
		if (client === null) {
			return;
		}

		client.disconnect(false);
	}

	removeAllListeners(event?: 'message' | 'error'): void {
		if (!event || event === 'message') {
			this.messageCallbacks.clear();
		}

		if (!event || event === 'error') {
			this.errorCallbacks.clear();
		}
	}

	private resolveSubscriptionUrl(): string {
		if (this.mode !== 'cluster' || this.clusterNodes.length === 0) {
			return this.url;
		}

		const node = this.clusterNodes[0];
		return `redis://${node.host}:${node.port}`;
	}
}

function createRetryStrategy(): (times: number) => number {
	return (times: number) => {
		const backoffMs = Math.min(times * 100, 2000);
		return backoffMs;
	};
}
