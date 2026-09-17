// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import type {IKVLogger, KVClientMode, KVClusterNode} from '@pkgs/kv_client/src/KVClientConfig';
import {resolveKVClusterConnection} from '@pkgs/kv_client/src/KVClusterConnection';
import Redis, {type RedisOptions} from 'ioredis';

interface KVSubscriptionConfig {
	url: string;
	mode?: KVClientMode;
	clusterNodes?: Array<KVClusterNode>;
	timeoutMs: number;
	logger: IKVLogger;
}

interface KVSubscriptionConnect {
	completion: Promise<void>;
	controller: AbortController;
}

export class KVSubscription implements IKVSubscription {
	private readonly url: string;
	private readonly mode: KVClientMode;
	private readonly clusterNodes: Array<KVClusterNode>;
	private readonly timeoutMs: number;
	private readonly logger: IKVLogger;
	private readonly desiredChannels = new Set<string>();
	private readonly messageCallbacks: Set<(channel: string, message: string) => void> = new Set();
	private readonly errorCallbacks: Set<(error: Error) => void> = new Set();
	private client: Redis | null = null;
	private connecting: KVSubscriptionConnect | null = null;
	private closing: Promise<void> | null = null;

	constructor(config: KVSubscriptionConfig) {
		this.url = config.url;
		this.mode = config.mode ?? 'standalone';
		this.clusterNodes = config.clusterNodes ?? [];
		this.timeoutMs = config.timeoutMs;
		this.logger = config.logger;
	}

	async connect(): Promise<void> {
		this.assertNotClosing();
		if (this.connecting !== null) {
			return await this.connecting.completion;
		}
		if (this.client?.status === 'ready') {
			return;
		}
		if (this.client === null || this.client.status === 'end') {
			this.client = this.createClient();
		}
		const controller = new AbortController();
		const connecting: KVSubscriptionConnect = {
			completion: this.connectClient(this.client, controller.signal),
			controller,
		};
		this.connecting = connecting;
		try {
			await connecting.completion;
		} finally {
			controller.abort();
			if (this.connecting === connecting) {
				this.connecting = null;
			}
		}
	}

	private createClient(): Redis {
		const options: RedisOptions = {
			autoResubscribe: true,
			connectTimeout: this.timeoutMs,
			commandTimeout: this.timeoutMs,
			maxRetriesPerRequest: 1,
			protocol: 2,
			retryStrategy: createRetryStrategy(),
		};
		const connection = this.mode === 'cluster' ? resolveKVClusterConnection(this.url, this.clusterNodes) : null;
		const client = connection
			? new Redis({...connection.redisOptions, ...connection.nodes[0], db: 0, ...options})
			: new Redis(this.url, options);
		client.on('message', (channel: string, message: string) => {
			if (this.client !== client || this.closing !== null) {
				return;
			}
			for (const callback of this.messageCallbacks) {
				callback(channel, message);
			}
		});
		client.on('error', (error: Error) => {
			if (this.client !== client || this.closing !== null) {
				return;
			}
			this.logger.error({error}, 'KV subscription error');
			for (const callback of this.errorCallbacks) {
				callback(error);
			}
		});
		return client;
	}

	private async connectClient(client: Redis, signal: AbortSignal): Promise<void> {
		try {
			const ready = this.waitForReady(client, signal);
			if (client.status === 'wait') {
				await Promise.all([ready, client.connect()]);
			} else {
				await ready;
			}
			this.assertCurrentClient(client);
			if (this.desiredChannels.size > 0) {
				await client.subscribe(...this.desiredChannels);
				this.assertCurrentClient(client);
			}
		} catch (error) {
			if (this.client === client && this.closing === null) {
				this.client = null;
				client.disconnect(false);
			}
			throw error;
		}
	}

	private waitForReady(client: Redis, signal: AbortSignal): Promise<void> {
		if (signal.aborted) {
			return Promise.reject(signal.reason);
		}
		if (client.status === 'ready') {
			return Promise.resolve();
		}
		return new Promise((resolve, reject) => {
			const finish = (error?: unknown): void => {
				clearTimeout(timer);
				client.off('ready', ready);
				client.off('end', ended);
				signal.removeEventListener('abort', aborted);
				if (error === undefined) {
					resolve();
				} else {
					reject(error);
				}
			};
			const ready = (): void => finish();
			const ended = (): void => finish(new Error('KV subscription connection was closed'));
			const aborted = (): void => finish(signal.reason);
			const timer = setTimeout(() => finish(new Error('KV subscription connection timed out')), this.timeoutMs);
			timer.unref();
			client.once('ready', ready);
			client.once('end', ended);
			signal.addEventListener('abort', aborted, {once: true});
		});
	}

	private assertNotClosing(): void {
		if (this.closing !== null) {
			throw new Error('KV subscription is closing');
		}
	}

	private assertCurrentClient(client: Redis): void {
		if (this.client !== client || this.closing !== null) {
			throw new Error('KV subscription connection was closed');
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

	off(event: 'message', callback: (channel: string, message: string) => void): void;
	off(event: 'error', callback: (error: Error) => void): void;
	off(
		event: 'message' | 'error',
		callback: ((channel: string, message: string) => void) | ((error: Error) => void),
	): void {
		if (event === 'message') {
			this.messageCallbacks.delete(callback as (channel: string, message: string) => void);
			return;
		}
		this.errorCallbacks.delete(callback as (error: Error) => void);
	}

	async subscribe(...channels: Array<string>): Promise<void> {
		this.assertNotClosing();
		const requestedChannels = [...new Set(channels)];
		for (const channel of requestedChannels) {
			this.desiredChannels.add(channel);
		}
		const client = this.client;
		if (requestedChannels.length === 0 || client === null) {
			return;
		}
		await client.subscribe(...requestedChannels);
		this.assertCurrentClient(client);
	}

	async unsubscribe(...channels: Array<string>): Promise<void> {
		this.assertNotClosing();
		const requestedChannels = [...new Set(channels)];
		for (const channel of requestedChannels) {
			this.desiredChannels.delete(channel);
		}
		const client = this.client;
		if (requestedChannels.length === 0 || client === null) {
			return;
		}
		await client.unsubscribe(...requestedChannels);
		this.assertCurrentClient(client);
	}

	async quit(): Promise<void> {
		if (this.closing !== null) {
			return await this.closing;
		}
		const client = this.client;
		if (client === null) {
			return;
		}
		const closing = Promise.resolve().then(() => this.closeClient(client));
		this.closing = closing;
		this.connecting?.controller.abort(new Error('KV subscription connection was closed'));
		try {
			await closing;
		} finally {
			if (this.client === client) {
				this.client = null;
			}
			this.connecting = null;
			if (this.closing === closing) {
				this.closing = null;
			}
		}
	}

	private async closeClient(client: Redis): Promise<void> {
		try {
			if (this.client === client && client.status !== 'end') {
				await client.quit();
			}
		} finally {
			client.disconnect(false);
		}
	}

	async disconnect(): Promise<void> {
		const client = this.client;
		this.client = null;
		this.connecting?.controller.abort(new Error('KV subscription connection was closed'));
		this.connecting = null;
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
}

function createRetryStrategy(): (times: number) => number {
	return (times) => Math.min(times * 100, 2000);
}
