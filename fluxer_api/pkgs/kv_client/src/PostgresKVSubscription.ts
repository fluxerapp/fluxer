// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import type {IKVLogger} from '@pkgs/kv_client/src/KVClientConfig';
import pg from 'pg';

type MessageCallback = (channel: string, message: string) => void;
type ErrorCallback = (error: Error) => void;

interface PostgresKVListenerOptions {
	clientConfig: pg.ClientConfig;
	channel: string;
	refreshMessage: string;
	logger: IKVLogger;
}

export class PostgresKVListener {
	private readonly subscribers = new Set<PostgresKVSubscription>();
	private client: pg.Client | null = null;
	private connecting: Promise<void> | null = null;
	private reconnectTimer: NodeJS.Timeout | null = null;
	private reconnectAttempts = 0;
	private closed = false;

	constructor(private readonly options: PostgresKVListenerOptions) {}

	async acquire(subscription: PostgresKVSubscription): Promise<void> {
		if (this.closed) throw new Error('KV subscription provider is closed');
		this.subscribers.add(subscription);
		try {
			await this.ensureConnected();
		} catch (error) {
			this.subscribers.delete(subscription);
			if (this.subscribers.size === 0) await this.disconnect();
			throw error;
		}
	}

	async release(subscription: PostgresKVSubscription): Promise<void> {
		if (!this.subscribers.delete(subscription) || this.subscribers.size > 0) return;
		await this.disconnect();
	}

	async close(): Promise<void> {
		this.closed = true;
		this.subscribers.clear();
		await this.disconnect();
	}

	private async ensureConnected(): Promise<void> {
		if (this.client !== null) return;
		this.connecting ??= this.connect().finally(() => {
			this.connecting = null;
		});
		await this.connecting;
	}

	private async connect(): Promise<void> {
		const client = new pg.Client(this.options.clientConfig);
		client.on('notification', (notification) => {
			if (this.client === client && notification.channel === this.options.channel) {
				this.deliver(notification.payload ?? '');
			}
		});
		client.on('error', (error) => this.lost(client, error));
		client.on('end', () => this.lost(client, new Error('KV subscription connection ended')));
		try {
			await client.connect();
			await client.query(`LISTEN ${this.options.channel}`);
		} catch (error) {
			client.removeAllListeners('end');
			await client.end().catch(() => undefined);
			throw error;
		}
		if (this.subscribers.size === 0 || this.closed) {
			client.removeAllListeners('end');
			await client.end().catch(() => undefined);
			return;
		}
		this.client = client;
	}

	private async disconnect(): Promise<void> {
		if (this.reconnectTimer !== null) {
			clearTimeout(this.reconnectTimer);
			this.reconnectTimer = null;
		}
		this.reconnectAttempts = 0;
		await this.connecting?.catch(() => undefined);
		const client = this.client;
		this.client = null;
		if (client === null) return;
		client.removeAllListeners('end');
		await client.end().catch(() => undefined);
	}

	private lost(client: pg.Client, error: Error): void {
		if (this.client !== client) return;
		this.client = null;
		client.removeAllListeners('end');
		client.end().catch(() => undefined);
		this.options.logger.error({error}, 'KV subscription connection lost');
		for (const subscriber of this.subscribers) subscriber.fail(error);
		this.scheduleReconnect();
	}

	private scheduleReconnect(): void {
		if (this.closed || this.subscribers.size === 0 || this.reconnectTimer !== null) return;
		this.reconnectAttempts++;
		const delayMs = Math.min(this.reconnectAttempts * 100, 2000);
		this.reconnectTimer = setTimeout(() => {
			this.reconnectTimer = null;
			if (this.closed || this.subscribers.size === 0) return;
			this.ensureConnected().then(
				() => {
					this.reconnectAttempts = 0;
					for (const subscriber of this.subscribers) subscriber.refresh(this.options.refreshMessage);
				},
				(error: unknown) => {
					this.options.logger.error({error}, 'KV subscription reconnect failed');
					this.scheduleReconnect();
				},
			);
		}, delayMs);
		this.reconnectTimer.unref();
	}

	private deliver(payload: string): void {
		let parsed: unknown;
		try {
			parsed = JSON.parse(payload);
		} catch {
			return;
		}
		if (!Array.isArray(parsed) || typeof parsed[0] !== 'string' || typeof parsed[1] !== 'string') return;
		for (const subscriber of this.subscribers) subscriber.deliver(parsed[0], parsed[1]);
	}
}

export class PostgresKVSubscription implements IKVSubscription {
	private readonly channels = new Set<string>();
	private readonly messageCallbacks = new Set<MessageCallback>();
	private readonly errorCallbacks = new Set<ErrorCallback>();
	private attached = false;

	constructor(private readonly listener: PostgresKVListener) {}

	async connect(): Promise<void> {
		if (this.attached) return;
		this.attached = true;
		try {
			await this.listener.acquire(this);
		} catch (error) {
			this.attached = false;
			throw error;
		}
	}

	on(event: 'message', callback: MessageCallback): void;
	on(event: 'error', callback: ErrorCallback): void;
	on(event: 'message' | 'error', callback: MessageCallback | ErrorCallback): void {
		if (event === 'message') this.messageCallbacks.add(callback as MessageCallback);
		else this.errorCallbacks.add(callback as ErrorCallback);
	}

	off(event: 'message', callback: MessageCallback): void;
	off(event: 'error', callback: ErrorCallback): void;
	off(event: 'message' | 'error', callback: MessageCallback | ErrorCallback): void {
		if (event === 'message') this.messageCallbacks.delete(callback as MessageCallback);
		else this.errorCallbacks.delete(callback as ErrorCallback);
	}

	async subscribe(...channels: Array<string>): Promise<void> {
		for (const channel of channels) this.channels.add(channel);
	}

	async quit(): Promise<void> {
		await this.detach();
	}

	async disconnect(): Promise<void> {
		await this.detach();
	}

	deliver(channel: string, message: string): void {
		if (!this.channels.has(channel)) return;
		for (const callback of this.messageCallbacks) callback(channel, message);
	}

	refresh(message: string): void {
		for (const channel of this.channels) {
			for (const callback of this.messageCallbacks) callback(channel, message);
		}
	}

	fail(error: Error): void {
		for (const callback of this.errorCallbacks) callback(error);
	}

	private async detach(): Promise<void> {
		if (!this.attached) return;
		this.attached = false;
		await this.listener.release(this);
	}
}
