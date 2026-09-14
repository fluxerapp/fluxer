// SPDX-License-Identifier: AGPL-3.0-or-later

import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import type {NatsConnectionOptions} from '@pkgs/nats/src/NatsConnectionOptions';
import {connect, ErrorCode, type NatsConnection, NatsError} from 'nats';

const DEFAULT_MAX_RECONNECT_ATTEMPTS = -1;
const DEFAULT_RECONNECT_TIME_WAIT_MS = 500;
const DEFAULT_CONNECT_TIMEOUT_MS = 5000;
const DEFAULT_DRAIN_TIMEOUT_MS = 5000;
const MAX_TIMER_DELAY_MS = 2_147_483_647;

interface NatsDrainDeadline {
	expired: boolean;
	promise: Promise<never>;
}

export class NatsConnectionManager implements INatsConnectionManager {
	private connection: NatsConnection | null = null;
	private connectPromise: Promise<void> | null = null;
	private drainPromise: Promise<void> | null = null;
	private drainGeneration = 0;
	private readonly options: NatsConnectionOptions;
	private readonly drainTimeoutMs: number;

	constructor(options: NatsConnectionOptions) {
		this.options = options;
		this.drainTimeoutMs = options.drainTimeoutMs ?? DEFAULT_DRAIN_TIMEOUT_MS;
		if (
			!Number.isSafeInteger(this.drainTimeoutMs) ||
			this.drainTimeoutMs < 1 ||
			this.drainTimeoutMs > MAX_TIMER_DELAY_MS
		) {
			throw new RangeError(`NATS drainTimeoutMs must be an integer between 1 and ${MAX_TIMER_DELAY_MS}`);
		}
	}

	async connect(): Promise<void> {
		this.assertNotDraining();
		if (this.connection !== null && !this.connection.isClosed()) {
			return;
		}
		const generation = this.drainGeneration;
		this.connectPromise ??= this.openConnection().finally(() => {
			this.connectPromise = null;
		});
		await this.connectPromise;
		if (generation !== this.drainGeneration) {
			throw NatsError.errorForCode(ErrorCode.ConnectionDraining);
		}
	}

	private async openConnection(): Promise<void> {
		this.connection = await connect({
			servers: this.options.url,
			token: this.options.token || undefined,
			name: this.options.name,
			maxReconnectAttempts: this.options.maxReconnectAttempts ?? DEFAULT_MAX_RECONNECT_ATTEMPTS,
			reconnectTimeWait: this.options.reconnectTimeWaitMs ?? DEFAULT_RECONNECT_TIME_WAIT_MS,
			timeout: this.options.connectTimeoutMs ?? DEFAULT_CONNECT_TIMEOUT_MS,
		});
	}

	getConnection(): NatsConnection {
		this.assertNotDraining();
		if (this.connection === null || this.connection.isClosed()) {
			throw new Error('NATS connection is not established. Call connect() first.');
		}
		return this.connection;
	}

	drain(): Promise<void> {
		if (this.drainPromise !== null) {
			return this.drainPromise;
		}
		if (this.connection === null && this.connectPromise === null) {
			return Promise.resolve();
		}
		this.drainGeneration++;
		const connectPromise = this.connectPromise;
		const timeout = Promise.withResolvers<never>();
		const deadline: NatsDrainDeadline = {expired: false, promise: timeout.promise};
		const timeoutError = new Error(`NATS connection drain exceeded ${this.drainTimeoutMs}ms`);
		const timer = setTimeout(() => {
			deadline.expired = true;
			timeout.reject(timeoutError);
		}, this.drainTimeoutMs);
		const cleanup = Promise.resolve()
			.then(() => this.drainConnection(connectPromise, deadline))
			.catch((error: unknown) => {
				if (deadline.expired && error !== timeoutError) {
					process.emitWarning(new Error('NATS connection cleanup failed after its drain deadline', {cause: error}));
				}
				throw error;
			})
			.finally(() => {
				clearTimeout(timer);
				this.drainPromise = null;
			});
		this.drainPromise = Promise.race([cleanup, deadline.promise]);
		return this.drainPromise;
	}

	private async drainConnection(connectPromise: Promise<void> | null, deadline: NatsDrainDeadline): Promise<void> {
		if (connectPromise !== null) {
			try {
				await connectPromise;
			} catch {
				return;
			}
		}
		const connection = this.connection;
		if (connection === null) {
			return;
		}
		const errors: Array<unknown> = [];
		try {
			if (!deadline.expired && !connection.isClosed()) {
				await Promise.race([connection.drain(), deadline.promise]);
				if (!connection.isClosed()) {
					throw new Error('NATS connection remained open after draining.');
				}
			}
		} catch (error) {
			errors.push(error);
		}
		try {
			await connection.close();
		} catch (error) {
			errors.push(error);
		} finally {
			if (this.connection === connection) {
				this.connection = null;
			}
		}
		if (errors.length === 1) throw errors[0];
		if (errors.length > 1) throw new AggregateError(errors, 'NATS connection drain and close failed');
	}

	private assertNotDraining(): void {
		if (this.drainPromise !== null) {
			throw NatsError.errorForCode(ErrorCode.ConnectionDraining);
		}
	}

	isClosed(): boolean {
		return this.connection === null || this.connection.isClosed();
	}
}
