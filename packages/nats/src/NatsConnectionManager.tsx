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

import type {INatsConnectionManager} from '@fluxer/nats/src/INatsConnectionManager';
import type {NatsConnectionOptions} from '@fluxer/nats/src/NatsConnectionOptions';
import {connect, type NatsConnection} from 'nats';

const DEFAULT_MAX_RECONNECT_ATTEMPTS = -1;
const DEFAULT_RECONNECT_TIME_WAIT_MS = 500;
const DEFAULT_CONNECT_TIMEOUT_MS = 5000;

export class NatsConnectionManager implements INatsConnectionManager {
	private connection: NatsConnection | null = null;
	private readonly options: NatsConnectionOptions;

	constructor(options: NatsConnectionOptions) {
		this.options = options;
	}

	async connect(): Promise<void> {
		if (this.connection !== null && !this.connection.isClosed()) {
			return;
		}

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
		if (this.connection === null || this.connection.isClosed()) {
			throw new Error('NATS connection is not established. Call connect() first.');
		}
		return this.connection;
	}

	async drain(): Promise<void> {
		if (this.connection === null) {
			return;
		}
		if (!this.connection.isClosed()) {
			await this.connection.drain();
		}
		this.connection = null;
	}

	isClosed(): boolean {
		return this.connection === null || this.connection.isClosed();
	}
}
