// SPDX-License-Identifier: AGPL-3.0-or-later

export interface NatsConnectionOptions {
	url: string;
	token?: string;
	name?: string;
	maxReconnectAttempts?: number;
	reconnectTimeWaitMs?: number;
	connectTimeoutMs?: number;
	drainTimeoutMs?: number;
}
