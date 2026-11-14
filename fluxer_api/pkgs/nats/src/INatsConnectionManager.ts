// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NatsConnection} from '@nats-io/transport-node';

export interface INatsConnectionManager {
	connect(): Promise<void>;
	getConnection(): NatsConnection;
	drain(): Promise<void>;
	isClosed(): boolean;
}
