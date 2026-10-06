// SPDX-License-Identifier: AGPL-3.0-or-later

import {type JetStreamClient, type JetStreamManager, jetstream, jetstreamManager} from '@nats-io/jetstream';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';

export class JetStreamConnectionManager extends NatsConnectionManager {
	getJetStreamClient(): JetStreamClient {
		return jetstream(this.getConnection());
	}

	async getJetStreamManager(): Promise<JetStreamManager> {
		return jetstreamManager(this.getConnection());
	}
}
