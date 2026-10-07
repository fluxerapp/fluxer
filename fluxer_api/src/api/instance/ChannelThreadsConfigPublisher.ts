// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelThreadsConfig} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const textEncoder = new TextEncoder();

export const CHANNEL_THREADS_CONFIG_NATS_SUBJECT = 'config.channel.threads';

interface ChannelThreadsConfigNatsMessage {
	type: 'channel_threads_config';
	config: ChannelThreadsConfig;
}

export class ChannelThreadsConfigPublisher {
	constructor(private readonly connectionManager: INatsConnectionManager) {}

	async publish(config: ChannelThreadsConfig): Promise<void> {
		if (this.connectionManager.isClosed()) {
			await this.connectionManager.connect();
		}
		const connection = this.connectionManager.getConnection();
		const message: ChannelThreadsConfigNatsMessage = {
			type: 'channel_threads_config',
			config,
		};
		connection.publish(CHANNEL_THREADS_CONFIG_NATS_SUBJECT, textEncoder.encode(JSON.stringify(message)));
		await connection.flush();
	}
}
