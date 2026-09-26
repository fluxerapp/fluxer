// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PushServiceDeliveryConfig} from '@fluxer/schema/src/domains/admin/PushServiceDeliverySchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const textEncoder = new TextEncoder();

export const PUSH_SERVICE_DELIVERY_CONFIG_NATS_SUBJECT = 'config.push.delivery';

interface PushServiceDeliveryConfigNatsMessage {
	type: 'push_service_delivery_config';
	config: PushServiceDeliveryConfig;
}

export class PushServiceDeliveryConfigPublisher {
	constructor(private readonly connectionManager: INatsConnectionManager) {}

	async publish(config: PushServiceDeliveryConfig): Promise<void> {
		if (this.connectionManager.isClosed()) {
			await this.connectionManager.connect();
		}
		const connection = this.connectionManager.getConnection();
		const message: PushServiceDeliveryConfigNatsMessage = {
			type: 'push_service_delivery_config',
			config,
		};
		connection.publish(PUSH_SERVICE_DELIVERY_CONFIG_NATS_SUBJECT, textEncoder.encode(JSON.stringify(message)));
		await connection.flush();
	}
}
