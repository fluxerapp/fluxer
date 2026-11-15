// SPDX-License-Identifier: AGPL-3.0-or-later
import type {GatewayRolloutConfig} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import type {LegacyPushServiceDeliveryWire} from '@fluxer/schema/src/domains/admin/PushRelaySchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const textEncoder = new TextEncoder();
type ConfigMessage =
	| [subject: 'config.gateway.rollout', type: 'gateway_rollout_config', config: GatewayRolloutConfig]
	| [subject: 'config.push.delivery', type: 'push_service_delivery_config', config: LegacyPushServiceDeliveryWire];
export class NatsConfigPublisher {
	constructor(private readonly connectionManager: INatsConnectionManager) {}
	async publish(...[subject, type, config]: ConfigMessage): Promise<void> {
		if (this.connectionManager.isClosed()) await this.connectionManager.connect();
		const connection = this.connectionManager.getConnection();
		connection.publish(subject, textEncoder.encode(JSON.stringify({type, config})));
		await connection.flush();
	}
}
