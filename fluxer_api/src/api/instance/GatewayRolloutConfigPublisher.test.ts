// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayRolloutConfig} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {type NatsConnection, StringCodec} from 'nats';
import {describe, expect, it} from 'vitest';
import {GATEWAY_ROLLOUT_CONFIG_NATS_SUBJECT, GatewayRolloutConfigPublisher} from './GatewayRolloutConfigPublisher';

interface FakePublish {
	subject: string;
	body: Record<string, unknown>;
}

class FakeNatsConnectionManager implements INatsConnectionManager {
	private readonly codec = StringCodec();
	private closed = true;
	readonly publishes: Array<FakePublish> = [];
	connectCalls = 0;
	flushCalls = 0;

	async connect(): Promise<void> {
		this.connectCalls += 1;
		this.closed = false;
	}

	getConnection(): NatsConnection {
		if (this.closed) {
			throw new Error('not connected');
		}
		return {
			publish: (subject: string, data: Uint8Array) => {
				this.publishes.push({
					subject,
					body: JSON.parse(this.codec.decode(data)) as Record<string, unknown>,
				});
			},
			flush: async () => {
				this.flushCalls += 1;
			},
		} as unknown as NatsConnection;
	}

	async drain(): Promise<void> {
		this.closed = true;
	}

	isClosed(): boolean {
		return this.closed;
	}
}

describe('GatewayRolloutConfigPublisher', () => {
	it('publishes the validated rollout config on the gateway rollout subject', async () => {
		const manager = new FakeNatsConnectionManager();
		const publisher = new GatewayRolloutConfigPublisher(manager);
		const config: GatewayRolloutConfig = {
			session_rollout_percentage: 25,
			session_rollout_mode: 'modulo',
			guild_rollout_percentage: 50,
			rpc_request_timeout_ms: 10000,
			max_concurrent_session_starts: 512,
			max_concurrent_guild_starts: 256,
			gateway_dispatch_relay_shards: 32,
			gateway_dispatch_relay_max_queue: 50000,
			voice_e2ee_scope: 'guild_feature_only',
		};

		await publisher.publish(config);

		expect(manager.connectCalls).toBe(1);
		expect(manager.flushCalls).toBe(1);
		expect(manager.publishes).toEqual([
			{
				subject: GATEWAY_ROLLOUT_CONFIG_NATS_SUBJECT,
				body: {
					type: 'gateway_rollout_config',
					config,
				},
			},
		]);
	});
});
