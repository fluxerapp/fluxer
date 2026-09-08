// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {GatewayRolloutConfigSchema, GatewayRolloutConfigUpdateRequest} from '../admin/GatewayRolloutSchemas';

describe('gateway rollout schemas', () => {
	test('full config applies defaults for omitted values', () => {
		expect(GatewayRolloutConfigSchema.parse({})).toMatchObject({
			session_rollout_percentage: 100,
			session_rollout_mode: 'modulo',
			rpc_request_timeout_ms: 10000,
			gateway_dispatch_relay_shards: 32,
			gateway_dispatch_relay_max_queue: 50000,
			voice_e2ee_scope: 'guild_feature_only',
		});
	});
	test('update request remains partial and does not inject defaults', () => {
		expect(
			GatewayRolloutConfigUpdateRequest.parse({
				rpc_request_timeout_ms: 5000,
				gateway_dispatch_relay_shards: 16,
			}),
		).toEqual({
			rpc_request_timeout_ms: 5000,
			gateway_dispatch_relay_shards: 16,
		});
	});
	test('voice e2ee scope accepts only known modes', () => {
		expect(GatewayRolloutConfigUpdateRequest.parse({voice_e2ee_scope: 'platform_wide'})).toEqual({
			voice_e2ee_scope: 'platform_wide',
		});
		expect(() => GatewayRolloutConfigUpdateRequest.parse({voice_e2ee_scope: 'guilds'})).toThrow();
	});
});
