// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

const GatewayRolloutModeEnum = z.enum(['modulo', 'random']);

const VoiceE2EEScopeEnum = z.enum(['guild_feature_only', 'platform_wide']);

const gatewayRolloutFields = {
	session_rollout_percentage: z
		.number()
		.min(0)
		.max(100)
		.describe('Percentage of accounts that can connect to the gateway, where 0 pauses every new connection'),
	session_rollout_mode: GatewayRolloutModeEnum.describe(
		'How the gateway picks accounts below 100 percent, modulo for the same accounts each time or random for a new pick at each connection',
	),
	guild_rollout_percentage: z
		.number()
		.min(0)
		.max(100)
		.describe('Percentage of communities the gateway starts, from 0 for none to 100 for all of them'),
	rpc_request_timeout_ms: z
		.number()
		.int()
		.min(1000)
		.max(60000)
		.describe('Milliseconds the gateway waits for the API to respond to one request'),
	max_concurrent_session_starts: z
		.number()
		.int()
		.min(1)
		.max(10000)
		.describe('Most connections each gateway node sets up at once, with the rest waiting in a queue'),
	max_concurrent_guild_starts: z
		.number()
		.int()
		.min(1)
		.max(10000)
		.describe('Most communities each gateway node starts at once, with the rest waiting their turn'),
	gateway_dispatch_relay_shards: z
		.number()
		.int()
		.min(1)
		.max(10000)
		.describe(
			'Number of dispatch relay workers on each gateway node that send events to connected sessions, which changes only when the gateway restarts',
		),
	gateway_dispatch_relay_max_queue: z
		.number()
		.int()
		.min(0)
		.max(1000000)
		.describe("Most events one dispatch relay worker queues before senders wait, where 0 uses the gateway's own limit"),
	voice_e2ee_scope: VoiceE2EEScopeEnum.describe(
		'Which calls encrypt their media with a key that your instance hands out, guild_feature_only for communities with the VOICE_E2EE feature or platform_wide for every call',
	),
};

export const GatewayRolloutConfigSchema = z.object({
	session_rollout_percentage: gatewayRolloutFields.session_rollout_percentage.default(100),
	session_rollout_mode: gatewayRolloutFields.session_rollout_mode.default('modulo'),
	guild_rollout_percentage: gatewayRolloutFields.guild_rollout_percentage.default(100),
	rpc_request_timeout_ms: gatewayRolloutFields.rpc_request_timeout_ms.default(10000),
	max_concurrent_session_starts: gatewayRolloutFields.max_concurrent_session_starts.default(512),
	max_concurrent_guild_starts: gatewayRolloutFields.max_concurrent_guild_starts.default(256),
	gateway_dispatch_relay_shards: gatewayRolloutFields.gateway_dispatch_relay_shards.default(32),
	gateway_dispatch_relay_max_queue: gatewayRolloutFields.gateway_dispatch_relay_max_queue.default(50000),
	voice_e2ee_scope: gatewayRolloutFields.voice_e2ee_scope.default('guild_feature_only'),
});

export type GatewayRolloutConfig = z.infer<typeof GatewayRolloutConfigSchema>;

export const GatewayRolloutConfigUpdateRequest = z.object(gatewayRolloutFields).partial();

export type GatewayRolloutConfigUpdateRequest = z.infer<typeof GatewayRolloutConfigUpdateRequest>;

export const GatewayRolloutConfigResponse = GatewayRolloutConfigSchema;

export type GatewayRolloutConfigResponse = z.infer<typeof GatewayRolloutConfigResponse>;
