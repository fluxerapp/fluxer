// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

const GatewayRolloutModeEnum = z.enum(['modulo', 'random']);

const VoiceE2EEScopeEnum = z.enum(['guild_feature_only', 'platform_wide']);

const gatewayRolloutFields = {
	session_rollout_percentage: z.number().min(0).max(100),
	session_rollout_mode: GatewayRolloutModeEnum,
	guild_rollout_percentage: z.number().min(0).max(100),
	rpc_request_timeout_ms: z.number().int().min(1000).max(60000),
	max_concurrent_session_starts: z.number().int().min(1).max(10000),
	max_concurrent_guild_starts: z.number().int().min(1).max(10000),
	gateway_dispatch_relay_shards: z.number().int().min(1).max(10000),
	gateway_dispatch_relay_max_queue: z.number().int().min(0).max(1000000),
	voice_e2ee_scope: VoiceE2EEScopeEnum,
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
