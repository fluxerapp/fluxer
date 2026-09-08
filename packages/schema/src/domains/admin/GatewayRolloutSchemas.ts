// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

const GatewayRolloutModeEnum = z.enum(['modulo', 'random']);

const VoiceE2EEScopeEnum = z.enum(['guild_feature_only', 'platform_wide']);

export const GatewayRolloutConfigSchema = z.object({
	session_rollout_percentage: z.number().min(0).max(100).default(100),
	session_rollout_mode: GatewayRolloutModeEnum.default('modulo'),
	guild_rollout_percentage: z.number().min(0).max(100).default(100),
	rpc_request_timeout_ms: z.number().int().min(1000).max(60000).default(10000),
	max_concurrent_session_starts: z.number().int().min(1).max(10000).default(512),
	max_concurrent_guild_starts: z.number().int().min(1).max(10000).default(256),
	gateway_dispatch_relay_shards: z.number().int().min(1).max(10000).default(32),
	gateway_dispatch_relay_max_queue: z.number().int().min(0).max(1000000).default(50000),
	voice_e2ee_scope: VoiceE2EEScopeEnum.default('guild_feature_only'),
});

export type GatewayRolloutConfig = z.infer<typeof GatewayRolloutConfigSchema>;

export const GatewayRolloutConfigUpdateRequest = z.object({
	session_rollout_percentage: z.number().min(0).max(100).optional(),
	session_rollout_mode: GatewayRolloutModeEnum.optional(),
	guild_rollout_percentage: z.number().min(0).max(100).optional(),
	rpc_request_timeout_ms: z.number().int().min(1000).max(60000).optional(),
	max_concurrent_session_starts: z.number().int().min(1).max(10000).optional(),
	max_concurrent_guild_starts: z.number().int().min(1).max(10000).optional(),
	gateway_dispatch_relay_shards: z.number().int().min(1).max(10000).optional(),
	gateway_dispatch_relay_max_queue: z.number().int().min(0).max(1000000).optional(),
	voice_e2ee_scope: VoiceE2EEScopeEnum.optional(),
});

export type GatewayRolloutConfigUpdateRequest = z.infer<typeof GatewayRolloutConfigUpdateRequest>;

export const GatewayRolloutConfigResponse = GatewayRolloutConfigSchema;

export type GatewayRolloutConfigResponse = z.infer<typeof GatewayRolloutConfigResponse>;
