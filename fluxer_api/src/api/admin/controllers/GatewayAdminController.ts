// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {GetProcessMemoryStatsQuery} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {
	GatewayVoiceStateCountsResponse,
	GuildMemoryStatsResponse,
	NodeStatsResponse,
	ReloadAllGuildsResponse,
	ReloadGuildsRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';

export function GatewayAdminController(app: HonoApp) {
	app.get(
		'/admin/gateway/stats',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GATEWAY_MEMORY_STATS),
		OpenAPI({
			operationId: 'get_admin_gateway_stats',
			summary: 'Get gateway node statistics',
			description:
				'Returns uptime, process memory, and guild count. Used to monitor gateway health and performance. Requires GATEWAY_MEMORY_STATS permission.',
			responseSchema: NodeStatsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.guildServiceAggregate.managementService.getNodeStats());
		},
	);
	app.get(
		'/admin/gateway/memory-stats',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GATEWAY_MEMORY_STATS),
		Validator('query', GetProcessMemoryStatsQuery),
		OpenAPI({
			operationId: 'get_admin_gateway_memory_stats',
			summary: 'Get guild memory statistics',
			description: 'Returns heap and resident memory usage per guild. Requires GATEWAY_MEMORY_STATS permission.',
			responseSchema: GuildMemoryStatsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {limit} = ctx.req.valid('query');
			return ctx.json(await adminService.guildServiceAggregate.managementService.getGuildMemoryStats(limit));
		},
	);
	app.get(
		'/admin/gateway/voice-state-counts',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GATEWAY_MEMORY_STATS),
		OpenAPI({
			operationId: 'get_admin_gateway_voice_state_counts',
			summary: 'Get gateway voice state counts',
			description:
				'Returns active voice state counts grouped by voice region and voice server. Requires GATEWAY_MEMORY_STATS permission.',
			responseSchema: GatewayVoiceStateCountsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.guildServiceAggregate.managementService.getVoiceStateCounts());
		},
	);
	app.post(
		'/admin/gateway/reloads',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GATEWAY_RELOAD),
		requireAdminACL(AdminACLs.GATEWAY_RELOAD_ALL),
		Validator('json', ReloadGuildsRequest),
		OpenAPI({
			operationId: 'create_admin_gateway_reload',
			summary: 'Reload gateway guilds',
			description:
				'Reconnects to the database and re-syncs guild state. Used for recovery after data inconsistencies. Requires GATEWAY_RELOAD_ALL permission.',
			responseSchema: ReloadAllGuildsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const guildIds = body.guild_ids.map((id) => createGuildID(id));
			const result = await adminService.guildServiceAggregate.managementService.reloadAllGuilds(guildIds);
			await adminService.auditService.createAuditLog({
				adminUserId,
				targetType: 'guild',
				targetId: BigInt(0),
				action: 'reload_guilds',
				auditLogReason,
				metadata: new Map([
					['guild_count', guildIds.length.toString()],
					['reloaded', result.count.toString()],
				]),
			});
			return ctx.json(result);
		},
	);
}
