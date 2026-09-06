// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	CreateVoiceRegionRequest,
	CreateVoiceRegionResponse,
	CreateVoiceServerRequest,
	CreateVoiceServerResponse,
	DeleteVoiceResponse,
	GetVoiceRegionQuery,
	GetVoiceRegionResponse,
	GetVoiceServerResponse,
	ListVoiceRegionsQuery,
	ListVoiceRegionsResponse,
	ListVoiceServersResponse,
	UpdateVoiceRegionRequest,
	UpdateVoiceRegionResponse,
	UpdateVoiceServerRequest,
	UpdateVoiceServerResponse,
	VoiceRegionIdParam,
	VoiceServerIdParam,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import type {Context} from 'hono';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export function VoiceAdminController(app: HonoApp) {
	app.get(
		'/admin/voice/regions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_REGION_LIST),
		Validator('query', ListVoiceRegionsQuery),
		OpenAPI({
			operationId: 'list_admin_voice_regions',
			summary: 'List voice regions',
			responseSchema: ListVoiceRegionsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists all configured voice server regions with status and server count. Shows region names, latency info, and availability. Requires VOICE_REGION_LIST permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.voiceService.listVoiceRegions(ctx.req.valid('query')));
		},
	);
	app.post(
		'/admin/voice/regions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_REGION_CREATE),
		Validator('json', CreateVoiceRegionRequest),
		OpenAPI({
			operationId: 'create_admin_voice_region',
			summary: 'Create voice region',
			responseSchema: CreateVoiceRegionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Creates a new voice server region. Defines geographic location and performance characteristics for voice routing. Creates audit log entry. Requires VOICE_REGION_CREATE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.voiceService.createVoiceRegion(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.get(
		'/admin/voice/regions/:region_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_REGION_LIST),
		Validator('param', VoiceRegionIdParam),
		Validator('query', GetVoiceRegionQuery),
		OpenAPI({
			operationId: 'get_admin_voice_region',
			summary: 'Get voice region',
			responseSchema: GetVoiceRegionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Gets detailed information about a voice region including assigned servers, capacity, and server details. Requires VOICE_REGION_LIST permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(
				await adminService.voiceService.getVoiceRegion({
					id: ctx.req.valid('param').region_id,
					include_servers: ctx.req.valid('query').include_servers,
				}),
			);
		},
	);
	app.patch(
		'/admin/voice/regions/:region_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_REGION_UPDATE),
		Validator('param', VoiceRegionIdParam),
		Validator('json', UpdateVoiceRegionRequest, {
			pre: (value: unknown, ctx: Context<HonoEnv>) => ({
				...(isPlainObject(value) ? value : {}),
				id: ctx.req.param('region_id'),
			}),
		}),
		OpenAPI({
			operationId: 'update_admin_voice_region',
			summary: 'Update voice region',
			responseSchema: UpdateVoiceRegionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Updates voice region settings such as latency thresholds or priority. Changes affect voice routing for new sessions. Creates audit log entry. Requires VOICE_REGION_UPDATE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.voiceService.updateVoiceRegion(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.delete(
		'/admin/voice/regions/:region_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_REGION_DELETE),
		Validator('param', VoiceRegionIdParam),
		OpenAPI({
			operationId: 'delete_admin_voice_region',
			summary: 'Delete voice region',
			responseSchema: DeleteVoiceResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Deletes a voice region. Removes region from routing and reassigns active connections. Creates audit log entry. Requires VOICE_REGION_DELETE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.voiceService.deleteVoiceRegion(
					{id: ctx.req.valid('param').region_id},
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.get(
		'/admin/voice/regions/:region_id/servers',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_SERVER_LIST),
		Validator('param', VoiceRegionIdParam),
		OpenAPI({
			operationId: 'list_admin_voice_servers',
			summary: 'List voice servers',
			responseSchema: ListVoiceServersResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists all voice servers in a region with connection counts and capacity. Shows server status, region assignment, and load information. Requires VOICE_SERVER_LIST permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.voiceService.listVoiceServers({region_id: ctx.req.valid('param').region_id}));
		},
	);
	app.post(
		'/admin/voice/regions/:region_id/servers',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_SERVER_CREATE),
		Validator('param', VoiceRegionIdParam),
		Validator('json', CreateVoiceServerRequest, {
			pre: (value: unknown, ctx: Context<HonoEnv>) => ({
				...(isPlainObject(value) ? value : {}),
				region_id: ctx.req.param('region_id'),
			}),
		}),
		OpenAPI({
			operationId: 'create_admin_voice_server',
			summary: 'Create voice server',
			responseSchema: CreateVoiceServerResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Creates and provisions a new voice server instance in a region. Configures capacity, codecs, and encryption. Creates audit log entry. Requires VOICE_SERVER_CREATE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.voiceService.createVoiceServer(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.get(
		'/admin/voice/regions/:region_id/servers/:server_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_SERVER_LIST),
		Validator('param', VoiceServerIdParam),
		OpenAPI({
			operationId: 'get_admin_voice_server',
			summary: 'Get voice server',
			responseSchema: GetVoiceServerResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Gets detailed voice server information including active connections and configuration. Requires VOICE_SERVER_LIST permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {region_id, server_id} = ctx.req.valid('param');
			return ctx.json(await adminService.voiceService.getVoiceServer({region_id, server_id}));
		},
	);
	app.patch(
		'/admin/voice/regions/:region_id/servers/:server_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_SERVER_UPDATE),
		Validator('param', VoiceServerIdParam),
		Validator('json', UpdateVoiceServerRequest, {
			pre: (value: unknown, ctx: Context<HonoEnv>) => ({
				...(isPlainObject(value) ? value : {}),
				region_id: ctx.req.param('region_id'),
				server_id: ctx.req.param('server_id'),
			}),
		}),
		OpenAPI({
			operationId: 'update_admin_voice_server',
			summary: 'Update voice server',
			responseSchema: UpdateVoiceServerResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Updates voice server configuration including capacity, region assignment, and quality settings. Changes apply to new connections. Creates audit log entry. Requires VOICE_SERVER_UPDATE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.voiceService.updateVoiceServer(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.delete(
		'/admin/voice/regions/:region_id/servers/:server_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_SERVER_DELETE),
		Validator('param', VoiceServerIdParam),
		OpenAPI({
			operationId: 'delete_admin_voice_server',
			summary: 'Delete voice server',
			responseSchema: DeleteVoiceResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Decommissions and removes a voice server instance. Disconnects active sessions and migrates to other servers. Creates audit log entry. Requires VOICE_SERVER_DELETE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {region_id, server_id} = ctx.req.valid('param');
			return ctx.json(
				await adminService.voiceService.deleteVoiceServer({region_id, server_id}, adminUserId, auditLogReason),
			);
		},
	);
}
