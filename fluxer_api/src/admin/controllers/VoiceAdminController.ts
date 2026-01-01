/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {HonoApp} from '~/App';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Validator} from '~/Validator';
import {
	CreateVoiceRegionRequest,
	CreateVoiceServerRequest,
	DeleteVoiceRegionRequest,
	DeleteVoiceServerRequest,
	GetVoiceRegionRequest,
	GetVoiceServerRequest,
	ListVoiceRegionsRequest,
	ListVoiceServersRequest,
	UpdateVoiceRegionRequest,
	UpdateVoiceServerRequest,
} from '../AdminModel';

export const VoiceAdminController = (app: HonoApp) => {
	app.post(
		'/admin/voice/regions/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_REGION_LIST),
		Validator('json', ListVoiceRegionsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.listVoiceRegions(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/regions/get',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_REGION_LIST),
		Validator('json', GetVoiceRegionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.getVoiceRegion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/regions/create',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_REGION_CREATE),
		Validator('json', CreateVoiceRegionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.createVoiceRegion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/regions/update',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_REGION_UPDATE),
		Validator('json', UpdateVoiceRegionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.updateVoiceRegion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/regions/delete',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_REGION_DELETE),
		Validator('json', DeleteVoiceRegionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.deleteVoiceRegion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/servers/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_SERVER_LIST),
		Validator('json', ListVoiceServersRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.listVoiceServers(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/servers/get',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.VOICE_SERVER_LIST),
		Validator('json', GetVoiceServerRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.getVoiceServer(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/servers/create',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_SERVER_CREATE),
		Validator('json', CreateVoiceServerRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.createVoiceServer(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/servers/update',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_SERVER_UPDATE),
		Validator('json', UpdateVoiceServerRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.updateVoiceServer(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/voice/servers/delete',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.VOICE_SERVER_DELETE),
		Validator('json', DeleteVoiceServerRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.deleteVoiceServer(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);
};
