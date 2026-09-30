// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead} from '@app/api/admin/AdminAuditRecorder';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {AdminRateLimitConfigs} from '@app/api/rate_limit_configs/AdminRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	BadgeIdParam,
	BadgeMutationResponse,
	BuiltinBadgeParam,
	CreateBadgeRequest,
	UpdateBadgeRequest,
	UpdateBuiltinBadgeIconRequest,
} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import {BadgesResponse, BuiltinBadgeIconsResponse} from '@fluxer/schema/src/domains/badge/BadgeSchemas';
import {SuccessResponse} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function BadgeAdminController(app: HonoApp) {
	app.get(
		'/admin/badges',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.BADGE_LIST),
		OpenAPI({
			operationId: 'list_admin_badges',
			summary: 'List badges',
			responseSchema: BadgesResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists every user and guild badge defined on the instance, together with the icon overrides for built-in badges. Requires BADGE_LIST permission.',
		}),
		async (ctx) => {
			const response = await ctx.get('adminService').badgeService.listBadges();
			await recordAdminRead(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_BADGES,
				metadata: {result_count: response.badges.length},
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/admin/badges',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GENERAL),
		requireAdminACL(AdminACLs.BADGE_CREATE),
		Validator('json', CreateBadgeRequest),
		OpenAPI({
			operationId: 'create_admin_badge',
			summary: 'Create badge',
			responseSchema: BadgeMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Creates a user or guild badge. Creates audit log entry. Requires BADGE_CREATE permission.',
		}),
		async (ctx) =>
			ctx.json(
				await ctx
					.get('adminService')
					.badgeService.createBadge(ctx.req.valid('json'), ctx.get('adminUserId'), ctx.get('auditLogReason')),
			),
	);
	app.patch(
		'/admin/badges/:badge_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GENERAL),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', BadgeIdParam),
		Validator('json', UpdateBadgeRequest),
		OpenAPI({
			operationId: 'update_admin_badge',
			summary: 'Update badge',
			responseSchema: BadgeMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Updates the name, tooltip, icon, URL, or position of a badge. Creates audit log entry. Requires BADGE_UPDATE permission.',
		}),
		async (ctx) =>
			ctx.json(
				await ctx
					.get('adminService')
					.badgeService.updateBadge(
						ctx.req.valid('param').badge_id,
						ctx.req.valid('json'),
						ctx.get('adminUserId'),
						ctx.get('auditLogReason'),
					),
			),
	);
	app.delete(
		'/admin/badges/:badge_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GENERAL),
		requireAdminACL(AdminACLs.BADGE_DELETE),
		Validator('param', BadgeIdParam),
		OpenAPI({
			operationId: 'delete_admin_badge',
			summary: 'Delete badge',
			responseSchema: SuccessResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Deletes a badge. Users and guilds that were assigned the badge stop showing it. Creates audit log entry. Requires BADGE_DELETE permission.',
		}),
		async (ctx) =>
			ctx.json(
				await ctx
					.get('adminService')
					.badgeService.deleteBadge(ctx.req.valid('param').badge_id, ctx.get('adminUserId'), ctx.get('auditLogReason')),
			),
	);
	app.put(
		'/admin/badges/builtin/:badge',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GENERAL),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', BuiltinBadgeParam),
		Validator('json', UpdateBuiltinBadgeIconRequest),
		OpenAPI({
			operationId: 'update_admin_builtin_badge_icon',
			summary: 'Update built-in badge icon',
			responseSchema: BuiltinBadgeIconsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Overrides the SVG icon of the premium, verified, partnered, or discoverable badge, or restores the default icon when null. Creates audit log entry. Requires BADGE_UPDATE permission.',
		}),
		async (ctx) =>
			ctx.json(
				await ctx
					.get('adminService')
					.badgeService.updateBuiltinBadgeIcon(
						ctx.req.valid('param').badge,
						ctx.req.valid('json').icon,
						ctx.get('adminUserId'),
						ctx.get('auditLogReason'),
					),
			),
	);
}
