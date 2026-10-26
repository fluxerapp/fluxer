// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {createUserID} from '@app/api/BrandedTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {AdminRateLimitConfigs} from '@app/api/rate_limit_configs/AdminRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	AdminCustomBadgeListResponse,
	AdminCustomBadgeResponse,
	CreateCustomBadgeRequest,
	CustomBadgeIdParam,
	UpdateCustomBadgeRequest,
	UpdateUserCustomBadgesRequest,
} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import {UserMutationResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function BadgeAdminController(app: HonoApp) {
	app.get(
		'/admin/badges',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.BADGE_LIST),
		OpenAPI({
			operationId: 'list_admin_badges',
			summary: 'List custom badges',
			description:
				'Lists the user and guild badges defined on this instance, in display order. Requires BADGE_LIST permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
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
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_CREATE),
		Validator('json', CreateCustomBadgeRequest),
		OpenAPI({
			operationId: 'create_admin_badge',
			summary: 'Create custom badge',
			description:
				'Creates a user or guild badge and uploads its image to the media proxy. Each type has at most 10 badges. Creates audit log entry. Requires BADGE_CREATE permission.',
			responseSchema: AdminCustomBadgeResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const badge = await ctx.get('adminService').badgeService.createBadge(ctx.req.valid('json'));
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: BigInt(badge.id),
				action: 'create_badge',
				metadata: {type: badge.type},
			});
			return ctx.json(badge);
		},
	);
	app.patch(
		'/admin/badges/:badge_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', CustomBadgeIdParam),
		Validator('json', UpdateCustomBadgeRequest),
		OpenAPI({
			operationId: 'update_admin_badge',
			summary: 'Update custom badge',
			description:
				'Replaces the name, description and link of a badge, and its image when one is sent. The type cannot change. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {badge_id: badgeId} = ctx.req.valid('param');
			const body = ctx.req.valid('json');
			const badge = await ctx.get('adminService').badgeService.updateBadge(badgeId, body);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: badgeId,
				action: 'update_badge',
				metadata: {image_replaced: body.image !== undefined},
			});
			return ctx.json(badge);
		},
	);
	app.delete(
		'/admin/badges/:badge_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_DELETE),
		Validator('param', CustomBadgeIdParam),
		OpenAPI({
			operationId: 'delete_admin_badge',
			summary: 'Delete custom badge',
			description:
				'Deletes a badge and its image. Users and guilds it was assigned to stop showing it. Creates audit log entry. Requires BADGE_DELETE permission.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {badge_id: badgeId} = ctx.req.valid('param');
			await ctx.get('adminService').badgeService.deleteBadge(badgeId);
			await recordAdminWrite(ctx, {targetType: 'badge', targetId: badgeId, action: 'delete_badge'});
			return ctx.body(null, 204);
		},
	);
	app.patch(
		'/admin/users/:user_id/badges',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BADGES),
		Validator('param', UserIdParam),
		Validator('json', UpdateUserCustomBadgesRequest),
		OpenAPI({
			operationId: 'update_admin_user_badges',
			summary: 'Update user custom badges',
			description:
				'Replaces the custom badges assigned to a user. Every ID must be a user badge, and a user can have at most 10. Creates audit log entry. Requires USER_UPDATE_BADGES permission.',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const {badge_ids: badgeIds} = ctx.req.valid('json');
			const response = await ctx.get('adminService').badgeService.updateUserBadges({
				userId,
				badgeIds,
				acls: ctx.get('adminUserAcls'),
			});
			await recordAdminWrite(ctx, {
				targetType: 'user',
				targetId: userId,
				action: 'update_user_badges',
				metadata: {badge_ids: badgeIds.join(',')},
			});
			return ctx.json(response);
		},
	);
}
