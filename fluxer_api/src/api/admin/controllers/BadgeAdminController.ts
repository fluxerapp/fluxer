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
	AdminCustomBadgeListQuery,
	AdminCustomBadgeListResponse,
	AdminCustomBadgeResponse,
	AdminCustomBadgeSettingsResponse,
	BuiltInBadgeKindParam,
	CreateCustomBadgeRequest,
	CustomBadgeIdParam,
	HideableBuiltInBadgeKindParam,
	MoveCustomBadgeRequest,
	ReorderCustomBadgesRequest,
	ReplaceBuiltInBadgeImageRequest,
	UpdateCustomBadgeRequest,
	UpdateCustomBadgeSettingsRequest,
	UpdateUserCustomBadgesRequest,
} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import {UserMutationResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function BadgeAdminController(app: HonoApp) {
	app.get(
		'/admin/badges',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.BADGE_LIST),
		Validator('query', AdminCustomBadgeListQuery),
		OpenAPI({
			operationId: 'list_admin_badges',
			summary: 'List custom badges',
			description:
				'Lists one page of the user and guild badges defined on this instance, in display order, with the badge settings and the built-in badge images. Pass next as after to get the following page. With query, it lists badges whose ID is the query or whose name holds it, and looks at up to 5,000 badges for each page. Requires BADGE_LIST permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const response = await ctx.get('adminService').badgeService.listBadges(ctx.req.valid('query'));
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
				'Creates a user or guild badge and uploads its image to the media proxy. The new badge is shown after every other badge of its type. Creates audit log entry. Requires BADGE_CREATE permission.',
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
	app.put(
		'/admin/badges/order',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('json', ReorderCustomBadgesRequest),
		OpenAPI({
			operationId: 'reorder_admin_badges',
			summary: 'Reorder custom badges',
			description:
				'Sets the display order of the custom badges. The body lists every user and guild badge on this instance exactly once, in the new order. It works on instances with up to 1,000 badges. Larger instances move badges one at a time with PATCH /admin/badges/order. The badges are written in groups. A failed request can leave part of the new order. Sending the same request again finishes it. Responds with the first page of badges. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {badge_ids: badgeIds} = ctx.req.valid('json');
			const response = await ctx.get('adminService').badgeService.reorderBadges(badgeIds);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: 'reorder_badges',
				metadata: {badge_ids: badgeIds.join(',')},
			});
			return ctx.json(response);
		},
	);
	app.patch(
		'/admin/badges/settings',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('json', UpdateCustomBadgeSettingsRequest),
		OpenAPI({
			operationId: 'update_admin_badge_settings',
			summary: 'Update custom badge settings',
			description:
				'Sets how many custom badges one user or one community can be given. Users can have up to 1,000 and communities up to 200. Lowering a limit keeps badges already given. A change that adds a badge to a user or community over the limit fails with MAX_BADGES. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeSettingsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const settings = await ctx.get('adminService').badgeService.updateSettings(body);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: 'update_badge_settings',
				metadata: {
					max_per_user: String(settings.max_per_user),
					max_per_guild: String(settings.max_per_guild),
				},
			});
			return ctx.json(settings);
		},
	);
	app.patch(
		'/admin/badges/order',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('json', MoveCustomBadgeRequest),
		OpenAPI({
			operationId: 'move_admin_badge',
			summary: 'Move custom badge',
			description:
				'Moves one badge within the badges of its type. Send direction to move it up or down one place or to the first or last place, or send after_id to show it right after another badge of the same type. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const body = ctx.req.valid('json');
			const badgeId = body.badge_id;
			const badge = await ctx.get('adminService').badgeService.moveBadge(badgeId, body);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: badgeId,
				action: 'move_badge',
				metadata:
					body.direction !== undefined
						? {direction: body.direction}
						: {after_id: body.after_id === null || body.after_id === undefined ? 'first' : body.after_id.toString()},
			});
			return ctx.json(badge);
		},
	);
	app.put(
		'/admin/badges/built-in/:kind',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', BuiltInBadgeKindParam),
		Validator('json', ReplaceBuiltInBadgeImageRequest),
		OpenAPI({
			operationId: 'replace_admin_built_in_badge_image',
			summary: 'Replace built-in badge image',
			description:
				'Shows an uploaded image everywhere a built-in profile or community badge appears. The name, tooltip and meaning of the badge stay the same. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {kind} = ctx.req.valid('param');
			const response = await ctx
				.get('adminService')
				.badgeService.replaceBuiltInBadgeImage(kind, ctx.req.valid('json').image);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: 'replace_built_in_badge_image',
				metadata: {kind},
			});
			return ctx.json(response);
		},
	);
	app.delete(
		'/admin/badges/built-in/:kind',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', BuiltInBadgeKindParam),
		OpenAPI({
			operationId: 'reset_admin_built_in_badge_image',
			summary: 'Use built-in badge image',
			description:
				'Goes back to the built-in artwork for a built-in profile or community badge and deletes the uploaded image. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {kind} = ctx.req.valid('param');
			const response = await ctx.get('adminService').badgeService.resetBuiltInBadgeImage(kind);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: 'reset_built_in_badge_image',
				metadata: {kind},
			});
			return ctx.json(response);
		},
	);
	app.put(
		'/admin/badges/built-in/:kind/hidden',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', HideableBuiltInBadgeKindParam),
		OpenAPI({
			operationId: 'hide_admin_built_in_badge',
			summary: 'Hide built-in community badge',
			description:
				'Stops clients on this instance from showing a built-in community badge. Communities keep the feature behind it, so a discoverable community stays listed in discovery. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {kind} = ctx.req.valid('param');
			const response = await ctx.get('adminService').badgeService.setBuiltInBadgeHidden(kind, true);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: 'hide_built_in_badge',
				metadata: {kind},
			});
			return ctx.json(response);
		},
	);
	app.delete(
		'/admin/badges/built-in/:kind/hidden',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.BADGE_UPDATE),
		Validator('param', HideableBuiltInBadgeKindParam),
		OpenAPI({
			operationId: 'show_admin_built_in_badge',
			summary: 'Show built-in community badge',
			description:
				'Shows a hidden built-in community badge again. Creates audit log entry. Requires BADGE_UPDATE permission.',
			responseSchema: AdminCustomBadgeListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {kind} = ctx.req.valid('param');
			const response = await ctx.get('adminService').badgeService.setBuiltInBadgeHidden(kind, false);
			await recordAdminWrite(ctx, {
				targetType: 'badge',
				targetId: 0n,
				action: 'show_built_in_badge',
				metadata: {kind},
			});
			return ctx.json(response);
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
				'Replaces the custom badges assigned to a user. Every ID must be a user badge. A change that adds a badge fails with MAX_BADGES when the user would have more than the max_per_user badge setting allows. Creates audit log entry. Requires USER_UPDATE_BADGES permission.',
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
