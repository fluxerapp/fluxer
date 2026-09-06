// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {
	BanGuildMemberBody,
	ListGuildAuditLogsResponse,
	ListGuildMembersQuery,
	ListGuildsQuery,
	UpdateGuildRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {
	GuildUpdateResponse,
	ListGuildEmojisResponse,
	ListGuildMembersResponse,
	ListGuildStickersResponse,
	LookupGuildResponse,
	SearchGuildsResponse,
	SuccessResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {GuildIdParam, GuildIdUserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {GuildAuditLogListQuery} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {createGuildID} from '../../BrandedTypes';
import {requireAdminACL, requireAnyAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {AdminRateLimitConfigs} from '../../rate_limit_configs/AdminRateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

const GUILD_UPDATE_ACLS = [
	AdminACLs.GUILD_UPDATE_NAME,
	AdminACLs.GUILD_UPDATE_SETTINGS,
	AdminACLs.GUILD_UPDATE_FEATURES,
	AdminACLs.GUILD_UPDATE_VANITY,
	AdminACLs.GUILD_TRANSFER_OWNERSHIP,
];

function hasGuildSettingsUpdate(body: UpdateGuildRequest): boolean {
	return (
		body.verification_level !== undefined ||
		body.mfa_level !== undefined ||
		body.nsfw_level !== undefined ||
		body.nsfw !== undefined ||
		body.content_warning_level !== undefined ||
		body.content_warning_text !== undefined ||
		body.explicit_content_filter !== undefined ||
		body.default_message_notifications !== undefined ||
		body.disabled_operations !== undefined
	);
}

function hasGuildFeatureUpdate(body: UpdateGuildRequest): boolean {
	return body.add_features !== undefined || body.remove_features !== undefined;
}

function selectGuildUpdateACLs(body: UpdateGuildRequest): Array<string> {
	const required: Array<string> = [];
	if (body.name !== undefined) {
		required.push(AdminACLs.GUILD_UPDATE_NAME);
	}
	if (body.fields !== undefined || hasGuildSettingsUpdate(body)) {
		required.push(AdminACLs.GUILD_UPDATE_SETTINGS);
	}
	if (hasGuildFeatureUpdate(body)) {
		required.push(AdminACLs.GUILD_UPDATE_FEATURES);
	}
	if (body.vanity_url_code !== undefined) {
		required.push(AdminACLs.GUILD_UPDATE_VANITY);
	}
	if (body.new_owner_id !== undefined) {
		required.push(AdminACLs.GUILD_TRANSFER_OWNERSHIP);
	}
	return required.length > 0 ? required : [AdminACLs.WILDCARD];
}

function requireAllAdminACLs(granted: ReadonlySet<string>, required: ReadonlyArray<string>): void {
	if (granted.has(AdminACLs.WILDCARD)) {
		return;
	}
	for (const acl of required) {
		if (!granted.has(acl)) {
			throw new MissingACLError(acl);
		}
	}
}

export function GuildAdminController(app: HonoApp) {
	app.get(
		'/admin/guilds',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('query', ListGuildsQuery),
		OpenAPI({
			operationId: 'list_admin_guilds',
			summary: 'List guilds',
			description:
				'Searches guilds by name, ID, and other criteria. Supports full-text search and pagination through limit and offset. Requires GUILD_LOOKUP permission.',
			responseSchema: SearchGuildsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const query = ctx.req.valid('query');
			return ctx.json(
				await adminService.searchService.searchGuilds({
					query: query.q,
					limit: query.limit,
					offset: query.offset,
				}),
			);
		},
	);
	app.get(
		'/admin/guilds/:guild_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'get_admin_guild',
			summary: 'Get guild',
			description:
				'Retrieves complete guild details including metadata, settings, channels, roles, and statistics. Requires GUILD_LOOKUP permission.',
			responseSchema: LookupGuildResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(
				await adminService.guildServiceAggregate.lookupService.lookupGuild({
					guild_id: ctx.req.valid('param').guild_id,
				}),
			);
		},
	);
	app.patch(
		'/admin/guilds/:guild_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAnyAdminACL(GUILD_UPDATE_ACLS),
		Validator('param', GuildIdParam),
		Validator('json', UpdateGuildRequest),
		OpenAPI({
			operationId: 'update_admin_guild',
			summary: 'Update guild',
			description:
				'Partially updates a guild. The permissions required are selected by the fields present in the body and are evaluated with all-of semantics: name requires GUILD_UPDATE_NAME, vanity_url_code requires GUILD_UPDATE_VANITY, new_owner_id requires GUILD_TRANSFER_OWNERSHIP, add_features and remove_features require GUILD_UPDATE_FEATURES, and fields together with every other setting requires GUILD_UPDATE_SETTINGS. A body carrying no field requires the wildcard permission. Every applied change is logged to the audit log.',
			responseSchema: GuildUpdateResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const guildIdRaw = ctx.req.valid('param').guild_id;
			const body = ctx.req.valid('json');
			requireAllAdminACLs(ctx.get('adminUserAcls'), selectGuildUpdateACLs(body));
			const {updateService, vanityService, lookupService} = adminService.guildServiceAggregate;
			if (body.fields !== undefined) {
				await updateService.clearGuildFields({guild_id: guildIdRaw, fields: body.fields}, adminUserId, auditLogReason);
			}
			if (hasGuildSettingsUpdate(body)) {
				await updateService.updateGuildSettings(
					{
						guild_id: guildIdRaw,
						verification_level: body.verification_level,
						mfa_level: body.mfa_level,
						nsfw_level: body.nsfw_level,
						nsfw: body.nsfw,
						content_warning_level: body.content_warning_level,
						content_warning_text: body.content_warning_text,
						explicit_content_filter: body.explicit_content_filter,
						default_message_notifications: body.default_message_notifications,
						disabled_operations: body.disabled_operations,
					},
					adminUserId,
					auditLogReason,
				);
			}
			if (hasGuildFeatureUpdate(body)) {
				await updateService.updateGuildFeatures({
					guildId: createGuildID(guildIdRaw),
					addFeatures: body.add_features ?? [],
					removeFeatures: body.remove_features ?? [],
					adminUserId,
					auditLogReason,
				});
			}
			if (body.name !== undefined) {
				await updateService.updateGuildName({guild_id: guildIdRaw, name: body.name}, adminUserId, auditLogReason);
			}
			if (body.vanity_url_code !== undefined) {
				await vanityService.updateGuildVanity(
					{guild_id: guildIdRaw, vanity_url_code: body.vanity_url_code},
					adminUserId,
					auditLogReason,
				);
			}
			if (body.new_owner_id !== undefined) {
				await updateService.transferGuildOwnership(
					{guild_id: guildIdRaw, new_owner_id: body.new_owner_id},
					adminUserId,
					auditLogReason,
				);
			}
			const {guild} = await lookupService.lookupGuild({guild_id: guildIdRaw});
			if (!guild) {
				throw new UnknownGuildError();
			}
			return ctx.json({
				guild: {
					id: guild.id,
					name: guild.name,
					features: guild.features,
					owner_id: guild.owner_id,
					icon: guild.icon,
					banner: guild.banner,
					member_count: guild.member_count,
					nsfw_level: guild.nsfw_level,
				},
			});
		},
	);
	app.delete(
		'/admin/guilds/:guild_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.GUILD_DELETE),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'delete_admin_guild',
			summary: 'Delete guild',
			description:
				'Permanently deletes a guild. Deletes all channels, messages, and settings. Irreversible operation with no recovery window. Logged to audit log. Requires GUILD_DELETE permission.',
			responseSchema: SuccessResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.guildServiceAggregate.managementService.deleteGuild(
					ctx.req.valid('param').guild_id,
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.get(
		'/admin/guilds/:guild_id/members',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LIST_MEMBERS),
		Validator('param', GuildIdParam),
		Validator('query', ListGuildMembersQuery),
		OpenAPI({
			operationId: 'list_admin_guild_members',
			summary: 'List guild members',
			description:
				'Lists all guild members with pagination. Returns member IDs, join dates, and roles. Requires GUILD_LIST_MEMBERS permission.',
			responseSchema: ListGuildMembersResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const query = ctx.req.valid('query');
			return ctx.json(
				await adminService.guildServiceAggregate.lookupService.listGuildMembers({
					guild_id: ctx.req.valid('param').guild_id,
					limit: query.limit,
					offset: query.offset,
				}),
			);
		},
	);
	app.put(
		'/admin/guilds/:guild_id/members/:user_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.GUILD_FORCE_ADD_MEMBER),
		Validator('param', GuildIdUserIdParam),
		OpenAPI({
			operationId: 'add_admin_guild_member',
			summary: 'Add guild member',
			description:
				'Forcefully adds a user to a guild. Bypasses normal invite flow for administrative account recovery. Logged to audit log. Requires GUILD_FORCE_ADD_MEMBER permission.',
			responseSchema: SuccessResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const requestCache = ctx.get('requestCache');
			const params = ctx.req.valid('param');
			return ctx.json(
				await adminService.guildServiceAggregate.membershipService.forceAddUserToGuild({
					data: {guild_id: params.guild_id, user_id: params.user_id},
					requestCache,
					adminUserId,
					auditLogReason,
				}),
			);
		},
	);
	app.delete(
		'/admin/guilds/:guild_id/members/:user_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.GUILD_KICK_MEMBER),
		Validator('param', GuildIdUserIdParam),
		OpenAPI({
			operationId: 'kick_admin_guild_member',
			summary: 'Remove guild member',
			description:
				'Temporarily removes a user from a guild. User can rejoin. Logged to audit log. Requires GUILD_KICK_MEMBER permission.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const params = ctx.req.valid('param');
			await adminService.guildServiceAggregate.membershipService.kickMember(
				{guild_id: params.guild_id, user_id: params.user_id},
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/admin/guilds/:guild_id/bans/:user_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.GUILD_BAN_MEMBER),
		Validator('param', GuildIdUserIdParam),
		Validator('json', BanGuildMemberBody),
		OpenAPI({
			operationId: 'ban_admin_guild_member',
			summary: 'Ban guild member',
			description:
				'Bans a user from a guild, optionally deleting their recent messages. Prevents the user from joining until the ban expires or is removed. Logged to audit log. Requires GUILD_BAN_MEMBER permission.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const params = ctx.req.valid('param');
			await adminService.guildServiceAggregate.membershipService.banMember(
				{...ctx.req.valid('json'), guild_id: params.guild_id, user_id: params.user_id},
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/admin/guilds/:guild_id/emojis',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ASSET_PURGE),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_admin_guild_emojis',
			summary: 'List guild emojis',
			description:
				'Lists all custom emojis in a guild. Returns ID, name, and creation date. Used for asset inventory and purge operations. Requires ASSET_PURGE permission.',
			responseSchema: ListGuildEmojisResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await adminService.guildServiceAggregate.lookupService.listGuildEmojis(guildId));
		},
	);
	app.get(
		'/admin/guilds/:guild_id/stickers',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ASSET_PURGE),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_admin_guild_stickers',
			summary: 'List guild stickers',
			description:
				'Lists all stickers in a guild. Returns ID, name, and asset information. Used for asset inventory and purge operations. Requires ASSET_PURGE permission.',
			responseSchema: ListGuildStickersResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await adminService.guildServiceAggregate.lookupService.listGuildStickers(guildId));
		},
	);
	app.get(
		'/admin/guilds/:guild_id/audit-logs',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_AUDIT_LOG_VIEW),
		Validator('param', GuildIdParam),
		Validator('query', GuildAuditLogListQuery),
		OpenAPI({
			operationId: 'list_admin_guild_audit_logs',
			summary: 'List guild audit logs',
			description:
				'Returns in-app guild audit log entries for a guild without requiring VIEW_AUDIT_LOG membership permission. Supports pagination via before/after log IDs and filtering by user_id or action_type. Requires GUILD_AUDIT_LOG_VIEW permission.',
			responseSchema: ListGuildAuditLogsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const query = ctx.req.valid('query');
			return ctx.json(
				await adminService.guildServiceAggregate.listGuildAuditLogs({
					guild_id: ctx.req.valid('param').guild_id,
					limit: query.limit,
					before: query.before,
					after: query.after,
					user_id: query.user_id,
					action_type: query.action_type,
				}),
			);
		},
	);
	app.post(
		'/admin/guilds/:guild_id/reloads',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.GUILD_RELOAD),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'create_admin_guild_reload',
			summary: 'Reload guild',
			description:
				'Reloads a single guild state from database. Used to recover from corruption or sync issues. Logged to audit log. Requires GUILD_RELOAD permission.',
			responseSchema: SuccessResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.guildServiceAggregate.managementService.reloadGuild(
					ctx.req.valid('param').guild_id,
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.post(
		'/admin/guilds/:guild_id/shutdowns',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.GUILD_SHUTDOWN),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'create_admin_guild_shutdown',
			summary: 'Shut down guild',
			description:
				'Shuts down and unloads a guild from the gateway. Guild data remains in database. Used for emergency resource cleanup. Logged to audit log. Requires GUILD_SHUTDOWN permission.',
			responseSchema: SuccessResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.guildServiceAggregate.managementService.shutdownGuild(
					ctx.req.valid('param').guild_id,
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
}
