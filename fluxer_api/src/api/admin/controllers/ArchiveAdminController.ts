// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {
	AdminArchiveCreateRequest,
	AdminArchiveResponseSchema,
	DownloadUrlResponseSchema,
	GetArchiveResponseSchema,
	ListArchivesQuery,
	ListArchivesResponseSchema,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {ArchivePathParam, GuildIdParam, UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createGuildID, createUserID} from '../../BrandedTypes';
import {requireAdminACL, requireAnyAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

function canViewArchive(adminAcls: Set<string>, subjectType: 'user' | 'guild'): boolean {
	if (adminAcls.has(AdminACLs.WILDCARD) || adminAcls.has(AdminACLs.ARCHIVE_VIEW_ALL)) return true;
	if (subjectType === 'user') return adminAcls.has(AdminACLs.ARCHIVE_TRIGGER_USER);
	return adminAcls.has(AdminACLs.ARCHIVE_TRIGGER_GUILD);
}

function requireArchiveSubjectAccess(adminAcls: Set<string>, subjectType: 'user' | 'guild'): void {
	if (canViewArchive(adminAcls, subjectType) || adminAcls.has(AdminACLs.WILDCARD)) return;
	throw new MissingACLError(subjectType === 'user' ? AdminACLs.ARCHIVE_TRIGGER_USER : AdminACLs.ARCHIVE_TRIGGER_GUILD);
}

function resolveListSubjectType(adminAcls: Set<string>, requested: 'all' | 'user' | 'guild'): 'all' | 'user' | 'guild' {
	if (requested !== 'all') {
		requireArchiveSubjectAccess(adminAcls, requested);
		return requested;
	}
	const viewUser = canViewArchive(adminAcls, 'user');
	const viewGuild = canViewArchive(adminAcls, 'guild');
	if (viewUser && viewGuild) return 'all';
	if (viewUser) return 'user';
	if (viewGuild) return 'guild';
	throw new MissingACLError(AdminACLs.ARCHIVE_VIEW_ALL);
}

export function ArchiveAdminController(app: HonoApp) {
	app.post(
		'/admin/users/:user_id/archives',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ARCHIVE_TRIGGER_USER),
		Validator('param', UserIdParam),
		Validator('json', AdminArchiveCreateRequest),
		OpenAPI({
			operationId: 'create_admin_user_archive',
			summary: 'Create user archive',
			responseSchema: AdminArchiveResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				"Initiates a data export for a user. Creates an archive containing all the user's data (messages, server memberships, preferences, etc.) for export or compliance purposes.",
		}),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminUserId = ctx.get('adminUserId');
			const result = await adminArchiveService.triggerUserArchive(
				createUserID(ctx.req.valid('param').user_id),
				adminUserId,
				ctx.req.valid('json').include_attachments,
			);
			return ctx.json(result, 200);
		},
	);
	app.post(
		'/admin/guilds/:guild_id/archives',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ARCHIVE_TRIGGER_GUILD),
		Validator('param', GuildIdParam),
		Validator('json', AdminArchiveCreateRequest),
		OpenAPI({
			operationId: 'create_admin_guild_archive',
			summary: 'Create guild archive',
			responseSchema: AdminArchiveResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Initiates a data export for a guild (server). Creates an archive containing all guild data including channels, messages, members, roles, and settings.',
		}),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminUserId = ctx.get('adminUserId');
			const result = await adminArchiveService.triggerGuildArchive(
				createGuildID(ctx.req.valid('param').guild_id),
				adminUserId,
				ctx.req.valid('json').include_attachments,
			);
			return ctx.json(result, 200);
		},
	);
	app.get(
		'/admin/archives',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.ARCHIVE_VIEW_ALL, AdminACLs.ARCHIVE_TRIGGER_USER, AdminACLs.ARCHIVE_TRIGGER_GUILD]),
		Validator('query', ListArchivesQuery),
		OpenAPI({
			operationId: 'list_admin_archives',
			summary: 'List archives',
			responseSchema: ListArchivesResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Query and filter created archives by type (user or guild), subject ID, requestor, and expiration status. Admins with limited ACLs see only archives matching their permissions.',
		}),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminAcls = ctx.get('adminUserAcls');
			const query = ctx.req.valid('query');
			const result = await adminArchiveService.listArchives({
				subjectType: resolveListSubjectType(adminAcls, query.subject_type),
				subjectId: query.subject_id ?? undefined,
				requestedBy: query.requested_by ?? undefined,
				limit: query.limit,
				includeExpired: query.include_expired,
			});
			return ctx.json({archives: result}, 200);
		},
	);
	app.get(
		'/admin/archives/:subject_type/:subject_id/:archive_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.ARCHIVE_VIEW_ALL, AdminACLs.ARCHIVE_TRIGGER_USER, AdminACLs.ARCHIVE_TRIGGER_GUILD]),
		Validator('param', ArchivePathParam),
		OpenAPI({
			operationId: 'get_admin_archive',
			summary: 'Get archive details',
			responseSchema: GetArchiveResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Retrieve metadata for a specific archive including its status, creation time, expiration, and file location. Does not return the archive contents themselves.',
		}),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminAcls = ctx.get('adminUserAcls');
			const params = ctx.req.valid('param');
			requireArchiveSubjectAccess(adminAcls, params.subject_type);
			const archive = await adminArchiveService.getArchive(params.subject_type, params.subject_id, params.archive_id);
			return ctx.json({archive}, 200);
		},
	);
	app.get(
		'/admin/archives/:subject_type/:subject_id/:archive_id/download',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.ARCHIVE_VIEW_ALL, AdminACLs.ARCHIVE_TRIGGER_USER, AdminACLs.ARCHIVE_TRIGGER_GUILD]),
		Validator('param', ArchivePathParam),
		OpenAPI({
			operationId: 'get_admin_archive_download',
			summary: 'Get archive download URL',
			responseSchema: DownloadUrlResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Generate a time-limited download link to the archive file. The URL provides direct access to download the compressed archive contents.',
		}),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminAcls = ctx.get('adminUserAcls');
			const params = ctx.req.valid('param');
			requireArchiveSubjectAccess(adminAcls, params.subject_type);
			const result = await adminArchiveService.getDownloadUrl(
				params.subject_type,
				params.subject_id,
				params.archive_id,
			);
			return ctx.json(result, 200);
		},
	);
}
