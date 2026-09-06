// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	IndexRefreshStatusResponse,
	RefreshSearchIndexRequest,
	RefreshSearchIndexResponse,
	SearchIndexNameParam,
	SearchIndexRefreshIdParam,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function SearchAdminController(app: HonoApp) {
	app.post(
		'/admin/search/indexes/:index_name/refreshes',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('param', SearchIndexNameParam),
		Validator('json', RefreshSearchIndexRequest),
		OpenAPI({
			operationId: 'create_admin_search_index_refresh',
			summary: 'Refresh a search index',
			description:
				'Trigger a full or partial rebuild of the named search index. Creates a background job and returns its refresh ID for status tracking. The channel_messages and guild_members indexes are rebuilt one guild at a time and require guild_id, and favorite_memes requires user_id. Requires GUILD_LOOKUP permission.',
			responseSchema: RefreshSearchIndexResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {index_name} = ctx.req.valid('param');
			const {guild_id, user_id} = ctx.req.valid('json');
			return ctx.json(
				await adminService.searchService.refreshSearchIndex(
					{index_type: index_name, guild_id, user_id},
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.get(
		'/admin/search/index-refreshes/:job_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('param', SearchIndexRefreshIdParam),
		OpenAPI({
			operationId: 'get_admin_search_index_refresh',
			summary: 'Get search index refresh',
			description:
				'Reads the progress of a queued search index refresh. Returns the completion counts and current phase, or a not_found status once the record has expired. Requires GUILD_LOOKUP permission.',
			responseSchema: IndexRefreshStatusResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {job_id} = ctx.req.valid('param');
			return ctx.json(await adminService.searchService.getIndexRefreshStatus(job_id));
		},
	);
}
