// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {
	AdminApplicationIdParam,
	ApplicationUpdateResponse,
	ListApplicationsQuery,
	ListApplicationsResponse,
	LookupApplicationResponse,
	TransferApplicationOwnershipRequest,
} from '@fluxer/schema/src/domains/admin/AdminApplicationSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createApplicationID, createGuildID, createUserID} from '../../BrandedTypes';
import {requireAdminACL, requireAnyAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

function requireRequestAdminACL(granted: ReadonlySet<string>, required: string): void {
	if (!granted.has(required) && !granted.has(AdminACLs.WILDCARD)) {
		throw new MissingACLError(required);
	}
}

export function ApplicationAdminController(app: HonoApp) {
	app.get(
		'/admin/applications',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.APPLICATION_LOOKUP, AdminACLs.APPLICATION_LIST_BY_OWNER]),
		Validator('query', ListApplicationsQuery),
		OpenAPI({
			operationId: 'list_admin_applications',
			summary: 'List applications',
			description:
				'Lists OAuth2 applications and bots. Pass owner_id to list the applications a user owns, or guild_id to list the applications whose bot users are members of a guild. Exactly one of the two is required. owner_id requires APPLICATION_LIST_BY_OWNER permission, guild_id requires APPLICATION_LOOKUP permission.',
			responseSchema: ListApplicationsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {owner_id: ownerId, guild_id: guildId} = ctx.req.valid('query');
			if (guildId != null && ownerId != null) {
				throw InputValidationError.create('guild_id', 'Only one of owner_id and guild_id may be supplied');
			}
			if (guildId != null) {
				requireRequestAdminACL(ctx.get('adminUserAcls'), AdminACLs.APPLICATION_LOOKUP);
				return ctx.json(await adminService.applicationService.listGuildApplications(createGuildID(guildId)));
			}
			if (ownerId != null) {
				requireRequestAdminACL(ctx.get('adminUserAcls'), AdminACLs.APPLICATION_LIST_BY_OWNER);
				return ctx.json(await adminService.applicationService.listUserApplications(createUserID(ownerId)));
			}
			throw InputValidationError.create('owner_id', 'One of owner_id and guild_id is required');
		},
	);
	app.get(
		'/admin/users/:user_id/applications',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.APPLICATION_LIST_BY_OWNER),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_applications',
			summary: 'List user applications',
			description: 'Lists the OAuth2 applications and bots a user owns. Requires APPLICATION_LIST_BY_OWNER permission.',
			responseSchema: ListApplicationsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const userId = createUserID(ctx.req.valid('param').user_id);
			return ctx.json(await adminService.applicationService.listUserApplications(userId));
		},
	);
	app.get(
		'/admin/applications/:application_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.APPLICATION_LOOKUP),
		Validator('param', AdminApplicationIdParam),
		OpenAPI({
			operationId: 'get_admin_application',
			summary: 'Get application',
			description:
				'Retrieves complete application details including ownership, bot user, OAuth2 redirect URIs, and credential status. Requires APPLICATION_LOOKUP permission.',
			responseSchema: LookupApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const applicationId = createApplicationID(ctx.req.valid('param').application_id);
			return ctx.json(await adminService.applicationService.lookupApplication(applicationId));
		},
	);
	app.patch(
		'/admin/applications/:application_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.APPLICATION_TRANSFER_OWNERSHIP),
		Validator('param', AdminApplicationIdParam),
		Validator('json', TransferApplicationOwnershipRequest),
		OpenAPI({
			operationId: 'update_admin_application',
			summary: 'Update application',
			description:
				'Updates an application. Transfers ownership to the user given by new_owner_id, which is used when the owner is inactive or for administrative recovery. Logged to audit log. Requires APPLICATION_TRANSFER_OWNERSHIP permission.',
			responseSchema: ApplicationUpdateResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const applicationId = createApplicationID(ctx.req.valid('param').application_id);
			return ctx.json(
				await adminService.applicationService.transferApplicationOwnership(
					applicationId,
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
}
