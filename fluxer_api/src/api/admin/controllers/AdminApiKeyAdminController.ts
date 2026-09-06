// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	CreateAdminApiKeyRequest,
	CreateAdminApiKeyResponse,
	type CreateAdminApiKeyResponse as CreateAdminApiKeyResponseType,
	DeleteApiKeyResponse,
	ListAdminApiKeyResponse,
	type ListAdminApiKeyResponse as ListAdminApiKeyResponseType,
	UpdateAdminApiKeyRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {KeyIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {z} from 'zod';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import type {AdminApiKeyView} from '../services/AdminApiKeyService';

function toApiKeyResponse(key: AdminApiKeyView): ListAdminApiKeyResponseType {
	return {
		key_id: key.keyId,
		name: key.name,
		created_at: key.createdAt.toISOString(),
		last_used_at: key.lastUsedAt?.toISOString() ?? null,
		expires_at: key.expiresAt?.toISOString() ?? null,
		created_by_user_id: String(key.createdById),
		acls: Array.from(key.acls),
	};
}

export function AdminApiKeyAdminController(app: HonoApp) {
	app.post(
		'/admin/api-keys',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_CODE_GENERATION),
		requireAdminACL(AdminACLs.ADMIN_API_KEY_MANAGE),
		Validator('json', CreateAdminApiKeyRequest),
		OpenAPI({
			operationId: 'create_admin_api_key',
			summary: 'Create admin API key',
			responseSchema: CreateAdminApiKeyResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				"Generates a new API key for administrative operations. The key is returned only once at creation time. Includes expiration settings and access control lists (ACLs) to limit the key's permissions.",
		}),
		async (ctx) => {
			const adminApiKeyService = ctx.get('adminApiKeyService');
			const user = ctx.get('user');
			const adminUserAcls = ctx.get('adminUserAcls');
			const request = ctx.req.valid('json');
			const result = await adminApiKeyService.createApiKey(request, user.id, adminUserAcls);
			const response: CreateAdminApiKeyResponseType = {
				key_id: result.apiKey.keyId,
				key: result.key,
				name: result.apiKey.name,
				created_at: result.apiKey.createdAt.toISOString(),
				expires_at: result.apiKey.expiresAt?.toISOString() ?? null,
				acls: Array.from(result.apiKey.acls),
			};
			return ctx.json(response);
		},
	);
	app.get(
		'/admin/api-keys',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ADMIN_API_KEY_MANAGE),
		OpenAPI({
			operationId: 'list_admin_api_keys',
			summary: 'List admin API keys',
			responseSchema: z.array(ListAdminApiKeyResponse),
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Retrieve all API keys created by the authenticated admin. Returns metadata including creation time, last used time, and assigned permissions. The actual key material is not returned.',
		}),
		async (ctx) => {
			const adminApiKeyService = ctx.get('adminApiKeyService');
			const user = ctx.get('user');
			const keys = await adminApiKeyService.listKeys(user.id);
			const response: Array<ListAdminApiKeyResponseType> = keys.map(toApiKeyResponse);
			return ctx.json(response);
		},
	);
	app.get(
		'/admin/api-keys/:key_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ADMIN_API_KEY_MANAGE),
		Validator('param', KeyIdParam),
		OpenAPI({
			operationId: 'get_admin_api_key',
			summary: 'Get admin API key',
			responseSchema: ListAdminApiKeyResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Retrieves a single API key created by the authenticated admin. Returns metadata including creation time, last used time, and assigned permissions. The actual key material is never returned.',
		}),
		async (ctx) => {
			const adminApiKeyService = ctx.get('adminApiKeyService');
			const user = ctx.get('user');
			const keyId = ctx.req.valid('param').key_id;
			const key = await adminApiKeyService.getKey(keyId, user.id);
			return ctx.json(toApiKeyResponse(key));
		},
	);
	app.patch(
		'/admin/api-keys/:key_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ADMIN_API_KEY_MANAGE),
		Validator('param', KeyIdParam),
		Validator('json', UpdateAdminApiKeyRequest),
		OpenAPI({
			operationId: 'update_admin_api_key',
			summary: 'Update admin API key',
			responseSchema: ListAdminApiKeyResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Renames an API key or replaces the access control lists (ACLs) it carries. The key may only carry permissions the acting admin already holds. Omitted fields are left unchanged and the key material is never rotated or returned.',
		}),
		async (ctx) => {
			const adminApiKeyService = ctx.get('adminApiKeyService');
			const user = ctx.get('user');
			const adminUserAcls = ctx.get('adminUserAcls');
			const keyId = ctx.req.valid('param').key_id;
			const key = await adminApiKeyService.updateKey(keyId, user.id, ctx.req.valid('json'), adminUserAcls);
			return ctx.json(toApiKeyResponse(key));
		},
	);
	app.delete(
		'/admin/api-keys/:key_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ADMIN_API_KEY_MANAGE),
		Validator('param', KeyIdParam),
		OpenAPI({
			operationId: 'delete_admin_api_key',
			summary: 'Revoke admin API key',
			responseSchema: DeleteApiKeyResponse,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Revokes an API key, immediately invalidating it for all future operations. This action cannot be undone.',
		}),
		async (ctx) => {
			const adminApiKeyService = ctx.get('adminApiKeyService');
			const user = ctx.get('user');
			const keyId = ctx.req.valid('param').key_id;
			await adminApiKeyService.revokeKey(keyId, user.id);
			return ctx.json({success: true}, 200);
		},
	);
}
