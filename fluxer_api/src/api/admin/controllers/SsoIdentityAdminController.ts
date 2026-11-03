// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import {SsoIdentityLinkService, ssoProviderIdOf} from '@app/api/auth/services/SsoIdentityLinks';
import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {createUserID} from '@app/api/BrandedTypes';
import type {UserSsoIdentityRow} from '@app/api/database/types/AuthTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	AdminSsoIdentityLinkRequest,
	AdminSsoIdentityLookupQuery,
	AdminSsoIdentityLookupResponse,
	AdminSsoIdentityMigrateRequest,
	AdminSsoIdentityMigrateResponse,
	type AdminSsoIdentityResponse,
	AdminUserSsoIdentityListResponse,
} from '@fluxer/schema/src/domains/admin/AdminSsoSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {Context} from 'hono';

const ssoIdentityRepository = new SsoIdentityRepository();

function mapSsoIdentity(row: UserSsoIdentityRow): AdminSsoIdentityResponse {
	return {
		provider_id: row.provider_id,
		subject: row.subject,
		user_id: row.user_id.toString(),
		claimed_at: row.claimed_at.toISOString(),
	};
}

async function currentProviderId(): Promise<string> {
	return ssoProviderIdOf(await getInstanceConfigRepository().getSsoConfig());
}

export async function migrateSsoIdentities(
	ctx: Context<HonoEnv>,
	fromProviderId: string,
	toProviderId: string,
): Promise<{moved: number; skipped: number}> {
	const result = await new SsoIdentityLinkService(ctx.get('userRepository')).migrate(fromProviderId, toProviderId);
	await recordAdminWrite(ctx, {
		targetType: 'instance_config',
		targetId: 0n,
		action: 'migrate_sso_identities',
		metadata: {
			from_provider_id: fromProviderId,
			to_provider_id: toProviderId,
			moved_count: result.moved,
			skipped_count: result.skipped,
		},
	});
	return result;
}

export function SsoIdentityAdminController(app: HonoApp) {
	app.get(
		'/admin/users/:user_id/sso-identities',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_sso_identities',
			summary: 'List user SSO identities',
			responseSchema: AdminUserSsoIdentityListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List the single sign-on provider subjects bound to a user, with the provider each belongs to and when it was bound. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			await ctx.get('userRepository').findUniqueAssert(userId);
			const identities = await ssoIdentityRepository.listForUser(userId);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_SSO_IDENTITIES,
				metadata: {identity_count: identities.length},
			});
			return ctx.json({sso_identities: identities.map(mapSsoIdentity)});
		},
	);
	app.get(
		'/admin/sso-identities',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('query', AdminSsoIdentityLookupQuery),
		OpenAPI({
			operationId: 'lookup_admin_sso_identity',
			summary: 'Look up an SSO identity',
			responseSchema: AdminSsoIdentityLookupResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Find the user a single sign-on provider subject is bound to. Returns null when no account holds the subject. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const {provider_id: providerId, subject} = ctx.req.valid('query');
			const identity = await ssoIdentityRepository.findIdentity(providerId, subject);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: identity?.user_id ?? 0n,
				action: AdminAuditReadActions.LOOKUP_SSO_IDENTITY,
				metadata: {provider_id: providerId, found: identity !== null},
			});
			return ctx.json({sso_identity: identity ? mapSsoIdentity(identity) : null});
		},
	);
	app.put(
		'/admin/users/:user_id/sso-identities',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('param', UserIdParam),
		Validator('json', AdminSsoIdentityLinkRequest),
		OpenAPI({
			operationId: 'link_admin_user_sso_identity',
			summary: 'Bind an SSO subject to a user',
			responseSchema: AdminUserSsoIdentityListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Bind a provider subject under the provider in use to the account, replacing every subject bound to it before. Use it when the provider started sending another subject for the same person. Refused with SSO_IDENTITY_LINKED_TO_ANOTHER_ACCOUNT when another account that is not deleted holds the subject. Creates audit log entry. Requires USER_UPDATE_EMAIL permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const providerId = await currentProviderId();
			const identities = await new SsoIdentityLinkService(ctx.get('userRepository')).link(
				user,
				providerId,
				ctx.req.valid('json').subject,
			);
			await ctx.get('adminService').auditService.createAuditLog({
				adminUserId: ctx.get('adminUserId'),
				targetType: 'user',
				targetId: userId,
				action: 'link_sso_identity',
				auditLogReason: ctx.get('auditLogReason'),
				metadata: new Map([['provider_id', providerId]]),
			});
			return ctx.json({sso_identities: identities.map(mapSsoIdentity)});
		},
	);
	app.delete(
		'/admin/users/:user_id/sso-identities',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'unlink_admin_user_sso_identities',
			summary: 'Unbind the SSO subjects of a user',
			responseSchema: AdminUserSsoIdentityListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Release every provider subject bound to the account and sign it out on every device. On an instance that stores email addresses, the next single sign-on with the same verified email binds the account again. A released subject of a deleted account can sign in to a new account. Creates audit log entry. Requires USER_UPDATE_EMAIL permission.',
		}),
		async (ctx) => {
			const userId = createUserID(ctx.req.valid('param').user_id);
			const user = await ctx.get('userRepository').findUniqueAssert(userId);
			const before = await ssoIdentityRepository.listForUser(userId);
			const identities = await new SsoIdentityLinkService(ctx.get('userRepository')).unlink(
				user,
				await currentProviderId(),
			);
			await ctx.get('adminService').auditService.createAuditLog({
				adminUserId: ctx.get('adminUserId'),
				targetType: 'user',
				targetId: userId,
				action: 'unlink_sso_identities',
				auditLogReason: ctx.get('auditLogReason'),
				metadata: new Map([['identity_count', before.length.toString()]]),
			});
			await revokeAllAuthSessions(ctx.get('apiContext').services, userId);
			return ctx.json({sso_identities: identities.map(mapSsoIdentity)});
		},
	);
	app.post(
		'/admin/sso-identities/migrate',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator('json', AdminSsoIdentityMigrateRequest),
		OpenAPI({
			operationId: 'migrate_admin_sso_identities',
			summary: 'Move SSO subjects to the provider in use',
			responseSchema: AdminSsoIdentityMigrateResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Move every subject bound under from_provider_id to the provider in use, keeping each on its account. Use it after the issuer changed while the provider still sends the same subjects, so people keep their accounts. A subject that another account already holds under the provider in use is left where it was and counted in skipped_count. Reads every subject of the old provider, so run it once after the change. Creates audit log entry. Requires INSTANCE_CONFIG_UPDATE permission.',
		}),
		async (ctx) => {
			const fromProviderId = ctx.req.valid('json').from_provider_id;
			const toProviderId = await currentProviderId();
			if (fromProviderId === toProviderId) {
				throw InputValidationError.create('from_provider_id', 'This is the provider in use');
			}
			const {moved, skipped} = await migrateSsoIdentities(ctx, fromProviderId, toProviderId);
			return ctx.json({to_provider_id: toProviderId, moved_count: moved, skipped_count: skipped});
		},
	);
}
