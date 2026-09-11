// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import {createApplicationID, createUserID} from '../BrandedTypes';
import {assertMutableUserId} from '../constants/Core';
import {Logger} from '../Logger';
import type {User} from '../models/User';
import type {HonoEnv} from '../types/HonoEnv';

const ADMIN_OAUTH2_APPLICATION_ID_BRANDED = createApplicationID(ADMIN_OAUTH2_APPLICATION_ID);
type AdminAuthTokenType = 'bearer' | 'session' | 'admin_api_key';
const ADMIN_TARGET_USER_PARAMS = ['user_id', 'target_user_id'] as const;

function ensureBearerIsBuiltInAdminApplication(ctx: Context<HonoEnv>): void {
	if (ctx.get('oauthBearerApplicationId') !== ADMIN_OAUTH2_APPLICATION_ID_BRANDED) {
		throw new AccessDeniedError();
	}
}

function getAdminTokenType(ctx: Context<HonoEnv>): AdminAuthTokenType {
	const tokenType = ctx.get('authTokenType');
	if (tokenType === 'bearer' || tokenType === 'session' || tokenType === 'admin_api_key') {
		return tokenType;
	}
	throw new UnauthorizedError();
}

function ensureAdminCanAuthenticate(adminUser: User): void {
	if (!adminUser.acls.has(AdminACLs.AUTHENTICATE) && !adminUser.acls.has(AdminACLs.WILDCARD)) {
		throw new MissingPermissionsError();
	}
}

function hasAnyAdminACL(acls: Set<string>, requiredACLs: ReadonlyArray<string>): boolean {
	return acls.has(AdminACLs.WILDCARD) || requiredACLs.some((acl) => acls.has(acl));
}

function ensureAdminApiKeyOwnerCanUseACLs(
	adminUser: User,
	tokenType: AdminAuthTokenType,
	requiredACLs: ReadonlyArray<string>,
): void {
	if (tokenType !== 'admin_api_key' || adminUser.acls.has(AdminACLs.WILDCARD)) {
		return;
	}
	if (!requiredACLs.some((acl) => adminUser.acls.has(acl))) {
		throw new MissingACLError(requiredACLs[0] ?? AdminACLs.AUTHENTICATE);
	}
}

function getRequestAdminACLs(ctx: Context<HonoEnv>, adminUser: User, tokenType: AdminAuthTokenType): Set<string> {
	return tokenType === 'admin_api_key' ? (ctx.get('adminApiKeyAcls') ?? new Set()) : adminUser.acls;
}

function ensureAdminTargetIsMutable(ctx: Context<HonoEnv>): void {
	if (ctx.req.method === 'GET') {
		return;
	}
	for (const paramName of ADMIN_TARGET_USER_PARAMS) {
		const rawUserId = ctx.req.param(paramName);
		if (rawUserId === undefined) {
			continue;
		}
		const parsedUserId = SnowflakeType.safeParse(rawUserId);
		if (!parsedUserId.success) {
			continue;
		}
		assertMutableUserId(createUserID(parsedUserId.data));
	}
}

function requireAdminAccess(requiredACLs: ReadonlyArray<string>) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		const adminUser = ctx.get('user');
		if (!adminUser) throw new UnauthorizedError();
		const tokenType = getAdminTokenType(ctx);
		if (tokenType === 'bearer') {
			ensureBearerIsBuiltInAdminApplication(ctx);
		}
		const requestAcls = getRequestAdminACLs(ctx, adminUser, tokenType);
		Logger.debug(
			{
				adminUserId: adminUser.id.toString(),
				acls: Array.from(requestAcls),
				requiredACLs,
				tokenType,
			},
			'Checking admin ACL requirements',
		);
		ensureAdminCanAuthenticate(adminUser);
		ensureAdminApiKeyOwnerCanUseACLs(adminUser, tokenType, requiredACLs);
		if (!hasAnyAdminACL(requestAcls, requiredACLs)) {
			throw new MissingACLError(requiredACLs[0] ?? AdminACLs.AUTHENTICATE);
		}
		ensureAdminTargetIsMutable(ctx);
		ctx.set('adminUserId', adminUser.id);
		ctx.set('adminUserAcls', requestAcls);
		await next();
	});
}

export function requireAdminACL(requiredACL: string) {
	return requireAdminAccess([requiredACL]);
}

export function requireAnyAdminACL(requiredACLs: Array<string>) {
	return requireAdminAccess(requiredACLs);
}
