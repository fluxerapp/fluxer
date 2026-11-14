// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {emitLogin} from '@app/api/auth/AuthLogin';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {requireConfiguredClientIp} from '@app/api/utils/RequestClientIp';
import {randomBase64Url, sha256Hex} from '@app/api/utils/SecretHash';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import type {AdminSignInLinkResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {seconds} from 'itty-time';

const SIGN_IN_LINK_KEY_PREFIX = 'sign_in_link:';
const SIGN_IN_LINK_TOKEN_BYTES = 32;
const SIGN_IN_LINK_TTL_SECONDS = seconds('15 minutes');

interface SignInLinkRecord {
	user_id: string;
	created_at: number;
}

function signInLinkKey(token: string): string {
	return `${SIGN_IN_LINK_KEY_PREFIX}${sha256Hex(token)}`;
}

function invalidSignInLink(): InputValidationError {
	return InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN);
}

function isAdmin(acls: ReadonlySet<string>): boolean {
	return acls.has(AdminACLs.WILDCARD);
}

export async function createSignInLink(
	ctx: ApiContext,
	auditService: AdminAuditService,
	args: {userId: UserID; adminUserId: UserID; auditLogReason: string | null},
): Promise<AdminSignInLinkResponse> {
	const user = await ctx.services.users.findUniqueAssert(args.userId);
	AuthUtility.assertNonBotUser(ctx, user);
	if (!isAdmin(user.acls)) {
		throw new MissingACLError(AdminACLs.WILDCARD);
	}
	const token = randomBase64Url(SIGN_IN_LINK_TOKEN_BYTES);
	const createdAt = Date.now();
	const record: SignInLinkRecord = {user_id: user.id.toString(), created_at: createdAt};
	await ctx.services.cache.set(signInLinkKey(token), record, SIGN_IN_LINK_TTL_SECONDS);
	await auditService.createAuditLog({
		adminUserId: args.adminUserId,
		targetType: 'user',
		targetId: BigInt(user.id),
		action: 'create_sign_in_link',
		auditLogReason: args.auditLogReason,
		metadata: new Map(),
	});
	return {
		url: `${Config.email.appBaseUrl}/login#sign_in_link=${token}`,
		expires_at: new Date(createdAt + SIGN_IN_LINK_TTL_SECONDS * 1000).toISOString(),
	};
}

export async function redeemSignInLink(
	ctx: ApiContext,
	auditService: AdminAuditService,
	args: {token: string; request: Request},
): Promise<{user_id: string; token: string}> {
	const record = await ctx.services.cache.getAndDelete<SignInLinkRecord>(signInLinkKey(args.token));
	if (!record) {
		throw invalidSignInLink();
	}
	const found = await ctx.services.users.findUnique(createUserID(BigInt(record.user_id)));
	if (!found || !isAdmin(found.acls)) {
		throw invalidSignInLink();
	}
	if (found.passwordLastChangedAt !== null && record.created_at < found.passwordLastChangedAt.getTime()) {
		throw invalidSignInLink();
	}
	AuthUtility.assertNonBotUser(ctx, found);
	const user = await AuthUtility.assertSignInAllowed(ctx, found);
	const clientIp = requireConfiguredClientIp(args.request, ctx.services.config.proxy);
	await ctx.services.users.createAuthorizedIp(user.id, clientIp);
	const [token] = await AuthSession.createAuthSession(ctx, {
		user,
		origin: AuthSession.resolveSessionOrigin(ctx, args.request),
	});
	await auditService.createAuditLog({
		adminUserId: user.id,
		targetType: 'user',
		targetId: BigInt(user.id),
		action: 'use_sign_in_link',
		auditLogReason: null,
		metadata: new Map([['via', 'rexctl']]),
	});
	emitLogin(user, true, {newIp: true, method: 'other'});
	return {user_id: user.id.toString(), token};
}
