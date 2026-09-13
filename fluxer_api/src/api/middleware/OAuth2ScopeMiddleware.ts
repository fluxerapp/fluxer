// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {OAuth2Scope} from '@fluxer/constants/src/OAuth2Constants';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {MissingOAuthScopeError} from '@fluxer/errors/src/domains/oauth/MissingOAuthScopeError';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';

type OAuth2ScopeCheckMode = 'strict' | 'bearer_only';

function ensureBearerScope(ctx: Context<HonoEnv>, scope: OAuth2Scope, mode: OAuth2ScopeCheckMode): boolean {
	const tokenType = ctx.get('authTokenType');
	if (tokenType !== 'bearer') {
		if (mode === 'strict') {
			throw new UnauthorizedError();
		}
		return false;
	}
	const oauthScopes = ctx.get('oauthBearerScopes');
	if (!oauthScopes || !oauthScopes.has(scope)) {
		throw new MissingOAuthScopeError(scope);
	}
	return true;
}

export function requireOAuth2Scope(scope: OAuth2Scope) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		ctx.set('oauthBearerAllowed', true);
		ensureBearerScope(ctx, scope, 'strict');
		await next();
	});
}

export function requireOAuth2ScopeForBearer(scope: OAuth2Scope) {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		ctx.set('oauthBearerAllowed', true);
		ensureBearerScope(ctx, scope, 'bearer_only');
		await next();
	});
}

export function requireOAuth2BearerToken() {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		ctx.set('oauthBearerAllowed', true);
		const tokenType = ctx.get('authTokenType');
		if (tokenType !== 'bearer') {
			throw new UnauthorizedError();
		}
		await next();
	});
}
