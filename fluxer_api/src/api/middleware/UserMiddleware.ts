// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import * as AuthSession from '../auth/AuthSession';
import {Logger} from '../Logger';
import type {User} from '../models/User';
import type {HonoEnv} from '../types/HonoEnv';
import {requireRequestClientIp} from '../utils/RequestClientIp';
import {stripApiPrefix} from '../utils/RequestPathUtils';
import {hashAuthToken, recordAbuseSignal} from './AbusiveIpAutoBanner';

type TokenType = 'session' | 'bearer' | 'bot' | 'admin_api_key';

interface ParsedAuthHeader {
	token: string;
	type: TokenType;
}

const SKIP_PATHS = new Set(['/_health', '/webhooks/livekit', '/webhooks/sweego']);
const SESSION_TOKEN_PATTERN = /^flx_[A-Za-z0-9]{36}$/;

function parseAuthHeader(authHeader?: string | null): ParsedAuthHeader | null {
	if (!authHeader) return null;
	if (authHeader !== authHeader.trim()) return null;
	const normalized = authHeader;
	if (!normalized) return null;
	if (normalized.startsWith('Bearer ')) {
		const token = normalized.slice('Bearer '.length);
		if (token.length === 0 || token !== token.trim()) return null;
		return {
			token,
			type: SESSION_TOKEN_PATTERN.test(token) ? 'session' : 'bearer',
		};
	}
	if (normalized.startsWith('Bot ')) {
		const token = normalized.slice('Bot '.length);
		if (token.length === 0 || token !== token.trim()) return null;
		return {
			token,
			type: 'bot',
		};
	}
	if (normalized.startsWith('Admin ')) {
		const token = normalized.slice('Admin '.length);
		if (token.length === 0 || token !== token.trim()) return null;
		return {
			token,
			type: 'admin_api_key',
		};
	}
	if (normalized.includes(' ')) return null;
	return {
		token: normalized,
		type: 'session',
	};
}

function setUserInContext(ctx: Context<HonoEnv>, user: User, trackActivity: boolean): void {
	ctx.set('user', user);
	if (trackActivity) {
		const now = new Date();
		const ip = requireRequestClientIp(ctx);
		const kvActivityTracker = ctx.get('kvActivityTracker');
		const userActivityBuffer = ctx.get('userActivityBuffer');
		userActivityBuffer.recordActivity(user.id, now, ip);
		void kvActivityTracker.updateActivity(user.id, now).catch((error: unknown) => {
			Logger.warn({error, userId: user.id}, 'Failed to update real-time user activity');
		});
	}
}

export const UserMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const path = stripApiPrefix(ctx.req.path);
	if (SKIP_PATHS.has(path)) {
		return next();
	}
	const rawAuthHeader = ctx.req.header('Authorization');
	const parsed = parseAuthHeader(rawAuthHeader);
	const resolvedClientIp = requireRequestClientIp(ctx);
	ctx.set('oauthBearerToken', undefined);
	ctx.set('oauthBearerApplicationId', undefined);
	ctx.set('oauthBearerAllowed', false);
	ctx.set('oauthBearerScopes', undefined);
	ctx.set('oauthBearerUserId', undefined);
	ctx.set('authToken', undefined);
	if (!parsed) {
		if (rawAuthHeader) {
			recordAbuseSignal(resolvedClientIp, 'auth_failure:malformed', {tokenHash: hashAuthToken(rawAuthHeader)});
		}
		return next();
	}
	const {token, type} = parsed;
	const tokenHash = hashAuthToken(token);
	ctx.set('authToken', token);
	if (type === 'session') {
		const apiContext = ctx.get('apiContext');
		const authSession = await AuthSession.getAuthSessionByToken(apiContext, token);
		if (authSession) {
			void AuthSession.updateAuthSessionLastUsed(apiContext, authSession.sessionIdHash);
			const user = await apiContext.services.users.findUnique(authSession.userId);
			if (user) {
				ctx.set('authSession', authSession);
				ctx.set('authTokenType', 'session');
				setUserInContext(ctx, user, true);
			} else {
				recordAbuseSignal(resolvedClientIp, 'auth_failure:session', {tokenHash});
			}
		} else {
			recordAbuseSignal(resolvedClientIp, 'auth_failure:session', {tokenHash});
		}
		await next();
		return;
	}
	if (type === 'bearer') {
		const oauth2TokenRepository = ctx.get('oauth2TokenRepository');
		const accessToken = await oauth2TokenRepository.getAccessToken(token);
		if (accessToken) {
			ctx.set('oauthBearerToken', token);
			ctx.set('oauthBearerApplicationId', accessToken.applicationId);
			ctx.set('oauthBearerScopes', accessToken.scope);
			ctx.set('oauthBearerUserId', accessToken.userId ?? undefined);
			ctx.set('authTokenType', 'bearer');
			const userId = accessToken.userId ?? null;
			if (userId) {
				const user = await ctx.get('apiContext').services.users.findUnique(userId);
				if (user) {
					setUserInContext(ctx, user, false);
				}
			}
			await next();
			return;
		}
		recordAbuseSignal(resolvedClientIp, 'auth_failure:bearer', {tokenHash});
		await next();
		return;
	}
	if (type === 'bot') {
		const botAuthService = ctx.get('botAuthService');
		const botUserId = await botAuthService.validateBotToken(token);
		if (botUserId) {
			const botUser = await ctx.get('apiContext').services.users.findUnique(botUserId);
			if (botUser) {
				ctx.set('authTokenType', 'bot');
				setUserInContext(ctx, botUser, false);
			}
		} else {
			recordAbuseSignal(resolvedClientIp, 'auth_failure:bot', {tokenHash});
		}
		await next();
		return;
	}
	if (type === 'admin_api_key') {
		const path = stripApiPrefix(ctx.req.path);
		if (!(path === '/admin' || path.startsWith('/admin/'))) {
			await next();
			return;
		}
		const adminApiKeyService = ctx.get('adminApiKeyService');
		const apiKey = await adminApiKeyService.validateApiKey(token);
		if (apiKey) {
			const user = await ctx.get('apiContext').services.users.findUnique(apiKey.createdById);
			if (user) {
				ctx.set('authTokenType', 'admin_api_key');
				ctx.set('adminApiKey', apiKey);
				ctx.set('adminApiKeyAcls', apiKey.acls);
				setUserInContext(ctx, user, false);
			}
		} else {
			recordAbuseSignal(resolvedClientIp, 'auth_failure:admin_api_key', {tokenHash});
		}
		await next();
		return;
	}
	await next();
});
