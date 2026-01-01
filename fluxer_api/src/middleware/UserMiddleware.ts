/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {User} from '~/Models';
import * as IpUtils from '~/utils/IpUtils';

type TokenType = 'session' | 'bearer' | 'bot';

interface ParsedAuthHeader {
	token: string;
	type: TokenType;
}

function parseAuthHeader(authHeader?: string | null): ParsedAuthHeader | null {
	if (!authHeader) return null;

	const normalized = authHeader.trim();
	if (!normalized) return null;

	if (normalized.startsWith('Bearer ')) {
		const token = normalized.slice('Bearer '.length);
		if (token.length === 0 || token !== token.trim()) return null;
		return {
			token,
			type: 'bearer',
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
		const userRepository = ctx.get('userRepository');
		const redisActivityTracker = ctx.get('redisActivityTracker');
		void Promise.all([
			userRepository.updateLastActiveAt({
				userId: user.id,
				lastActiveAt: now,
				lastActiveIp: IpUtils.requireClientIp(ctx.req.raw),
			}),
			redisActivityTracker.updateActivity(user.id, now),
		]);
	}
}

export const UserMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const rawAuthHeader = ctx.req.header('Authorization');
	const parsed = parseAuthHeader(rawAuthHeader);

	ctx.set('oauthBearerToken', undefined);
	ctx.set('oauthBearerScopes', undefined);
	ctx.set('oauthBearerUserId', undefined);
	ctx.set('authToken', undefined);

	if (!parsed) {
		return next();
	}

	const {token, type} = parsed;
	ctx.set('authToken', token);

	if (type === 'session') {
		const authService = ctx.get('authService');
		const authSession = await authService.getAuthSessionByToken(token);
		if (authSession) {
			void authService.updateAuthSessionLastUsed(authSession.sessionIdHash);
			void authService.updateUserActivity({
				userId: authSession.userId,
				clientIp: IpUtils.requireClientIp(ctx.req.raw),
			});

			const user = await ctx.get('userService').findUniqueAssert(authSession.userId);

			ctx.set('authSession', authSession);
			ctx.set('authTokenType', 'session');
			setUserInContext(ctx, user, true);
		} else {
			getMetricsService().counter({
				name: 'auth.token.invalid',
				dimensions: {type: 'session'},
			});
		}
		await next();
		return;
	}

	if (type === 'bearer') {
		const accessToken = await ctx.get('oauth2TokenRepository').getAccessToken(token);
		const userId = accessToken?.userId ?? null;
		if (accessToken) {
			ctx.set('oauthBearerToken', token);
			ctx.set('oauthBearerScopes', accessToken.scope);
			ctx.set('oauthBearerUserId', accessToken.userId ?? undefined);
		} else {
			getMetricsService().counter({
				name: 'auth.token.invalid',
				dimensions: {type: 'bearer'},
			});
		}
		if (userId) {
			const user = await ctx.get('userService').findUnique(userId);
			if (user) {
				ctx.set('authTokenType', 'bearer');
				setUserInContext(ctx, user, false);
			}
		}
		await next();
		return;
	}

	if (type === 'bot') {
		const botAuthService = ctx.get('botAuthService');
		const botUserId = await botAuthService.validateBotToken(token);
		if (botUserId) {
			const botUser = await ctx.get('userService').findUnique(botUserId);
			if (botUser) {
				ctx.set('authTokenType', 'bot');
				setUserInContext(ctx, botUser, false);
			}
		} else {
			getMetricsService().counter({
				name: 'auth.token.invalid',
				dimensions: {type: 'bot'},
			});
		}
		await next();
		return;
	}

	await next();
});
