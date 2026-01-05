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

import {type AuthSessionResponse, mapAuthSessionsToResponse} from '~/auth/AuthModel';
import type {UserID} from '~/BrandedTypes';
import {AccessDeniedError} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {AuthSession, User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import * as IpUtils from '~/utils/IpUtils';

interface CreateAuthSessionParams {
	user: User;
	request: Request;
}

interface LogoutAuthSessionsParams {
	user: User;
	sessionIdHashes: Array<string>;
}

interface UpdateUserActivityParams {
	userId: UserID;
	clientIp: string;
}
export class AuthSessionService {
	constructor(
		private repository: IUserRepository,
		private gatewayService: IGatewayService,
		private generateAuthToken: () => Promise<string>,
		private getTokenIdHash: (token: string) => Uint8Array,
	) {}

	async createAuthSession({user, request}: CreateAuthSessionParams): Promise<[token: string, AuthSession]> {
		if (user.isBot) throw new AccessDeniedError('Bot users cannot create auth sessions');

		const now = new Date();
		const token = await this.generateAuthToken();
		const ip = IpUtils.requireClientIp(request);

		const platformHeader = request.headers.get('x-fluxer-platform')?.trim().toLowerCase() ?? null;
		const uaRaw = request.headers.get('user-agent') ?? '';
		const isDesktopClient = platformHeader === 'desktop';

		const authSession = await this.repository.createAuthSession({
			user_id: user.id,
			session_id_hash: Buffer.from(this.getTokenIdHash(token)),
			created_at: now,
			approx_last_used_at: now,
			client_ip: ip,
			client_user_agent: uaRaw || null,
			client_is_desktop: isDesktopClient,
			version: 1,
		});

		return [token, authSession];
	}

	async getAuthSessionByToken(token: string): Promise<AuthSession | null> {
		return this.repository.getAuthSessionByToken(Buffer.from(this.getTokenIdHash(token)));
	}

	async getAuthSessions(userId: UserID): Promise<Array<AuthSessionResponse>> {
		const authSessions = await this.repository.listAuthSessions(userId);
		return await mapAuthSessionsToResponse({authSessions});
	}

	async updateAuthSessionLastUsed(tokenHash: Uint8Array): Promise<void> {
		await this.repository.updateAuthSessionLastUsed(Buffer.from(tokenHash));
	}

	async updateUserActivity({userId, clientIp}: UpdateUserActivityParams): Promise<void> {
		await this.repository.updateUserActivity(userId, clientIp);
	}

	async revokeToken(token: string): Promise<void> {
		const tokenHash = Buffer.from(this.getTokenIdHash(token));
		const authSession = await this.repository.getAuthSessionByToken(tokenHash);
		if (!authSession) return;

		await this.repository.revokeAuthSession(tokenHash);

		await this.gatewayService.terminateSession({
			userId: authSession.userId,
			sessionIdHashes: [Buffer.from(authSession.sessionIdHash).toString('base64url')],
		});
	}

	async logoutAuthSessions({user, sessionIdHashes}: LogoutAuthSessionsParams): Promise<void> {
		const hashes = sessionIdHashes.map((hash) => Buffer.from(hash, 'base64url'));
		await this.repository.deleteAuthSessions(user.id, hashes);

		await this.gatewayService.terminateSession({
			userId: user.id,
			sessionIdHashes,
		});
	}

	async terminateAllUserSessions(userId: UserID): Promise<void> {
		const authSessions = await this.repository.listAuthSessions(userId);
		if (authSessions.length === 0) return;

		const hashes = authSessions.map((s) => s.sessionIdHash);
		await this.repository.deleteAuthSessions(userId, hashes);

		await this.gatewayService.terminateSession({
			userId,
			sessionIdHashes: authSessions.map((s) => Buffer.from(s.sessionIdHash).toString('base64url')),
		});
	}

	async dispatchAuthSessionChange(params: {
		userId: UserID;
		oldAuthSessionIdHash: string;
		newAuthSessionIdHash: string;
		newToken: string;
	}): Promise<void> {
		const {userId, oldAuthSessionIdHash, newAuthSessionIdHash, newToken} = params;

		await this.gatewayService.dispatchPresence({
			userId,
			event: 'AUTH_SESSION_CHANGE',
			data: {
				old_auth_session_id_hash: oldAuthSessionIdHash,
				new_auth_session_id_hash: newAuthSessionIdHash,
				new_token: newToken,
			},
		});
	}
}
