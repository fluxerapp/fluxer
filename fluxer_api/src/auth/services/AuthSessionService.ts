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

import {UAParser} from 'ua-parser-js';
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
		if (user.isBot) {
			throw new AccessDeniedError('Bot users cannot create auth sessions');
		}

		const token = await this.generateAuthToken();
		const ip = IpUtils.requireClientIp(request);
		const userAgent = request.headers.get('user-agent') || '';
		const platformHeader = request.headers.get('x-fluxer-platform')?.toLowerCase() ?? null;
		const parsedUserAgent = new UAParser(userAgent).getResult();
		const geoipResult = await IpUtils.getCountryCodeDetailed(ip);
		const clientLocationLabel = IpUtils.formatGeoipLocation(geoipResult);
		const detectedPlatform = parsedUserAgent.browser.name ?? 'Unknown';
		const clientPlatform = platformHeader === 'desktop' ? 'Fluxer Desktop' : detectedPlatform;

		const authSession = await this.repository.createAuthSession({
			user_id: user.id,
			session_id_hash: Buffer.from(this.getTokenIdHash(token)),
			created_at: new Date(),
			approx_last_used_at: new Date(),
			client_ip: ip,
			client_os: parsedUserAgent.os.name ?? 'Unknown',
			client_platform: clientPlatform,
			client_country:
				(geoipResult.countryName ?? geoipResult.countryCode) === IpUtils.UNKNOWN_LOCATION
					? null
					: (geoipResult.countryName ?? geoipResult.countryCode),
			client_location: clientLocationLabel === IpUtils.UNKNOWN_LOCATION ? null : clientLocationLabel,
			version: 1,
		});

		return [token, authSession];
	}

	async getAuthSessionByToken(token: string): Promise<AuthSession | null> {
		return this.repository.getAuthSessionByToken(Buffer.from(this.getTokenIdHash(token)));
	}

	async getAuthSessions(userId: UserID): Promise<Array<AuthSessionResponse>> {
		const authSessions = await this.repository.listAuthSessions(userId);
		return mapAuthSessionsToResponse({authSessions});
	}

	async updateAuthSessionLastUsed(tokenHash: Uint8Array): Promise<void> {
		await this.repository.updateAuthSessionLastUsed(Buffer.from(tokenHash));
	}

	async updateUserActivity({userId, clientIp}: UpdateUserActivityParams): Promise<void> {
		await this.repository.updateUserActivity(userId, clientIp);
	}

	async revokeToken(token: string): Promise<void> {
		const tokenHash = this.getTokenIdHash(token);
		const authSession = await this.repository.getAuthSessionByToken(Buffer.from(tokenHash));

		if (authSession) {
			await this.repository.revokeAuthSession(Buffer.from(tokenHash));
			await this.gatewayService.terminateSession({
				userId: authSession.userId,
				sessionIdHashes: [Buffer.from(authSession.sessionIdHash).toString('base64url')],
			});
		}
	}

	async logoutAuthSessions({user, sessionIdHashes}: LogoutAuthSessionsParams): Promise<void> {
		const hashes = sessionIdHashes.map((hash) => Buffer.from(hash, 'base64url'));
		await this.repository.deleteAuthSessions(user.id, hashes);
		await this.gatewayService.terminateSession({
			userId: user.id,
			sessionIdHashes: sessionIdHashes,
		});
	}

	async terminateAllUserSessions(userId: UserID): Promise<void> {
		const authSessions = await this.repository.listAuthSessions(userId);
		await this.repository.deleteAuthSessions(
			userId,
			authSessions.map((session) => session.sessionIdHash),
		);
		await this.gatewayService.terminateSession({
			userId,
			sessionIdHashes: authSessions.map((session) => Buffer.from(session.sessionIdHash).toString('base64url')),
		});
	}

	async dispatchAuthSessionChange({
		userId,
		oldAuthSessionIdHash,
		newAuthSessionIdHash,
		newToken,
	}: {
		userId: UserID;
		oldAuthSessionIdHash: string;
		newAuthSessionIdHash: string;
		newToken: string;
	}): Promise<void> {
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
