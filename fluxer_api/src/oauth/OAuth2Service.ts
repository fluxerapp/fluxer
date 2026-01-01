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

import {randomBytes} from 'node:crypto';
import argon2 from 'argon2';
import type {ApplicationID, UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import type {OAuth2AccessTokenRow, OAuth2AuthorizationCodeRow, OAuth2RefreshTokenRow} from '~/database/CassandraTypes';
import {
	AccessDeniedError,
	InvalidClientError,
	InvalidGrantError,
	InvalidRequestError,
	InvalidScopeError,
	InvalidTokenError,
} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import {Logger} from '~/Logger';
import type {Application} from '~/models/Application';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToOAuthResponse} from '~/user/UserMappers';
import type {OAuthScope} from './OAuthModels';
import {ApplicationRepository} from './repositories/ApplicationRepository';
import type {IApplicationRepository} from './repositories/IApplicationRepository';
import type {IOAuth2TokenRepository} from './repositories/IOAuth2TokenRepository';
import {OAuth2TokenRepository} from './repositories/OAuth2TokenRepository';

interface OAuth2ServiceDeps {
	userRepository: IUserRepository;
	applicationRepository?: IApplicationRepository;
	oauth2TokenRepository?: IOAuth2TokenRepository;
	cacheService?: ICacheService;
}

const PREFERRED_SCOPE_ORDER = ['identify', 'email', 'guilds', 'connections', 'bot', 'applications.commands'];

const sortScopes = (scope: Set<string>): Array<string> => {
	return Array.from(scope).sort((a, b) => {
		const ai = PREFERRED_SCOPE_ORDER.indexOf(a);
		const bi = PREFERRED_SCOPE_ORDER.indexOf(b);
		if (ai === -1 && bi === -1) return a.localeCompare(b);
		if (ai === -1) return 1;
		if (bi === -1) return -1;
		return ai - bi;
	});
};

export const ACCESS_TOKEN_TTL_SECONDS = 7 * 24 * 3600;

export class OAuth2Service {
	private applications: IApplicationRepository;
	private tokens: IOAuth2TokenRepository;
	private static readonly ALLOWED_SCOPES = [
		'identify',
		'email',
		'guilds',
		'connections',
		'bot',
		'applications.commands',
	];

	constructor(private readonly deps: OAuth2ServiceDeps) {
		this.applications = deps.applicationRepository ?? new ApplicationRepository();
		this.tokens = deps.oauth2TokenRepository ?? new OAuth2TokenRepository();
	}

	private parseScope(scope: string): Array<OAuthScope> {
		const parts = scope.split(/\s+/).filter(Boolean);
		return parts as Array<OAuthScope>;
	}

	private validateRedirectUri(application: Application, redirectUri: string): boolean {
		if (application.oauth2RedirectUris.size === 0) {
			return false;
		}
		return application.oauth2RedirectUris.has(redirectUri);
	}

	async authorizeAndConsent(params: {
		clientId: string;
		redirectUri?: string;
		scope: string;
		state?: string;
		codeChallenge?: string;
		codeChallengeMethod?: 'S256' | 'plain';
		responseType?: 'code';
		userId: UserID;
	}): Promise<{redirectTo: string}> {
		const parsedClientId = BigInt(params.clientId) as ApplicationID;
		const application = await this.applications.getApplication(parsedClientId);

		if (!application) {
			throw new InvalidClientError();
		}

		const scopeSet = new Set<string>(this.parseScope(params.scope));
		for (const s of scopeSet) {
			if (!OAuth2Service.ALLOWED_SCOPES.includes(s)) {
				throw new InvalidScopeError();
			}
		}

		if (scopeSet.has('bot') && !application.botIsPublic && params.userId !== application.ownerUserId) {
			throw new AccessDeniedError();
		}

		const isBotOnly = scopeSet.size === 1 && scopeSet.has('bot');

		const redirectUri = params.redirectUri;
		const requireRedirect = !isBotOnly;

		if (!redirectUri && requireRedirect) {
			throw new InvalidRequestError('Missing redirect_uri');
		}

		if (redirectUri && !this.validateRedirectUri(application, redirectUri)) {
			throw new InvalidRequestError('Invalid redirect_uri');
		}

		const resolvedRedirectUri = redirectUri ?? Config.endpoints.webApp;

		let loc: URL;
		try {
			loc = new URL(resolvedRedirectUri);
		} catch {
			throw new InvalidRequestError('Invalid redirect_uri');
		}

		const codeRow: OAuth2AuthorizationCodeRow = {
			code: randomBytes(32).toString('base64url'),
			application_id: application.applicationId,
			user_id: params.userId,
			redirect_uri: loc.toString(),
			scope: scopeSet,
			nonce: null,
			created_at: new Date(),
		};

		await this.tokens.createAuthorizationCode(codeRow);

		loc.searchParams.set('code', codeRow.code);
		if (params.state) {
			loc.searchParams.set('state', params.state);
		}

		return {redirectTo: loc.toString()};
	}

	private basicAuth(credentialsHeader?: string): {clientId: string; clientSecret: string} | null {
		if (!credentialsHeader) {
			return null;
		}

		const m = /^Basic\s+(.+)$/.exec(credentialsHeader);
		if (!m) {
			return null;
		}

		const decoded = Buffer.from(m[1], 'base64').toString('utf8');
		const idx = decoded.indexOf(':');

		if (idx < 0) {
			return null;
		}

		return {
			clientId: decoded.slice(0, idx),
			clientSecret: decoded.slice(idx + 1),
		};
	}

	private async issueTokens(args: {application: Application; userId: UserID | null; scope: Set<string>}): Promise<{
		accessToken: OAuth2AccessTokenRow;
		refreshToken?: OAuth2RefreshTokenRow;
		token_type: 'Bearer';
		expires_in: number;
		scope?: string;
	}> {
		const accessToken: OAuth2AccessTokenRow = {
			token_: randomBytes(32).toString('base64url'),
			application_id: args.application.applicationId,
			user_id: args.userId,
			scope: args.scope,
			created_at: new Date(),
		};

		const createdAccess = await this.tokens.createAccessToken(accessToken);

		let refreshToken: OAuth2RefreshTokenRow | undefined;
		if (args.userId) {
			const row: OAuth2RefreshTokenRow = {
				token_: randomBytes(32).toString('base64url'),
				application_id: args.application.applicationId,
				user_id: args.userId,
				scope: args.scope,
				created_at: new Date(),
			};
			const created = await this.tokens.createRefreshToken(row);
			refreshToken = created.toRow();
		}

		return {
			accessToken: createdAccess.toRow(),
			refreshToken,
			token_type: 'Bearer',
			expires_in: ACCESS_TOKEN_TTL_SECONDS,
			scope: sortScopes(args.scope).join(' '),
		};
	}

	async tokenExchange(params: {
		headersAuthorization?: string;
		grantType: 'authorization_code' | 'refresh_token';
		code?: string;
		refreshToken?: string;
		redirectUri?: string;
		clientId?: string;
		clientSecret?: string;
	}): Promise<{
		access_token: string;
		token_type: 'Bearer';
		expires_in: number;
		scope?: string;
		refresh_token?: string;
	}> {
		Logger.debug(
			{
				grant_type: params.grantType,
				client_id_present: !!params.clientId || /^Basic\s+/.test(params.headersAuthorization ?? ''),
				has_basic_auth: /^Basic\s+/.test(params.headersAuthorization ?? ''),
				code_present: !!params.code,
				refresh_token_present: !!params.refreshToken,
				redirect_uri_present: !!params.redirectUri,
			},
			'OAuth2 tokenExchange start',
		);

		const basic = this.basicAuth(params.headersAuthorization);
		const clientId = params.clientId ?? basic?.clientId ?? '';
		const clientSecret = params.clientSecret ?? basic?.clientSecret;
		const parsedClientId = BigInt(clientId) as ApplicationID;
		const application = await this.applications.getApplication(parsedClientId);

		if (!application) {
			Logger.debug({client_id_len: clientId.length}, 'OAuth2 tokenExchange: unknown application');
			throw new InvalidClientError();
		}

		if (!clientSecret) {
			Logger.debug(
				{application_id: application.applicationId.toString()},
				'OAuth2 tokenExchange: missing client_secret',
			);
			throw new InvalidClientError('Missing client_secret');
		}

		if (application.clientSecretHash) {
			const ok = await argon2.verify(application.clientSecretHash, clientSecret);
			if (!ok) {
				Logger.debug(
					{application_id: application.applicationId.toString()},
					'OAuth2 tokenExchange: client_secret verification failed',
				);
				throw new InvalidClientError('Invalid client_secret');
			}
		}

		if (params.grantType === 'authorization_code') {
			const code = params.code!;
			const authCode = await this.tokens.getAuthorizationCode(code);

			if (!authCode) {
				Logger.debug({code_len: code.length}, 'OAuth2 tokenExchange: authorization code not found');
				throw new InvalidGrantError();
			}

			if (authCode.applicationId !== application.applicationId) {
				Logger.debug(
					{application_id: application.applicationId.toString()},
					'OAuth2 tokenExchange: code application mismatch',
				);
				throw new InvalidGrantError();
			}

			if (params.redirectUri && authCode.redirectUri !== params.redirectUri) {
				Logger.debug(
					{expected: authCode.redirectUri, got: params.redirectUri},
					'OAuth2 tokenExchange: redirect_uri mismatch',
				);
				throw new InvalidGrantError();
			}

			await this.tokens.deleteAuthorizationCode(code);

			const res = await this.issueTokens({
				application,
				userId: authCode.userId,
				scope: authCode.scope,
			});

			return {
				access_token: res.accessToken.token_,
				token_type: 'Bearer',
				expires_in: res.expires_in,
				scope: res.scope,
				refresh_token: res.refreshToken?.token_,
			};
		}

		const refresh = await this.tokens.getRefreshToken(params.refreshToken!);
		if (!refresh) {
			throw new InvalidGrantError();
		}

		if (refresh.applicationId !== application.applicationId) {
			throw new InvalidGrantError();
		}

		const res = await this.issueTokens({
			application,
			userId: refresh.userId,
			scope: refresh.scope,
		});

		return {
			access_token: res.accessToken.token_,
			token_type: 'Bearer',
			expires_in: res.expires_in,
			scope: res.scope,
			refresh_token: res.refreshToken?.token_,
		};
	}

	async userInfo(accessToken: string) {
		const token = await this.tokens.getAccessToken(accessToken);
		if (!token || !token.userId) {
			throw new InvalidTokenError();
		}

		const application = await this.applications.getApplication(token.applicationId);
		if (!application) {
			throw new InvalidTokenError();
		}

		const user = await this.deps.userRepository.findUnique(token.userId);
		if (!user) {
			throw new InvalidTokenError();
		}

		const includeEmail = token.scope.has('email');
		return mapUserToOAuthResponse(user, {includeEmail});
	}

	async introspect(
		tokenStr: string,
		auth: {clientId: ApplicationID; clientSecret?: string | null},
	): Promise<{
		active: boolean;
		client_id?: string;
		sub?: string;
		scope?: string;
		token_type?: string;
		exp?: number;
		iat?: number;
	}> {
		const application = await this.applications.getApplication(auth.clientId);
		if (!application) {
			return {active: false};
		}

		if (!auth.clientSecret) {
			return {active: false};
		}

		if (application.clientSecretHash) {
			const valid = await argon2.verify(application.clientSecretHash, auth.clientSecret);
			if (!valid) {
				return {active: false};
			}
		}

		const token = await this.tokens.getAccessToken(tokenStr);
		if (!token) {
			return {active: false};
		}

		if (token.applicationId !== application.applicationId) {
			return {active: false};
		}

		return {
			active: true,
			client_id: token.applicationId.toString(),
			sub: token.userId ? token.userId.toString() : undefined,
			scope: sortScopes(token.scope).join(' '),
			token_type: 'Bearer',
			exp: Math.floor((token.createdAt.getTime() + ACCESS_TOKEN_TTL_SECONDS * 1000) / 1000),
			iat: Math.floor(token.createdAt.getTime() / 1000),
		};
	}

	async revoke(
		tokenStr: string,
		tokenTypeHint: 'access_token' | 'refresh_token' | undefined,
		auth: {clientId: ApplicationID; clientSecret?: string | null},
	): Promise<void> {
		const application = await this.applications.getApplication(auth.clientId);
		if (!application) {
			throw new InvalidClientError();
		}

		if (application.clientSecretHash) {
			const valid = auth.clientSecret ? await argon2.verify(application.clientSecretHash, auth.clientSecret) : false;
			if (!valid) {
				throw new InvalidClientError('Invalid client_secret');
			}
		}

		if (tokenTypeHint === 'refresh_token') {
			const refresh = await this.tokens.getRefreshToken(tokenStr);
			if (refresh && refresh.applicationId === application.applicationId) {
				await this.tokens.deleteRefreshToken(tokenStr, application.applicationId, refresh.userId);
				return;
			}
		}

		const access = await this.tokens.getAccessToken(tokenStr);
		if (access && access.applicationId === application.applicationId) {
			await this.tokens.deleteAccessToken(tokenStr, application.applicationId, access.userId);
			return;
		}
	}
}
