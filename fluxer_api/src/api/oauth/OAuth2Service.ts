// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {
	OAuth2AccessTokenRow,
	OAuth2AuthorizationCodeRow,
	OAuth2RefreshTokenRow,
} from '@app/api/database/types/OAuth2Types';
import {Logger} from '@app/api/Logger';
import type {Application} from '@app/api/models/Application';
import {filterOAuth2ScopeSet, isOAuth2Scope, sortOAuth2Scopes} from '@app/api/oauth/OAuth2ScopeUtils';
import {ACCESS_TOKEN_TTL_SECONDS} from '@app/api/oauth/OAuth2TokenConstants';
import {generateOAuthTokenSecret} from '@app/api/oauth/OAuthTokenSecret';
import {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import type {IApplicationRepository} from '@app/api/oauth/repositories/IApplicationRepository';
import type {IOAuth2TokenRepository} from '@app/api/oauth/repositories/IOAuth2TokenRepository';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {mapUserToOAuthResponse} from '@app/api/user/UserMappers';
import {verifyPassword} from '@app/api/utils/PasswordUtils';
import {InvalidRequestError} from '@fluxer/errors/src/domains/core/InvalidRequestError';
import {InvalidTokenError} from '@fluxer/errors/src/domains/core/InvalidTokenError';
import {BotIsPrivateError} from '@fluxer/errors/src/domains/oauth/BotIsPrivateError';
import {InvalidClientError} from '@fluxer/errors/src/domains/oauth/InvalidClientError';
import {InvalidClientSecretError} from '@fluxer/errors/src/domains/oauth/InvalidClientSecretError';
import {InvalidGrantError} from '@fluxer/errors/src/domains/oauth/InvalidGrantError';
import {InvalidRedirectUriError} from '@fluxer/errors/src/domains/oauth/InvalidRedirectUriError';
import {InvalidScopeError} from '@fluxer/errors/src/domains/oauth/InvalidScopeError';
import {MissingClientSecretError} from '@fluxer/errors/src/domains/oauth/MissingClientSecretError';
import {MissingRedirectUriError} from '@fluxer/errors/src/domains/oauth/MissingRedirectUriError';

interface OAuth2ServiceDeps {
	applicationRepository?: IApplicationRepository;
	oauth2TokenRepository?: IOAuth2TokenRepository;
}

export {ACCESS_TOKEN_TTL_SECONDS};

export class OAuth2Service {
	private applications: IApplicationRepository;
	private tokens: IOAuth2TokenRepository;

	constructor(
		private readonly apiContext: ApiContext,
		deps: OAuth2ServiceDeps,
	) {
		this.applications = deps.applicationRepository ?? new ApplicationRepository();
		this.tokens = deps.oauth2TokenRepository ?? new OAuth2TokenRepository();
	}

	private parseScope(scope: string): Array<string> {
		return scope.split(/[\s+]+/).filter(Boolean);
	}

	private validateRedirectUri(application: Application, redirectUri: string): boolean {
		if (application.oauth2RedirectUris.size === 0) {
			return false;
		}
		return application.oauth2RedirectUris.has(redirectUri);
	}

	async resolveErrorRedirectBase(clientId: string, redirectUri?: string): Promise<string> {
		if (!redirectUri) {
			return Config.endpoints.webApp;
		}
		let parsedClientId: ApplicationID;
		try {
			parsedClientId = BigInt(clientId) as ApplicationID;
		} catch {
			return Config.endpoints.webApp;
		}
		const application = await this.applications.getApplication(parsedClientId);
		if (!application || !this.validateRedirectUri(application, redirectUri)) {
			return Config.endpoints.webApp;
		}
		return redirectUri;
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
	}): Promise<{
		redirectTo: string;
	}> {
		let parsedClientId: ApplicationID;
		try {
			parsedClientId = BigInt(params.clientId) as ApplicationID;
		} catch {
			throw new InvalidClientError();
		}
		const application = await this.applications.getApplication(parsedClientId);
		if (!application) {
			throw new InvalidClientError();
		}
		const scopeSet = new Set<string>(this.parseScope(params.scope));
		for (const s of scopeSet) {
			if (!isOAuth2Scope(s)) {
				throw new InvalidScopeError();
			}
		}
		if (scopeSet.has('bot') && !application.botIsPublic && params.userId !== application.ownerUserId) {
			throw new BotIsPrivateError();
		}
		const isBotOnly = scopeSet.size === 1 && scopeSet.has('bot');
		const redirectUri = params.redirectUri;
		const requireRedirect = !isBotOnly || application.botRequireCodeGrant;
		if (!redirectUri && requireRedirect) {
			throw new MissingRedirectUriError();
		}
		if (redirectUri && !this.validateRedirectUri(application, redirectUri)) {
			throw new InvalidRedirectUriError();
		}
		const resolvedRedirectUri = redirectUri ?? Config.endpoints.webApp;
		let loc: URL;
		try {
			loc = new URL(resolvedRedirectUri);
		} catch {
			throw new InvalidRequestError();
		}
		const codeRow: OAuth2AuthorizationCodeRow = {
			code: generateOAuthTokenSecret(),
			application_id: application.applicationId,
			user_id: params.userId,
			redirect_uri: loc.toString(),
			scope: scopeSet,
			nonce: null,
			code_challenge: params.codeChallenge ?? null,
			code_challenge_method: params.codeChallengeMethod ?? null,
			created_at: new Date(),
		};
		await this.tokens.createAuthorizationCode(codeRow);
		loc.searchParams.set('code', codeRow.code);
		if (params.state) {
			loc.searchParams.set('state', params.state);
		}
		return {redirectTo: loc.toString()};
	}

	private basicAuth(credentialsHeader?: string): {
		clientId: string;
		clientSecret: string;
	} | null {
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
		const scope = filterOAuth2ScopeSet(args.scope);
		const accessToken: OAuth2AccessTokenRow = {
			token_: generateOAuthTokenSecret(),
			application_id: args.application.applicationId,
			user_id: args.userId,
			scope,
			created_at: new Date(),
		};
		const createdAccess = await this.tokens.createAccessToken(accessToken);
		let refreshToken: OAuth2RefreshTokenRow | undefined;
		if (args.userId) {
			const row: OAuth2RefreshTokenRow = {
				token_: generateOAuthTokenSecret(),
				application_id: args.application.applicationId,
				user_id: args.userId,
				scope,
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
			scope: sortOAuth2Scopes(scope).join(' '),
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
		codeVerifier?: string;
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
		let parsedClientId: ApplicationID;
		try {
			parsedClientId = BigInt(clientId) as ApplicationID;
		} catch {
			throw new InvalidClientError();
		}
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
			throw new MissingClientSecretError();
		}
		if (application.clientSecretHash) {
			const ok = await verifyPassword({password: clientSecret, passwordHash: application.clientSecretHash});
			if (!ok) {
				Logger.debug(
					{application_id: application.applicationId.toString()},
					'OAuth2 tokenExchange: client_secret verification failed',
				);
				throw new InvalidClientSecretError();
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
			const expectedRedirectUri = authCode.redirectUri ?? '';
			const providedRedirectUri = params.redirectUri ?? '';
			if (expectedRedirectUri !== providedRedirectUri) {
				Logger.debug(
					{expected: expectedRedirectUri, got: providedRedirectUri},
					'OAuth2 tokenExchange: redirect_uri mismatch',
				);
				throw new InvalidGrantError();
			}
			if (authCode.codeChallenge) {
				const verifier = params.codeVerifier;
				if (!verifier) {
					throw new InvalidGrantError();
				}
				const method = authCode.codeChallengeMethod ?? 'plain';
				const expected = method === 'S256' ? createHash('sha256').update(verifier).digest('base64url') : verifier;
				if (expected !== authCode.codeChallenge) {
					throw new InvalidGrantError();
				}
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
		await this.tokens.deleteRefreshToken(params.refreshToken!, refresh.applicationId, refresh.userId);
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
		const user = await this.apiContext.services.users.findUnique(token.userId);
		if (!user) {
			throw new InvalidTokenError();
		}
		const includeEmail = token.scope.has('email');
		return mapUserToOAuthResponse(user, {includeEmail});
	}

	async introspect(
		tokenStr: string,
		auth: {
			clientId: ApplicationID;
			clientSecret?: string | null;
		},
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
			const valid = await verifyPassword({password: auth.clientSecret, passwordHash: application.clientSecretHash});
			if (!valid) {
				return {active: false};
			}
		}
		const accessToken = await this.tokens.getAccessToken(tokenStr);
		if (accessToken && accessToken.applicationId === application.applicationId) {
			return {
				active: true,
				client_id: accessToken.applicationId.toString(),
				sub: accessToken.userId ? accessToken.userId.toString() : undefined,
				scope: sortOAuth2Scopes(filterOAuth2ScopeSet(accessToken.scope)).join(' '),
				token_type: 'Bearer',
				exp: Math.floor((accessToken.createdAt.getTime() + ACCESS_TOKEN_TTL_SECONDS * 1000) / 1000),
				iat: Math.floor(accessToken.createdAt.getTime() / 1000),
			};
		}
		const refreshToken = await this.tokens.getRefreshToken(tokenStr);
		if (refreshToken && refreshToken.applicationId === application.applicationId) {
			return {
				active: true,
				client_id: refreshToken.applicationId.toString(),
				sub: refreshToken.userId.toString(),
				scope: sortOAuth2Scopes(filterOAuth2ScopeSet(refreshToken.scope)).join(' '),
				token_type: 'refresh_token',
				iat: Math.floor(refreshToken.createdAt.getTime() / 1000),
			};
		}
		return {active: false};
	}

	async revoke(
		tokenStr: string,
		tokenTypeHint: 'access_token' | 'refresh_token' | undefined,
		auth: {
			clientId: ApplicationID;
			clientSecret?: string | null;
		},
	): Promise<void> {
		const application = await this.applications.getApplication(auth.clientId);
		if (!application) {
			throw new InvalidClientError();
		}
		if (application.clientSecretHash) {
			const valid = auth.clientSecret
				? await verifyPassword({password: auth.clientSecret, passwordHash: application.clientSecretHash})
				: false;
			if (!valid) {
				throw new InvalidClientSecretError();
			}
		}
		if (tokenTypeHint === 'refresh_token') {
			const refresh = await this.tokens.getRefreshToken(tokenStr);
			if (refresh && refresh.applicationId === application.applicationId) {
				await this.tokens.deleteAllTokensForUserAndApplication(refresh.userId, application.applicationId);
				return;
			}
		}
		const access = await this.tokens.getAccessToken(tokenStr);
		if (access && access.applicationId === application.applicationId) {
			if (access.userId) {
				await this.tokens.deleteAllTokensForUserAndApplication(access.userId, application.applicationId);
				return;
			}
			await this.tokens.deleteAccessToken(tokenStr, application.applicationId, access.userId);
			return;
		}
	}
}
