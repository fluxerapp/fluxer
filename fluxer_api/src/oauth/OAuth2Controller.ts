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
import type {HonoApp, HonoEnv} from '~/App';
import {requireSudoMode} from '~/auth/services/SudoVerificationService';
import {createApplicationID, createGuildID, createRoleID} from '~/BrandedTypes';
import {ALL_PERMISSIONS, Permissions} from '~/Constants';
import {JoinSourceTypes} from '~/constants/Guild';
import {
	AccessDeniedError,
	BotAlreadyInGuildError,
	BotUserNotFoundError,
	InvalidClientError,
	InvalidGrantError,
	InvalidRequestError,
	InvalidTokenError,
	MissingPermissionsError,
	NotABotApplicationError,
	UnknownApplicationError,
	UnknownGuildMemberError,
} from '~/Errors';
import {Logger} from '~/Logger';
import {DefaultUserOnly, LoginRequiredAllowSuspicious} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {SudoModeMiddleware} from '~/middleware/SudoModeMiddleware';
import {
	AuthorizeConsentRequest,
	AuthorizeRequest,
	IntrospectRequestForm,
	RevokeRequestForm,
	TokenRequest,
} from '~/oauth/OAuthModels';
import {parseClientCredentials} from '~/oauth/utils/parseClientCredentials';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {SudoVerificationSchema, type z} from '~/Schema';
import {mapUserToOAuthResponse, mapUserToPartialResponse} from '~/user/UserMappers';
import {Validator} from '~/Validator';
import {ApplicationNotOwnedError} from './ApplicationService';
import {mapApplicationToResponse, mapBotTokenResetResponse} from './OAuth2Mappers';
import {ACCESS_TOKEN_TTL_SECONDS} from './OAuth2Service';

type FormContext<TForm> = Context<HonoEnv, string, {out: {form: TForm}}>;

const extractBearerToken = (authHeader: string): string | null => {
	const match = /^Bearer\s+(.+)$/.exec(authHeader);
	return match ? match[1] : null;
};

const handleTokenExchange = async (ctx: FormContext<z.infer<typeof TokenRequest>>, logPrefix: string) => {
	try {
		const form = ctx.req.valid('form');
		const hasAuthHeader = !!ctx.req.header('authorization');
		const isAuthorizationCodeRequest = form.grant_type === 'authorization_code';
		const isRefreshRequest = form.grant_type === 'refresh_token';

		Logger.debug(
			{
				grant_type: form.grant_type,
				client_id_present: form.client_id != null,
				redirect_uri_present: isAuthorizationCodeRequest ? form.redirect_uri != null : undefined,
				code_len: isAuthorizationCodeRequest ? form.code.length : undefined,
				refresh_token_len: isRefreshRequest ? form.refresh_token.length : undefined,
				auth_header_basic: hasAuthHeader && /^Basic\s+/i.test(ctx.req.header('authorization') ?? ''),
			},
			`${logPrefix} token request received`,
		);

		if (form.grant_type === 'authorization_code') {
			const result = await ctx.get('oauth2Service').tokenExchange({
				headersAuthorization: ctx.req.header('authorization') ?? undefined,
				grantType: 'authorization_code',
				code: form.code,
				redirectUri: form.redirect_uri,
				clientId: form.client_id ? form.client_id.toString() : undefined,
				clientSecret: form.client_secret,
			});
			return ctx.json(result);
		} else {
			const result = await ctx.get('oauth2Service').tokenExchange({
				headersAuthorization: ctx.req.header('authorization') ?? undefined,
				grantType: 'refresh_token',
				refreshToken: form.refresh_token,
				clientId: form.client_id ? form.client_id.toString() : undefined,
				clientSecret: form.client_secret,
			});
			return ctx.json(result);
		}
	} catch (err: unknown) {
		if (err instanceof InvalidGrantError) {
			Logger.warn({error: (err as Error).message}, `${logPrefix} token request failed`);
		}
		throw err;
	}
};

const handleUserInfo = async (ctx: Context<HonoEnv>) => {
	const authHeader = ctx.req.header('authorization') ?? '';
	const token = extractBearerToken(authHeader);

	if (!token) {
		throw new InvalidTokenError();
	}

	const user = await ctx.get('oauth2Service').userInfo(token);
	return ctx.json(user);
};

const handleRevoke = async (ctx: FormContext<z.infer<typeof RevokeRequestForm>>) => {
	const body = ctx.req.valid('form');
	const {clientId: clientIdStr, clientSecret: secret} = parseClientCredentials(
		ctx.req.header('authorization') ?? undefined,
		body.client_id,
		body.client_secret,
	);
	if (!secret) {
		throw new InvalidClientError('Missing client_secret');
	}
	await ctx.get('oauth2Service').revoke(body.token, body.token_type_hint ?? undefined, {
		clientId: createApplicationID(BigInt(clientIdStr)),
		clientSecret: secret,
	});
	return ctx.body(null, 200);
};

const handleIntrospect = async (ctx: FormContext<z.infer<typeof IntrospectRequestForm>>) => {
	const body = ctx.req.valid('form');
	const {clientId: clientIdStr, clientSecret: secret} = parseClientCredentials(
		ctx.req.header('authorization') ?? undefined,
		body.client_id,
		body.client_secret,
	);
	if (!secret) {
		throw new InvalidClientError('Missing client_secret');
	}
	const result = await ctx.get('oauth2Service').introspect(body.token, {
		clientId: createApplicationID(BigInt(clientIdStr)),
		clientSecret: secret,
	});
	return ctx.json(result);
};

export const OAuth2Controller = (app: HonoApp) => {
	app.get(
		'/oauth2/authorize',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_AUTHORIZE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('query', AuthorizeRequest),
		async (ctx) => {
			const q = ctx.req.valid('query');
			Logger.info(
				{client_id: q.client_id?.toString?.(), scope: q.scope},
				'GET /oauth2/authorize called; not supported',
			);
			throw new InvalidRequestError('GET /oauth2/authorize is not supported. Use POST /oauth2/authorize/consent.');
		},
	);

	app.post(
		'/oauth2/authorize/consent',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_AUTHORIZE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', AuthorizeConsentRequest),
		async (ctx) => {
			const body: z.infer<typeof AuthorizeConsentRequest> = ctx.req.valid('json');
			const user = ctx.get('user');
			const scopeStr: string = body.scope;
			const scopeSet = new Set(scopeStr.split(/\s+/).filter(Boolean));
			const isBotOnly = scopeSet.size === 1 && scopeSet.has('bot');
			const responseType = body.response_type ?? (isBotOnly ? undefined : 'code');
			const guildId = body.guild_id ? createGuildID(body.guild_id) : null;
			let requestedPermissions: bigint | null = null;
			if (body.permissions !== undefined) {
				try {
					requestedPermissions = BigInt(body.permissions);
				} catch {
					throw new InvalidRequestError('permissions must be a valid integer');
				}
				if (requestedPermissions < 0) {
					throw new InvalidRequestError('permissions must be non-negative');
				}
				requestedPermissions = requestedPermissions & ALL_PERMISSIONS;
			}

			if (!isBotOnly && responseType !== 'code') {
				throw new InvalidRequestError('response_type must be code for non-bot scopes');
			}
			if (!isBotOnly && !body.redirect_uri) {
				throw new InvalidRequestError('redirect_uri required for non-bot scopes');
			}

			const {redirectTo} = await ctx.get('oauth2Service').authorizeAndConsent({
				clientId: body.client_id.toString(),
				redirectUri: body.redirect_uri,
				scope: body.scope,
				state: body.state ?? undefined,
				responseType: responseType as 'code' | undefined,
				userId: user.id,
			});

			const authCode = (() => {
				try {
					const url = new URL(redirectTo);
					return url.searchParams.get('code');
				} catch {
					return null;
				}
			})();

			if (scopeSet.has('bot') && guildId) {
				try {
					const applicationId = createApplicationID(BigInt(body.client_id));
					const application = await ctx.get('applicationRepository').getApplication(applicationId);
					if (!application || !application.botUserId) {
						throw new NotABotApplicationError();
					}

					const botUserId = application.botUserId;
					const hasManageGuild = await ctx.get('gatewayService').checkPermission({
						guildId,
						userId: user.id,
						permission: Permissions.MANAGE_GUILD,
					});

					if (!hasManageGuild) {
						throw new MissingPermissionsError();
					}

					try {
						await ctx.get('guildService').members.getMember({
							userId: user.id,
							targetId: botUserId,
							guildId,
							requestCache: ctx.get('requestCache'),
						});
						throw new BotAlreadyInGuildError();
					} catch (err) {
						if (!(err instanceof UnknownGuildMemberError)) {
							throw err;
						}
					}

					await ctx.get('guildService').members.addUserToGuild({
						userId: botUserId,
						guildId,
						skipGuildLimitCheck: true,
						skipBanCheck: true,
						joinSourceType: JoinSourceTypes.BOT_INVITE,
						requestCache: ctx.get('requestCache'),
						initiatorId: user.id,
					});

					if (requestedPermissions && requestedPermissions > 0n) {
						const role = await ctx.get('guildService').createRole({
							userId: user.id,
							guildId,
							data: {
								name: `${application.name}`,
								color: 0,
								permissions: requestedPermissions,
							},
						});

						await ctx.get('guildService').members.addMemberRole({
							userId: user.id,
							targetId: botUserId,
							guildId,
							roleId: createRoleID(BigInt(role.id)),
							requestCache: ctx.get('requestCache'),
						});
					}
				} catch (err) {
					if (authCode) {
						await ctx.get('oauth2TokenRepository').deleteAuthorizationCode(authCode);
					}
					throw err;
				}
			}

			Logger.info({redirectTo}, 'OAuth2 consent: returning redirect URL');
			return ctx.json({redirect_to: redirectTo});
		},
	);

	app.post(
		'/oauth2/token',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_TOKEN),
		Validator('form', TokenRequest),
		async (ctx) => handleTokenExchange(ctx, 'OAuth2'),
	);

	app.get('/oauth2/userinfo', RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT), async (ctx) => {
		return handleUserInfo(ctx);
	});

	app.post(
		'/oauth2/token/revoke',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		Validator('form', RevokeRequestForm),
		async (ctx) => handleRevoke(ctx),
	);

	app.post(
		'/oauth2/introspect',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		Validator('form', IntrospectRequestForm),
		async (ctx) => handleIntrospect(ctx),
	);

	app.get('/oauth2/@me', LoginRequiredAllowSuspicious, DefaultUserOnly, async (ctx) => {
		const authHeader = ctx.req.header('authorization') ?? '';
		const token = extractBearerToken(authHeader);

		if (!token) {
			throw new InvalidTokenError();
		}

		try {
			const tokenData = await ctx.get('oauth2TokenRepository').getAccessToken(token);
			if (!tokenData) {
				throw new InvalidTokenError();
			}

			const application = await ctx.get('applicationRepository').getApplication(tokenData.applicationId);
			if (!application) {
				throw new InvalidTokenError();
			}

			const scopes = Array.from(tokenData.scope);
			const expiresAt = new Date(tokenData.createdAt.getTime() + ACCESS_TOKEN_TTL_SECONDS * 1000);
			const response: {
				application: {
					id: string;
					name: string;
					icon: null;
					description: null;
					bot_public: boolean;
					bot_require_code_grant: boolean;
					verify_key: null;
					flags: number;
				};
				scopes: Array<string>;
				expires: string;
				user?: ReturnType<typeof mapUserToOAuthResponse>;
			} = {
				application: {
					id: application.applicationId.toString(),
					name: application.name,
					icon: null,
					description: null,
					bot_public: application.botIsPublic,
					bot_require_code_grant: false,
					verify_key: null,
					flags: 0,
				},
				scopes,
				expires: expiresAt.toISOString(),
			};

			if (tokenData.userId && tokenData.scope.has('identify')) {
				const user = await ctx.get('userRepository').findUnique(tokenData.userId);
				if (user) {
					response.user = mapUserToOAuthResponse(user, {includeEmail: tokenData.scope.has('email')});
				}
			}

			return ctx.json(response);
		} catch (err) {
			if (err instanceof InvalidTokenError) {
				throw err;
			}
			throw new InvalidTokenError();
		}
	});

	app.get(
		'/oauth2/applications/:id/public',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		async (ctx) => {
			const appId = createApplicationID(BigInt(ctx.req.param('id')));
			const application = await ctx.get('applicationRepository').getApplication(appId);
			if (!application) {
				throw new UnknownApplicationError();
			}

			let botUser = null;
			if (application.hasBotUser() && application.getBotUserId()) {
				botUser = await ctx.get('userRepository').findUnique(application.getBotUserId()!);
			}

			const scopes: Array<string> = [];
			if (application.hasBotUser()) {
				scopes.push('bot');
			}

			return ctx.json({
				id: application.applicationId.toString(),
				name: application.name,
				icon: botUser?.avatarHash ?? null,
				description: null,
				redirect_uris: Array.from(application.oauth2RedirectUris),
				scopes,
				bot_public: application.botIsPublic,
				bot: botUser ? mapUserToPartialResponse(botUser) : null,
			});
		},
	);

	const extractBotToken = (authHeader: string): string | null => {
		const match = /^Bot\s+(.+)$/i.exec(authHeader);
		return match ? match[1] : null;
	};

	app.get('/applications/@me', async (ctx) => {
		const authHeader = ctx.req.header('authorization') ?? '';
		const botToken = extractBotToken(authHeader);
		if (!botToken) {
			throw new InvalidTokenError();
		}

		const botUserId = await ctx.get('botAuthService').validateBotToken(botToken);
		if (!botUserId) {
			throw new InvalidTokenError();
		}

		const [appIdStr] = botToken.split('.');
		if (!appIdStr) {
			throw new InvalidTokenError();
		}

		const application = await ctx.get('applicationRepository').getApplication(createApplicationID(BigInt(appIdStr)));
		if (!application) {
			throw new InvalidTokenError();
		}

		const response: {
			id: string;
			name: string;
			icon: null;
			description: null;
			bot_public: boolean;
			bot_require_code_grant: boolean;
			verify_key: null;
			flags: number;
			bot?: ReturnType<typeof mapUserToPartialResponse>;
		} = {
			id: application.applicationId.toString(),
			name: application.name,
			icon: null,
			description: null,
			bot_public: application.botIsPublic,
			bot_require_code_grant: false,
			verify_key: null,
			flags: 0,
		};

		if (application.hasBotUser() && application.getBotUserId()) {
			const botUser = await ctx.get('userRepository').findUnique(application.getBotUserId()!);
			if (botUser) {
				response.bot = mapUserToPartialResponse(botUser);
			}
		}

		return ctx.json(response);
	});

	app.post(
		'/oauth2/applications/:id/bot/reset-token',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_ROTATE_SECRET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));

			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));

			try {
				const {token} = await ctx.get('applicationService').rotateBotToken(user.id, applicationId);

				const application = await ctx.get('applicationRepository').getApplication(applicationId);
				if (!application || !application.botUserId) {
					throw new BotUserNotFoundError();
				}

				const botUser = await ctx.get('userRepository').findUnique(application.botUserId);
				if (!botUser) {
					throw new BotUserNotFoundError();
				}

				return ctx.json(mapBotTokenResetResponse(botUser, token));
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
					throw new UnknownApplicationError();
				}
				throw err;
			}
		},
	);

	app.post(
		'/oauth2/applications/:id/client-secret/reset',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_ROTATE_SECRET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));

			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));

			try {
				const {clientSecret} = await ctx.get('applicationService').rotateClientSecret(user.id, applicationId);
				const application = await ctx.get('applicationRepository').getApplication(applicationId);
				if (!application) {
					throw new UnknownApplicationError();
				}

				return ctx.json(mapApplicationToResponse(application, {clientSecret}));
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
					throw new UnknownApplicationError();
				}
				throw err;
			}
		},
	);

	app.get(
		'/oauth2/@me/authorizations',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		async (ctx) => {
			const user = ctx.get('user');
			const refreshTokens = await ctx.get('oauth2TokenRepository').listRefreshTokensForUser(user.id);

			const appMap = new Map<
				string,
				{
					applicationId: string;
					scopes: Set<string>;
					createdAt: Date;
					application: {
						id: string;
						name: string;
						icon: string | null;
						description: null;
						bot_public: boolean;
					};
				}
			>();

			for (const token of refreshTokens) {
				const appIdStr = token.applicationId.toString();
				const existing = appMap.get(appIdStr);

				if (existing) {
					for (const scope of token.scope) {
						existing.scopes.add(scope);
					}
					if (token.createdAt < existing.createdAt) {
						existing.createdAt = token.createdAt;
					}
				} else {
					const application = await ctx.get('applicationRepository').getApplication(token.applicationId);
					if (application) {
						const nonBotScopes = new Set([...token.scope].filter((s) => s !== 'bot'));
						if (nonBotScopes.size > 0) {
							let botUser = null;
							if (application.hasBotUser() && application.getBotUserId()) {
								botUser = await ctx.get('userRepository').findUnique(application.getBotUserId()!);
							}

							appMap.set(appIdStr, {
								applicationId: appIdStr,
								scopes: nonBotScopes,
								createdAt: token.createdAt,
								application: {
									id: application.applicationId.toString(),
									name: application.name,
									icon: botUser?.avatarHash ?? null,
									description: null,
									bot_public: application.botIsPublic,
								},
							});
						}
					}
				}
			}

			const authorizations = Array.from(appMap.values()).map((entry) => ({
				application: entry.application,
				scopes: Array.from(entry.scopes),
				authorized_at: entry.createdAt.toISOString(),
			}));

			return ctx.json(authorizations);
		},
	);

	app.delete(
		'/oauth2/@me/authorizations/:applicationId',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		async (ctx) => {
			const user = ctx.get('user');
			const applicationId = createApplicationID(BigInt(ctx.req.param('applicationId')));

			const application = await ctx.get('applicationRepository').getApplication(applicationId);
			if (!application) {
				throw new UnknownApplicationError();
			}

			await ctx.get('oauth2TokenRepository').deleteAllTokensForUserAndApplication(user.id, applicationId);

			return ctx.body(null, 204);
		},
	);
};
