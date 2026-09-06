// SPDX-License-Identifier: AGPL-3.0-or-later

import {canAuthorizeBotInvite, normalizeBotInvitePermissions} from '@fluxer/constants/src/BotPermissionUtils';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InvalidPermissionsIntegerError} from '@fluxer/errors/src/domains/core/InvalidPermissionsIntegerError';
import {InvalidPermissionsNegativeError} from '@fluxer/errors/src/domains/core/InvalidPermissionsNegativeError';
import {InvalidTokenError} from '@fluxer/errors/src/domains/core/InvalidTokenError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import {BotAlreadyInGuildError} from '@fluxer/errors/src/domains/oauth/BotAlreadyInGuildError';
import {BotUserNotFoundError} from '@fluxer/errors/src/domains/oauth/BotUserNotFoundError';
import {InvalidClientError} from '@fluxer/errors/src/domains/oauth/InvalidClientError';
import {InvalidGrantError} from '@fluxer/errors/src/domains/oauth/InvalidGrantError';
import {InvalidResponseTypeForNonBotError} from '@fluxer/errors/src/domains/oauth/InvalidResponseTypeForNonBotError';
import {MissingClientSecretError} from '@fluxer/errors/src/domains/oauth/MissingClientSecretError';
import {NotABotApplicationError} from '@fluxer/errors/src/domains/oauth/NotABotApplicationError';
import {RedirectUriRequiredForNonBotError} from '@fluxer/errors/src/domains/oauth/RedirectUriRequiredForNonBotError';
import {UnknownApplicationError} from '@fluxer/errors/src/domains/oauth/UnknownApplicationError';
import type {
	ApplicationResponse,
	ApplicationsMeResponse,
	AuthorizeConsentRequest,
	BotTokenResetResponse,
	IntrospectRequestForm,
	OAuth2ConsentResponse,
	OAuth2IntrospectResponse,
	OAuth2MeResponse,
	OAuth2TokenResponse,
	OAuth2UserInfoResponse,
	RevokeRequestForm,
	TokenRequest,
} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import type {Context} from 'hono';
import type {z} from 'zod';
import type {ApiContext} from '../ApiContext';
import type {SudoVerificationBody} from '../auth/services/SudoVerificationService';
import {requireSudoMode} from '../auth/services/SudoVerificationService';
import {createApplicationID, createChannelID, createGuildID, createRoleID, type UserID} from '../BrandedTypes';
import type {ChannelService} from '../channel/services/ChannelService';
import type {GuildService} from '../guild/services/GuildService';
import {Logger} from '../Logger';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import {mapUserToOAuthResponse, mapUserToPartialResponse} from '../user/UserMappers';
import {verifyPassword} from '../utils/PasswordUtils';
import type {ApplicationService} from './ApplicationService';
import {ApplicationNotOwnedError} from './ApplicationService';
import type {BotAuthService} from './BotAuthService';
import {
	mapApplicationToResponse,
	mapBotProfileToResponse,
	mapBotTokenResetResponse,
	mapBotUserToResponse,
} from './OAuth2Mappers';
import {filterOAuth2Scopes} from './OAuth2ScopeUtils';
import {ACCESS_TOKEN_TTL_SECONDS, type OAuth2Service} from './OAuth2Service';
import type {IApplicationRepository} from './repositories/IApplicationRepository';
import type {IOAuth2TokenRepository} from './repositories/IOAuth2TokenRepository';
import {parseClientCredentials} from './utils/ParseClientCredentials';

const COMPAT_VERIFY_KEY_PLACEHOLDER = '0'.repeat(64);

export class OAuth2RequestService {
	constructor(
		private readonly apiContext: ApiContext,
		private readonly oauth2Service: OAuth2Service,
		private readonly applicationRepository: IApplicationRepository,
		private readonly oauth2TokenRepository: IOAuth2TokenRepository,
		private readonly botAuthService: BotAuthService,
		private readonly applicationService: ApplicationService,
		private readonly guildService: GuildService,
		private readonly channelService: ChannelService,
	) {}

	async tokenExchange(params: {
		form: z.infer<typeof TokenRequest>;
		authorizationHeader?: string;
		logPrefix: string;
	}): Promise<OAuth2TokenResponse> {
		try {
			const {form, authorizationHeader, logPrefix} = params;
			const hasAuthHeader = !!authorizationHeader;
			const isAuthorizationCodeRequest = form.grant_type === 'authorization_code';
			const isRefreshRequest = form.grant_type === 'refresh_token';
			Logger.debug(
				{
					grant_type: form.grant_type,
					client_id_present: form.client_id != null,
					redirect_uri_present: isAuthorizationCodeRequest ? form.redirect_uri != null : undefined,
					code_len: isAuthorizationCodeRequest ? form.code.length : undefined,
					refresh_token_len: isRefreshRequest ? form.refresh_token.length : undefined,
					auth_header_basic: hasAuthHeader && /^Basic\s+/i.test(authorizationHeader ?? ''),
				},
				`${logPrefix} token request received`,
			);
			if (form.grant_type === 'authorization_code') {
				const response = await this.oauth2Service.tokenExchange({
					headersAuthorization: authorizationHeader,
					grantType: 'authorization_code',
					code: form.code,
					redirectUri: form.redirect_uri,
					clientId: form.client_id ? form.client_id.toString() : undefined,
					clientSecret: form.client_secret,
					codeVerifier: form.code_verifier,
				});
				return this.requireTokenResponseFields(response);
			}
			const response = await this.oauth2Service.tokenExchange({
				headersAuthorization: authorizationHeader,
				grantType: 'refresh_token',
				refreshToken: form.refresh_token,
				clientId: form.client_id ? form.client_id.toString() : undefined,
				clientSecret: form.client_secret,
			});
			return this.requireTokenResponseFields(response);
		} catch (err: unknown) {
			if (err instanceof InvalidGrantError) {
				Logger.warn({error: (err as Error).message}, `${params.logPrefix} token request failed`);
			}
			throw err;
		}
	}

	async userInfo(authorizationHeader: string | undefined): Promise<OAuth2UserInfoResponse> {
		const token = this.extractBearerToken(authorizationHeader ?? '');
		if (!token) {
			throw new InvalidTokenError();
		}
		return this.oauth2Service.userInfo(token);
	}

	private requireTokenResponseFields(response: {
		access_token: string;
		token_type: string;
		expires_in: number;
		scope?: string;
		refresh_token?: string;
	}): OAuth2TokenResponse {
		if (!response.refresh_token || !response.scope) {
			throw new InvalidGrantError();
		}
		return {
			access_token: response.access_token,
			token_type: response.token_type,
			expires_in: response.expires_in,
			refresh_token: response.refresh_token,
			scope: response.scope,
		};
	}

	async revoke(params: {form: z.infer<typeof RevokeRequestForm>; authorizationHeader?: string}): Promise<void> {
		const {clientId: clientIdStr, clientSecret: secret} = parseClientCredentials(
			params.authorizationHeader,
			params.form.client_id,
			params.form.client_secret,
		);
		if (!secret) {
			throw new MissingClientSecretError();
		}
		await this.oauth2Service.revoke(params.form.token, params.form.token_type_hint ?? undefined, {
			clientId: createApplicationID(BigInt(clientIdStr)),
			clientSecret: secret,
		});
	}

	async introspect(params: {
		form: z.infer<typeof IntrospectRequestForm>;
		authorizationHeader?: string;
	}): Promise<OAuth2IntrospectResponse> {
		const {clientId: clientIdStr, clientSecret: secret} = parseClientCredentials(
			params.authorizationHeader,
			params.form.client_id,
			params.form.client_secret,
		);
		if (!secret) {
			throw new MissingClientSecretError();
		}
		const applicationId = createApplicationID(BigInt(clientIdStr));
		const application = await this.applicationRepository.getApplication(applicationId);
		if (!application) {
			throw new InvalidClientError();
		}
		if (application.clientSecretHash) {
			const valid = await verifyPassword({password: secret, passwordHash: application.clientSecretHash});
			if (!valid) {
				throw new InvalidClientError();
			}
		}
		return this.oauth2Service.introspect(params.form.token, {
			clientId: applicationId,
			clientSecret: secret,
		});
	}

	async authorizeConsent(params: {
		body: z.infer<typeof AuthorizeConsentRequest>;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<OAuth2ConsentResponse> {
		const scopeStr = params.body.scope;
		const scopeSet = new Set(scopeStr.split(/[\s+]+/).filter(Boolean));
		const isBotOnly = scopeSet.size === 1 && scopeSet.has('bot');
		const responseType = params.body.response_type ?? (isBotOnly ? undefined : 'code');
		const guildId = params.body.guild_id ? createGuildID(params.body.guild_id) : null;
		const channelId = params.body.channel_id ? createChannelID(params.body.channel_id) : null;
		if (guildId && channelId) {
			throw InputValidationError.create('channel_id', 'channel_id cannot be used with guild_id');
		}
		let requestedPermissions: bigint | null = null;
		if (params.body.permissions !== undefined) {
			try {
				requestedPermissions = BigInt(params.body.permissions);
			} catch {
				throw new InvalidPermissionsIntegerError();
			}
			if (requestedPermissions < 0) {
				throw new InvalidPermissionsNegativeError();
			}
			requestedPermissions = normalizeBotInvitePermissions(requestedPermissions);
		}
		if (!isBotOnly && responseType !== 'code') {
			throw new InvalidResponseTypeForNonBotError();
		}
		if (!isBotOnly && !params.body.redirect_uri) {
			throw new RedirectUriRequiredForNonBotError();
		}
		const {redirectTo} = await this.oauth2Service.authorizeAndConsent({
			clientId: params.body.client_id.toString(),
			redirectUri: params.body.redirect_uri,
			scope: params.body.scope,
			state: params.body.state ?? undefined,
			codeChallenge: params.body.code_challenge,
			codeChallengeMethod: params.body.code_challenge_method as 'S256' | 'plain' | undefined,
			responseType: responseType as 'code' | undefined,
			userId: params.userId,
		});
		const authCode = (() => {
			try {
				const url = new URL(redirectTo);
				return url.searchParams.get('code');
			} catch {
				return null;
			}
		})();
		if (scopeSet.has('bot') && (guildId || channelId)) {
			try {
				const applicationId = createApplicationID(BigInt(params.body.client_id));
				const application = await this.applicationRepository.getApplication(applicationId);
				if (!application || !application.botUserId) {
					throw new NotABotApplicationError();
				}
				const botUserId = application.botUserId;
				if (guildId) {
					const userPermissions = await this.apiContext.services.gateway.getUserPermissions({
						guildId,
						userId: params.userId,
					});
					if (
						!canAuthorizeBotInvite({
							userPermissions,
							requestedPermissions,
						})
					) {
						throw new MissingPermissionsError();
					}
					try {
						await this.guildService.members.getMember({
							userId: params.userId,
							targetId: botUserId,
							guildId,
							requestCache: params.requestCache,
						});
						throw new BotAlreadyInGuildError();
					} catch (err) {
						if (!(err instanceof UnknownGuildMemberError)) {
							throw err;
						}
					}
					await this.guildService.members.addUserToGuild({
						skipRiskGate: true,
						userId: botUserId,
						guildId,
						skipGuildLimitCheck: true,
						skipBanCheck: true,
						joinSourceType: JoinSourceTypes.BOT_INVITE,
						inviterId: params.userId,
						requestCache: params.requestCache,
						initiatorId: params.userId,
					});
					if (requestedPermissions && requestedPermissions > 0n) {
						const role = await this.guildService.roles.systemCreateRole({
							initiatorId: params.userId,
							guildId,
							data: {
								name: `${application.name}`,
								color: 0,
								permissions: requestedPermissions,
							},
						});
						await this.guildService.members.systemAddMemberRole({
							targetId: botUserId,
							guildId,
							roleId: createRoleID(BigInt(role.id)),
							initiatorId: params.userId,
							requestCache: params.requestCache,
						});
					}
				} else if (channelId) {
					await this.channelService.groupDms.addBotRecipientToChannel({
						userId: params.userId,
						channelId,
						botUserId,
						requestCache: params.requestCache,
					});
				}
			} catch (err) {
				if (authCode) {
					await this.oauth2TokenRepository.deleteAuthorizationCode(authCode);
				}
				throw err;
			}
		}
		Logger.info(
			{clientId: params.body.client_id.toString(), userId: params.userId.toString()},
			'OAuth2 consent: returning redirect URL',
		);
		return {redirect_to: redirectTo};
	}

	async getMe(authorizationHeader: string | undefined): Promise<OAuth2MeResponse> {
		const token = this.extractBearerToken(authorizationHeader ?? '');
		if (!token) {
			throw new InvalidTokenError();
		}
		try {
			const tokenData = await this.oauth2TokenRepository.getAccessToken(token);
			if (!tokenData) {
				throw new InvalidTokenError();
			}
			const application = await this.applicationRepository.getApplication(tokenData.applicationId);
			if (!application) {
				throw new InvalidTokenError();
			}
			const scopes = filterOAuth2Scopes(tokenData.scope);
			const expiresAt = new Date(tokenData.createdAt.getTime() + ACCESS_TOKEN_TTL_SECONDS * 1000);
			const response: OAuth2MeResponse = {
				application: {
					id: application.applicationId.toString(),
					name: application.name,
					icon: null,
					description: null,
					bot_public: application.botIsPublic,
					bot_require_code_grant: application.botRequireCodeGrant,
					flags: 0,
				},
				scopes,
				expires: expiresAt.toISOString(),
			};
			if (tokenData.userId && tokenData.scope.has('identify')) {
				const user = await this.apiContext.services.users.findUnique(tokenData.userId);
				if (user) {
					response.user = mapUserToOAuthResponse(user, {includeEmail: tokenData.scope.has('email')});
				}
			}
			return response;
		} catch (err) {
			if (err instanceof InvalidTokenError) {
				throw err;
			}
			throw new InvalidTokenError();
		}
	}

	async getApplicationPublic(applicationId: bigint, requestingUserId?: UserID) {
		const application = await this.applicationRepository.getApplication(createApplicationID(applicationId));
		if (!application) {
			throw new UnknownApplicationError();
		}
		let botUser = null;
		if (application.hasBotUser() && application.getBotUserId()) {
			botUser = await this.apiContext.services.users.findUnique(application.getBotUserId()!);
		}
		const requestingUser = requestingUserId ? await this.apiContext.services.users.findUnique(requestingUserId) : null;
		const scopes: Array<string> = [];
		if (application.hasBotUser()) {
			scopes.push('bot');
		}
		const isOwner = requestingUserId !== undefined && requestingUserId === application.ownerUserId;
		return {
			id: application.applicationId.toString(),
			name: application.name,
			icon: botUser?.avatarHash ?? null,
			description: null,
			redirect_uris: Array.from(application.oauth2RedirectUris),
			scopes,
			bot_public: application.botIsPublic || isOwner,
			bot: botUser ? mapBotProfileToResponse(botUser) : null,
			current_user: requestingUser ? mapUserToPartialResponse(requestingUser) : null,
		};
	}

	async getApplicationsMe(authorizationHeader: string | undefined): Promise<ApplicationsMeResponse> {
		const botToken = this.extractBotToken(authorizationHeader ?? '');
		if (!botToken) {
			throw new InvalidTokenError();
		}
		const botUserId = await this.botAuthService.validateBotToken(botToken);
		if (!botUserId) {
			throw new InvalidTokenError();
		}
		const [appIdStr] = botToken.split('.');
		if (!appIdStr) {
			throw new InvalidTokenError();
		}
		const application = await this.applicationRepository.getApplication(createApplicationID(BigInt(appIdStr)));
		if (!application) {
			throw new InvalidTokenError();
		}
		const owner = await this.apiContext.services.users.findUnique(application.ownerUserId);
		if (!owner) {
			throw new InvalidTokenError();
		}
		const botUser =
			application.hasBotUser() && application.getBotUserId()
				? await this.apiContext.services.users.findUnique(application.getBotUserId()!)
				: null;
		const response: ApplicationsMeResponse = {
			id: application.applicationId.toString(),
			name: application.name,
			icon: botUser?.avatarHash ?? null,
			description: botUser?.bio ?? null,
			bot_public: application.botIsPublic,
			bot_require_code_grant: application.botRequireCodeGrant,
			verify_key: COMPAT_VERIFY_KEY_PLACEHOLDER,
			owner: mapUserToPartialResponse(owner),
			redirect_uris: Array.from(application.oauth2RedirectUris),
		};
		if (botUser) {
			response.bot = mapBotUserToResponse(botUser);
		}
		return response;
	}

	async resetBotToken(params: {
		ctx: Context;
		userId: UserID;
		body: SudoVerificationBody;
		applicationId: bigint;
	}): Promise<BotTokenResetResponse> {
		await requireSudoMode(params.ctx, params.ctx.get('user'), params.body);
		try {
			const {token} = await this.applicationService.rotateBotToken(
				params.userId,
				createApplicationID(params.applicationId),
			);
			const application = await this.applicationRepository.getApplication(createApplicationID(params.applicationId));
			if (!application || !application.botUserId) {
				throw new BotUserNotFoundError();
			}
			const botUser = await this.apiContext.services.users.findUnique(application.botUserId);
			if (!botUser) {
				throw new BotUserNotFoundError();
			}
			return mapBotTokenResetResponse(botUser, token);
		} catch (err) {
			if (err instanceof ApplicationNotOwnedError) {
				throw new AccessDeniedError();
			}
			if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
				throw new UnknownApplicationError();
			}
			throw err;
		}
	}

	async resetClientSecret(params: {
		ctx: Context;
		userId: UserID;
		body: SudoVerificationBody;
		applicationId: bigint;
	}): Promise<ApplicationResponse> {
		await requireSudoMode(params.ctx, params.ctx.get('user'), params.body);
		try {
			const {clientSecret} = await this.applicationService.rotateClientSecret(
				params.userId,
				createApplicationID(params.applicationId),
			);
			const application = await this.applicationRepository.getApplication(createApplicationID(params.applicationId));
			if (!application) {
				throw new UnknownApplicationError();
			}
			return mapApplicationToResponse(application, {clientSecret});
		} catch (err) {
			if (err instanceof ApplicationNotOwnedError) {
				throw new AccessDeniedError();
			}
			if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
				throw new UnknownApplicationError();
			}
			throw err;
		}
	}

	async listAuthorizations(userId: UserID) {
		const refreshTokens = await this.oauth2TokenRepository.listRefreshTokensForUser(userId);
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
				const application = await this.applicationRepository.getApplication(token.applicationId);
				if (application) {
					const nonBotScopes = new Set(filterOAuth2Scopes(token.scope).filter((s) => s !== 'bot'));
					if (nonBotScopes.size > 0) {
						let botUser = null;
						if (application.hasBotUser() && application.getBotUserId()) {
							botUser = await this.apiContext.services.users.findUnique(application.getBotUserId()!);
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
		return Array.from(appMap.values()).map((entry) => ({
			application: entry.application,
			scopes: Array.from(entry.scopes),
			authorized_at: entry.createdAt.toISOString(),
		}));
	}

	async deleteAuthorization(userId: UserID, applicationId: bigint): Promise<void> {
		const application = await this.applicationRepository.getApplication(createApplicationID(applicationId));
		if (!application) {
			throw new UnknownApplicationError();
		}
		await this.oauth2TokenRepository.deleteAllTokensForUserAndApplication(userId, createApplicationID(applicationId));
	}

	async deleteAuthorizations(userId: UserID, applicationIds: Array<bigint>): Promise<void> {
		const uniqueApplicationIds = [...new Set(applicationIds.map((applicationId) => applicationId.toString()))].map(
			(id) => createApplicationID(BigInt(id)),
		);
		for (const applicationId of uniqueApplicationIds) {
			const application = await this.applicationRepository.getApplication(applicationId);
			if (!application) {
				throw new UnknownApplicationError();
			}
		}
		await Promise.all(
			uniqueApplicationIds.map((applicationId) =>
				this.oauth2TokenRepository.deleteAllTokensForUserAndApplication(userId, applicationId),
			),
		);
	}

	private extractBearerToken(authHeader: string): string | null {
		const match = /^Bearer\s+(.+)$/.exec(authHeader);
		return match ? match[1] : null;
	}

	private extractBotToken(authHeader: string): string | null {
		const match = /^Bot\s+(.+)$/i.exec(authHeader);
		return match ? match[1] : null;
	}
}
