// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthEmail from '@app/api/auth/AuthEmail';
import * as AuthEmailRevert from '@app/api/auth/AuthEmailRevert';
import * as AuthLogin from '@app/api/auth/AuthLogin';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthRegistration from '@app/api/auth/AuthRegistration';
import * as AuthSession from '@app/api/auth/AuthSession';
import {getTokenIdHash} from '@app/api/auth/AuthUtility';
import type {DesktopHandoffService} from '@app/api/auth/services/DesktopHandoffService';
import type {SsoService} from '@app/api/auth/services/SsoService';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {
	classifyWebPushOrigin,
	encodePushSessionIdHash,
	recordPushSessionPredecessor,
} from '@app/api/user/services/WebPushOriginReplacement';
import {mapUserToPartialResponse} from '@app/api/user/UserMappers';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {generateUsernameSuggestions} from '@app/api/utils/UsernameSuggestionUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	AuthLoginResponse,
	AuthorizeIpRequest,
	AuthRegisterResponse,
	AuthSessionsResponse,
	AuthTokenWithUserIdResponse,
	EmailRevertRequest,
	ForgotPasswordRequest,
	HandoffCompleteRequest,
	HandoffInfoResponse,
	HandoffInitiateResponse,
	HandoffStatusResponse,
	IpAuthorizationPollResponse,
	LoginRequest,
	LogoutAuthSessionsRequest,
	MfaTicketRequest,
	RegisterRequest,
	ResetPasswordRequest,
	SsoCompleteRequest,
	SsoStartRequest,
	UsernameSuggestionsResponse,
	VerifyEmailRequest,
	WebAuthnAuthenticateRequest,
	WebAuthnMfaRequest,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

interface AuthRegisterRequest {
	data: RegisterRequest;
	request: Request;
	requestCache: RequestCache;
}

interface AuthLoginRequest {
	data: LoginRequest;
	request: Request;
	requestCache: RequestCache;
}

interface AuthForgotPasswordRequest {
	data: ForgotPasswordRequest;
	request: Request;
}

interface AuthResetPasswordRequest {
	data: ResetPasswordRequest;
	request: Request;
}

interface AuthRevertEmailChangeRequest {
	data: EmailRevertRequest;
	request: Request;
}

interface AuthLoginMfaRequest {
	code: string;
	ticket: string;
	request: Request;
}

interface AuthLogoutRequest {
	authToken?: string;
}

interface AuthHandoffCompleteRequest {
	data: HandoffCompleteRequest;
	clientIp: string;
	authToken?: string;
	approverOrigin?: string | null;
}

interface AuthAuthorizeIpRequest {
	data: AuthorizeIpRequest;
}

interface AuthUsernameSuggestionsRequest {
	globalName: string;
}

interface AuthPollIpRequest {
	ticket: string;
}

interface AuthWebAuthnAuthenticateRequest {
	data: WebAuthnAuthenticateRequest;
	request: Request;
}

interface AuthWebAuthnMfaRequest {
	data: WebAuthnMfaRequest;
	request: Request;
}

interface AuthLogoutAuthSessionsRequest {
	user: User;
	data: LogoutAuthSessionsRequest;
}

interface AuthHandoffInitiateRequest {
	request: Request;
}

interface AuthHandoffInfoRequest {
	code: string;
	clientIp: string;
}

interface AuthHandoffStatusRequest {
	code: string;
	clientIp: string;
	pollSecret?: string;
}

interface AuthHandoffCancelRequest {
	code: string;
	pollSecret: string;
}

export class AuthRequestService {
	constructor(
		private apiContext: ApiContext,
		private ssoService: SsoService,
		private desktopHandoffService: DesktopHandoffService,
		private registrationDependencies: AuthRegistration.RegistrationDependencies,
		private loginDependencies: AuthLogin.LoginDependencies,
	) {}

	getSsoStatus() {
		return this.ssoService.getPublicStatus();
	}

	startSso(data: SsoStartRequest) {
		return this.ssoService.startLogin({
			redirectTo: data.redirect_to ?? undefined,
			redirectUri: data.redirect_uri ?? undefined,
		});
	}

	completeSso(data: SsoCompleteRequest, request: Request) {
		return this.toSsoCompleteResponse(this.ssoService.completeLogin({code: data.code, state: data.state, request}));
	}

	async register({data, request, requestCache}: AuthRegisterRequest): Promise<AuthRegisterResponse> {
		const result = await AuthRegistration.register(this.apiContext, this.registrationDependencies, {
			data,
			request,
			requestCache,
		});
		if ('registration_pending_approval' in result) {
			return result;
		}
		return await this.toAuthLoginResponse(result);
	}

	async login({data, request, requestCache: _requestCache}: AuthLoginRequest): Promise<AuthLoginResponse> {
		const result = await AuthLogin.login(this.apiContext, this.loginDependencies, {data, request});
		return await this.toAuthLoginResponse(result);
	}

	async loginMfaTotp({code, ticket, request}: AuthLoginMfaRequest): Promise<AuthTokenWithUserIdResponse> {
		const result = await AuthLogin.loginMfaTotp(this.apiContext, {code, ticket, request});
		return await this.toAuthTokenResponse(result);
	}

	async logout({authToken}: AuthLogoutRequest): Promise<void> {
		if (authToken) {
			await AuthSession.revokeToken(this.apiContext, authToken);
		}
	}

	async verifyEmail(data: VerifyEmailRequest): Promise<void> {
		const success = await AuthEmail.verifyEmail(this.apiContext, data);
		if (!success) {
			throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_VERIFICATION_TOKEN);
		}
	}

	async resendVerificationEmail(user: User): Promise<void> {
		await AuthEmail.resendVerificationEmail(this.apiContext, user);
	}

	async forgotPassword({data, request}: AuthForgotPasswordRequest): Promise<void> {
		await AuthPassword.forgotPassword(this.apiContext, {data, request});
	}

	async validateResetPasswordToken(token: string): Promise<{
		valid: boolean;
	}> {
		const valid = await AuthPassword.validateResetToken(this.apiContext, token);
		return {valid};
	}

	async resetPassword({data, request}: AuthResetPasswordRequest): Promise<AuthLoginResponse> {
		const result = await AuthPassword.resetPassword(this.apiContext, {data, request});
		return await this.toAuthLoginResponse(result);
	}

	async revertEmailChange({data, request}: AuthRevertEmailChangeRequest): Promise<AuthLoginResponse> {
		const result = await AuthEmailRevert.revertEmailChange(this.apiContext, {
			token: data.token,
			password: data.password,
			request,
		});
		return await this.toAuthLoginResponse(result);
	}

	getAuthSessions(userId: UserID, currentSessionIdHash?: Uint8Array): Promise<AuthSessionsResponse> {
		return AuthSession.getAuthSessions(this.apiContext, userId, currentSessionIdHash);
	}

	async logoutAuthSessions({user, data}: AuthLogoutAuthSessionsRequest): Promise<void> {
		await AuthSession.logoutAuthSessions(this.apiContext, {
			user,
			sessionIdHashes: data.session_id_hashes,
		});
	}

	async completeIpAuthorization({data}: AuthAuthorizeIpRequest): Promise<void> {
		const {cache} = this.apiContext.services;
		const result = await AuthLogin.completeIpAuthorization(this.apiContext, data.token);
		const payload = JSON.stringify({token: result.token, user_id: result.user_id});
		await cache.set(`ip-auth-result:${result.ticket}`, payload, 60);
	}

	async resendIpAuthorization({ticket}: MfaTicketRequest): Promise<void> {
		await AuthLogin.resendIpAuthorization(this.apiContext, ticket);
	}

	async pollIpAuthorization({ticket}: AuthPollIpRequest): Promise<IpAuthorizationPollResponse> {
		const {cache} = this.apiContext.services;
		const result = await cache.get<string>(`ip-auth-result:${ticket}`);
		if (result) {
			const parsed = parseJsonRecord(result);
			if (typeof parsed?.token !== 'string' || typeof parsed.user_id !== 'string') {
				throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TICKET);
			}
			return {
				completed: true,
				token: parsed.token,
				user_id: parsed.user_id,
				user: await this.getUserPartial(parsed.user_id),
			};
		}
		const ticketPayload = await cache.get(AuthLogin.getTicketCacheKey(ticket));
		if (!ticketPayload) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TICKET);
		}
		return {completed: false};
	}

	async getWebAuthnAuthenticationOptions(origin: string | undefined) {
		return AuthMfa.generateWebAuthnAuthenticationOptionsDiscoverable(this.apiContext, origin);
	}

	async authenticateWebAuthnDiscoverable({data, request}: AuthWebAuthnAuthenticateRequest) {
		const user = await AuthMfa.verifyWebAuthnAuthenticationDiscoverable(this.apiContext, data.response, data.challenge);
		const [token] = await AuthLogin.createLoginSession(this.apiContext, user, request);
		return {token, user_id: user.id.toString(), user: mapUserToPartialResponse(user)};
	}

	async getWebAuthnMfaOptions({ticket}: MfaTicketRequest, origin: string | undefined) {
		return AuthMfa.generateWebAuthnAuthenticationOptionsForMfa(this.apiContext, ticket, origin);
	}

	async loginMfaWebAuthn({data, request}: AuthWebAuthnMfaRequest): Promise<AuthTokenWithUserIdResponse> {
		const result = await AuthLogin.loginMfaWebAuthn(this.apiContext, {
			response: data.response,
			challenge: data.challenge,
			ticket: data.ticket,
			request,
		});
		return await this.toAuthTokenResponse(result);
	}

	getUsernameSuggestions({globalName}: AuthUsernameSuggestionsRequest): UsernameSuggestionsResponse {
		return {suggestions: generateUsernameSuggestions(globalName)};
	}

	async initiateHandoff({request}: AuthHandoffInitiateRequest): Promise<HandoffInitiateResponse> {
		const origin = AuthSession.resolveSessionOrigin(this.apiContext, request);
		const result = await this.desktopHandoffService.initiateHandoff({
			origin,
			initiatorOrigin: request.headers.get('origin'),
		});
		return {
			code: result.code,
			expires_at: result.expiresAt.toISOString(),
			poll_secret: result.pollSecret,
		};
	}

	async getHandoffInfo({code, clientIp}: AuthHandoffInfoRequest): Promise<HandoffInfoResponse> {
		const info = await this.desktopHandoffService.getHandoffInfo(code, clientIp);
		if (info.status === 'expired' || !info.origin) {
			return {status: info.status, client_info: null};
		}
		const geo = await lookupGeoip(info.origin.ip);
		const {branding} = await getInstanceConfigRepository().getAppPublicConfig();
		const resolved = resolveSessionClientInfo({
			userAgent: info.origin.userAgent,
			reportedOs: info.origin.clientOs,
			productName: branding.product_name,
		});
		return {
			status: 'pending',
			client_info: {
				platform: resolved.platform,
				os: resolved.os,
				device: resolved.device,
				location: {
					city: geo.city,
					region: geo.region,
					country: geo.countryName,
				},
			},
		};
	}

	async completeHandoff({data, clientIp, authToken, approverOrigin}: AuthHandoffCompleteRequest): Promise<void> {
		const sessionToken = data.token ?? authToken;
		if (!sessionToken) {
			throw new UnauthorizedError();
		}
		let createdToken: string | null = null;
		const {initiatorOrigin} = await this.desktopHandoffService.completeHandoff(
			data.code,
			async (origin) => {
				const created = await AuthSession.createAdditionalAuthSessionFromToken(this.apiContext, {
					token: sessionToken,
					expectedUserId: data.user_id,
					origin,
				});
				createdToken = created.token;
				return created;
			},
			clientIp,
		);
		if (createdToken !== null) {
			await this.recordPushSessionPredecessor(createdToken, sessionToken, initiatorOrigin, approverOrigin);
		}
	}

	private async recordPushSessionPredecessor(
		createdToken: string,
		approverToken: string,
		initiatorOrigin: string | null,
		approverOrigin: string | null | undefined,
	): Promise<void> {
		const {config, kv} = this.apiContext.services;
		const {selfHosted} = config.instance;
		if (
			classifyWebPushOrigin(initiatorOrigin, selfHosted) !== 'target' ||
			classifyWebPushOrigin(approverOrigin, selfHosted) !== 'legacy'
		) {
			return;
		}
		try {
			await recordPushSessionPredecessor(
				kv,
				encodePushSessionIdHash(getTokenIdHash(this.apiContext, createdToken)),
				encodePushSessionIdHash(getTokenIdHash(this.apiContext, approverToken)),
			);
		} catch (error) {
			Logger.warn({error}, 'Failed to record the push session predecessor');
		}
	}

	async getHandoffStatus({code, clientIp, pollSecret}: AuthHandoffStatusRequest): Promise<HandoffStatusResponse> {
		const result = await this.desktopHandoffService.getHandoffStatus(code, clientIp, pollSecret);
		return {
			status: result.status,
			token: result.token,
			user_id: result.userId,
			user: result.userId ? await this.getUserPartial(result.userId) : undefined,
		};
	}

	async cancelHandoff({code, pollSecret}: AuthHandoffCancelRequest): Promise<void> {
		await this.desktopHandoffService.cancelHandoff(code, pollSecret);
	}

	private async getUserPartial(userId: string): Promise<UserPartialResponse> {
		const user = await this.apiContext.services.users.findUnique(createUserID(BigInt(userId)));
		if (!user) {
			throw new UnknownUserError();
		}
		return mapUserToPartialResponse(user);
	}

	private async toAuthTokenResponse(result: {user_id: string; token: string}): Promise<AuthTokenWithUserIdResponse> {
		return {
			...result,
			user: await this.getUserPartial(result.user_id),
		};
	}

	private async toSsoCompleteResponse(
		resultPromise: Promise<{user_id: string; token: string; redirect_to: string}>,
	): Promise<AuthTokenWithUserIdResponse & {redirect_to: string}> {
		const result = await resultPromise;
		const tokenResponse = await this.toAuthTokenResponse(result);
		return {
			...tokenResponse,
			redirect_to: result.redirect_to,
		};
	}

	private async toAuthLoginResponse(
		result:
			| {
					user_id: string;
					token: string;
			  }
			| {
					mfa: true;
					ticket: string;
					allowed_methods: Array<string>;
			  },
	): Promise<AuthLoginResponse> {
		if (!('mfa' in result)) {
			return await this.toAuthTokenResponse(result);
		}
		const allowedMethods = new Set(result.allowed_methods);
		return {
			...result,
			totp: allowedMethods.has('totp'),
			webauthn: allowedMethods.has('webauthn'),
			backup_codes: allowedMethods.has('backup_codes'),
		};
	}
}
