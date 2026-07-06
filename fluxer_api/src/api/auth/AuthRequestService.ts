// SPDX-License-Identifier: AGPL-3.0-or-later

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
import type {ApiContext} from '../ApiContext';
import {createUserID, type UserID} from '../BrandedTypes';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {User} from '../models/User';
import {mapUserToPartialResponse} from '../user/UserMappers';
import {lookupGeoip} from '../utils/IpUtils';
import {parseJsonRecord} from '../utils/JsonBoundaryUtils';
import {resolveSessionClientInfo} from '../utils/UserAgentUtils';
import {generateUsernameSuggestions} from '../utils/UsernameSuggestionUtils';
import * as AuthEmail from './AuthEmail';
import * as AuthEmailRevert from './AuthEmailRevert';
import * as AuthLogin from './AuthLogin';
import * as AuthMfa from './AuthMfa';
import * as AuthPassword from './AuthPassword';
import * as AuthRegistration from './AuthRegistration';
import * as AuthSession from './AuthSession';
import type {DesktopHandoffService} from './services/DesktopHandoffService';
import type {SsoService} from './services/SsoService';

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
	authorizationHeader?: string;
	authToken?: string;
}

interface AuthHandoffCompleteRequest {
	data: HandoffCompleteRequest;
	request: Request;
	clientIp: string;
	authToken?: string;
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
	userAgent?: string;
	clientIp: string;
	clientPlatform?: string;
}

interface AuthHandoffInfoRequest {
	code: string;
	clientIp: string;
}

interface AuthHandoffStatusRequest {
	code: string;
	clientIp: string;
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
		const result = await AuthLogin.loginMfaTotp(this.apiContext, this.loginDependencies, {code, ticket, request});
		return await this.toAuthTokenResponse(result);
	}

	async logout({authorizationHeader, authToken}: AuthLogoutRequest): Promise<void> {
		const token = authorizationHeader ?? authToken;
		if (token) {
			await AuthSession.revokeToken(this.apiContext, token);
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

	getAuthSessions(userId: UserID): Promise<AuthSessionsResponse> {
		return AuthSession.getAuthSessions(this.apiContext, userId);
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
		const ticketPayload = await cache.get(`ip-auth-ticket:${ticket}`);
		if (!ticketPayload) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TICKET);
		}
		return {completed: false};
	}

	async getWebAuthnAuthenticationOptions() {
		return AuthMfa.generateWebAuthnAuthenticationOptionsDiscoverable(this.apiContext);
	}

	async authenticateWebAuthnDiscoverable({data, request}: AuthWebAuthnAuthenticateRequest) {
		const user = await AuthMfa.verifyWebAuthnAuthenticationDiscoverable(this.apiContext, data.response, data.challenge);
		const [token] = await AuthSession.createAuthSession(this.apiContext, {user, request});
		return {token, user_id: user.id.toString(), user: mapUserToPartialResponse(user)};
	}

	async getWebAuthnMfaOptions({ticket}: MfaTicketRequest) {
		return AuthMfa.generateWebAuthnAuthenticationOptionsForMfa(this.apiContext, ticket);
	}

	async loginMfaWebAuthn({data, request}: AuthWebAuthnMfaRequest): Promise<AuthTokenWithUserIdResponse> {
		const result = await AuthLogin.loginMfaWebAuthn(this.apiContext, this.loginDependencies, {
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

	async initiateHandoff({
		userAgent,
		clientIp,
		clientPlatform,
	}: AuthHandoffInitiateRequest): Promise<HandoffInitiateResponse> {
		const result = await this.desktopHandoffService.initiateHandoff({userAgent, clientIp, clientPlatform});
		return {
			code: result.code,
			expires_at: result.expiresAt.toISOString(),
		};
	}

	async getHandoffInfo({code, clientIp}: AuthHandoffInfoRequest): Promise<HandoffInfoResponse> {
		const info = await this.desktopHandoffService.getHandoffInfo(code, clientIp);
		if (info.status === 'expired' || !info.clientIp) {
			return {status: info.status, client_info: null};
		}
		const geo = await lookupGeoip(info.clientIp);
		const {clientOs, clientPlatform} = resolveSessionClientInfo({
			userAgent: info.userAgent ?? null,
			isDesktopClient: info.clientPlatform === 'desktop',
		});
		return {
			status: 'pending',
			client_info: {
				platform: clientPlatform,
				os: clientOs,
				location: {
					city: geo.city,
					region: geo.region,
					country: geo.countryName,
				},
			},
		};
	}

	async completeHandoff({data, request, clientIp, authToken}: AuthHandoffCompleteRequest): Promise<void> {
		const sessionToken = data.token ?? authToken;
		if (!sessionToken) {
			throw new UnauthorizedError();
		}
		await this.desktopHandoffService.completeHandoff(
			data.code,
			() =>
				AuthSession.createAdditionalAuthSessionFromToken(this.apiContext, {
					token: sessionToken,
					expectedUserId: data.user_id,
					request,
				}),
			clientIp,
		);
	}

	async getHandoffStatus({code, clientIp}: AuthHandoffStatusRequest): Promise<HandoffStatusResponse> {
		const result = await this.desktopHandoffService.getHandoffStatus(code, clientIp);
		return {
			status: result.status,
			token: result.token,
			user_id: result.userId,
			user: result.userId ? await this.getUserPartial(result.userId) : undefined,
		};
	}

	async cancelHandoff({code}: {code: string}): Promise<void> {
		await this.desktopHandoffService.cancelHandoff(code);
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
		};
	}
}
