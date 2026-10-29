// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthEmail from '@app/api/auth/AuthEmail';
import * as AuthEmailRevert from '@app/api/auth/AuthEmailRevert';
import * as AuthIdentify from '@app/api/auth/AuthIdentify';
import * as AuthInstantAccounts from '@app/api/auth/AuthInstantAccounts';
import * as AuthLogin from '@app/api/auth/AuthLogin';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthRecoveryKit from '@app/api/auth/AuthRecoveryKit';
import * as AuthRegistration from '@app/api/auth/AuthRegistration';
import * as AuthSession from '@app/api/auth/AuthSession';
import {getTokenIdHash} from '@app/api/auth/AuthUtility';
import type {DesktopHandoffService} from '@app/api/auth/services/DesktopHandoffService';
import type {SsoService} from '@app/api/auth/services/SsoService';
import {createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {TierContext} from '@app/api/user/AccountTier';
import {
	classifyWebPushOrigin,
	encodePushSessionIdHash,
	recordPushSessionPredecessor,
} from '@app/api/user/services/WebPushOriginReplacement';
import {isUsernameTaken} from '@app/api/user/UniqueUsernames';
import {mapUserToPartialResponse} from '@app/api/user/UserMappers';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {generateUsernameSuggestions} from '@app/api/utils/UsernameSuggestionUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import type {
	AccountHandoffRedeemRequest,
	AccountHandoffRequest,
	AccountHandoffResponse,
	AuthLoginResponse,
	AuthorizeIpRequest,
	AuthRegisterResponse,
	AuthRegisterSuccessResponse,
	AuthTokenWithUserIdResponse,
	EmailCodeSendResponse,
	EmailCodeVerifyRequest,
	EmailCodeVerifyResponse,
	EmailRevertRequest,
	HandoffCompleteRequest,
	HandoffCompleteResponse,
	HandoffInfoResponse,
	HandoffInitiateResponse,
	HandoffStatusResponse,
	IdentifyResponse,
	IpAuthorizationPollResponse,
	LoginRequest,
	LogoutAuthSessionsRequest,
	RecoverAccountRequest,
	RecoverAccountResponse,
	RegisterRequest,
	ResetPasswordRequest,
	SsoCompleteRequest,
	SsoCompleteResponse,
	SsoStartRequest,
	UsernameAvailabilityResponse,
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
	tierContext: TierContext;
}

interface AuthLoginRequest {
	data: LoginRequest;
	request: Request;
	captchaVerified?: boolean;
}

interface AuthResetPasswordRequest {
	data: ResetPasswordRequest;
	request: Request;
}

interface AuthRecoverAccountRequest {
	data: RecoverAccountRequest;
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
	returnUri?: string | null;
}

interface AuthHandoffInfoRequest {
	code: string;
	clientIp: string;
}

interface AuthHandoffStatusRequest {
	code: string;
	clientIp: string;
	pollSecret?: string;
	grant?: string;
}

export class AuthRequestService {
	constructor(
		private apiContext: ApiContext,
		private ssoService: SsoService,
		private desktopHandoffService: DesktopHandoffService,
		private registrationDependencies: AuthRegistration.RegistrationDependencies,
		private loginDependencies: AuthLogin.LoginDependencies,
	) {}

	startSso(data: SsoStartRequest) {
		return this.ssoService.startLogin({
			redirectTo: data.redirect_to ?? undefined,
			redirectUri: data.redirect_uri ?? undefined,
		});
	}

	completeSso(data: SsoCompleteRequest, request: Request, requestCache: RequestCache, clientIp: string) {
		return this.toSsoCompleteResponse(
			this.ssoService.completeLogin({
				code: data.code,
				state: data.state,
				clientBinding: data.client_binding ?? undefined,
				request,
				requestCache,
				clientIp,
			}),
		);
	}

	async register({data, request, requestCache, tierContext}: AuthRegisterRequest): Promise<AuthRegisterResponse> {
		const result = await AuthRegistration.register(this.apiContext, this.registrationDependencies, {
			data,
			request,
			requestCache,
			tierContext,
		});
		return toAuthRegisterResponse(result);
	}

	async login({data, request, captchaVerified}: AuthLoginRequest): Promise<AuthLoginResponse> {
		const result = await AuthLogin.login(this.apiContext, this.loginDependencies, {data, request, captchaVerified});
		return await this.toAuthLoginResponse(result);
	}

	async loginMfaTotp({code, ticket, request}: AuthLoginMfaRequest): Promise<AuthTokenWithUserIdResponse> {
		const result = await AuthLogin.loginMfaTotp(this.apiContext, {code, ticket, request, ssoLinks: this.ssoService});
		return await this.toAuthTokenResponse(result);
	}

	async verifyEmail(data: VerifyEmailRequest): Promise<void> {
		const success = await AuthEmail.verifyEmail(this.apiContext, data);
		if (!success) {
			throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_VERIFICATION_TOKEN);
		}
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

	async recoverAccount({data, request}: AuthRecoverAccountRequest): Promise<RecoverAccountResponse> {
		const {result, recoveryKey, createdAt} = await AuthRecoveryKit.recoverAccount(this.apiContext, {data, request});
		return {
			...(await this.toAuthLoginResponse(result)),
			recovery_key: recoveryKey,
			recovery_kit_created_at: createdAt.toISOString(),
		};
	}

	async revertEmailChange({data, request}: AuthRevertEmailChangeRequest): Promise<AuthTokenWithUserIdResponse> {
		const result = await AuthEmailRevert.revertEmailChange(this.apiContext, {
			token: data.token,
			password: data.password,
			request,
		});
		return await this.toAuthTokenResponse(result);
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

	async authenticateWebAuthnDiscoverable({data, request}: AuthWebAuthnAuthenticateRequest) {
		const user = await AuthMfa.verifyWebAuthnAuthenticationDiscoverable(this.apiContext, data.response, data.challenge);
		const [token] = await AuthLogin.createLoginSession(this.apiContext, user, request);
		return {token, user_id: user.id.toString(), user: mapUserToPartialResponse(user)};
	}

	async loginMfaWebAuthn({data, request}: AuthWebAuthnMfaRequest): Promise<AuthTokenWithUserIdResponse> {
		const result = await AuthLogin.loginMfaWebAuthn(this.apiContext, {
			response: data.response,
			challenge: data.challenge,
			ticket: data.ticket,
			request,
			ssoLinks: this.ssoService,
		});
		return await this.toAuthTokenResponse(result);
	}

	getUsernameSuggestions({globalName}: AuthUsernameSuggestionsRequest): UsernameSuggestionsResponse {
		return {suggestions: generateUsernameSuggestions(globalName)};
	}

	async getUsernameAvailability(username: string): Promise<UsernameAvailabilityResponse> {
		return {available: !(await isUsernameTaken(this.apiContext.services.users, username))};
	}

	identify(params: {
		identifier: string;
		deviceProof: string | undefined;
		registrationUrlCode: string | null | undefined;
		request: Request;
		clientIp: string;
		captcha: AuthIdentify.CaptchaGate;
	}): Promise<IdentifyResponse> {
		return AuthIdentify.identify(this.apiContext, params);
	}

	sendEmailCode(params: {
		ticket: string;
		registrationUrlCode: string | null | undefined;
		request: Request;
		clientIp: string;
		captcha: AuthIdentify.CaptchaGate;
	}): Promise<EmailCodeSendResponse> {
		return AuthIdentify.sendEmailCode(this.apiContext, params);
	}

	async verifyEmailCode(params: {
		data: EmailCodeVerifyRequest;
		request: Request;
		clientIp: string;
		requestCache: RequestCache;
		tierContext: TierContext;
		captcha: AuthIdentify.CaptchaGate;
	}): Promise<EmailCodeVerifyResponse> {
		const {data} = params;
		const result = await AuthIdentify.verifyEmailCode(this.apiContext, this.registrationDependencies, {
			ticket: data.ticket,
			code: data.code,
			consent: data.consent,
			registrationUrlCode: data.registration_url_code,
			inviteCode: data.invite_code,
			request: params.request,
			clientIp: params.clientIp,
			requestCache: params.requestCache,
			tierContext: params.tierContext,
			captcha: params.captcha,
		});
		if (result.kind === 'sign_up') return toAuthRegisterResponse(result.result);
		if (result.kind === 'mfa') return result.mfa;
		return {
			token: result.token,
			user_id: result.user.id.toString(),
			user: mapUserToPartialResponse(result.user),
			device_proof: result.deviceProof,
		};
	}

	instantAccountChallengeSteps(
		request: Request,
		clientIp: string,
		registrationUrlCode: string | null = null,
	): Promise<number> {
		return AuthInstantAccounts.instantAccountChallengeSteps(this.apiContext, request, clientIp, registrationUrlCode);
	}

	createAccountHandoff(params: {data: AccountHandoffRequest; clientIp: string}): Promise<AccountHandoffResponse> {
		return AuthInstantAccounts.createAccountHandoff(this.apiContext, {
			globalName: params.data.global_name,
			nonceHash: params.data.nonce_hash,
			locale: params.data.locale,
			registrationUrlCode: params.data.registration_url_code,
			clientIp: params.clientIp,
		});
	}

	async redeemHandoff(params: {
		data: AccountHandoffRedeemRequest;
		request: Request;
		requestCache: RequestCache;
		tierContext: TierContext;
	}): Promise<AuthRegisterSuccessResponse> {
		const {user, token, deviceProof, entry} = await AuthInstantAccounts.redeemHandoff(
			this.apiContext,
			this.registrationDependencies,
			{
				handoffCode: params.data.handoff_code,
				nonce: params.data.nonce,
				consent: params.data.consent,
				globalName: params.data.global_name,
				request: params.request,
				requestCache: params.requestCache,
				tierContext: params.tierContext,
			},
		);
		return {
			token,
			user_id: user.id.toString(),
			user: mapUserToPartialResponse(user),
			device_proof: deviceProof,
			entry,
		};
	}

	async initiateHandoff({request, returnUri}: AuthHandoffInitiateRequest): Promise<HandoffInitiateResponse> {
		const origin = AuthSession.resolveSessionOrigin(this.apiContext, request);
		const result = await this.desktopHandoffService.initiateHandoff({
			origin,
			initiatorOrigin: request.headers.get('origin'),
			returnUri,
		});
		return {
			code: result.code,
			expires_at: result.expiresAt.toISOString(),
			poll_secret: result.pollSecret,
			return_method: result.returnMethod,
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
			return_method: info.returnMethod,
		};
	}

	async completeHandoff({
		data,
		clientIp,
		authToken,
		approverOrigin,
	}: AuthHandoffCompleteRequest): Promise<HandoffCompleteResponse | null> {
		const sessionToken = data.token ?? authToken;
		if (!sessionToken) {
			throw new UnauthorizedError();
		}
		let createdToken: string | null = null;
		const {initiatorOrigin, returnUrl} = await this.desktopHandoffService.completeHandoff(
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
			data.return_method,
		);
		if (createdToken !== null) {
			await this.recordPushSessionPredecessor(createdToken, sessionToken, initiatorOrigin, approverOrigin);
		}
		return returnUrl ? {return_url: returnUrl} : null;
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

	async getHandoffStatus({
		code,
		clientIp,
		pollSecret,
		grant,
	}: AuthHandoffStatusRequest): Promise<HandoffStatusResponse> {
		const result = await this.desktopHandoffService.getHandoffStatus(code, clientIp, pollSecret, grant);
		return {
			status: result.status,
			token: result.token,
			user_id: result.userId,
			user: result.userId ? await this.getUserPartial(result.userId) : undefined,
		};
	}

	private async getUserPartial(userId: string): Promise<UserPartialResponse> {
		const user = await this.apiContext.services.users.findUniqueAssert(createUserID(BigInt(userId)));
		return mapUserToPartialResponse(user);
	}

	async toAuthTokenResponse(result: {user_id: string; token: string}): Promise<AuthTokenWithUserIdResponse> {
		return {
			...result,
			user: await this.getUserPartial(result.user_id),
		};
	}

	private async toSsoCompleteResponse(
		resultPromise: ReturnType<SsoService['completeLogin']>,
	): Promise<SsoCompleteResponse> {
		const {redirect_to, ...result} = await resultPromise;
		if ('linked' in result) {
			return {linked: true, redirect_to};
		}
		return {...(await this.toAuthLoginResponse(result)), redirect_to};
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

function toAuthRegisterResponse(result: AuthRegistration.AccountCreationResult): AuthRegisterResponse {
	if (result.pendingApproval) {
		return {registration_pending_approval: true, user_id: result.user.id.toString()};
	}
	return {
		token: result.token,
		user_id: result.user.id.toString(),
		user: mapUserToPartialResponse(result.user),
		device_proof: result.deviceProof,
		entry: result.entry,
	};
}
