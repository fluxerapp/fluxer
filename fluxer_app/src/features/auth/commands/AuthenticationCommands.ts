// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import AccountManager from '@app/features/auth/state/AccountManager';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import Authentication from '@app/features/auth/state/Authentication';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import UserSettings from '@app/features/user/state/UserSettings';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {
	AuthRegistrationPendingApprovalResponse,
	RegisterRequest,
	SsoCompleteResponse,
	SsoStartRequest,
	SsoStartResponse,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';

const logger = new Logger('AuthService');
const getPlatformHeaderValue = (): 'web' | 'desktop' | 'mobile' => (isDesktop() ? 'desktop' : 'web');
const withPlatformHeader = (headers?: Record<string, string>): Record<string, string> => ({
	'X-Fluxer-Platform': getPlatformHeaderValue(),
	'Accept-Language': UserSettings.getLocale(),
	...(headers ?? {}),
});
export const VerificationResult = {
	SUCCESS: 'SUCCESS',
	EXPIRED_TOKEN: 'EXPIRED_TOKEN',
	RATE_LIMITED: 'RATE_LIMITED',
	SERVER_ERROR: 'SERVER_ERROR',
} as const;

export type VerificationResult = ValueOf<typeof VerificationResult>;

type CaptchaType = 'turnstile' | 'hcaptcha';

type RegisterData = RegisterRequest & {
	captchaToken?: string;
	captchaType?: CaptchaType;
};

export type AuthResponseUser = UserPartial & {
	email?: string | null;
};

export interface AuthTokenResponse {
	user_id: string;
	token: string;
	theme?: string;
	redirect_to?: string;
	user?: AuthResponseUser | null;
}

interface StandardLoginResponse extends AuthTokenResponse {
	mfa?: false;
}

interface MfaLoginResponse {
	mfa: true;
	ticket: string;
	totp: boolean;
	webauthn: boolean;
	allowed_methods?: Array<string>;
}

type LoginResponse = StandardLoginResponse | MfaLoginResponse;

export interface IpAuthorizationRequiredResponse {
	ip_authorization_required: true;
	ticket: string;
	email: string;
	resend_available_in: number;
}

export function authResponseUserToUserData(user?: AuthResponseUser | null): UserData | undefined {
	if (!user) {
		return undefined;
	}
	const userData: UserData = {
		username: user.username,
		discriminator: user.discriminator,
		globalName: user.global_name,
		avatar: user.avatar,
	};
	if (user.email !== undefined) {
		userData.email = user.email;
	}
	return userData;
}

export type TokenResponse = AuthTokenResponse;
export type RegistrationPendingApprovalResponse = AuthRegistrationPendingApprovalResponse;
export type RegisterResponse = TokenResponse | RegistrationPendingApprovalResponse;

export function isIpAuthorizationRequiredResponse(
	response: LoginResponse | IpAuthorizationRequiredResponse,
): response is IpAuthorizationRequiredResponse {
	return (response as IpAuthorizationRequiredResponse).ip_authorization_required === true;
}

export function isRegistrationPendingApprovalResponse(
	response: RegisterResponse,
): response is RegistrationPendingApprovalResponse {
	return 'registration_pending_approval' in response && response.registration_pending_approval === true;
}

export type ResetPasswordResponse = AuthTokenResponse | MfaLoginResponse;

interface DesktopHandoffInitiateResponse {
	code: string;
	expires_at: string;
}

interface DesktopHandoffStatusResponse {
	status: 'pending' | 'completed' | 'expired';
	token?: string;
	user_id?: string;
	user?: AuthResponseUser | null;
}

interface DesktopHandoffInfoClientInfo {
	platform?: string | null;
	os?: string | null;
	location?: {
		city?: string | null;
		region?: string | null;
		country?: string | null;
	} | null;
}

export interface DesktopHandoffInfoResponse {
	status: 'pending' | 'expired';
	client_info?: DesktopHandoffInfoClientInfo | null;
}

interface LoginParams {
	email: string;
	password: string;
	captchaToken?: string;
	inviteCode?: string;
	captchaType?: CaptchaType;
}

interface CaptchaParams {
	captchaToken?: string;
	captchaType?: CaptchaType;
}

function captchaHeaders({captchaToken, captchaType}: CaptchaParams): Record<string, string> {
	if (!captchaToken) {
		return {};
	}
	return {
		'X-Captcha-Token': captchaToken,
		'X-Captcha-Type': captchaType || 'hcaptcha',
	};
}

function withInviteCode<T extends object>(body: T, inviteCode?: string): T & {invite_code?: string} {
	return inviteCode ? {...body, invite_code: inviteCode} : body;
}

function loginBody({email, password, inviteCode}: Pick<LoginParams, 'email' | 'password' | 'inviteCode'>): {
	email: string;
	password: string;
	invite_code?: string;
} {
	return withInviteCode({email, password}, inviteCode);
}

function mfaTotpBody(
	code: string,
	ticket: string,
	inviteCode?: string,
): {code: string; ticket: string; invite_code?: string} {
	return withInviteCode({code, ticket}, inviteCode);
}

function mfaWebAuthnBody(
	response: AuthenticationResponseJSON,
	challenge: string,
	ticket: string,
	inviteCode?: string,
): {response: AuthenticationResponseJSON; challenge: string; ticket: string; invite_code?: string} {
	return withInviteCode({response, challenge, ticket}, inviteCode);
}

function webAuthnBody(
	response: AuthenticationResponseJSON,
	challenge: string,
	inviteCode?: string,
): {response: AuthenticationResponseJSON; challenge: string; invite_code?: string} {
	return withInviteCode({response, challenge}, inviteCode);
}

function registerBody(data: RegisterData): RegisterRequest {
	const {captchaToken: _, captchaType: __, ...bodyData} = data;
	return bodyData;
}

function ticketBody(ticket: string): {ticket: string} {
	return {ticket};
}

function tokenBody(token: string): {token: string} {
	return {token};
}

function loginIpAuthorizationResponse(error: HttpError): IpAuthorizationRequiredResponse | null {
	if (error.status !== 403 || failureCode(error) !== APIErrorCodes.IP_AUTHORIZATION_REQUIRED) {
		return null;
	}
	const body = error.body as Record<string, unknown> | undefined;
	return {
		ip_authorization_required: true,
		ticket: body?.ticket as string,
		email: body?.email as string,
		resend_available_in: (body?.resend_available_in as number) ?? 30,
	};
}

function verificationResultFromError(
	error: unknown,
	invalidResult: VerificationResult,
	invalidStatus: number,
): VerificationResult {
	const responseErr = error as {
		status?: number;
	};
	return responseErr.status === invalidStatus ? invalidResult : VerificationResult.SERVER_ERROR;
}

export async function login({
	email,
	password,
	captchaToken,
	inviteCode,
	captchaType,
}: LoginParams): Promise<LoginResponse | IpAuthorizationRequiredResponse> {
	try {
		const response = await http.post<LoginResponse>(Endpoints.AUTH_LOGIN, {
			body: loginBody({email, password, inviteCode}),
			headers: withPlatformHeader(captchaHeaders({captchaToken, captchaType})),
		});
		logger.debug('Login successful', {mfa: response.body?.mfa});
		return response.body;
	} catch (error) {
		if (error instanceof HttpError) {
			const ipAuthorization = loginIpAuthorizationResponse(error);
			if (ipAuthorization) {
				logger.info('Login requires IP authorization', {email});
				return ipAuthorization;
			}
		}
		logger.error('Login failed', error);
		throw error;
	}
}

export async function loginMfaTotp(code: string, ticket: string, inviteCode?: string): Promise<TokenResponse> {
	try {
		const response = await http.post<TokenResponse>(Endpoints.AUTH_LOGIN_MFA_TOTP, {
			body: mfaTotpBody(code, ticket, inviteCode),
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('MFA TOTP authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA TOTP authentication failed', error);
		throw error;
	}
}

export async function loginMfaWebAuthn(
	response: AuthenticationResponseJSON,
	challenge: string,
	ticket: string,
	inviteCode?: string,
): Promise<TokenResponse> {
	try {
		const httpResponse = await http.post<TokenResponse>(Endpoints.AUTH_LOGIN_MFA_WEBAUTHN, {
			body: mfaWebAuthnBody(response, challenge, ticket, inviteCode),
			headers: withPlatformHeader(),
		});
		const responseBody = httpResponse.body;
		logger.debug('MFA WebAuthn authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA WebAuthn authentication failed', error);
		throw error;
	}
}

export async function getWebAuthnMfaOptions(ticket: string): Promise<PublicKeyCredentialRequestOptionsJSON> {
	try {
		const response = await http.post<PublicKeyCredentialRequestOptionsJSON>(Endpoints.AUTH_LOGIN_MFA_WEBAUTHN_OPTIONS, {
			body: ticketBody(ticket),
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('WebAuthn MFA options retrieved');
		return responseBody;
	} catch (error) {
		logger.error('Failed to get WebAuthn MFA options', error);
		throw error;
	}
}

export async function getWebAuthnAuthenticationOptions(): Promise<PublicKeyCredentialRequestOptionsJSON> {
	try {
		const response = await http.post<PublicKeyCredentialRequestOptionsJSON>(Endpoints.AUTH_WEBAUTHN_OPTIONS, {
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('WebAuthn authentication options retrieved');
		return responseBody;
	} catch (error) {
		logger.error('Failed to get WebAuthn authentication options', error);
		throw error;
	}
}

export async function authenticateWithWebAuthn(
	response: AuthenticationResponseJSON,
	challenge: string,
	inviteCode?: string,
): Promise<TokenResponse> {
	try {
		const httpResponse = await http.post<TokenResponse>(Endpoints.AUTH_WEBAUTHN_AUTHENTICATE, {
			body: webAuthnBody(response, challenge, inviteCode),
			headers: withPlatformHeader(),
		});
		const responseBody = httpResponse.body;
		logger.debug('WebAuthn authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('WebAuthn authentication failed', error);
		throw error;
	}
}

export async function register(data: RegisterData): Promise<RegisterResponse> {
	try {
		const response = await http.post<RegisterResponse>(Endpoints.AUTH_REGISTER, {
			body: registerBody(data),
			headers: withPlatformHeader(captchaHeaders(data)),
		});
		const responseBody = response.body;
		logger.info('Registration successful');
		return responseBody;
	} catch (error) {
		logger.error('Registration failed', error);
		throw error;
	}
}

interface UsernameSuggestionsResponse {
	suggestions: Array<string>;
}

export async function getUsernameSuggestions(globalName: string): Promise<Array<string>> {
	try {
		const response = await http.post<UsernameSuggestionsResponse>(Endpoints.AUTH_USERNAME_SUGGESTIONS, {
			body: {global_name: globalName},
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('Username suggestions retrieved', {count: responseBody?.suggestions?.length || 0});
		return responseBody?.suggestions ?? [];
	} catch (error) {
		logger.error('Failed to fetch username suggestions', error);
		throw error;
	}
}

export async function forgotPassword(
	email: string,
	captchaToken?: string,
	captchaType?: 'turnstile' | 'hcaptcha',
): Promise<void> {
	try {
		await http.post(Endpoints.AUTH_FORGOT_PASSWORD, {
			body: {email},
			headers: withPlatformHeader(captchaHeaders({captchaToken, captchaType})),
		});
		logger.debug('Password reset email sent');
	} catch (error) {
		logger.warn('Password reset request failed, but returning success to user', error);
	}
}

export async function validateResetPasswordToken(token: string): Promise<boolean> {
	try {
		const response = await http.get<{
			valid: boolean;
		}>(Endpoints.AUTH_VALIDATE_RESET_PASSWORD_TOKEN(token), {
			headers: withPlatformHeader(),
		});
		return response.body.valid;
	} catch (error) {
		logger.error('Password reset token validation failed', error);
		throw error;
	}
}

export async function resetPassword(token: string, password: string): Promise<ResetPasswordResponse> {
	try {
		const response = await http.post<ResetPasswordResponse>(Endpoints.AUTH_RESET_PASSWORD, {
			body: {token, password},
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.info('Password reset successful');
		return responseBody;
	} catch (error) {
		logger.error('Password reset failed', error);
		throw error;
	}
}

export async function revertEmailChange(token: string, password: string): Promise<TokenResponse> {
	try {
		const response = await http.post<TokenResponse>(Endpoints.AUTH_EMAIL_REVERT, {
			body: {token, password},
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.info('Email revert successful');
		return responseBody;
	} catch (error) {
		logger.error('Email revert failed', error);
		throw error;
	}
}

export async function verifyEmail(token: string): Promise<VerificationResult> {
	try {
		await http.post(Endpoints.AUTH_VERIFY_EMAIL, {
			body: tokenBody(token),
			headers: withPlatformHeader(),
		});
		logger.info('Email verification successful');
		return VerificationResult.SUCCESS;
	} catch (error) {
		const result = verificationResultFromError(error, VerificationResult.EXPIRED_TOKEN, 400);
		if (result === VerificationResult.EXPIRED_TOKEN) {
			logger.warn('Email verification failed - expired or invalid token');
			return result;
		}
		logger.error('Email verification failed - server error', error);
		return result;
	}
}

export async function resendVerificationEmail(): Promise<VerificationResult> {
	try {
		await http.post(Endpoints.AUTH_RESEND_VERIFICATION, {
			headers: withPlatformHeader(),
		});
		logger.info('Verification email resent');
		return VerificationResult.SUCCESS;
	} catch (error) {
		const result = verificationResultFromError(error, VerificationResult.RATE_LIMITED, 429);
		if (result === VerificationResult.RATE_LIMITED) {
			logger.warn('Rate limited when resending verification email');
			return result;
		}
		logger.error('Failed to resend verification email - server error', error);
		return result;
	}
}

export async function logout(): Promise<void> {
	await AccountManager.logout();
}

export async function authorizeIp(token: string): Promise<VerificationResult> {
	try {
		await http.post(Endpoints.AUTH_AUTHORIZE_IP, {
			body: tokenBody(token),
			headers: withPlatformHeader(),
		});
		logger.info('IP authorization successful');
		return VerificationResult.SUCCESS;
	} catch (error) {
		const result = verificationResultFromError(error, VerificationResult.EXPIRED_TOKEN, 400);
		if (result === VerificationResult.EXPIRED_TOKEN) {
			logger.warn('IP authorization failed - expired or invalid token');
			return result;
		}
		logger.error('IP authorization failed - server error', error);
		return result;
	}
}

export async function resendIpAuthorization(ticket: string): Promise<void> {
	await http.post(Endpoints.AUTH_IP_AUTHORIZATION_RESEND, {
		body: ticketBody(ticket),
		headers: withPlatformHeader(),
	});
}

export interface IpAuthorizationPollResult {
	completed: boolean;
	token?: string;
	user_id?: string;
	user?: AuthResponseUser | null;
}

export async function pollIpAuthorization(ticket: string): Promise<IpAuthorizationPollResult> {
	const response = await http.get<IpAuthorizationPollResult>(Endpoints.AUTH_IP_AUTHORIZATION_POLL(ticket), {
		headers: withPlatformHeader(),
	});
	return response.body;
}

export async function initiateDesktopHandoff(): Promise<DesktopHandoffInitiateResponse> {
	const response = await http.post<DesktopHandoffInitiateResponse>(Endpoints.AUTH_HANDOFF_INITIATE, {
		auth: 'none',
	});
	return response.body;
}

export async function pollDesktopHandoffStatus(code: string): Promise<DesktopHandoffStatusResponse> {
	const response = await http.get<DesktopHandoffStatusResponse>(Endpoints.AUTH_HANDOFF_STATUS(code), {
		auth: 'none',
	});
	return response.body;
}

export async function completeDesktopHandoff({
	code,
	token,
	userId,
}: {
	code: string;
	token: string;
	userId: string;
}): Promise<void> {
	await http.post(Endpoints.AUTH_HANDOFF_COMPLETE, {
		body: {code, user_id: userId},
		headers: withPlatformHeader({Authorization: token}),
		auth: 'none',
	});
}

export async function fetchDesktopHandoffInfo(code: string): Promise<DesktopHandoffInfoResponse> {
	const response = await http.get<DesktopHandoffInfoResponse>(Endpoints.AUTH_HANDOFF_INFO(code), {
		auth: 'none',
	});
	return response.body;
}

export function startSession(
	token: string,
	options: {
		startGateway?: boolean;
	} = {},
): void {
	const {startGateway = true} = options;
	logger.info('Starting new session');
	Authentication.handleSessionStart({token});
	if (!startGateway) {
		return;
	}
	GatewayConnection.startSession(token);
}

let sessionStartInProgress = false;

export async function ensureSessionStarted(): Promise<void> {
	if (sessionStartInProgress) {
		return;
	}
	if (AccountManager.isSwitching) {
		return;
	}
	if (!Authentication.isAuthenticated) {
		return;
	}
	if (GatewayConnection.isConnected || GatewayConnection.isConnecting) {
		return;
	}
	if (GatewayConnection.socket) {
		return;
	}
	sessionStartInProgress = true;
	try {
		logger.info('Ensuring session is started');
		const token = Authentication.authToken;
		if (token) {
			GatewayConnection.startSession(token);
		}
	} finally {
		setTimeout(() => {
			sessionStartInProgress = false;
		}, 100);
	}
}

export interface CompleteLoginOptions {
	redirectPath?: string | null;
}

export async function completeLogin(
	{
		token,
		userId,
		userData,
	}: {
		token: string;
		userId: string;
		userData?: UserData;
	},
	options: CompleteLoginOptions = {},
): Promise<void> {
	logger.info('Completing login process');
	if (userId && token) {
		await AccountManager.switchToNewAccount(userId, token, userData, options.redirectPath);
	} else {
		startSession(token, {startGateway: true});
	}
}

export async function startSso({
	redirectTo,
	redirectUri,
}: {
	redirectTo?: string;
	redirectUri?: string;
} = {}): Promise<SsoStartResponse> {
	const body: SsoStartRequest = {
		redirect_to: redirectTo,
		redirect_uri: redirectUri,
	};
	const response = await http.post<SsoStartResponse>(Endpoints.AUTH_SSO_START, {
		body,
		headers: withPlatformHeader(),
	});
	return response.body;
}

export async function completeSso({code, state}: {code: string; state: string}): Promise<SsoCompleteResponse> {
	const response = await http.post<SsoCompleteResponse>(Endpoints.AUTH_SSO_COMPLETE, {
		body: {code, state},
		headers: withPlatformHeader(),
	});
	return response.body;
}

interface SetMfaTicketPayload {
	ticket: string;
	totp: boolean;
	webauthn: boolean;
}

export function setMfaTicket({ticket, totp, webauthn}: SetMfaTicketPayload): void {
	logger.debug('Setting MFA ticket');
	Authentication.handleMfaTicketSet({ticket, totp, webauthn});
}

export function clearMfaTicket(): void {
	logger.debug('Clearing MFA ticket');
	Authentication.handleMfaTicketClear();
}
