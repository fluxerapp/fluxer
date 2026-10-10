// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import InstanceCapabilities, {InstanceResponseCapability} from '@app/features/app/state/InstanceCapabilities';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import Accounts from '@app/features/auth/state/Accounts';
import Authentication from '@app/features/auth/state/Authentication';
import type {AuthRequestTarget} from '@app/features/auth/state/AuthRequestTarget';
import {type InstanceHTTPTarget, instanceRequest} from '@app/features/platform/transport/InstanceHTTP';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode, ipAuthorizationRequiredResponseFromError} from '@app/features/platform/utils/ResponseInspection';
import UserSettings from '@app/features/user/state/UserSettings';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import type {
	AuthRegistrationPendingApprovalResponse,
	DesktopHandoffReturnMethod,
	RegisterRequest,
	SsoCompleteResponse,
	SsoStartRequest,
	SsoStartResponse,
	UsernameAvailabilityResponse,
	UsernameSuggestionsResponse,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';

const logger = new Logger('AuthService');
const withAuthLocaleHeader = (headers?: Record<string, string>): Record<string, string> => ({
	'Accept-Language': UserSettings.getLocale(),
	...(headers ?? {}),
});

function observeInstanceResponse(
	target: InstanceHTTPTarget,
	capability: InstanceResponseCapability,
	supported: boolean,
): void {
	InstanceCapabilities.observeResponse({instanceKey: target.instanceKey, capability, supported});
}

function isRouteUnimplemented(error: unknown): boolean {
	return error instanceof HttpError && (error.status === 404 || error.status === 405);
}

export const VerificationResult = {
	SUCCESS: 'SUCCESS',
	EXPIRED_TOKEN: 'EXPIRED_TOKEN',
	RATE_LIMITED: 'RATE_LIMITED',
	SERVER_ERROR: 'SERVER_ERROR',
} as const;

export type VerificationResult = ValueOf<typeof VerificationResult>;

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

export interface MfaLoginResponse {
	mfa: true;
	ticket: string;
	totp: boolean;
	webauthn: boolean;
	backup_codes?: boolean;
	allowed_methods?: Array<string>;
}

type LoginResponse = StandardLoginResponse | MfaLoginResponse;

export interface IpAuthorizationRequiredResponse {
	ip_authorization_required: true;
	ticket: string;
	email: string;
	resend_available_in: number;
}

export type AuthResponseUserIdentity = Pick<
	AuthResponseUser,
	'username' | 'discriminator' | 'global_name' | 'avatar' | 'email'
>;

export function authResponseUserToUserData(user?: AuthResponseUserIdentity | null): UserData | undefined {
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

interface RecoveryKitIssued {
	recovery_key: string;
	recovery_kit_created_at: string;
}

export type RecoverAccountResponse = (AuthTokenResponse | MfaLoginResponse) & RecoveryKitIssued;

interface DesktopHandoffInitiateResponse {
	code: string;
	expires_at: string;
	poll_secret?: string;
	return_method?: DesktopHandoffReturnMethod;
}

interface DesktopHandoffStatusResponse {
	status: 'pending' | 'completed' | 'denied' | 'expired';
	token?: string;
	user_id?: string;
	user?: AuthResponseUser | null;
}

interface DesktopHandoffInfoClientInfo {
	platform?: string | null;
	os?: string | null;
	device?: 'mobile' | 'desktop';
	location?: {
		city?: string | null;
		region?: string | null;
		country?: string | null;
	} | null;
}

export interface DesktopHandoffInfoResponse {
	status: 'pending' | 'expired';
	client_info?: DesktopHandoffInfoClientInfo | null;
	return_method?: DesktopHandoffReturnMethod;
}

interface DesktopHandoffCompleteResponse {
	return_url?: string;
}

export type LoginIdentifier = {email: string; login?: undefined} | {login: string; email?: undefined};

type LoginParams = LoginIdentifier & {
	password: string;
	inviteCode?: string;
	target: AuthRequestTarget;
};

interface MfaCodeLoginRequest {
	code: string;
	ticket: string;
	inviteCode?: string;
	target: InstanceHTTPTarget;
}

interface MfaWebAuthnLoginRequest {
	response: AuthenticationResponseJSON;
	challenge: string;
	ticket: string;
	inviteCode?: string;
	target: InstanceHTTPTarget;
}

interface MfaWebAuthnOptionsRequest {
	ticket: string;
	target: InstanceHTTPTarget;
}

interface WebAuthnOptionsRequest {
	target: InstanceHTTPTarget;
}

interface WebAuthnLoginRequest {
	response: AuthenticationResponseJSON;
	challenge: string;
	inviteCode?: string;
	target: InstanceHTTPTarget;
}

interface IpAuthorizationTicketRequest {
	ticket: string;
	target: InstanceHTTPTarget;
}

function withInviteCode<T extends object>(body: T, inviteCode?: string): T & {invite_code?: string} {
	return inviteCode ? {...body, invite_code: inviteCode} : body;
}

function loginBody(params: LoginParams): {
	email?: string;
	login?: string;
	password: string;
	invite_code?: string;
} {
	if (params.login !== undefined) {
		return withInviteCode({login: params.login, password: params.password}, params.inviteCode);
	}
	return withInviteCode({email: params.email, password: params.password}, params.inviteCode);
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

function ticketBody(ticket: string): {ticket: string} {
	return {ticket};
}

function tokenBody(token: string): {token: string} {
	return {token};
}

class MalformedIpAuthorizationChallengeError extends HttpError {
	constructor(error: HttpError) {
		super({
			method: error.method,
			path: error.path,
			status: error.status,
			body: error.body,
			responseHeaders: error.responseHeaders,
		});
		this.name = 'MalformedIpAuthorizationChallengeError';
	}
}

function loginIpAuthorizationResponse(error: HttpError): IpAuthorizationRequiredResponse | null {
	if (error.status !== 403 || failureCode(error) !== APIErrorCodes.IP_AUTHORIZATION_REQUIRED) {
		return null;
	}
	const challenge = ipAuthorizationRequiredResponseFromError(error);
	if (challenge === null) {
		throw new MalformedIpAuthorizationChallengeError(error);
	}
	return challenge;
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

export async function login(params: LoginParams): Promise<LoginResponse | IpAuthorizationRequiredResponse> {
	try {
		const response = await instanceRequest<LoginResponse>({
			method: 'POST',
			path: Endpoints.AUTH_LOGIN,
			target: params.target.http,
			body: loginBody(params),
			headers: withAuthLocaleHeader(),
		});
		logger.debug('Login successful', {mfa: response.body?.mfa});
		return response.body;
	} catch (error) {
		if (error instanceof HttpError) {
			const ipAuthorization = loginIpAuthorizationResponse(error);
			if (ipAuthorization) {
				logger.info('Login requires IP authorization', {email: params.email});
				return ipAuthorization;
			}
		}
		logger.error('Login failed', error);
		throw error;
	}
}

export async function loginMfaTotp({code, ticket, inviteCode, target}: MfaCodeLoginRequest): Promise<TokenResponse> {
	try {
		const response = await instanceRequest<TokenResponse>({
			method: 'POST',
			path: Endpoints.AUTH_LOGIN_MFA_TOTP,
			target,
			body: mfaTotpBody(code, ticket, inviteCode),
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.debug('MFA TOTP authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA TOTP authentication failed', error);
		throw error;
	}
}

export async function loginMfaWebAuthn({
	response,
	challenge,
	ticket,
	inviteCode,
	target,
}: MfaWebAuthnLoginRequest): Promise<TokenResponse> {
	try {
		const httpResponse = await instanceRequest<TokenResponse>({
			method: 'POST',
			path: Endpoints.AUTH_LOGIN_MFA_WEBAUTHN,
			target,
			body: mfaWebAuthnBody(response, challenge, ticket, inviteCode),
			headers: withAuthLocaleHeader(),
		});
		const responseBody = httpResponse.body;
		logger.debug('MFA WebAuthn authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA WebAuthn authentication failed', error);
		throw error;
	}
}

export async function getWebAuthnMfaOptions({
	ticket,
	target,
}: MfaWebAuthnOptionsRequest): Promise<PublicKeyCredentialRequestOptionsJSON> {
	try {
		const response = await instanceRequest<PublicKeyCredentialRequestOptionsJSON>({
			method: 'POST',
			path: Endpoints.AUTH_LOGIN_MFA_WEBAUTHN_OPTIONS,
			target,
			body: ticketBody(ticket),
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.debug('WebAuthn MFA options retrieved');
		return responseBody;
	} catch (error) {
		logger.error('Failed to get WebAuthn MFA options', error);
		throw error;
	}
}

export async function getWebAuthnAuthenticationOptions({
	target,
}: WebAuthnOptionsRequest): Promise<PublicKeyCredentialRequestOptionsJSON> {
	try {
		const response = await instanceRequest<PublicKeyCredentialRequestOptionsJSON>({
			method: 'POST',
			path: Endpoints.AUTH_WEBAUTHN_OPTIONS,
			target,
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.debug('WebAuthn authentication options retrieved');
		return responseBody;
	} catch (error) {
		logger.error('Failed to get WebAuthn authentication options', error);
		throw error;
	}
}

export async function authenticateWithWebAuthn({
	response,
	challenge,
	inviteCode,
	target,
}: WebAuthnLoginRequest): Promise<TokenResponse> {
	try {
		const httpResponse = await instanceRequest<TokenResponse>({
			method: 'POST',
			path: Endpoints.AUTH_WEBAUTHN_AUTHENTICATE,
			target,
			body: webAuthnBody(response, challenge, inviteCode),
			headers: withAuthLocaleHeader(),
		});
		const responseBody = httpResponse.body;
		logger.debug('WebAuthn authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('WebAuthn authentication failed', error);
		throw error;
	}
}

export async function register(data: RegisterRequest, target: AuthRequestTarget): Promise<RegisterResponse> {
	try {
		const response = await instanceRequest<RegisterResponse>({
			method: 'POST',
			path: Endpoints.AUTH_REGISTER,
			target: target.http,
			body: data,
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.info('Registration successful');
		return responseBody;
	} catch (error) {
		logger.error('Registration failed', error);
		throw error;
	}
}

export async function getUsernameSuggestions(globalName: string, target: InstanceHTTPTarget): Promise<Array<string>> {
	try {
		const response = await instanceRequest<UsernameSuggestionsResponse>({
			method: 'POST',
			path: Endpoints.AUTH_USERNAME_SUGGESTIONS,
			target,
			body: {global_name: globalName},
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.debug('Username suggestions retrieved', {count: responseBody?.suggestions?.length || 0});
		return responseBody?.suggestions ?? [];
	} catch (error) {
		logger.error('Failed to fetch username suggestions', error);
		throw error;
	}
}

export async function checkUsernameAvailability(
	username: string,
	target: InstanceHTTPTarget,
	signal?: AbortSignal,
): Promise<boolean> {
	const response = await instanceRequest<UsernameAvailabilityResponse>({
		method: 'GET',
		path: `${Endpoints.AUTH_USERNAME_AVAILABILITY}?${new URLSearchParams({username}).toString()}`,
		target,
		headers: withAuthLocaleHeader(),
		signal,
	});
	return response.body.available;
}

export async function forgotPassword(email: string, target: AuthRequestTarget): Promise<void> {
	try {
		await instanceRequest({
			method: 'POST',
			path: Endpoints.AUTH_FORGOT_PASSWORD,
			target: target.http,
			body: {email},
			headers: withAuthLocaleHeader(),
		});
		logger.debug('Password reset email sent');
	} catch (error) {
		logger.error('Password reset request failed', error);
		throw error;
	}
}

export async function validateResetPasswordToken(token: string, target: AuthRequestTarget): Promise<boolean> {
	try {
		const response = await instanceRequest<{valid: boolean}>({
			method: 'GET',
			path: Endpoints.AUTH_VALIDATE_RESET_PASSWORD_TOKEN(token),
			target: target.http,
			headers: withAuthLocaleHeader(),
		});
		return response.body.valid;
	} catch (error) {
		logger.error('Password reset token validation failed', error);
		throw error;
	}
}

export async function resetPassword(
	token: string,
	password: string,
	target: AuthRequestTarget,
): Promise<ResetPasswordResponse> {
	try {
		const response = await instanceRequest<ResetPasswordResponse>({
			method: 'POST',
			path: Endpoints.AUTH_RESET_PASSWORD,
			target: target.http,
			body: {token, password},
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.info('Password reset successful');
		return responseBody;
	} catch (error) {
		logger.error('Password reset failed', error);
		throw error;
	}
}

export async function recoverAccount({
	login,
	recoveryKey,
	password,
	target,
}: {
	login: string;
	recoveryKey: string;
	password: string;
	target: AuthRequestTarget;
}): Promise<RecoverAccountResponse> {
	try {
		const response = await instanceRequest<RecoverAccountResponse>({
			method: 'POST',
			path: Endpoints.AUTH_RECOVER,
			target: target.http,
			body: {login, recovery_key: recoveryKey, password},
			headers: withAuthLocaleHeader(),
		});
		logger.info('Account recovery successful');
		return response.body;
	} catch (error) {
		logger.error('Account recovery failed', error);
		throw error;
	}
}

export async function revertEmailChange(
	token: string,
	password: string,
	target: AuthRequestTarget,
): Promise<TokenResponse> {
	try {
		const response = await instanceRequest<TokenResponse>({
			method: 'POST',
			path: Endpoints.AUTH_EMAIL_REVERT,
			target: target.http,
			body: {token, password},
			headers: withAuthLocaleHeader(),
		});
		const responseBody = response.body;
		logger.info('Email revert successful');
		return responseBody;
	} catch (error) {
		logger.error('Email revert failed', error);
		throw error;
	}
}

export async function verifyEmail(token: string, target: AuthRequestTarget): Promise<VerificationResult> {
	try {
		await instanceRequest({
			method: 'POST',
			path: Endpoints.AUTH_VERIFY_EMAIL,
			target: target.http,
			body: tokenBody(token),
			headers: withAuthLocaleHeader(),
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

export async function resendVerificationEmail(target: InstanceHTTPTarget): Promise<VerificationResult> {
	try {
		await instanceRequest({
			method: 'POST',
			path: Endpoints.AUTH_RESEND_VERIFICATION,
			target,
			headers: withAuthLocaleHeader(),
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
	await Accounts.logout();
}

export async function authorizeIp(token: string, target: AuthRequestTarget): Promise<VerificationResult> {
	try {
		await instanceRequest({
			method: 'POST',
			path: Endpoints.AUTH_AUTHORIZE_IP,
			target: target.http,
			body: tokenBody(token),
			headers: withAuthLocaleHeader(),
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

export async function resendIpAuthorization({ticket, target}: IpAuthorizationTicketRequest): Promise<void> {
	await instanceRequest({
		method: 'POST',
		path: Endpoints.AUTH_IP_AUTHORIZATION_RESEND,
		target,
		body: ticketBody(ticket),
		headers: withAuthLocaleHeader(),
	});
}

export interface IpAuthorizationPollResult {
	completed: boolean;
	token?: string;
	user_id?: string;
	user?: AuthResponseUser | null;
}

export async function pollIpAuthorization({
	ticket,
	target,
}: IpAuthorizationTicketRequest): Promise<IpAuthorizationPollResult> {
	const response = await instanceRequest<IpAuthorizationPollResult>({
		method: 'GET',
		path: Endpoints.AUTH_IP_AUTHORIZATION_POLL(ticket),
		target,
		headers: withAuthLocaleHeader(),
	});
	return response.body;
}

export async function initiateDesktopHandoff(target: InstanceHTTPTarget): Promise<DesktopHandoffInitiateResponse> {
	const response = await instanceRequest<DesktopHandoffInitiateResponse>({
		method: 'POST',
		path: Endpoints.AUTH_HANDOFF_INITIATE,
		target,
		auth: 'none',
	});
	observeInstanceResponse(target, InstanceResponseCapability.HANDOFF_POLL_SECRET, response.body.poll_secret != null);
	return response.body;
}

async function readDesktopHandoffStatus(
	code: string,
	target: InstanceHTTPTarget,
): Promise<DesktopHandoffStatusResponse> {
	const response = await instanceRequest<DesktopHandoffStatusResponse>({
		method: 'GET',
		path: Endpoints.AUTH_HANDOFF_STATUS(code),
		target,
		auth: 'none',
	});
	return response.body;
}

export async function pollDesktopHandoffStatus(
	code: string,
	pollSecret: string | null | undefined,
	target: InstanceHTTPTarget,
): Promise<DesktopHandoffStatusResponse> {
	if (pollSecret == null || pollSecret.length === 0) {
		return readDesktopHandoffStatus(code, target);
	}
	try {
		const response = await instanceRequest<DesktopHandoffStatusResponse>({
			method: 'POST',
			path: Endpoints.AUTH_HANDOFF_STATUS(code),
			target,
			body: {poll_secret: pollSecret},
			auth: 'none',
		});
		return response.body;
	} catch (error) {
		if (!isRouteUnimplemented(error)) {
			throw error;
		}
		observeInstanceResponse(target, InstanceResponseCapability.HANDOFF_POLL_SECRET, false);
		return readDesktopHandoffStatus(code, target);
	}
}

export async function completeDesktopHandoff({
	code,
	token,
	userId,
	returnMethod,
	target,
}: {
	code: string;
	token: string;
	userId: string;
	returnMethod: DesktopHandoffReturnMethod;
	target: InstanceHTTPTarget;
}): Promise<string | null> {
	const response = await instanceRequest<DesktopHandoffCompleteResponse | null>({
		method: 'POST',
		path: Endpoints.AUTH_HANDOFF_COMPLETE,
		target,
		body: {code, user_id: userId, return_method: returnMethod},
		headers: withAuthLocaleHeader({Authorization: token}),
		auth: 'none',
	});
	return response.body?.return_url ?? null;
}

export async function denyDesktopHandoff({code, target}: {code: string; target: InstanceHTTPTarget}): Promise<void> {
	await instanceRequest({
		method: 'POST',
		path: Endpoints.AUTH_HANDOFF_DENY(code),
		target,
		headers: withAuthLocaleHeader(),
		auth: 'none',
	});
}

export async function fetchDesktopHandoffInfo({
	code,
	token,
	target,
}: {
	code: string;
	token: string;
	target: InstanceHTTPTarget;
}): Promise<DesktopHandoffInfoResponse> {
	const response = await instanceRequest<DesktopHandoffInfoResponse>({
		method: 'GET',
		path: Endpoints.AUTH_HANDOFF_INFO(code),
		target,
		headers: withAuthLocaleHeader({Authorization: token}),
		auth: 'none',
	});
	return response.body;
}

export interface CompleteLoginOptions {
	redirectPath?: string | null;
}

interface CompleteLoginPayload {
	token: string;
	userId: string;
	userData?: UserData;
	runtimeSnapshot: RuntimeConfigSnapshot;
}

export class InvalidLoginCredentialsError extends Error {
	constructor(field: 'token' | 'userId') {
		super(`Login response ${field} must be a non-empty string`);
		this.name = 'InvalidLoginCredentialsError';
	}
}

function requireLoginCredential(value: string, field: 'token' | 'userId'): string {
	if (value.trim().length === 0) {
		throw new InvalidLoginCredentialsError(field);
	}
	return value;
}

export async function completeLogin(
	{token, userId, userData, runtimeSnapshot}: CompleteLoginPayload,
	options: CompleteLoginOptions = {},
): Promise<void> {
	logger.info('Completing login process');
	await Accounts.switchToNewAccount({
		userId: requireLoginCredential(userId, 'userId'),
		token: requireLoginCredential(token, 'token'),
		userData,
		runtimeSnapshot,
		redirectPath: options.redirectPath,
	});
}

export async function startSso({
	redirectTo,
	redirectUri,
	target,
}: {
	redirectTo?: string;
	redirectUri?: string;
	target: InstanceHTTPTarget;
}): Promise<SsoStartResponse> {
	const body: SsoStartRequest = {
		redirect_to: redirectTo,
		redirect_uri: redirectUri,
	};
	const response = await instanceRequest<SsoStartResponse>({
		method: 'POST',
		path: Endpoints.AUTH_SSO_START,
		target,
		body,
		headers: withAuthLocaleHeader(),
	});
	return response.body;
}

export async function completeSso({
	code,
	state,
	target,
}: {
	code: string;
	state: string;
	target: InstanceHTTPTarget;
}): Promise<SsoCompleteResponse> {
	const response = await instanceRequest<SsoCompleteResponse>({
		method: 'POST',
		path: Endpoints.AUTH_SSO_COMPLETE,
		target,
		body: {code, state},
		headers: withAuthLocaleHeader(),
	});
	return response.body;
}

interface SetMfaTicketPayload {
	ticket: string;
	totp: boolean;
	webauthn: boolean;
	backupCodes: boolean;
	runtimeSnapshot: RuntimeConfigSnapshot;
}

export function setMfaTicket({ticket, totp, webauthn, backupCodes, runtimeSnapshot}: SetMfaTicketPayload): void {
	logger.debug('Setting MFA ticket');
	Authentication.handleMfaTicketSet({ticket, totp, webauthn, backupCodes, runtimeSnapshot});
}

export function clearMfaTicket(): void {
	logger.debug('Clearing MFA ticket');
	Authentication.handleMfaTicketClear();
}
