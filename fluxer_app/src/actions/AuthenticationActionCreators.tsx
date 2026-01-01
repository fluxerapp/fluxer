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

import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';
import {APIErrorCodes} from '~/Constants';
import {Endpoints} from '~/Endpoints';
import type {UserData} from '~/lib/AccountStorage';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import AccountManager from '~/stores/AccountManager';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ConnectionStore from '~/stores/ConnectionStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {isDesktop} from '~/utils/NativeUtils';

const logger = new Logger('AuthService');

const getPlatformHeaderValue = (): 'web' | 'desktop' | 'mobile' => (isDesktop() ? 'desktop' : 'web');
const withPlatformHeader = (headers?: Record<string, string>): Record<string, string> => ({
	'X-Fluxer-Platform': getPlatformHeaderValue(),
	...(headers ?? {}),
});

export const VerificationResult = {
	SUCCESS: 'SUCCESS',
	EXPIRED_TOKEN: 'EXPIRED_TOKEN',
	RATE_LIMITED: 'RATE_LIMITED',
	SERVER_ERROR: 'SERVER_ERROR',
} as const;
export type VerificationResult = (typeof VerificationResult)[keyof typeof VerificationResult];

interface RegisterData {
	email?: string;
	global_name?: string;
	username?: string;
	password?: string;
	beta_code: string;
	date_of_birth: string;
	consent: boolean;
	captchaToken?: string;
	captchaType?: 'turnstile' | 'hcaptcha';
	invite_code?: string;
}

interface StandardLoginResponse {
	mfa: false;
	user_id: string;
	token: string;
	theme?: string;
	pending_verification?: boolean;
}

interface MfaLoginResponse {
	mfa: true;
	ticket: string;
	sms: boolean;
	totp: boolean;
	webauthn: boolean;
}

type LoginResponse = StandardLoginResponse | MfaLoginResponse;

export interface IpAuthorizationRequiredResponse {
	ip_authorization_required: true;
	ticket: string;
	email: string;
	resend_available_in: number;
}

export const isIpAuthorizationRequiredResponse = (
	response: LoginResponse | IpAuthorizationRequiredResponse,
): response is IpAuthorizationRequiredResponse => {
	return (response as IpAuthorizationRequiredResponse).ip_authorization_required === true;
};

interface TokenResponse {
	user_id: string;
	token: string;
	theme?: string;
	pending_verification?: boolean;
}

interface DesktopHandoffInitiateResponse {
	code: string;
	expires_at: string;
}

interface DesktopHandoffStatusResponse {
	status: 'pending' | 'completed' | 'expired';
	token?: string;
	user_id?: string;
}

export const login = async ({
	email,
	password,
	captchaToken,
	inviteCode,
	captchaType,
	customApiEndpoint,
}: {
	email: string;
	password: string;
	captchaToken?: string;
	inviteCode?: string;
	captchaType?: 'turnstile' | 'hcaptcha';
	customApiEndpoint?: string;
}): Promise<LoginResponse | IpAuthorizationRequiredResponse> => {
	try {
		if (customApiEndpoint) {
			await RuntimeConfigStore.connectToEndpoint(customApiEndpoint);
		}

		const headers: Record<string, string> = {};
		if (captchaToken) {
			headers['X-Captcha-Token'] = captchaToken;
			headers['X-Captcha-Type'] = captchaType || 'hcaptcha';
		}
		const body: {
			email: string;
			password: string;
			invite_code?: string;
		} = {email, password};
		if (inviteCode) {
			body.invite_code = inviteCode;
		}
		const response = await http.post<LoginResponse>({
			url: Endpoints.AUTH_LOGIN,
			body,
			headers: withPlatformHeader(headers),
		});
		logger.debug('Login successful', {mfa: response.body?.mfa});
		return response.body;
	} catch (error) {
		const httpError = error as {status?: number; body?: any};
		if (httpError.status === 403 && httpError.body?.code === APIErrorCodes.IP_AUTHORIZATION_REQUIRED) {
			logger.info('Login requires IP authorization', {email});
			return {
				ip_authorization_required: true,
				ticket: httpError.body?.ticket,
				email: httpError.body?.email,
				resend_available_in: httpError.body?.resend_available_in ?? 30,
			};
		}
		logger.error('Login failed', error);
		throw error;
	}
};

export const loginMfaTotp = async (code: string, ticket: string, inviteCode?: string): Promise<TokenResponse> => {
	try {
		const body: {
			code: string;
			ticket: string;
			invite_code?: string;
		} = {code, ticket};
		if (inviteCode) {
			body.invite_code = inviteCode;
		}
		const response = await http.post<TokenResponse>({
			url: Endpoints.AUTH_LOGIN_MFA_TOTP,
			body,
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('MFA TOTP authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA TOTP authentication failed', error);
		throw error;
	}
};

export const loginMfaSmsSend = async (ticket: string): Promise<void> => {
	try {
		await http.post({
			url: Endpoints.AUTH_LOGIN_MFA_SMS_SEND,
			body: {ticket},
			headers: withPlatformHeader(),
		});
		logger.debug('SMS MFA code sent');
	} catch (error) {
		logger.error('Failed to send SMS MFA code', error);
		throw error;
	}
};

export const loginMfaSms = async (code: string, ticket: string, inviteCode?: string): Promise<TokenResponse> => {
	try {
		const body: {
			code: string;
			ticket: string;
			invite_code?: string;
		} = {code, ticket};
		if (inviteCode) {
			body.invite_code = inviteCode;
		}
		const response = await http.post<TokenResponse>({
			url: Endpoints.AUTH_LOGIN_MFA_SMS,
			body,
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('MFA SMS authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA SMS authentication failed', error);
		throw error;
	}
};

export const loginMfaWebAuthn = async (
	response: AuthenticationResponseJSON,
	challenge: string,
	ticket: string,
	inviteCode?: string,
): Promise<TokenResponse> => {
	try {
		const body: {
			response: AuthenticationResponseJSON;
			challenge: string;
			ticket: string;
			invite_code?: string;
		} = {response, challenge, ticket};
		if (inviteCode) {
			body.invite_code = inviteCode;
		}
		const httpResponse = await http.post<TokenResponse>({
			url: Endpoints.AUTH_LOGIN_MFA_WEBAUTHN,
			body,
			headers: withPlatformHeader(),
		});
		const responseBody = httpResponse.body;
		logger.debug('MFA WebAuthn authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('MFA WebAuthn authentication failed', error);
		throw error;
	}
};

export const getWebAuthnMfaOptions = async (ticket: string): Promise<PublicKeyCredentialRequestOptionsJSON> => {
	try {
		const response = await http.post<PublicKeyCredentialRequestOptionsJSON>({
			url: Endpoints.AUTH_LOGIN_MFA_WEBAUTHN_OPTIONS,
			body: {ticket},
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('WebAuthn MFA options retrieved');
		return responseBody;
	} catch (error) {
		logger.error('Failed to get WebAuthn MFA options', error);
		throw error;
	}
};

export const getWebAuthnAuthenticationOptions = async (): Promise<PublicKeyCredentialRequestOptionsJSON> => {
	try {
		const response = await http.post<PublicKeyCredentialRequestOptionsJSON>({
			url: Endpoints.AUTH_WEBAUTHN_OPTIONS,
			headers: withPlatformHeader(),
		});
		const responseBody = response.body;
		logger.debug('WebAuthn authentication options retrieved');
		return responseBody;
	} catch (error) {
		logger.error('Failed to get WebAuthn authentication options', error);
		throw error;
	}
};

export const authenticateWithWebAuthn = async (
	response: AuthenticationResponseJSON,
	challenge: string,
	inviteCode?: string,
): Promise<TokenResponse> => {
	try {
		const body: {
			response: AuthenticationResponseJSON;
			challenge: string;
			invite_code?: string;
		} = {response, challenge};
		if (inviteCode) {
			body.invite_code = inviteCode;
		}
		const httpResponse = await http.post<TokenResponse>({
			url: Endpoints.AUTH_WEBAUTHN_AUTHENTICATE,
			body,
			headers: withPlatformHeader(),
		});
		const responseBody = httpResponse.body;
		logger.debug('WebAuthn authentication successful');
		return responseBody;
	} catch (error) {
		logger.error('WebAuthn authentication failed', error);
		throw error;
	}
};

export const register = async (data: RegisterData): Promise<TokenResponse> => {
	try {
		const headers: Record<string, string> = {};
		if (data.captchaToken) {
			headers['X-Captcha-Token'] = data.captchaToken;
			headers['X-Captcha-Type'] = data.captchaType || 'hcaptcha';
		}
		const {captchaToken: _, captchaType: __, ...bodyData} = data;
		const response = await http.post<TokenResponse>({
			url: Endpoints.AUTH_REGISTER,
			body: bodyData,
			headers: withPlatformHeader(headers),
		});
		const responseBody = response.body;
		logger.info('Registration successful');
		return responseBody;
	} catch (error) {
		logger.error('Registration failed', error);
		throw error;
	}
};

interface UsernameSuggestionsResponse {
	suggestions: Array<string>;
}

export const getUsernameSuggestions = async (globalName: string): Promise<Array<string>> => {
	try {
		const response = await http.post<UsernameSuggestionsResponse>({
			url: Endpoints.AUTH_USERNAME_SUGGESTIONS,
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
};

export const forgotPassword = async (
	email: string,
	captchaToken?: string,
	captchaType?: 'turnstile' | 'hcaptcha',
): Promise<void> => {
	try {
		const headers: Record<string, string> = {};
		if (captchaToken) {
			headers['X-Captcha-Token'] = captchaToken;
			headers['X-Captcha-Type'] = captchaType || 'hcaptcha';
		}
		await http.post({
			url: Endpoints.AUTH_FORGOT_PASSWORD,
			body: {email},
			headers: withPlatformHeader(headers),
		});
		logger.debug('Password reset email sent');
	} catch (error) {
		logger.warn('Password reset request failed, but returning success to user', error);
	}
};

export const resetPassword = async (token: string, password: string): Promise<TokenResponse> => {
	try {
		const response = await http.post<TokenResponse>({
			url: Endpoints.AUTH_RESET_PASSWORD,
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
};

export const revertEmailChange = async (token: string, password: string): Promise<TokenResponse> => {
	try {
		const response = await http.post<TokenResponse>({
			url: Endpoints.AUTH_EMAIL_REVERT,
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
};

export const verifyEmail = async (token: string): Promise<VerificationResult> => {
	try {
		await http.post({
			url: Endpoints.AUTH_VERIFY_EMAIL,
			body: {token},
			headers: withPlatformHeader(),
		});
		logger.info('Email verification successful');
		return VerificationResult.SUCCESS;
	} catch (error) {
		const httpError = error as {status?: number};
		if (httpError.status === 400) {
			logger.warn('Email verification failed - expired or invalid token');
			return VerificationResult.EXPIRED_TOKEN;
		}
		logger.error('Email verification failed - server error', error);
		return VerificationResult.SERVER_ERROR;
	}
};

export const resendVerificationEmail = async (): Promise<VerificationResult> => {
	try {
		await http.post({
			url: Endpoints.AUTH_RESEND_VERIFICATION,
			headers: withPlatformHeader(),
		});
		logger.info('Verification email resent');
		return VerificationResult.SUCCESS;
	} catch (error) {
		const httpError = error as {status?: number};
		if (httpError.status === 429) {
			logger.warn('Rate limited when resending verification email');
			return VerificationResult.RATE_LIMITED;
		}
		logger.error('Failed to resend verification email - server error', error);
		return VerificationResult.SERVER_ERROR;
	}
};

export const logout = async (): Promise<void> => {
	await AccountManager.logout();
};

export const authorizeIp = async (token: string): Promise<VerificationResult> => {
	try {
		await http.post({
			url: Endpoints.AUTH_AUTHORIZE_IP,
			body: {token},
			headers: withPlatformHeader(),
		});
		logger.info('IP authorization successful');
		return VerificationResult.SUCCESS;
	} catch (error) {
		const httpError = error as {status?: number};
		if (httpError.status === 400) {
			logger.warn('IP authorization failed - expired or invalid token');
			return VerificationResult.EXPIRED_TOKEN;
		}
		logger.error('IP authorization failed - server error', error);
		return VerificationResult.SERVER_ERROR;
	}
};

export const resendIpAuthorization = async (ticket: string): Promise<void> => {
	await http.post({
		url: Endpoints.AUTH_IP_AUTHORIZATION_RESEND,
		body: {ticket},
		headers: withPlatformHeader(),
	});
};

export const subscribeToIpAuthorization = (ticket: string): EventSource => {
	const base = RuntimeConfigStore.apiEndpoint || '';
	const url = `${base}${Endpoints.AUTH_IP_AUTHORIZATION_STREAM(ticket)}`;
	return new EventSource(url);
};

export const initiateDesktopHandoff = async (): Promise<DesktopHandoffInitiateResponse> => {
	const response = await http.post<DesktopHandoffInitiateResponse>({
		url: Endpoints.AUTH_HANDOFF_INITIATE,
		skipAuth: true,
	});
	return response.body;
};

export const pollDesktopHandoffStatus = async (
	code: string,
	customApiEndpoint?: string,
): Promise<DesktopHandoffStatusResponse> => {
	const url = customApiEndpoint
		? `${customApiEndpoint}${Endpoints.AUTH_HANDOFF_STATUS(code)}`
		: Endpoints.AUTH_HANDOFF_STATUS(code);
	const response = await http.get<DesktopHandoffStatusResponse>({
		url,
		skipAuth: true,
	});
	return response.body;
};

export const completeDesktopHandoff = async ({
	code,
	token,
	userId,
}: {
	code: string;
	token: string;
	userId: string;
}): Promise<void> => {
	await http.post({
		url: Endpoints.AUTH_HANDOFF_COMPLETE,
		body: {code, token, user_id: userId},
		skipAuth: true,
	});
};

export const startSession = (token: string, options: {startGateway?: boolean} = {}): void => {
	const {startGateway = true} = options;

	logger.info('Starting new session');
	AuthenticationStore.handleSessionStart({token});

	if (!startGateway) {
		return;
	}

	ConnectionStore.startSession(token);
};

let sessionStartInProgress = false;

export const ensureSessionStarted = async (): Promise<void> => {
	if (sessionStartInProgress) {
		return;
	}

	if (AccountManager.isSwitching) {
		return;
	}

	if (!AuthenticationStore.isAuthenticated) {
		return;
	}

	if (ConnectionStore.isConnected || ConnectionStore.isConnecting) {
		return;
	}

	if (ConnectionStore.socket) {
		return;
	}

	sessionStartInProgress = true;

	try {
		logger.info('Ensuring session is started');

		const token = AuthenticationStore.authToken;
		if (token) {
			ConnectionStore.startSession(token);
		}
	} finally {
		setTimeout(() => {
			sessionStartInProgress = false;
		}, 100);
	}
};

export const completeLogin = async ({
	token,
	userId,
	userData,
}: {
	token: string;
	userId: string;
	userData?: UserData;
}): Promise<void> => {
	logger.info('Completing login process');

	if (userId && token) {
		await AccountManager.switchToNewAccount(userId, token, userData, false);
	} else {
		startSession(token, {startGateway: true});
	}
};

interface SetMfaTicketPayload {
	ticket: string;
	sms: boolean;
	totp: boolean;
	webauthn: boolean;
}

export const setMfaTicket = ({ticket, sms, totp, webauthn}: SetMfaTicketPayload): void => {
	logger.debug('Setting MFA ticket');
	AuthenticationStore.handleMfaTicketSet({ticket, sms, totp, webauthn});
};

export const clearMfaTicket = (): void => {
	logger.debug('Clearing MFA ticket');
	AuthenticationStore.handleMfaTicketClear();
};

export const redeemBetaCode = async (betaCode: string): Promise<void> => {
	try {
		await http.post({
			url: Endpoints.AUTH_REDEEM_BETA_CODE,
			body: {beta_code: betaCode},
			headers: withPlatformHeader(),
		});
		logger.info('Beta code redeemed successfully');
	} catch (error) {
		logger.error('Beta code redemption failed', error);
		throw error;
	}
};
