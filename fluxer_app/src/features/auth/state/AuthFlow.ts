// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';

export interface LoginSuccessPayload {
	token: string;
	userId: string;
	userData?: UserData;
	redirect_to?: string;
}

export interface MfaChallenge {
	ticket: string;
	sms?: boolean;
	totp: boolean;
	webauthn: boolean;
}

export interface IpAuthorizationChallenge {
	ticket: string;
	email: string;
	resendAvailableIn: number;
}

export type LoginResult =
	| {type: 'success'; payload: LoginSuccessPayload}
	| {type: 'mfa'; challenge: MfaChallenge}
	| {type: 'ip_authorization'; challenge: IpAuthorizationChallenge}
	| {type: 'suspended'; banViewToken: string};

function toLoginSuccessPayload(response: AuthenticationCommands.AuthTokenResponse): LoginSuccessPayload {
	const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
	return {
		token: response.token,
		userId: response.user_id,
		...(userData ? {userData} : {}),
	};
}

export async function loginWithPassword({
	email,
	password,
	inviteCode,
}: {
	email: string;
	password: string;
	inviteCode?: string;
}): Promise<LoginResult> {
	const response = await AuthenticationCommands.login({
		email,
		password,
		inviteCode,
	});
	if (AuthenticationCommands.isIpAuthorizationRequiredResponse(response)) {
		return {
			type: 'ip_authorization',
			challenge: {
				ticket: response.ticket,
				email: response.email,
				resendAvailableIn: response.resend_available_in ?? 30,
			},
		};
	}
	if ('ban_view_token' in response) {
		return {
			type: 'suspended',
			banViewToken: (response as {ban_view_token: string}).ban_view_token,
		};
	}
	if (response.mfa) {
		return {
			type: 'mfa',
			challenge: {
				ticket: response.ticket,
				sms: (response as {sms?: boolean}).sms ?? false,
				totp: response.totp,
				webauthn: response.webauthn,
			},
		};
	}
	return {
		type: 'success',
		payload: toLoginSuccessPayload(response),
	};
}

export async function completeLoginSession(payload: LoginSuccessPayload): Promise<void> {
	await AuthenticationCommands.completeLogin(payload);
}

export async function startSession(token: string): Promise<void> {
	AuthenticationCommands.startSession(token, {startGateway: true});
}

export async function loginWithMfaCode({
	code,
	ticket,
	inviteCode,
}: {
	code: string;
	ticket: string;
	inviteCode?: string;
}): Promise<LoginSuccessPayload> {
	const response = await AuthenticationCommands.loginMfaTotp(code, ticket, inviteCode);
	return toLoginSuccessPayload(response);
}

export async function getWebAuthnMfaOptions(ticket: string): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return AuthenticationCommands.getWebAuthnMfaOptions(ticket);
}

export async function authenticateMfaWithWebAuthn({
	response,
	challenge,
	ticket,
	inviteCode,
}: {
	response: AuthenticationResponseJSON;
	challenge: string;
	ticket: string;
	inviteCode?: string;
}): Promise<LoginSuccessPayload> {
	const result = await AuthenticationCommands.loginMfaWebAuthn(response, challenge, ticket, inviteCode);
	return toLoginSuccessPayload(result);
}

export async function getWebAuthnAuthenticationOptions(): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return AuthenticationCommands.getWebAuthnAuthenticationOptions();
}

export async function authenticateWithWebAuthn({
	response,
	challenge,
	inviteCode,
}: {
	response: AuthenticationResponseJSON;
	challenge: string;
	inviteCode?: string;
}): Promise<LoginSuccessPayload> {
	const result = await AuthenticationCommands.authenticateWithWebAuthn(response, challenge, inviteCode);
	return toLoginSuccessPayload(result);
}

const SSO_REDIRECT_TO_STORAGE_KEY = 'fluxer:sso:redirect_to';

function storeSsoRedirectTo(redirectTo?: string): void {
	try {
		if (redirectTo) {
			window.sessionStorage.setItem(SSO_REDIRECT_TO_STORAGE_KEY, redirectTo);
		} else {
			window.sessionStorage.removeItem(SSO_REDIRECT_TO_STORAGE_KEY);
		}
	} catch {}
}

export function clearPendingSsoRedirectTo(): void {
	storeSsoRedirectTo(undefined);
}

export function getPendingSsoRedirectTo(): string | undefined {
	try {
		return window.sessionStorage.getItem(SSO_REDIRECT_TO_STORAGE_KEY) ?? undefined;
	} catch {
		return undefined;
	}
}

export async function startSsoLogin({redirectTo, redirectUri}: {redirectTo?: string; redirectUri?: string}): Promise<{
	authorizationUrl: string;
	redirectUri: string;
}> {
	const safeRedirectTo = safeRedirectTarget(redirectTo);
	const result = await AuthenticationCommands.startSso({
		redirectTo: safeRedirectTo ?? undefined,
		redirectUri,
	});
	storeSsoRedirectTo(safeRedirectTo ?? undefined);
	return {authorizationUrl: result.authorization_url, redirectUri: result.redirect_uri};
}

export async function completeSsoLogin({code, state}: {code: string; state: string}): Promise<LoginSuccessPayload> {
	const result = await AuthenticationCommands.completeSso({code, state});
	return {
		...toLoginSuccessPayload(result),
		redirect_to: result.redirect_to,
	};
}

interface RegisterSuccessResult {
	type: 'success';
	payload: LoginSuccessPayload;
}

interface RegisterPendingApprovalResult {
	type: 'pending_approval';
	userId: string;
}

export type RegisterResult = RegisterSuccessResult | RegisterPendingApprovalResult;

export async function registerAccount({
	email,
	globalName,
	username,
	password,
	dateOfBirth,
	consent,
	inviteCode,
	giftCode,
	registrationUrlCode,
}: {
	email: string;
	globalName?: string;
	username?: string;
	password: string;
	dateOfBirth: string;
	consent: boolean;
	inviteCode?: string;
	giftCode?: string;
	registrationUrlCode?: string;
}): Promise<RegisterResult> {
	const response = await AuthenticationCommands.register({
		email,
		global_name: globalName,
		username,
		password,
		date_of_birth: dateOfBirth,
		consent,
		invite_code: inviteCode ?? giftCode,
		registration_url_code: registrationUrlCode,
	});
	if (AuthenticationCommands.isRegistrationPendingApprovalResponse(response)) {
		return {
			type: 'pending_approval',
			userId: response.user_id,
		};
	}
	return {
		type: 'success',
		payload: toLoginSuccessPayload(response),
	};
}

export async function requestPasswordReset(email: string): Promise<void> {
	return AuthenticationCommands.forgotPassword(email);
}

export type PasswordResetResult =
	| {type: 'success'; payload: LoginSuccessPayload}
	| {type: 'mfa'; challenge: MfaChallenge};

export async function resetPassword(token: string, password: string): Promise<PasswordResetResult> {
	const response = await AuthenticationCommands.resetPassword(token, password);
	if ('token' in response) {
		return {
			type: 'success',
			payload: toLoginSuccessPayload(response),
		};
	}
	return {
		type: 'mfa',
		challenge: {
			ticket: response.ticket,
			sms: (response as {sms?: boolean}).sms ?? false,
			totp: response.totp,
			webauthn: response.webauthn,
		},
	};
}

export async function verifyEmail(token: string): Promise<AuthenticationCommands.VerificationResult> {
	return AuthenticationCommands.verifyEmail(token);
}

export async function resendVerificationEmail(): Promise<AuthenticationCommands.VerificationResult> {
	return AuthenticationCommands.resendVerificationEmail();
}

export async function authorizeIp(token: string): Promise<AuthenticationCommands.VerificationResult> {
	return AuthenticationCommands.authorizeIp(token);
}

export const VerificationResult = AuthenticationCommands.VerificationResult;

export async function resendIpAuthorization(ticket: string): Promise<void> {
	return AuthenticationCommands.resendIpAuthorization(ticket);
}

export async function pollIpAuthorization(ticket: string): Promise<AuthenticationCommands.IpAuthorizationPollResult> {
	return AuthenticationCommands.pollIpAuthorization(ticket);
}

export async function initiateDesktopHandoff() {
	return AuthenticationCommands.initiateDesktopHandoff();
}

export async function completeDesktopHandoff(params: {code: string; token: string; userId: string}) {
	return AuthenticationCommands.completeDesktopHandoff(params);
}
