// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import type {UserData} from '@app/features/auth/state/AccountStorage';
import {type AuthRequestTarget, authRequestTargetFromSnapshot} from '@app/features/auth/state/AuthRequestTarget';
import {isValidOAuthState, storeSsoPendingContext} from '@app/features/auth/state/SsoPendingContext';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {type InstanceHTTPTarget, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {AuthenticationResponseJSON, PublicKeyCredentialRequestOptionsJSON} from '@simplewebauthn/browser';

const logger = new Logger('AuthFlow');

export function authCommandTarget(runtimeSnapshot: RuntimeConfigSnapshot): InstanceHTTPTarget {
	return instanceTargetFromSnapshot(runtimeSnapshot);
}

export interface LoginSuccessPayload {
	token: string;
	userId: string;
	userData?: UserData;
	redirect_to?: string;
}

export interface MfaChallenge {
	ticket: string;
	totp: boolean;
	webauthn: boolean;
	backupCodes: boolean;
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

function mfaChallengeFromResponse(response: AuthenticationCommands.MfaLoginResponse): MfaChallenge {
	return {
		ticket: response.ticket,
		totp: response.totp,
		webauthn: response.webauthn,
		backupCodes: response.backup_codes ?? false,
	};
}

export function toLoginSuccessPayload(response: AuthenticationCommands.AuthTokenResponse): LoginSuccessPayload {
	const userData = AuthenticationCommands.authResponseUserToUserData(response.user);
	return {
		token: response.token,
		userId: response.user_id,
		...(userData ? {userData} : {}),
	};
}

export async function loginWithPassword({
	runtimeSnapshot,
	...params
}: AuthenticationCommands.LoginIdentifier & {
	password: string;
	inviteCode?: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<LoginResult> {
	const response = await AuthenticationCommands.login({
		...params,
		target: authRequestTargetFromSnapshot(runtimeSnapshot),
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
			challenge: mfaChallengeFromResponse(response),
		};
	}
	return {
		type: 'success',
		payload: toLoginSuccessPayload(response),
	};
}

export async function completeLoginSession(
	payload: LoginSuccessPayload,
	runtimeSnapshot: RuntimeConfigSnapshot,
): Promise<void> {
	await AuthenticationCommands.completeLogin({...payload, runtimeSnapshot});
}

export async function loginWithMfaCode({
	code,
	ticket,
	inviteCode,
	runtimeSnapshot,
}: {
	code: string;
	ticket: string;
	inviteCode?: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<LoginSuccessPayload> {
	const response = await AuthenticationCommands.loginMfaTotp({
		code,
		ticket,
		inviteCode,
		target: authCommandTarget(runtimeSnapshot),
	});
	return toLoginSuccessPayload(response);
}

export async function getWebAuthnMfaOptions(
	ticket: string,
	runtimeSnapshot: RuntimeConfigSnapshot,
): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return AuthenticationCommands.getWebAuthnMfaOptions({ticket, target: authCommandTarget(runtimeSnapshot)});
}

export async function authenticateMfaWithWebAuthn({
	response,
	challenge,
	ticket,
	inviteCode,
	runtimeSnapshot,
}: {
	response: AuthenticationResponseJSON;
	challenge: string;
	ticket: string;
	inviteCode?: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<LoginSuccessPayload> {
	const result = await AuthenticationCommands.loginMfaWebAuthn({
		response,
		challenge,
		ticket,
		inviteCode,
		target: authCommandTarget(runtimeSnapshot),
	});
	return toLoginSuccessPayload(result);
}

export async function getWebAuthnAuthenticationOptions(
	runtimeSnapshot: RuntimeConfigSnapshot,
): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return AuthenticationCommands.getWebAuthnAuthenticationOptions({target: authCommandTarget(runtimeSnapshot)});
}

export async function authenticateWithWebAuthn({
	response,
	challenge,
	inviteCode,
	runtimeSnapshot,
}: {
	response: AuthenticationResponseJSON;
	challenge: string;
	inviteCode?: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<LoginSuccessPayload> {
	const result = await AuthenticationCommands.authenticateWithWebAuthn({
		response,
		challenge,
		inviteCode,
		target: authCommandTarget(runtimeSnapshot),
	});
	return toLoginSuccessPayload(result);
}

function ssoStateFromAuthorizationUrl(authorizationUrl: string): string | null {
	try {
		return new URL(authorizationUrl).searchParams.get('state');
	} catch {
		return null;
	}
}

export interface StartSsoLoginRequest {
	redirectTo?: string | null;
	redirectUri?: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}

export interface StartSsoLoginResult {
	authorizationUrl: string;
	redirectUri: string;
	state: string | null;
}

export async function startSsoLogin({
	redirectTo,
	redirectUri,
	runtimeSnapshot,
}: StartSsoLoginRequest): Promise<StartSsoLoginResult> {
	const safeRedirectTo = safeRedirectTarget(redirectTo);
	const result = await AuthenticationCommands.startSso({
		redirectTo: safeRedirectTo ?? undefined,
		redirectUri,
		target: authCommandTarget(runtimeSnapshot),
	});
	const state = result.state ?? ssoStateFromAuthorizationUrl(result.authorization_url);
	if (state != null && isValidOAuthState(state)) {
		try {
			await storeSsoPendingContext(state, {
				redirectTo: safeRedirectTo,
				runtimeSnapshot,
			});
		} catch (error) {
			logger.warn('Failed to persist the SSO pending context, continuing without it', error);
		}
	}
	return {authorizationUrl: result.authorization_url, redirectUri: result.redirect_uri, state};
}

export async function completeSsoLogin({
	code,
	state,
	runtimeSnapshot,
}: {
	code: string;
	state: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<LoginSuccessPayload> {
	const result = await AuthenticationCommands.completeSso({
		code,
		state,
		target: authCommandTarget(runtimeSnapshot),
	});
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
	runtimeSnapshot,
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
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<RegisterResult> {
	const response = await AuthenticationCommands.register(
		{
			email,
			global_name: globalName,
			username,
			password,
			date_of_birth: dateOfBirth,
			consent,
			invite_code: inviteCode ?? giftCode,
			registration_url_code: registrationUrlCode,
		},
		authRequestTargetFromSnapshot(runtimeSnapshot),
	);
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

export type PasswordResetResult =
	| {type: 'success'; payload: LoginSuccessPayload}
	| {type: 'mfa'; challenge: MfaChallenge};

export async function resetPassword(
	token: string,
	password: string,
	target: AuthRequestTarget,
): Promise<PasswordResetResult> {
	const response = await AuthenticationCommands.resetPassword(token, password, target);
	if ('token' in response) {
		return {
			type: 'success',
			payload: toLoginSuccessPayload(response),
		};
	}
	return {
		type: 'mfa',
		challenge: mfaChallengeFromResponse(response),
	};
}

export interface IssuedRecoveryKit {
	recoveryKey: string;
	createdAt: string;
}

export type AccountRecoveryResult = PasswordResetResult & {kit: IssuedRecoveryKit};

export async function recoverAccount({
	login,
	recoveryKey,
	password,
	runtimeSnapshot,
}: {
	login: string;
	recoveryKey: string;
	password: string;
	runtimeSnapshot: RuntimeConfigSnapshot;
}): Promise<AccountRecoveryResult> {
	const response = await AuthenticationCommands.recoverAccount({
		login,
		recoveryKey,
		password,
		target: authRequestTargetFromSnapshot(runtimeSnapshot),
	});
	const kit = {recoveryKey: response.recovery_key, createdAt: response.recovery_kit_created_at};
	if ('token' in response) {
		return {type: 'success', payload: toLoginSuccessPayload(response), kit};
	}
	return {
		type: 'mfa',
		challenge: {
			ticket: response.ticket,
			totp: response.totp,
			webauthn: response.webauthn,
			backupCodes: response.backup_codes ?? false,
		},
		kit,
	};
}

export async function resendIpAuthorization(ticket: string, runtimeSnapshot: RuntimeConfigSnapshot): Promise<void> {
	return AuthenticationCommands.resendIpAuthorization({ticket, target: authCommandTarget(runtimeSnapshot)});
}

export async function pollIpAuthorization(
	ticket: string,
	runtimeSnapshot: RuntimeConfigSnapshot,
): Promise<AuthenticationCommands.IpAuthorizationPollResult> {
	return AuthenticationCommands.pollIpAuthorization({ticket, target: authCommandTarget(runtimeSnapshot)});
}
