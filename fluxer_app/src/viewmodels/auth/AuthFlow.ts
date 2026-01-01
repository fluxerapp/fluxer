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
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';

export interface LoginSuccessPayload {
	token: string;
	userId: string;
	pendingVerification?: boolean;
}

export interface MfaChallenge {
	ticket: string;
	sms: boolean;
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
	| {type: 'ip_authorization'; challenge: IpAuthorizationChallenge};

export async function loginWithPassword({
	email,
	password,
	inviteCode,
	customApiEndpoint,
}: {
	email: string;
	password: string;
	inviteCode?: string;
	customApiEndpoint?: string;
}): Promise<LoginResult> {
	const response = await AuthenticationActionCreators.login({
		email,
		password,
		inviteCode,
		customApiEndpoint,
	});

	if (AuthenticationActionCreators.isIpAuthorizationRequiredResponse(response)) {
		return {
			type: 'ip_authorization',
			challenge: {
				ticket: response.ticket,
				email: response.email,
				resendAvailableIn: response.resend_available_in ?? 30,
			},
		};
	}

	if (response.mfa) {
		return {
			type: 'mfa',
			challenge: {
				ticket: response.ticket,
				sms: response.sms,
				totp: response.totp,
				webauthn: response.webauthn,
			},
		};
	}

	const successResponse = response as {token: string; user_id: string; pending_verification?: boolean};

	return {
		type: 'success',
		payload: {
			token: successResponse.token,
			userId: successResponse.user_id,
			pendingVerification: successResponse.pending_verification,
		},
	};
}

export async function completeLoginSession(payload: LoginSuccessPayload): Promise<void> {
	await AuthenticationActionCreators.completeLogin(payload);
}

export async function startSession(token: string): Promise<void> {
	AuthenticationActionCreators.startSession(token, {startGateway: true});
}

export type MfaCodeMethod = 'sms' | 'totp';

export async function loginWithMfaCode({
	code,
	ticket,
	inviteCode,
	method,
}: {
	code: string;
	ticket: string;
	inviteCode?: string;
	method: MfaCodeMethod;
}): Promise<LoginSuccessPayload> {
	const response =
		method === 'sms'
			? await AuthenticationActionCreators.loginMfaSms(code, ticket, inviteCode)
			: await AuthenticationActionCreators.loginMfaTotp(code, ticket, inviteCode);

	return {token: response.token, userId: response.user_id};
}

export async function sendMfaSms(ticket: string): Promise<void> {
	await AuthenticationActionCreators.loginMfaSmsSend(ticket);
}

export async function getWebAuthnMfaOptions(ticket: string): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return AuthenticationActionCreators.getWebAuthnMfaOptions(ticket);
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
	const result = await AuthenticationActionCreators.loginMfaWebAuthn(response, challenge, ticket, inviteCode);
	return {token: result.token, userId: result.user_id};
}

export async function getWebAuthnAuthenticationOptions(): Promise<PublicKeyCredentialRequestOptionsJSON> {
	return AuthenticationActionCreators.getWebAuthnAuthenticationOptions();
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
	const result = await AuthenticationActionCreators.authenticateWithWebAuthn(response, challenge, inviteCode);
	return {token: result.token, userId: result.user_id};
}

interface RegisterPendingVerificationResult {
	type: 'pending_verification';
}

interface RegisterSuccessResult {
	type: 'success';
	payload: LoginSuccessPayload;
}

export type RegisterResult = RegisterPendingVerificationResult | RegisterSuccessResult;

export async function registerAccount({
	email,
	globalName,
	username,
	password,
	betaCode,
	dateOfBirth,
	consent,
	inviteCode,
	giftCode,
}: {
	email: string;
	globalName?: string;
	username?: string;
	password: string;
	betaCode: string;
	dateOfBirth: string;
	consent: boolean;
	inviteCode?: string;
	giftCode?: string;
}): Promise<RegisterResult> {
	const response = await AuthenticationActionCreators.register({
		email,
		global_name: globalName,
		username,
		password,
		beta_code: betaCode,
		date_of_birth: dateOfBirth,
		consent,
		invite_code: inviteCode ?? giftCode,
	});

	if (response.pending_verification) {
		return {type: 'pending_verification'};
	}

	return {
		type: 'success',
		payload: {token: response.token, userId: response.user_id},
	};
}

export async function requestPasswordReset(email: string): Promise<void> {
	return AuthenticationActionCreators.forgotPassword(email);
}

export async function resetPassword(token: string, password: string): Promise<LoginSuccessPayload> {
	const response = await AuthenticationActionCreators.resetPassword(token, password);
	return {token: response.token, userId: response.user_id};
}

export async function verifyEmail(token: string): Promise<AuthenticationActionCreators.VerificationResult> {
	return AuthenticationActionCreators.verifyEmail(token);
}

export async function resendVerificationEmail(): Promise<AuthenticationActionCreators.VerificationResult> {
	return AuthenticationActionCreators.resendVerificationEmail();
}

export async function authorizeIp(token: string): Promise<AuthenticationActionCreators.VerificationResult> {
	return AuthenticationActionCreators.authorizeIp(token);
}

export const VerificationResult = AuthenticationActionCreators.VerificationResult;

export async function resendIpAuthorization(ticket: string): Promise<void> {
	return AuthenticationActionCreators.resendIpAuthorization(ticket);
}

export async function subscribeToIpAuthorization(ticket: string): Promise<EventSource> {
	return AuthenticationActionCreators.subscribeToIpAuthorization(ticket);
}

export async function initiateDesktopHandoff() {
	return AuthenticationActionCreators.initiateDesktopHandoff();
}

export async function completeDesktopHandoff(params: {code: string; token: string; userId: string}) {
	return AuthenticationActionCreators.completeDesktopHandoff(params);
}
