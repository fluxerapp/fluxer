// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {
	type AuthTokenResponse,
	isRegistrationPendingApprovalResponse,
	type MfaLoginResponse,
	type RegistrationPendingApprovalResponse,
} from '@app/features/auth/commands/AuthenticationCommands';
import {readDeviceProof, storeDeviceProof} from '@app/features/auth/state/AccountEntryStorage';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import {type LoginSuccessPayload, type MfaChallenge, toLoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {instanceRequest, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {getCurrentOrDetectedLocale} from '@app/features/user/utils/LocaleUtils';
import type {EmailCodeSendResponse, IdentifyResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {GuildEntryResult} from '@fluxer/schema/src/domains/user/AccountSchemas';

type EntryTokenResponse = AuthTokenResponse & {device_proof?: string; entry?: GuildEntryResult | null};

export type AccountEntryResult =
	| {type: 'success'; payload: LoginSuccessPayload; entry: GuildEntryResult | null}
	| {type: 'mfa'; challenge: MfaChallenge}
	| {type: 'pending_approval'};

function localeHeaders(): Record<string, string> {
	return {'Accept-Language': getCurrentOrDetectedLocale()};
}

function deviceProofForEmail(email: string): string | undefined {
	const normalized = email.trim().toLowerCase();
	for (const account of switcherAccounts()) {
		if (account.userData?.email?.toLowerCase() === normalized) {
			return readDeviceProof(account.userId) ?? undefined;
		}
	}
	return undefined;
}

function toEntryResult(
	response: EntryTokenResponse | MfaLoginResponse | RegistrationPendingApprovalResponse,
): AccountEntryResult {
	if ('mfa' in response && response.mfa === true) {
		return {
			type: 'mfa',
			challenge: {
				ticket: response.ticket,
				totp: response.totp,
				webauthn: response.webauthn,
				backupCodes: response.backup_codes ?? false,
			},
		};
	}
	if (isRegistrationPendingApprovalResponse(response as RegistrationPendingApprovalResponse)) {
		return {type: 'pending_approval'};
	}
	const tokenResponse = response as EntryTokenResponse;
	storeDeviceProof(tokenResponse.user_id, tokenResponse.device_proof);
	return {type: 'success', payload: toLoginSuccessPayload(tokenResponse), entry: tokenResponse.entry ?? null};
}

export async function identify(email: string, snapshot: RuntimeConfigSnapshot): Promise<IdentifyResponse> {
	const deviceProof = deviceProofForEmail(email);
	const response = await instanceRequest<IdentifyResponse>({
		method: 'POST',
		path: Endpoints.AUTH_IDENTIFY,
		target: instanceTargetFromSnapshot(snapshot),
		body: deviceProof === undefined ? {email} : {email, device_proof: deviceProof},
		headers: localeHeaders(),
	});
	return response.body;
}

export async function sendEmailCode(
	ticket: string,
	registrationUrlCode: string | null,
	snapshot: RuntimeConfigSnapshot,
): Promise<EmailCodeSendResponse> {
	const response = await instanceRequest<EmailCodeSendResponse>({
		method: 'POST',
		path: Endpoints.AUTH_EMAIL_CODE_SEND,
		target: instanceTargetFromSnapshot(snapshot),
		body: registrationUrlCode === null ? {ticket} : {ticket, registration_url_code: registrationUrlCode},
		headers: localeHeaders(),
	});
	return response.body;
}

interface VerifyEmailCodeRequest {
	readonly ticket: string;
	readonly code: string;
	readonly consent: boolean;
	readonly inviteCode: string | null;
	readonly registrationUrlCode: string | null;
	readonly profile: NewAccountProfile | null;
	readonly snapshot: RuntimeConfigSnapshot;
}

export interface NewAccountProfile {
	readonly globalName: string;
	readonly username: string;
}

export async function verifyEmailCode({
	ticket,
	code,
	consent,
	inviteCode,
	registrationUrlCode,
	profile,
	snapshot,
}: VerifyEmailCodeRequest): Promise<AccountEntryResult> {
	const response = await instanceRequest<EntryTokenResponse | MfaLoginResponse | RegistrationPendingApprovalResponse>({
		method: 'POST',
		path: Endpoints.AUTH_EMAIL_CODE_VERIFY,
		target: instanceTargetFromSnapshot(snapshot),
		body: {
			ticket,
			code,
			consent,
			...(inviteCode === null ? {} : {invite_code: inviteCode}),
			...(registrationUrlCode === null ? {} : {registration_url_code: registrationUrlCode}),
			...(profile === null ? {} : {global_name: profile.globalName, username: profile.username}),
		},
		headers: localeHeaders(),
	});
	return toEntryResult(response.body);
}

interface InstantAccountRequest {
	readonly globalName: string;
	readonly consent: boolean;
	readonly inviteCode: string | null;
	readonly registrationUrlCode: string | null;
	readonly snapshot: RuntimeConfigSnapshot;
}

export async function createInstantAccount({
	globalName,
	consent,
	inviteCode,
	registrationUrlCode,
	snapshot,
}: InstantAccountRequest): Promise<AccountEntryResult> {
	const response = await instanceRequest<EntryTokenResponse | RegistrationPendingApprovalResponse>({
		method: 'POST',
		path: Endpoints.AUTH_REGISTER,
		target: instanceTargetFromSnapshot(snapshot),
		body: {
			...(globalName === '' ? {} : {global_name: globalName}),
			consent,
			...(inviteCode === null ? {} : {invite_code: inviteCode}),
			...(registrationUrlCode === null ? {} : {registration_url_code: registrationUrlCode}),
		},
		headers: localeHeaders(),
	});
	return toEntryResult(response.body);
}

interface RedeemAccountHandoffRequest {
	readonly handoffCode: string;
	readonly nonce: string;
	readonly consent: boolean;
	readonly globalName: string;
	readonly snapshot: RuntimeConfigSnapshot;
}

export async function redeemAccountHandoff({
	handoffCode,
	nonce,
	consent,
	globalName,
	snapshot,
}: RedeemAccountHandoffRequest): Promise<{payload: LoginSuccessPayload; entry: GuildEntryResult | null}> {
	const response = await instanceRequest<EntryTokenResponse>({
		method: 'POST',
		path: Endpoints.AUTH_ACCOUNT_HANDOFF_REDEEM,
		target: instanceTargetFromSnapshot(snapshot),
		body: {
			handoff_code: handoffCode,
			nonce,
			consent,
			global_name: globalName,
		},
		headers: localeHeaders(),
		auth: 'none',
	});
	storeDeviceProof(response.body.user_id, response.body.device_proof);
	return {payload: toLoginSuccessPayload(response.body), entry: response.body.entry ?? null};
}
