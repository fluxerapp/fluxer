// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import AuthSession from '@app/features/auth/state/AuthSession';
import Sudo from '@app/features/auth/state/AuthSudo';
import SudoPrompt from '@app/features/auth/state/SudoPrompt';
import type {SudoVerificationPayload} from '@app/features/auth/types/AuthSudoTypes';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SessionManager from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Users from '@app/features/user/state/Users';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {
	HarvestDownloadUrlResponse,
	HarvestStatusResponse,
} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import type {
	BackupCode,
	EmailChangeRequestNewResponse,
	EmailChangeVerifyOriginalResponse,
	PasswordChangeCompleteResponse,
	PasswordChangeStartResponse,
	PasswordChangeVerifyResponse,
	UserPasswordUpdateResponse,
	UserPrivate,
} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {PublicKeyCredentialCreationOptionsJSON, RegistrationResponseJSON} from '@simplewebauthn/browser';

export interface BulkDeleteMyMessagesFilter {
	scope: 'selected' | 'inaccessible_only';
	include_dms: boolean;
	include_dms_closed: boolean;
	include_group_dms: boolean;
	include_guilds: boolean;
	guild_filter_mode: 'exclude' | 'include_only';
	excluded_guild_ids: Array<string>;
	included_guild_ids: Array<string>;
	start_date: string | null;
	end_date: string | null;
}

export type HarvestDataFilter = BulkDeleteMyMessagesFilter;

const logger = new Logger('User');

interface FluxerTagAvailabilityResponse {
	taken: boolean;
}

interface EmailChangeStartResponse {
	ticket: string;
	require_original: boolean;
	original_proof?: string | null;
	original_code_expires_at?: string;
	resend_available_at?: string | null;
}

interface EmailChangeVerifyNewResponse {
	email_token: string;
}

type UserUpdatePayload = Partial<UserPrivate> & {
	avatar?: string | null;
	password?: string;
	new_password?: string;
	premium_badge_hidden?: boolean;
	premium_badge_masked?: boolean;
	premium_badge_timestamp_hidden?: boolean;
	premium_badge_sequence_hidden?: boolean;
	accent_color?: number | null;
	has_dismissed_premium_onboarding?: boolean;
	has_unread_gift_inventory?: boolean;
	email_token?: string;
	mention_flags?: number;
};
type UserUpdateResponse = UserPrivate & {
	token?: string;
	auth_session_id_hash?: string;
};

interface HarvestRequestResponse {
	harvest_id: string;
}

interface WebAuthnTwoFactorResponse {
	user: UserPrivate;
	backup_codes: Array<BackupCode> | null;
}

export type PreloadedDirectMessages = Record<string, WireMessage>;

const EMPTY_BODY: Record<string, never> = {};

export async function update(user: UserUpdatePayload): Promise<UserUpdateResponse> {
	logger.debug('Updating current user profile');
	const userData = (await http.patch<UserUpdateResponse>(Endpoints.USER_ME, {body: user})).body;
	logger.debug('Successfully updated user profile');
	const updatedFields = Object.keys(user).filter((key) => key !== 'new_password');
	if (updatedFields.length > 0) {
		logger.debug(`Updated fields: ${updatedFields.join(', ')}`);
	}
	if (userData.token) {
		SessionManager.setToken(userData.token);
		GatewaySessions.foregroundConnection.setToken(userData.token);
		if (userData.auth_session_id_hash) {
			AuthSession.handleAuthSessionChange(userData.auth_session_id_hash);
		}
		logger.debug('Authentication token was refreshed');
	}
	return userData;
}

export async function checkFluxerTagAvailability({
	username,
	discriminator,
}: {
	username: string;
	discriminator: string;
}): Promise<boolean> {
	logger.debug(`Checking availability for FluxerTag ${username}#${discriminator}`);
	const response = await http.get<FluxerTagAvailabilityResponse>(Endpoints.USER_CHECK_TAG, {
		query: {username, discriminator},
	});
	return response.body.taken;
}

export async function startEmailChange(): Promise<EmailChangeStartResponse> {
	logger.debug('Starting email change flow');
	const response = await http.post<EmailChangeStartResponse>(Endpoints.USER_EMAIL_CHANGE_START, {
		body: {},
	});
	return response.body;
}

export async function resendEmailChangeOriginal(ticket: string): Promise<void> {
	logger.debug('Resending email change original code');
	await http.post(Endpoints.USER_EMAIL_CHANGE_RESEND_ORIGINAL, {
		body: {ticket: ticket},
	});
}

export async function verifyEmailChangeOriginal(
	ticket: string,
	code: string,
): Promise<EmailChangeVerifyOriginalResponse> {
	logger.debug('Verifying original email code');
	const response = await http.post<EmailChangeVerifyOriginalResponse>(Endpoints.USER_EMAIL_CHANGE_VERIFY_ORIGINAL, {
		body: {ticket: ticket, code: code},
	});
	return response.body;
}

export async function requestEmailChangeNew(
	ticket: string,
	newEmail: string,
	originalProof: string,
	newPassword?: string,
): Promise<EmailChangeRequestNewResponse> {
	logger.debug('Requesting new email code');
	const response = await http.post<EmailChangeRequestNewResponse>(Endpoints.USER_EMAIL_CHANGE_REQUEST_NEW, {
		body: {
			ticket: ticket,
			new_email: newEmail,
			original_proof: originalProof,
			...(newPassword != null ? {new_password: newPassword} : {}),
		},
	});
	return response.body;
}

export async function resendEmailChangeNew(ticket: string): Promise<void> {
	logger.debug('Resending new email code');
	await http.post(Endpoints.USER_EMAIL_CHANGE_RESEND_NEW, {
		body: {ticket: ticket},
	});
}

export async function verifyEmailChangeNew(
	ticket: string,
	code: string,
	originalProof: string,
): Promise<EmailChangeVerifyNewResponse> {
	logger.debug('Verifying new email code');
	const response = await http.post<EmailChangeVerifyNewResponse>(Endpoints.USER_EMAIL_CHANGE_VERIFY_NEW, {
		body: {ticket: ticket, code: code, original_proof: originalProof},
	});
	return response.body;
}

export async function requestBouncedEmailChangeNew(newEmail: string): Promise<EmailChangeRequestNewResponse> {
	logger.debug('Requesting bounced email replacement code');
	const response = await http.post<EmailChangeRequestNewResponse>(Endpoints.USER_EMAIL_CHANGE_BOUNCED_REQUEST_NEW, {
		body: {new_email: newEmail},
	});
	return response.body;
}

export async function resendBouncedEmailChangeNew(ticket: string): Promise<void> {
	logger.debug('Resending bounced email replacement code');
	await http.post(Endpoints.USER_EMAIL_CHANGE_BOUNCED_RESEND_NEW, {
		body: {ticket: ticket},
	});
}

export async function verifyBouncedEmailChangeNew(ticket: string, code: string): Promise<UserPrivate> {
	logger.debug('Verifying bounced email replacement code');
	const response = await http.post<UserPrivate>(Endpoints.USER_EMAIL_CHANGE_BOUNCED_VERIFY_NEW, {
		body: {ticket: ticket, code: code},
	});
	return response.body;
}

export async function startPasswordChange(): Promise<PasswordChangeStartResponse> {
	logger.debug('Starting password change flow');
	const response = await http.post<PasswordChangeStartResponse>(Endpoints.USER_PASSWORD_CHANGE_START, {
		body: {},
	});
	return response.body;
}

export async function resendPasswordChangeCode(ticket: string): Promise<void> {
	logger.debug('Resending password change code');
	await http.post(Endpoints.USER_PASSWORD_CHANGE_RESEND, {
		body: {ticket: ticket},
	});
}

export async function verifyPasswordChangeCode(ticket: string, code: string): Promise<PasswordChangeVerifyResponse> {
	logger.debug('Verifying password change code');
	const response = await http.post<PasswordChangeVerifyResponse>(Endpoints.USER_PASSWORD_CHANGE_VERIFY, {
		body: {ticket: ticket, code: code},
	});
	return response.body;
}

export async function completePasswordChange(
	ticket: string,
	verificationProof: string,
	newPassword: string,
): Promise<void> {
	logger.debug('Completing password change');
	const response = await http.post<PasswordChangeCompleteResponse>(Endpoints.USER_PASSWORD_CHANGE_COMPLETE, {
		body: {
			ticket: ticket,
			verification_proof: verificationProof,
			new_password: newPassword,
		},
	});
	await SessionManager.setToken(response.body.token);
	GatewaySessions.foregroundConnection.setToken(response.body.token);
	AuthSession.handleAuthSessionChange(response.body.auth_session_id_hash);
	logger.info('Password changed successfully');
}

export async function updatePasswordWithSudo(newPassword: string): Promise<void> {
	logger.debug('Updating password with sudo verification');
	const response = await http.post<UserPasswordUpdateResponse>(Endpoints.USER_PASSWORD, {
		body: {new_password: newPassword},
	});
	SessionManager.setToken(response.body.token);
	GatewaySessions.foregroundConnection.setToken(response.body.token);
	AuthSession.handleAuthSessionChange(response.body.auth_session_id_hash);
	logger.info('Password changed successfully');
}

export async function getWebAuthnRegistrationOptions(): Promise<PublicKeyCredentialCreationOptionsJSON> {
	logger.debug('Getting WebAuthn registration options');
	const response = await http.post<PublicKeyCredentialCreationOptionsJSON>(
		Endpoints.USER_MFA_WEBAUTHN_REGISTRATION_OPTIONS,
		{body: {}},
	);
	const data = response.body;
	logger.debug('WebAuthn registration options retrieved');
	return data;
}

export async function registerWebAuthnCredential(
	response: RegistrationResponseJSON,
	challenge: string,
	name: string,
): Promise<void> {
	logger.debug('Registering WebAuthn credential');
	await http.post(Endpoints.USER_MFA_WEBAUTHN_CREDENTIALS, {
		body: {response: response, challenge: challenge, name: name},
	});
	logger.info('WebAuthn credential registered');
	Sudo.clearToken();
}

export async function setWebAuthnTwoFactor(enabled: boolean): Promise<Array<BackupCode> | null> {
	logger.debug('Updating the passkey two-factor preference');
	const response = await http.put<WebAuthnTwoFactorResponse>(Endpoints.USER_MFA_WEBAUTHN_TWO_FACTOR, {
		body: {enabled},
	});
	const {user, backup_codes: backupCodes} = response.body;
	Users.handleUserUpdate(user);
	logger.info('Passkey two-factor preference updated');
	Sudo.clearToken();
	return backupCodes ?? null;
}

export async function renameWebAuthnCredential(credentialId: string, name: string): Promise<void> {
	logger.debug('Renaming WebAuthn credential');
	await http.patch(Endpoints.USER_MFA_WEBAUTHN_CREDENTIAL(credentialId), {body: {name}});
	logger.info('WebAuthn credential renamed');
}

export async function deleteWebAuthnCredential(credentialId: string): Promise<void> {
	logger.debug('Deleting WebAuthn credential');
	await http.delete(Endpoints.USER_MFA_WEBAUTHN_CREDENTIAL(credentialId), {body: EMPTY_BODY});
	logger.info('WebAuthn credential deleted');
}

export async function disableAccount(): Promise<void> {
	logger.debug('Disabling account');
	await http.post(Endpoints.USER_DISABLE, {body: EMPTY_BODY});
	logger.info('Account disabled');
}

export async function deleteAccount(): Promise<void> {
	logger.debug('Deleting account');
	await http.post(Endpoints.USER_DELETE, {body: EMPTY_BODY});
	logger.info('Account scheduled for deletion');
}

export async function forgetAuthorizedIps(sudoPayload: SudoVerificationPayload): Promise<void> {
	logger.debug('Forgetting authorized IPs');
	await http.delete(Endpoints.USER_AUTHORIZED_IPS, {body: sudoPayload});
	logger.info('Authorised IPs cleared');
}

export async function bulkDeleteMyMessages(filter: BulkDeleteMyMessagesFilter): Promise<void> {
	logger.debug('Requesting bulk deletion of my messages', filter);
	await http.post(Endpoints.USER_BULK_DELETE_MY_MESSAGES, {
		body: {
			...filter,
			...(Sudo.hasValidToken()
				? {}
				: await SudoPrompt.requestVerification({method: 'POST', path: Endpoints.USER_BULK_DELETE_MY_MESSAGES})),
		},
	});
	logger.info('Bulk message deletion queued');
}

export async function resetPremiumState(): Promise<void> {
	logger.debug('Resetting premium state for current user');
	await http.post(Endpoints.USER_PREMIUM_RESET);
	logger.info('Reset premium state for current user');
}

export async function requestDataHarvest(): Promise<{
	harvestId: string;
}> {
	logger.debug('Requesting data harvest');
	const harvestId = (await http.post<HarvestRequestResponse>(Endpoints.USER_HARVEST)).body.harvest_id;
	logger.info('Data harvest request submitted', {harvestId});
	return {harvestId};
}

export async function requestFilteredDataHarvest(filter: HarvestDataFilter): Promise<{harvestId: string}> {
	logger.debug('Requesting filtered data harvest');
	const response = await http.post<HarvestRequestResponse>(Endpoints.USER_HARVEST_FILTERED, {
		body: filter,
	});
	logger.info('Filtered data harvest request submitted', {harvestId: response.body.harvest_id});
	return {harvestId: response.body.harvest_id};
}

export async function getLatestHarvest(): Promise<HarvestStatusResponse | null> {
	logger.debug('Fetching latest harvest');
	const response = await http.get<HarvestStatusResponse | null>(Endpoints.USER_HARVEST_LATEST);
	return response.body;
}

export async function getHarvestDownloadUrl(harvestId: string): Promise<HarvestDownloadUrlResponse> {
	logger.debug('Fetching harvest download URL', {harvestId});
	const response = await http.get<HarvestDownloadUrlResponse>(Endpoints.USER_HARVEST_DOWNLOAD(harvestId));
	return response.body;
}

export async function preloadDMMessages(channelIds: Array<string>): Promise<PreloadedDirectMessages> {
	logger.debug('Preloading DM messages', {channelCount: channelIds.length});
	const preloadedData =
		(await http.post<PreloadedDirectMessages>(Endpoints.USER_PRELOAD_MESSAGES, {body: {channels: channelIds}})).body ??
		{};
	Messages.handleMessagePreload({messages: preloadedData});
	return preloadedData;
}
