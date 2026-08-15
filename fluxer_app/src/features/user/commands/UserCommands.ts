// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import AuthSession from '@app/features/auth/state/AuthSession';
import Sudo from '@app/features/auth/state/AuthSudo';
import SudoPrompt from '@app/features/auth/state/SudoPrompt';
import type {SudoVerificationPayload} from '@app/features/auth/types/AuthSudoTypes';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SessionManager from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {HarvestStatusResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import type {PasswordChangeCompleteResponse, UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
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

interface PhoneVerifyResult {
	verified: true;
}

export interface InboundPhoneChallengeResponse {
	challenge_code: string;
	our_number: string;
	expires_at: string;
}

export type PhoneInboundChallengeReason =
	| 'voip'
	| 'canadian'
	| 'unknown_line_type'
	| 'expensive_destination'
	| 'account_forced'
	| 'behavioural_risk';

export interface PhoneSendVerificationInboundChallengeResponse extends InboundPhoneChallengeResponse {
	channel: 'inbound_challenge';
	reason: PhoneInboundChallengeReason;
}

export type PhoneVerificationSendChannel = 'sms' | 'inbound_challenge';
export type PhoneSendVerificationResult =
	| {
			channel: 'sms';
	  }
	| PhoneSendVerificationInboundChallengeResponse;
type PhoneSendVerificationApiResponse = PhoneSendVerificationResult;

interface EmailChangeStartResponse {
	ticket: string;
	require_original: boolean;
	original_proof?: string | null;
	original_code_expires_at?: string;
	resend_available_at?: string | null;
}

interface EmailChangeVerifyOriginalResponse {
	original_proof: string;
}

interface EmailChangeRequestNewResponse {
	ticket: string;
	new_email: string;
	new_code_expires_at: string;
	resend_available_at: string | null;
}

interface EmailChangeVerifyNewResponse {
	email_token: string;
}

interface PasswordChangeStartResponse {
	ticket: string;
	code_expires_at: string;
	resend_available_at: string | null;
}

interface PasswordChangeVerifyResponse {
	verification_proof: string;
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
};

interface HarvestRequestResponse {
	harvest_id: string;
}

export type PreloadedDirectMessages = Record<string, WireMessage>;

const EMPTY_BODY: Record<string, never> = {};

function updatedUserFields(user: UserUpdatePayload): Array<string> {
	return Object.keys(user).filter((key) => key !== 'new_password');
}

async function requestUserUpdate(user: UserUpdatePayload): Promise<UserUpdateResponse> {
	const response = await http.patch<UserUpdateResponse>(Endpoints.USER_ME, {body: user});
	return response.body;
}

function phoneVerificationRequest(
	phone: string,
	channel?: PhoneVerificationSendChannel,
): {phone: string; channel?: PhoneVerificationSendChannel} {
	return channel ? {phone, channel} : {phone};
}

function phoneCodeRequest(phone: string, code: string): {phone: string; code: string} {
	return {phone, code};
}

function emailTicketRequest(ticket: string): {ticket: string} {
	return {ticket};
}

function emailCodeRequest(ticket: string, code: string): {ticket: string; code: string} {
	return {ticket, code};
}

function requestNewEmailBody(
	ticket: string,
	newEmail: string,
	originalProof: string,
	newPassword?: string,
): {ticket: string; new_email: string; original_proof: string; new_password?: string} {
	return {
		ticket,
		new_email: newEmail,
		original_proof: originalProof,
		...(newPassword != null ? {new_password: newPassword} : {}),
	};
}

function verifyNewEmailBody(
	ticket: string,
	code: string,
	originalProof: string,
): {ticket: string; code: string; original_proof: string} {
	return {ticket, code, original_proof: originalProof};
}

function bouncedEmailRequest(newEmail: string): {new_email: string} {
	return {new_email: newEmail};
}

function completePasswordChangeBody(
	ticket: string,
	verificationProof: string,
	newPassword: string,
): {ticket: string; verification_proof: string; new_password: string} {
	return {
		ticket,
		verification_proof: verificationProof,
		new_password: newPassword,
	};
}

async function emailApplySudoPayload(): Promise<SudoVerificationPayload> {
	return Sudo.hasValidToken()
		? {}
		: await SudoPrompt.requestVerification({
				method: 'POST',
				path: Endpoints.USER_EMAIL_CHANGE_APPLY,
			});
}

async function requestEmailApply(emailToken: string): Promise<UserUpdateResponse> {
	const response = await http.post<UserUpdateResponse>(Endpoints.USER_EMAIL_CHANGE_APPLY, {
		body: {email_token: emailToken, ...(await emailApplySudoPayload())},
	});
	return response.body;
}

function webAuthnRegistrationBody(
	response: RegistrationResponseJSON,
	challenge: string,
	name: string,
): {response: RegistrationResponseJSON; challenge: string; name: string} {
	return {response, challenge, name};
}

async function requestAccountPost(endpoint: string): Promise<void> {
	await http.post(endpoint, {body: EMPTY_BODY});
}

async function bulkDeleteMyMessagesSudoPayload(): Promise<SudoVerificationPayload> {
	return Sudo.hasValidToken()
		? {}
		: await SudoPrompt.requestVerification({
				method: 'POST',
				path: Endpoints.USER_BULK_DELETE_MY_MESSAGES,
			});
}

async function requestHarvest(): Promise<string> {
	const response = await http.post<HarvestRequestResponse>(Endpoints.USER_HARVEST);
	return response.body.harvest_id;
}

async function requestPreloadedDMMessages(channelIds: Array<string>): Promise<PreloadedDirectMessages> {
	const response = await http.post<PreloadedDirectMessages>(Endpoints.USER_PRELOAD_MESSAGES, {
		body: {channels: channelIds},
	});
	return response.body ?? {};
}

export async function update(user: UserUpdatePayload): Promise<UserUpdateResponse> {
	try {
		logger.debug('Updating current user profile');
		const userData = await requestUserUpdate(user);
		logger.debug('Successfully updated user profile');
		const updatedFields = updatedUserFields(user);
		if (updatedFields.length > 0) {
			logger.debug(`Updated fields: ${updatedFields.join(', ')}`);
		}
		if (userData.token) {
			logger.debug('Authentication token was refreshed');
		}
		return userData;
	} catch (error) {
		logger.error('Failed to update user profile:', error);
		throw error;
	}
}

export async function checkFluxerTagAvailability({
	username,
	discriminator,
}: {
	username: string;
	discriminator: string;
}): Promise<boolean> {
	try {
		logger.debug(`Checking availability for FluxerTag ${username}#${discriminator}`);
		const response = await http.get<FluxerTagAvailabilityResponse>(Endpoints.USER_CHECK_TAG, {
			query: {username, discriminator},
		});
		return response.body.taken;
	} catch (error) {
		logger.error('Failed to check FluxerTag availability:', error);
		throw error;
	}
}

export async function startInboundPhoneChallenge(): Promise<InboundPhoneChallengeResponse> {
	try {
		logger.debug('Starting inbound phone challenge');
		const response = await http.post<InboundPhoneChallengeResponse>(Endpoints.USER_PHONE_INBOUND_CHALLENGE, {
			body: {},
		});
		logger.debug('Inbound phone challenge started');
		return response.body;
	} catch (error) {
		logger.error('Failed to start inbound phone challenge', error);
		throw error;
	}
}

export async function sendPhoneVerification(
	phone: string,
	channel?: PhoneVerificationSendChannel,
): Promise<PhoneSendVerificationResult> {
	try {
		logger.debug('Sending phone verification code');
		const response = await http.post<PhoneSendVerificationApiResponse | undefined>(
			Endpoints.USER_PHONE_SEND_VERIFICATION,
			{body: phoneVerificationRequest(phone, channel)},
		);
		logger.debug('Phone verification code sent');
		if (!response.body) return {channel: 'sms'};
		return response.body;
	} catch (error) {
		logger.error('Failed to send phone verification code', error);
		throw error;
	}
}

export async function verifyPhone(phone: string, code: string): Promise<PhoneVerifyResult> {
	try {
		logger.debug('Verifying phone code');
		const response = await http.post<PhoneVerifyResult>(Endpoints.USER_PHONE_VERIFY, {
			body: phoneCodeRequest(phone, code),
		});
		logger.debug('Phone code verified');
		return response.body;
	} catch (error) {
		logger.error('Failed to verify phone code', error);
		throw error;
	}
}

export async function startEmailChange(): Promise<EmailChangeStartResponse> {
	try {
		logger.debug('Starting email change flow');
		const response = await http.post<EmailChangeStartResponse>(Endpoints.USER_EMAIL_CHANGE_START, {
			body: {},
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to start email change', error);
		throw error;
	}
}

export async function resendEmailChangeOriginal(ticket: string): Promise<void> {
	try {
		logger.debug('Resending email change original code');
		await http.post(Endpoints.USER_EMAIL_CHANGE_RESEND_ORIGINAL, {
			body: emailTicketRequest(ticket),
		});
	} catch (error) {
		logger.error('Failed to resend original email code', error);
		throw error;
	}
}

export async function verifyEmailChangeOriginal(
	ticket: string,
	code: string,
): Promise<EmailChangeVerifyOriginalResponse> {
	try {
		logger.debug('Verifying original email code');
		const response = await http.post<EmailChangeVerifyOriginalResponse>(Endpoints.USER_EMAIL_CHANGE_VERIFY_ORIGINAL, {
			body: emailCodeRequest(ticket, code),
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to verify original email code', error);
		throw error;
	}
}

export async function requestEmailChangeNew(
	ticket: string,
	newEmail: string,
	originalProof: string,
	newPassword?: string,
): Promise<EmailChangeRequestNewResponse> {
	try {
		logger.debug('Requesting new email code');
		const response = await http.post<EmailChangeRequestNewResponse>(Endpoints.USER_EMAIL_CHANGE_REQUEST_NEW, {
			body: requestNewEmailBody(ticket, newEmail, originalProof, newPassword),
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to request new email code', error);
		throw error;
	}
}

export async function resendEmailChangeNew(ticket: string): Promise<void> {
	try {
		logger.debug('Resending new email code');
		await http.post(Endpoints.USER_EMAIL_CHANGE_RESEND_NEW, {
			body: emailTicketRequest(ticket),
		});
	} catch (error) {
		logger.error('Failed to resend new email code', error);
		throw error;
	}
}

export async function verifyEmailChangeNew(
	ticket: string,
	code: string,
	originalProof: string,
): Promise<EmailChangeVerifyNewResponse> {
	try {
		logger.debug('Verifying new email code');
		const response = await http.post<EmailChangeVerifyNewResponse>(Endpoints.USER_EMAIL_CHANGE_VERIFY_NEW, {
			body: verifyNewEmailBody(ticket, code, originalProof),
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to verify new email code', error);
		throw error;
	}
}

export async function applyEmailChange(emailToken: string): Promise<
	UserPrivate & {
		token?: string;
	}
> {
	try {
		logger.debug('Applying verified email change');
		return await requestEmailApply(emailToken);
	} catch (error) {
		logger.error('Failed to apply email change', error);
		throw error;
	}
}

export async function requestBouncedEmailChangeNew(newEmail: string): Promise<EmailChangeRequestNewResponse> {
	try {
		logger.debug('Requesting bounced email replacement code');
		const response = await http.post<EmailChangeRequestNewResponse>(Endpoints.USER_EMAIL_CHANGE_BOUNCED_REQUEST_NEW, {
			body: bouncedEmailRequest(newEmail),
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to request bounced email replacement code', error);
		throw error;
	}
}

export async function resendBouncedEmailChangeNew(ticket: string): Promise<void> {
	try {
		logger.debug('Resending bounced email replacement code');
		await http.post(Endpoints.USER_EMAIL_CHANGE_BOUNCED_RESEND_NEW, {
			body: emailTicketRequest(ticket),
		});
	} catch (error) {
		logger.error('Failed to resend bounced email replacement code', error);
		throw error;
	}
}

export async function verifyBouncedEmailChangeNew(ticket: string, code: string): Promise<UserPrivate> {
	try {
		logger.debug('Verifying bounced email replacement code');
		const response = await http.post<UserPrivate>(Endpoints.USER_EMAIL_CHANGE_BOUNCED_VERIFY_NEW, {
			body: emailCodeRequest(ticket, code),
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to verify bounced email replacement code', error);
		throw error;
	}
}

export async function startPasswordChange(): Promise<PasswordChangeStartResponse> {
	try {
		logger.debug('Starting password change flow');
		const response = await http.post<PasswordChangeStartResponse>(Endpoints.USER_PASSWORD_CHANGE_START, {
			body: {},
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to start password change', error);
		throw error;
	}
}

export async function resendPasswordChangeCode(ticket: string): Promise<void> {
	try {
		logger.debug('Resending password change code');
		await http.post(Endpoints.USER_PASSWORD_CHANGE_RESEND, {
			body: emailTicketRequest(ticket),
		});
	} catch (error) {
		logger.error('Failed to resend password change code', error);
		throw error;
	}
}

export async function verifyPasswordChangeCode(ticket: string, code: string): Promise<PasswordChangeVerifyResponse> {
	try {
		logger.debug('Verifying password change code');
		const response = await http.post<PasswordChangeVerifyResponse>(Endpoints.USER_PASSWORD_CHANGE_VERIFY, {
			body: emailCodeRequest(ticket, code),
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to verify password change code', error);
		throw error;
	}
}

export async function completePasswordChange(
	ticket: string,
	verificationProof: string,
	newPassword: string,
): Promise<void> {
	try {
		logger.debug('Completing password change');
		const response = await http.post<PasswordChangeCompleteResponse>(Endpoints.USER_PASSWORD_CHANGE_COMPLETE, {
			body: completePasswordChangeBody(ticket, verificationProof, newPassword),
		});
		SessionManager.setToken(response.body.token);
		GatewayConnection.setToken(response.body.token);
		AuthSession.handleAuthSessionChange(response.body.auth_session_id_hash);
		logger.info('Password changed successfully');
	} catch (error) {
		logger.error('Failed to complete password change', error);
		throw error;
	}
}

export async function getWebAuthnRegistrationOptions(): Promise<PublicKeyCredentialCreationOptionsJSON> {
	try {
		logger.debug('Getting WebAuthn registration options');
		const response = await http.post<PublicKeyCredentialCreationOptionsJSON>(
			Endpoints.USER_MFA_WEBAUTHN_REGISTRATION_OPTIONS,
			{body: {}},
		);
		const data = response.body;
		logger.debug('WebAuthn registration options retrieved');
		return data;
	} catch (error) {
		logger.error('Failed to get WebAuthn registration options', error);
		throw error;
	}
}

export async function registerWebAuthnCredential(
	response: RegistrationResponseJSON,
	challenge: string,
	name: string,
): Promise<void> {
	try {
		logger.debug('Registering WebAuthn credential');
		await http.post(Endpoints.USER_MFA_WEBAUTHN_CREDENTIALS, {
			body: webAuthnRegistrationBody(response, challenge, name),
		});
		logger.info('WebAuthn credential registered');
		Sudo.clearToken();
	} catch (error) {
		logger.error('Failed to register WebAuthn credential', error);
		throw error;
	}
}

export async function renameWebAuthnCredential(credentialId: string, name: string): Promise<void> {
	try {
		logger.debug('Renaming WebAuthn credential');
		await http.patch(Endpoints.USER_MFA_WEBAUTHN_CREDENTIAL(credentialId), {body: {name}});
		logger.info('WebAuthn credential renamed');
	} catch (error) {
		logger.error('Failed to rename WebAuthn credential', error);
		throw error;
	}
}

export async function deleteWebAuthnCredential(credentialId: string): Promise<void> {
	try {
		logger.debug('Deleting WebAuthn credential');
		await http.delete(Endpoints.USER_MFA_WEBAUTHN_CREDENTIAL(credentialId), {body: EMPTY_BODY});
		logger.info('WebAuthn credential deleted');
	} catch (error) {
		logger.error('Failed to delete WebAuthn credential', error);
		throw error;
	}
}

export async function disableAccount(): Promise<void> {
	try {
		logger.debug('Disabling account');
		await requestAccountPost(Endpoints.USER_DISABLE);
		logger.info('Account disabled');
	} catch (error) {
		logger.error('Failed to disable account', error);
		throw error;
	}
}

export async function deleteAccount(): Promise<void> {
	try {
		logger.debug('Deleting account');
		await requestAccountPost(Endpoints.USER_DELETE);
		logger.info('Account scheduled for deletion');
	} catch (error) {
		logger.error('Failed to delete account', error);
		throw error;
	}
}

export async function forgetAuthorizedIps(sudoPayload: SudoVerificationPayload): Promise<void> {
	try {
		logger.debug('Forgetting authorized IPs');
		await http.delete(Endpoints.USER_AUTHORIZED_IPS, {body: sudoPayload});
		logger.info('Authorised IPs cleared');
	} catch (error) {
		logger.error('Failed to forget authorized IPs', error);
		throw error;
	}
}

export async function bulkDeleteMyMessages(filter: BulkDeleteMyMessagesFilter): Promise<void> {
	try {
		logger.debug('Requesting bulk deletion of my messages', filter);
		await http.post(Endpoints.USER_BULK_DELETE_MY_MESSAGES, {
			body: {...filter, ...(await bulkDeleteMyMessagesSudoPayload())},
		});
		logger.info('Bulk message deletion queued');
	} catch (error) {
		logger.error('Failed to queue bulk message deletion', error);
		throw error;
	}
}

export async function resetPremiumState(): Promise<void> {
	try {
		logger.debug('Resetting premium state for current user');
		await http.post(Endpoints.USER_PREMIUM_RESET);
		logger.info('Reset premium state for current user');
	} catch (error) {
		logger.error('Failed to reset premium state', error);
		throw error;
	}
}

export async function requestDataHarvest(): Promise<{
	harvestId: string;
}> {
	try {
		logger.debug('Requesting data harvest');
		const harvestId = await requestHarvest();
		logger.info('Data harvest request submitted', {harvestId});
		return {harvestId};
	} catch (error) {
		logger.error('Failed to request data harvest', error);
		throw error;
	}
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
	try {
		logger.debug('Fetching latest harvest');
		const response = await http.get<HarvestStatusResponse | null>(Endpoints.USER_HARVEST_LATEST);
		return response.body;
	} catch (error) {
		logger.error('Failed to fetch latest harvest', error);
		throw error;
	}
}

export async function getHarvestStatus(harvestId: string): Promise<HarvestStatusResponse> {
	try {
		logger.debug('Fetching harvest status', {harvestId});
		const response = await http.get<HarvestStatusResponse>(Endpoints.USER_HARVEST_STATUS(harvestId));
		return response.body;
	} catch (error) {
		logger.error('Failed to fetch harvest status', error);
		throw error;
	}
}

export async function preloadDMMessages(channelIds: Array<string>): Promise<PreloadedDirectMessages> {
	try {
		logger.debug('Preloading DM messages', {channelCount: channelIds.length});
		const preloadedData = await requestPreloadedDMMessages(channelIds);
		Messages.handleMessagePreload({messages: preloadedData});
		return preloadedData;
	} catch (error) {
		logger.error('Failed to preload DM messages', error);
		throw error;
	}
}
