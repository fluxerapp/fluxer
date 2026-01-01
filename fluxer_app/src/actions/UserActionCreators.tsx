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

import type {PublicKeyCredentialCreationOptionsJSON, RegistrationResponseJSON} from '@simplewebauthn/browser';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Message} from '~/records/MessageRecord';
import type {UserPrivate} from '~/records/UserRecord';
import MessageStore from '~/stores/MessageStore';
import SudoStore from '~/stores/SudoStore';

const logger = new Logger('User');

interface FluxerTagAvailabilityResponse {
	taken: boolean;
}

export interface WebAuthnCredential {
	id: string;
	name: string;
	created_at: string;
	last_used_at: string | null;
}

interface PhoneTokenResponse {
	phone_token: string;
}

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

export const update = async (
	user: Partial<UserPrivate> & {
		avatar?: string | null;
		new_password?: string;
		premium_badge_hidden?: boolean;
		premium_badge_masked?: boolean;
		premium_badge_timestamp_hidden?: boolean;
		premium_badge_sequence_hidden?: boolean;
		accent_color?: number | null;
		has_dismissed_premium_onboarding?: boolean;
		has_unread_gift_inventory?: boolean;
		email_token?: string;
	},
): Promise<UserPrivate & {token?: string}> => {
	try {
		logger.debug('Updating current user profile');
		const response = await http.patch<UserPrivate & {token?: string}>(Endpoints.USER_ME, user);
		const userData = response.body;
		logger.debug('Successfully updated user profile');
		const updatedFields = Object.keys(user).filter((key) => key !== 'new_password');
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
};

export const checkFluxerTagAvailability = async ({
	username,
	discriminator,
}: {
	username: string;
	discriminator: string;
}): Promise<boolean> => {
	try {
		logger.debug(`Checking availability for FluxerTag ${username}#${discriminator}`);
		const response = await http.get<FluxerTagAvailabilityResponse>({
			url: Endpoints.USER_CHECK_TAG,
			query: {username, discriminator},
		});
		return response.body.taken;
	} catch (error) {
		logger.error('Failed to check FluxerTag availability:', error);
		throw error;
	}
};

export const sendPhoneVerification = async (phone: string): Promise<void> => {
	try {
		logger.debug('Sending phone verification code');
		await http.post({url: Endpoints.USER_PHONE_SEND_VERIFICATION, body: {phone}});
		logger.debug('Phone verification code sent');
	} catch (error) {
		logger.error('Failed to send phone verification code', error);
		throw error;
	}
};

export const verifyPhone = async (phone: string, code: string): Promise<PhoneTokenResponse> => {
	try {
		logger.debug('Verifying phone code');
		const response = await http.post<PhoneTokenResponse>(Endpoints.USER_PHONE_VERIFY, {phone, code});
		logger.debug('Phone code verified');
		return response.body;
	} catch (error) {
		logger.error('Failed to verify phone code', error);
		throw error;
	}
};

export const addPhone = async (phoneToken: string): Promise<void> => {
	try {
		logger.debug('Adding phone to account');
		await http.post({url: Endpoints.USER_PHONE, body: {phone_token: phoneToken}});
		logger.info('Phone added to account');
	} catch (error) {
		logger.error('Failed to add phone to account', error);
		throw error;
	}
};

export const startEmailChange = async (): Promise<EmailChangeStartResponse> => {
	try {
		logger.debug('Starting email change flow');
		const response = await http.post<EmailChangeStartResponse>({
			url: Endpoints.USER_EMAIL_CHANGE_START,
			body: {},
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to start email change', error);
		throw error;
	}
};

export const resendEmailChangeOriginal = async (ticket: string): Promise<void> => {
	try {
		logger.debug('Resending email change original code');
		await http.post({
			url: Endpoints.USER_EMAIL_CHANGE_RESEND_ORIGINAL,
			body: {ticket},
		});
	} catch (error) {
		logger.error('Failed to resend original email code', error);
		throw error;
	}
};

export const verifyEmailChangeOriginal = async (
	ticket: string,
	code: string,
): Promise<EmailChangeVerifyOriginalResponse> => {
	try {
		logger.debug('Verifying original email code');
		const response = await http.post<EmailChangeVerifyOriginalResponse>({
			url: Endpoints.USER_EMAIL_CHANGE_VERIFY_ORIGINAL,
			body: {ticket, code},
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to verify original email code', error);
		throw error;
	}
};

export const requestEmailChangeNew = async (
	ticket: string,
	newEmail: string,
	originalProof: string,
): Promise<EmailChangeRequestNewResponse> => {
	try {
		logger.debug('Requesting new email code');
		const response = await http.post<EmailChangeRequestNewResponse>({
			url: Endpoints.USER_EMAIL_CHANGE_REQUEST_NEW,
			body: {ticket, new_email: newEmail, original_proof: originalProof},
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to request new email code', error);
		throw error;
	}
};

export const resendEmailChangeNew = async (ticket: string): Promise<void> => {
	try {
		logger.debug('Resending new email code');
		await http.post({
			url: Endpoints.USER_EMAIL_CHANGE_RESEND_NEW,
			body: {ticket},
		});
	} catch (error) {
		logger.error('Failed to resend new email code', error);
		throw error;
	}
};

export const verifyEmailChangeNew = async (
	ticket: string,
	code: string,
	originalProof: string,
): Promise<EmailChangeVerifyNewResponse> => {
	try {
		logger.debug('Verifying new email code');
		const response = await http.post<EmailChangeVerifyNewResponse>({
			url: Endpoints.USER_EMAIL_CHANGE_VERIFY_NEW,
			body: {ticket, code, original_proof: originalProof},
		});
		return response.body;
	} catch (error) {
		logger.error('Failed to verify new email code', error);
		throw error;
	}
};

export const removePhone = async (): Promise<void> => {
	try {
		logger.debug('Removing phone from account');
		await http.delete({url: Endpoints.USER_PHONE, body: {}});
		logger.info('Phone removed from account');
	} catch (error) {
		logger.error('Failed to remove phone from account', error);
		throw error;
	}
};

export const enableSmsMfa = async (): Promise<void> => {
	try {
		logger.debug('Enabling SMS MFA');
		await http.post({url: Endpoints.USER_MFA_SMS_ENABLE, body: {}});
		logger.info('SMS MFA enabled');
		SudoStore.clearToken();
	} catch (error) {
		logger.error('Failed to enable SMS MFA', error);
		throw error;
	}
};

export const disableSmsMfa = async (): Promise<void> => {
	try {
		logger.debug('Disabling SMS MFA');
		await http.post({url: Endpoints.USER_MFA_SMS_DISABLE, body: {}});
		logger.info('SMS MFA disabled');
	} catch (error) {
		logger.error('Failed to disable SMS MFA', error);
		throw error;
	}
};

export const listWebAuthnCredentials = async (): Promise<Array<WebAuthnCredential>> => {
	try {
		logger.debug('Fetching WebAuthn credentials');
		const response = await http.get<Array<WebAuthnCredential>>({url: Endpoints.USER_MFA_WEBAUTHN_CREDENTIALS});
		const data = response.body ?? [];
		logger.debug(`Found ${data.length} WebAuthn credentials`);
		return data;
	} catch (error) {
		logger.error('Failed to fetch WebAuthn credentials', error);
		throw error;
	}
};

export const getWebAuthnRegistrationOptions = async (): Promise<PublicKeyCredentialCreationOptionsJSON> => {
	try {
		logger.debug('Getting WebAuthn registration options');
		const response = await http.post<PublicKeyCredentialCreationOptionsJSON>({
			url: Endpoints.USER_MFA_WEBAUTHN_REGISTRATION_OPTIONS,
			body: {},
		});
		const data = response.body;
		logger.debug('WebAuthn registration options retrieved');
		return data;
	} catch (error) {
		logger.error('Failed to get WebAuthn registration options', error);
		throw error;
	}
};

export const registerWebAuthnCredential = async (
	response: RegistrationResponseJSON,
	challenge: string,
	name: string,
): Promise<void> => {
	try {
		logger.debug('Registering WebAuthn credential');
		await http.post({url: Endpoints.USER_MFA_WEBAUTHN_CREDENTIALS, body: {response, challenge, name}});
		logger.info('WebAuthn credential registered');
		SudoStore.clearToken();
	} catch (error) {
		logger.error('Failed to register WebAuthn credential', error);
		throw error;
	}
};

export const renameWebAuthnCredential = async (credentialId: string, name: string): Promise<void> => {
	try {
		logger.debug('Renaming WebAuthn credential');
		await http.patch({url: Endpoints.USER_MFA_WEBAUTHN_CREDENTIAL(credentialId), body: {name}});
		logger.info('WebAuthn credential renamed');
	} catch (error) {
		logger.error('Failed to rename WebAuthn credential', error);
		throw error;
	}
};

export const deleteWebAuthnCredential = async (credentialId: string): Promise<void> => {
	try {
		logger.debug('Deleting WebAuthn credential');
		await http.delete({url: Endpoints.USER_MFA_WEBAUTHN_CREDENTIAL(credentialId), body: {}});
		logger.info('WebAuthn credential deleted');
	} catch (error) {
		logger.error('Failed to delete WebAuthn credential', error);
		throw error;
	}
};

export const disableAccount = async (): Promise<void> => {
	try {
		logger.debug('Disabling account');
		await http.post({url: Endpoints.USER_DISABLE, body: {}});
		logger.info('Account disabled');
	} catch (error) {
		logger.error('Failed to disable account', error);
		throw error;
	}
};

export const deleteAccount = async (): Promise<void> => {
	try {
		logger.debug('Deleting account');
		await http.post({url: Endpoints.USER_DELETE, body: {}});
		logger.info('Account scheduled for deletion');
	} catch (error) {
		logger.error('Failed to delete account', error);
		throw error;
	}
};

export const bulkDeleteAllMessages = async (): Promise<void> => {
	try {
		logger.debug('Requesting bulk deletion of all messages');
		await http.post({url: Endpoints.USER_BULK_DELETE_MESSAGES, body: {}});
		logger.info('Bulk message deletion queued');
	} catch (error) {
		logger.error('Failed to queue bulk message deletion', error);
		throw error;
	}
};

export const cancelBulkDeleteAllMessages = async (): Promise<void> => {
	try {
		logger.debug('Cancelling bulk deletion of all messages');
		await http.delete({url: Endpoints.USER_BULK_DELETE_MESSAGES, body: {}});
		logger.info('Bulk message deletion cancelled');
	} catch (error) {
		logger.error('Failed to cancel bulk message deletion', error);
		throw error;
	}
};

export const testBulkDeleteAllMessages = async (): Promise<void> => {
	try {
		logger.debug('Requesting test bulk deletion of all messages (15s delay)');
		await http.post({url: Endpoints.USER_BULK_DELETE_MESSAGES_TEST});
		logger.info('Test bulk message deletion queued (15s delay)');
	} catch (error) {
		logger.error('Failed to queue test bulk message deletion', error);
		throw error;
	}
};

export const requestDataHarvest = async (): Promise<{harvestId: string}> => {
	try {
		logger.debug('Requesting data harvest');
		const response = await http.post<{harvest_id: string}>({url: Endpoints.USER_HARVEST});
		logger.info('Data harvest request submitted', {harvestId: response.body.harvest_id});
		return {harvestId: response.body.harvest_id};
	} catch (error) {
		logger.error('Failed to request data harvest', error);
		throw error;
	}
};

export const getLatestHarvest = async (): Promise<any> => {
	try {
		logger.debug('Fetching latest harvest');
		const response = await http.get<any>({url: Endpoints.USER_HARVEST_LATEST});
		return response.body;
	} catch (error) {
		logger.error('Failed to fetch latest harvest', error);
		throw error;
	}
};

export const getHarvestStatus = async (harvestId: string): Promise<any> => {
	try {
		logger.debug('Fetching harvest status', {harvestId});
		const response = await http.get<any>({url: Endpoints.USER_HARVEST_STATUS(harvestId)});
		return response.body;
	} catch (error) {
		logger.error('Failed to fetch harvest status', error);
		throw error;
	}
};

export type PreloadedDirectMessages = Record<string, Message>;

export const preloadDMMessages = async (channelIds: Array<string>): Promise<PreloadedDirectMessages> => {
	try {
		logger.debug('Preloading DM messages', {channelCount: channelIds.length});
		const response = await http.post<PreloadedDirectMessages>(Endpoints.USER_PRELOAD_MESSAGES, {
			channels: channelIds,
		});
		const preloadedData = response.body ?? {};

		MessageStore.handleMessagePreload({messages: preloadedData});

		return preloadedData;
	} catch (error) {
		logger.error('Failed to preload DM messages', error);
		throw error;
	}
};
