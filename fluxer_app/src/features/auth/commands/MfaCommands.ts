// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import Sudo from '@app/features/auth/state/AuthSudo';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {BackupCode} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const logger = new Logger('MFA');

interface BackupCodesResponse {
	backup_codes: Array<BackupCode>;
}

interface BackupCodesChallengeResponse {
	ticket: string;
	code_expires_at: string;
	resend_available_at: string | null;
}

interface BackupCodesChallengeVerifyResponse extends BackupCodesResponse {
	verification_proof: string;
}

export interface BackupCodesChallenge {
	ticket: string;
	verificationProof: string;
}

const CHALLENGE_CREDENTIAL_PATHS = new Set(['ticket', 'verification_proof']);

export function isExpiredBackupCodesChallengeError(error: unknown): boolean {
	if (!error || typeof error !== 'object' || !('body' in error)) {
		return false;
	}
	const body = (error as {body?: {errors?: Array<{path?: string}>}}).body;
	return body?.errors?.some((validationError) => CHALLENGE_CREDENTIAL_PATHS.has(validationError.path ?? '')) ?? false;
}

type BackupCodeMode = 'fetch' | 'regenerate';

function backupCodeMode(regenerate: boolean): BackupCodeMode {
	return regenerate ? 'regenerate' : 'fetch';
}

async function requestTotpEnable(secret: string, code: string): Promise<Array<BackupCode>> {
	const response = await http.post<BackupCodesResponse>(Endpoints.USER_MFA_TOTP_ENABLE, {
		body: {secret, code},
	});
	return response.body.backup_codes;
}

async function requestTotpDisable(code: string): Promise<void> {
	await http.post(Endpoints.USER_MFA_TOTP_DISABLE, {body: {code}});
}

async function requestBackupCodes(regenerate: boolean): Promise<Array<BackupCode>> {
	const response = await http.post<BackupCodesResponse>(Endpoints.USER_MFA_BACKUP_CODES, {
		body: {regenerate},
	});
	return response.body.backup_codes;
}

async function requestBackupCodesChallengeStart(): Promise<BackupCodesChallengeResponse> {
	const response = await http.post<BackupCodesChallengeResponse>(Endpoints.USER_MFA_BACKUP_CODES_CHALLENGE_START, {
		body: {},
	});
	return response.body;
}

async function requestBackupCodesChallengeResend(ticket: string): Promise<void> {
	await http.post(Endpoints.USER_MFA_BACKUP_CODES_CHALLENGE_RESEND, {body: {ticket}});
}

async function requestBackupCodesChallengeVerify(
	ticket: string,
	code: string,
): Promise<BackupCodesChallengeVerifyResponse> {
	const response = await http.post<BackupCodesChallengeVerifyResponse>(
		Endpoints.USER_MFA_BACKUP_CODES_CHALLENGE_VERIFY,
		{body: {ticket, code}},
	);
	return response.body;
}

async function requestBackupCodesChallengeRegenerate(
	ticket: string,
	verificationProof: string,
): Promise<Array<BackupCode>> {
	const response = await http.post<BackupCodesResponse>(Endpoints.USER_MFA_BACKUP_CODES_CHALLENGE_REGENERATE, {
		body: {ticket, verification_proof: verificationProof},
	});
	return response.body.backup_codes;
}

function rethrowMfaFailure(message: string, error: unknown): never {
	logger.error(message, error);
	throw error;
}

export async function enableMfaTotp(secret: string, code: string): Promise<Array<BackupCode>> {
	try {
		logger.debug('Enabling TOTP-based MFA');
		const backupCodes = await requestTotpEnable(secret, code);
		logger.debug('Successfully enabled TOTP-based MFA');
		Sudo.clearToken();
		return backupCodes;
	} catch (error) {
		rethrowMfaFailure('Failed to enable TOTP-based MFA:', error);
	}
}

export async function disableMfaTotp(code: string): Promise<void> {
	try {
		logger.debug('Disabling TOTP-based MFA');
		await requestTotpDisable(code);
		logger.debug('Successfully disabled TOTP-based MFA');
	} catch (error) {
		rethrowMfaFailure('Failed to disable TOTP-based MFA:', error);
	}
}

export async function getBackupCodes(regenerate = false): Promise<Array<BackupCode>> {
	const mode = backupCodeMode(regenerate);
	try {
		logger.debug(`${mode === 'regenerate' ? 'Regenerating' : 'Fetching'} MFA backup codes`);
		const backupCodes = await requestBackupCodes(regenerate);
		logger.debug(`Successfully ${mode === 'regenerate' ? 'regenerated' : 'fetched'} MFA backup codes`);
		return backupCodes;
	} catch (error) {
		rethrowMfaFailure(`Failed to ${mode} MFA backup codes:`, error);
	}
}

export async function startBackupCodesChallenge(): Promise<BackupCodesChallengeResponse> {
	try {
		logger.debug('Starting MFA backup codes challenge');
		const challenge = await requestBackupCodesChallengeStart();
		logger.debug('Successfully started MFA backup codes challenge');
		return challenge;
	} catch (error) {
		rethrowMfaFailure('Failed to start MFA backup codes challenge:', error);
	}
}

export async function resendBackupCodesChallengeCode(ticket: string): Promise<void> {
	try {
		logger.debug('Resending MFA backup codes challenge code');
		await requestBackupCodesChallengeResend(ticket);
		logger.debug('Successfully resent MFA backup codes challenge code');
	} catch (error) {
		rethrowMfaFailure('Failed to resend MFA backup codes challenge code:', error);
	}
}

export async function verifyBackupCodesChallenge(
	ticket: string,
	code: string,
): Promise<BackupCodesChallengeVerifyResponse> {
	try {
		logger.debug('Verifying MFA backup codes challenge code');
		const result = await requestBackupCodesChallengeVerify(ticket, code);
		logger.debug('Successfully verified MFA backup codes challenge code');
		return result;
	} catch (error) {
		rethrowMfaFailure('Failed to verify MFA backup codes challenge code:', error);
	}
}

export async function regenerateBackupCodesWithChallenge(challenge: BackupCodesChallenge): Promise<Array<BackupCode>> {
	try {
		logger.debug('Regenerating MFA backup codes with a verified challenge');
		const backupCodes = await requestBackupCodesChallengeRegenerate(challenge.ticket, challenge.verificationProof);
		logger.debug('Successfully regenerated MFA backup codes with a verified challenge');
		return backupCodes;
	} catch (error) {
		rethrowMfaFailure('Failed to regenerate MFA backup codes with a verified challenge:', error);
	}
}
