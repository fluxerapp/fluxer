// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	assertChangeCooldown,
	checkChangeRateLimit,
	generateChangeVerificationCode,
} from '@app/api/user/services/UserChangeChallengeUtils';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {MfaBackupCodesResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ms} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import {requireEmailVerified} from '../../auth/EmailVerificationUtils';
import type {User} from '../../models/User';

interface MfaBackupCodesChallengeTicket {
	user_id: string;
	code: string;
	code_sent_at: number;
	code_expires_at: number;
}

interface StartMfaBackupCodesChallengeResult {
	ticket: string;
	code_expires_at: string;
	resend_available_at: string;
}

function getChallengeCacheKey(ticket: string): string {
	return `mfa-backup-codes-challenge:${ticket}`;
}

function normalizeVerificationCode(code: string): string {
	return code.trim().toUpperCase().replaceAll('-', '');
}

export class MfaBackupCodesChallengeService {
	private readonly CODE_TTL_MS = ms('10 minutes');
	private readonly TICKET_TTL_MS = ms('30 minutes');
	private readonly RESEND_COOLDOWN_MS = ms('30 seconds');

	constructor(private readonly apiContext: ApiContext) {}

	async start(user: User): Promise<StartMfaBackupCodesChallengeResult> {
		const {email, rateLimit} = this.apiContext.services;
		requireEmailVerified(user, 'mfa');
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
		}
		if (!user.totpSecret || !user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP)) {
			throw new MfaNotEnabledError();
		}
		await checkChangeRateLimit(rateLimit, {
			identifier: `mfa_backup_codes_challenge:start:${user.id}`,
			maxAttempts: 3,
			windowMs: ms('15 minutes'),
		});
		const ticket = randomUUID();
		const now = Date.now();
		const code = generateChangeVerificationCode();
		const codeExpiresAt = now + this.CODE_TTL_MS;
		await email.sendMfaBackupCodesVerification(user.email, user.username, code, user.locale);
		await this.storeTicket(ticket, {
			user_id: user.id.toString(),
			code,
			code_sent_at: now,
			code_expires_at: codeExpiresAt,
		});
		return {
			ticket,
			code_expires_at: new Date(codeExpiresAt).toISOString(),
			resend_available_at: new Date(now + this.RESEND_COOLDOWN_MS).toISOString(),
		};
	}

	async resend(user: User, ticket: string): Promise<void> {
		const {email, rateLimit} = this.apiContext.services;
		const state = await this.getTicketForUser(ticket, user);
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
		}
		assertChangeCooldown(new Date(state.code_sent_at), this.RESEND_COOLDOWN_MS);
		await checkChangeRateLimit(rateLimit, {
			identifier: `mfa_backup_codes_challenge:resend:${user.id}`,
			maxAttempts: 3,
			windowMs: ms('15 minutes'),
		});
		const now = Date.now();
		const code = generateChangeVerificationCode();
		const codeExpiresAt = now + this.CODE_TTL_MS;
		await email.sendMfaBackupCodesVerification(user.email, user.username, code, user.locale);
		await this.storeTicket(ticket, {
			user_id: state.user_id,
			code,
			code_sent_at: now,
			code_expires_at: codeExpiresAt,
		});
	}

	async verify(user: User, ticket: string, code: string): Promise<MfaBackupCodesResponse> {
		const {cache, rateLimit, users} = this.apiContext.services;
		const state = await this.getTicketForUser(ticket, user);
		await checkChangeRateLimit(rateLimit, {
			identifier: `mfa_backup_codes_challenge:verify:${ticket}`,
			maxAttempts: 5,
			windowMs: ms('15 minutes'),
		});
		if (state.code_expires_at < Date.now()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_EXPIRED);
		}
		if (normalizeVerificationCode(state.code) !== normalizeVerificationCode(code)) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_VERIFICATION_CODE);
		}
		await cache.delete(getChallengeCacheKey(ticket));
		const backupCodes = await users.listMfaBackupCodes(user.id);
		return {
			backup_codes: backupCodes.map((backupCode) => ({
				code: backupCode.code,
				consumed: backupCode.consumed,
			})),
		};
	}

	private async storeTicket(ticket: string, state: MfaBackupCodesChallengeTicket): Promise<void> {
		const {cache} = this.apiContext.services;
		await cache.set<MfaBackupCodesChallengeTicket>(
			getChallengeCacheKey(ticket),
			state,
			Math.ceil(this.TICKET_TTL_MS / 1000),
		);
	}

	private async getTicketForUser(ticket: string, user: User): Promise<MfaBackupCodesChallengeTicket> {
		const {cache} = this.apiContext.services;
		const state = await cache.get<MfaBackupCodesChallengeTicket>(getChallengeCacheKey(ticket));
		if (!state || state.user_id !== user.id.toString()) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_TICKET);
		}
		return state;
	}
}
