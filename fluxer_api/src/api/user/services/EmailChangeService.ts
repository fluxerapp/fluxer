// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	assertChangeCooldown,
	checkChangeRateLimit,
	generateChangeVerificationCode,
	getActiveChangeTicketForUser,
} from '@app/api/user/services/UserChangeChallengeUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ms} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import {EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS} from '../../auth/AuthEmail';
import * as AuthPassword from '../../auth/AuthPassword';
import type {User} from '../../models/User';
import type {EmailChangeRepository} from '../repositories/auth/EmailChangeRepository';

interface StartEmailChangeResult {
	ticket: string;
	require_original: boolean;
	original_email?: string | null;
	original_proof?: string | null;
	original_code_expires_at?: string | null;
	resend_available_at?: string | null;
}

interface VerifyOriginalResult {
	original_proof: string;
}

interface RequestNewEmailResult {
	ticket: string;
	new_email: string;
	new_code_expires_at: string;
	resend_available_at: string | null;
}

export class EmailChangeService {
	private readonly ORIGINAL_CODE_TTL_MS = ms('10 minutes');
	private readonly NEW_CODE_TTL_MS = ms('10 minutes');
	private readonly TOKEN_TTL_MS = ms('30 minutes');
	private readonly RESEND_COOLDOWN_MS = ms('30 seconds');

	constructor(
		private readonly apiContext: ApiContext,
		private readonly repo: EmailChangeRepository,
	) {}

	async start(user: User): Promise<StartEmailChangeResult> {
		const {email, rateLimit} = this.apiContext.services;
		const isUnclaimed = user.isUnclaimedAccount();
		const hasEmail = !!user.email;
		if (!hasEmail && !isUnclaimed) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.MUST_HAVE_EMAIL_TO_CHANGE_IT);
		}
		const ticket = randomUUID();
		const requireOriginal = !!user.emailVerified && hasEmail;
		const now = new Date();
		let originalCode: string | null = null;
		let originalCodeExpiresAt: Date | null = null;
		let originalCodeSentAt: Date | null = null;
		if (requireOriginal) {
			await checkChangeRateLimit(rateLimit, {
				identifier: `email_change:orig:${user.id}`,
				maxAttempts: 3,
				windowMs: ms('15 minutes'),
			});
			originalCode = generateChangeVerificationCode();
			originalCodeExpiresAt = new Date(now.getTime() + this.ORIGINAL_CODE_TTL_MS);
			originalCodeSentAt = now;
			await email.sendEmailChangeOriginal(user.email!, user.username, originalCode, user.locale);
		}
		const originalProof = requireOriginal ? null : randomUUID();
		await this.repo.createTicket({
			ticket,
			user_id: user.id,
			require_original: requireOriginal,
			original_email: user.email,
			original_verified: !requireOriginal,
			original_proof: originalProof,
			original_code: originalCode,
			original_code_sent_at: originalCodeSentAt,
			original_code_expires_at: originalCodeExpiresAt,
			new_email: null,
			new_code: null,
			new_code_sent_at: null,
			new_code_expires_at: null,
			status: requireOriginal ? 'pending_original' : 'pending_new',
			created_at: now,
			updated_at: now,
		});
		return {
			ticket,
			require_original: requireOriginal,
			original_email: user.email ?? null,
			original_proof: originalProof,
			original_code_expires_at: originalCodeExpiresAt ? originalCodeExpiresAt.toISOString() : null,
			resend_available_at: requireOriginal ? new Date(now.getTime() + this.RESEND_COOLDOWN_MS).toISOString() : null,
		};
	}

	async resendOriginal(user: User, ticket: string): Promise<void> {
		const {email, rateLimit} = this.apiContext.services;
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!row.require_original || row.original_verified) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.ORIGINAL_EMAIL_ALREADY_VERIFIED);
		}
		if (!row.original_email) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.NO_ORIGINAL_EMAIL_ON_RECORD);
		}
		assertChangeCooldown(row.original_code_sent_at, this.RESEND_COOLDOWN_MS);
		await checkChangeRateLimit(rateLimit, {
			identifier: `email_change:orig:${user.id}`,
			maxAttempts: 3,
			windowMs: ms('15 minutes'),
		});
		const now = new Date();
		const originalCode = generateChangeVerificationCode();
		const originalCodeExpiresAt = new Date(now.getTime() + this.ORIGINAL_CODE_TTL_MS);
		await email.sendEmailChangeOriginal(row.original_email, user.username, originalCode, user.locale);
		row.original_code = originalCode;
		row.original_code_sent_at = now;
		row.original_code_expires_at = originalCodeExpiresAt;
		row.updated_at = now;
		await this.repo.updateTicket(row);
	}

	async verifyOriginal(user: User, ticket: string, code: string): Promise<VerifyOriginalResult> {
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!row.require_original) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.ORIGINAL_VERIFICATION_NOT_REQUIRED);
		}
		if (row.original_verified && row.original_proof) {
			return {original_proof: row.original_proof};
		}
		if (!row.original_code || !row.original_code_expires_at) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_NOT_ISSUED);
		}
		if (row.original_code_expires_at.getTime() < Date.now()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_EXPIRED);
		}
		if (row.original_code !== code.trim()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_VERIFICATION_CODE);
		}
		const now = new Date();
		const originalProof = randomUUID();
		row.original_verified = true;
		row.original_proof = originalProof;
		row.status = 'pending_new';
		row.updated_at = now;
		await this.repo.updateTicket(row);
		return {original_proof: originalProof};
	}

	async requestNewEmail(
		user: User,
		ticket: string,
		newEmail: string,
		originalProof: string,
		newPassword?: string,
	): Promise<RequestNewEmailResult> {
		const {email, emailDnsValidation, users, rateLimit} = this.apiContext.services;
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!row.original_verified || !row.original_proof) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST);
		}
		if (row.original_proof !== originalProof) {
			throw InputValidationError.fromCode('original_proof', ValidationErrorCodes.INVALID_PROOF_TOKEN);
		}
		const trimmedEmail = newEmail.trim();
		if (!trimmedEmail) {
			throw InputValidationError.fromCode('new_email', ValidationErrorCodes.EMAIL_IS_REQUIRED);
		}
		if (row.original_email && trimmedEmail.toLowerCase() === row.original_email.toLowerCase()) {
			throw InputValidationError.fromCode('new_email', ValidationErrorCodes.NEW_EMAIL_MUST_BE_DIFFERENT);
		}
		const hasValidDns = await emailDnsValidation.hasValidDnsRecords(trimmedEmail);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('new_email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}
		const existing = await users.findByEmail(trimmedEmail.toLowerCase());
		if (existing && existing.id !== user.id) {
			throw InputValidationError.fromCode('new_email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
		}
		if (newPassword != null && (await AuthPassword.isPasswordPwned(this.apiContext, newPassword))) {
			throw InputValidationError.fromCode('new_password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
		}
		assertChangeCooldown(row.new_code_sent_at, this.RESEND_COOLDOWN_MS);
		await checkChangeRateLimit(rateLimit, {
			identifier: `email_change:new:${user.id}`,
			maxAttempts: 5,
			windowMs: ms('15 minutes'),
		});
		const now = new Date();
		const newCode = generateChangeVerificationCode();
		const newCodeExpiresAt = new Date(now.getTime() + this.NEW_CODE_TTL_MS);
		await email.sendEmailChangeNew(trimmedEmail, user.username, newCode, user.locale);
		row.new_email = trimmedEmail;
		row.new_code = newCode;
		row.new_code_sent_at = now;
		row.new_code_expires_at = newCodeExpiresAt;
		row.status = 'pending_new';
		row.updated_at = now;
		await this.repo.updateTicket(row);
		return {
			ticket,
			new_email: trimmedEmail,
			new_code_expires_at: newCodeExpiresAt.toISOString(),
			resend_available_at: new Date(now.getTime() + this.RESEND_COOLDOWN_MS).toISOString(),
		};
	}

	async resendNew(user: User, ticket: string): Promise<void> {
		const {email, rateLimit} = this.apiContext.services;
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!row.new_email) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.NO_NEW_EMAIL_REQUESTED);
		}
		assertChangeCooldown(row.new_code_sent_at, this.RESEND_COOLDOWN_MS);
		await checkChangeRateLimit(rateLimit, {
			identifier: `email_change:new:${user.id}`,
			maxAttempts: 5,
			windowMs: ms('15 minutes'),
		});
		const now = new Date();
		const newCode = generateChangeVerificationCode();
		const newCodeExpiresAt = new Date(now.getTime() + this.NEW_CODE_TTL_MS);
		await email.sendEmailChangeNew(row.new_email, user.username, newCode, user.locale);
		row.new_code = newCode;
		row.new_code_sent_at = now;
		row.new_code_expires_at = newCodeExpiresAt;
		row.updated_at = now;
		await this.repo.updateTicket(row);
	}

	async verifyNew(user: User, ticket: string, code: string, originalProof: string): Promise<string> {
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!row.original_verified || !row.original_proof) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST);
		}
		if (row.original_proof !== originalProof) {
			throw InputValidationError.fromCode('original_proof', ValidationErrorCodes.INVALID_PROOF_TOKEN);
		}
		if (!row.new_email || !row.new_code || !row.new_code_expires_at) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_NOT_ISSUED);
		}
		if (row.new_code_expires_at.getTime() < Date.now()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_EXPIRED);
		}
		if (row.new_code !== code.trim()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_VERIFICATION_CODE);
		}
		const now = new Date();
		const token = randomUUID();
		const expiresAt = new Date(now.getTime() + this.TOKEN_TTL_MS);
		await this.repo.createToken({
			token_: token,
			user_id: user.id,
			new_email: row.new_email,
			expires_at: expiresAt,
			created_at: now,
		});
		row.status = 'completed';
		row.updated_at = now;
		await this.repo.updateTicket(row);
		return token;
	}

	async getTokenEmail(userId: bigint, token: string): Promise<string> {
		const row = await this.repo.findToken(token);
		if (!row || row.user_id !== userId) {
			throw InputValidationError.fromCode('email_token', ValidationErrorCodes.INVALID_EMAIL_TOKEN);
		}
		if (row.expires_at.getTime() < Date.now()) {
			await this.repo.deleteToken(token);
			throw InputValidationError.fromCode('email_token', ValidationErrorCodes.EMAIL_TOKEN_EXPIRED);
		}
		return row.new_email;
	}

	async deleteToken(token: string): Promise<void> {
		await this.repo.deleteToken(token);
	}

	async requestBouncedNewEmail(user: User, newEmail: string): Promise<RequestNewEmailResult> {
		this.ensureBouncedEmailRecoveryAllowed(user);
		const startResult = await this.start(user);
		if (startResult.require_original || !startResult.original_proof) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST);
		}
		return await this.requestNewEmail(user, startResult.ticket, newEmail, startResult.original_proof);
	}

	async resendBouncedNew(user: User, ticket: string): Promise<void> {
		this.ensureBouncedEmailRecoveryAllowed(user);
		await this.resendNew(user, ticket);
	}

	async verifyBouncedNew(user: User, ticket: string, code: string): Promise<User> {
		const {users} = this.apiContext.services;
		this.ensureBouncedEmailRecoveryAllowed(user);
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (row.require_original || !row.original_proof) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.ORIGINAL_EMAIL_MUST_BE_VERIFIED_FIRST);
		}
		const emailToken = await this.verifyNew(user, ticket, code, row.original_proof);
		const updatedEmail = await this.getTokenEmail(user.id, emailToken);
		const updates: {
			email: string;
			email_verified: boolean;
			email_bounced: boolean;
			suspicious_activity_flags?: number;
		} = {
			email: updatedEmail,
			email_verified: true,
			email_bounced: false,
		};
		if (user.suspiciousActivityFlags !== null && user.suspiciousActivityFlags !== 0) {
			const newFlags = user.suspiciousActivityFlags & ~EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS;
			if (newFlags !== user.suspiciousActivityFlags) {
				updates.suspicious_activity_flags = newFlags;
			}
		}
		const updatedUser = await users.patchUpsert(user.id, updates, user.toRow());
		await this.deleteToken(emailToken);
		return updatedUser;
	}

	private ensureBouncedEmailRecoveryAllowed(user: User): void {
		if (!user.emailBounced) {
			throw new AccessDeniedError();
		}
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.MUST_HAVE_EMAIL_TO_CHANGE_IT);
		}
	}
}
