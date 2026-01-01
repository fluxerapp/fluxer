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

import crypto from 'node:crypto';
import {InputValidationError, RateLimitError} from '~/Errors';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import {Logger} from '~/Logger';
import type {User} from '~/Models';
import type {EmailChangeRepository} from '../repositories/auth/EmailChangeRepository';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';

export interface StartEmailChangeResult {
	ticket: string;
	require_original: boolean;
	original_email?: string | null;
	original_proof?: string | null;
	original_code_expires_at?: string;
	resend_available_at?: string | null;
}

export interface VerifyOriginalResult {
	original_proof: string;
}

export interface RequestNewEmailResult {
	ticket: string;
	new_email: string;
	new_code_expires_at: string;
	resend_available_at: string | null;
}

export class EmailChangeService {
	private readonly ORIGINAL_CODE_TTL_MS = 10 * 60 * 1000;
	private readonly NEW_CODE_TTL_MS = 10 * 60 * 1000;
	private readonly TOKEN_TTL_MS = 30 * 60 * 1000;
	private readonly RESEND_COOLDOWN_MS = 30 * 1000;

	constructor(
		private readonly repo: EmailChangeRepository,
		private readonly emailService: IEmailService,
		private readonly userAccountRepository: IUserAccountRepository,
		private readonly rateLimitService: IRateLimitService,
	) {}

	async start(user: User): Promise<StartEmailChangeResult> {
		const isUnclaimed = !user.passwordHash;
		const hasEmail = !!user.email;
		if (!hasEmail && !isUnclaimed) {
			throw InputValidationError.create('email', 'You must have an email to change it.');
		}

		const ticket = this.generateTicket();
		const requireOriginal = !!user.emailVerified && hasEmail;
		const now = new Date();

		let originalCode: string | null = null;
		let originalCodeExpiresAt: Date | null = null;
		let originalCodeSentAt: Date | null = null;

		if (requireOriginal) {
			await this.ensureRateLimit(`email_change:orig:${user.id}`, 3, 15 * 60 * 1000);
			originalCode = this.generateCode();
			originalCodeExpiresAt = new Date(now.getTime() + this.ORIGINAL_CODE_TTL_MS);
			originalCodeSentAt = now;
			await this.emailService.sendEmailChangeOriginal(user.email!, user.username, originalCode, user.locale);
		}

		const originalProof = requireOriginal ? null : this.generateProof();

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
			original_email: user.email,
			original_proof: originalProof,
			original_code_expires_at: originalCodeExpiresAt?.toISOString(),
			resend_available_at: requireOriginal ? new Date(now.getTime() + this.RESEND_COOLDOWN_MS).toISOString() : null,
		};
	}

	async resendOriginal(user: User, ticket: string): Promise<void> {
		const row = await this.getTicketForUser(ticket, user.id);
		if (!row.require_original || row.original_verified) {
			throw InputValidationError.create('ticket', 'Original email already verified.');
		}
		if (!row.original_email) {
			throw InputValidationError.create('ticket', 'No original email on record.');
		}

		this.assertCooldown(row.original_code_sent_at);
		await this.ensureRateLimit(`email_change:orig:${user.id}`, 3, 15 * 60 * 1000);

		const now = new Date();
		const originalCode = this.generateCode();
		const originalCodeExpiresAt = new Date(now.getTime() + this.ORIGINAL_CODE_TTL_MS);

		await this.emailService.sendEmailChangeOriginal(row.original_email, user.username, originalCode, user.locale);

		row.original_code = originalCode;
		row.original_code_sent_at = now;
		row.original_code_expires_at = originalCodeExpiresAt;
		row.updated_at = now;
		await this.repo.updateTicket(row);
	}

	async verifyOriginal(user: User, ticket: string, code: string): Promise<VerifyOriginalResult> {
		const row = await this.getTicketForUser(ticket, user.id);
		if (!row.require_original) {
			throw InputValidationError.create('ticket', 'Original verification not required for this flow.');
		}
		if (row.original_verified && row.original_proof) {
			return {original_proof: row.original_proof};
		}
		if (!row.original_code || !row.original_code_expires_at) {
			throw InputValidationError.create('code', 'Verification code not issued.');
		}
		if (row.original_code_expires_at.getTime() < Date.now()) {
			throw InputValidationError.create('code', 'Verification code expired.');
		}
		if (row.original_code !== code.trim()) {
			throw InputValidationError.create('code', 'Invalid verification code.');
		}

		const now = new Date();
		const originalProof = this.generateProof();
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
	): Promise<RequestNewEmailResult> {
		const row = await this.getTicketForUser(ticket, user.id);
		if (!row.original_verified || !row.original_proof) {
			throw InputValidationError.create('ticket', 'Original email must be verified first.');
		}
		if (row.original_proof !== originalProof) {
			throw InputValidationError.create('original_proof', 'Invalid proof token.');
		}
		const trimmedEmail = newEmail.trim();
		if (!trimmedEmail) {
			throw InputValidationError.create('new_email', 'Email is required.');
		}
		if (row.original_email && trimmedEmail.toLowerCase() === row.original_email.toLowerCase()) {
			throw InputValidationError.create('new_email', 'New email must be different.');
		}
		const existing = await this.userAccountRepository.findByEmail(trimmedEmail.toLowerCase());
		if (existing && existing.id !== user.id) {
			throw InputValidationError.create('new_email', 'Email already in use.');
		}

		this.assertCooldown(row.new_code_sent_at);
		await this.ensureRateLimit(`email_change:new:${user.id}`, 5, 15 * 60 * 1000);

		const now = new Date();
		const newCode = this.generateCode();
		const newCodeExpiresAt = new Date(now.getTime() + this.NEW_CODE_TTL_MS);

		await this.emailService.sendEmailChangeNew(trimmedEmail, user.username, newCode, user.locale);

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
		const row = await this.getTicketForUser(ticket, user.id);
		if (!row.new_email) {
			throw InputValidationError.create('ticket', 'No new email requested.');
		}
		this.assertCooldown(row.new_code_sent_at);
		await this.ensureRateLimit(`email_change:new:${user.id}`, 5, 15 * 60 * 1000);

		const now = new Date();
		const newCode = this.generateCode();
		const newCodeExpiresAt = new Date(now.getTime() + this.NEW_CODE_TTL_MS);

		await this.emailService.sendEmailChangeNew(row.new_email, user.username, newCode, user.locale);

		row.new_code = newCode;
		row.new_code_sent_at = now;
		row.new_code_expires_at = newCodeExpiresAt;
		row.updated_at = now;
		await this.repo.updateTicket(row);
	}

	async verifyNew(user: User, ticket: string, code: string, originalProof: string): Promise<string> {
		const row = await this.getTicketForUser(ticket, user.id);
		if (!row.original_verified || !row.original_proof) {
			throw InputValidationError.create('ticket', 'Original email must be verified first.');
		}
		if (row.original_proof !== originalProof) {
			throw InputValidationError.create('original_proof', 'Invalid proof token.');
		}
		if (!row.new_email || !row.new_code || !row.new_code_expires_at) {
			throw InputValidationError.create('code', 'Verification code not issued.');
		}
		if (row.new_code_expires_at.getTime() < Date.now()) {
			throw InputValidationError.create('code', 'Verification code expired.');
		}
		if (row.new_code !== code.trim()) {
			throw InputValidationError.create('code', 'Invalid verification code.');
		}

		const now = new Date();
		const token = this.generateToken();
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

	async consumeToken(userId: bigint, token: string): Promise<string> {
		const row = await this.repo.findToken(token);
		if (!row || row.user_id !== userId) {
			throw InputValidationError.create('email_token', 'Invalid email token.');
		}
		if (row.expires_at.getTime() < Date.now()) {
			await this.repo.deleteToken(token).catch((error) => Logger.warn({error}, 'Failed to delete expired email token'));
			throw InputValidationError.create('email_token', 'Email token expired.');
		}
		await this.repo.deleteToken(token);
		return row.new_email;
	}

	private async getTicketForUser(ticket: string, userId: bigint) {
		const row = await this.repo.findTicket(ticket);
		if (!row || row.user_id !== userId) {
			throw InputValidationError.create('ticket', 'Invalid or expired ticket.');
		}
		if (row.status === 'completed') {
			throw InputValidationError.create('ticket', 'Ticket already completed.');
		}
		return row;
	}

	private generateCode(): string {
		const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
		let raw = '';
		while (raw.length < 8) {
			const byte = crypto.randomBytes(1)[0];
			const idx = byte % alphabet.length;
			raw += alphabet[idx];
		}
		return `${raw.slice(0, 4)}-${raw.slice(4, 8)}`;
	}

	private generateTicket(): string {
		return crypto.randomUUID();
	}

	private generateToken(): string {
		return crypto.randomUUID();
	}

	private generateProof(): string {
		return crypto.randomUUID();
	}

	private assertCooldown(sentAt: Date | null | undefined) {
		if (!sentAt) return;
		const nextAllowed = sentAt.getTime() + this.RESEND_COOLDOWN_MS;
		if (nextAllowed > Date.now()) {
			const retryAfter = Math.ceil((nextAllowed - Date.now()) / 1000);
			throw new RateLimitError({
				message: 'Please wait before resending.',
				retryAfter,
				limit: 1,
				resetTime: new Date(nextAllowed),
			});
		}
	}

	private async ensureRateLimit(identifier: string, maxAttempts: number, windowMs: number) {
		const result = await this.rateLimitService.checkLimit({identifier, maxAttempts, windowMs});
		if (!result.allowed) {
			throw new RateLimitError({
				message: 'Too many attempts. Please try again later.',
				retryAfter: result.retryAfter || 0,
				limit: maxAttempts,
				resetTime: new Date(Date.now() + (result.retryAfter || 0) * 1000),
			});
		}
	}
}
