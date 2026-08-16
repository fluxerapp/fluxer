// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	assertChangeCooldown,
	checkChangeRateLimit,
	generateChangeVerificationCode,
	getActiveChangeTicketForUser,
} from '@app/api/user/services/UserChangeChallengeUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {SsoRequiredError} from '@fluxer/errors/src/domains/auth/SsoRequiredError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ms} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import * as AuthPassword from '../../auth/AuthPassword';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import type {User} from '../../models/User';
import type {PasswordChangeRepository} from '../repositories/auth/PasswordChangeRepository';

interface StartPasswordChangeResult {
	ticket: string;
	code_expires_at: string;
	resend_available_at: string;
}

interface VerifyPasswordChangeResult {
	verification_proof: string;
}

export class PasswordChangeService {
	private readonly CODE_TTL_MS = ms('10 minutes');
	private readonly RESEND_COOLDOWN_MS = ms('30 seconds');

	constructor(
		private readonly apiContext: ApiContext,
		private readonly repo: PasswordChangeRepository,
	) {}

	async start(user: User): Promise<StartPasswordChangeResult> {
		if (user.traits.has('sso')) {
			const ssoConfig = await getInstanceConfigRepository().getSsoConfig();
			if ((ssoConfig.enabled && ssoConfig.enforced) || ssoConfig.disableAdditionalAuth) {
				throw new SsoRequiredError();
			}
		}
		const {email, rateLimit} = this.apiContext.services;
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.MUST_HAVE_EMAIL_TO_CHANGE_IT);
		}
		await checkChangeRateLimit(rateLimit, {
			identifier: `password_change:start:${user.id}`,
			maxAttempts: 3,
			windowMs: ms('15 minutes'),
		});
		const ticket = randomUUID();
		const now = new Date();
		const code = generateChangeVerificationCode();
		const codeExpiresAt = new Date(now.getTime() + this.CODE_TTL_MS);
		await email.sendPasswordChangeVerification(user.email, user.username, code, user.locale);
		await this.repo.createTicket({
			ticket,
			user_id: user.id,
			code,
			code_sent_at: now,
			code_expires_at: codeExpiresAt,
			verified: false,
			verification_proof: null,
			status: 'pending',
			created_at: now,
			updated_at: now,
		});
		return {
			ticket,
			code_expires_at: codeExpiresAt.toISOString(),
			resend_available_at: new Date(now.getTime() + this.RESEND_COOLDOWN_MS).toISOString(),
		};
	}

	async resend(user: User, ticket: string): Promise<void> {
		const {email, rateLimit} = this.apiContext.services;
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.MUST_HAVE_EMAIL_TO_CHANGE_IT);
		}
		assertChangeCooldown(row.code_sent_at, this.RESEND_COOLDOWN_MS);
		await checkChangeRateLimit(rateLimit, {
			identifier: `password_change:resend:${user.id}`,
			maxAttempts: 3,
			windowMs: ms('15 minutes'),
		});
		const now = new Date();
		const code = generateChangeVerificationCode();
		const codeExpiresAt = new Date(now.getTime() + this.CODE_TTL_MS);
		await email.sendPasswordChangeVerification(user.email, user.username, code, user.locale);
		row.code = code;
		row.code_sent_at = now;
		row.code_expires_at = codeExpiresAt;
		row.updated_at = now;
		await this.repo.updateTicket(row);
	}

	async verify(user: User, ticket: string, code: string): Promise<VerifyPasswordChangeResult> {
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (row.verified && row.verification_proof) {
			return {verification_proof: row.verification_proof};
		}
		if (!row.code || !row.code_expires_at) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_NOT_ISSUED);
		}
		if (row.code_expires_at.getTime() < Date.now()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VERIFICATION_CODE_EXPIRED);
		}
		if (row.code !== code.trim()) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_VERIFICATION_CODE);
		}
		const now = new Date();
		const verificationProof = randomUUID();
		row.verified = true;
		row.verification_proof = verificationProof;
		row.status = 'verified';
		row.updated_at = now;
		await this.repo.updateTicket(row);
		return {verification_proof: verificationProof};
	}

	async complete(user: User, ticket: string, verificationProof: string, newPassword: string): Promise<void> {
		const {users} = this.apiContext.services;
		const row = await getActiveChangeTicketForUser(this.repo, ticket, user.id);
		if (!row.verified || !row.verification_proof) {
			throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_TICKET);
		}
		if (row.verification_proof !== verificationProof) {
			throw InputValidationError.fromCode('verification_proof', ValidationErrorCodes.INVALID_PROOF_TOKEN);
		}
		if (await AuthPassword.isPasswordPwned(this.apiContext, newPassword)) {
			throw InputValidationError.fromCode('new_password', ValidationErrorCodes.PASSWORD_IS_TOO_COMMON);
		}
		const newPasswordHash = await AuthPassword.hashPassword(this.apiContext, newPassword);
		await users.patchUpsert(user.id, {
			password_hash: newPasswordHash,
			password_last_changed_at: new Date(),
		});
		const now = new Date();
		row.status = 'completed';
		row.updated_at = now;
		await this.repo.updateTicket(row);
	}
}
