// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

const CHANGE_CODE_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
const CHANGE_CODE_LENGTH = 8;
const CHANGE_CODE_GROUP_LENGTH = 4;

interface UserChangeTicketRow {
	user_id: bigint;
	status: string;
}

interface UserChangeTicketRepository<TicketRow extends UserChangeTicketRow> {
	findTicket(ticket: string): Promise<TicketRow | null>;
}

export function generateChangeVerificationCode(): string {
	let raw = '';
	while (raw.length < CHANGE_CODE_LENGTH) {
		raw += CHANGE_CODE_ALPHABET[randomInt(CHANGE_CODE_ALPHABET.length)];
	}
	return `${raw.slice(0, CHANGE_CODE_GROUP_LENGTH)}-${raw.slice(CHANGE_CODE_GROUP_LENGTH)}`;
}

export function assertChangeCooldown(sentAt: Date | null | undefined, cooldownMs: number): void {
	if (!sentAt) {
		return;
	}
	const nextAllowed = sentAt.getTime() + cooldownMs;
	if (nextAllowed > Date.now()) {
		const retryAfter = Math.ceil((nextAllowed - Date.now()) / 1000);
		throw new RateLimitError({
			retryAfter,
			limit: 1,
			resetTime: new Date(nextAllowed),
		});
	}
}

export async function getActiveChangeTicketForUser<TicketRow extends UserChangeTicketRow>(
	repository: UserChangeTicketRepository<TicketRow>,
	ticket: string,
	userId: bigint,
): Promise<TicketRow> {
	const row = await repository.findTicket(ticket);
	if (!row || row.user_id !== userId) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_TICKET);
	}
	if (row.status === 'completed') {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.TICKET_ALREADY_COMPLETED);
	}
	return row;
}

export async function checkChangeRateLimit(
	rateLimit: IRateLimitService,
	params: {
		identifier: string;
		maxAttempts: number;
		windowMs: number;
	},
): Promise<void> {
	const result = await rateLimit.checkLimit(params);
	if (!result.allowed) {
		throw createRateLimitError(result);
	}
}
