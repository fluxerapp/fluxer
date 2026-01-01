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

import {randomBytes} from 'node:crypto';
import {APIErrorCodes} from '~/constants/API';
import {BadRequestError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';

const HANDOFF_CODE_EXPIRY_SECONDS = 5 * 60;
const HANDOFF_CODE_PREFIX = 'desktop-handoff:';
const HANDOFF_TOKEN_PREFIX = 'desktop-handoff-token:';

const CODE_CHARACTERS = 'ABCDEFGHJKMNPQRSTUVWXYZ23456789';
const CODE_LENGTH = 8;
const NORMALIZED_CODE_REGEX = /^[ABCDEFGHJKMNPQRSTUVWXYZ23456789]{8}$/;

interface HandoffData {
	createdAt: number;
	userAgent?: string;
}

interface HandoffTokenData {
	token: string;
	userId: string;
}

function generateHandoffCode(): string {
	const bytes = randomBytes(CODE_LENGTH);
	let code = '';
	for (let i = 0; i < CODE_LENGTH; i++) {
		code += CODE_CHARACTERS[bytes[i] % CODE_CHARACTERS.length];
	}
	return `${code.slice(0, 4)}-${code.slice(4, 8)}`;
}

function normalizeHandoffCode(code: string): string {
	return code.replace(/[-\s]/g, '').toUpperCase();
}

function assertValidHandoffCode(code: string): void {
	if (!NORMALIZED_CODE_REGEX.test(code)) {
		throw new BadRequestError({
			code: APIErrorCodes.INVALID_HANDOFF_CODE,
			message: 'Invalid handoff code format',
		});
	}
}

export class DesktopHandoffService {
	constructor(private readonly cacheService: ICacheService) {}

	async initiateHandoff(userAgent?: string): Promise<{code: string; expiresAt: Date}> {
		const code = generateHandoffCode();
		const normalizedCode = normalizeHandoffCode(code);

		const handoffData: HandoffData = {
			createdAt: Date.now(),
			userAgent,
		};

		await this.cacheService.set(`${HANDOFF_CODE_PREFIX}${normalizedCode}`, handoffData, HANDOFF_CODE_EXPIRY_SECONDS);

		const expiresAt = new Date(Date.now() + HANDOFF_CODE_EXPIRY_SECONDS * 1000);

		return {code, expiresAt};
	}

	async completeHandoff(code: string, token: string, userId: string): Promise<void> {
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		const handoffData = await this.cacheService.get<HandoffData>(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);

		if (!handoffData) {
			throw new BadRequestError({
				code: APIErrorCodes.INVALID_HANDOFF_CODE,
				message: 'Invalid or expired handoff code',
			});
		}

		const tokenData: HandoffTokenData = {
			token,
			userId,
		};

		const remainingSeconds = Math.max(
			0,
			HANDOFF_CODE_EXPIRY_SECONDS - Math.floor((Date.now() - handoffData.createdAt) / 1000),
		);

		if (remainingSeconds <= 0) {
			throw new BadRequestError({
				code: APIErrorCodes.HANDOFF_CODE_EXPIRED,
				message: 'Handoff code has expired',
			});
		}

		await this.cacheService.set(`${HANDOFF_TOKEN_PREFIX}${normalizedCode}`, tokenData, remainingSeconds);

		await this.cacheService.delete(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
	}

	async getHandoffStatus(
		code: string,
	): Promise<{status: 'pending' | 'completed' | 'expired'; token?: string; userId?: string}> {
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		const tokenData = await this.cacheService.getAndDelete<HandoffTokenData>(
			`${HANDOFF_TOKEN_PREFIX}${normalizedCode}`,
		);

		if (tokenData) {
			return {
				status: 'completed',
				token: tokenData.token,
				userId: tokenData.userId,
			};
		}

		const handoffData = await this.cacheService.get<HandoffData>(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);

		if (handoffData) {
			return {status: 'pending'};
		}

		return {status: 'expired'};
	}

	async cancelHandoff(code: string): Promise<void> {
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		await this.cacheService.delete(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		await this.cacheService.delete(`${HANDOFF_TOKEN_PREFIX}${normalizedCode}`);
	}
}
