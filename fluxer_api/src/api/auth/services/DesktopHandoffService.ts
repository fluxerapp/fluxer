// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {HandoffCodeExpiredError} from '@fluxer/errors/src/domains/auth/HandoffCodeExpiredError';
import {InvalidHandoffCodeError} from '@fluxer/errors/src/domains/auth/InvalidHandoffCodeError';
import {ms, seconds} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import type {SessionOrigin} from '../AuthSession';

const HANDOFF_CODE_PREFIX = 'desktop-handoff-v2:';
const HANDOFF_TOKEN_PREFIX = 'desktop-handoff-token:';
const CODE_CHARACTERS = 'ABCDEFGHJKMNPQRSTUVWXYZ23456789';
const CODE_LENGTH = 12;
const NORMALIZED_CODE_REGEX = /^[ABCDEFGHJKMNPQRSTUVWXYZ23456789]{12}$/;
const HANDOFF_ATTEMPT_PREFIX = 'desktop-handoff-attempts:';
const HANDOFF_APPROVER_PREFIX = 'desktop-handoff-approver:';
const MAX_FAILED_ATTEMPTS = 5;
const ATTEMPT_TTL_SECONDS = 900;
const MAX_INFO_LOOKUPS = 3;

interface HandoffData {
	createdAt: number;
	origin: SessionOrigin;
	infoLookupCount: number;
}

interface HandoffTokenData {
	token: string;
	userId: string;
}

interface HandoffApproverData {
	approvedAt: number;
}

function generateHandoffCode(): string {
	const maxUnbiased = 256 - (256 % CODE_CHARACTERS.length);
	let code = '';
	while (code.length < CODE_LENGTH) {
		const bytes = randomBytes(CODE_LENGTH - code.length);
		for (let i = 0; i < bytes.length && code.length < CODE_LENGTH; i++) {
			if (bytes[i] < maxUnbiased) {
				code += CODE_CHARACTERS[bytes[i] % CODE_CHARACTERS.length];
			}
		}
	}
	return `${code.slice(0, 6)}-${code.slice(6, 12)}`;
}

function normalizeHandoffCode(code: string): string {
	return code.replace(/[-\s]/g, '').toUpperCase();
}

function assertValidHandoffCode(code: string): void {
	if (!NORMALIZED_CODE_REGEX.test(code)) {
		throw new InvalidHandoffCodeError();
	}
}

export class DesktopHandoffService {
	constructor(private readonly apiContext: ApiContext) {}

	async initiateHandoff(args: {origin: SessionOrigin}): Promise<{
		code: string;
		expiresAt: Date;
	}> {
		const {cache} = this.apiContext.services;
		const code = generateHandoffCode();
		const normalizedCode = normalizeHandoffCode(code);
		const handoffData: HandoffData = {
			createdAt: Date.now(),
			origin: args.origin,
			infoLookupCount: 0,
		};
		const expirySeconds = seconds('5 minutes');
		await cache.set(`${HANDOFF_CODE_PREFIX}${normalizedCode}`, handoffData, expirySeconds);
		const expiresAt = new Date(Date.now() + ms('5 minutes'));
		return {code, expiresAt};
	}

	async completeHandoff(
		code: string,
		createTokenData: (origin: SessionOrigin) => Promise<{token: string; userId: string}>,
		approverIp: string,
	): Promise<void> {
		const {cache} = this.apiContext.services;
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		await this.checkAttemptLimit(approverIp);
		const storedApprover = await cache.get<HandoffApproverData>(`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`);
		if (!storedApprover) {
			await this.recordFailedAttempt(approverIp);
			throw new InvalidHandoffCodeError();
		}
		const handoffData = await cache.get<HandoffData>(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		if (!handoffData) {
			await this.recordFailedAttempt(approverIp);
			throw new InvalidHandoffCodeError();
		}
		const remainingSeconds = Math.max(
			0,
			seconds('5 minutes') - Math.floor((Date.now() - handoffData.createdAt) / 1000),
		);
		if (remainingSeconds <= 0) {
			throw new HandoffCodeExpiredError();
		}
		const {token, userId} = await createTokenData(handoffData.origin);
		const tokenData: HandoffTokenData = {
			token,
			userId,
		};
		await cache.set(`${HANDOFF_TOKEN_PREFIX}${normalizedCode}`, tokenData, remainingSeconds);
		await cache.delete(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		await cache.delete(`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`);
	}

	async getHandoffInfo(
		code: string,
		approverIp: string,
	): Promise<{
		status: 'pending' | 'expired';
		origin?: SessionOrigin;
	}> {
		const {cache} = this.apiContext.services;
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		await this.checkAttemptLimit(approverIp);
		const codeKey = `${HANDOFF_CODE_PREFIX}${normalizedCode}`;
		const handoffData = await cache.get<HandoffData>(codeKey);
		if (!handoffData) {
			await this.recordFailedAttempt(approverIp);
			return {status: 'expired'};
		}
		if (handoffData.infoLookupCount >= MAX_INFO_LOOKUPS) {
			throw new InvalidHandoffCodeError();
		}
		const remainingTtl = await cache.ttl(codeKey);
		if (remainingTtl > 0) {
			handoffData.infoLookupCount += 1;
			await cache.set(codeKey, handoffData, remainingTtl);
		}
		await cache.set<HandoffApproverData>(
			`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`,
			{approvedAt: Date.now()},
			remainingTtl > 0 ? remainingTtl : seconds('5 minutes'),
		);
		return {status: 'pending', origin: handoffData.origin};
	}

	async getHandoffStatus(
		code: string,
		_pollerIp: string,
	): Promise<{
		status: 'pending' | 'completed' | 'expired';
		token?: string;
		userId?: string;
	}> {
		const {cache} = this.apiContext.services;
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		const tokenKey = `${HANDOFF_TOKEN_PREFIX}${normalizedCode}`;
		const tokenData = await cache.get<HandoffTokenData>(tokenKey);
		if (tokenData) {
			await cache.delete(tokenKey);
			return {
				status: 'completed',
				token: tokenData.token,
				userId: tokenData.userId,
			};
		}
		const handoffData = await cache.get<HandoffData>(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		if (handoffData) {
			return {status: 'pending'};
		}
		return {status: 'expired'};
	}

	async cancelHandoff(code: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const normalizedCode = normalizeHandoffCode(code);
		assertValidHandoffCode(normalizedCode);
		await cache.delete(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		await cache.delete(`${HANDOFF_TOKEN_PREFIX}${normalizedCode}`);
		await cache.delete(`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`);
	}

	private async checkAttemptLimit(clientIp: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const count = await cache.get<number>(`${HANDOFF_ATTEMPT_PREFIX}${clientIp}`);
		if (count != null && count >= MAX_FAILED_ATTEMPTS) {
			throw new InvalidHandoffCodeError();
		}
	}

	private async recordFailedAttempt(clientIp: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const key = `${HANDOFF_ATTEMPT_PREFIX}${clientIp}`;
		const current = await cache.get<number>(key);
		await cache.set(key, (current ?? 0) + 1, ATTEMPT_TTL_SECONDS);
	}
}
