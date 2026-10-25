// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import type {SessionOrigin} from '@app/api/auth/AuthSession';
import {randomBase64Url, secretMatchesHash, sha256Hex} from '@app/api/utils/SecretHash';
import {HandoffAttemptsExceededError} from '@fluxer/errors/src/domains/auth/HandoffAttemptsExceededError';
import {HandoffCodeAlreadyUsedError} from '@fluxer/errors/src/domains/auth/HandoffCodeAlreadyUsedError';
import {HandoffCodeExpiredError} from '@fluxer/errors/src/domains/auth/HandoffCodeExpiredError';
import {InvalidHandoffCodeError} from '@fluxer/errors/src/domains/auth/InvalidHandoffCodeError';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {
	DESKTOP_HANDOFF_CODE_ALPHABET,
	DESKTOP_HANDOFF_CODE_LENGTH,
	formatDesktopHandoffCode,
	parseDesktopHandoffCode,
} from '@fluxer/schema/src/domains/auth/DesktopHandoffCode';
import {ms, seconds} from 'itty-time';

const HANDOFF_CODE_PREFIX = 'desktop-handoff-v2:';
const HANDOFF_TOKEN_PREFIX = 'desktop-handoff-token:';
const HANDOFF_ATTEMPT_PREFIX = 'desktop-handoff-attempts:';
const HANDOFF_APPROVER_PREFIX = 'desktop-handoff-approver:';
const HANDOFF_USED_PREFIX = 'desktop-handoff-used:';
const MAX_FAILED_ATTEMPTS = 5;
const ATTEMPT_TTL_SECONDS = 900;
const MAX_INFO_LOOKUPS = 3;
const POLL_SECRET_BYTES = 32;
const GRANT_BYTES = 32;

interface HandoffData {
	createdAt: number;
	origin: SessionOrigin;
	initiatorOrigin?: string | null;
	infoLookupCount: number;
	pollSecretHash: string;
	returnUri?: string | null;
	denied?: boolean;
}

interface HandoffTokenData {
	token: string;
	userId: string;
	pollSecretHash: string;
	grantHash?: string;
}

export type DesktopHandoffReturnMethod = 'deep_link' | 'code';

interface HandoffApproverData {
	approvedAt: number;
}

interface HandoffUsedData {
	approverIpHash: string;
}

function generateNormalizedHandoffCode(): string {
	const maxUnbiased = 256 - (256 % DESKTOP_HANDOFF_CODE_ALPHABET.length);
	let code = '';
	while (code.length < DESKTOP_HANDOFF_CODE_LENGTH) {
		const bytes = randomBytes(DESKTOP_HANDOFF_CODE_LENGTH - code.length);
		for (let i = 0; i < bytes.length && code.length < DESKTOP_HANDOFF_CODE_LENGTH; i++) {
			if (bytes[i] < maxUnbiased) {
				code += DESKTOP_HANDOFF_CODE_ALPHABET[bytes[i] % DESKTOP_HANDOFF_CODE_ALPHABET.length];
			}
		}
	}
	return code;
}

function requireNormalizedHandoffCode(code: string): string {
	const normalized = parseDesktopHandoffCode(code);
	if (normalized == null) {
		throw new InvalidHandoffCodeError();
	}
	return normalized;
}

function resolveReturnMethod(handoffData: HandoffData): DesktopHandoffReturnMethod {
	return handoffData.returnUri ? 'deep_link' : 'code';
}

export class DesktopHandoffService {
	constructor(private readonly apiContext: ApiContext) {}

	async initiateHandoff(args: {
		origin: SessionOrigin;
		initiatorOrigin?: string | null;
		returnUri?: string | null;
	}): Promise<{
		code: string;
		expiresAt: Date;
		pollSecret: string;
		returnMethod: DesktopHandoffReturnMethod;
	}> {
		const {cache} = this.apiContext.services;
		const normalizedCode = generateNormalizedHandoffCode();
		const pollSecret = randomBase64Url(POLL_SECRET_BYTES);
		const handoffData: HandoffData = {
			createdAt: Date.now(),
			origin: args.origin,
			initiatorOrigin: args.initiatorOrigin ?? null,
			infoLookupCount: 0,
			pollSecretHash: sha256Hex(pollSecret),
			returnUri: args.returnUri ?? null,
		};
		const expirySeconds = seconds('5 minutes');
		await cache.set(`${HANDOFF_CODE_PREFIX}${normalizedCode}`, handoffData, expirySeconds);
		const expiresAt = new Date(Date.now() + ms('5 minutes'));
		return {
			code: formatDesktopHandoffCode(normalizedCode),
			expiresAt,
			pollSecret,
			returnMethod: resolveReturnMethod(handoffData),
		};
	}

	async completeHandoff(
		code: string,
		createTokenData: (origin: SessionOrigin) => Promise<{token: string; userId: string}>,
		approverIp: string,
		returnMethod: DesktopHandoffReturnMethod = 'code',
	): Promise<{initiatorOrigin: string | null; returnUrl: string | null}> {
		const {cache} = this.apiContext.services;
		const normalizedCode = requireNormalizedHandoffCode(code);
		await this.checkAttemptLimit(approverIp);
		const storedApprover = await cache.get<HandoffApproverData>(`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`);
		if (!storedApprover) {
			await this.rejectUnknownCode(normalizedCode, approverIp);
		}
		const handoffData = await cache.get<HandoffData>(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		if (!handoffData || handoffData.denied) {
			await this.recordFailedAttempt(approverIp);
			throw new InvalidHandoffCodeError();
		}
		if (returnMethod === 'deep_link' && !handoffData.returnUri) {
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
		const grant = returnMethod === 'deep_link' ? randomBase64Url(GRANT_BYTES) : null;
		const tokenData: HandoffTokenData = {
			token,
			userId,
			pollSecretHash: handoffData.pollSecretHash,
			...(grant ? {grantHash: sha256Hex(grant)} : {}),
		};
		await cache.set(`${HANDOFF_TOKEN_PREFIX}${normalizedCode}`, tokenData, remainingSeconds);
		await cache.delete(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		await cache.delete(`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`);
		await cache.set<HandoffUsedData>(
			`${HANDOFF_USED_PREFIX}${normalizedCode}`,
			{approverIpHash: sha256Hex(getSameIpDecisionKey(approverIp) ?? approverIp)},
			ATTEMPT_TTL_SECONDS,
		);
		let returnUrl: string | null = null;
		if (grant && handoffData.returnUri) {
			const params = new URLSearchParams({code: formatDesktopHandoffCode(normalizedCode), grant});
			returnUrl = `${handoffData.returnUri}?${params.toString()}`;
		}
		return {initiatorOrigin: handoffData.initiatorOrigin ?? null, returnUrl};
	}

	async getHandoffInfo(
		code: string,
		approverIp: string,
	): Promise<{
		status: 'pending' | 'expired';
		origin?: SessionOrigin;
		returnMethod?: DesktopHandoffReturnMethod;
	}> {
		const {cache} = this.apiContext.services;
		const normalizedCode = requireNormalizedHandoffCode(code);
		await this.checkAttemptLimit(approverIp);
		const codeKey = `${HANDOFF_CODE_PREFIX}${normalizedCode}`;
		const handoffData = await cache.get<HandoffData>(codeKey);
		if (!handoffData || handoffData.denied) {
			if (!handoffData && (await this.wasUsedBy(normalizedCode, approverIp))) {
				throw new HandoffCodeAlreadyUsedError();
			}
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
		return {status: 'pending', origin: handoffData.origin, returnMethod: resolveReturnMethod(handoffData)};
	}

	async denyHandoff(code: string, approverIp: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const normalizedCode = requireNormalizedHandoffCode(code);
		await this.checkAttemptLimit(approverIp);
		const approverKey = `${HANDOFF_APPROVER_PREFIX}${normalizedCode}`;
		const codeKey = `${HANDOFF_CODE_PREFIX}${normalizedCode}`;
		const storedApprover = await cache.get<HandoffApproverData>(approverKey);
		const handoffData = await cache.get<HandoffData>(codeKey);
		if (!storedApprover || !handoffData) {
			await this.recordFailedAttempt(approverIp);
			throw new InvalidHandoffCodeError();
		}
		const remainingTtl = await cache.ttl(codeKey);
		if (remainingTtl > 0) {
			await cache.set(codeKey, {...handoffData, denied: true}, remainingTtl);
		}
		await cache.delete(approverKey);
	}

	async getHandoffStatus(
		code: string,
		pollerIp: string,
		pollSecret: string | undefined,
		grant?: string,
	): Promise<{
		status: 'pending' | 'completed' | 'denied' | 'expired';
		token?: string;
		userId?: string;
	}> {
		const {cache} = this.apiContext.services;
		const normalizedCode = requireNormalizedHandoffCode(code);
		const tokenKey = `${HANDOFF_TOKEN_PREFIX}${normalizedCode}`;
		const tokenData = await cache.get<HandoffTokenData>(tokenKey);
		const handoffData = await cache.get<HandoffData>(`${HANDOFF_CODE_PREFIX}${normalizedCode}`);
		if (!secretMatchesHash(pollSecret, tokenData?.pollSecretHash ?? handoffData?.pollSecretHash)) {
			await this.checkAttemptLimit(pollerIp);
		}
		if (tokenData) {
			if (!secretMatchesHash(pollSecret, tokenData.pollSecretHash)) {
				await this.recordFailedAttempt(pollerIp);
				return {status: 'pending'};
			}
			if (tokenData.grantHash) {
				if (!grant) {
					return {status: 'pending'};
				}
				if (!secretMatchesHash(grant, tokenData.grantHash)) {
					await this.recordFailedAttempt(pollerIp);
					return {status: 'pending'};
				}
			}
			await cache.delete(tokenKey);
			return {
				status: 'completed',
				token: tokenData.token,
				userId: tokenData.userId,
			};
		}
		if (handoffData) {
			return {status: handoffData.denied ? 'denied' : 'pending'};
		}
		return {status: 'expired'};
	}

	async cancelHandoff(code: string, pollSecret: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const normalizedCode = requireNormalizedHandoffCode(code);
		const codeKey = `${HANDOFF_CODE_PREFIX}${normalizedCode}`;
		const tokenKey = `${HANDOFF_TOKEN_PREFIX}${normalizedCode}`;
		const handoffData = await cache.get<HandoffData>(codeKey);
		const tokenData = await cache.get<HandoffTokenData>(tokenKey);
		const storedHash = handoffData?.pollSecretHash ?? tokenData?.pollSecretHash;
		if (!secretMatchesHash(pollSecret, storedHash)) {
			throw new InvalidHandoffCodeError();
		}
		await cache.delete(codeKey);
		await cache.delete(tokenKey);
		await cache.delete(`${HANDOFF_APPROVER_PREFIX}${normalizedCode}`);
	}

	private async checkAttemptLimit(clientIp: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const key = `${HANDOFF_ATTEMPT_PREFIX}${clientIp}`;
		const count = await cache.get<number>(key);
		if (count != null && count >= MAX_FAILED_ATTEMPTS) {
			const remaining = await cache.ttl(key);
			throw new HandoffAttemptsExceededError(remaining > 0 ? remaining : ATTEMPT_TTL_SECONDS);
		}
	}

	private async wasUsedBy(normalizedCode: string, clientIp: string): Promise<boolean> {
		const {cache} = this.apiContext.services;
		const used = await cache.get<HandoffUsedData>(`${HANDOFF_USED_PREFIX}${normalizedCode}`);
		return used != null && secretMatchesHash(getSameIpDecisionKey(clientIp) ?? clientIp, used.approverIpHash);
	}

	private async rejectUnknownCode(normalizedCode: string, clientIp: string): Promise<never> {
		if (await this.wasUsedBy(normalizedCode, clientIp)) {
			throw new HandoffCodeAlreadyUsedError();
		}
		await this.recordFailedAttempt(clientIp);
		throw new InvalidHandoffCodeError();
	}

	private async recordFailedAttempt(clientIp: string): Promise<void> {
		const {cache} = this.apiContext.services;
		const key = `${HANDOFF_ATTEMPT_PREFIX}${clientIp}`;
		const current = await cache.get<number>(key);
		await cache.set(key, (current ?? 0) + 1, ATTEMPT_TTL_SECONDS);
	}
}
