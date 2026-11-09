// SPDX-License-Identifier: AGPL-3.0-or-later

export const EMAIL_OWNER_COMMANDS = {
	claim: 'emailOwnerClaim',
	finalize: 'emailOwnerFinalize',
	release: 'emailOwnerRelease',
} as const;

export const PURGE_BUCKET_TTL_SECONDS = 3600;

const MAX_DATE_TIMESTAMP = 8640000000000000;

export type StringWrite = {kind: 'keep'} | {kind: 'delete'} | {kind: 'set'; value: string; ttlMs: number | null};

export interface StringTransition {
	write: StringWrite;
	reply: string;
}

export interface BulkDeletionClaim {
	claimed: boolean;
	score: number | null;
}

export interface BulkDeletionRemoval {
	removed: boolean;
	member: string | null;
	deleteSecondary: boolean;
}

export interface PurgeBucketState {
	tokens: number;
	lastRefill: number;
}

export interface PurgeBatchPlan {
	toPop: number;
	bucket: PurgeBucketState | null;
}

export interface PurgeBatchSettlement {
	bucketValue: string | null;
	tokensConsumed: number;
}

type ScriptRecord = Record<string, unknown>;

function decodeRecord(raw: string): ScriptRecord | null {
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		return null;
	}
	if (typeof parsed === 'object' && parsed !== null && !Array.isArray(parsed)) {
		return parsed as ScriptRecord;
	}
	return {};
}

function luaString(value: unknown): string {
	if (typeof value === 'string') return value;
	if (typeof value === 'number') return luaNumberString(value);
	if (typeof value === 'boolean') return value ? 'true' : 'false';
	return '';
}

function luaNumberString(value: number): string {
	if (Number.isInteger(value) && Math.abs(value) < 1e14) return String(value);
	const formatted = value.toPrecision(14);
	if (formatted.includes('e')) {
		const [mantissa, exponent] = formatted.split('e');
		const trimmed = mantissa.includes('.') ? mantissa.replace(/\.?0+$/, '') : mantissa;
		const sign = exponent.startsWith('-') ? '-' : '+';
		const digits = exponent.replace(/^[+-]/, '').padStart(2, '0');
		return `${trimmed}e${sign}${digits}`;
	}
	return formatted.includes('.') ? formatted.replace(/\.?0+$/, '') : formatted;
}

function luaNumber(value: unknown): number | null {
	if (typeof value === 'number') return Number.isNaN(value) ? null : value;
	if (typeof value !== 'string' || value.trim().length === 0) return null;
	const parsed = Number(value);
	return Number.isNaN(parsed) ? null : parsed;
}

function reply(value: ScriptRecord): string {
	return JSON.stringify(value);
}

export function emailOwnerClaim(
	current: string | null,
	userId: string,
	token: string,
	nowMs: number,
	pendingUntil: number,
): StringTransition {
	const pendingState = (epoch: number): string =>
		JSON.stringify({version: 1, status: 'pending', userId, token, epoch, pendingUntil, updatedAt: nowMs});
	const state = current === null ? null : decodeRecord(current);
	if (state) {
		const ownerUserId = luaString(state.userId);
		const status = luaString(state.status);
		if (status === 'claimed') {
			if (ownerUserId === userId) {
				return {
					write: {kind: 'keep'},
					reply: reply({status: 'already_owner', token: luaString(state.token), epoch: luaNumber(state.epoch) ?? 0}),
				};
			}
			return {write: {kind: 'keep'}, reply: reply({status: 'conflict', ownerUserId})};
		}
		if (status === 'pending') {
			const currentPendingUntil = luaNumber(state.pendingUntil) ?? 0;
			if (currentPendingUntil > nowMs && ownerUserId !== userId) {
				return {write: {kind: 'keep'}, reply: reply({status: 'conflict', ownerUserId})};
			}
			if (currentPendingUntil > nowMs && ownerUserId === userId) {
				return {
					write: {kind: 'set', value: JSON.stringify({...state, pendingUntil, updatedAt: nowMs}), ttlMs: null},
					reply: reply({status: 'pending', token: luaString(state.token), epoch: luaNumber(state.epoch) ?? 0}),
				};
			}
			const epoch = (luaNumber(state.epoch) ?? 0) + 1;
			return {
				write: {kind: 'set', value: pendingState(epoch), ttlMs: null},
				reply: reply({status: 'pending', token, epoch}),
			};
		}
	}
	return {
		write: {kind: 'set', value: pendingState(1), ttlMs: null},
		reply: reply({status: 'pending', token, epoch: 1}),
	};
}

export function emailOwnerFinalize(
	current: string | null,
	userId: string,
	expectedToken: string,
	nowMs: number,
): StringTransition {
	if (current === null) {
		return {write: {kind: 'keep'}, reply: reply({status: 'missing'})};
	}
	const state = decodeRecord(current);
	if (!state) {
		return {write: {kind: 'keep'}, reply: reply({status: 'mismatch'})};
	}
	const ownerUserId = luaString(state.userId);
	if (ownerUserId !== userId || luaString(state.token) !== expectedToken) {
		return {write: {kind: 'keep'}, reply: reply({status: 'mismatch', ownerUserId})};
	}
	const value = JSON.stringify({
		version: 1,
		status: 'claimed',
		userId,
		token: expectedToken,
		epoch: luaNumber(state.epoch) ?? 1,
		claimedAt: nowMs,
		updatedAt: nowMs,
	});
	return {write: {kind: 'set', value, ttlMs: null}, reply: reply({status: 'finalized', ownerUserId})};
}

export function emailOwnerRelease(current: string | null, userId: string, expectedToken: string): StringTransition {
	if (current === null) {
		return {write: {kind: 'keep'}, reply: reply({status: 'missing'})};
	}
	const state = decodeRecord(current);
	if (!state) {
		return {write: {kind: 'delete'}, reply: reply({status: 'released'})};
	}
	const ownerUserId = luaString(state.userId);
	if (ownerUserId !== userId) {
		return {write: {kind: 'keep'}, reply: reply({status: 'not_owner', ownerUserId})};
	}
	if (expectedToken !== '' && luaString(state.token) !== expectedToken) {
		return {write: {kind: 'keep'}, reply: reply({status: 'token_mismatch', ownerUserId})};
	}
	return {write: {kind: 'delete'}, reply: reply({status: 'released'})};
}

export function emailOwnerTransition(
	command: string,
	current: string | null,
	args: ReadonlyArray<string | number>,
): StringTransition {
	switch (command) {
		case EMAIL_OWNER_COMMANDS.claim:
			return emailOwnerClaim(current, String(args[0]), String(args[1]), Number(args[2]), Number(args[3]));
		case EMAIL_OWNER_COMMANDS.finalize:
			return emailOwnerFinalize(current, String(args[0]), String(args[1]), Number(args[2]));
		case EMAIL_OWNER_COMMANDS.release:
			return emailOwnerRelease(current, String(args[0]), String(args[1] ?? ''));
		default:
			throw new Error(`Unsupported KV script command: ${command}`);
	}
}

export function claimBulkDeletion(
	currentScore: number | null,
	maxScore: number,
	leaseScore: number,
): BulkDeletionClaim {
	if (currentScore === null || currentScore > maxScore) {
		return {claimed: false, score: currentScore};
	}
	return {claimed: true, score: leaseScore};
}

export function removeBulkDeletion(memberRemoved: boolean, secondaryValue: string | null): BulkDeletionRemoval {
	if (memberRemoved) {
		return {removed: true, member: null, deleteSecondary: true};
	}
	if (secondaryValue === null) {
		return {removed: false, member: null, deleteSecondary: false};
	}
	return {removed: true, member: secondaryValue, deleteSecondary: true};
}

function decodePurgeBucketState(raw: string): PurgeBucketState {
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		throw new Error('invalid stored bucket state');
	}
	if (typeof parsed !== 'object' || parsed === null) {
		throw new Error('invalid stored bucket state');
	}
	const {tokens, lastRefill} = parsed as ScriptRecord;
	if (typeof tokens !== 'number' || Number.isNaN(tokens) || tokens < 0 || tokens === Number.POSITIVE_INFINITY) {
		throw new Error('invalid stored bucket amount: tokens');
	}
	if (typeof lastRefill !== 'number' || !Number.isInteger(lastRefill) || Math.abs(lastRefill) > MAX_DATE_TIMESTAMP) {
		throw new Error('invalid stored bucket timestamp: lastRefill');
	}
	return {tokens, lastRefill};
}

export function planPurgeBatch(
	rawBucket: string | null,
	queueSize: number,
	maxItems: number,
	maxTokens: number,
	refillRate: number,
	refillIntervalMs: number,
	nowMs: number,
): PurgeBatchPlan {
	if (queueSize === 0) {
		return {toPop: 0, bucket: null};
	}
	let {tokens, lastRefill} =
		rawBucket === null ? {tokens: maxTokens, lastRefill: nowMs} : decodePurgeBucketState(rawBucket);
	const elapsed = nowMs - lastRefill;
	if (elapsed >= refillIntervalMs) {
		const intervals = Math.floor(elapsed / refillIntervalMs);
		tokens = Math.min(maxTokens, tokens + intervals * refillRate);
		lastRefill = nowMs;
	}
	return {toPop: Math.max(0, Math.min(maxItems, Math.floor(tokens), queueSize)), bucket: {tokens, lastRefill}};
}

export function settlePurgeBatch(plan: PurgeBatchPlan, popped: number): PurgeBatchSettlement {
	if (plan.bucket === null) {
		return {bucketValue: null, tokensConsumed: 0};
	}
	return {
		bucketValue: JSON.stringify({tokens: plan.bucket.tokens - popped, lastRefill: plan.bucket.lastRefill}),
		tokensConsumed: popped,
	};
}
