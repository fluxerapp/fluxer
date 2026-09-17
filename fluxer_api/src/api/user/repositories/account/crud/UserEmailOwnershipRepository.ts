// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomUUID} from 'node:crypto';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {UserByEmailRow, UserEmailOwnerRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {UserByEmail, UserEmailOwners} from '@app/api/Tables';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

type EmailOwnerLookupRow = Pick<UserEmailOwnerRow, 'user_id' | 'claimed' | 'claimed_at'>;
type ValkeyEmailOwnerStatus = 'pending' | 'claimed';

interface ValkeyEmailOwnerState {
	version: 1;
	status: ValkeyEmailOwnerStatus;
	userId: string;
	token: string;
	epoch: number;
	claimedAt?: number | null;
	pendingUntil?: number | null;
	updatedAt: number;
}

interface ValkeyClaimResult {
	status: 'pending' | 'already_owner' | 'conflict';
	token?: string;
	epoch?: number;
	ownerUserId?: string;
}

interface ValkeyFinalizeResult {
	status: 'finalized' | 'missing' | 'mismatch';
	ownerUserId?: string;
}

interface ValkeyReleaseResult {
	status: 'released' | 'missing' | 'not_owner' | 'token_mismatch';
	ownerUserId?: string;
}

export interface EmailClaimReservation {
	emailLower: string;
	userId: UserID;
	token: string;
	newlyClaimed: boolean;
}

interface ScriptableKVProvider extends IKVProvider {
	evalScript(command: string, script: string, keyCount: number, ...args: Array<string | number>): Promise<unknown>;
}

const FETCH_EMAIL_OWNER_QUERY = UserEmailOwners.select({
	columns: ['user_id', 'claimed', 'claimed_at'],
	where: UserEmailOwners.where.eq('email_lower'),
	limit: 1,
});
const FETCH_LEGACY_EMAIL_USERS_QUERY = UserByEmail.select({
	columns: ['user_id'],
	where: UserByEmail.where.eq('email_lower'),
});

function parseOptionalUserId(value: unknown): UserID | null {
	if (typeof value === 'bigint') {
		return createUserID(value);
	}
	if (typeof value === 'number' || typeof value === 'string') {
		try {
			return createUserID(BigInt(value));
		} catch {
			return null;
		}
	}
	return null;
}

function normalizeEmail(email: string): string {
	return email.trim().toLowerCase();
}

function isValkeyClaimResult(value: unknown): value is ValkeyClaimResult {
	if (!isJsonRecord(value)) return false;
	if (value.status !== 'pending' && value.status !== 'already_owner' && value.status !== 'conflict') return false;
	return (
		(value.token === undefined || typeof value.token === 'string') &&
		(value.epoch === undefined || typeof value.epoch === 'number') &&
		(value.ownerUserId === undefined || typeof value.ownerUserId === 'string')
	);
}

function isValkeyFinalizeResult(value: unknown): value is ValkeyFinalizeResult {
	if (!isJsonRecord(value)) return false;
	return (
		(value.status === 'finalized' || value.status === 'missing' || value.status === 'mismatch') &&
		(value.ownerUserId === undefined || typeof value.ownerUserId === 'string')
	);
}

function isValkeyReleaseResult(value: unknown): value is ValkeyReleaseResult {
	if (!isJsonRecord(value)) return false;
	return (
		(value.status === 'released' ||
			value.status === 'missing' ||
			value.status === 'not_owner' ||
			value.status === 'token_mismatch') &&
		(value.ownerUserId === undefined || typeof value.ownerUserId === 'string')
	);
}

function isValkeyEmailOwnerState(value: unknown): value is ValkeyEmailOwnerState {
	if (!isJsonRecord(value)) return false;
	return (
		value.version === 1 &&
		(value.status === 'pending' || value.status === 'claimed') &&
		typeof value.userId === 'string' &&
		typeof value.token === 'string' &&
		typeof value.epoch === 'number' &&
		typeof value.updatedAt === 'number' &&
		(value.claimedAt === undefined || value.claimedAt === null || typeof value.claimedAt === 'number') &&
		(value.pendingUntil === undefined || value.pendingUntil === null || typeof value.pendingUntil === 'number')
	);
}

const EMAIL_OWNER_KEY_PREFIX = 'email-owner:v1';
const EMAIL_CLAIM_RESERVATION_GRACE_MS = 10 * 60 * 1000;
const EMAIL_OWNER_BEGIN_CLAIM_SCRIPT = `
local key = KEYS[1]
local userId = ARGV[1]
local token = ARGV[2]
local nowMs = tonumber(ARGV[3])
local pendingUntil = tonumber(ARGV[4])

local function encode(value)
	return cjson.encode(value)
end

local function pendingState(epoch)
	return {
		version = 1,
		status = 'pending',
		userId = userId,
		token = token,
		epoch = epoch,
		pendingUntil = pendingUntil,
		updatedAt = nowMs,
	}
end

local raw = redis.call('GET', key)
if raw then
	local ok, state = pcall(cjson.decode, raw)
	if ok and state then
		local ownerUserId = tostring(state.userId or '')
		local status = tostring(state.status or '')
		if status == 'claimed' then
			if ownerUserId == userId then
				return encode({status = 'already_owner', token = tostring(state.token or ''), epoch = tonumber(state.epoch) or 0})
			end
			return encode({status = 'conflict', ownerUserId = ownerUserId})
		end
		if status == 'pending' then
			local currentPendingUntil = tonumber(state.pendingUntil) or 0
			if currentPendingUntil > nowMs and ownerUserId ~= userId then
				return encode({status = 'conflict', ownerUserId = ownerUserId})
			end
			if currentPendingUntil > nowMs and ownerUserId == userId then
				state.pendingUntil = pendingUntil
				state.updatedAt = nowMs
				redis.call('SET', key, encode(state))
				return encode({status = 'pending', token = tostring(state.token or ''), epoch = tonumber(state.epoch) or 0})
			end
			local epoch = (tonumber(state.epoch) or 0) + 1
			redis.call('SET', key, encode(pendingState(epoch)))
			return encode({status = 'pending', token = token, epoch = epoch})
		end
	end
end

redis.call('SET', key, encode(pendingState(1)))
return encode({status = 'pending', token = token, epoch = 1})
`;
const EMAIL_OWNER_FINALIZE_CLAIM_SCRIPT = `
local key = KEYS[1]
local userId = ARGV[1]
local expectedToken = ARGV[2]
local nowMs = tonumber(ARGV[3])

local raw = redis.call('GET', key)
if not raw then
	return cjson.encode({status = 'missing'})
end

local ok, state = pcall(cjson.decode, raw)
if not ok or not state then
	return cjson.encode({status = 'mismatch'})
end

local ownerUserId = tostring(state.userId or '')
if ownerUserId ~= userId then
	return cjson.encode({status = 'mismatch', ownerUserId = ownerUserId})
end
if tostring(state.token or '') ~= expectedToken then
	return cjson.encode({status = 'mismatch', ownerUserId = ownerUserId})
end

local epoch = tonumber(state.epoch) or 1
redis.call('SET', key, cjson.encode({
	version = 1,
	status = 'claimed',
	userId = userId,
	token = expectedToken,
	epoch = epoch,
	claimedAt = nowMs,
	updatedAt = nowMs,
}))
return cjson.encode({status = 'finalized', ownerUserId = ownerUserId})
`;
const EMAIL_OWNER_RELEASE_SCRIPT = `
local key = KEYS[1]
local userId = ARGV[1]
local expectedToken = ARGV[2]

local raw = redis.call('GET', key)
if not raw then
	return cjson.encode({status = 'missing'})
end

local ok, state = pcall(cjson.decode, raw)
if not ok or not state then
	redis.call('DEL', key)
	return cjson.encode({status = 'released'})
end

local ownerUserId = tostring(state.userId or '')
if ownerUserId ~= userId then
	return cjson.encode({status = 'not_owner', ownerUserId = ownerUserId})
end
if expectedToken ~= '' and tostring(state.token or '') ~= expectedToken then
	return cjson.encode({status = 'token_mismatch', ownerUserId = ownerUserId})
end

redis.call('DEL', key)
return cjson.encode({status = 'released'})
`;

export class UserEmailOwnershipRepository {
	constructor(
		private readonly findUniqueUser: (userId: UserID) => Promise<User | null>,
		private readonly kv: IKVProvider,
	) {}

	async findOwnerId(email: string): Promise<UserID | null> {
		const emailLower = normalizeEmail(email);
		if (!emailLower) return null;
		const valkeyOwner = await this.resolveValkeyOwnerForLookup(emailLower);
		if (valkeyOwner.kind === 'owner') {
			return valkeyOwner.ownerId;
		}
		if (valkeyOwner.kind === 'blocked') {
			return null;
		}
		const projectedOwnerId = await this.findValidClaimedOwnerId(emailLower);
		if (projectedOwnerId) {
			await this.backfillOwnership(emailLower, projectedOwnerId);
			return projectedOwnerId;
		}
		const legacyOwnerId = await this.findLegacyOwnerId(emailLower);
		if (!legacyOwnerId) {
			return null;
		}
		await this.backfillOwnership(emailLower, legacyOwnerId);
		return legacyOwnerId;
	}

	async claimEmail(email: string, userId: UserID): Promise<EmailClaimReservation | null> {
		const emailLower = normalizeEmail(email);
		if (!emailLower) return null;
		const conflictingOwnerId = await this.findConflictingOwnerId(emailLower, userId);
		if (conflictingOwnerId) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
		}
		const claimResult = await this.claimValkeyOwner(emailLower, userId);
		if (claimResult.status === 'conflict') {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_ALREADY_IN_USE);
		}
		await this.acquireOwnership(emailLower, userId);
		return {
			emailLower,
			userId,
			token: claimResult.token ?? '',
			newlyClaimed: claimResult.status === 'pending',
		};
	}

	async finalizeEmailClaim(reservation: EmailClaimReservation | null): Promise<void> {
		if (!reservation?.newlyClaimed) {
			return;
		}
		const result = await this.finalizeValkeyOwner(reservation.emailLower, reservation.userId, reservation.token);
		if (result.status !== 'finalized') {
			throw new Error(`Failed to finalize email claim for ${reservation.emailLower}: ${result.status}`);
		}
	}

	async abortEmailClaim(reservation: EmailClaimReservation | null): Promise<void> {
		if (!reservation?.newlyClaimed) {
			return;
		}
		await this.releaseValkeyOwner(reservation.emailLower, reservation.userId, reservation.token);
		await this.releaseClaimRow(reservation.emailLower, reservation.userId);
	}

	async releaseEmail(email: string, userId: UserID): Promise<void> {
		const emailLower = normalizeEmail(email);
		if (!emailLower) return;
		await this.releaseValkeyOwner(emailLower, userId);
		await this.releaseClaimRow(emailLower, userId);
	}

	private async claimValkeyOwner(emailLower: string, userId: UserID): Promise<ValkeyClaimResult> {
		const result = await this.runJsonScript(
			'emailOwnerClaim',
			EMAIL_OWNER_BEGIN_CLAIM_SCRIPT,
			1,
			this.ownerKey(emailLower),
			userId.toString(),
			randomUUID(),
			Date.now(),
			Date.now() + EMAIL_CLAIM_RESERVATION_GRACE_MS,
		);
		if (!isValkeyClaimResult(result)) {
			throw new Error(`Unexpected email owner claim result: ${JSON.stringify(result)}`);
		}
		return result;
	}

	private async finalizeValkeyOwner(emailLower: string, userId: UserID, token: string): Promise<ValkeyFinalizeResult> {
		const result = await this.runJsonScript(
			'emailOwnerFinalize',
			EMAIL_OWNER_FINALIZE_CLAIM_SCRIPT,
			1,
			this.ownerKey(emailLower),
			userId.toString(),
			token,
			Date.now(),
		);
		if (!isValkeyFinalizeResult(result)) {
			throw new Error(`Unexpected email owner finalize result: ${JSON.stringify(result)}`);
		}
		return result;
	}

	private async releaseValkeyOwner(emailLower: string, userId: UserID, token = ''): Promise<ValkeyReleaseResult> {
		const result = await this.runJsonScript(
			'emailOwnerRelease',
			EMAIL_OWNER_RELEASE_SCRIPT,
			1,
			this.ownerKey(emailLower),
			userId.toString(),
			token,
		);
		if (!isValkeyReleaseResult(result)) {
			throw new Error(`Unexpected email owner release result: ${JSON.stringify(result)}`);
		}
		return result;
	}

	private async runJsonScript(
		command: string,
		script: string,
		keyCount: number,
		...args: Array<string | number>
	): Promise<unknown> {
		const kv = this.scriptableKV();
		const result = await kv.evalScript(command, script, keyCount, ...args);
		const parsed: unknown = JSON.parse(String(result));
		return parsed;
	}

	private scriptableKV(): ScriptableKVProvider {
		const maybeScriptable = this.kv as Partial<ScriptableKVProvider>;
		if (typeof maybeScriptable.evalScript !== 'function') {
			throw new Error('Email ownership requires a KV provider with email-owner script support');
		}
		return maybeScriptable as ScriptableKVProvider;
	}

	private ownerKey(emailLower: string): string {
		const digest = createHash('sha256').update(emailLower).digest('hex');
		return `${EMAIL_OWNER_KEY_PREFIX}:${digest}`;
	}

	private async fetchValkeyOwnerState(emailLower: string): Promise<ValkeyEmailOwnerState | null> {
		const raw = await this.kv.get(this.ownerKey(emailLower));
		if (!raw) {
			return null;
		}
		return parseValkeyOwnerState(raw);
	}

	private async resolveValkeyOwnerForLookup(
		emailLower: string,
	): Promise<{kind: 'owner'; ownerId: UserID} | {kind: 'blocked'} | {kind: 'none'}> {
		const state = await this.fetchValkeyOwnerState(emailLower);
		if (!state) {
			return {kind: 'none'};
		}
		const ownerId = parseOptionalUserId(state.userId);
		if (!ownerId) {
			return {kind: 'blocked'};
		}
		const owner = await this.findUniqueUser(ownerId);
		if (state.status !== 'claimed') {
			if (owner?.email?.toLowerCase() === emailLower) {
				await this.finalizeValkeyOwner(emailLower, owner.id, state.token).catch((error) => {
					Logger.warn(
						{email: emailLower, ownerId, error},
						'[UserEmailOwnershipRepository] Failed to lazily finalize pending email ownership state',
					);
				});
				return {kind: 'owner', ownerId: owner.id};
			}
			if (this.isValkeyReservationRecent(state)) {
				return {kind: 'blocked'};
			}
			await this.releaseValkeyOwner(emailLower, ownerId, state.token);
			return {kind: 'none'};
		}
		if (owner?.email?.toLowerCase() === emailLower) {
			return {kind: 'owner', ownerId: owner.id};
		}
		if (this.isValkeyReservationRecent(state)) {
			return {kind: 'blocked'};
		}
		Logger.warn(
			{email: emailLower, ownerId},
			'[UserEmailOwnershipRepository] Clearing stale Valkey email ownership state that no longer matches the canonical user record',
		);
		await this.releaseValkeyOwner(emailLower, ownerId, state.token);
		await this.releaseClaimRow(emailLower, ownerId);
		return {kind: 'none'};
	}

	private async releaseClaimRow(emailLower: string, expectedOwnerId?: UserID): Promise<void> {
		const ownerRow = await this.fetchOwnerRow(emailLower);
		const currentOwnerId = parseOptionalUserId(ownerRow?.user_id);
		if (ownerRow?.claimed !== true || currentOwnerId === null) {
			return;
		}
		if (expectedOwnerId !== undefined && currentOwnerId !== expectedOwnerId) {
			Logger.error(
				{email: emailLower, expectedOwnerId, actualOwnerId: currentOwnerId},
				'[UserEmailOwnershipRepository] Refusing to release email claim owned by a different user',
			);
			return;
		}
		await upsertOne(
			UserEmailOwners.patchByPk(
				{email_lower: emailLower},
				{
					user_id: Db.clear(),
					claimed_at: Db.clear(),
					claimed: Db.set(false),
				},
			),
		);
	}

	private async acquireOwnership(emailLower: string, userId: UserID): Promise<void> {
		await upsertOne(
			UserEmailOwners.upsertAll({
				email_lower: emailLower,
				user_id: userId,
				claimed_at: new Date(),
				claimed: true,
			}),
		);
	}

	private async findValidClaimedOwnerId(emailLower: string): Promise<UserID | null> {
		const ownerRow = await this.fetchOwnerRow(emailLower);
		if (ownerRow?.claimed !== true) {
			return null;
		}
		const ownerId = parseOptionalUserId(ownerRow.user_id);
		if (!ownerId) {
			return null;
		}
		const owner = await this.findUniqueUser(ownerId);
		if (owner?.email?.toLowerCase() === emailLower) {
			return owner.id;
		}
		if (this.isReservationRecent(ownerRow)) {
			return null;
		}
		Logger.warn(
			{email: emailLower, ownerId},
			'[UserEmailOwnershipRepository] Clearing stale email ownership row that no longer matches the canonical user record',
		);
		await this.releaseClaimRow(emailLower, ownerId);
		return null;
	}

	private async findConflictingOwnerId(emailLower: string, claimantUserId: UserID): Promise<UserID | null> {
		const valkeyState = await this.fetchValkeyOwnerState(emailLower);
		if (valkeyState) {
			const valkeyOwnerId = parseOptionalUserId(valkeyState.userId);
			if (valkeyOwnerId && valkeyOwnerId !== claimantUserId) {
				const owner = await this.findUniqueUser(valkeyOwnerId);
				if (owner?.email?.toLowerCase() === emailLower || this.isValkeyReservationRecent(valkeyState)) {
					return valkeyOwnerId;
				}
				await this.releaseValkeyOwner(emailLower, valkeyOwnerId, valkeyState.token);
				await this.releaseClaimRow(emailLower, valkeyOwnerId);
			}
		}
		const ownerRow = await this.fetchOwnerRow(emailLower);
		if (ownerRow?.claimed === true) {
			const ownerId = parseOptionalUserId(ownerRow.user_id);
			if (ownerId && ownerId !== claimantUserId) {
				const owner = await this.findUniqueUser(ownerId);
				if (owner?.email?.toLowerCase() === emailLower || this.isReservationRecent(ownerRow)) {
					return ownerId;
				}
				await this.releaseClaimRow(emailLower, ownerId);
			}
		}
		return await this.findLegacyOwnerId(emailLower, claimantUserId);
	}

	private async fetchOwnerRow(emailLower: string): Promise<EmailOwnerLookupRow | null> {
		return await fetchOne<EmailOwnerLookupRow>(FETCH_EMAIL_OWNER_QUERY.bind({email_lower: emailLower}));
	}

	private isReservationRecent(row: EmailOwnerLookupRow): boolean {
		if (!row.claimed_at) {
			return false;
		}
		return Date.now() - row.claimed_at.getTime() < EMAIL_CLAIM_RESERVATION_GRACE_MS;
	}

	private isValkeyReservationRecent(state: ValkeyEmailOwnerState): boolean {
		if (state.status === 'pending') {
			return (state.pendingUntil ?? 0) > Date.now();
		}
		if (!state.claimedAt) {
			return false;
		}
		return Date.now() - state.claimedAt < EMAIL_CLAIM_RESERVATION_GRACE_MS;
	}

	private async findLegacyOwnerId(emailLower: string, ignoredUserId?: UserID): Promise<UserID | null> {
		const rows = await fetchMany<Pick<UserByEmailRow, 'user_id'>>(
			FETCH_LEGACY_EMAIL_USERS_QUERY.bind({email_lower: emailLower}),
		);
		if (rows.length === 0) {
			return null;
		}
		const candidateIds = Array.from(new Set(rows.map((row) => row.user_id.toString()))).map((userId) =>
			createUserID(BigInt(userId)),
		);
		const users = await Promise.all(candidateIds.map((userId) => this.findUniqueUser(userId)));
		const matchingUsers = users.filter(
			(user): user is User => user?.email?.toLowerCase() === emailLower && user.id !== ignoredUserId,
		);
		if (matchingUsers.length > 1) {
			Logger.error(
				{email: emailLower, ownerIds: matchingUsers.map((user) => user.id)},
				'[UserEmailOwnershipRepository] Found multiple canonical users for the same email while resolving legacy ownership',
			);
		}
		return matchingUsers[0]?.id ?? null;
	}

	private async backfillOwnership(emailLower: string, userId: UserID): Promise<void> {
		try {
			const claimResult = await this.claimValkeyOwner(emailLower, userId);
			if (claimResult.status !== 'conflict') {
				if (claimResult.status === 'pending' && claimResult.token) {
					await this.finalizeValkeyOwner(emailLower, userId, claimResult.token);
				}
				await this.acquireOwnership(emailLower, userId);
			}
		} catch (error) {
			Logger.warn(
				{email: emailLower, userId, error},
				'[UserEmailOwnershipRepository] Failed to backfill email ownership from legacy lookup rows',
			);
		}
	}
}

function parseValkeyOwnerState(raw: string): ValkeyEmailOwnerState | null {
	return parseJsonWithGuard(raw, isValkeyEmailOwnerState);
}
