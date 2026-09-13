// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {Logger} from '@app/api/Logger';
import {AuthSessions} from '@app/api/Tables';
import {UserAccountRepository} from '@app/api/user/repositories/account/UserAccountRepository';
import {isJsonRecord, parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';

const PENDING_HASH_KEY = 'user_activity:pending';
const PENDING_AUTH_SESSION_HASH_KEY = 'auth_session_activity:pending';
const AUTH_SESSION_TOUCH_KEY_PREFIX = 'auth_session_activity:touched:';
const WRITE_CONCURRENCY = 64;
const AUTH_SESSION_TOUCH_DEBOUNCE_TTL_SECONDS = seconds('5 minutes');
type ActivityWriter = typeof upsertOne;

interface UserActivityAccountWriter {
	updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void>;
}

interface PendingEntry {
	ts: number;
	ip: string | null;
}

interface PendingAuthSessionEntry {
	ts: number;
}

interface FlushStats {
	drained: number;
	written: number;
	skipped: number;
}

function parsePendingEntry(json: string): PendingEntry | null {
	const decoded = parseJsonRecord(json);
	if (!decoded || typeof decoded.ts !== 'number' || !Number.isFinite(decoded.ts)) {
		return null;
	}
	return {
		ts: decoded.ts,
		ip: typeof decoded.ip === 'string' ? decoded.ip : null,
	};
}

function parsePendingAuthSessionEntry(json: string): PendingAuthSessionEntry | null {
	const decoded = parseJsonRecord(json);
	if (!decoded || typeof decoded.ts !== 'number' || !Number.isFinite(decoded.ts)) {
		return null;
	}
	return {ts: decoded.ts};
}

function isStringRecord(value: unknown): value is Record<string, string> {
	if (!isJsonRecord(value)) return false;
	return Object.values(value).every((entry) => typeof entry === 'string');
}

async function flushActivityEntries<T>(
	entries: ReadonlyArray<T>,
	write: (entry: T) => Promise<unknown>,
	failureMessage: string,
): Promise<FlushStats> {
	let written = 0;
	let skipped = 0;
	for (let index = 0; index < entries.length; index += WRITE_CONCURRENCY) {
		const results = await Promise.allSettled(
			entries.slice(index, index + WRITE_CONCURRENCY).map(async (entry) => write(entry)),
		);
		for (const result of results) {
			if (result.status === 'fulfilled') {
				written += 1;
			} else {
				skipped += 1;
				Logger.warn({error: result.reason}, failureMessage);
			}
		}
	}
	return {drained: entries.length, written, skipped};
}

export class UserActivityBuffer {
	private readonly kv: IKVProvider;
	private readonly writer: ActivityWriter;
	private readonly accounts: UserActivityAccountWriter;

	constructor(
		kv: IKVProvider,
		writer: ActivityWriter = upsertOne,
		accounts: UserActivityAccountWriter = new UserAccountRepository(kv),
	) {
		this.kv = kv;
		this.writer = writer;
		this.accounts = accounts;
	}

	recordActivity(userId: UserID, timestamp: Date, ip: string | null): void {
		const payload: PendingEntry = {ts: timestamp.getTime(), ip};
		void this.kv.hset(PENDING_HASH_KEY, userId.toString(), JSON.stringify(payload)).catch((error) => {
			Logger.debug({error, userId: userId.toString()}, 'Failed to enqueue user activity to buffer');
		});
	}

	async recordAuthSessionActivity(sessionIdHash: Buffer, timestamp: Date): Promise<boolean> {
		const encodedSessionIdHash = this.encodeSessionIdHash(sessionIdHash);
		const touchKey = `${AUTH_SESSION_TOUCH_KEY_PREFIX}${encodedSessionIdHash}`;
		let touched: string | null;
		try {
			touched = await this.kv.set(
				touchKey,
				timestamp.getTime().toString(),
				'EX',
				AUTH_SESSION_TOUCH_DEBOUNCE_TTL_SECONDS,
				'NX',
			);
		} catch (error) {
			Logger.debug({error}, 'Failed to debounce auth session activity');
			return false;
		}
		if (touched !== 'OK') {
			return false;
		}
		const payload: PendingAuthSessionEntry = {ts: timestamp.getTime()};
		try {
			await this.kv.hset(PENDING_AUTH_SESSION_HASH_KEY, encodedSessionIdHash, JSON.stringify(payload));
			return true;
		} catch (error) {
			Logger.debug({error}, 'Failed to enqueue auth session activity to buffer');
			void this.kv.del(touchKey).catch(() => undefined);
			return false;
		}
	}

	async drainAndFlush(): Promise<
		FlushStats & {
			users: FlushStats;
			authSessions: FlushStats;
		}
	> {
		const [users, authSessions] = await Promise.all([this.drainAndFlushUsers(), this.drainAndFlushAuthSessions()]);
		return {
			drained: users.drained + authSessions.drained,
			written: users.written + authSessions.written,
			skipped: users.skipped + authSessions.skipped,
			users,
			authSessions,
		};
	}

	private async drainAndFlushUsers(): Promise<FlushStats> {
		const drained = await this.atomicDrain();
		return flushActivityEntries(
			drained,
			({userId, entry}) =>
				this.accounts.updateLastActiveAt({
					userId,
					lastActiveAt: new Date(entry.ts),
					lastActiveIp: entry.ip ?? undefined,
				}),
			'Failed to flush a user activity entry',
		);
	}

	private async drainAndFlushAuthSessions(): Promise<FlushStats> {
		const drained = await this.atomicDrainAuthSessions();
		return flushActivityEntries(
			drained,
			({sessionIdHash, entry}) =>
				this.writer(
					AuthSessions.patchByPk({session_id_hash: sessionIdHash}, {approx_last_used_at: Db.set(new Date(entry.ts))}),
				),
			'Failed to flush an auth session activity entry',
		);
	}

	private async drainPendingHash(key: string): Promise<Array<[string, string]>> {
		const result = await this.kv.multi().hgetall(key).del(key).exec();
		const [readResult, deleteResult] = result;
		if (result.length !== 2 || !readResult || !deleteResult) {
			throw new Error(`Failed to drain activity hash ${key}: expected two transaction results`);
		}
		const [readError, pending] = readResult;
		const [deleteError, deleted] = deleteResult;
		if (readError) {
			throw new Error(`Failed to drain activity hash ${key}: HGETALL failed`, {cause: readError});
		}
		if (deleteError) {
			throw new Error(`Failed to drain activity hash ${key}: DEL failed`, {cause: deleteError});
		}
		if (!isStringRecord(pending)) {
			throw new Error(`Failed to drain activity hash ${key}: HGETALL returned an invalid hash`);
		}
		if (deleted !== 0 && deleted !== 1) {
			throw new Error(`Failed to drain activity hash ${key}: DEL returned an invalid deletion count`);
		}
		const entries = Object.entries(pending);
		if (entries.length > 0 && deleted !== 1) {
			throw new Error(`Failed to drain activity hash ${key}: DEL did not remove the non-empty hash`);
		}
		return entries;
	}

	private async atomicDrain(): Promise<Array<{userId: UserID; entry: PendingEntry}>> {
		const entries = await this.drainPendingHash(PENDING_HASH_KEY);
		const out: Array<{userId: UserID; entry: PendingEntry}> = [];
		for (const [userIdStr, json] of entries) {
			let userId: bigint;
			try {
				userId = BigInt(userIdStr);
			} catch {
				continue;
			}
			const parsed = parsePendingEntry(json);
			if (!parsed) continue;
			out.push({userId: userId as UserID, entry: parsed});
		}
		return out;
	}

	private async atomicDrainAuthSessions(): Promise<Array<{sessionIdHash: Buffer; entry: PendingAuthSessionEntry}>> {
		const entries = await this.drainPendingHash(PENDING_AUTH_SESSION_HASH_KEY);
		const out: Array<{sessionIdHash: Buffer; entry: PendingAuthSessionEntry}> = [];
		for (const [encodedSessionIdHash, json] of entries) {
			let sessionIdHash: Buffer;
			try {
				sessionIdHash = Buffer.from(encodedSessionIdHash, 'base64url');
			} catch {
				continue;
			}
			if (sessionIdHash.length === 0) {
				continue;
			}
			const parsed = parsePendingAuthSessionEntry(json);
			if (!parsed) continue;
			out.push({sessionIdHash, entry: parsed});
		}
		return out;
	}

	private encodeSessionIdHash(sessionIdHash: Buffer): string {
		return Buffer.from(sessionIdHash).toString('base64url');
	}
}
