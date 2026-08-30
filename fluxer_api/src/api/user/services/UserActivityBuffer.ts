// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';
import type {UserID} from '../../BrandedTypes';
import {upsertOne} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import {Logger} from '../../Logger';
import {AuthSessions} from '../../Tables';
import {isJsonRecord, parseJsonRecord} from '../../utils/JsonBoundaryUtils';
import {UserAccountRepository} from '../repositories/account/UserAccountRepository';

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
		if (drained.length === 0) {
			return {drained: 0, written: 0, skipped: 0};
		}
		let written = 0;
		let skipped = 0;
		for (let i = 0; i < drained.length; i += WRITE_CONCURRENCY) {
			const chunk = drained.slice(i, i + WRITE_CONCURRENCY);
			const results = await Promise.allSettled(
				chunk.map(({userId, entry}) =>
					this.accounts.updateLastActiveAt({
						userId,
						lastActiveAt: new Date(entry.ts),
						lastActiveIp: entry.ip ?? undefined,
					}),
				),
			);
			for (const r of results) {
				if (r.status === 'fulfilled') {
					written += 1;
				} else {
					skipped += 1;
					Logger.warn({error: r.reason}, 'Failed to flush a user activity entry');
				}
			}
		}
		return {drained: drained.length, written, skipped};
	}

	private async drainAndFlushAuthSessions(): Promise<FlushStats> {
		const drained = await this.atomicDrainAuthSessions();
		if (drained.length === 0) {
			return {drained: 0, written: 0, skipped: 0};
		}
		let written = 0;
		let skipped = 0;
		for (let i = 0; i < drained.length; i += WRITE_CONCURRENCY) {
			const chunk = drained.slice(i, i + WRITE_CONCURRENCY);
			const results = await Promise.allSettled(
				chunk.map(async ({sessionIdHash, entry}) => {
					const approximateLastUsedAt = new Date(entry.ts);
					await this.writer(
						AuthSessions.patchByPk(
							{session_id_hash: sessionIdHash},
							{approx_last_used_at: Db.set(approximateLastUsedAt)},
						),
					);
				}),
			);
			for (const r of results) {
				if (r.status === 'fulfilled') {
					written += 1;
				} else {
					skipped += 1;
					Logger.warn({error: r.reason}, 'Failed to flush an auth session activity entry');
				}
			}
		}
		return {drained: drained.length, written, skipped};
	}

	private async atomicDrain(): Promise<Array<{userId: UserID; entry: PendingEntry}>> {
		const result = await this.kv.multi().hgetall(PENDING_HASH_KEY).del(PENDING_HASH_KEY).exec();
		if (!result || result.length === 0) {
			return [];
		}
		const hgetallResult = result[0];
		if (!hgetallResult || hgetallResult[0]) {
			return [];
		}
		const raw = hgetallResult[1];
		if (!isStringRecord(raw)) {
			return [];
		}
		const out: Array<{userId: UserID; entry: PendingEntry}> = [];
		for (const [userIdStr, json] of Object.entries(raw)) {
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
		const result = await this.kv
			.multi()
			.hgetall(PENDING_AUTH_SESSION_HASH_KEY)
			.del(PENDING_AUTH_SESSION_HASH_KEY)
			.exec();
		if (!result || result.length === 0) {
			return [];
		}
		const hgetallResult = result[0];
		if (!hgetallResult || hgetallResult[0]) {
			return [];
		}
		const raw = hgetallResult[1];
		if (!isStringRecord(raw)) {
			return [];
		}
		const out: Array<{sessionIdHash: Buffer; entry: PendingAuthSessionEntry}> = [];
		for (const [encodedSessionIdHash, json] of Object.entries(raw)) {
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
