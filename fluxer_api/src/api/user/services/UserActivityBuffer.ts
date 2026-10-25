// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {Logger} from '@app/api/Logger';
import {AuthSessions, AuthSessionsByAppChannel} from '@app/api/Tables';
import {UserAccountRepository} from '@app/api/user/repositories/account/UserAccountRepository';
import {isJsonRecord, parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {
	type ReportedClientApp,
	readReportedClientApp,
	reportedClientAppRecord,
} from '@app/api/utils/SessionClientIdentity';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';

const PENDING_HASH_KEY = 'user_activity:pending';
const PENDING_AUTH_SESSION_HASH_KEY = 'auth_session_activity:pending';
const AUTH_SESSION_TOUCH_KEY_PREFIX = 'auth_session_activity:touched:';
const WRITE_CONCURRENCY = 64;
const AUTH_SESSION_TOUCH_DEBOUNCE_TTL_SECONDS = seconds('5 minutes');
const AUTH_SESSION_EXISTENCE_READ_SLICE = 100;
type ActivityWriter = typeof upsertOne;

const FETCH_AUTH_SESSION_OWNERS_CQL = AuthSessions.selectCql({
	columns: ['session_id_hash', 'user_id', 'app_platform', 'app_release_channel'],
	where: AuthSessions.where.in('session_id_hash', 'session_id_hashes'),
});

interface UserActivityAccountWriter {
	updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void>;
}

interface PendingEntry {
	ts: number;
	ip: string | null;
}

interface PendingAuthSessionEntry {
	ts: number;
	app: ReportedClientApp | null;
}

interface LiveAuthSession {
	userId: UserID;
	appPlatform: string | null;
	appReleaseChannel: string | null;
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
	return {ts: decoded.ts, app: isJsonRecord(decoded.app) ? readReportedClientApp(decoded.app) : null};
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

function dbValue<T>(value: T | null) {
	return value === null ? Db.clear<T>() : Db.set(value);
}

function authSessionActivityStatements(
	sessionIdHash: Buffer,
	entry: PendingAuthSessionEntry,
	session: LiveAuthSession,
): Array<PreparedQuery> {
	const lastUsed = {approx_last_used_at: Db.set(new Date(entry.ts))};
	const {app} = entry;
	if (!app) return [AuthSessions.patchByPk({session_id_hash: sessionIdHash}, lastUsed)];
	const statements = [
		AuthSessions.patchByPk(
			{session_id_hash: sessionIdHash},
			{
				...lastUsed,
				app_platform: dbValue(app.platform),
				app_version: dbValue(app.version),
				app_release_channel: dbValue(app.releaseChannel),
				app_build_number: dbValue(app.buildNumber),
				app_beta_number: dbValue(app.betaNumber),
			},
		),
	];
	const unchanged = session.appPlatform === app.platform && session.appReleaseChannel === app.releaseChannel;
	if (unchanged) return statements;
	if (session.appPlatform && session.appReleaseChannel) {
		statements.push(
			AuthSessionsByAppChannel.deleteByPk({
				app_platform: session.appPlatform,
				app_release_channel: session.appReleaseChannel,
				session_id_hash: sessionIdHash,
			}),
		);
	}
	if (app.platform && app.releaseChannel) {
		statements.push(
			AuthSessionsByAppChannel.insert({
				app_platform: app.platform,
				app_release_channel: app.releaseChannel,
				session_id_hash: sessionIdHash,
				user_id: session.userId,
			}),
		);
	}
	return statements;
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

	async recordAuthSessionActivity(
		sessionIdHash: Buffer,
		timestamp: Date,
		app: ReportedClientApp | null = null,
	): Promise<boolean> {
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
		const payload = {ts: timestamp.getTime(), ...(app ? {app: reportedClientAppRecord(app)} : {})};
		try {
			await this.kv.hset(PENDING_AUTH_SESSION_HASH_KEY, encodedSessionIdHash, JSON.stringify(payload));
			return true;
		} catch (error) {
			Logger.debug({error}, 'Failed to enqueue auth session activity to buffer');
			void this.kv.del(touchKey).catch(() => undefined);
			return false;
		}
	}

	async forgetAuthSessions(sessionIdHashes: ReadonlyArray<Buffer>): Promise<void> {
		if (sessionIdHashes.length === 0) return;
		const encoded = sessionIdHashes.map((sessionIdHash) => this.encodeSessionIdHash(sessionIdHash));
		await this.kv.hdel(PENDING_AUTH_SESSION_HASH_KEY, ...encoded);
		await this.kv.del(...encoded.map((value) => `${AUTH_SESSION_TOUCH_KEY_PREFIX}${value}`));
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
		const live = await this.liveAuthSessions(drained.map(({sessionIdHash}) => sessionIdHash));
		const writable = drained.flatMap((entry) => {
			const session = live.get(this.encodeSessionIdHash(entry.sessionIdHash));
			return session ? [{...entry, session}] : [];
		});
		const stats = await flushActivityEntries(
			writable,
			async ({sessionIdHash, entry, session}) => {
				for (const statement of authSessionActivityStatements(sessionIdHash, entry, session)) {
					await this.writer(statement);
				}
			},
			'Failed to flush an auth session activity entry',
		);
		return {...stats, drained: drained.length, skipped: stats.skipped + drained.length - writable.length};
	}

	private async liveAuthSessions(sessionIdHashes: ReadonlyArray<Buffer>): Promise<Map<string, LiveAuthSession>> {
		const live = new Map<string, LiveAuthSession>();
		for (let index = 0; index < sessionIdHashes.length; index += AUTH_SESSION_EXISTENCE_READ_SLICE) {
			const rows = await fetchMany<{
				session_id_hash: Buffer;
				user_id: UserID | null;
				app_platform: string | null;
				app_release_channel: string | null;
			}>(FETCH_AUTH_SESSION_OWNERS_CQL, {
				session_id_hashes: sessionIdHashes.slice(index, index + AUTH_SESSION_EXISTENCE_READ_SLICE),
			});
			for (const row of rows) {
				if (row.user_id == null) continue;
				live.set(this.encodeSessionIdHash(row.session_id_hash), {
					userId: row.user_id,
					appPlatform: row.app_platform ?? null,
					appReleaseChannel: row.app_release_channel ?? null,
				});
			}
		}
		return live;
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
