// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import type {AuthSessionRow, AuthSessionTombstoneRow} from '@app/api/database/types/AuthTypes';
import {Logger} from '@app/api/Logger';
import {getCacheService, getUserActivityBuffer} from '@app/api/middleware/ServiceSingletons';
import {AuthSession, AuthSessionTombstone} from '@app/api/models/AuthSession';
import {AuthSessions, AuthSessionsByUserId, AuthSessionTombstones} from '@app/api/Tables';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {isValidTimestamp} from '@app/api/utils/TimestampUtils';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const AUTH_SESSION_CACHE_TTL_SECONDS = 30;
const AUTH_SESSION_MISS_CACHE_TTL_SECONDS = 5;
const AUTH_SESSION_READ_SLICE = 100;
const AUTH_SESSIONS_PER_BATCH = 20;

interface CachedAuthSession {
	user_id: string;
	session_id_hash: string;
	created_at: number;
	approx_last_used_at: number;
	client_ip: string;
	client_user_agent: string | null;
	client_os: string | null;
	client_country: string | null;
	version: number;
}

const CachedAuthSessionSchema = z.object({
	user_id: z
		.string()
		.refine((value) => value.length <= 19 && !/\D/.test(value) && SnowflakeType.safeParse(value).success),
	session_id_hash: z.string(),
	created_at: z.custom<number>(isValidTimestamp),
	approx_last_used_at: z.custom<number>(isValidTimestamp),
	client_ip: z.string(),
	client_user_agent: z.string().nullable(),
	client_os: z.string().nullable(),
	client_country: z.string().nullable(),
	version: z.int().nonnegative(),
}) satisfies z.ZodType<CachedAuthSession>;

function authSessionCacheKey(sessionIdHash: Buffer): string {
	return `auth:session:${sessionIdHash.toString('base64url')}`;
}

function encodeCachedAuthSession(row: AuthSessionRow): CachedAuthSession {
	return {
		user_id: row.user_id.toString(),
		session_id_hash: row.session_id_hash.toString('base64url'),
		created_at: row.created_at.getTime(),
		approx_last_used_at: row.approx_last_used_at.getTime(),
		client_ip: row.client_ip,
		client_user_agent: row.client_user_agent,
		client_os: row.client_os,
		client_country: row.client_country,
		version: row.version,
	};
}

function decodeCachedAuthSession(value: unknown, sessionIdHash: Buffer): AuthSessionRow {
	const parsed = CachedAuthSessionSchema.safeParse(value);
	if (!parsed.success) {
		throw new TypeError('Cached auth session has an invalid shape', {cause: parsed.error});
	}
	const cached = parsed.data;
	if (cached.session_id_hash !== sessionIdHash.toString('base64url')) {
		throw new Error('Cached auth session does not match the requested token hash');
	}
	return {
		user_id: createUserID(BigInt(cached.user_id)),
		session_id_hash: Buffer.from(sessionIdHash),
		created_at: new Date(cached.created_at),
		approx_last_used_at: new Date(cached.approx_last_used_at),
		client_ip: cached.client_ip,
		client_user_agent: cached.client_user_agent,
		client_os: cached.client_os,
		client_country: cached.client_country,
		version: cached.version,
	};
}

async function invalidateAuthSessionCache(sessionIdHashes: ReadonlyArray<Buffer>): Promise<void> {
	if (sessionIdHashes.length === 0) return;
	try {
		const cache = getCacheService();
		await awaitAll(
			sessionIdHashes.map(async (sessionIdHash) => cache.delete(authSessionCacheKey(sessionIdHash))),
			'Failed to invalidate cached auth sessions',
		);
	} catch (error) {
		Logger.error({error}, 'Failed to invalidate cached auth sessions; they expire with the cache ttl');
	}
}

const FETCH_AUTH_SESSIONS_CQL = AuthSessions.selectCql({
	where: AuthSessions.where.in('session_id_hash', 'session_id_hashes'),
});
const FETCH_AUTH_SESSION_BY_TOKEN_CQL = AuthSessions.selectCql({
	where: AuthSessions.where.eq('session_id_hash'),
	limit: 1,
});
const FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL = AuthSessionsByUserId.selectCql({
	columns: ['session_id_hash'],
	where: AuthSessionsByUserId.where.eq('user_id'),
});
const FETCH_AUTH_SESSION_TOMBSTONES_BY_USER_ID_CQL = AuthSessionTombstones.selectCql({
	where: AuthSessionTombstones.where.eq('user_id'),
});

export class AuthSessionRepository {
	async createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession> {
		const batch = new BatchBuilder();
		batch.addPrepared(AuthSessions.insert(sessionData));
		batch.addPrepared(
			AuthSessionsByUserId.insert({
				user_id: sessionData.user_id,
				session_id_hash: sessionData.session_id_hash,
			}),
		);
		await batch.execute();
		await invalidateAuthSessionCache([sessionData.session_id_hash]);
		return new AuthSession(sessionData);
	}

	async getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null> {
		let sessionRead: Promise<AuthSessionRow | null> | undefined;
		const readSession = () => (sessionRead ??= this.fetchAuthSessionByToken(sessionIdHash));
		try {
			const cached = await getCacheService().getOrSet<unknown>(
				authSessionCacheKey(sessionIdHash),
				async () => {
					const session = await readSession();
					return session ? encodeCachedAuthSession(session) : null;
				},
				(value) => (value === null ? AUTH_SESSION_MISS_CACHE_TTL_SECONDS : AUTH_SESSION_CACHE_TTL_SECONDS),
			);
			return cached === null ? null : new AuthSession(decodeCachedAuthSession(cached, sessionIdHash));
		} catch (error) {
			const session = await readSession();
			Logger.warn({error}, 'Auth session cache lookup failed; falling back to the datastore');
			return session ? new AuthSession(session) : null;
		}
	}

	private async fetchAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSessionRow | null> {
		const row = await fetchOne<AuthSessionRow>(FETCH_AUTH_SESSION_BY_TOKEN_CQL, {
			session_id_hash: sessionIdHash,
		});
		return row && isLiveAuthSessionRow(row) ? row : null;
	}

	async listAuthSessions(userId: UserID): Promise<Array<AuthSession>> {
		const sessionHashes = await fetchMany<{
			session_id_hash: Buffer;
		}>(FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL, {
			user_id: userId,
		});
		if (sessionHashes.length === 0) return [];
		const sessions = await fetchAuthSessionRows(sessionHashes.map((s) => s.session_id_hash));
		return sessions.filter((session) => session.user_id === userId).map((session) => new AuthSession(session));
	}

	async listAuthSessionTombstones(userId: UserID): Promise<Array<AuthSessionTombstone>> {
		const rows = await fetchMany<AuthSessionTombstoneRow>(FETCH_AUTH_SESSION_TOMBSTONES_BY_USER_ID_CQL, {
			user_id: userId,
		});
		return rows.map((row) => new AuthSessionTombstone(row));
	}

	async updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void> {
		const approximateLastUsedAt = new Date();
		await upsertOne(
			AuthSessions.patchByPk({session_id_hash: sessionIdHash}, {approx_last_used_at: Db.set(approximateLastUsedAt)}),
		);
		await invalidateAuthSessionCache([sessionIdHash]);
	}

	async deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void> {
		if (sessionIdHashes.length === 0) return;
		await invalidateAuthSessionCache(sessionIdHashes);
		const originals = new Map(
			(await fetchRawAuthSessionRows(sessionIdHashes)).map((row) => [row.session_id_hash.toString('base64url'), row]),
		);
		const deletedAt = new Date();
		const revoked: Array<Buffer> = [];
		const groups: Array<Array<PreparedQuery>> = [];
		for (const sessionIdHash of sessionIdHashes) {
			const original = originals.get(sessionIdHash.toString('base64url'));
			if (original && isLiveAuthSessionRow(original) && original.user_id !== userId) continue;
			revoked.push(sessionIdHash);
			groups.push(
				revokeStatements(userId, sessionIdHash, original && isLiveAuthSessionRow(original) ? original : null, {
					deletePrimary: original !== undefined,
					deletedAt,
				}),
			);
		}
		await executeSessionGroups(groups);
		await forgetAuthSessionActivity(revoked);
		await invalidateAuthSessionCache(sessionIdHashes);
	}

	async deleteAllAuthSessions(userId: UserID): Promise<void> {
		const sessionRefs = await fetchMany<{
			session_id_hash: Buffer;
		}>(FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL, {
			user_id: userId,
		});
		if (sessionRefs.length === 0) return;
		const sessionIdHashes = sessionRefs.map((session) => session.session_id_hash);
		await invalidateAuthSessionCache(sessionIdHashes);
		let originals = new Map<string, AuthSessionRow>();
		try {
			originals = new Map(
				(await fetchAuthSessionRows(sessionIdHashes)).map((row) => [row.session_id_hash.toString('base64url'), row]),
			);
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), error},
				'Failed to read auth sessions before bulk delete; tombstones will be skipped',
			);
		}
		const deletedAt = new Date();
		const groups = sessionIdHashes.map((sessionIdHash) => {
			const original = originals.get(sessionIdHash.toString('base64url'));
			return revokeStatements(userId, sessionIdHash, original?.user_id === userId ? original : null, {
				deletePrimary: true,
				deletedAt,
			});
		});
		await executeSessionGroups(groups);
		await forgetAuthSessionActivity(sessionIdHashes);
		await invalidateAuthSessionCache(sessionIdHashes);
	}
}

function isLiveAuthSessionRow(row: AuthSessionRow): boolean {
	return row.user_id != null && row.created_at != null;
}

async function fetchRawAuthSessionRows(sessionIdHashes: ReadonlyArray<Buffer>): Promise<Array<AuthSessionRow>> {
	const rows: Array<AuthSessionRow> = [];
	for (let index = 0; index < sessionIdHashes.length; index += AUTH_SESSION_READ_SLICE) {
		rows.push(
			...(await fetchMany<AuthSessionRow>(FETCH_AUTH_SESSIONS_CQL, {
				session_id_hashes: sessionIdHashes.slice(index, index + AUTH_SESSION_READ_SLICE),
			})),
		);
	}
	return rows;
}

async function fetchAuthSessionRows(sessionIdHashes: ReadonlyArray<Buffer>): Promise<Array<AuthSessionRow>> {
	return (await fetchRawAuthSessionRows(sessionIdHashes)).filter(isLiveAuthSessionRow);
}

function revokeStatements(
	userId: UserID,
	sessionIdHash: Buffer,
	original: AuthSessionRow | null,
	options: {deletePrimary: boolean; deletedAt: Date},
): Array<PreparedQuery> {
	const statements: Array<PreparedQuery> = [];
	if (options.deletePrimary) statements.push(AuthSessions.deleteByPk({session_id_hash: sessionIdHash}));
	statements.push(AuthSessionsByUserId.deleteByPk({user_id: userId, session_id_hash: sessionIdHash}));
	if (original) statements.push(AuthSessionTombstones.insert(toTombstoneRow(original, options.deletedAt)));
	return statements;
}

async function executeSessionGroups(groups: ReadonlyArray<ReadonlyArray<PreparedQuery>>): Promise<void> {
	for (let index = 0; index < groups.length; index += AUTH_SESSIONS_PER_BATCH) {
		const batch = new BatchBuilder();
		for (const group of groups.slice(index, index + AUTH_SESSIONS_PER_BATCH)) {
			for (const statement of group) batch.addPrepared(statement);
		}
		await batch.execute();
	}
}

async function forgetAuthSessionActivity(sessionIdHashes: ReadonlyArray<Buffer>): Promise<void> {
	if (sessionIdHashes.length === 0) return;
	try {
		await getUserActivityBuffer().forgetAuthSessions(sessionIdHashes);
	} catch (error) {
		Logger.warn({error}, 'Failed to clear pending activity for revoked auth sessions');
	}
}

function toTombstoneRow(row: AuthSessionRow, deletedAt: Date): AuthSessionTombstoneRow {
	return {
		user_id: row.user_id,
		session_id_hash: row.session_id_hash,
		created_at: row.created_at,
		approx_last_used_at: row.approx_last_used_at,
		client_ip: row.client_ip,
		client_user_agent: row.client_user_agent,
		client_os: row.client_os ?? null,
		client_country: row.client_country ?? null,
		deleted_at: deletedAt,
		version: row.version,
	};
}
