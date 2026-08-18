// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '../../../database/CassandraQueryExecution';
import {Db} from '../../../database/CassandraTypes';
import type {AuthSessionRow, AuthSessionTombstoneRow, UserCountryHistoryRow} from '../../../database/types/AuthTypes';
import {Logger} from '../../../Logger';
import {getPhoneFraudGraphService} from '../../../middleware/ServiceSingletons';
import {AuthSession, AuthSessionTombstone} from '../../../models/AuthSession';
import {AuthSessions, AuthSessionsByUserId, AuthSessionTombstones, UserCountryHistory} from '../../../Tables';

function invalidateAuthSessionCache(_sessionIdHash: Buffer): void {}

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
const FETCH_USER_COUNTRY_HISTORY_CQL = UserCountryHistory.selectCql({
	columns: ['country'],
	where: UserCountryHistory.where.eq('user_id'),
});
const FETCH_USER_COUNTRY_HISTORY_ENTRY_CQL = UserCountryHistory.selectCql({
	columns: ['first_seen_at'],
	where: [UserCountryHistory.where.eq('user_id'), UserCountryHistory.where.eq('country')],
	limit: 1,
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
		try {
			await getPhoneFraudGraphService().recordSessionForCohortGraph(
				sessionData.user_id,
				sessionData.client_ip,
				sessionData.created_at,
			);
		} catch (error) {
			Logger.warn({error}, 'phone_fraud_graph.record_session_cohort failed (non-fatal)');
		}
		if (sessionData.client_country) {
			try {
				await this.recordCountrySighting(sessionData.user_id, sessionData.client_country, sessionData.created_at);
			} catch (error) {
				Logger.warn(
					{userId: sessionData.user_id.toString(), country: sessionData.client_country, error},
					'Failed to record country sighting at session creation',
				);
			}
		}
		return new AuthSession(sessionData);
	}

	async recordCountrySighting(userId: UserID, country: string, now: Date = new Date()): Promise<void> {
		const normalizedCountry = country.toUpperCase();
		const existing = await fetchOne<{
			first_seen_at: Date;
		}>(FETCH_USER_COUNTRY_HISTORY_ENTRY_CQL, {
			user_id: userId,
			country: normalizedCountry,
		});
		const firstSeenAt = existing?.first_seen_at ?? now;
		await upsertOne(
			UserCountryHistory.insert({
				user_id: userId,
				country: normalizedCountry,
				first_seen_at: firstSeenAt,
				last_seen_at: now,
			}),
		);
	}

	async hasCountrySightingOutsideSet(userId: UserID, countryCodes: Iterable<string>): Promise<boolean> {
		const rows = await fetchMany<Pick<UserCountryHistoryRow, 'country'>>(FETCH_USER_COUNTRY_HISTORY_CQL, {
			user_id: userId,
		});
		const excludedCountries = new Set(Array.from(countryCodes, (country) => country.toUpperCase()));
		return rows.some((row) => !excludedCountries.has(row.country.toUpperCase()));
	}

	async listCountryHistory(userId: UserID): Promise<
		Array<{
			country: string;
		}>
	> {
		const rows = await fetchMany<Pick<UserCountryHistoryRow, 'country'>>(FETCH_USER_COUNTRY_HISTORY_CQL, {
			user_id: userId,
		});
		return rows.map((r) => ({country: r.country}));
	}

	async getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null> {
		const session = await fetchOne<AuthSessionRow>(FETCH_AUTH_SESSION_BY_TOKEN_CQL, {
			session_id_hash: sessionIdHash,
		});
		return session ? new AuthSession(session) : null;
	}

	async listAuthSessions(userId: UserID): Promise<Array<AuthSession>> {
		const sessionHashes = await fetchMany<{
			session_id_hash: Buffer;
		}>(FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL, {
			user_id: userId,
		});
		if (sessionHashes.length === 0) return [];
		const sessions = await fetchMany<AuthSessionRow>(FETCH_AUTH_SESSIONS_CQL, {
			session_id_hashes: sessionHashes.map((s) => s.session_id_hash),
		});
		return sessions.map((session) => new AuthSession(session));
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
		invalidateAuthSessionCache(sessionIdHash);
	}

	async deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void> {
		if (sessionIdHashes.length === 0) return;
		let originals: Array<AuthSessionRow> = [];
		try {
			originals = await fetchMany<AuthSessionRow>(FETCH_AUTH_SESSIONS_CQL, {
				session_id_hashes: sessionIdHashes,
			});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), error},
				'Failed to read auth sessions before delete; aborting to avoid cross-user deletion',
			);
			return;
		}
		const owned = originals.filter((original) => original.user_id === userId);
		if (owned.length === 0) return;
		const deletedAt = new Date();
		const batch = new BatchBuilder();
		for (const original of owned) {
			batch.addPrepared(AuthSessions.deleteByPk({session_id_hash: original.session_id_hash}));
			batch.addPrepared(AuthSessionsByUserId.deleteByPk({user_id: userId, session_id_hash: original.session_id_hash}));
			batch.addPrepared(AuthSessionTombstones.insert(toTombstoneRow(original, deletedAt)));
		}
		await batch.execute();
	}

	async deleteAllAuthSessions(userId: UserID): Promise<void> {
		const sessionRefs = await fetchMany<{
			session_id_hash: Buffer;
		}>(FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL, {
			user_id: userId,
		});
		if (sessionRefs.length === 0) return;
		let originals: Array<AuthSessionRow> = [];
		try {
			originals = await fetchMany<AuthSessionRow>(FETCH_AUTH_SESSIONS_CQL, {
				session_id_hashes: sessionRefs.map((s) => s.session_id_hash),
			});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), error},
				'Failed to read auth sessions before bulk delete; tombstones will be skipped',
			);
		}
		const deletedAt = new Date();
		const batch = new BatchBuilder();
		for (const session of sessionRefs) {
			batch.addPrepared(AuthSessions.deleteByPk({session_id_hash: session.session_id_hash}));
			batch.addPrepared(
				AuthSessionsByUserId.deleteByPk({
					user_id: userId,
					session_id_hash: session.session_id_hash,
				}),
			);
		}
		for (const original of originals) {
			batch.addPrepared(AuthSessionTombstones.insert(toTombstoneRow(original, deletedAt)));
		}
		await batch.execute();
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
