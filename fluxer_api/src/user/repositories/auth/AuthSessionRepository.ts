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

import type {UserID} from '~/BrandedTypes';
import {BatchBuilder, Db, executeConditional, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {AuthSessionRow} from '~/database/CassandraTypes';
import {Logger} from '~/Logger';
import {AuthSession} from '~/Models';
import {AuthSessions, AuthSessionsByUserId} from '~/Tables';

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

export class AuthSessionRepository {
	async createAuthSession(sessionData: AuthSessionRow): Promise<AuthSession> {
		const sessionResult = await executeConditional(AuthSessions.insertIfNotExists(sessionData));

		if (!sessionResult.applied) {
			const existingSession = await this.getAuthSessionByToken(sessionData.session_id_hash);
			if (!existingSession) {
				Logger.error(
					{sessionIdHash: sessionData.session_id_hash},
					'Failed to create or retrieve existing auth session',
				);
				throw new Error('Failed to create or retrieve existing auth session');
			}
			Logger.debug(
				{sessionIdHash: sessionData.session_id_hash},
				'Auth session already exists, returning existing session',
			);
			return existingSession;
		}

		try {
			await upsertOne(
				AuthSessionsByUserId.insert({
					user_id: sessionData.user_id,
					session_id_hash: sessionData.session_id_hash,
				}),
			);
		} catch (error) {
			Logger.error({sessionIdHash: sessionData.session_id_hash, error}, 'Failed to create AuthSessionsByUserId entry');
		}

		return new AuthSession(sessionData);
	}

	async getAuthSessionByToken(sessionIdHash: Buffer): Promise<AuthSession | null> {
		const session = await fetchOne<AuthSessionRow>(FETCH_AUTH_SESSION_BY_TOKEN_CQL, {session_id_hash: sessionIdHash});
		return session ? new AuthSession(session) : null;
	}

	async listAuthSessions(userId: UserID): Promise<Array<AuthSession>> {
		const sessionHashes = await fetchMany<{session_id_hash: Buffer}>(FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL, {
			user_id: userId,
		});
		if (sessionHashes.length === 0) return [];
		const sessions = await fetchMany<AuthSessionRow>(FETCH_AUTH_SESSIONS_CQL, {
			session_id_hashes: sessionHashes.map((s) => s.session_id_hash),
		});
		return sessions.map((session) => new AuthSession(session));
	}

	async updateAuthSessionLastUsed(sessionIdHash: Buffer): Promise<void> {
		await upsertOne(
			AuthSessions.patchByPk(
				{session_id_hash: sessionIdHash},
				{
					approx_last_used_at: Db.set(new Date()),
				},
			),
		);
	}

	async deleteAuthSessions(userId: UserID, sessionIdHashes: Array<Buffer>): Promise<void> {
		const batch = new BatchBuilder();
		for (const sessionIdHash of sessionIdHashes) {
			batch.addPrepared(AuthSessions.deleteByPk({session_id_hash: sessionIdHash}));
			batch.addPrepared(AuthSessionsByUserId.deleteByPk({user_id: userId, session_id_hash: sessionIdHash}));
		}
		await batch.execute();
	}

	async deleteAllAuthSessions(userId: UserID): Promise<void> {
		const sessions = await fetchMany<{session_id_hash: Buffer}>(FETCH_AUTH_SESSION_HASHES_BY_USER_ID_CQL, {
			user_id: userId,
		});

		const batch = new BatchBuilder();
		for (const session of sessions) {
			batch.addPrepared(
				AuthSessions.deleteByPk({
					session_id_hash: session.session_id_hash,
				}),
			);
			batch.addPrepared(
				AuthSessionsByUserId.deleteByPk({
					user_id: userId,
					session_id_hash: session.session_id_hash,
				}),
			);
		}

		if (batch) {
			await batch.execute();
		}
	}
}
