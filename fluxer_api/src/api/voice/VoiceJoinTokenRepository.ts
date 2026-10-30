// SPDX-License-Identifier: AGPL-3.0-or-later

import {executeConditional, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import {Db} from '@app/api/database/CassandraTypes';
import {VOICE_JOIN_TOKEN_COLUMNS, type VoiceJoinTokenRow} from '@app/api/database/types/VoiceTypes';

const KEEP_AFTER_EXPIRY_SECONDS = 7 * 24 * 60 * 60;

const VoiceJoinTokens = defineTable<VoiceJoinTokenRow, 'token_id'>({
	name: 'voice_join_tokens',
	columns: VOICE_JOIN_TOKEN_COLUMNS,
	primaryKey: ['token_id'],
});

const GET_TOKEN_CQL = VoiceJoinTokens.selectCql({where: VoiceJoinTokens.where.eq('token_id')});
const LIST_TOKENS_CQL = VoiceJoinTokens.selectCql();

function ttlFor(row: Pick<VoiceJoinTokenRow, 'expires_at'>): number {
	return Math.max(1, Math.floor((row.expires_at.getTime() - Date.now()) / 1000) + KEEP_AFTER_EXPIRY_SECONDS);
}

export class VoiceJoinTokenRepository {
	async create(row: VoiceJoinTokenRow): Promise<void> {
		const applied = await executeConditional(VoiceJoinTokens.insertIfNotExistsWithTtl(row, ttlFor(row)));
		if (!applied) {
			throw new Error(`Voice join token ${row.token_id} already exists`);
		}
	}

	async get(tokenId: bigint): Promise<VoiceJoinTokenRow | null> {
		return fetchOne<VoiceJoinTokenRow>(GET_TOKEN_CQL, {token_id: tokenId});
	}

	async list(): Promise<Array<VoiceJoinTokenRow>> {
		return fetchMany<VoiceJoinTokenRow>(LIST_TOKENS_CQL, {});
	}

	async claim(row: VoiceJoinTokenRow, claimId: string, now: Date): Promise<boolean> {
		return executeConditional(
			VoiceJoinTokens.conditionalPatchByPkWithTtl(
				{token_id: row.token_id},
				{claim_id: Db.set(claimId), claimed_at: Db.set(now)},
				{
					secret_hash: row.secret_hash,
					used_at: null,
					revoked_at: null,
					claim_id: row.claim_id,
					claimed_at: row.claimed_at,
				},
				ttlFor(row),
			),
		);
	}

	async release(row: VoiceJoinTokenRow, claimId: string): Promise<boolean> {
		return executeConditional(
			VoiceJoinTokens.conditionalPatchByPkWithTtl(
				{token_id: row.token_id},
				{claim_id: Db.clear(), claimed_at: Db.clear()},
				{claim_id: claimId},
				ttlFor(row),
			),
		);
	}

	async markUsed(
		row: VoiceJoinTokenRow,
		claimId: string,
		used: {at: Date; ip: string | null; serverId: string},
	): Promise<boolean> {
		return executeConditional(
			VoiceJoinTokens.conditionalPatchByPkWithTtl(
				{token_id: row.token_id},
				{used_at: Db.set(used.at), used_by_ip: Db.set(used.ip), used_server_id: Db.set(used.serverId)},
				{claim_id: claimId},
				ttlFor(row),
			),
		);
	}

	async revoke(row: VoiceJoinTokenRow, now: Date): Promise<boolean> {
		return executeConditional(
			VoiceJoinTokens.conditionalPatchByPkWithTtl(
				{token_id: row.token_id},
				{revoked_at: Db.set(now)},
				{secret_hash: row.secret_hash, used_at: null, revoked_at: null},
				ttlFor(row),
			),
		);
	}
}
