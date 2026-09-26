// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {InboundSmsChallengeRow} from '@app/api/database/types/RiskTypes';
import {InboundSmsChallenges, InboundSmsChallengesByUser} from '@app/api/Tables';
import {randomNumericCode} from '@app/api/utils/RandomUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface IssueChallengeParams {
	userId: UserID;
	ourNumber: string;
	ttlSeconds?: number;
}

export interface IssuedChallenge {
	challengeCode: string;
	ourNumber: string;
	expiresAt: Date;
}

interface ConsumedChallenge {
	userId: UserID;
	consumedAt: Date;
}

interface IInboundSmsChallengeRepository {
	insertChallenge(row: InboundSmsChallengeRow): Promise<void>;
	getChallenge(code: string): Promise<InboundSmsChallengeRow | null>;
	markConsumed(code: string, fromPhone: string, consumedAt: Date): Promise<void>;
}

const DEFAULT_TTL_SECONDS = 900;
const CODE_LENGTH = 6;
const CONSUME_LOCK_PREFIX = 'sms_challenge:consume:';
const CONSUME_LOCK_TTL_SECONDS = 30;

export class InboundSmsChallengeService {
	constructor(
		private readonly repository: IInboundSmsChallengeRepository,
		private readonly kvClient: IKVProvider,
	) {}

	async issueChallenge(params: IssueChallengeParams): Promise<IssuedChallenge> {
		const ttl = params.ttlSeconds ?? DEFAULT_TTL_SECONDS;
		const challengeCode = generateNumericCode(CODE_LENGTH);
		const now = new Date();
		const expiresAt = new Date(now.getTime() + ttl * 1000);
		await this.repository.insertChallenge({
			challenge_code: challengeCode,
			user_id: params.userId,
			our_number: params.ourNumber,
			created_at: now,
			expires_at: expiresAt,
			consumed_at: null,
			consumed_from_phone: null,
		});
		return {challengeCode, ourNumber: params.ourNumber, expiresAt};
	}

	async consumeChallenge(args: {code: string; fromPhone: string}): Promise<ConsumedChallenge | null> {
		const normalizedCode = args.code.trim();
		const lockKey = `${CONSUME_LOCK_PREFIX}${normalizedCode}`;
		const acquired = await this.kvClient.acquireLock(lockKey, '1', CONSUME_LOCK_TTL_SECONDS);
		if (!acquired) {
			return null;
		}
		try {
			const challenge = await this.repository.getChallenge(normalizedCode);
			if (!challenge) return null;
			if (challenge.consumed_at !== null) return null;
			if (challenge.expires_at.getTime() < Date.now()) return null;
			const consumedAt = new Date();
			await this.repository.markConsumed(normalizedCode, args.fromPhone, consumedAt);
			return {userId: challenge.user_id, consumedAt};
		} finally {
			await this.kvClient.del(lockKey).catch(() => {});
		}
	}
}

export class CassandraInboundSmsChallengeRepository implements IInboundSmsChallengeRepository {
	async insertChallenge(row: InboundSmsChallengeRow): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(InboundSmsChallenges.insert(row));
		batch.addPrepared(
			InboundSmsChallengesByUser.insert({
				user_id: row.user_id,
				created_at: row.created_at,
				challenge_code: row.challenge_code,
				expires_at: row.expires_at,
			}),
		);
		await batch.execute(false);
	}

	async getChallenge(code: string): Promise<InboundSmsChallengeRow | null> {
		return (
			(await fetchOne<InboundSmsChallengeRow>(
				InboundSmsChallenges.selectCql({where: InboundSmsChallenges.where.eq('challenge_code'), limit: 1}),
				{challenge_code: code},
			)) ?? null
		);
	}

	async markConsumed(code: string, fromPhone: string, consumedAt: Date): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			InboundSmsChallenges.patchByPk(
				{challenge_code: code},
				{consumed_at: Db.set(consumedAt), consumed_from_phone: Db.set(fromPhone)},
			),
		);
		await batch.execute(false);
	}
}

function generateNumericCode(length: number): string {
	return randomNumericCode(length);
}
