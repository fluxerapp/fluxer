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
import {BatchBuilder, executeConditional, fetchOne, upsertOne} from '~/database/Cassandra';
import {PendingVerifications, PendingVerificationsByTime} from '~/Tables';

const FETCH_PENDING_VERIFICATION_CQL = PendingVerifications.selectCql({
	where: PendingVerifications.where.eq('user_id'),
	limit: 1,
});

export class PendingVerificationRepository {
	async createPendingVerification(userId: UserID, createdAt: Date, metadata: Map<string, string>): Promise<void> {
		const verificationResult = await executeConditional(
			PendingVerifications.insertIfNotExists({
				user_id: userId,
				created_at: createdAt,
				version: 1,
				metadata,
			}),
		);

		if (!verificationResult.applied) {
			return;
		}

		await upsertOne(
			PendingVerificationsByTime.insert({
				created_at: createdAt,
				user_id: userId,
			}),
		);
	}

	async deletePendingVerification(userId: UserID): Promise<void> {
		const pending = await fetchOne<{user_id: bigint; created_at: Date}>(FETCH_PENDING_VERIFICATION_CQL, {
			user_id: userId,
		});
		if (!pending) return;

		const batch = new BatchBuilder();
		batch.addPrepared(PendingVerifications.deleteByPk({user_id: userId}));
		batch.addPrepared(
			PendingVerificationsByTime.deleteByPk({
				created_at: pending.created_at,
				user_id: userId,
			}),
		);
		await batch.execute();
	}
}
