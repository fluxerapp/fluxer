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
import {deleteOneOrMany, fetchMany, prepared, upsertOne} from '~/database/Cassandra';
import type {PushSubscriptionRow} from '~/database/CassandraTypes';
import {PushSubscription} from '~/Models';
import {PushSubscriptions} from '~/Tables';

const FETCH_PUSH_SUBSCRIPTIONS_CQL = PushSubscriptions.selectCql({
	where: PushSubscriptions.where.eq('user_id'),
});

const FETCH_BULK_PUSH_SUBSCRIPTIONS_CQL = PushSubscriptions.selectCql({
	where: PushSubscriptions.where.in('user_id', 'user_ids'),
});

export class PushSubscriptionRepository {
	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		const rows = await fetchMany<PushSubscriptionRow>(FETCH_PUSH_SUBSCRIPTIONS_CQL, {user_id: userId});
		return rows.map((row) => new PushSubscription(row));
	}

	async createPushSubscription(data: PushSubscriptionRow): Promise<PushSubscription> {
		await upsertOne(PushSubscriptions.upsertAll(data));
		return new PushSubscription(data);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		await deleteOneOrMany(PushSubscriptions.deleteByPk({user_id: userId, subscription_id: subscriptionId}));
	}

	async getBulkPushSubscriptions(userIds: Array<UserID>): Promise<Map<UserID, Array<PushSubscription>>> {
		if (userIds.length === 0) return new Map();

		const rows = await fetchMany<PushSubscriptionRow>(FETCH_BULK_PUSH_SUBSCRIPTIONS_CQL, {user_ids: userIds});

		const map = new Map<UserID, Array<PushSubscription>>();
		for (const row of rows) {
			const sub = new PushSubscription(row);
			const existing = map.get(row.user_id) ?? [];
			existing.push(sub);
			map.set(row.user_id, existing);
		}
		return map;
	}

	async deleteAllPushSubscriptions(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			prepared(PushSubscriptions.deleteCql({where: PushSubscriptions.where.eq('user_id')}), {user_id: userId}),
		);
	}
}
