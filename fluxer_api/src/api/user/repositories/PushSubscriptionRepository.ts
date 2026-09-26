// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {PushSubscriptionRow} from '@app/api/database/types/UserTypes';
import {PushSubscription} from '@app/api/models/PushSubscription';
import {PushSubscriptions} from '@app/api/Tables';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';

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

	async deletePushSubscriptionsForAuthSessions(
		userId: UserID,
		authSessionIdHashes: Array<string>,
		options: {deleteUnboundSubscriptions: boolean},
	): Promise<void> {
		if (authSessionIdHashes.length === 0 && !options.deleteUnboundSubscriptions) return;
		const subscriptions = await this.listPushSubscriptions(userId);
		const authSessionIdHashSet = new Set(authSessionIdHashes);
		const subscriptionsToDelete = subscriptions.filter((subscription) => {
			if (subscription.authSessionIdHash === null) {
				return options.deleteUnboundSubscriptions;
			}
			return authSessionIdHashSet.has(subscription.authSessionIdHash);
		});
		if (subscriptionsToDelete.length === 0) return;
		await awaitAll(
			subscriptionsToDelete.map(async (subscription) =>
				this.deletePushSubscription(userId, subscription.subscriptionId),
			),
			'Failed to delete push subscriptions for auth sessions',
		);
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
			PushSubscriptions.delete({where: PushSubscriptions.where.eq('user_id')}).bind({user_id: userId}),
		);
	}
}
