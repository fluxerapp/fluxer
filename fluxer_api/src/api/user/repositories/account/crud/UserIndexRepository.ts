// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {UserID} from '../../../../BrandedTypes';
import {BatchBuilder} from '../../../../database/CassandraQueryExecution';
import type {UserRow} from '../../../../database/types/UserTypes';
import {
	UserByEmail,
	UserByLastActiveIp,
	UserByLastActiveIpTrustKey,
	UserByStripeCustomerId,
	UserByStripeSubscriptionId,
	UserByUsername,
} from '../../../../Tables';

export class UserIndexRepository {
	async syncIndices(data: UserRow, oldData?: UserRow | null): Promise<void> {
		const batch = new BatchBuilder();
		if (!!data.username && data.discriminator != null && data.discriminator !== undefined) {
			batch.addPrepared(
				UserByUsername.upsertAll({
					username: data.username.toLowerCase(),
					discriminator: data.discriminator,
					user_id: data.user_id,
				}),
			);
		}
		if (oldData?.username && oldData.discriminator != null && oldData.discriminator !== undefined) {
			if (
				oldData.username.toLowerCase() !== data.username?.toLowerCase() ||
				oldData.discriminator !== data.discriminator
			) {
				batch.addPrepared(
					UserByUsername.deleteByPk({
						username: oldData.username.toLowerCase(),
						discriminator: oldData.discriminator,
						user_id: oldData.user_id,
					}),
				);
			}
		}
		if (data.email) {
			batch.addPrepared(UserByEmail.upsertAll({email_lower: data.email.toLowerCase(), user_id: data.user_id}));
		}
		if (oldData?.email && oldData.email.toLowerCase() !== data.email?.toLowerCase()) {
			batch.addPrepared(
				UserByEmail.deleteByPk({
					email_lower: oldData.email.toLowerCase(),
					user_id: oldData.user_id,
				}),
			);
		}
		if (data.stripe_subscription_id) {
			batch.addPrepared(
				UserByStripeSubscriptionId.upsertAll({
					stripe_subscription_id: data.stripe_subscription_id,
					user_id: data.user_id,
				}),
			);
		}
		if (oldData?.stripe_subscription_id && oldData.stripe_subscription_id !== data.stripe_subscription_id) {
			batch.addPrepared(
				UserByStripeSubscriptionId.deleteByPk({
					stripe_subscription_id: oldData.stripe_subscription_id,
					user_id: oldData.user_id,
				}),
			);
		}
		if (data.stripe_customer_id) {
			batch.addPrepared(
				UserByStripeCustomerId.upsertAll({
					stripe_customer_id: data.stripe_customer_id,
					user_id: data.user_id,
				}),
			);
		}
		if (oldData?.stripe_customer_id && oldData.stripe_customer_id !== data.stripe_customer_id) {
			batch.addPrepared(
				UserByStripeCustomerId.deleteByPk({
					stripe_customer_id: oldData.stripe_customer_id,
					user_id: oldData.user_id,
				}),
			);
		}
		if (data.last_active_ip) {
			batch.addPrepared(
				UserByLastActiveIp.upsertAll({
					last_active_ip: data.last_active_ip,
					user_id: data.user_id,
					last_active_at: data.last_active_at ?? null,
				}),
			);
			const trustKey = getSameIpDecisionKey(data.last_active_ip);
			if (trustKey) {
				batch.addPrepared(
					UserByLastActiveIpTrustKey.upsertAll({
						last_active_ip_trust_key: trustKey,
						user_id: data.user_id,
						last_active_at: data.last_active_at ?? null,
					}),
				);
			}
		}
		if (oldData?.last_active_ip && oldData.last_active_ip !== data.last_active_ip) {
			batch.addPrepared(
				UserByLastActiveIp.deleteByPk({
					last_active_ip: oldData.last_active_ip,
					user_id: oldData.user_id,
				}),
			);
			const oldTrustKey = getSameIpDecisionKey(oldData.last_active_ip);
			const newTrustKey = data.last_active_ip ? getSameIpDecisionKey(data.last_active_ip) : null;
			if (oldTrustKey && oldTrustKey !== newTrustKey) {
				batch.addPrepared(
					UserByLastActiveIpTrustKey.deleteByPk({
						last_active_ip_trust_key: oldTrustKey,
						user_id: oldData.user_id,
					}),
				);
			}
		}
		await batch.execute();
	}

	async updateLastActiveIpIndex(
		userId: UserID,
		lastActiveIp: string,
		lastActiveAt: Date,
		previousLastActiveIp?: string | null,
	): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			UserByLastActiveIp.upsertAll({
				last_active_ip: lastActiveIp,
				user_id: userId,
				last_active_at: lastActiveAt,
			}),
		);
		const trustKey = getSameIpDecisionKey(lastActiveIp);
		if (trustKey) {
			batch.addPrepared(
				UserByLastActiveIpTrustKey.upsertAll({
					last_active_ip_trust_key: trustKey,
					user_id: userId,
					last_active_at: lastActiveAt,
				}),
			);
		}
		if (previousLastActiveIp && previousLastActiveIp !== lastActiveIp) {
			batch.addPrepared(
				UserByLastActiveIp.deleteByPk({
					last_active_ip: previousLastActiveIp,
					user_id: userId,
				}),
			);
			const previousTrustKey = getSameIpDecisionKey(previousLastActiveIp);
			if (previousTrustKey && previousTrustKey !== trustKey) {
				batch.addPrepared(
					UserByLastActiveIpTrustKey.deleteByPk({
						last_active_ip_trust_key: previousTrustKey,
						user_id: userId,
					}),
				);
			}
		}
		await batch.execute(false);
	}

	async deleteIndices(
		userId: UserID,
		username: string,
		discriminator: number,
		email?: string | null,
		stripeCustomerId?: string | null,
		stripeSubscriptionId?: string | null,
		lastActiveIp?: string | null,
	): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			UserByUsername.deleteByPk({
				username: username.toLowerCase(),
				discriminator: discriminator,
				user_id: userId,
			}),
		);
		if (email) {
			batch.addPrepared(
				UserByEmail.deleteByPk({
					email_lower: email.toLowerCase(),
					user_id: userId,
				}),
			);
		}
		if (stripeCustomerId) {
			batch.addPrepared(
				UserByStripeCustomerId.deleteByPk({
					stripe_customer_id: stripeCustomerId,
					user_id: userId,
				}),
			);
		}
		if (stripeSubscriptionId) {
			batch.addPrepared(
				UserByStripeSubscriptionId.deleteByPk({
					stripe_subscription_id: stripeSubscriptionId,
					user_id: userId,
				}),
			);
		}
		if (lastActiveIp) {
			batch.addPrepared(
				UserByLastActiveIp.deleteByPk({
					last_active_ip: lastActiveIp,
					user_id: userId,
				}),
			);
			const trustKey = getSameIpDecisionKey(lastActiveIp);
			if (trustKey) {
				batch.addPrepared(
					UserByLastActiveIpTrustKey.deleteByPk({
						last_active_ip_trust_key: trustKey,
						user_id: userId,
					}),
				);
			}
		}
		await batch.execute();
	}
}
