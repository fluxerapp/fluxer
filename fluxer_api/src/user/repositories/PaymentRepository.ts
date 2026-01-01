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
import {BatchBuilder, Db, executeVersionedUpdate, fetchMany, fetchOne} from '~/database/Cassandra';
import type {PaymentBySubscriptionRow, PaymentRow} from '~/database/CassandraTypes';
import {Payment} from '~/Models';
import {Payments, PaymentsByPaymentIntent, PaymentsBySubscription, PaymentsByUser} from '~/Tables';

const FETCH_PAYMENT_BY_CHECKOUT_SESSION_QUERY = Payments.selectCql({
	where: Payments.where.eq('checkout_session_id'),
	limit: 1,
});

const FETCH_PAYMENT_BY_PAYMENT_INTENT_QUERY = PaymentsByPaymentIntent.selectCql({
	columns: ['checkout_session_id'],
	where: PaymentsByPaymentIntent.where.eq('payment_intent_id'),
});

const FETCH_PAYMENT_BY_SUBSCRIPTION_QUERY = PaymentsBySubscription.selectCql({
	where: PaymentsBySubscription.where.eq('subscription_id'),
});

const FETCH_PAYMENTS_BY_USER_QUERY = PaymentsByUser.selectCql({
	columns: ['checkout_session_id'],
	where: PaymentsByUser.where.eq('user_id'),
});

const FETCH_PAYMENTS_BY_IDS_QUERY = Payments.selectCql({
	where: Payments.where.in('checkout_session_id', 'checkout_session_ids'),
});

export class PaymentRepository {
	async createPayment(data: {
		checkout_session_id: string;
		user_id: UserID;
		price_id: string;
		product_type: string;
		status: string;
		is_gift: boolean;
		created_at: Date;
	}): Promise<void> {
		const batch = new BatchBuilder();

		const paymentRow: PaymentRow = {
			checkout_session_id: data.checkout_session_id,
			user_id: data.user_id,
			price_id: data.price_id,
			product_type: data.product_type,
			status: data.status,
			is_gift: data.is_gift,
			created_at: data.created_at,
			stripe_customer_id: null,
			payment_intent_id: null,
			subscription_id: null,
			invoice_id: null,
			amount_cents: 0,
			currency: '',
			gift_code: null,
			completed_at: null,
			version: 1,
		};

		batch.addPrepared(Payments.upsertAll(paymentRow));

		batch.addPrepared(
			PaymentsByUser.upsertAll({
				user_id: data.user_id,
				created_at: data.created_at,
				checkout_session_id: data.checkout_session_id,
			}),
		);

		await batch.execute();
	}

	async updatePayment(data: Partial<PaymentRow> & {checkout_session_id: string}): Promise<{applied: boolean}> {
		const checkoutSessionId = data.checkout_session_id;

		const result = await executeVersionedUpdate(
			() =>
				fetchOne<PaymentRow>(FETCH_PAYMENT_BY_CHECKOUT_SESSION_QUERY, {
					checkout_session_id: checkoutSessionId,
				}),
			(current) => {
				type PatchOp = ReturnType<typeof Db.set> | ReturnType<typeof Db.clear>;
				const patch: Record<string, PatchOp> = {};

				const addField = <K extends keyof PaymentRow>(key: K) => {
					const newVal = data[key];
					const oldVal = current?.[key];
					if (newVal === null) {
						if (current && oldVal !== null && oldVal !== undefined) {
							patch[key] = Db.clear();
						}
					} else if (newVal !== undefined) {
						patch[key] = Db.set(newVal);
					}
				};

				addField('stripe_customer_id');
				addField('payment_intent_id');
				addField('subscription_id');
				addField('invoice_id');
				addField('amount_cents');
				addField('currency');
				addField('status');
				addField('gift_code');
				addField('completed_at');

				return {
					pk: {checkout_session_id: checkoutSessionId},
					patch,
				};
			},
			Payments,
			{onFailure: 'throw'},
		);

		if (result.applied) {
			await this.updatePaymentIndexes(data);
		}

		return result;
	}

	private async updatePaymentIndexes(data: Partial<PaymentRow> & {checkout_session_id: string}): Promise<void> {
		const batch = new BatchBuilder();

		if (data.payment_intent_id) {
			batch.addPrepared(
				PaymentsByPaymentIntent.upsertAll({
					payment_intent_id: data.payment_intent_id,
					checkout_session_id: data.checkout_session_id,
				}),
			);
		}

		if (data.subscription_id) {
			const payment = await this.getPaymentByCheckoutSession(data.checkout_session_id);
			if (payment?.priceId && payment.productType) {
				batch.addPrepared(
					PaymentsBySubscription.upsertAll({
						subscription_id: data.subscription_id,
						checkout_session_id: data.checkout_session_id,
						user_id: payment.userId,
						price_id: payment.priceId,
						product_type: payment.productType,
					}),
				);
			}
		}

		await batch.execute();
	}

	async getPaymentByCheckoutSession(checkoutSessionId: string): Promise<Payment | null> {
		const result = await fetchOne<PaymentRow>(FETCH_PAYMENT_BY_CHECKOUT_SESSION_QUERY, {
			checkout_session_id: checkoutSessionId,
		});
		return result ? new Payment(result) : null;
	}

	async getPaymentByPaymentIntent(paymentIntentId: string): Promise<Payment | null> {
		const mapping = await fetchOne<{checkout_session_id: string}>(FETCH_PAYMENT_BY_PAYMENT_INTENT_QUERY, {
			payment_intent_id: paymentIntentId,
		});
		if (!mapping) return null;
		return this.getPaymentByCheckoutSession(mapping.checkout_session_id);
	}

	async getSubscriptionInfo(subscriptionId: string): Promise<PaymentBySubscriptionRow | null> {
		const result = await fetchOne<PaymentBySubscriptionRow>(FETCH_PAYMENT_BY_SUBSCRIPTION_QUERY, {
			subscription_id: subscriptionId,
		});
		return result ?? null;
	}

	async findPaymentsByUserId(userId: UserID): Promise<Array<Payment>> {
		const paymentRefs = await fetchMany<{checkout_session_id: string}>(FETCH_PAYMENTS_BY_USER_QUERY, {
			user_id: userId,
		});
		if (paymentRefs.length === 0) return [];
		const rows = await fetchMany<PaymentRow>(FETCH_PAYMENTS_BY_IDS_QUERY, {
			checkout_session_ids: paymentRefs.map((r) => r.checkout_session_id),
		});
		return rows.map((r) => new Payment(r));
	}
}
