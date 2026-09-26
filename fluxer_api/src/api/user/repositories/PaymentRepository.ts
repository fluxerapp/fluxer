// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp} from '@app/api/database/CassandraTypes';
import {executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {PaymentBySubscriptionRow, PaymentRow} from '@app/api/database/types/PaymentTypes';
import {Payment} from '@app/api/models/Payment';
import {Payments, PaymentsByPaymentIntent, PaymentsBySubscription, PaymentsByUser} from '@app/api/Tables';

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
		purchase_geoip_country_code?: string | null;
		purchase_client_country_code?: string | null;
		eu_withdrawal_waiver_required?: boolean;
		eu_withdrawal_waiver_accepted?: boolean;
		eu_withdrawal_waiver_accepted_at?: Date | null;
		eu_withdrawal_waiver_text_version?: string | null;
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
			purchase_geoip_country_code: data.purchase_geoip_country_code ?? null,
			purchase_client_country_code: data.purchase_client_country_code ?? null,
			eu_withdrawal_waiver_required: data.eu_withdrawal_waiver_required ?? false,
			eu_withdrawal_waiver_accepted: data.eu_withdrawal_waiver_accepted ?? false,
			eu_withdrawal_waiver_accepted_at: data.eu_withdrawal_waiver_accepted_at ?? null,
			eu_withdrawal_waiver_text_version: data.eu_withdrawal_waiver_text_version ?? null,
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

	async updatePayment(
		data: Partial<PaymentRow> & {
			checkout_session_id: string;
		},
	): Promise<void> {
		const checkoutSessionId = data.checkout_session_id;
		await executeVersionedUpdate(
			() =>
				fetchOne<PaymentRow>(FETCH_PAYMENT_BY_CHECKOUT_SESSION_QUERY, {
					checkout_session_id: checkoutSessionId,
				}),
			(current) => {
				type PatchOp = DbOp<unknown>;
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
				addField('purchase_geoip_country_code');
				addField('purchase_client_country_code');
				addField('eu_withdrawal_waiver_required');
				addField('eu_withdrawal_waiver_accepted');
				addField('eu_withdrawal_waiver_accepted_at');
				addField('eu_withdrawal_waiver_text_version');
				addField('completed_at');
				return {
					pk: {checkout_session_id: checkoutSessionId},
					patch,
				};
			},
			Payments,
		);
		await this.updatePaymentIndexes(data);
	}

	private async updatePaymentIndexes(
		data: Partial<PaymentRow> & {
			checkout_session_id: string;
		},
	): Promise<void> {
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
		const mapping = await fetchOne<{
			checkout_session_id: string;
		}>(FETCH_PAYMENT_BY_PAYMENT_INTENT_QUERY, {
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

	async hasEverPaidSuccessfully(userId: UserID): Promise<boolean> {
		const refs = await fetchMany<{
			checkout_session_id: string;
		}>(FETCH_PAYMENTS_BY_USER_QUERY, {
			user_id: userId,
		});
		if (refs.length === 0) return false;
		const rows = await fetchMany<PaymentRow>(FETCH_PAYMENTS_BY_IDS_QUERY, {
			checkout_session_ids: refs.map((r) => r.checkout_session_id),
		});
		return rows.some((r) => r.status === 'completed');
	}

	async findPaymentsByUserId(userId: UserID): Promise<Array<Payment>> {
		const paymentRefs = await fetchMany<{
			checkout_session_id: string;
		}>(FETCH_PAYMENTS_BY_USER_QUERY, {
			user_id: userId,
		});
		if (paymentRefs.length === 0) return [];
		const rows = await fetchMany<PaymentRow>(FETCH_PAYMENTS_BY_IDS_QUERY, {
			checkout_session_ids: paymentRefs.map((r) => r.checkout_session_id),
		});
		return rows.map((r) => new Payment(r));
	}
}
