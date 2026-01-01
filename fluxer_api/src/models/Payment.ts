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

import type {PaymentRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';

export class Payment {
	readonly checkoutSessionId: string;
	readonly userId: UserID;
	readonly stripeCustomerId: string | null;
	readonly paymentIntentId: string | null;
	readonly subscriptionId: string | null;
	readonly invoiceId: string | null;
	readonly priceId: string | null;
	readonly productType: string | null;
	readonly amountCents: number;
	readonly currency: string;
	readonly status: string;
	readonly isGift: boolean;
	readonly giftCode: string | null;
	readonly createdAt: Date;
	readonly completedAt: Date | null;
	readonly version: number;

	constructor(row: PaymentRow) {
		this.checkoutSessionId = row.checkout_session_id;
		this.userId = row.user_id as UserID;
		this.stripeCustomerId = row.stripe_customer_id ?? null;
		this.paymentIntentId = row.payment_intent_id ?? null;
		this.subscriptionId = row.subscription_id ?? null;
		this.invoiceId = row.invoice_id ?? null;
		this.priceId = row.price_id ?? null;
		this.productType = row.product_type ?? null;
		this.amountCents = row.amount_cents;
		this.currency = row.currency;
		this.status = row.status;
		this.isGift = row.is_gift;
		this.giftCode = row.gift_code ?? null;
		this.createdAt = row.created_at;
		this.completedAt = row.completed_at ?? null;
		this.version = row.version;
	}

	toRow(): PaymentRow {
		return {
			checkout_session_id: this.checkoutSessionId,
			user_id: this.userId,
			stripe_customer_id: this.stripeCustomerId,
			payment_intent_id: this.paymentIntentId,
			subscription_id: this.subscriptionId,
			invoice_id: this.invoiceId,
			price_id: this.priceId,
			product_type: this.productType,
			amount_cents: this.amountCents,
			currency: this.currency,
			status: this.status,
			is_gift: this.isGift,
			gift_code: this.giftCode,
			created_at: this.createdAt,
			completed_at: this.completedAt,
			version: this.version,
		};
	}
}
