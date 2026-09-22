// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {PaymentRow} from '@app/api/database/types/PaymentTypes';

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
	readonly purchaseGeoipCountryCode: string | null;
	readonly purchaseClientCountryCode: string | null;
	readonly euWithdrawalWaiverRequired: boolean;
	readonly euWithdrawalWaiverAccepted: boolean;
	readonly euWithdrawalWaiverAcceptedAt: Date | null;
	readonly euWithdrawalWaiverTextVersion: string | null;
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
		this.purchaseGeoipCountryCode = row.purchase_geoip_country_code ?? null;
		this.purchaseClientCountryCode = row.purchase_client_country_code ?? null;
		this.euWithdrawalWaiverRequired = row.eu_withdrawal_waiver_required ?? false;
		this.euWithdrawalWaiverAccepted = row.eu_withdrawal_waiver_accepted ?? false;
		this.euWithdrawalWaiverAcceptedAt = row.eu_withdrawal_waiver_accepted_at ?? null;
		this.euWithdrawalWaiverTextVersion = row.eu_withdrawal_waiver_text_version ?? null;
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
			purchase_geoip_country_code: this.purchaseGeoipCountryCode,
			purchase_client_country_code: this.purchaseClientCountryCode,
			eu_withdrawal_waiver_required: this.euWithdrawalWaiverRequired,
			eu_withdrawal_waiver_accepted: this.euWithdrawalWaiverAccepted,
			eu_withdrawal_waiver_accepted_at: this.euWithdrawalWaiverAcceptedAt,
			eu_withdrawal_waiver_text_version: this.euWithdrawalWaiverTextVersion,
			created_at: this.createdAt,
			completed_at: this.completedAt,
			version: this.version,
		};
	}
}
