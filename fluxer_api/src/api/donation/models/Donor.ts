// SPDX-License-Identifier: AGPL-3.0-or-later

import {nextVersion} from '@app/api/database/CassandraTypes';
import type {DonorRow} from '@app/api/database/types/DonationTypes';

export class Donor {
	readonly email: string;
	readonly stripeCustomerId: string | null;
	readonly businessName: string | null;
	readonly taxId: string | null;
	readonly taxIdType: string | null;
	readonly stripeSubscriptionId: string | null;
	readonly subscriptionAmountCents: number | null;
	readonly subscriptionCurrency: string | null;
	readonly subscriptionInterval: string | null;
	readonly subscriptionCurrentPeriodEnd: Date | null;
	readonly subscriptionCancelAt: Date | null;
	readonly createdAt: Date;
	readonly updatedAt: Date;
	readonly version: number;

	constructor(row: DonorRow) {
		this.email = row.email;
		this.stripeCustomerId = row.stripe_customer_id ?? null;
		this.businessName = row.business_name ?? null;
		this.taxId = row.tax_id ?? null;
		this.taxIdType = row.tax_id_type ?? null;
		this.stripeSubscriptionId = row.stripe_subscription_id ?? null;
		this.subscriptionAmountCents = row.subscription_amount_cents ?? null;
		this.subscriptionCurrency = row.subscription_currency ?? null;
		this.subscriptionInterval = row.subscription_interval ?? null;
		this.subscriptionCurrentPeriodEnd = row.subscription_current_period_end ?? null;
		this.subscriptionCancelAt = row.subscription_cancel_at ?? null;
		this.createdAt = row.created_at;
		this.updatedAt = row.updated_at;
		this.version = row.version;
	}

	toRow(): DonorRow {
		return {
			email: this.email,
			stripe_customer_id: this.stripeCustomerId,
			business_name: this.businessName,
			tax_id: this.taxId,
			tax_id_type: this.taxIdType,
			stripe_subscription_id: this.stripeSubscriptionId,
			subscription_amount_cents: this.subscriptionAmountCents,
			subscription_currency: this.subscriptionCurrency,
			subscription_interval: this.subscriptionInterval,
			subscription_current_period_end: this.subscriptionCurrentPeriodEnd,
			subscription_cancel_at: this.subscriptionCancelAt,
			created_at: this.createdAt,
			updated_at: this.updatedAt,
			version: this.version,
		};
	}

	hasActiveSubscription(): boolean {
		if (!this.stripeSubscriptionId || !this.subscriptionCurrentPeriodEnd) {
			return false;
		}
		if (this.subscriptionCancelAt !== null) {
			return false;
		}
		return this.subscriptionCurrentPeriodEnd > new Date();
	}

	isBusiness(): boolean {
		return this.taxId !== null;
	}

	withUpdatedSubscription(data: {
		stripeCustomerId: string | null;
		businessName?: string | null;
		taxId?: string | null;
		taxIdType?: string | null;
		stripeSubscriptionId: string | null;
		subscriptionAmountCents: number | null;
		subscriptionCurrency: string | null;
		subscriptionInterval: string | null;
		subscriptionCurrentPeriodEnd: Date | null;
		subscriptionCancelAt?: Date | null;
	}): DonorRow {
		return {
			email: this.email,
			stripe_customer_id: data.stripeCustomerId,
			business_name: data.businessName ?? this.businessName,
			tax_id: data.taxId ?? this.taxId,
			tax_id_type: data.taxIdType ?? this.taxIdType,
			stripe_subscription_id: data.stripeSubscriptionId,
			subscription_amount_cents: data.subscriptionAmountCents,
			subscription_currency: data.subscriptionCurrency,
			subscription_interval: data.subscriptionInterval,
			subscription_current_period_end: data.subscriptionCurrentPeriodEnd,
			subscription_cancel_at: data.subscriptionCancelAt ?? this.subscriptionCancelAt,
			created_at: this.createdAt,
			updated_at: new Date(),
			version: nextVersion(this.version),
		};
	}
}
