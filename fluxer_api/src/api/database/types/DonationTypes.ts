// SPDX-License-Identifier: AGPL-3.0-or-later

export interface DonorRow {
	email: string;
	stripe_customer_id: string | null;
	business_name: string | null;
	tax_id: string | null;
	tax_id_type: string | null;
	stripe_subscription_id: string | null;
	subscription_amount_cents: number | null;
	subscription_currency: string | null;
	subscription_interval: string | null;
	subscription_current_period_end: Date | null;
	subscription_cancel_at: Date | null;
	subscription_status: string | null;
	created_at: Date;
	updated_at: Date;
	version: number;
}

export interface DonorByStripeCustomerIdRow {
	stripe_customer_id: string;
	email: string;
}

export interface DonorByStripeSubscriptionIdRow {
	stripe_subscription_id: string;
	email: string;
}

export interface DonorMagicLinkTokenRow {
	token_: string;
	donor_email: string;
	expires_at: Date;
	used_at: Date | null;
}

export interface DonorMagicLinkTokenByEmailRow {
	donor_email: string;
	token_: string;
}

export const DONOR_COLUMNS = [
	'email',
	'stripe_customer_id',
	'business_name',
	'tax_id',
	'tax_id_type',
	'stripe_subscription_id',
	'subscription_amount_cents',
	'subscription_currency',
	'subscription_interval',
	'subscription_current_period_end',
	'subscription_cancel_at',
	'subscription_status',
	'created_at',
	'updated_at',
	'version',
] as const satisfies ReadonlyArray<keyof DonorRow>;
export const DONOR_BY_STRIPE_CUSTOMER_ID_COLUMNS = ['stripe_customer_id', 'email'] as const satisfies ReadonlyArray<
	keyof DonorByStripeCustomerIdRow
>;
export const DONOR_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS = [
	'stripe_subscription_id',
	'email',
] as const satisfies ReadonlyArray<keyof DonorByStripeSubscriptionIdRow>;
export const DONOR_MAGIC_LINK_TOKEN_COLUMNS = [
	'token_',
	'donor_email',
	'expires_at',
	'used_at',
] as const satisfies ReadonlyArray<keyof DonorMagicLinkTokenRow>;
export const DONOR_MAGIC_LINK_TOKEN_BY_EMAIL_COLUMNS = ['donor_email', 'token_'] as const satisfies ReadonlyArray<
	keyof DonorMagicLinkTokenByEmailRow
>;
