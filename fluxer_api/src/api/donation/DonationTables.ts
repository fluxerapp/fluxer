// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import {
	DONOR_BY_STRIPE_CUSTOMER_ID_COLUMNS,
	DONOR_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS,
	DONOR_COLUMNS,
	DONOR_MAGIC_LINK_TOKEN_BY_EMAIL_COLUMNS,
	DONOR_MAGIC_LINK_TOKEN_COLUMNS,
	type DonorByStripeCustomerIdRow,
	type DonorByStripeSubscriptionIdRow,
	type DonorMagicLinkTokenByEmailRow,
	type DonorMagicLinkTokenRow,
	type DonorRow,
} from '@app/api/database/types/DonationTypes';
import {seconds} from 'itty-time';

export const Donors = defineTable<DonorRow, 'email'>({
	name: 'donors',
	columns: DONOR_COLUMNS,
	primaryKey: ['email'],
});
export const DonorsByStripeCustomerId = defineTable<
	DonorByStripeCustomerIdRow,
	'stripe_customer_id' | 'email',
	'stripe_customer_id'
>({
	name: 'donors_by_stripe_customer_id',
	columns: DONOR_BY_STRIPE_CUSTOMER_ID_COLUMNS,
	primaryKey: ['stripe_customer_id', 'email'],
	partitionKey: ['stripe_customer_id'],
});
export const DonorsByStripeSubscriptionId = defineTable<
	DonorByStripeSubscriptionIdRow,
	'stripe_subscription_id' | 'email',
	'stripe_subscription_id'
>({
	name: 'donors_by_stripe_subscription_id',
	columns: DONOR_BY_STRIPE_SUBSCRIPTION_ID_COLUMNS,
	primaryKey: ['stripe_subscription_id', 'email'],
	partitionKey: ['stripe_subscription_id'],
});
export const DonorMagicLinkTokens = defineTable<DonorMagicLinkTokenRow, 'token_'>({
	name: 'donor_magic_link_tokens',
	columns: DONOR_MAGIC_LINK_TOKEN_COLUMNS,
	primaryKey: ['token_'],
	defaultTtlSeconds: seconds('15 minutes'),
});
export const DonorMagicLinkTokensByEmail = defineTable<DonorMagicLinkTokenByEmailRow, 'donor_email' | 'token_'>({
	name: 'donor_magic_link_tokens_by_email',
	columns: DONOR_MAGIC_LINK_TOKEN_BY_EMAIL_COLUMNS,
	primaryKey: ['donor_email', 'token_'],
	defaultTtlSeconds: seconds('15 minutes'),
});
