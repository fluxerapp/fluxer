// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const BUY_GIFT_DESCRIPTOR = msg({
	message: 'Buy gift',
	comment: 'Checkout button for buying a gift subscription.',
});
export const MANAGE_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Manage subscription',
	comment: 'Billing action that opens subscription management or the customer billing portal.',
});
export const CLAIM_ACCOUNT_TO_PURCHASE_OR_REDEEM_PREMIUM_DESCRIPTOR = msg({
	message: 'Claim your account to purchase or redeem {premiumProductFullName}.',
	comment:
		'Billing tooltip shown when an unclaimed account cannot buy or redeem premium. premiumProductFullName is the full paid product name.',
});
export const GIFT_PREMIUM_DESCRIPTOR = msg({
	message: 'Gift {premiumProductName}',
	comment: 'Premium gift purchase section title. premiumProductName is the short paid product name.',
});
export const PREMIUM_SUBSCRIPTION_DESCRIPTOR = msg({
	message: '{premiumProductName} subscription',
	comment: 'Premium subscription card title. premiumProductName is the short paid product name.',
});
export const VIEW_PLANS_DESCRIPTOR = msg({
	message: 'View plans',
	comment: 'Button in a premium upsell banner that opens available paid plans.',
});

export const CUSTOMER_PORTAL_FAILED_TITLE_DESCRIPTOR = msg({
	message: "Couldn't open the billing portal",
	comment:
		'Title of the error modal shown when opening the billing customer portal from the premium-expired nagbar fails.',
});
export const CUSTOMER_PORTAL_FAILED_MESSAGE_DESCRIPTOR = msg({
	message: 'Something went wrong while opening the billing portal. Please try again in a moment.',
	comment:
		'Body of the error modal shown when opening the billing customer portal from the premium-expired nagbar fails.',
});
