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
export const PAYMENT_DISPUTE_OPEN_DESCRIPTOR = msg({
	message:
		'Purchases are paused while a payment dispute on your account is open. They come back on their own when the dispute closes.',
	comment:
		'Shown wherever something can be bought while a payment dispute this account made is open. A payment dispute is a chargeback filed with a bank or card company. Every purchase is paused until the bank closes the dispute, then purchases come back without the person doing anything. Neutral tone, never suggest the person did something wrong.',
});
export const PURCHASES_PAUSED_TITLE_DESCRIPTOR = msg({
	message: 'Purchases paused',
	comment: 'Dialog title when checkout is refused because a payment dispute this account made is still open.',
});
export const OPERATOR_PASS_OFFER_BONUS_DESCRIPTOR = msg({
	message:
		"With a pass on your account, you have the Operator badge, a spot on the Operators' wall and #operators in {labsName}.",
	comment:
		"Line in the dialog for an Operator Pass someone passed on to you. The Operators' wall is the list of Operators on the homepage. Operator is the badge name and #operators is a channel name, keep both as is. {labsName} is the community name. Name these things plainly and never call them a bonus, a perk, free or included.",
});
export const OPERATOR_PASS_LISTING_UNCHANGED_DESCRIPTOR = msg({
	message: "Your Operators' wall listing stays the same.",
	comment:
		"Line for someone who already has an Operator Pass and gets another one, at checkout or when accepting an offer. The Operators' wall is the list of Operators on the homepage.",
});
export const OPERATOR_PASS_AGE_CONFIRMATION_DESCRIPTOR = msg({
	message: "I'm 18 or older, or have a parent's or guardian's permission.",
	comment:
		'Required checkbox in the Operator Pass checkout dialog for every purchase, and in the dialog where a Visionary turns on the Operator Pass that comes with Visionary. The person confirms their age or that a parent or guardian allows it.',
});
export const OPERATOR_PASS_REQUIRED_BOX_DESCRIPTOR = msg({
	message: 'Check this box to continue.',
	comment:
		'Error under a required checkbox in the Operator Pass checkout dialog or the dialog for an Operator Pass someone offered you, shown when they try to continue without checking it.',
});
export const OPEN_OPERATORS_DESCRIPTOR = msg({
	message: 'Open #operators',
	comment:
		'Button after an Operator Pass purchase, in the Operator Pass dialog and in the Operator Pass settings. It takes the person to #operators, a channel in the Fluxer Labs community, and adds them to Fluxer Labs first when the pass allows it. Keep #operators as is, it is the channel name.',
});
export const WALL_SAVE_FAILED_DESCRIPTOR = msg({
	message: "Couldn't save. Try again.",
	comment: "Toast when saving the Operators' wall settings or the welcome in #operators failed.",
});
export const SUBSCRIPTION_SECTION_DESCRIPTOR = msg({
	message: 'Subscription',
	comment: 'Name of the section in the Subscriptions settings tab that shows the Plutonium subscription.',
});
export const PAYMENT_METHODS_SECTION_DESCRIPTOR = msg({
	message: 'Payment methods',
	comment: 'Heading of the section in the Payments settings tab for the cards and other payment methods on file.',
});
export const REFUNDS_SECTION_DESCRIPTOR = msg({
	message: 'Refunds',
	comment:
		'Heading of the section in the Payments settings tab that lists purchases the user can still get money back for.',
});
export const RECEIPTS_SECTION_DESCRIPTOR = msg({
	message: 'Receipts',
	comment: 'Heading of the section in the Payments settings tab that lists the receipts for past payments.',
});
export const OPERATOR_PASS_PASSES_SECTION_DESCRIPTOR = msg({
	message: 'Your passes',
	comment: 'Name of the section in the Operator Pass settings tab that lists the Operator Passes the account holds.',
});
export const OPERATOR_PASS_WALL_SECTION_DESCRIPTOR = msg({
	message: "Operators' wall",
	comment:
		"Heading of the Operators' wall section in the Operator Pass settings tab. Operators' wall is the name of a section of the homepage.",
});
export const OPERATOR_PASS_LABS_SECTION_DESCRIPTOR = msg({
	message: 'Fluxer Labs',
	comment:
		'Name of the section in the Operator Pass settings tab about the Fluxer Labs community. Fluxer Labs is the name of a community, keep it as is.',
});
