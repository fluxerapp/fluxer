// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DONATION_CURRENCIES,
	getDonationAmountConstraints,
} from '@fluxer/schema/src/domains/donation/DonationAmountUtils';
import {z} from 'zod';

export const DonationRequestLinkRequest = z.object({
	email: z.email().max(254).describe('Email address to send the magic link to'),
});

export type DonationRequestLinkRequest = z.infer<typeof DonationRequestLinkRequest>;

export const DonationManageQuery = z.object({
	token: z.string().length(64).describe('Magic link token for donor authentication'),
});

export type DonationManageQuery = z.infer<typeof DonationManageQuery>;

export const DonationCurrency = z.enum(DONATION_CURRENCIES);

export type DonationCurrency = z.infer<typeof DonationCurrency>;

export const DonationCheckoutRequest = z
	.object({
		email: z.email().max(254).describe('Donor email address'),
		amount_cents: z.number().int().describe('Donation amount in minor units for the selected currency'),
		currency: DonationCurrency.describe('Currency for the donation'),
		interval: z.enum(['month', 'year']).nullable().describe('Billing interval (null for one-time donation)'),
		is_business: z
			.boolean()
			.optional()
			.describe(
				'Whether the donation is from a business. When true, Stripe Checkout requires a billing address for tax invoicing. When false or omitted, billing address collection is left to Stripe.',
			),
	})
	.superRefine((value, ctx) => {
		const constraints = getDonationAmountConstraints(value.currency);
		if (value.amount_cents < constraints.minimumAmountMinor || value.amount_cents > constraints.maximumAmountMinor) {
			ctx.addIssue({
				code: 'custom',
				path: ['amount_cents'],
				message: 'Donation amount is outside the allowed range for the selected currency',
			});
		}
	});

export type DonationCheckoutRequest = z.infer<typeof DonationCheckoutRequest>;

export const DonationCheckoutResponse = z.object({
	url: z.url().describe('Stripe checkout URL to redirect the user to'),
});

export type DonationCheckoutResponse = z.infer<typeof DonationCheckoutResponse>;
