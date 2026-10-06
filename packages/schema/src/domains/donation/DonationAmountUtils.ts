// SPDX-License-Identifier: AGPL-3.0-or-later

export const DONATION_CURRENCIES = ['usd', 'eur', 'brl', 'inr', 'pln', 'try', 'sek', 'dkk', 'nok'] as const;

export type DonationCurrency = (typeof DONATION_CURRENCIES)[number];

interface DonationAmountConstraints {
	minimumAmountMinor: number;
	maximumAmountMinor: number;
}

const STRIPE_MAXIMUM_AMOUNT_MINOR = 99_999_999;

const DONATION_AMOUNT_CONSTRAINTS: Readonly<Record<DonationCurrency, DonationAmountConstraints>> = {
	usd: {minimumAmountMinor: 300, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	eur: {minimumAmountMinor: 300, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	brl: {minimumAmountMinor: 1000, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	inr: {minimumAmountMinor: 20000, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	pln: {minimumAmountMinor: 800, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	try: {minimumAmountMinor: 10000, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	sek: {minimumAmountMinor: 3000, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	dkk: {minimumAmountMinor: 2000, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
	nok: {minimumAmountMinor: 3000, maximumAmountMinor: STRIPE_MAXIMUM_AMOUNT_MINOR},
};

export function getDonationAmountConstraints(currency: DonationCurrency): DonationAmountConstraints {
	return DONATION_AMOUNT_CONSTRAINTS[currency];
}

export function isDonationAmountWithinConstraints(amountMinor: number, currency: DonationCurrency): boolean {
	const constraints = getDonationAmountConstraints(currency);
	return amountMinor >= constraints.minimumAmountMinor && amountMinor <= constraints.maximumAmountMinor;
}
