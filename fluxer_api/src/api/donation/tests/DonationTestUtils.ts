// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {
	DONATION_CURRENCIES,
	type DonationCurrency,
	getDonationAmountConstraints,
} from '@fluxer/schema/src/domains/donation/DonationAmountUtils';

interface DonationTestEmailRecord {
	to: string;
	type: string;
	timestamp: string;
	metadata: Record<string, string>;
}

export async function listDonationTestEmails(
	harness: ApiTestHarness,
	params?: {
		recipient?: string;
	},
): Promise<Array<DonationTestEmailRecord>> {
	const query = params?.recipient ? `?recipient=${encodeURIComponent(params.recipient)}` : '';
	const response = await createBuilderWithoutAuth<{
		emails: Array<DonationTestEmailRecord>;
	}>(harness)
		.get(`/test/emails${query}`)
		.execute();
	return response.emails;
}

export async function clearDonationTestEmails(harness: ApiTestHarness): Promise<void> {
	await createBuilderWithoutAuth(harness).delete('/test/emails').expect(204).execute();
}

export const TEST_DONOR_EMAIL = 'donor@test.com';
export const TEST_MAGIC_LINK_TOKEN = 'a'.repeat(64);
export const TEST_INVALID_TOKEN = 'invalid-token-too-short';
export const DONATION_AMOUNTS = {
	MINIMUM: getDonationAmountConstraints('usd').minimumAmountMinor,
	BELOW_MINIMUM: getDonationAmountConstraints('usd').minimumAmountMinor - 100,
	STANDARD: 2500,
	ABOVE_MAXIMUM: getDonationAmountConstraints('usd').maximumAmountMinor + 100,
	MAXIMUM: getDonationAmountConstraints('usd').maximumAmountMinor,
} as const;
export const DONATION_CURRENCY_VALUES = {
	USD: DONATION_CURRENCIES[0],
	EUR: DONATION_CURRENCIES[1],
	BRL: DONATION_CURRENCIES[2],
	INR: DONATION_CURRENCIES[3],
	PLN: DONATION_CURRENCIES[4],
	TRY: DONATION_CURRENCIES[5],
	SEK: DONATION_CURRENCIES[6],
	DKK: DONATION_CURRENCIES[7],
	NOK: DONATION_CURRENCIES[8],
} as const;
export const DONATION_INTERVALS = {
	MONTH: 'month',
	YEAR: 'year',
} as const;

interface DonationCheckoutRequestBody {
	email: string;
	amount_cents: number;
	currency: DonationCurrency;
	interval: 'month' | 'year';
}

interface DonationCheckoutResponse {
	url: string;
}

export function createDonationRequestLinkBuilder(harness: ApiTestHarness): TestRequestBuilder<void> {
	return createBuilderWithoutAuth<void>(harness).post('/donations/request-link');
}

export function createDonationCheckoutBuilder(harness: ApiTestHarness): TestRequestBuilder<DonationCheckoutResponse> {
	return createBuilderWithoutAuth<DonationCheckoutResponse>(harness).post('/donations/checkout');
}

export function createDonationManageBuilder(harness: ApiTestHarness, token: string): TestRequestBuilder<void> {
	return createBuilderWithoutAuth<void>(harness).get(`/donations/manage?token=${encodeURIComponent(token)}`);
}

export function createValidCheckoutBody(overrides?: Partial<DonationCheckoutRequestBody>): DonationCheckoutRequestBody {
	return {
		email: TEST_DONOR_EMAIL,
		amount_cents: DONATION_AMOUNTS.STANDARD,
		currency: DONATION_CURRENCY_VALUES.USD,
		interval: DONATION_INTERVALS.MONTH,
		...overrides,
	};
}

export function createUniqueEmail(prefix = 'donation'): string {
	return `${prefix}-${randomUUID()}@test.com`;
}
