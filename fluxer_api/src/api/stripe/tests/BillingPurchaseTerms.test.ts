// SPDX-License-Identifier: AGPL-3.0-or-later

import {purchaseTermsFromMetadata} from '@app/api/stripe/services/BillingInvoiceDetails';
import {describe, expect, test} from 'vitest';

const EU = {eu_withdrawal_waiver_required: 'true'};

function lastDay(metadata: Record<string, string>, paidAt: string): string | null {
	return purchaseTermsFromMetadata(metadata, new Date(paidAt), null).withdrawalEndsOn;
}

describe('Plutonium receipt withdrawal deadline', () => {
	test('moves a deadline that falls on a Saturday to the next working day', () => {
		expect(lastDay(EU, '2026-10-31T12:00:00Z')).toBe('2026-11-16');
	});

	test('moves a deadline that falls on a Swedish public holiday to the next working day', () => {
		expect(lastDay(EU, '2026-12-23T12:00:00Z')).toBe('2027-01-07');
	});

	test('gives a Brazilian consumer 7 days from the payment', () => {
		expect(lastDay({withdrawal_country: 'BR'}, '2026-10-31T12:00:00Z')).toBe('2026-11-07');
	});

	test('gives no withdrawal deadline outside the EU, the EEA, the UK and Brazil', () => {
		expect(lastDay({eu_withdrawal_waiver_required: 'false'}, '2026-10-31T12:00:00Z')).toBeNull();
	});

	test('repeats the immediate start and the days-used rule on a receipt for a purchase the checkout box covered', () => {
		const paidAt = new Date('2026-11-02T12:00:00Z');
		expect(purchaseTermsFromMetadata({...EU, withdrawal_pro_rata: 'true'}, paidAt, null)).toMatchObject({
			consentAt: paidAt,
			withdrawalProRata: true,
		});
		expect(purchaseTermsFromMetadata(EU, paidAt, null)).toMatchObject({consentAt: null, withdrawalProRata: false});
	});
});
