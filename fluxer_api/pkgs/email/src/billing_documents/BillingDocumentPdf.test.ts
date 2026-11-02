// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingDocumentBuyer} from '@pkgs/email/src/billing_documents/BillingDocumentContent';
import {buyerNameLines, money} from '@pkgs/email/src/billing_documents/BillingDocumentPdf';
import {describe, expect, test} from 'vitest';

const BUYER: BillingDocumentBuyer = {
	business: true,
	name: null,
	businessName: null,
	email: null,
	address: null,
	country: 'DE',
	vatId: null,
	vatIdVerification: null,
};

describe('billing document text', () => {
	test('prints the currency code once', () => {
		const text = (amountMinor: number, currency: string) => money(amountMinor, currency, 'en').replace(/\s/gu, ' ');
		expect(text(39200, 'sek')).toBe('SEK 392.00');
		expect(text(-8000, 'nok')).toBe('-NOK 80.00');
		expect(text(499, 'eur')).toBe('€4.99 EUR');
	});

	test('prints a business name that Stripe also stored as the customer name once', () => {
		expect(buyerNameLines({...BUYER, name: 'Beispiel Abo GmbH', businessName: 'Beispiel Abo GmbH'})).toEqual([
			'Beispiel Abo GmbH',
		]);
		expect(buyerNameLines({...BUYER, name: 'Erika Muster', businessName: 'Beispiel Abo GmbH'})).toEqual([
			'Beispiel Abo GmbH',
			'Erika Muster',
		]);
		expect(buyerNameLines({...BUYER, name: 'Erika Muster'})).toEqual(['Erika Muster']);
	});
});
