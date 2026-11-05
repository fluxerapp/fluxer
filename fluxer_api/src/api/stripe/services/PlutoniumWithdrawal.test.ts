// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	opensPlutoniumWithdrawalPeriod,
	plutoniumWithdrawalPeriod,
	proRataWithdrawalRefundMinor,
} from '@app/api/stripe/services/PlutoniumWithdrawal';
import {describe, expect, test} from 'vitest';

const PAID_AT = new Date('2026-11-02T12:00:00Z');

describe('plutoniumWithdrawalPeriod', () => {
	test('gives EU and EEA consumers 14 days, moved past a weekend', () => {
		const period = plutoniumWithdrawalPeriod({purchase_geoip_country_code: 'DE'}, PAID_AT);
		expect(period?.rule).toBe('eu');
		expect(period?.endsOn).toBe('2026-11-16');
	});

	test('gives UK consumers exactly 14 days, ending at midnight UK time with no weekend move', () => {
		const period = plutoniumWithdrawalPeriod(
			{country_code: 'GB', eu_withdrawal_waiver_required: 'true'},
			new Date('2026-11-07T12:00:00Z'),
		);
		expect(period).toEqual({endsOn: '2026-11-21', endsAt: new Date('2026-11-22T00:00:00Z'), rule: 'uk'});
		expect(plutoniumWithdrawalPeriod({country_code: 'GB'}, new Date('2026-06-01T23:30:00Z'))?.endsAt).toEqual(
			new Date('2026-06-16T23:00:00Z'),
		);
	});

	test('gives Brazilian consumers 7 days ending 23:59 UTC-5, business buyers without a CNPJ included', () => {
		const period = plutoniumWithdrawalPeriod({withdrawal_country: 'BR', is_business: 'true'}, PAID_AT);
		expect(period?.rule).toBe('brazil');
		expect(period?.endsOn).toBe('2026-11-09');
		expect(period?.endsAt.toISOString()).toBe('2026-11-10T05:00:00.000Z');
	});

	test('gives business buyers with a CNPJ, business buyers elsewhere and other buyers no period', () => {
		expect(
			plutoniumWithdrawalPeriod({withdrawal_country: 'BR', is_business: 'true', cnpj: '11222333000181'}, PAID_AT),
		).toBeNull();
		expect(plutoniumWithdrawalPeriod({purchase_geoip_country_code: 'SE', is_business: 'true'}, PAID_AT)).toBeNull();
		expect(plutoniumWithdrawalPeriod({purchase_geoip_country_code: 'US'}, PAID_AT)).toBeNull();
		expect(plutoniumWithdrawalPeriod(null, PAID_AT)).toBeNull();
	});
});

describe('opensPlutoniumWithdrawalPeriod', () => {
	const renewal = {billingReason: 'subscription_cycle', priceId: 'price_monthly', metadata: null};
	const proRataRenewal = {...renewal, metadata: {withdrawal_pro_rata: 'true'}};

	test('lets a renewal open a period only for an EU subscription started under the earlier terms', () => {
		expect(opensPlutoniumWithdrawalPeriod('eu', renewal, 'price_monthly')).toBe(true);
		expect(opensPlutoniumWithdrawalPeriod('eu', proRataRenewal, 'price_monthly')).toBe(false);
		expect(opensPlutoniumWithdrawalPeriod('uk', proRataRenewal, 'price_monthly')).toBe(false);
		expect(opensPlutoniumWithdrawalPeriod('brazil', renewal, 'price_monthly')).toBe(false);
	});

	test('opens a period on the first payment and on a plan switch', () => {
		for (const rule of ['eu', 'uk', 'brazil'] as const) {
			expect(
				opensPlutoniumWithdrawalPeriod(rule, {...proRataRenewal, billingReason: 'subscription_create'}, null),
			).toBe(true);
			expect(
				opensPlutoniumWithdrawalPeriod(
					rule,
					{...proRataRenewal, billingReason: 'subscription_update', priceId: 'price_yearly'},
					null,
				),
			).toBe(true);
			expect(opensPlutoniumWithdrawalPeriod(rule, proRataRenewal, 'price_yearly')).toBe(true);
		}
	});
});

describe('proRataWithdrawalRefundMinor', () => {
	const usage = {startedAt: new Date('2026-11-01T10:00:00Z'), endsAt: new Date('2026-12-01T10:00:00Z')};

	test('keeps the share for each started day of the period', () => {
		expect(proRataWithdrawalRefundMinor(3000, usage, new Date('2026-11-04T09:00:00Z'))).toBe(2700);
		expect(proRataWithdrawalRefundMinor(3000, usage, new Date('2026-11-01T10:00:00Z'))).toBe(3000);
	});

	test('refunds everything when nothing was used yet', () => {
		expect(proRataWithdrawalRefundMinor(3000, null, new Date('2026-11-04T09:00:00Z'))).toBe(3000);
	});
});
