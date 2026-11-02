// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getCacheService, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCheckoutService} from '@app/api/operator_pass/OperatorPassCheckoutService';
import {OperatorPassEntitlementService} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {
	addOperatorPassPreviewEntry,
	findOperatorPassPreviewMatch,
	type OperatorPassPreviewEntry,
} from '@app/api/operator_pass/OperatorPassPreviewAccess';
import {planOperatorPassReset, resetOperatorPassPreviewData} from '@app/api/operator_pass/OperatorPassPreviewReset';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {OperatorPassNotOnSaleError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassNotOnSaleError';
import type {OperatorPassCheckoutRequest} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type Stripe from 'stripe';
import {afterAll, beforeAll, describe, expect, test, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;
const NOW = new Date('2026-10-31T20:00:00.000Z');

function entry(value: string, expiresAt: string | null = null): OperatorPassPreviewEntry {
	return {
		id: value,
		value,
		label: `label ${value}`,
		expires_at: expiresAt,
		created_at: NOW.toISOString(),
		created_by: '1',
	};
}

describe('Operator Pass preview IP matching', () => {
	test('exact addresses and CIDR ranges match in both families and nothing else does', () => {
		const entries = [entry('198.51.100.7'), entry('203.0.113.0/24'), entry('2001:db8:1:2::/64'), entry('2001:db8::5')];
		const label = (ip: string) => findOperatorPassPreviewMatch(entries, [], ip, NOW)?.label ?? null;
		expect(label('198.51.100.7')).toBe('label 198.51.100.7');
		expect(label('198.51.100.8')).toBeNull();
		expect(label('203.0.113.200')).toBe('label 203.0.113.0/24');
		expect(label('203.0.114.1')).toBeNull();
		expect(label('::ffff:203.0.113.9')).toBe('label 203.0.113.0/24');
		expect(label('2001:db8:1:2:aaaa:bbbb:cccc:dddd')).toBe('label 2001:db8:1:2::/64');
		expect(label('2001:db8:1:3::1')).toBeNull();
		expect(label('2001:0db8:0000:0000:0000:0000:0000:0005')).toBe('label 2001:db8::5');
		expect(label('2001:db8::6')).toBeNull();
	});

	test('an expired entry stops matching and the config list still applies', () => {
		const entries = [entry('198.51.100.7', '2026-10-31T19:59:59.000Z')];
		expect(findOperatorPassPreviewMatch(entries, [], '198.51.100.7', NOW)).toBeNull();
		expect(findOperatorPassPreviewMatch(entries, ['198.51.100.0/28'], '198.51.100.7', NOW)?.entryId).toBeNull();
	});
});

describe('Operator Pass preview before the sale opens', () => {
	let harness: ApiTestHarness;
	let saleOpensAt: Date | null;
	let priceRisesAt: Date | null;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		saleOpensAt = Config.operatorPass.saleOpensAt;
		priceRisesAt = Config.operatorPass.priceRisesAt;
		Config.operatorPass.saleOpensAt = new Date(Date.now() + DAY_MS);
		Config.operatorPass.priceRisesAt = new Date(Date.now() + 60 * DAY_MS);
		await addOperatorPassPreviewEntry(getKVClient(), {
			value: '2001:db8:42::/48',
			label: 'Hampus at home',
			expiresAt: null,
			createdBy: '1',
			now: new Date(),
		});
	});

	afterAll(async () => {
		Config.operatorPass.saleOpensAt = saleOpensAt;
		Config.operatorPass.priceRisesAt = priceRisesAt;
		await harness.shutdown();
	});

	async function setUp() {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();
		const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
		const createSession = vi.fn(async (params: Stripe.Checkout.SessionCreateParams) => ({
			id: `cs_test_preview_${account.userId}_${createSession.mock.calls.length}`,
			url: 'https://checkout.stripe.test/session',
			params,
		}));
		const createCoupon = vi.fn(async (params: Stripe.CouponCreateParams) => ({id: 'coupon_preview', ...params}));
		const stripe = {
			checkout: {sessions: {create: createSession}},
			coupons: {create: createCoupon},
		} as unknown as Stripe;
		const repository = new OperatorPassRepository();
		const service = new OperatorPassCheckoutService(stripe, repository, new OperatorPassEntitlementService(repository));
		const checkout = (clientIp: string) =>
			service.createCheckoutSession({
				user,
				request: {
					quantity: 2,
					certificate_names: ['Hampus Lindqvist'],
					wall_opt_out: true,
					age_confirmed: true,
					delivery_consent: true,
				} as OperatorPassCheckoutRequest,
				geoipCountryCode: 'SE',
				clientIp,
				userAgent: 'vitest',
			});
		return {service, repository, createSession, createCoupon, checkout, user};
	}

	test('a listed IP sees the sale open and checks out at no cost with a one-use 100% coupon', async () => {
		const {service, repository, createSession, createCoupon, checkout, user} = await setUp();
		const state = await service.getState({user, countryCode: 'SE', clientIp: '2001:db8:42:7::9'});
		expect(state.phase).toBe('early_bird');
		expect(state.purchase_blocked_reason).toBeNull();
		const {sessionId} = await checkout('2001:db8:42:7::9');
		expect(createCoupon.mock.calls[0]![0]).toMatchObject({percent_off: 100, duration: 'once', max_redemptions: 1});
		const params = createSession.mock.calls[0]![0];
		expect(params.discounts).toEqual([{coupon: 'coupon_preview'}]);
		expect(params.allow_promotion_codes).toBeUndefined();
		expect(params.payment_intent_data).toBeUndefined();
		expect(params.metadata).toMatchObject({
			operator_pass_preview: 'true',
			operator_pass_preview_label: 'Hampus at home',
			operator_pass_amount_minor: '0',
		});
		const purchase = await repository.findPurchase(sessionId);
		expect(purchase).toMatchObject({amount_minor: 0, preview_label: 'Hampus at home', phase: 'early_bird'});
	});

	test('an unlisted IP keeps the waitlist and can not check out', async () => {
		const {service, createSession, createCoupon, checkout, user} = await setUp();
		const state = await service.getState({user, countryCode: 'SE', clientIp: '2001:db8:43::9'});
		expect(state.phase).toBe('waitlist');
		expect(state.purchase_blocked_reason).toBe('not_on_sale');
		await expect(checkout('2001:db8:43::9')).rejects.toBeInstanceOf(OperatorPassNotOnSaleError);
		expect(createSession).not.toHaveBeenCalled();
		expect(createCoupon).not.toHaveBeenCalled();
	});

	test('the reset is refused while any purchase was not made through a preview entry', async () => {
		const {repository, user} = await setUp();
		await repository.createPurchase({
			checkout_session_id: 'cs_test_real_customer',
			purchaser_id: user.id,
			quantity: 1,
			status: 'completed',
			phase: 'early_bird',
			amount_minor: 19900,
			currency: 'USD',
			quote_honored: false,
			created_at: new Date(),
			version: 1,
		} as OperatorPassPurchaseRow);
		const plan = await planOperatorPassReset(repository);
		expect(plan).toMatchObject({allowed: false, refused_reason: 'non_preview_purchases'});
		await expect(
			resetOperatorPassPreviewData({
				fulfilment: {repository} as unknown as OperatorPassFulfilmentService,
				cacheService: getCacheService(),
				kvClient: getKVClient(),
			}),
		).rejects.toBeInstanceOf(ConflictError);
		expect(await repository.findPurchase('cs_test_real_customer')).not.toBeNull();
	});
});
