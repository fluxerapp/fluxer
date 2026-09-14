// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface CurrentUserPremiumState {
	premium_type: number | null;
	premium_since: string | null;
	premium_until: string | null;
	premium_will_cancel: boolean;
	premium_billing_cycle: string | null;
	premium_lifetime_sequence: number | null;
	premium_enabled_override: boolean;
}

describe('User premium reset endpoint', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('staff user can reset own premium state', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				set_flags: ['STAFF'],
				set_premium_flags: ['ENABLED_OVERRIDE'],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/premium`)
			.body({
				premium_type: 2,
				premium_since: '2026-01-01T00:00:00.000Z',
				premium_until: '2026-12-31T00:00:00.000Z',
				premium_will_cancel: true,
				premium_billing_cycle: 'yearly',
				premium_lifetime_sequence: 42,
				stripe_subscription_id: 'sub_test_reset',
				stripe_customer_id: 'cus_test_reset',
				first_refund_at: '2026-01-02T00:00:00.000Z',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder<void>(harness, account.token)
			.post('/users/@me/premium/reset')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const me = await createBuilder<CurrentUserPremiumState>(harness, account.token)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(me.premium_type).toBe(0);
		expect(me.premium_since).toBeNull();
		expect(me.premium_until).toBeNull();
		expect(me.premium_will_cancel).toBe(false);
		expect(me.premium_billing_cycle).toBeNull();
		expect(me.premium_lifetime_sequence).toBeNull();
		expect(me.premium_enabled_override).toBe(false);
	});
	test('non-staff user cannot reset premium state', async () => {
		const account = await createTestAccount(harness);
		await createBuilder<void>(harness, account.token)
			.post('/users/@me/premium/reset')
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
	});
});
