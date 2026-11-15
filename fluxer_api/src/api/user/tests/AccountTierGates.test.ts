// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Headers} from '@fluxer/constants/src/Headers';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MOCK_PRICES = {
	monthlyUsd: 'price_tier_monthly_usd',
	yearlyUsd: 'price_tier_yearly_usd',
	monthlyBrl: 'price_tier_monthly_brl',
	yearlyBrl: 'price_tier_yearly_brl',
	gift1MonthUsd: 'price_tier_gift_1_month_usd',
	gift1YearUsd: 'price_tier_gift_1_year_usd',
	gift1MonthBrl: 'price_tier_gift_1_month_brl',
	gift1YearBrl: 'price_tier_gift_1_year_brl',
};

interface Registered {
	token: string;
	user_id: string;
}

describe('Tier gates on purchases and joins', () => {
	let harness: ApiTestHarness;
	let originalPrices: typeof Config.stripe.prices | undefined;
	beforeAll(async () => {
		originalPrices = Config.stripe.prices;
		Config.stripe.prices = MOCK_PRICES;
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		Config.stripe.prices = MOCK_PRICES;
		server.use(
			...createStripeApiHandlers({
				prices: {[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month'}},
				subscriptionsListEmpty: true,
			}).handlers,
		);
		await getInstanceConfigRepository().updateAccountsConfig({interaction_tier: 'enforced'});
	});
	afterAll(async () => {
		await harness?.shutdown();
		Config.stripe.prices = originalPrices;
	});

	async function createUnclaimed(): Promise<Registered> {
		return await createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username: 'shopper', consent: true, date_of_birth: '1995-05-05'})
			.execute();
	}

	async function createInvite(): Promise<string> {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<{system_channel_id: string}>(harness, owner.token)
			.post('/guilds')
			.body({name: 'Gate Guild'})
			.execute();
		const invite = await createBuilder<{code: string}>(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({})
			.execute();
		return invite.code;
	}

	it('asks unclaimed and unconfirmed accounts to finish their account before checkout', async () => {
		const unclaimed = await createUnclaimed();
		await createBuilder(harness, unclaimed.token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyUsd})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES)
			.execute();
		const unverified = await createTestAccount(harness, {skipEmailVerification: true});
		await createBuilder(harness, unverified.token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyUsd})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED)
			.execute();
	});

	it('refuses unclaimed accounts a full join on every client once visits are on', async () => {
		const code = await createInvite();
		const plain = await createUnclaimed();
		await createBuilder(harness, plain.token).post(`/invites/${code}`).body(null).execute();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
		const visitor = await createUnclaimed();
		const refused = await createBuilder<{action: string; tier: string}>(harness, visitor.token)
			.post(`/invites/${code}`)
			.header(Headers.X_FLUXER_CAPABILITIES, 'guild_visits')
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED)
			.execute();
		expect(refused).toMatchObject({action: 'guild_join', tier: 'unclaimed'});
		await createBuilder(harness, visitor.token)
			.post(`/invites/${code}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED)
			.execute();
		const member = await createTestAccount(harness);
		await createBuilder(harness, member.token).post(`/invites/${code}`).body(null).execute();
	});

	it('lets older clients join when the instance limits the gate to visits clients', async () => {
		const code = await createInvite();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
		await getInstanceConfigRepository().updateAccountsConfig({guild_join_gate: 'visits_capable'});
		const visitor = await createUnclaimed();
		await createBuilder(harness, visitor.token)
			.post(`/invites/${code}`)
			.header(Headers.X_FLUXER_CAPABILITIES, 'guild_visits')
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED)
			.execute();
		await createBuilder(harness, visitor.token).post(`/invites/${code}`).body(null).execute();
	});
});
