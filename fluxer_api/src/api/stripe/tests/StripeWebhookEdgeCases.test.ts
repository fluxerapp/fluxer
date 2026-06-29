// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {createGuild, createRole, getMember} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';

describe('Stripe Webhook Edge Cases', () => {
	let harness: ApiTestHarness;
	let originalVisionariesGuildId: string | undefined;
	let originalVisionariesGuildVisionaryRoleId: string | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		originalVisionariesGuildId = Config.instance.visionariesGuildId ?? undefined;
		originalVisionariesGuildVisionaryRoleId = Config.instance.visionariesGuildVisionaryRoleId ?? undefined;
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.instance.visionariesGuildId = originalVisionariesGuildId;
		Config.instance.visionariesGuildVisionaryRoleId = originalVisionariesGuildVisionaryRoleId;
	});
	beforeEach(async () => {
		await harness.resetData();
		const owner = await createTestAccount(harness);
		const visionariesGuild = await createGuild(harness, owner.token, 'Visionaries Webhook Test Guild');
		const visionaryRole = await createRole(harness, owner.token, visionariesGuild.id, {name: 'Visionary'});
		Config.instance.visionariesGuildId = visionariesGuild.id;
		Config.instance.visionariesGuildVisionaryRoleId = visionaryRole.id;
	});
	describe('Premium stacking - consecutive grants extending duration', () => {
		test('stacks multiple monthly subscriptions end-to-end', async () => {
			const account = await createTestAccount(harness);
			const now = new Date();
			const oneMonthLater = new Date(now.getTime() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: oneMonthLater.toISOString(),
				})
				.execute();
			const me1 = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const firstExpiry = new Date(me1.premium_until!);
			const twoMonthsLater = new Date(oneMonthLater.getTime() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: twoMonthsLater.toISOString(),
				})
				.execute();
			const me2 = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const secondExpiry = new Date(me2.premium_until!);
			const daysDifference = (secondExpiry.getTime() - firstExpiry.getTime()) / (24 * 60 * 60 * 1000);
			expect(daysDifference).toBeGreaterThanOrEqual(28);
			expect(daysDifference).toBeLessThanOrEqual(31);
		});
		test('stacks yearly subscription on top of monthly', async () => {
			const account = await createTestAccount(harness);
			const now = new Date();
			const oneMonthLater = new Date(now.getTime() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: oneMonthLater.toISOString(),
				})
				.execute();
			const me1 = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const firstExpiry = new Date(me1.premium_until!);
			const oneYearAfterFirst = new Date(firstExpiry.getTime() + 365 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: oneYearAfterFirst.toISOString(),
				})
				.execute();
			const me2 = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const secondExpiry = new Date(me2.premium_until!);
			const daysDifference = (secondExpiry.getTime() - firstExpiry.getTime()) / (24 * 60 * 60 * 1000);
			expect(daysDifference).toBeGreaterThanOrEqual(360);
			expect(daysDifference).toBeLessThanOrEqual(370);
		});
		test('stacks premium when current premium has already expired', async () => {
			const account = await createTestAccount(harness);
			const past = new Date(Date.now() - 10 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: past.toISOString(),
				})
				.execute();
			const future = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: future.toISOString(),
				})
				.execute();
			const me = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const expiry = new Date(me.premium_until!);
			const now = new Date();
			const daysDifference = (expiry.getTime() - now.getTime()) / (24 * 60 * 60 * 1000);
			expect(daysDifference).toBeGreaterThanOrEqual(28);
			expect(daysDifference).toBeLessThanOrEqual(31);
		});
		test('preserves premium_since on stacking', async () => {
			const account = await createTestAccount(harness);
			const originalSince = new Date('2024-01-01T00:00:00Z');
			const oneMonthLater = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_since: originalSince.toISOString(),
					premium_until: oneMonthLater.toISOString(),
				})
				.execute();
			const me1 = await createBuilder<{
				premium_since: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me1.premium_since).toBe(originalSince.toISOString());
			const twoMonthsLater = new Date(oneMonthLater.getTime() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: twoMonthsLater.toISOString(),
				})
				.execute();
			const me2 = await createBuilder<{
				premium_since: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me2.premium_since).toBe(originalSince.toISOString());
		});
	});
	describe('Visionary slot management', () => {
		test('multiple users can get consecutive visionary slots', async () => {
			const account1 = await createTestAccount(harness);
			const account2 = await createTestAccount(harness);
			const account3 = await createTestAccount(harness);
			await createBuilder(harness, account1.token)
				.post(`/test/users/${account1.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 0,
				})
				.execute();
			await createBuilder(harness, account2.token)
				.post(`/test/users/${account2.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 1,
				})
				.execute();
			await createBuilder(harness, account3.token)
				.post(`/test/users/${account3.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 2,
				})
				.execute();
			const me1 = await createBuilder<{
				premium_lifetime_sequence: number | null;
			}>(harness, account1.token)
				.get('/users/@me')
				.execute();
			const me2 = await createBuilder<{
				premium_lifetime_sequence: number | null;
			}>(harness, account2.token)
				.get('/users/@me')
				.execute();
			const me3 = await createBuilder<{
				premium_lifetime_sequence: number | null;
			}>(harness, account3.token)
				.get('/users/@me')
				.execute();
			expect(me1.premium_lifetime_sequence).toBe(0);
			expect(me2.premium_lifetime_sequence).toBe(1);
			expect(me3.premium_lifetime_sequence).toBe(2);
		});
	});
	describe('Lifetime + subscription conflicts', () => {
		test('purchasing lifetime when already have subscription clears subscription', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: 'sub_lifetime_cancel',
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 90 * 24 * 60 * 60 * 1000).toISOString(),
					premium_billing_cycle: 'monthly',
				})
				.execute();
			const meBefore = await createBuilder<{
				premium_type: number;
				premium_billing_cycle: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(meBefore.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(meBefore.premium_billing_cycle).toBe('monthly');
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 0,
					stripe_subscription_id: null,
					premium_billing_cycle: null,
				})
				.execute();
			const meAfter = await createBuilder<{
				premium_type: number;
				premium_billing_cycle: string | null;
				premium_lifetime_sequence: number | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(meAfter.premium_type).toBe(UserPremiumTypes.LIFETIME);
			expect(meAfter.premium_billing_cycle).toBeNull();
			expect(meAfter.premium_lifetime_sequence).toBe(0);
		});
		test('cannot redeem plutonium gift when already have visionary', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 5,
				})
				.execute();
			await createBuilder(harness, account.token)
				.post(`/test/gifts/PLUTONIUMGIFT`)
				.body({
					duration_type: 'months',
					duration_quantity: 1,
					created_by_user_id: account.userId.toString(),
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/gifts/PLUTONIUMGIFT/redeem')
				.expect(400, APIErrorCodes.CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY)
				.execute();
		});
		test('visionary users retain lifetime status', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 10,
				})
				.execute();
			const me1 = await createBuilder<{
				premium_type: number;
				premium_lifetime_sequence: number | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me1.premium_type).toBe(UserPremiumTypes.LIFETIME);
			expect(me1.premium_lifetime_sequence).toBe(10);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 10,
				})
				.execute();
			const me2 = await createBuilder<{
				premium_type: number;
				premium_lifetime_sequence: number | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me2.premium_type).toBe(UserPremiumTypes.LIFETIME);
			expect(me2.premium_lifetime_sequence).toBe(10);
		});
		test('upgrading from subscription to lifetime changes premium type', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
				})
				.execute();
			const meBefore = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(meBefore.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(meBefore.premium_until).not.toBeNull();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 15,
					premium_until: null,
				})
				.execute();
			const meAfter = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
				premium_lifetime_sequence: number | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(meAfter.premium_type).toBe(UserPremiumTypes.LIFETIME);
			expect(meAfter.premium_until).toBeNull();
			expect(meAfter.premium_lifetime_sequence).toBe(15);
		});
	});
	describe('Gift code redemption behavior', () => {
		test('redeeming lifetime gift code grants visionary', async () => {
			const gifter = await createTestAccount(harness);
			const receiver = await createTestAccount(harness);
			await createBuilder(harness, gifter.token)
				.post('/test/gifts/LIFETIMEGIFT123')
				.body({
					duration_type: 'months',
					duration_quantity: 0,
					created_by_user_id: gifter.userId.toString(),
					visionary_sequence_number: 0,
				})
				.execute();
			const receiverBefore = await createBuilder<{
				premium_type: number;
			}>(harness, receiver.token)
				.get('/users/@me')
				.execute();
			expect(receiverBefore.premium_type).toBe(UserPremiumTypes.NONE);
			await createBuilder(harness, receiver.token).post('/gifts/LIFETIMEGIFT123/redeem').expect(204).execute();
			const receiverAfter = await createBuilder<{
				premium_type: number;
				premium_lifetime_sequence: number | null;
			}>(harness, receiver.token)
				.get('/users/@me')
				.execute();
			expect(receiverAfter.premium_type).toBe(UserPremiumTypes.LIFETIME);
			expect(receiverAfter.premium_lifetime_sequence).toBe(0);
			const member = await getMember(harness, receiver.token, Config.instance.visionariesGuildId!, receiver.userId);
			expect(member.roles).toContain(Config.instance.visionariesGuildVisionaryRoleId);
		});
		test('redeeming 1-month gift code grants subscription premium', async () => {
			const gifter = await createTestAccount(harness);
			const receiver = await createTestAccount(harness);
			await createBuilder(harness, gifter.token)
				.post('/test/gifts/MONTHGIFT456')
				.body({
					duration_type: 'months',
					duration_quantity: 1,
					created_by_user_id: gifter.userId.toString(),
				})
				.execute();
			await createBuilder(harness, receiver.token).post('/gifts/MONTHGIFT456/redeem').expect(204).execute();
			const receiverAfter = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, receiver.token)
				.get('/users/@me')
				.execute();
			expect(receiverAfter.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(receiverAfter.premium_until).not.toBeNull();
			const premiumUntil = new Date(receiverAfter.premium_until!);
			const now = new Date();
			const daysDiff = (premiumUntil.getTime() - now.getTime()) / (24 * 60 * 60 * 1000);
			expect(daysDiff).toBeGreaterThanOrEqual(27);
			expect(daysDiff).toBeLessThanOrEqual(32);
		});
		test('redeeming 12-month gift code grants full year', async () => {
			const gifter = await createTestAccount(harness);
			const receiver = await createTestAccount(harness);
			await createBuilder(harness, gifter.token)
				.post('/test/gifts/YEARGIFT789')
				.body({
					duration_type: 'years',
					duration_quantity: 1,
					created_by_user_id: gifter.userId.toString(),
				})
				.execute();
			await createBuilder(harness, receiver.token).post('/gifts/YEARGIFT789/redeem').expect(204).execute();
			const receiverAfter = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, receiver.token)
				.get('/users/@me')
				.execute();
			expect(receiverAfter.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			const premiumUntil = new Date(receiverAfter.premium_until!);
			const now = new Date();
			const daysDiff = (premiumUntil.getTime() - now.getTime()) / (24 * 60 * 60 * 1000);
			expect(daysDiff).toBeGreaterThanOrEqual(360);
			expect(daysDiff).toBeLessThanOrEqual(370);
		});
		test('gift codes can only be redeemed once', async () => {
			const gifter = await createTestAccount(harness);
			const receiver1 = await createTestAccount(harness);
			const receiver2 = await createTestAccount(harness);
			await createBuilder(harness, gifter.token)
				.post('/test/gifts/ONCECODE')
				.body({
					duration_type: 'months',
					duration_quantity: 1,
					created_by_user_id: gifter.userId.toString(),
				})
				.execute();
			await createBuilder(harness, receiver1.token).post('/gifts/ONCECODE/redeem').expect(204).execute();
			await createBuilder(harness, receiver2.token)
				.post('/gifts/ONCECODE/redeem')
				.expect(400, APIErrorCodes.GIFT_CODE_ALREADY_REDEEMED)
				.execute();
		});
	});
	describe('Premium duration edge cases', () => {
		test('premium_until clamped to maximum date', async () => {
			const account = await createTestAccount(harness);
			const farFuture = new Date('2099-12-31T23:59:59.999Z');
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: farFuture.toISOString(),
				})
				.execute();
			const me = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const premiumUntil = new Date(me.premium_until!);
			expect(premiumUntil.getTime()).toBeLessThanOrEqual(farFuture.getTime());
		});
		test('stacking near maximum date stays clamped', async () => {
			const account = await createTestAccount(harness);
			const nearMax = new Date('2098-01-01T00:00:00.000Z');
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: nearMax.toISOString(),
				})
				.execute();
			const farFuture = new Date('2099-12-31T23:59:59.999Z');
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: farFuture.toISOString(),
				})
				.execute();
			const me = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const premiumUntil = new Date(me.premium_until!);
			expect(premiumUntil.toISOString()).toBe(farFuture.toISOString());
		});
	});
});
