// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {GuildMemberOperationsService} from '@app/api/guild/services/member/GuildMemberOperationsService';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	createRole,
	getMember,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MaxGuildsError} from '@fluxer/errors/src/domains/guild/MaxGuildsError';
import {afterAll, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

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
	describe('Lifetime + subscription conflicts', () => {
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
		test('redeeming a lifetime gift code grants visionary when the receiver is at the community limit', async () => {
			const gifter = await createTestAccount(harness);
			const receiver = await createTestAccount(harness);
			await createBuilder(harness, gifter.token)
				.post('/test/gifts/LIFETIMECAP1')
				.body({
					duration_type: 'months',
					duration_quantity: 0,
					created_by_user_id: gifter.userId.toString(),
					visionary_sequence_number: 2,
				})
				.execute();
			const limit = vi
				.spyOn(
					GuildMemberOperationsService.prototype as unknown as {enforceGuildLimit: () => Promise<void>},
					'enforceGuildLimit',
				)
				.mockRejectedValue(new MaxGuildsError(1));
			try {
				await createBuilder(harness, receiver.token).post('/gifts/LIFETIMECAP1/redeem').expect(204).execute();
			} finally {
				limit.mockRestore();
			}
			const receiverAfter = await createBuilder<{premium_type: number}>(harness, receiver.token)
				.get('/users/@me')
				.execute();
			expect(receiverAfter.premium_type).toBe(UserPremiumTypes.LIFETIME);
		});
		test('redeeming lifetime gift code gives a Labs member the Labs Visionary role', async () => {
			const originalOperatorPass = {...Config.operatorPass};
			const originalSelfHosted = Config.instance.selfHosted;
			try {
				const labsOwner = await createTestAccount(harness);
				const labs = await createGuild(harness, labsOwner.token, 'Labs Test Guild');
				const operatorRole = await createRole(harness, labsOwner.token, labs.id, {name: 'Operator'});
				const labsVisionaryRole = await createRole(harness, labsOwner.token, labs.id, {name: 'Visionary'});
				Config.instance.selfHosted = false;
				Config.operatorPass.labsGuildId = BigInt(labs.id);
				Config.operatorPass.labsOperatorRoleId = BigInt(operatorRole.id);
				Config.operatorPass.labsVisionaryRoleId = BigInt(labsVisionaryRole.id);
				const receiver = await createTestAccount(harness);
				const invite = await createChannelInvite(harness, labsOwner.token, labs.system_channel_id!);
				await acceptInvite(harness, receiver.token, invite.code);
				const before = await getMember(harness, receiver.token, labs.id, receiver.userId);
				expect(before.roles).not.toContain(labsVisionaryRole.id);
				await createBuilder(harness, labsOwner.token)
					.post('/test/gifts/LIFETIMELABS1')
					.body({
						duration_type: 'months',
						duration_quantity: 0,
						created_by_user_id: labsOwner.userId.toString(),
						visionary_sequence_number: 1,
					})
					.execute();
				await createBuilder(harness, receiver.token).post('/gifts/LIFETIMELABS1/redeem').expect(204).execute();
				const after = await getMember(harness, receiver.token, labs.id, receiver.userId);
				expect(after.roles).toContain(labsVisionaryRole.id);
				expect(after.roles).not.toContain(operatorRole.id);
			} finally {
				Object.assign(Config.operatorPass, originalOperatorPass);
				Config.instance.selfHosted = originalSelfHosted;
			}
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
});
