// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import {createGuild, createRole, getMember} from '@app/api/guild/tests/GuildTestUtils';
import {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('StripePremiumService', () => {
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
		const visionariesGuild = await createGuild(harness, owner.token, 'Visionaries Test Guild');
		const visionaryRole = await createRole(harness, owner.token, visionariesGuild.id, {name: 'Visionary'});
		Config.instance.visionariesGuildId = visionariesGuild.id;
		Config.instance.visionariesGuildVisionaryRoleId = visionaryRole.id;
	});
	describe('POST /premium/visionary/rejoin', () => {
		test('allows visionary users to rejoin guild', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 0,
				})
				.execute();
			await createBuilder(harness, account.token).post('/premium/visionary/rejoin').expect(204).execute();
			const member = await getMember(harness, account.token, Config.instance.visionariesGuildId!, account.userId);
			expect(member.roles).toContain(Config.instance.visionariesGuildVisionaryRoleId);
		});
		test('rejects users without visionary access', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
				})
				.execute();
			await createBuilder(harness, account.token)
				.post('/premium/visionary/rejoin')
				.expect(403, APIErrorCodes.MISSING_ACCESS)
				.execute();
		});
	});
	describe('premium duration and stacking', () => {
		test('keeps Visionary number 0 when a lifetime grant comes from a subscription period', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
					premium_lifetime_sequence: 0,
				})
				.execute();
			const premiumService = new StripePremiumService(
				new UserRepository(),
				new NoopGatewayService(),
				{} as GuildRepository,
				{} as GuildService,
			);
			await premiumService.setPremiumFromSubscriptionPeriod(
				createUserID(BigInt(account.userId)),
				UserPremiumTypes.LIFETIME,
				new Date(Date.now() + 86_400_000),
			);
			const me = await createBuilder<{premium_lifetime_sequence: number | null}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_lifetime_sequence).toBe(0);
		});
	});
});
