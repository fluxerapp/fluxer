// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createGuild, createRole} from '@app/api/guild/tests/GuildTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const PARKED = createUserID(-1n);

describe('visionary numbers on gift redemption', () => {
	let harness: ApiTestHarness;
	const original = {
		selfHosted: Config.instance.selfHosted,
		guildId: Config.instance.visionariesGuildId,
		roleId: Config.instance.visionariesGuildVisionaryRoleId,
	};

	beforeAll(async () => {
		harness = await createApiTestHarness();
		Config.instance.selfHosted = false;
	});

	beforeEach(async () => {
		await harness.resetData();
		await getUserRepository().expandVisionarySlots(4);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Visionaries');
		const role = await createRole(harness, owner.token, guild.id, {name: 'Visionary'});
		Config.instance.visionariesGuildId = guild.id;
		Config.instance.visionariesGuildVisionaryRoleId = role.id;
	});

	afterAll(async () => {
		await harness.shutdown();
		Config.instance.selfHosted = original.selfHosted;
		Config.instance.visionariesGuildId = original.guildId;
		Config.instance.visionariesGuildVisionaryRoleId = original.roleId;
	});

	async function createVisionaryGift(creatorId: UserID, sequence: number): Promise<string> {
		const code = `visionarygift${randomUUID().replaceAll('-', '').slice(0, 16)}`;
		await getUserRepository().createGiftCode({
			code,
			duration_months: null,
			duration_type: 'months',
			duration_quantity: 0,
			created_at: new Date(),
			created_by_user_id: creatorId,
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: null,
			visionary_sequence_number: sequence,
			checkout_session_id: null,
			version: 1,
		});
		return code;
	}

	async function slotHolder(index: number): Promise<UserID | null> {
		const slots = await getUserRepository().listVisionarySlots();
		return slots.find((slot) => slot.slotIndex === index)?.userId ?? null;
	}

	it('moves a parked gift number to the redeemer', async () => {
		const creator = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const redeemerId = createUserID(BigInt(redeemer.userId));
		await getUserRepository().reserveVisionarySlot(2, PARKED);
		const code = await createVisionaryGift(createUserID(BigInt(creator.userId)), 2);

		await createBuilder(harness, redeemer.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();

		const user = await getUserRepository().findUniqueAssert(redeemerId);
		expect(user.premiumType).toBe(UserPremiumTypes.LIFETIME);
		expect(user.premiumLifetimeSequence).toBe(2);
		expect(await slotHolder(2)).toBe(redeemerId);
	});

	it('keeps a gift number that another Visionary already holds', async () => {
		const creator = await createTestAccount(harness);
		const holder = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const holderId = createUserID(BigInt(holder.userId));
		const redeemerId = createUserID(BigInt(redeemer.userId));
		const holderUser = await getUserRepository().findUniqueAssert(holderId);
		await getUserRepository().patchUpsert(
			holderId,
			{premium_type: UserPremiumTypes.LIFETIME, premium_since: new Date(), premium_lifetime_sequence: 2},
			holderUser.toRow(),
		);
		await getUserRepository().reserveVisionarySlot(2, holderId);
		const code = await createVisionaryGift(createUserID(BigInt(creator.userId)), 2);

		await createBuilder(harness, redeemer.token).post(`/gifts/${code}/redeem`).expect(HTTP_STATUS.NO_CONTENT).execute();

		expect(await slotHolder(2)).toBe(holderId);
		const user = await getUserRepository().findUniqueAssert(redeemerId);
		expect(user.premiumType).toBe(UserPremiumTypes.LIFETIME);
		expect(user.premiumLifetimeSequence).not.toBeNull();
		expect(user.premiumLifetimeSequence).not.toBe(2);
		expect(await slotHolder(user.premiumLifetimeSequence!)).toBe(redeemerId);
	});

	it('never hands a held number to another account', async () => {
		const first = createUserID(BigInt((await createTestAccount(harness)).userId));
		const second = createUserID(BigInt((await createTestAccount(harness)).userId));
		await getUserRepository().reserveVisionarySlot(3, first);
		await getUserRepository().reserveVisionarySlot(3, second);
		expect(await slotHolder(3)).toBe(first);
		await getUserRepository().unreserveVisionarySlot(3, PARKED);
		expect(await slotHolder(3)).toBe(first);
	});
});
