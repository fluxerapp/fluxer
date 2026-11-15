// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {getConfig} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getGatewayService, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {
	createUserCacheService,
	getAdminRepository,
	getGuildRepository,
	getPremiumStateReconciliationQueueService,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {createTestStoreEntitlementService} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import processExpiredPremiumSweep from '@app/api/worker/tasks/ProcessExpiredPremiumSweep';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const REASON = 'Gift given by mistake, ticket 8812';

interface ErrorBody {
	code?: string;
	errors?: Array<{path: string; message: string}>;
}

async function loadUser(account: TestAccount) {
	return getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
}

async function patchUser(account: TestAccount, patch: Partial<UserRow>): Promise<void> {
	const user = await loadUser(account);
	await getUserRepository().patchUpsert(user.id, patch, user.toRow());
}

async function premiumFields(account: TestAccount) {
	const user = await loadUser(account);
	return {
		premiumType: user.premiumType ?? null,
		premiumUntil: user.premiumUntil,
		premiumGiftExtensionEndsAt: user.premiumGiftExtensionEndsAt,
		premiumGraceEndsAt: user.premiumGraceEndsAt,
		premiumWillCancel: user.premiumWillCancel,
		premiumBillingCycle: user.premiumBillingCycle,
		premiumFlags: user.premiumFlags,
		isPremium: user.isPremium(),
	};
}

describe('admin premium removal', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const originalSelfHosted = getConfig().instance.selfHosted;
	const originalPremiumMode = getCachedInstancePremiumMode();

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		getConfig().instance.selfHosted = false;
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.GIFT_CODES_GENERATE,
			AdminACLs.USER_UPDATE_FLAGS,
		]);
	});

	afterEach(() => {
		vi.restoreAllMocks();
		clearWorkerDependencies();
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function giftPremiumAccountInCommunity() {
		const {codes} = await createBuilder<{codes: Array<string>}>(harness, admin.token)
			.post('/admin/gift-codes')
			.body({count: 1, duration_type: 'months', duration_quantity: 1})
			.execute();
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(`/gifts/${codes[0]!.split('/').pop()}/redeem`)
			.expect(204)
			.execute();
		const guild = await createGuild(harness, account.token, 'Premium community');
		const guildId = createGuildID(BigInt(guild.id));
		const member = await getGuildRepository().getMember(guildId, createUserID(BigInt(account.userId)));
		await getGuildRepository().upsertMember({...member!.toRow(), avatar_hash: 'a_5f1c', bio: 'Premium bio'});
		return {account, guildId};
	}

	async function memberSanitized(account: TestAccount, guildId: ReturnType<typeof createGuildID>) {
		return (await getGuildRepository().getMember(guildId, createUserID(BigInt(account.userId))))?.isPremiumSanitized;
	}

	function removePremium(account: TestAccount, reason: string | null = REASON) {
		const builder = createBuilder<ErrorBody>(harness, admin.token).delete(`/admin/users/${account.userId}/premium`);
		if (reason) {
			builder.header('X-Audit-Log-Reason', reason);
		}
		return builder.executeRaw();
	}

	async function expectRefused(account: TestAccount, message: string, path = 'user_id'): Promise<void> {
		const {response, json} = await removePremium(account);
		expect(response.status).toBe(400);
		expect(json.errors).toEqual([{path, message}]);
	}

	it('ends gift premium the same way the expired premium sweep does and records the reason', async () => {
		const removed = await giftPremiumAccountInCommunity();
		const expired = await giftPremiumAccountInCommunity();
		await patchUser(expired.account, {
			premium_gift_extension_ends_at: new Date(Date.now() - 60_000),
			premium_grace_ends_at: new Date(Date.now() - 1_000),
		});
		setWorkerDependenciesForTest({
			userRepository: getUserRepository(),
			guildRepository: getGuildRepository(),
			userCacheService: createUserCacheService(),
			gatewayService: getGatewayService(),
			premiumStateReconciliationQueueService: getPremiumStateReconciliationQueueService(),
			storeEntitlementService: createTestStoreEntitlementService(),
			workerService: getWorkerService(),
		});
		await processExpiredPremiumSweep({}, createTestWorkerHelpers());
		const previousGiftEnd = (await loadUser(removed.account)).premiumGiftExtensionEndsAt!;
		const presence = vi.spyOn(getGatewayService(), 'dispatchPresence');

		const {response} = await removePremium(removed.account);

		expect(response.status).toBe(200);
		const afterRemoval = await premiumFields(removed.account);
		expect(afterRemoval).toEqual(await premiumFields(expired.account));
		expect(afterRemoval.isPremium).toBe(false);
		expect(afterRemoval.premiumFlags & PremiumFlags.PERKS_SANITIZED).toBe(PremiumFlags.PERKS_SANITIZED);
		expect(await memberSanitized(removed.account, removed.guildId)).toBe(true);
		expect(await memberSanitized(expired.account, expired.guildId)).toBe(true);
		expect(presence).toHaveBeenCalledWith(
			expect.objectContaining({userId: createUserID(BigInt(removed.account.userId)), event: 'USER_UPDATE'}),
		);
		const entries = (await getAdminRepository().listAllAuditLogsPaginated(100)).filter(
			(entry) => entry.action === 'remove_premium',
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]!.adminUserId.toString()).toBe(admin.userId);
		expect(entries[0]!.targetId.toString()).toBe(removed.account.userId);
		expect(entries[0]!.auditLogReason).toBe(REASON);
		expect(Object.fromEntries(entries[0]!.metadata)).toEqual({
			previous_premium_type: UserPremiumTypes.SUBSCRIPTION.toString(),
			previous_premium_until: previousGiftEnd.toISOString(),
			removed_enabled_override: 'false',
		});
	});

	it('requires a reason', async () => {
		const {account} = await giftPremiumAccountInCommunity();
		const {response, json} = await removePremium(account, null);
		expect(response.status).toBe(400);
		expect(json.errors?.[0]?.path).toBe('X-Audit-Log-Reason');
		expect((await premiumFields(account)).isPremium).toBe(true);
	});

	it('requires the user flags permission', async () => {
		const {account} = await giftPremiumAccountInCommunity();
		const viewer = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE]);
		await createBuilder(harness, viewer.token)
			.delete(`/admin/users/${account.userId}/premium`)
			.header('X-Audit-Log-Reason', REASON)
			.expect(403)
			.execute();
		expect((await premiumFields(account)).isPremium).toBe(true);
	});

	it('refuses an account with a Stripe subscription so billing is not left running', async () => {
		const {account} = await giftPremiumAccountInCommunity();
		await patchUser(account, {stripe_subscription_id: 'sub_admin_removal', stripe_customer_id: 'cus_admin_removal'});
		await expectRefused(
			account,
			'This account has a Stripe subscription. Cancel the subscription in Stripe first, then remove premium.',
		);
		expect((await premiumFields(account)).isPremium).toBe(true);
	});

	it('refuses Visionary lifetime premium on the hosted instance', async () => {
		const account = await createTestAccount(harness);
		await patchUser(account, {premium_type: UserPremiumTypes.LIFETIME, premium_since: new Date()});
		await expectRefused(
			account,
			'This account has Visionary lifetime premium. Visionary is a paid purchase, so Remove premium does not end it.',
		);
		expect((await premiumFields(account)).premiumType).toBe(UserPremiumTypes.LIFETIME);
	});

	it('refuses an account without premium', async () => {
		await expectRefused(await createTestAccount(harness), 'This account has no premium to remove.');
	});

	it('removes premium granted by ENABLED_OVERRIDE', async () => {
		const account = await createTestAccount(harness);
		await patchUser(account, {premium_flags: PremiumFlags.ENABLED_OVERRIDE | PremiumFlags.PERKS_SANITIZED});
		const {response} = await removePremium(account);
		expect(response.status).toBe(200);
		const fields = await premiumFields(account);
		expect(fields.isPremium).toBe(false);
		expect(fields.premiumFlags).toBe(PremiumFlags.PERKS_SANITIZED);
	});

	it('removes lifetime premium on a self-hosted instance', async () => {
		const account = await createTestAccount(harness);
		await patchUser(account, {premium_type: UserPremiumTypes.LIFETIME, premium_since: new Date()});
		getConfig().instance.selfHosted = true;
		setCachedInstancePremiumMode('mirror');
		const {response} = await removePremium(account);
		expect(response.status).toBe(200);
		expect(await premiumFields(account)).toMatchObject({premiumType: null, isPremium: false});
	});

	it('explains that removal does nothing when the premium mode is everyone', async () => {
		const account = await createTestAccount(harness);
		getConfig().instance.selfHosted = true;
		setCachedInstancePremiumMode('everyone');
		await expectRefused(
			account,
			'Every account on this instance has premium because the premium mode is everyone. Switch the premium mode to mirror to remove premium from one account.',
			'premium_mode',
		);
		expect((await premiumFields(account)).isPremium).toBe(true);
	});
});
