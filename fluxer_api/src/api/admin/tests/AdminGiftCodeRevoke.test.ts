// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getAdminRepository, getCacheService, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {createGiftCodeRow} from '@app/api/models/GiftCode';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const REASON = 'Codes posted in a public channel';

interface ErrorBody {
	code?: string;
	errors?: Array<{path: string; message: string}>;
}

describe('admin gift code revoke', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.GIFT_CODES_GENERATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function generateCode(): Promise<string> {
		const {codes} = await createBuilder<{codes: Array<string>}>(harness, admin.token)
			.post('/admin/gift-codes')
			.body({count: 1, duration_type: 'months', duration_quantity: 1})
			.execute();
		return codes[0]!.split('/').pop()!;
	}

	function revoke(code: string) {
		return createBuilder<ErrorBody>(harness, admin.token)
			.delete(`/admin/gift-codes/${code}`)
			.header('X-Audit-Log-Reason', REASON)
			.executeRaw();
	}

	async function redeem(account: TestAccount, code: string) {
		return createBuilder<ErrorBody>(harness, account.token).post(`/gifts/${code}/redeem`).executeRaw();
	}

	it('revokes an unredeemed code so it can no longer be redeemed, and records it', async () => {
		const code = await generateCode();
		expect((await revoke(code)).response.status).toBe(204);
		const account = await createTestAccount(harness);
		const attempt = await redeem(account, code);
		expect(attempt.response.status).toBe(404);
		expect(attempt.json.code).toBe(APIErrorCodes.UNKNOWN_GIFT_CODE);
		expect((await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)))).isPremium()).toBe(false);
		expect((await getUserRepository().findGiftCode(code))?.revokedAt).not.toBeNull();
		const entries = (await getAdminRepository().listAllAuditLogsPaginated(100)).filter(
			(entry) => entry.action === 'revoke_gift_code',
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]!.auditLogReason).toBe(REASON);
		expect(Object.fromEntries(entries[0]!.metadata)).toEqual({
			code,
			duration_type: 'months',
			duration_quantity: '1',
		});
	});

	it('refuses a redeemed code and points to Remove premium', async () => {
		const code = await generateCode();
		const account = await createTestAccount(harness);
		expect((await redeem(account, code)).response.status).toBe(204);
		const {response, json} = await revoke(code);
		expect(response.status).toBe(400);
		expect(json.errors).toEqual([
			{
				path: 'code',
				message: `This gift code was already redeemed by account ${account.userId}. Use Remove premium on that account instead.`,
			},
		]);
		expect((await getUserRepository().findGiftCode(code))?.revokedAt).toBeNull();
		expect((await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)))).isPremium()).toBe(true);
	});

	it('refuses a code a customer bought', async () => {
		const buyer = await createTestAccount(harness);
		await getUserRepository().createGiftCode(
			createGiftCodeRow({
				code: 'boughtgiftcode0000000000000000aa',
				durationType: 'months',
				durationQuantity: 1,
				createdAt: new Date(),
				createdByUserId: createUserID(BigInt(buyer.userId)),
				paymentIntentId: 'pi_bought_gift',
			}),
		);
		const {response, json} = await revoke('boughtgiftcode0000000000000000aa');
		expect(response.status).toBe(400);
		expect(json.errors?.[0]?.message).toBe(
			'This gift code was bought by a customer. Refund the purchase instead, which revokes the code.',
		);
	});

	it('returns 404 for an unknown or already revoked code', async () => {
		const code = await generateCode();
		expect((await revoke(code)).response.status).toBe(204);
		expect((await revoke(code)).response.status).toBe(404);
		expect((await revoke('nosuchgiftcode')).response.status).toBe(404);
	});

	it('waits for a redemption that is in progress', async () => {
		const code = await generateCode();
		const token = await getCacheService().acquireLock(`gift_redeem_lock:${code}`, 60);
		try {
			const {response, json} = await revoke(code);
			expect(response.status).toBe(400);
			expect(json.code).toBe(APIErrorCodes.STRIPE_GIFT_REDEMPTION_IN_PROGRESS);
		} finally {
			await getCacheService().releaseLock(`gift_redeem_lock:${code}`, token!);
		}
		expect((await getUserRepository().findGiftCode(code))?.revokedAt).toBeNull();
	});

	it('requires the gift code permission', async () => {
		const code = await generateCode();
		const viewer = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE]);
		await createBuilder(harness, viewer.token).delete(`/admin/gift-codes/${code}`).expect(403).execute();
	});
});
