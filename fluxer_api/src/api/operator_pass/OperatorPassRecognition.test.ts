// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import type {
	OperatorPassPurchaseRow,
	OperatorPassRow,
	OperatorPassUnitRow,
	OperatorPassWallEventRow,
} from '@app/api/database/types/OperatorPassTypes';
import {DonationRepository} from '@app/api/donation/DonationRepository';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCheckoutService} from '@app/api/operator_pass/OperatorPassCheckoutService';
import {
	OperatorPassEntitlementService,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	calculateOperatorPassRecognition,
	loadOperatorPassRecognition,
} from '@app/api/operator_pass/OperatorPassRecognition';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {reconstructOperatorPassWallEntry} from '@app/api/operator_pass/OperatorPassWallHistory';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {OperatorPassPassesResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, test, vi} from 'vitest';

const buyer = createUserID(100n);
const recipient = createUserID(200n);
const original = new Date('2026-11-01T12:00:00Z');
const received = new Date('2026-11-05T12:00:00Z');

function unit(index: number, overrides: Partial<OperatorPassUnitRow> = {}): OperatorPassUnitRow {
	return {
		pass_id: `purchase:${index}`,
		purchase_id: 'purchase',
		buyer_id: buyer,
		holder_id: buyer,
		source: 'purchase',
		status: 'held',
		transferable: true,
		delivered_at: original,
		first_delivered_at: original,
		paused_at: null,
		ended_at: null,
		end_reason: null,
		withdrawal_country: null,
		withdrawal_days: null,
		version: 1,
		...overrides,
	};
}

const pass = {
	user_id: buyer,
	granted_at: original,
	source: 'purchase',
	pass_count: 1,
	paused_at: null,
} as OperatorPassRow;

function transferred(index: number): OperatorPassUnitRow {
	return unit(index, {holder_id: recipient, source: 'transfer', transferable: false, delivered_at: received});
}

describe('Operator Pass recognition', () => {
	test('ten purchases, nine passed on: ten for the buyer and nine for their recipient', () => {
		const units = [unit(1), ...Array.from({length: 9}, (_, index) => transferred(index + 2))];
		expect(calculateOperatorPassRecognition(buyer, units)).toEqual({count: 10, since: original});
		expect(calculateOperatorPassRecognition(recipient, units)).toEqual({count: 9, since: received});
	});

	test('deduplicates a held purchase returned by both indexes', () => {
		expect(calculateOperatorPassRecognition(buyer, [unit(1), unit(1)])).toEqual({count: 1, since: original});
	});

	test('uses the newest unit version when an index read overlaps a transfer or reversal', () => {
		expect(calculateOperatorPassRecognition(buyer, [unit(1), transferred(1), unit(2)])?.count).toBe(2);
		expect(calculateOperatorPassRecognition(buyer, [unit(1, {status: 'revoked', version: 2}), unit(1)])).toBeNull();
		expect(calculateOperatorPassRecognition(buyer, [{...transferred(1), version: 2}, unit(1)])).toBeNull();
	});

	test('combines purchases passed on with passes received from others', () => {
		const receivedPass = unit(3, {buyer_id: recipient, source: 'transfer', delivered_at: received});
		expect(calculateOperatorPassRecognition(buyer, [transferred(1), transferred(2), receivedPass])).toEqual({
			count: 3,
			since: original,
		});
	});

	test('requires an active held pass, regardless of purchase credit', () => {
		expect(calculateOperatorPassRecognition(buyer, [transferred(1)])).toBeNull();
		expect(calculateOperatorPassRecognition(buyer, [unit(2, {paused_at: received}), transferred(1)])).toBeNull();
	});

	test('excludes refunded, revoked and paused units for both accounts', () => {
		const units = [unit(1), transferred(2), transferred(3), transferred(4)];
		units[1].status = 'withdrawn';
		units[2].status = 'revoked';
		units[3].paused_at = received;
		expect(calculateOperatorPassRecognition(buyer, units)).toEqual({count: 1, since: original});
		expect(calculateOperatorPassRecognition(recipient, units)).toBeNull();
		units[3].paused_at = null;
		expect(calculateOperatorPassRecognition(buyer, units)?.count).toBe(2);
		expect(calculateOperatorPassRecognition(recipient, units)).toEqual({count: 1, since: received});
	});

	test('moves the date forward when the earliest credited pass ends', () => {
		const later = unit(2, {first_delivered_at: received, delivered_at: received});
		expect(calculateOperatorPassRecognition(buyer, [unit(1, {status: 'revoked'}), later])).toEqual({
			count: 1,
			since: received,
		});
	});

	test('uses the receipt date even when a transferred pass was originally bought earlier', () => {
		const laterPurchase = unit(2, {
			buyer_id: recipient,
			holder_id: recipient,
			first_delivered_at: new Date('2026-11-03T12:00:00Z'),
			delivered_at: new Date('2026-11-03T12:00:00Z'),
		});
		expect(calculateOperatorPassRecognition(recipient, [transferred(1), laterPurchase])?.since).toEqual(
			laterPurchase.delivered_at,
		);
	});

	test('counts an admin or Visionary grant without requiring a purchase', () => {
		expect(
			calculateOperatorPassRecognition(buyer, [unit(1, {source: 'admin', buyer_id: null, purchase_id: null})])?.count,
		).toBe(1);
	});

	test('loads existing purchases without trusting original quantity or duplicating holdings', async () => {
		const repository = {
			listUnitsByHolder: vi.fn().mockResolvedValue([unit(1)]),
			listPurchasesByPurchaser: vi.fn().mockResolvedValue([{checkout_session_id: 'purchase', quantity: 100}]),
			listUnitsByPurchase: vi.fn().mockResolvedValue([unit(1), transferred(2), unit(3, {status: 'withdrawn'})]),
		};
		expect(await loadOperatorPassRecognition(repository, buyer, pass)).toEqual({count: 2, since: original});
	});

	test('preserves pre-ledger grants but never revives ended ledger units', async () => {
		const repository = {
			listUnitsByHolder: vi.fn().mockResolvedValue([]),
			listPurchasesByPurchaser: vi.fn().mockResolvedValue([]),
			listUnitsByPurchase: vi.fn().mockResolvedValue([]),
		};
		expect(await loadOperatorPassRecognition(repository, buyer, pass)).toEqual({count: 1, since: original});
		repository.listUnitsByHolder.mockResolvedValue([unit(1, {status: 'revoked'})]);
		expect(await loadOperatorPassRecognition(repository, buyer, pass)).toBeNull();
	});

	test('uses recognition for display without changing the held aggregate', () => {
		expect(resolveOperatorPassHolder({pass, removal: null, recognition: {count: 10, since: original}})?.count).toBe(10);
		expect(pass.pass_count).toBe(1);
		expect(
			resolveOperatorPassHolder({pass: null, removal: null, recognition: {count: 10, since: original}}),
		).toBeNull();
		expect(resolveOperatorPassHolder({pass, removal: null, recognition: null})).toBeNull();
	});

	test('wall history keeps old events and uses recorded qualifying dates for new events', () => {
		const events = [
			{kind: 'listed', occurred_at: original},
			{kind: 'count_changed', occurred_at: original, pass_count: 1},
			{kind: 'count_changed', occurred_at: received, pass_count: 10, first_pass_at: original},
		] as Array<OperatorPassWallEventRow>;
		const parameters = {
			userId: buyer,
			username: 'Buyer',
			revisions: [],
			decisions: [],
			events,
			tierMinimums: [1, 2, 5, 10],
		};
		expect(reconstructOperatorPassWallEntry({...parameters, at: original})).toMatchObject({
			count: 1,
			tier: 1,
			firstPassAt: original,
		});
		expect(reconstructOperatorPassWallEntry({...parameters, at: received})).toMatchObject({
			count: 10,
			tier: 4,
			firstPassAt: original,
		});
	});
});

test('profile and holdings APIs expose separate recognition and ownership counts', async () => {
	const harness = await createApiTestHarness();
	try {
		const account = await createTestAccount(harness);
		const accountId = createUserID(BigInt(account.userId));
		const repository = new OperatorPassRepository();
		await repository.saveOperatorPass({...pass, user_id: accountId, version: 1});
		await repository.createPurchase({
			checkout_session_id: 'purchase',
			purchaser_id: accountId,
			quantity: 10,
			version: 1,
		} as OperatorPassPurchaseRow);
		for (let index = 1; index <= 10; index++) {
			await repository.createUnit(
				unit(index, {
					buyer_id: accountId,
					holder_id: index === 1 ? accountId : recipient,
					source: index === 1 ? 'purchase' : 'transfer',
				}),
			);
		}
		const profile = await createBuilder<UserProfileFullResponse>(harness, account.token)
			.get(`/users/${account.userId}/profile`)
			.expect(200)
			.execute();
		const badge = profile.profile_badges?.find((entry) => entry.kind === 'operator_pass');
		expect(badge).toMatchObject({count: 10});
		expect(badge?.details?.lines).toContain('Includes valid passes you have and passes you bought and passed on.');
		const holdings = await createBuilder<OperatorPassPassesResponse>(harness, account.token)
			.get('/operator-pass/passes')
			.expect(200)
			.execute();
		expect(holdings).toMatchObject({count: 1, recognition_count: 10});
		expect(holdings.passes).toHaveLength(1);
		const checkout = new OperatorPassCheckoutService(
			null,
			repository,
			new OperatorPassEntitlementService(repository),
			new DonationRepository(),
		);
		const state = await checkout.getState({
			user: await getUserRepository().findUniqueAssert(accountId),
			countryCode: 'SE',
		});
		expect(state.holder).toMatchObject({count: 1, recognition_count: 10});
	} finally {
		await harness.shutdown();
	}
});
