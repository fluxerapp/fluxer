// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {
	OperatorPassPurchaseRow,
	OperatorPassRow,
	OperatorPassUnitRow,
} from '@app/api/database/types/OperatorPassTypes';
import {
	endOperatorPassesForDeletion,
	OperatorPassHoldingService,
} from '@app/api/operator_pass/OperatorPassHoldingService';
import {describe, expect, test, vi} from 'vitest';

const buyer = createUserID(100n);
const recipient = createUserID(200n);
const now = new Date('2026-11-05T12:00:00Z');
const pass = {user_id: buyer, pass_count: 1, paused_at: null} as OperatorPassRow;
const purchase = {
	checkout_session_id: 'purchase',
	purchaser_id: buyer,
	payment_intent_id: 'payment',
} as OperatorPassPurchaseRow;
const transferred: OperatorPassUnitRow = {
	pass_id: 'purchase:1',
	purchase_id: 'purchase',
	buyer_id: buyer,
	holder_id: recipient,
	source: 'transfer',
	status: 'held',
	transferable: false,
	delivered_at: now,
	first_delivered_at: now,
	paused_at: null,
	ended_at: null,
	end_reason: null,
	withdrawal_country: null,
	withdrawal_days: null,
	version: 1,
};

function fixture(unit: OperatorPassUnitRow = transferred) {
	const repository = {
		findUnit: vi.fn().mockResolvedValue(unit),
		listUnitsByPurchase: vi.fn().mockResolvedValue([unit]),
		listUnitsByHolder: vi.fn().mockResolvedValue([unit]),
		listOffersByUser: vi.fn().mockResolvedValue([]),
		findOperatorPass: vi.fn().mockResolvedValue(pass),
		updateUnit: vi.fn().mockResolvedValue({...unit, status: 'revoked'}),
		appendUnitEvent: vi.fn(),
		removeUnitFromHolder: vi.fn(),
	};
	const wallService = {recordHidden: vi.fn(), recordShownAgain: vi.fn(), takeDown: vi.fn()};
	const service = new OperatorPassHoldingService({
		repository,
		wallService,
		userRepository: {findUnique: vi.fn().mockResolvedValue(null)},
	} as unknown as ConstructorParameters<typeof OperatorPassHoldingService>[0]);
	const sync = vi.spyOn(service, 'syncHolding').mockResolvedValue(pass);
	return {service, sync, repository, wallService};
}

describe('Operator Pass purchase-credit updates', () => {
	test('pausing a transferred purchase refreshes the current holder and original buyer', async () => {
		const {service, sync} = fixture();
		expect(await service.setPurchasePaused(purchase, true)).toEqual([recipient, buyer]);
		expect(sync.mock.calls.map(([id]) => id)).toEqual([recipient, buyer]);
	});

	test('resuming a transferred purchase restores buyer credit too', async () => {
		const {service, sync} = fixture({...transferred, paused_at: now});
		await service.setPurchasePaused(purchase, false);
		expect(sync.mock.calls.map(([id]) => id)).toEqual([recipient, buyer]);
	});

	test('a partial refund refreshes the buyer and only affected holders', async () => {
		const {service, sync, repository} = fixture();
		await service.endPurchaseUnits(purchase, 'refund', 'revoked', 1, new Set([transferred.pass_id]));
		expect(repository.updateUnit).toHaveBeenCalledTimes(1);
		expect(sync.mock.calls.map(([id]) => id)).toEqual([recipient, buyer]);
	});

	test('withdrawal of a transferred unit refreshes its buyer', async () => {
		const {service, sync} = fixture();
		await service.withdrawUnit(transferred, purchase);
		expect(sync.mock.calls.map(([id]) => id)).toEqual([recipient, buyer]);
	});

	test('recipient deletion returns the original buyer for public-count refresh', async () => {
		const {repository} = fixture();
		const buyers = await endOperatorPassesForDeletion(
			repository as unknown as Parameters<typeof endOperatorPassesForDeletion>[0],
			recipient,
		);
		expect(buyers).toEqual([buyer]);
		expect(repository.removeUnitFromHolder).toHaveBeenCalledWith(transferred.pass_id, recipient);
	});

	test('an ordinary purchase refreshes the buyer only once', async () => {
		const {service, sync} = fixture({...transferred, holder_id: buyer, source: 'purchase'});
		await service.setPurchasePaused(purchase, true);
		expect(sync.mock.calls.map(([id]) => id)).toEqual([buyer]);
	});
});
