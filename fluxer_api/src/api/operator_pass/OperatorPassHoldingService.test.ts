// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {
	OperatorPassOfferRow,
	OperatorPassPurchaseRow,
	OperatorPassRemovalRow,
	OperatorPassUnitRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import {
	endOperatorPassesForDeletion,
	OperatorPassHoldingService,
} from '@app/api/operator_pass/OperatorPassHoldingService';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {expect, test, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;

test.each([
	['loses the bonus for good', {}, {removal_state: 'removed', removed_until: null}],
	['asks to delete their account', {pendingDeletionAt: new Date()}, null],
])(
	'keeps the pass with the sender when the recipient %s while the pass waits to move',
	async (_case, recipientChanges, removal) => {
		const senderId = createUserID(910n);
		const recipientId = createUserID(911n);
		const users = new Map<bigint, Partial<User>>([
			[senderId, {id: senderId, username: 'sender', email: 'sender@example.com', locale: 'en-US', flags: 0n}],
			[recipientId, {id: recipientId, username: 'recipient', flags: 0n, ...recipientChanges}],
		]);
		const purchase = {
			checkout_session_id: 'cs_test_offer_recipient',
			status: 'completed',
			purchased_at: new Date(Date.now() - 10 * DAY_MS),
		} as OperatorPassPurchaseRow;
		const unit = {
			pass_id: `${purchase.checkout_session_id}:0`,
			purchase_id: purchase.checkout_session_id,
			holder_id: senderId,
			status: 'held',
			source: 'purchase',
			withdrawal_days: null,
			first_delivered_at: purchase.purchased_at,
		} as OperatorPassUnitRow;
		const offer = {
			offer_id: 'offer_test_recipient',
			pass_id: unit.pass_id,
			sender_id: senderId,
			recipient_id: recipientId,
			status: 'accepted',
		} as OperatorPassOfferRow;
		const repository = {
			scanOffers: vi.fn(async () => ({rows: [offer], pageState: null})),
			findUnit: vi.fn(async () => unit),
			updateUnit: vi.fn(),
			updateOffer: vi.fn(async (row: OperatorPassOfferRow) => ({...row, status: 'cancelled', ended_at: new Date()})),
			appendUnitEvent: vi.fn(),
			findOperatorPass: vi.fn(async () => null),
			listUnitsByHolder: vi.fn(async () => []),
			listOffersByUser: vi.fn(async () => []),
			findRemoval: vi.fn(async (userId: bigint) =>
				userId === recipientId ? (removal as OperatorPassRemovalRow | null) : null,
			),
		};
		const send = vi.fn(async () => true);
		const holdingService = new OperatorPassHoldingService({
			repository,
			userRepository: {findUnique: vi.fn(async (userId: bigint) => users.get(userId) ?? null)},
			roleService: {},
			gatewayService: {dispatchPresence: vi.fn()},
			emailService: {send},
			wallService: {},
			findPurchase: async () => purchase,
		} as unknown as ConstructorParameters<typeof OperatorPassHoldingService>[0]);

		expect(await holdingService.completeDueOffers()).toBe(0);
		expect(repository.updateUnit).not.toHaveBeenCalled();
		expect(repository.updateOffer).toHaveBeenCalledWith(
			offer,
			expect.objectContaining({status: {kind: 'set', value: 'cancelled'}}),
		);
		expect(send).toHaveBeenCalledWith(
			'operator_pass_offer_ended',
			'sender@example.com',
			{otherName: 'recipient', ended: 'unavailable'},
			'en-US',
		);
	},
);

test('deleting an account clears the wall names copied to its purchases and to offers it received', async () => {
	const userId = createUserID(920n);
	const senderId = createUserID(921n);
	const purchases = new Map<string, OperatorPassPurchaseRow>([
		[
			'cs_test_deleted_buyer',
			{
				checkout_session_id: 'cs_test_deleted_buyer',
				purchaser_id: userId,
				status: 'completed',
				certificate_names: ['Ada Lovelace'],
				wall_name: 'Ada',
			} as OperatorPassPurchaseRow,
		],
	]);
	const offers = new Map<string, OperatorPassOfferRow>(
		(['completed', 'declined', 'pending'] as const).map((status, index) => [
			`offer_test_deleted_${index}`,
			{
				offer_id: `offer_test_deleted_${index}`,
				pass_id: `cs_test_sender:${index}`,
				sender_id: senderId,
				recipient_id: userId,
				status,
				wall_opt_out: false,
				wall_name: 'Ada',
				version: 1,
			} as OperatorPassOfferRow,
		]),
	);
	const cleared = (value: {kind: string; value?: unknown} | undefined, current: unknown) => {
		if (value === undefined) return current;
		return value.kind === 'clear' ? null : value.value;
	};
	const repository = {
		listPurchasesByPurchaser: async () => [...purchases.values()],
		findPurchase: async (id: string) => purchases.get(id) ?? null,
		listCertificatesByPurchase: async () => [],
		clearCertificatePersonalData: async () => {},
		updatePurchase: async (
			current: OperatorPassPurchaseRow,
			patch: Record<string, {kind: string; value?: unknown}>,
		) => {
			const next = {
				...current,
				certificate_names: cleared(patch.certificate_names, current.certificate_names),
				wall_name: cleared(patch.wall_name, current.wall_name),
			} as OperatorPassPurchaseRow;
			purchases.set(current.checkout_session_id, next);
			return next;
		},
		listOffersByUser: async () => [...offers.values()],
		findUnit: async () => null,
		updateOffer: async (current: OperatorPassOfferRow, patch: Record<string, {kind: string; value?: unknown}>) => {
			const stored = offers.get(current.offer_id)!;
			if (stored.version !== current.version) return null;
			const next = {
				...stored,
				status: cleared(patch.status, stored.status),
				wall_name: cleared(patch.wall_name, stored.wall_name),
				version: stored.version + 1,
			} as OperatorPassOfferRow;
			offers.set(current.offer_id, next);
			return next;
		},
		appendUnitEvent: async () => {},
		listUnitsByHolder: async () => [],
		deleteLabsMember: async () => {},
		findVisionaryActivation: async () => null,
	} as unknown as OperatorPassRepository;

	const locks = {acquireLock: async () => 'token', releaseLock: async () => true} as unknown as ICacheService;
	await endOperatorPassesForDeletion(repository, locks, userId);

	for (const purchase of purchases.values()) {
		expect(purchase).toMatchObject({certificate_names: null, wall_name: null});
	}
	for (const offer of offers.values()) {
		expect(offer.wall_name).toBeNull();
	}
	expect(offers.get('offer_test_deleted_2')?.status).toBe('cancelled');
});
