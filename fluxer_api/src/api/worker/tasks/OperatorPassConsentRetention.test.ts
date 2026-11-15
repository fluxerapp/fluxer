// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {OperatorPassPurchaseRow, OperatorPassRow} from '@app/api/database/types/OperatorPassTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import type {OperatorPassCertificatePageSummary} from '@app/api/operator_pass/OperatorPassRepository';
import {
	isOperatorPassPurchaseDisputed,
	runOperatorPassConsentRetention,
} from '@app/api/worker/tasks/OperatorPassConsentRetention';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {expect, test} from 'vitest';

const NOW = new Date('2029-01-15T00:00:00Z').getTime();

function purchase(id: string, purchasedAt: string, purchaser: string): OperatorPassPurchaseRow {
	return {
		checkout_session_id: id,
		purchaser_id: BigInt(purchaser) as UserID,
		purchased_at: new Date(purchasedAt),
		waiver_consent_at: new Date(purchasedAt),
		waiver_consent_ip: '203.0.113.7',
		waiver_consent_user_agent: 'Mozilla/5.0',
		waiver_consent_text: 'Deliver my certificate now',
	} as OperatorPassPurchaseRow;
}

function openReview(product: string, orderId: string): PaymentDisputeReviewRow {
	return {
		product,
		order_id: orderId,
		payment_intent_id: null,
		status: 'pending',
		outcome: null,
		closed_at: null,
	} as unknown as PaymentDisputeReviewRow;
}

test('checkout IP and browser details are cleared 2 years after payment unless a dispute about that purchase is open', async () => {
	const purchases = new Map([
		['cs_old', purchase('cs_old', '2026-12-01T10:00:00Z', '1')],
		['cs_new', purchase('cs_new', '2027-06-01T10:00:00Z', '1')],
		['cs_disputed', purchase('cs_disputed', '2026-12-01T10:00:00Z', '2')],
		['cs_other_dispute', purchase('cs_other_dispute', '2026-12-01T10:00:00Z', '3')],
	]);
	const reviews = new Map<bigint, Array<PaymentDisputeReviewRow>>([
		[2n, [openReview('operator_pass', 'cs_disputed')]],
		[3n, [openReview('plutonium', 'sub_test_unrelated'), openReview('operator_pass', 'cs_some_other_purchase')]],
	]);
	const disputes = {listReviewsByPurchaser: async (userId: UserID) => reviews.get(userId) ?? []};
	const passes: Array<OperatorPassRow> = [
		{
			user_id: 1n as UserID,
			waiver_consent_at: new Date('2026-12-01T10:00:00Z'),
			waiver_consent_ip: '203.0.113.7',
			waiver_consent_user_agent: 'Mozilla/5.0',
		} as OperatorPassRow,
	];
	const clearedPasses: Array<UserID> = [];
	const kvClient = {
		acquireLock: async () => true,
		extendLock: async () => true,
		releaseLock: async () => true,
	} as unknown as IKVProvider;
	const result = await runOperatorPassConsentRetention({
		kvClient,
		now: () => NOW,
		isOnHold: (target) => isOperatorPassPurchaseDisputed(disputes, target),
		repository: {
			scanPurchases: async () => ({rows: [...purchases.values()], pageState: null}),
			scanOperatorPasses: async () => ({rows: passes, pageState: null}),
			scanCertificatePages: async () => ({rows: [], pageState: null}),
			scanVisionaryActivations: async () => ({rows: [], pageState: null}),
			updateVisionaryActivation: async () => null,
			clearCertificatePersonalData: async () => {},
			findPurchase: async (id) => purchases.get(id) ?? null,
			updatePurchase: async (current, patch) => {
				const next = {
					...current,
					waiver_consent_ip: patch.waiver_consent_ip?.kind === 'clear' ? null : current.waiver_consent_ip,
					waiver_consent_user_agent:
						patch.waiver_consent_user_agent?.kind === 'clear' ? null : current.waiver_consent_user_agent,
				};
				purchases.set(current.checkout_session_id, next);
				return next;
			},
			clearOperatorPassConsentDetails: async (userId) => {
				clearedPasses.push(userId);
			},
		},
	});
	expect(result).toEqual({ran: true, purchases: 2, passes: 1, cancelledPages: 0, visionaryActivations: 0});
	expect(purchases.get('cs_other_dispute')?.waiver_consent_ip).toBeNull();
	expect(purchases.get('cs_old')?.waiver_consent_ip).toBeNull();
	expect(purchases.get('cs_old')?.waiver_consent_user_agent).toBeNull();
	expect(purchases.get('cs_old')?.waiver_consent_text).toBe('Deliver my certificate now');
	expect(purchases.get('cs_new')?.waiver_consent_ip).toBe('203.0.113.7');
	expect(purchases.get('cs_disputed')?.waiver_consent_ip).toBe('203.0.113.7');
	expect(clearedPasses).toEqual([1n]);
});

test('the name and PDF of a cancelled certificate page are erased 2 years after payment unless a dispute is open', async () => {
	const named = (id: string, purchasedAt: string, purchaser: string) => ({
		...purchase(id, purchasedAt, purchaser),
		waiver_consent_ip: null,
		waiver_consent_user_agent: null,
		quantity: 2,
		certificate_names: ['Ada Lovelace', 'Grace Hopper'],
	});
	const purchases = new Map<string, OperatorPassPurchaseRow>([
		['cs_old', named('cs_old', '2026-12-01T10:00:00Z', '1')],
		['cs_new', named('cs_new', '2027-06-01T10:00:00Z', '1')],
		['cs_disputed', named('cs_disputed', '2026-12-01T10:00:00Z', '2')],
	]);
	const page = (
		passId: string,
		version: number,
		paidAt: string,
		cancelled: boolean,
	): OperatorPassCertificatePageSummary => ({
		pass_id: passId,
		version,
		purchase_id: passId.slice(0, passId.indexOf(':')),
		holder_name: passId.endsWith(':1') ? 'Ada Lovelace' : 'Grace Hopper',
		paid_at: new Date(paidAt),
		cancelled_at: cancelled ? new Date('2027-01-01T00:00:00Z') : null,
	});
	const pages = [
		page('cs_old:1', 1, '2026-12-01T10:00:00Z', false),
		page('cs_old:2', 1, '2026-12-01T10:00:00Z', true),
		page('cs_old:2', 2, '2026-12-01T10:00:00Z', true),
		page('cs_new:2', 1, '2027-06-01T10:00:00Z', true),
		page('cs_disputed:2', 1, '2026-12-01T10:00:00Z', true),
	];
	const reviews = new Map<bigint, Array<PaymentDisputeReviewRow>>([[2n, [openReview('operator_pass', 'cs_disputed')]]]);
	const disputes = {listReviewsByPurchaser: async (userId: UserID) => reviews.get(userId) ?? []};
	const cleared: Array<string> = [];
	const result = await runOperatorPassConsentRetention({
		kvClient: {
			acquireLock: async () => true,
			extendLock: async () => true,
			releaseLock: async () => true,
		} as unknown as IKVProvider,
		now: () => NOW,
		isOnHold: (target) => isOperatorPassPurchaseDisputed(disputes, target),
		repository: {
			scanPurchases: async () => ({rows: [...purchases.values()], pageState: null}),
			scanOperatorPasses: async () => ({rows: [], pageState: null}),
			scanCertificatePages: async () => ({rows: pages, pageState: null}),
			scanVisionaryActivations: async () => ({rows: [], pageState: null}),
			updateVisionaryActivation: async () => null,
			clearCertificatePersonalData: async (passId) => {
				cleared.push(passId);
			},
			findPurchase: async (id) => purchases.get(id) ?? null,
			updatePurchase: async (current, patch) => {
				const names = patch.certificate_names;
				const next = {...current, certificate_names: names?.kind === 'set' ? names.value : current.certificate_names};
				purchases.set(current.checkout_session_id, next);
				return next;
			},
			clearOperatorPassConsentDetails: async () => {},
		},
	});
	expect(result).toEqual({ran: true, purchases: 0, passes: 0, cancelledPages: 1, visionaryActivations: 0});
	expect(cleared).toEqual(['cs_old:2']);
	expect(purchases.get('cs_old')?.certificate_names).toEqual(['Ada Lovelace', '']);
	expect(purchases.get('cs_new')?.certificate_names).toEqual(['Ada Lovelace', 'Grace Hopper']);
	expect(purchases.get('cs_disputed')?.certificate_names).toEqual(['Ada Lovelace', 'Grace Hopper']);
});
