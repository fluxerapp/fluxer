// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	OperatorPassPurchaseBlockedReason,
	OperatorPassPurchaseResponse,
	OperatorPassStateResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

export type BlockingNotice =
	| 'email_unverified'
	| 'email_undeliverable'
	| 'unclaimed'
	| 'purchase_disabled'
	| 'payment_dispute_open'
	| 'removed';

export type CheckoutStage =
	| {kind: 'loading'}
	| {kind: 'failed'}
	| {kind: 'notice'; notice: BlockingNotice}
	| {kind: 'waitlist'}
	| {kind: 'owned'}
	| {kind: 'choose'}
	| {kind: 'details'}
	| {kind: 'agree'}
	| {kind: 'waiting'}
	| {kind: 'done'; purchase: OperatorPassPurchaseResponse}
	| {kind: 'outcome'; result: 'refunded' | 'unavailable'; purchase: OperatorPassPurchaseResponse | null};

export type CheckoutFormStage = 'choose' | 'details' | 'agree';

export type CheckoutEvent =
	| {type: 'loaded'; state: OperatorPassStateResponse}
	| {type: 'load_failed'}
	| {type: 'retry'}
	| {type: 'continue'}
	| {type: 'back'}
	| {type: 'checkout_started'}
	| {type: 'checkout_refused'; stage: CheckoutFormStage | {notice: BlockingNotice}}
	| {type: 'completed'; purchase: OperatorPassPurchaseResponse}
	| {type: 'refunded'; purchase: OperatorPassPurchaseResponse}
	| {type: 'expired'}
	| {type: 'start_over'};

function blockingNotice(reason: OperatorPassPurchaseBlockedReason | null): BlockingNotice | null {
	switch (reason) {
		case 'email_unverified':
		case 'email_undeliverable':
		case 'unclaimed':
		case 'purchase_disabled':
		case 'payment_dispute_open':
		case 'removed':
			return reason;
		default:
			return null;
	}
}

export function stageForState(state: OperatorPassStateResponse): CheckoutStage {
	const reason = state.purchase_blocked_reason ?? null;
	const onSale = state.phase !== 'waitlist' && reason !== 'not_on_sale';
	if (!onSale) return state.holder ? {kind: 'owned'} : {kind: 'waitlist'};
	const notice = blockingNotice(reason);
	return notice ? {kind: 'notice', notice} : {kind: 'choose'};
}

export function stageForCheckoutError(
	code: string | null,
	blockedReason: string | null,
): CheckoutFormStage | {notice: BlockingNotice} | 'reload' | null {
	switch (code) {
		case APIErrorCodes.OPERATOR_PASS_CERTIFICATE_NAME_REJECTED:
			return 'choose';
		case APIErrorCodes.OPERATOR_PASS_WALL_NAME_REQUIRED:
		case APIErrorCodes.OPERATOR_PASS_WALL_NAME_REJECTED:
			return 'details';
		case APIErrorCodes.OPERATOR_PASS_DELIVERY_CONSENT_REQUIRED:
		case APIErrorCodes.OPERATOR_PASS_AGE_CONFIRMATION_REQUIRED:
			return 'agree';
		case APIErrorCodes.OPERATOR_PASS_NOT_ON_SALE:
			return 'reload';
		case APIErrorCodes.OPERATOR_PASS_REMOVED:
			return {notice: 'removed'};
		case APIErrorCodes.EMAIL_VERIFICATION_REQUIRED:
		case APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED:
			return {notice: 'email_unverified'};
		case APIErrorCodes.OPERATOR_PASS_EMAIL_UNDELIVERABLE:
			return {notice: 'email_undeliverable'};
		case APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES:
			return {notice: 'unclaimed'};
		case APIErrorCodes.PREMIUM_PURCHASE_BLOCKED:
			return {notice: blockedReason === 'payment_dispute_open' ? 'payment_dispute_open' : 'purchase_disabled'};
		default:
			return null;
	}
}

export function nextCheckoutStage(stage: CheckoutStage, event: CheckoutEvent): CheckoutStage {
	switch (event.type) {
		case 'loaded':
			return stage.kind === 'loading' || stage.kind === 'failed' || stage.kind === 'notice'
				? stageForState(event.state)
				: stage;
		case 'load_failed':
			return stage.kind === 'loading' ? {kind: 'failed'} : stage;
		case 'retry':
			return stage.kind === 'failed' || stage.kind === 'agree' ? {kind: 'loading'} : stage;
		case 'continue':
			if (stage.kind === 'choose') return {kind: 'details'};
			if (stage.kind === 'details') return {kind: 'agree'};
			return stage;
		case 'back':
			if (stage.kind === 'details') return {kind: 'choose'};
			if (stage.kind === 'agree') return {kind: 'details'};
			return stage;
		case 'checkout_started':
			return stage.kind === 'agree' ? {kind: 'waiting'} : stage;
		case 'checkout_refused':
			if (stage.kind !== 'agree') return stage;
			return typeof event.stage === 'string' ? {kind: event.stage} : {kind: 'notice', notice: event.stage.notice};
		case 'completed':
			return stage.kind === 'waiting' || stage.kind === 'done' ? {kind: 'done', purchase: event.purchase} : stage;
		case 'refunded':
			return stage.kind === 'waiting' ? {kind: 'outcome', result: 'refunded', purchase: event.purchase} : stage;
		case 'expired':
			return stage.kind === 'waiting' ? {kind: 'outcome', result: 'unavailable', purchase: null} : stage;
		case 'start_over':
			return stage.kind === 'waiting' || (stage.kind === 'outcome' && stage.result === 'unavailable')
				? {kind: 'agree'}
				: stage;
	}
}

const REFUNDED_STATUSES: ReadonlySet<OperatorPassPurchaseResponse['status']> = new Set([
	'refunded',
	'duplicate_refunded',
	'withdrawn',
]);

export const CHECKOUT_EXPIRY_GRACE_MS = 60_000;

export function checkoutEventForPurchases(
	purchases: ReadonlyArray<OperatorPassPurchaseResponse>,
	sessionIds: ReadonlyArray<string>,
	expiresAt: number,
	now: number,
): CheckoutEvent | null {
	const ours = purchases.filter((purchase) => sessionIds.includes(purchase.id));
	const completed = ours.find((purchase) => purchase.status === 'completed');
	if (completed) return {type: 'completed', purchase: completed};
	const refunded = ours.find((purchase) => REFUNDED_STATUSES.has(purchase.status));
	if (refunded) return {type: 'refunded', purchase: refunded};
	if (ours.length === 0 && expiresAt > 0 && now > expiresAt + CHECKOUT_EXPIRY_GRACE_MS) return {type: 'expired'};
	return null;
}
