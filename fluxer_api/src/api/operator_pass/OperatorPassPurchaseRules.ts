// SPDX-License-Identifier: AGPL-3.0-or-later

import {getWithdrawalPeriodEndsAt} from '@app/api/billing/WithdrawalPeriod';
import {Config} from '@app/api/Config';
import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import {normalizeOperatorPassCertificateName} from '@app/api/operator_pass/OperatorPassCertificateNames';
import type {
	OperatorPassCurrency,
	OperatorPassPurchaseResponse,
	OperatorPassPurchaseStatus,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

const WITHDRAWABLE_STATUSES: ReadonlyArray<OperatorPassPurchaseStatus> = ['completed', 'disputed'];
const UNDELIVERED_WITHDRAWABLE_STATUSES: ReadonlyArray<OperatorPassPurchaseStatus> = [
	'pending',
	...WITHDRAWABLE_STATUSES,
];

export const ENDED_PURCHASE_STATUSES: ReadonlyArray<OperatorPassPurchaseStatus> = [
	'refunded',
	'withdrawn',
	'dispute_lost',
	'duplicate_refunded',
];

const COUNTRY_DEADLINE_UTC_OFFSET_MS = -5 * 60 * 60 * 1000;
const COUNTRY_START_UTC_OFFSET_MS = -2 * 60 * 60 * 1000;

export function getOperatorPassWithdrawalEndsOn(endsAt: Date): string {
	return new Date(endsAt.getTime() + COUNTRY_DEADLINE_UTC_OFFSET_MS).toISOString().slice(0, 10);
}

export function getOperatorPassCountryWithdrawalDeadline(endsAt: Date): Date {
	const [year, month, day] = getOperatorPassWithdrawalEndsOn(endsAt).split('-').map(Number) as [number, number, number];
	return new Date(Date.UTC(year, month - 1, day + 1) - COUNTRY_DEADLINE_UTC_OFFSET_MS);
}

export function getOperatorPassWithdrawalDeadline(purchase: OperatorPassPurchaseRow): Date | null {
	if (!purchase.withdrawal_ends_at) return null;
	return purchase.withdrawal_days != null
		? getOperatorPassCountryWithdrawalDeadline(purchase.withdrawal_ends_at)
		: purchase.withdrawal_ends_at;
}

export function isOperatorPassWithinWithdrawalDeadline(purchase: OperatorPassPurchaseRow, receivedAt: Date): boolean {
	const deadline = getOperatorPassWithdrawalDeadline(purchase);
	return deadline != null && receivedAt.getTime() < deadline.getTime();
}

export function getOperatorPassWithdrawalEndsAt(purchasedAt: Date, days: number | null): Date {
	if (days === null) return getWithdrawalPeriodEndsAt(purchasedAt);
	const start = new Date(purchasedAt.getTime() + COUNTRY_START_UTC_OFFSET_MS);
	const endsOn = Date.UTC(start.getUTCFullYear(), start.getUTCMonth(), start.getUTCDate() + days, 12);
	return new Date(endsOn - COUNTRY_DEADLINE_UTC_OFFSET_MS);
}

export function isOperatorPassPurchaseWithdrawable(
	purchase: OperatorPassPurchaseRow,
	units: ReadonlyArray<OperatorPassUnitRow>,
	receivedAt: Date,
): boolean {
	return (
		UNDELIVERED_WITHDRAWABLE_STATUSES.includes(purchase.status) &&
		purchase.purchased_at != null &&
		purchase.payment_intent_id != null &&
		purchase.delivered_at == null &&
		purchase.perks_granted_at == null &&
		units.length === 0 &&
		isOperatorPassWithinWithdrawalDeadline(purchase, receivedAt)
	);
}

export function wasOperatorPassPurchaseUndeliveredAt(purchase: OperatorPassPurchaseRow, receivedAt: Date): boolean {
	return (
		UNDELIVERED_WITHDRAWABLE_STATUSES.includes(purchase.status) &&
		purchase.purchased_at != null &&
		purchase.purchased_at.getTime() <= receivedAt.getTime() &&
		purchase.payment_intent_id != null &&
		[purchase.delivered_at, purchase.perks_granted_at].every(
			(at) => at == null || at.getTime() > receivedAt.getTime(),
		) &&
		isOperatorPassWithinWithdrawalDeadline(purchase, receivedAt)
	);
}

export function listOperatorPassWithdrawableUnits(
	purchase: OperatorPassPurchaseRow,
	units: ReadonlyArray<OperatorPassUnitRow>,
	receivedAt: Date,
): Array<OperatorPassUnitRow> {
	return units.filter((unit) => isOperatorPassUnitWithdrawable(purchase, unit, receivedAt));
}

function isOperatorPassUnitWithdrawable(
	purchase: OperatorPassPurchaseRow,
	unit: OperatorPassUnitRow,
	receivedAt: Date,
): boolean {
	return (
		WITHDRAWABLE_STATUSES.includes(purchase.status) &&
		unit.status === 'held' &&
		unit.source === 'purchase' &&
		unit.holder_id === purchase.purchaser_id &&
		purchase.withdrawal_days != null &&
		isOperatorPassWithinWithdrawalDeadline(purchase, receivedAt)
	);
}

export function isOperatorPassSinglePassWithdrawable(
	purchase: OperatorPassPurchaseRow,
	unit: OperatorPassUnitRow,
	receivedAt: Date,
): boolean {
	return purchase.status !== 'disputed' && isOperatorPassUnitWithdrawable(purchase, unit, receivedAt);
}

export function getOperatorPassWithdrawalRightEndsAt(purchase: OperatorPassPurchaseRow): Date | null {
	if (purchase.withdrawal_days != null) {
		return getOperatorPassWithdrawalDeadline(purchase);
	}
	return purchase.delivered_at ?? purchase.perks_granted_at ?? purchase.withdrawal_ends_at ?? null;
}

const CANCELLING_END_REASONS: ReadonlySet<string> = new Set(['withdrawal', 'refund', 'dispute']);

export function isOperatorPassWelcomeSettled(purchase: OperatorPassPurchaseRow): boolean {
	return purchase.welcome_sent_at != null || purchase.welcome_skipped_at != null;
}

export function isOperatorPassCertificatePageCancelled(unit: OperatorPassUnitRow): boolean {
	return unit.status !== 'held' && unit.end_reason != null && CANCELLING_END_REASONS.has(unit.end_reason);
}

export function getOperatorPassPassId(purchaseId: string, index: number): string {
	return `${purchaseId}:${index}`;
}

export function getOperatorPassCertificateUrl(token: string): string {
	return `${Config.endpoints.marketing}/pass/c/${token}`;
}

export function listOperatorPassPassIds(
	purchase: Pick<OperatorPassPurchaseRow, 'checkout_session_id' | 'quantity' | 'certificate_names'>,
): Array<string> {
	const count = Math.max(1, purchase.quantity ?? 1, purchase.certificate_names?.length ?? 0);
	return Array.from({length: count}, (_, offset) => getOperatorPassPassId(purchase.checkout_session_id, offset + 1));
}

export function getOperatorPassUnitIndex(passId: string): number {
	const index = Number(passId.slice(passId.lastIndexOf(':') + 1));
	return Number.isFinite(index) ? index : 0;
}

export function replaceOperatorPassCertificateName(
	purchase: OperatorPassPurchaseRow,
	passId: string,
	name: string,
): Array<string> {
	const names = purchase.certificate_names ?? [];
	const quantity = Math.max(1, purchase.quantity ?? 1, names.length);
	const index = getOperatorPassUnitIndex(passId);
	return Array.from({length: quantity}, (_, position) =>
		position === index - 1 ? name : (names[position] ?? names[0] ?? ''),
	);
}

function listOperatorPassCertificatePages(
	purchase: OperatorPassPurchaseRow,
	units: ReadonlyArray<OperatorPassUnitRow>,
	certificateUrls: ReadonlyMap<string, string>,
): OperatorPassPurchaseResponse['pages'] {
	if (purchase.delivered_at == null) return [];
	const names = purchase.certificate_names ?? [];
	return units
		.filter((unit) => !isOperatorPassCertificatePageCancelled(unit))
		.map((unit) => ({unit, index: getOperatorPassUnitIndex(unit.pass_id)}))
		.sort((left, right) => left.index - right.index)
		.map(({unit, index}) => ({
			pass_id: unit.pass_id,
			name: normalizeOperatorPassCertificateName(names[index - 1] ?? names[0] ?? ''),
			certificate_url: certificateUrls.get(unit.pass_id) ?? null,
		}));
}

export function mapOperatorPassPurchaseToResponse(
	purchase: OperatorPassPurchaseRow,
	units: ReadonlyArray<OperatorPassUnitRow>,
	now: Date,
	certificateUrls: ReadonlyMap<string, string>,
): OperatorPassPurchaseResponse {
	const withdrawableUnits = listOperatorPassWithdrawableUnits(purchase, units, now);
	const withdrawableBeforeDelivery = isOperatorPassPurchaseWithdrawable(purchase, units, now);
	const withdrawable = withdrawableBeforeDelivery || withdrawableUnits.length > 0;
	const unitPrice = Math.round(purchase.amount_minor / Math.max(1, purchase.quantity ?? 1));
	return {
		id: purchase.checkout_session_id,
		status: purchase.status,
		order_number: purchase.invoice_number ?? null,
		quantity: Math.max(1, purchase.quantity ?? 1),
		amount_minor: purchase.amount_minor,
		currency: purchase.currency as OperatorPassCurrency,
		purchased_at: purchase.purchased_at?.toISOString() ?? null,
		delivered_at: (purchase.delivered_at ?? purchase.perks_granted_at)?.toISOString() ?? null,
		waiver_consent_at: purchase.waiver_consent_at?.toISOString() ?? null,
		withdrawal_country: purchase.withdrawal_country ?? null,
		withdrawal_ends_at:
			withdrawable || purchase.withdrawal_days != null
				? (getOperatorPassWithdrawalDeadline(purchase)?.toISOString() ?? null)
				: null,
		withdrawal_ends_on:
			purchase.withdrawal_days != null && purchase.withdrawal_ends_at
				? getOperatorPassWithdrawalEndsOn(purchase.withdrawal_ends_at)
				: null,
		withdrawn_at: purchase.withdrawn_at?.toISOString() ?? null,
		pages: listOperatorPassCertificatePages(purchase, units, certificateUrls),
		certificate_available:
			purchase.delivered_at != null &&
			(units.length === 0
				? !ENDED_PURCHASE_STATUSES.includes(purchase.status)
				: units.some((unit) => !isOperatorPassCertificatePageCancelled(unit))),
		can_withdraw: withdrawable,
		refundable_minor: withdrawableBeforeDelivery ? purchase.amount_minor : unitPrice * withdrawableUnits.length,
		certificate_emailed: purchase.welcome_sent_at != null,
		refusal_reason: operatorPassRefusalReason(purchase.refusal_reason),
	};
}

const OPERATOR_PASS_REFUSAL_REASONS: Readonly<Record<string, OperatorPassPurchaseResponse['refusal_reason']>> = {
	operator_pass_removed: 'removed',
	operator_pass_names_missing: 'names_missing',
	operator_pass_email_undeliverable: 'email_undeliverable',
};

function operatorPassRefusalReason(stored: string | null | undefined): OperatorPassPurchaseResponse['refusal_reason'] {
	return stored ? (OPERATOR_PASS_REFUSAL_REASONS[stored] ?? null) : null;
}
