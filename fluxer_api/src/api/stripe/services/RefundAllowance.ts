// SPDX-License-Identifier: AGPL-3.0-or-later

import type {User} from '@app/api/models/User';
import type {GooglePlayRefundTarget} from '@app/api/store_billing/GooglePlayRefundTarget';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import type {
	SelfServeRefundEligibilityResponse,
	SelfServeRefundIneligibilityReason,
	SelfServeRefundProvider,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

export const SELF_SERVE_REFUND_WINDOW_DAYS = 3;
const SELF_SERVE_REFUND_COOLDOWN_DAYS = 30;

export function refundCooldownExpiresAt(user: Pick<User, 'lastRefundAt'>): Date | null {
	if (!user.lastRefundAt) {
		return null;
	}
	const expiresAt = new Date(user.lastRefundAt.getTime() + SELF_SERVE_REFUND_COOLDOWN_DAYS * MS_PER_DAY);
	return expiresAt.getTime() > Date.now() ? expiresAt : null;
}

function refundWindowExpiresAt(paidAt: Date): Date {
	return new Date(paidAt.getTime() + SELF_SERVE_REFUND_WINDOW_DAYS * MS_PER_DAY);
}

function refundIneligibilityReason(
	windowExpiresAt: Date | null,
	cooldownExpiresAt: Date | null | undefined,
): SelfServeRefundIneligibilityReason | null {
	if (windowExpiresAt && windowExpiresAt.getTime() <= Date.now()) {
		return 'outside_refund_window';
	}
	return cooldownExpiresAt ? 'cooldown_active' : null;
}

export interface RefundEligibilityTarget {
	provider: SelfServeRefundProvider;
	invoiceId: string;
	amountPaidCents: number | null;
	currency: string | null;
	paidAt: Date | null;
	subscriptionId: string | null;
}

export function googlePlayRefundEligibilityTarget(target: GooglePlayRefundTarget): RefundEligibilityTarget {
	return {
		provider: 'google_play',
		invoiceId: target.orderId,
		amountPaidCents: null,
		currency: null,
		paidAt: target.paidAt,
		subscriptionId: target.row.kind === 'subscription' ? target.row.id.toString() : null,
	};
}

export function latestRefundTarget<T extends {paidAt: Date | null}>(...targets: Array<T | null>): T | null {
	let latest: T | null = null;
	for (const target of targets) {
		if (target && (!latest || (target.paidAt?.getTime() ?? 0) > (latest.paidAt?.getTime() ?? 0))) {
			latest = target;
		}
	}
	return latest;
}

export function isInsideRefundWindow(paidAt: Date | null, now: number = Date.now()): boolean {
	return paidAt != null && refundWindowExpiresAt(paidAt).getTime() > now;
}

export function combinedRefundTarget<T extends RefundEligibilityTarget>(
	latest: T,
	targets: ReadonlyArray<T>,
	now: number = Date.now(),
): T {
	const inWindow = targets.filter((target) => isInsideRefundWindow(target.paidAt, now));
	if (inWindow.length <= 1) {
		return latest;
	}
	const sameCurrency = inWindow.every((target) => target.currency === latest.currency);
	return {
		...latest,
		amountPaidCents: sameCurrency
			? inWindow.reduce((sum, target) => sum + (target.amountPaidCents ?? 0), 0)
			: latest.amountPaidCents,
		subscriptionId: latest.subscriptionId ?? inWindow.find((target) => target.subscriptionId)?.subscriptionId ?? null,
	};
}

export function buildRefundEligibility({
	reason,
	target,
	cooldownExpiresAt,
}: {
	reason?: SelfServeRefundIneligibilityReason | null;
	target?: RefundEligibilityTarget | null;
	cooldownExpiresAt?: Date | null;
}): SelfServeRefundEligibilityResponse {
	const paidAt = target?.paidAt ?? null;
	const windowExpiresAt = paidAt ? refundWindowExpiresAt(paidAt) : null;
	let resolvedReason = reason;
	if (resolvedReason === undefined) {
		resolvedReason = target ? refundIneligibilityReason(windowExpiresAt, cooldownExpiresAt) : 'no_refundable_purchase';
	}
	return {
		eligible: resolvedReason === null && target != null,
		reason: resolvedReason,
		invoice_id: target?.invoiceId ?? null,
		invoice_amount_paid_cents: target?.amountPaidCents ?? null,
		currency: target?.currency ?? null,
		paid_at: paidAt?.toISOString() ?? null,
		refund_window_expires_at: windowExpiresAt?.toISOString() ?? null,
		cooldown_expires_at: cooldownExpiresAt?.toISOString() ?? null,
		cancels_subscription: target?.subscriptionId != null,
		provider: target?.provider ?? null,
	};
}
