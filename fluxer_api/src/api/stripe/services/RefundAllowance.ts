// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingRefundRow} from '@app/api/database/types/BillingTypes';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {
	SelfServeRefundEligibilityResponse,
	SelfServeRefundIneligibilityReason,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

const REFUND_ALLOWANCE_BLOCK_THRESHOLD = 2;

function isCountedAgainstAllowance(refund: BillingRefundRow): boolean {
	if (refund.status !== 'succeeded') {
		return false;
	}
	return (refund.metadata?.get('rejection_reason') ?? null) === null;
}

async function listCountedRefundIds(user: User, userRepository: UserRepository): Promise<Array<string>> {
	const payments = await userRepository.findPaymentsByUserId(user.id);
	const paymentIntentIds = [
		...new Set(payments.map((payment) => payment.paymentIntentId).filter((id): id is string => id !== null)),
	];
	const counted = new Set<string>();
	for (const paymentIntentId of paymentIntentIds) {
		for (const refund of await getBillingRepository().refunds.listByPaymentIntent(paymentIntentId)) {
			if (isCountedAgainstAllowance(refund)) {
				counted.add(refund.provider_id);
			}
		}
	}
	return [...counted].sort();
}

export async function shouldBlockFurtherPurchases(
	user: User,
	userRepository: UserRepository,
): Promise<{blocked: boolean; countedRefundIds: Array<string>}> {
	const countedRefundIds = await listCountedRefundIds(user, userRepository);
	return {blocked: countedRefundIds.length >= REFUND_ALLOWANCE_BLOCK_THRESHOLD, countedRefundIds};
}

const MILLISECONDS_PER_DAY = 24 * 60 * 60 * 1000;
export const SELF_SERVE_REFUND_WINDOW_DAYS = 3;
const SELF_SERVE_REFUND_COOLDOWN_DAYS = 30;

export function refundCooldownExpiresAt(user: Pick<User, 'firstRefundAt'>): Date | null {
	if (!user.firstRefundAt) return null;
	const expiresAt = new Date(user.firstRefundAt.getTime() + SELF_SERVE_REFUND_COOLDOWN_DAYS * MILLISECONDS_PER_DAY);
	return expiresAt.getTime() > Date.now() ? expiresAt : null;
}

export function refundWindowExpiresAt(paidAt: Date): Date {
	return new Date(paidAt.getTime() + SELF_SERVE_REFUND_WINDOW_DAYS * MILLISECONDS_PER_DAY);
}

export function refundIneligibilityReason(
	windowExpiresAt: Date | null,
	cooldownExpiresAt: Date | null | undefined,
): SelfServeRefundIneligibilityReason | null {
	if (windowExpiresAt && windowExpiresAt.getTime() <= Date.now()) return 'outside_refund_window';
	return cooldownExpiresAt ? 'cooldown_active' : null;
}

export function buildRefundEligibility({
	reason,
	target,
	cooldownExpiresAt,
}: {
	reason?: SelfServeRefundIneligibilityReason | null;
	target?: {
		invoiceId: string;
		amountPaidCents: number | null;
		currency: string | null;
		paidAt: Date | null;
		subscriptionId: string | null;
	} | null;
	cooldownExpiresAt?: Date | null;
}): SelfServeRefundEligibilityResponse {
	const paidAt = target?.paidAt ?? null;
	const windowExpiresAt = paidAt ? refundWindowExpiresAt(paidAt) : null;
	const resolvedReason =
		reason === undefined
			? target
				? refundIneligibilityReason(windowExpiresAt, cooldownExpiresAt)
				: 'no_refundable_purchase'
			: reason;
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
	};
}
