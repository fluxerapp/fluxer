// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingRefundRow} from '@app/api/database/types/BillingTypes';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';

export const REFUND_ALLOWANCE_BLOCK_THRESHOLD = 2;

function isCountedAgainstAllowance(refund: BillingRefundRow): boolean {
	if (refund.status !== 'succeeded') {
		return false;
	}
	return (refund.metadata?.get('rejection_reason') ?? null) === null;
}

export async function listCountedRefundIds(user: User, userRepository: IUserRepository): Promise<Array<string>> {
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
	userRepository: IUserRepository,
): Promise<{blocked: boolean; countedRefundIds: Array<string>}> {
	const countedRefundIds = await listCountedRefundIds(user, userRepository);
	return {blocked: countedRefundIds.length >= REFUND_ALLOWANCE_BLOCK_THRESHOLD, countedRefundIds};
}
