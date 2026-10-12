// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type Stripe from 'stripe';

const DEFAULT_MESSAGE = 'Mirror upsert failed after Stripe write; reconciler will heal';
export async function mirrorBestEffort(
	write: () => Promise<unknown>,
	context: Record<string, unknown>,
	message = DEFAULT_MESSAGE,
): Promise<void> {
	try {
		await write();
	} catch (mirrorErr) {
		Logger.error({mirrorErr, ...context}, message);
	}
}

export async function mirrorSubscription(
	subscription: Stripe.Subscription,
	knownUserId?: bigint,
	message?: string,
): Promise<void> {
	await mirrorBestEffort(
		() =>
			getBillingRepository().subscriptions.upsertFromStripe(subscription, {
				knownUserId,
				snapshotCapturedAt: new Date(),
			}),
		{subId: subscription.id},
		message,
	);
}
