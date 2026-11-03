// SPDX-License-Identifier: AGPL-3.0-or-later

type PremiumPurchaseBlockedReason = 'lifetime' | 'existing_subscription' | 'purchase_disabled' | 'payment_dispute_open';

export function getPremiumPurchaseBlockedReason(body: unknown): PremiumPurchaseBlockedReason | null {
	if (body == null || typeof body !== 'object' || !('reason' in body)) {
		return null;
	}

	const reason = body.reason;
	switch (reason) {
		case 'lifetime':
		case 'existing_subscription':
		case 'purchase_disabled':
		case 'payment_dispute_open':
			return reason;
		default:
			return null;
	}
}
