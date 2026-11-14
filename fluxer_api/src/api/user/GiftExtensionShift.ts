// SPDX-License-Identifier: AGPL-3.0-or-later

export function shiftGiftExtensionEnd(
	giftEnd: Date | null | undefined,
	anchorMs: number,
	nextPremiumUntil: Date,
): Date | null {
	if (!giftEnd) {
		return null;
	}
	const shiftMs = nextPremiumUntil.getTime() - anchorMs;
	if (shiftMs <= 0 || giftEnd.getTime() <= anchorMs) {
		return giftEnd;
	}
	return new Date(giftEnd.getTime() + shiftMs);
}

export function shiftGiftExtensionPastPremiumUntil(
	current: {premiumUntil: Date | null | undefined; giftEnd: Date | null | undefined},
	nextPremiumUntil: Date,
	now: Date,
	periodStart: Date | null,
): Date | null {
	const paidFromMs = Math.min(now.getTime(), periodStart?.getTime() ?? now.getTime());
	const anchorMs = Math.max(paidFromMs, current.premiumUntil?.getTime() ?? 0);
	return shiftGiftExtensionEnd(current.giftEnd, anchorMs, nextPremiumUntil);
}
