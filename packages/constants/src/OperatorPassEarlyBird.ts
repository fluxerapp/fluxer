// SPDX-License-Identifier: AGPL-3.0-or-later

const WESTERNMOST_UTC_OFFSET_MS = -12 * 60 * 60 * 1000;
const DAY_MS = 24 * 60 * 60 * 1000;

export function getOperatorPassEarlyBirdEndsOn(priceRisesAt: Date | string): string {
	const risesAt = typeof priceRisesAt === 'string' ? Date.parse(priceRisesAt) : priceRisesAt.getTime();
	return new Date(risesAt + WESTERNMOST_UTC_OFFSET_MS - DAY_MS).toISOString().slice(0, 10);
}
