// SPDX-License-Identifier: AGPL-3.0-or-later

export function jitteredExponentialDelay(base: number, cap: number, attempt: number, random: () => number): number {
	return Math.round(Math.min(base * 2 ** attempt, cap) * (0.5 + random() * 0.5));
}
