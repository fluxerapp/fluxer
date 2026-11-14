// SPDX-License-Identifier: AGPL-3.0-or-later

export function yearlySavingsPercent(
	monthlyMinor: number | null | undefined,
	yearlyMinor: number | null | undefined,
): number | null {
	if (monthlyMinor == null || yearlyMinor == null || monthlyMinor <= 0) return null;
	const percent = Math.round((1 - yearlyMinor / (monthlyMinor * 12)) * 100);
	return percent > 0 ? percent : null;
}
