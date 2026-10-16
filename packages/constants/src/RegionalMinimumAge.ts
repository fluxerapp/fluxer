// SPDX-License-Identifier: AGPL-3.0-or-later

const DEFAULT_MINIMUM_AGE = 13;

const REGIONAL_MINIMUM_AGE: Readonly<Record<string, number>> = {
	AT: 14,
	AW: 16,
	BG: 14,
	BQ: 16,
	CL: 14,
	CO: 14,
	CW: 16,
	CY: 14,
	CZ: 15,
	DE: 16,
	ES: 14,
	FR: 15,
	GR: 15,
	HR: 16,
	HU: 16,
	IE: 16,
	IT: 14,
	KR: 14,
	LT: 14,
	LU: 16,
	NL: 16,
	PE: 14,
	PL: 16,
	RO: 16,
	RS: 15,
	SI: 16,
	SK: 16,
	SM: 16,
	SX: 16,
	VE: 14,
	VN: 15,
};

export function getRegionalMinimumAge(countryCode: string | null | undefined): number {
	const normalized = countryCode?.trim().toUpperCase();
	return (normalized && REGIONAL_MINIMUM_AGE[normalized]) || DEFAULT_MINIMUM_AGE;
}
