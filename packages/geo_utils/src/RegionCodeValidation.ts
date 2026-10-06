// SPDX-License-Identifier: AGPL-3.0-or-later

const ASCII_UPPER_A = 65;
const ASCII_UPPER_Z = 90;
const REGION_CODE_LENGTH = 2;

function isAsciiUpperAlpha2(value: string): boolean {
	return (
		value.length === REGION_CODE_LENGTH &&
		value.charCodeAt(0) >= ASCII_UPPER_A &&
		value.charCodeAt(0) <= ASCII_UPPER_Z &&
		value.charCodeAt(1) >= ASCII_UPPER_A &&
		value.charCodeAt(1) <= ASCII_UPPER_Z
	);
}

export function normalizeRegionCode(regionCode: string): string | undefined {
	const trimmedRegionCode = regionCode.trim();
	if (trimmedRegionCode.length !== REGION_CODE_LENGTH) {
		return undefined;
	}
	const upperRegionCode = trimmedRegionCode.toUpperCase();
	if (!isAsciiUpperAlpha2(upperRegionCode)) {
		return undefined;
	}
	return upperRegionCode;
}
