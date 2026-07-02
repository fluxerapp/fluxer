// SPDX-License-Identifier: AGPL-3.0-or-later

const MAX_SAFE_INTEGER_BIGINT = BigInt(Number.MAX_SAFE_INTEGER);

export function assertSafeByteSize(value: bigint | number, fieldName = 'byte size'): number {
	if (typeof value === 'bigint') {
		if (value < 0n || value > MAX_SAFE_INTEGER_BIGINT) {
			throw new Error(`${fieldName} must fit a non-negative JavaScript safe integer`);
		}
		return Number(value);
	}
	if (!Number.isSafeInteger(value) || value < 0) {
		throw new Error(`${fieldName} must fit a non-negative JavaScript safe integer`);
	}
	return value;
}
