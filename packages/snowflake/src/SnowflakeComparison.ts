// SPDX-License-Identifier: AGPL-3.0-or-later

export function compareSnowflakes(left: bigint, right: bigint): number {
	if (left < right) {
		return -1;
	}
	if (left > right) {
		return 1;
	}
	return 0;
}
