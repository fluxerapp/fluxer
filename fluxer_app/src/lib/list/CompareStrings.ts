// SPDX-License-Identifier: AGPL-3.0-or-later

export function compareStrings(left: string, right: string): number {
	if (left < right) return -1;
	if (left > right) return 1;
	return 0;
}
