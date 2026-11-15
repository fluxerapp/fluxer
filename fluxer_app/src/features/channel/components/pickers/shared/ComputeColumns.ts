// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ComputeColumnsOptions {
	minColumns?: number;
}

export function computeMasonryColumns(
	containerWidth: number,
	tileSpacing: number,
	options: ComputeColumnsOptions = {},
): number {
	const minColumns = options.minColumns ?? 1;
	if (containerWidth <= 0) return minColumns;
	const columns = Math.floor((containerWidth + tileSpacing) / (200 + tileSpacing));
	return Math.max(minColumns, Math.min(columns, 8));
}
