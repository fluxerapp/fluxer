// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ComputeColumnsOptions {
	targetTileWidth?: number;
	maxLanes?: number;
	minColumns?: number;
}

export function computeMasonryColumns(
	containerWidth: number,
	tileSpacing: number,
	options: ComputeColumnsOptions = {},
): number {
	const targetTileWidth = options.targetTileWidth ?? 200;
	const maxLanes = options.maxLanes ?? 8;
	const minColumns = options.minColumns ?? 1;
	if (containerWidth <= 0) return minColumns;
	const columns = Math.floor((containerWidth + tileSpacing) / (targetTileWidth + tileSpacing));
	return Math.max(minColumns, Math.min(columns, maxLanes));
}
