/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

export interface ComputeColumnsOptions {
	desiredItemWidth?: number;
	maxColumns?: number;
	minColumns?: number;
}

export function computeMasonryColumns(
	containerWidth: number,
	itemGutter: number,
	options: ComputeColumnsOptions = {},
): number {
	const desiredItemWidth = options.desiredItemWidth ?? 200;
	const maxColumns = options.maxColumns ?? 8;
	const minColumns = options.minColumns ?? 1;

	if (containerWidth <= 0) return minColumns;

	const columns = Math.floor((containerWidth + itemGutter) / (desiredItemWidth + itemGutter));
	return Math.max(minColumns, Math.min(columns, maxColumns));
}
