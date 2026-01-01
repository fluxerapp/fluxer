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

import {useCallback, useEffect, useRef, useState} from 'react';
import type {MasonryListComputer} from '~/components/channel/MasonryListComputer';

interface UseMasonryGridNavigationOptions {
	gridData: MasonryListComputer['gridData'] | null;
	itemKeys: ReadonlyArray<string>;
	columns: number;
	onSelect?: (itemKey: string) => void;
	containerRef?: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	isEnabled?: boolean;
	checkSuspension?: () => boolean;
}

export const useMasonryGridNavigation = (options: UseMasonryGridNavigationOptions) => {
	const {
		gridData,
		itemKeys,
		columns,
		onSelect,
		containerRef,
		isEnabled = true,
		checkSuspension = () => false,
	} = options;

	const [focusedItemKey, setFocusedItemKey] = useState<string | null>(null);
	const focusedItemRef = useRef<string | null>(null);

	useEffect(() => {
		focusedItemRef.current = focusedItemKey;
	}, [focusedItemKey]);

	const focusItem = useCallback(
		(itemKey: string | null) => {
			if (!itemKey) {
				setFocusedItemKey(null);
				return;
			}

			setFocusedItemKey(itemKey);

			if (containerRef?.current) {
				const element = containerRef.current.querySelector(`[data-grid-item="${itemKey}"]`) as HTMLElement;
				if (element) {
					element.scrollIntoView({block: 'nearest', inline: 'nearest'});
				}
			}
		},
		[containerRef],
	);

	const getItemCoordinates = useCallback(
		(itemKey: string) => {
			if (!gridData) return null;
			return gridData.coordinates[itemKey] || null;
		},
		[gridData],
	);

	const findItemByCoordinates = useCallback(
		(row: number, column: number): string | null => {
			if (!gridData) return null;

			for (const itemKey of itemKeys) {
				const coords = gridData.coordinates[itemKey];
				if (coords && coords.row === row && coords.column === column) {
					return itemKey;
				}
			}
			return null;
		},
		[gridData, itemKeys],
	);

	const findNearestItemInColumn = useCallback(
		(targetRow: number, targetColumn: number, direction: 1 | -1): string | null => {
			if (!gridData) return null;

			let bestItem: string | null = null;
			let bestDistance = Number.POSITIVE_INFINITY;

			for (const itemKey of itemKeys) {
				const coords = gridData.coordinates[itemKey];
				if (!coords || coords.column !== targetColumn) continue;

				const rowDiff = coords.row - targetRow;
				if ((direction > 0 && rowDiff > 0) || (direction < 0 && rowDiff < 0)) {
					const distance = Math.abs(rowDiff);
					if (distance < bestDistance) {
						bestDistance = distance;
						bestItem = itemKey;
					}
				}
			}

			return bestItem;
		},
		[gridData, itemKeys],
	);

	const handleKeyDown = useCallback(
		(event: KeyboardEvent) => {
			if (!isEnabled || checkSuspension()) return;
			if (!gridData || itemKeys.length === 0) return;

			const target = event.target as HTMLElement;
			if (
				target &&
				(target.tagName === 'INPUT' ||
					target.tagName === 'TEXTAREA' ||
					target.tagName === 'SELECT' ||
					target.isContentEditable)
			) {
				return;
			}

			if (!['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'Enter', 'Escape'].includes(event.key)) {
				return;
			}

			const currentKey = focusedItemRef.current;

			if (event.key === 'Escape') {
				event.preventDefault();
				event.stopPropagation();
				setFocusedItemKey(null);
				return;
			}

			if (event.key === 'Enter' && currentKey && onSelect) {
				event.preventDefault();
				event.stopPropagation();
				onSelect(currentKey);
				return;
			}

			const currentCoords = currentKey ? getItemCoordinates(currentKey) : null;

			if (!currentCoords) {
				focusItem(itemKeys[0]);
				event.preventDefault();
				event.stopPropagation();
				return;
			}

			let nextItemKey: string | null = null;

			switch (event.key) {
				case 'ArrowLeft': {
					event.preventDefault();
					event.stopPropagation();
					const newColumn = currentCoords.column - 1;
					if (newColumn >= 0) {
						nextItemKey = findItemByCoordinates(currentCoords.row, newColumn);
						if (!nextItemKey) {
							nextItemKey = findNearestItemInColumn(currentCoords.row, newColumn, -1);
						}
					}
					break;
				}

				case 'ArrowRight': {
					event.preventDefault();
					event.stopPropagation();
					const newColumn = currentCoords.column + 1;
					if (newColumn < columns) {
						nextItemKey = findItemByCoordinates(currentCoords.row, newColumn);
						if (!nextItemKey) {
							nextItemKey = findNearestItemInColumn(currentCoords.row, newColumn, 1);
						}
					}
					break;
				}

				case 'ArrowUp': {
					event.preventDefault();
					event.stopPropagation();
					nextItemKey = findNearestItemInColumn(currentCoords.row, currentCoords.column, -1);
					break;
				}

				case 'ArrowDown': {
					event.preventDefault();
					event.stopPropagation();
					nextItemKey = findNearestItemInColumn(currentCoords.row, currentCoords.column, 1);
					break;
				}
			}

			if (nextItemKey) {
				focusItem(nextItemKey);
			}
		},
		[
			isEnabled,
			checkSuspension,
			gridData,
			itemKeys,
			columns,
			onSelect,
			getItemCoordinates,
			findItemByCoordinates,
			findNearestItemInColumn,
			focusItem,
		],
	);

	useEffect(() => {
		if (!isEnabled) return;

		window.addEventListener('keydown', handleKeyDown);
		return () => {
			window.removeEventListener('keydown', handleKeyDown);
		};
	}, [handleKeyDown, isEnabled]);

	return {
		focusedItemKey,
		setFocusedItemKey: focusItem,
	};
};
