// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TileFlowSolver} from '@app/features/channel/components/TileFlowSolver';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {useCallback, useEffect, useRef, useState} from 'react';

interface UseMasonryGridNavigationOptions {
	navGrid: TileFlowSolver['navGrid'] | null;
	itemKeys: ReadonlyArray<string>;
	columns: number;
	onSelect?: (itemKey: string) => void;
	containerRef?: React.RefObject<HTMLElement | null> | React.RefObject<HTMLElement>;
	isEnabled?: boolean;
	checkSuspension?: () => boolean;
}

export const useMasonryGridNavigation = (options: UseMasonryGridNavigationOptions) => {
	const {navGrid, itemKeys, columns, onSelect, containerRef, isEnabled = true, checkSuspension = () => false} = options;
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
			if (!navGrid) return null;
			return navGrid.tileCells[itemKey] || null;
		},
		[navGrid],
	);
	const findItemByCoordinates = useCallback(
		(row: number, column: number): string | null => {
			if (!navGrid) return null;
			for (const itemKey of itemKeys) {
				const coords = navGrid.tileCells[itemKey];
				if (coords && coords.row === row && coords.column === column) {
					return itemKey;
				}
			}
			return null;
		},
		[navGrid, itemKeys],
	);
	const findNearestItemInColumn = useCallback(
		(targetRow: number, targetColumn: number, direction: 1 | -1): string | null => {
			if (!navGrid) return null;
			let bestItem: string | null = null;
			let bestDistance = Number.POSITIVE_INFINITY;
			for (const itemKey of itemKeys) {
				const coords = navGrid.tileCells[itemKey];
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
		[navGrid, itemKeys],
	);
	const handleKeyDown = useCallback(
		(event: KeyboardEvent) => {
			if (!isEnabled || checkSuspension()) return;
			if (!navGrid || itemKeys.length === 0) return;
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
			if (
				!['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'Escape'].includes(event.key) &&
				!isKeyboardActivationKey(event.key)
			) {
				return;
			}
			const currentKey = focusedItemRef.current;
			if (event.key === 'Escape') {
				event.preventDefault();
				event.stopPropagation();
				setFocusedItemKey(null);
				return;
			}
			if (isKeyboardActivationKey(event.key) && currentKey && onSelect) {
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
			navGrid,
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
