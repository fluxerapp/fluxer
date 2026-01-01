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

import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

export type NavigationDirection = 'up' | 'down' | 'home' | 'end';

export interface UseListNavigationOptions {
	itemCount: number;
	initialIndex?: number;
	loop?: boolean;
	onSelect?: (index: number) => void;
}

export interface UseListNavigationReturn {
	selectedIndex: number;
	hoverIndex: number;
	keyboardFocusIndex: number;
	hoverIndexForRender: number;
	handleKeyboardNavigation: (direction: NavigationDirection) => void;
	handleMouseEnter: (index: number) => void;
	handleMouseLeave: () => void;
	reset: () => void;
	selectCurrent: () => void;
	setSelectedIndex: React.Dispatch<React.SetStateAction<number>>;
}

export const useListNavigation = ({
	itemCount,
	initialIndex = 0,
	loop = true,
	onSelect,
}: UseListNavigationOptions): UseListNavigationReturn => {
	const getDefaultIndex = useCallback(
		(count: number) => {
			if (count <= 0) return -1;
			const clampedInitial = Math.max(0, Math.min(initialIndex, count - 1));
			return clampedInitial;
		},
		[initialIndex],
	);

	const [selectedIndex, setSelectedIndex] = useState(() => getDefaultIndex(itemCount));
	const [hoverIndex, setHoverIndex] = useState(-1);
	const [hasMouseInteracted, setHasMouseInteracted] = useState(false);

	const handleKeyboardNavigation = useCallback(
		(direction: NavigationDirection) => {
			setSelectedIndex((prev) => {
				if (itemCount === 0) return prev;

				switch (direction) {
					case 'down': {
						const next = prev + 1;
						if (next >= itemCount) {
							return loop ? 0 : itemCount - 1;
						}
						return next;
					}
					case 'up': {
						const next = prev - 1;
						if (next < 0) {
							return loop ? itemCount - 1 : 0;
						}
						return next;
					}
					case 'home':
						return 0;
					case 'end':
						return itemCount - 1;
					default:
						return prev;
				}
			});
		},
		[itemCount, loop],
	);

	const handleMouseEnter = useCallback((index: number) => {
		setHoverIndex(index);
		setHasMouseInteracted(true);
	}, []);

	const handleMouseLeave = useCallback(() => {
		setHoverIndex(-1);
	}, []);

	const reset = useCallback(() => {
		setSelectedIndex(getDefaultIndex(itemCount));
		setHoverIndex(-1);
		setHasMouseInteracted(false);
	}, [getDefaultIndex, itemCount]);

	useEffect(() => {
		setSelectedIndex((prev) => {
			if (itemCount <= 0) return -1;
			if (prev < 0) return getDefaultIndex(itemCount);
			return Math.min(prev, itemCount - 1);
		});
	}, [getDefaultIndex, itemCount]);

	const selectCurrent = useCallback(() => {
		if (selectedIndex < 0 || selectedIndex >= itemCount) return;
		onSelect?.(selectedIndex);
	}, [itemCount, onSelect, selectedIndex]);

	const keyboardFocusIndex = selectedIndex;
	const hoverIndexForRender = hasMouseInteracted ? hoverIndex : -1;

	return {
		selectedIndex,
		hoverIndex,
		keyboardFocusIndex,
		hoverIndexForRender,
		handleKeyboardNavigation,
		handleMouseEnter,
		handleMouseLeave,
		reset,
		selectCurrent,
		setSelectedIndex,
	};
};
