// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

type NavigationDirection = 'up' | 'down' | 'home' | 'end';

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
