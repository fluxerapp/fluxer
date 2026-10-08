// SPDX-License-Identifier: AGPL-3.0-or-later

export type TabListOrientation = 'horizontal' | 'vertical' | 'both';
export type TabNavigationDirection = 'previous' | 'next' | 'first' | 'last';

export function getTabNavigationDirection(key: string, orientation: TabListOrientation): TabNavigationDirection | null {
	if (key === 'Home') return 'first';
	if (key === 'End') return 'last';
	if (orientation === 'horizontal' || orientation === 'both') {
		if (key === 'ArrowLeft') return 'previous';
		if (key === 'ArrowRight') return 'next';
	}
	if (orientation === 'vertical' || orientation === 'both') {
		if (key === 'ArrowUp') return 'previous';
		if (key === 'ArrowDown') return 'next';
	}
	return null;
}

export function getNextTabIndex(
	currentIndex: number,
	itemCount: number,
	direction: TabNavigationDirection,
	loop = true,
): number | null {
	if (itemCount <= 0) return null;
	if (direction === 'first') return 0;
	if (direction === 'last') return itemCount - 1;
	const current = currentIndex >= 0 && currentIndex < itemCount ? currentIndex : direction === 'next' ? -1 : 0;
	let nextIndex = current + (direction === 'next' ? 1 : -1);
	if (loop) {
		nextIndex = (nextIndex + itemCount) % itemCount;
	} else {
		nextIndex = Math.min(Math.max(nextIndex, 0), itemCount - 1);
	}
	return nextIndex;
}
