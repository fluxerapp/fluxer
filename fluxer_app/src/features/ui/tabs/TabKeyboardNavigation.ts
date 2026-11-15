// SPDX-License-Identifier: AGPL-3.0-or-later

type TabListOrientation = 'horizontal' | 'vertical' | 'both';
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
): number | null {
	if (itemCount <= 0) {
		return null;
	}
	if (direction === 'first') {
		return 0;
	}
	if (direction === 'last') {
		return itemCount - 1;
	}
	let current: number;
	if (currentIndex >= 0 && currentIndex < itemCount) {
		current = currentIndex;
	} else if (direction === 'next') {
		current = -1;
	} else {
		current = 0;
	}
	const nextIndex = current + (direction === 'next' ? 1 : -1);
	return (nextIndex + itemCount) % itemCount;
}
