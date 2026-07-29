// SPDX-License-Identifier: AGPL-3.0-or-later

export interface BrowserNavigationEntry {
	id: string;
	url?: string;
}

export interface BrowserNavigation {
	canGoBack?: boolean;
	canGoForward?: boolean;
	currentEntry?: BrowserNavigationEntry | null;
	back?: () => unknown;
	addEventListener(type: string, listener: (event: Event) => void): void;
	removeEventListener?: (type: string, listener: (event: Event) => void) => void;
}

function isNavigationEntry(value: unknown): value is BrowserNavigationEntry {
	if (typeof value !== 'object' || value === null) return false;
	const entry = value as {id?: unknown; url?: unknown};
	return typeof entry.id === 'string' && (entry.url === undefined || typeof entry.url === 'string');
}

function isBrowserNavigation(value: unknown): value is BrowserNavigation {
	if (typeof value !== 'object' || value === null) return false;
	const navigation = value as {
		addEventListener?: unknown;
		back?: unknown;
		canGoBack?: unknown;
		canGoForward?: unknown;
		currentEntry?: unknown;
		removeEventListener?: unknown;
	};
	if (typeof navigation.addEventListener !== 'function') return false;
	if (navigation.back !== undefined && typeof navigation.back !== 'function') return false;
	if (navigation.canGoBack !== undefined && typeof navigation.canGoBack !== 'boolean') return false;
	if (navigation.canGoForward !== undefined && typeof navigation.canGoForward !== 'boolean') return false;
	if (
		navigation.currentEntry !== undefined &&
		navigation.currentEntry !== null &&
		!isNavigationEntry(navigation.currentEntry)
	) {
		return false;
	}
	return navigation.removeEventListener === undefined || typeof navigation.removeEventListener === 'function';
}

export function getBrowserNavigation(): BrowserNavigation | null {
	if (typeof window === 'undefined') return null;
	const navigation: unknown = Reflect.get(window, 'navigation');
	return isBrowserNavigation(navigation) ? navigation : null;
}
