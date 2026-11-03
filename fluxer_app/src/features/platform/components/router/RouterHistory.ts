// SPDX-License-Identifier: AGPL-3.0-or-later

import {getBrowserNavigation} from '@app/features/platform/components/router/BrowserNavigation';
import type {HistoryAdapter, HistoryLocation} from '@app/features/platform/components/router/RouterTypes';

const TRAVERSAL_SETTLE_TIMEOUT_MS = 500;

let pendingTraversals = 0;
let traversalSettleTimer: ReturnType<typeof setTimeout> | null = null;
let traversalListenerInstalled = false;
const writesAfterTraversal: Array<() => void> = [];

function flushWritesAfterTraversal(): void {
	pendingTraversals = 0;
	if (traversalSettleTimer !== null) {
		clearTimeout(traversalSettleTimer);
		traversalSettleTimer = null;
	}
	for (const write of writesAfterTraversal.splice(0)) {
		write();
	}
}

function settleTraversal(): void {
	if (pendingTraversals === 0) {
		return;
	}
	pendingTraversals -= 1;
	if (pendingTraversals === 0) {
		flushWritesAfterTraversal();
	}
}

function beginTraversal(): void {
	if (!traversalListenerInstalled) {
		traversalListenerInstalled = true;
		window.addEventListener('popstate', settleTraversal);
	}
	pendingTraversals += 1;
	if (traversalSettleTimer !== null) {
		clearTimeout(traversalSettleTimer);
	}
	traversalSettleTimer = setTimeout(flushWritesAfterTraversal, TRAVERSAL_SETTLE_TIMEOUT_MS);
}

function writeAfterTraversal(write: () => void): void {
	if (pendingTraversals > 0) {
		writesAfterTraversal.push(write);
		return;
	}
	write();
}

function currentEntryIndex(): number | null {
	const index = getBrowserNavigation()?.currentEntry?.index;
	return typeof index === 'number' && index >= 0 ? index : null;
}

export function createBrowserHistory(): HistoryAdapter {
	const listeners = new Set<(location: HistoryLocation, action: 'pop') => void>();
	let backDepth = 0;
	let forwardDepth = 0;
	let ownTraversals = 0;
	let returnTraversals = 0;
	let range: {floor: number; ceiling: number} | null = null;
	const moveBy = (delta: number) => {
		backDepth = Math.max(0, backDepth + delta);
		forwardDepth = Math.max(0, forwardDepth - delta);
	};
	const returnInsideBoundary = (): boolean => {
		const index = currentEntryIndex();
		if (range === null || index === null) {
			return false;
		}
		const target = Math.min(Math.max(index, range.floor), range.ceiling);
		if (target === index) {
			return false;
		}
		beginTraversal();
		returnTraversals += 1;
		window.history.go(target - index);
		return true;
	};
	const syncDepth = (): boolean => {
		const index = currentEntryIndex();
		if (range === null || index === null) {
			return false;
		}
		backDepth = index - range.floor;
		forwardDepth = range.ceiling - index;
		return true;
	};
	const getLocation = (): HistoryLocation => ({
		url: new URL(window.location.href),
		state: window.history.state ?? null,
	});
	const notify = () => {
		const loc = getLocation();
		for (const l of listeners) l(loc, 'pop');
	};
	window.addEventListener('popstate', () => {
		if (returnTraversals > 0) {
			returnTraversals -= 1;
			return;
		}
		if (returnInsideBoundary()) {
			return;
		}
		const own = ownTraversals > 0;
		if (own) {
			ownTraversals -= 1;
		}
		if (!syncDepth() && !own) {
			moveBy(-1);
		}
		notify();
	});
	const push = (url: URL, state?: unknown) => {
		writeAfterTraversal(() => {
			window.history.pushState(state ?? null, '', url);
			backDepth += 1;
			forwardDepth = 0;
			const index = currentEntryIndex();
			if (range !== null && index !== null) {
				range.ceiling = index;
			}
			notify();
		});
	};
	const replace = (url: URL, state?: unknown) => {
		writeAfterTraversal(() => {
			window.history.replaceState(state ?? null, '', url);
			notify();
		});
	};
	const listen = (listener: (location: HistoryLocation, action: 'pop') => void) => {
		listeners.add(listener);
		return () => {
			listeners.delete(listener);
		};
	};
	const go = (delta: number) => {
		if (delta === 0) {
			return;
		}
		beginTraversal();
		ownTraversals += 1;
		moveBy(delta);
		window.history.go(delta);
	};
	const back = () => go(-1);
	const markBoundary = () => {
		backDepth = 0;
		forwardDepth = 0;
		const index = currentEntryIndex();
		range = index === null ? null : {floor: index, ceiling: index};
	};
	const canGo = (delta: number) => (delta < 0 ? backDepth >= -delta : forwardDepth >= delta);
	return {
		getLocation,
		push,
		replace,
		listen,
		go,
		back,
		markBoundary,
		canGo,
		get location() {
			return getLocation().url;
		},
	};
}
