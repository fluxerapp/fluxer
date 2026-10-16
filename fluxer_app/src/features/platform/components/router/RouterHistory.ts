// SPDX-License-Identifier: AGPL-3.0-or-later

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

export function createBrowserHistory(): HistoryAdapter {
	const listeners = new Set<(location: HistoryLocation, action: 'pop') => void>();
	const getLocation = (): HistoryLocation => ({
		url: new URL(window.location.href),
		state: window.history.state ?? null,
	});
	const notify = () => {
		const loc = getLocation();
		for (const l of listeners) l(loc, 'pop');
	};
	const push = (url: URL, state?: unknown) => {
		writeAfterTraversal(() => {
			window.history.pushState(state ?? null, '', url);
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
		const handler = () => listener(getLocation(), 'pop');
		window.addEventListener('popstate', handler);
		return () => {
			listeners.delete(listener);
			window.removeEventListener('popstate', handler);
		};
	};
	const go = (delta: number) => {
		if (delta === 0) {
			return;
		}
		beginTraversal();
		window.history.go(delta);
	};
	const back = () => {
		beginTraversal();
		window.history.back();
	};
	return {
		getLocation,
		push,
		replace,
		listen,
		go,
		back,
		get location() {
			return getLocation().url;
		},
	};
}

export function createMemoryHistory(initialHref = 'http://localhost/'): HistoryAdapter {
	const stack: Array<HistoryLocation> = [{url: new URL(initialHref), state: null}];
	let index = 0;
	const listeners = new Set<(location: HistoryLocation, action: 'pop') => void>();
	const notify = () => {
		for (const l of listeners) l(stack[index], 'pop');
	};
	const getLocation = (): HistoryLocation => stack[index];
	const push = (url: URL, state?: unknown) => {
		index++;
		stack.splice(index, stack.length - index, {url, state: state ?? null});
		notify();
	};
	const replace = (url: URL, state?: unknown) => {
		stack[index] = {url, state: state ?? null};
		notify();
	};
	const listen = (listener: (location: HistoryLocation, action: 'pop') => void) => {
		listeners.add(listener);
		return () => listeners.delete(listener);
	};
	const go = (delta: number) => {
		const newIndex = Math.max(0, Math.min(stack.length - 1, index + delta));
		if (newIndex !== index) {
			index = newIndex;
			notify();
		}
	};
	const back = () => go(-1);
	return {
		getLocation,
		push,
		replace,
		listen,
		go,
		back,
		get location() {
			return getLocation().url;
		},
	};
}
