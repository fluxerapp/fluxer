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

import type {HistoryAdapter, HistoryLocation} from './types';

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
		window.history.pushState(state ?? null, '', url);
		notify();
	};

	const replace = (url: URL, state?: unknown) => {
		window.history.replaceState(state ?? null, '', url);
		notify();
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
		window.history.go(delta);
	};

	const back = () => {
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
