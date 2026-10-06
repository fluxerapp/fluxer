// SPDX-License-Identifier: AGPL-3.0-or-later

type ResizeCallback = (entry: ResizeObserverEntry) => void;

const callbacks = new Map<Element, Map<ResizeCallback, number>>();

let nativeObserver: ResizeObserver | null = null;

function getObserver(): ResizeObserver {
	if (nativeObserver) return nativeObserver;
	const observer = new ResizeObserver((entries) => {
		for (const entry of entries) {
			if (nativeObserver !== observer) return;
			const handlers = callbacks.get(entry.target);
			if (!handlers) continue;
			for (const callback of [...handlers.keys()]) {
				if (callbacks.get(entry.target) !== handlers) break;
				if (!handlers.has(callback)) continue;
				try {
					callback(entry);
				} catch (error) {
					console.error('SharedResizeObserver callback threw:', error);
				}
			}
		}
	});
	nativeObserver = observer;
	return observer;
}

export function observeResize(element: Element, callback: ResizeCallback): () => void {
	const observer = getObserver();
	let handlers = callbacks.get(element);
	if (!handlers) {
		try {
			observer.observe(element);
		} catch (error) {
			if (callbacks.size === 0) {
				nativeObserver = null;
				observer.disconnect();
			}
			throw error;
		}
		handlers = new Map();
		callbacks.set(element, handlers);
	}
	handlers.set(callback, (handlers.get(callback) ?? 0) + 1);
	let released = false;
	return () => {
		if (released) return;
		released = true;
		const count = handlers.get(callback);
		if (!count) throw new Error('Resize subscription has no active callback');
		if (count > 1) {
			handlers.set(callback, count - 1);
			return;
		}
		handlers.delete(callback);
		if (handlers.size > 0) return;
		callbacks.delete(element);
		if (callbacks.size === 0) {
			nativeObserver = null;
			observer.disconnect();
		} else {
			observer.unobserve(element);
		}
	};
}
