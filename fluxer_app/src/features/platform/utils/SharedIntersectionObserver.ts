// SPDX-License-Identifier: AGPL-3.0-or-later

type Callback = (entry: IntersectionObserverEntry) => void;

interface SharedObserverState {
	observer: IntersectionObserver;
	callbacks: WeakMap<Element, Set<Callback>>;
	refCount: number;
}

interface ObserverOptions {
	root?: Element | null;
	rootMargin?: string;
	threshold?: number | Array<number>;
}

const pool = new Map<string, SharedObserverState>();

function thresholdKey(threshold: number | Array<number> | undefined): string {
	if (threshold === undefined) return '0';
	if (typeof threshold === 'number') return String(threshold);
	return threshold.join(',');
}

const rootIds = new WeakMap<Element, string>();
let rootIdCounter = 0;

function rootKey(root: Element | null | undefined): string {
	if (!root) return 'document';
	let id = rootIds.get(root);
	if (id === undefined) {
		id = `r${++rootIdCounter}`;
		rootIds.set(root, id);
	}
	return `el:${id}`;
}

function buildKey(options: ObserverOptions): string {
	return `${rootKey(options.root ?? null)}|${options.rootMargin ?? '0px'}|${thresholdKey(options.threshold)}`;
}

function acquirePooledObserver(options: ObserverOptions): SharedObserverState {
	const key = buildKey(options);
	let state = pool.get(key);
	if (state) return state;
	const callbacks = new WeakMap<Element, Set<Callback>>();
	const observer = new IntersectionObserver(
		(entries) => {
			for (let i = 0; i < entries.length; i++) {
				const entry = entries[i];
				const handlers = callbacks.get(entry.target);
				if (!handlers) continue;
				for (const cb of handlers) {
					try {
						cb(entry);
					} catch (error) {
						console.error('SharedIntersectionObserver callback threw:', error);
					}
				}
			}
		},
		{
			root: options.root ?? null,
			rootMargin: options.rootMargin,
			threshold: Array.isArray(options.threshold) ? options.threshold.slice() : options.threshold,
		},
	);
	state = {observer, callbacks, refCount: 0};
	pool.set(key, state);
	return state;
}

export function observeIntersection(element: Element, callback: Callback, options: ObserverOptions = {}): () => void {
	const state = acquirePooledObserver(options);
	let handlers = state.callbacks.get(element);
	if (!handlers) {
		handlers = new Set();
		state.callbacks.set(element, handlers);
		state.observer.observe(element);
		state.refCount++;
	}
	handlers.add(callback);
	const key = buildKey(options);
	return () => {
		const current = pool.get(key);
		if (!current) return;
		const set = current.callbacks.get(element);
		if (!set) return;
		set.delete(callback);
		if (set.size === 0) {
			current.callbacks.delete(element);
			current.observer.unobserve(element);
			current.refCount--;
			if (current.refCount === 0) {
				current.observer.disconnect();
				pool.delete(key);
			}
		}
	};
}
