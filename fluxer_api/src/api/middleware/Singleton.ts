// SPDX-License-Identifier: AGPL-3.0-or-later

const singletonClearers: Array<() => void> = [];

export function singleton<T>(factory: () => T, teardown?: (instance: T) => void): () => T {
	let instance: T | undefined;
	singletonClearers.push(() => {
		if (instance !== undefined) {
			teardown?.(instance);
		}
		instance = undefined;
	});
	return () => {
		if (instance === undefined) {
			instance = factory();
		}
		return instance;
	};
}

export function keyedSingleton<K, T>(
	source: () => K,
	factory: (key: K) => T,
	teardown?: (instance: T) => void,
): () => T {
	let instance: T | undefined;
	let instanceKey: K | undefined;
	singletonClearers.push(() => {
		if (instance !== undefined) {
			teardown?.(instance);
		}
		instance = undefined;
		instanceKey = undefined;
	});
	return () => {
		const key = source();
		if (instance === undefined || instanceKey !== key) {
			instance = factory(key);
			instanceKey = key;
		}
		return instance;
	};
}

export function clearSingletonsForTesting(): void {
	for (const clear of singletonClearers) {
		clear();
	}
}
