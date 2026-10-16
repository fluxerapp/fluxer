// SPDX-License-Identifier: AGPL-3.0-or-later

import 'fake-indexeddb/auto';

interface TestLockManager {
	request<Result>(name: string, operation: () => Promise<Result>): Promise<Result>;
}

function createTestLockManager(): TestLockManager {
	const tails = new Map<string, Promise<void>>();
	return {
		request<Result>(name: string, operation: () => Promise<Result>): Promise<Result> {
			const result = (tails.get(name) ?? Promise.resolve()).then(operation);
			const tail = result.then(
				() => undefined,
				() => undefined,
			);
			tails.set(name, tail);
			void tail.then(() => {
				if (tails.get(name) === tail) {
					tails.delete(name);
				}
			});
			return result;
		},
	};
}

if (typeof window !== 'undefined' && window.localStorage === undefined && typeof Storage === 'function') {
	Object.defineProperty(window, 'localStorage', {value: new Storage(), configurable: true, writable: true});
}

if (typeof navigator !== 'undefined' && navigator.locks == null) {
	Object.defineProperty(navigator, 'locks', {value: createTestLockManager(), configurable: true, writable: true});
}
