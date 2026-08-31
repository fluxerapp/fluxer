// SPDX-License-Identifier: AGPL-3.0-or-later

import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {describe, expect, it} from 'vitest';

function deferred<T>(): {promise: Promise<T>; resolve: (value: T) => void} {
	let resolve!: (value: T) => void;
	const promise = new Promise<T>((r) => {
		resolve = r;
	});
	return {promise, resolve};
}

describe('cache invalidation during an in-flight produce', () => {
	it('does not resurrect a value deleted while the factory was running', async () => {
		const cache = new InMemoryProvider();
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		await cache.delete('session');
		gate.resolve('revoked-session');
		await expect(pending).resolves.toBe('revoked-session');
		expect(await cache.get('session')).toBeNull();
	});

	it('still stores the value when no invalidation happens', async () => {
		const cache = new InMemoryProvider();
		const gate = deferred<string>();
		const pending = cache.getOrSet('session', async () => await gate.promise, 30);
		gate.resolve('live-session');
		await pending;
		expect(await cache.get('session')).toBe('live-session');
	});

	it('keeps a later produce cacheable after an earlier one was invalidated', async () => {
		const cache = new InMemoryProvider();
		const first = deferred<string>();
		const pending = cache.getOrSet('session', async () => await first.promise, 30);
		await cache.delete('session');
		first.resolve('stale');
		await pending;
		expect(await cache.get('session')).toBeNull();
		await cache.getOrSet('session', async () => 'fresh', 30);
		expect(await cache.get('session')).toBe('fresh');
	});
});
