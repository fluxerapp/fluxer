// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import {createControllerCachePurgeAdapter} from '@app/api/infrastructure/ControllerCachePurgeAdapter';
import {afterEach, beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

const ENDPOINT = 'https://controller.test/v1/orgs/org-a/sites/site-b/purge';

const CONFIG: APICachePurgeConfig = {
	adapter: 'controller',
	http: {endpoint: '', token: '', timeoutMs: 10_000},
	controller: {endpoint: ENDPOINT, token: 'pak_test', timeoutMs: 10_000},
};

function prefixes(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => `media.test/attachments/1/${index}/photo`);
}

function sentPatterns(spy: MockInstance<typeof fetch>, call: number): Array<string> {
	const init = spy.mock.calls[call]![1]!;
	return JSON.parse(String(init.body)).patterns;
}

describe('createControllerCachePurgeAdapter', () => {
	let fetchSpy: MockInstance<typeof fetch>;

	beforeEach(() => {
		fetchSpy = vi.spyOn(globalThis, 'fetch');
	});

	afterEach(() => {
		fetchSpy.mockRestore();
	});

	it('sends each prefix as a wildcard pattern with the bearer token', async () => {
		fetchSpy.mockResolvedValue(new Response(null, {status: 202}));
		const outcome = await createControllerCachePurgeAdapter(CONFIG).purge(['media.test/emojis/1']);
		expect(outcome).toEqual({kind: 'purged'});
		const [url, init] = fetchSpy.mock.calls[0]!;
		expect(url).toBe(ENDPOINT);
		expect(init!.method).toBe('POST');
		expect(new Headers(init!.headers).get('Authorization')).toBe('Bearer pak_test');
		expect(JSON.parse(String(init!.body))).toEqual({patterns: ['media.test/emojis/1*'], soft: false});
	});

	it('splits a batch into requests of at most 30 patterns', async () => {
		fetchSpy.mockImplementation(async () => new Response(null, {status: 202}));
		const outcome = await createControllerCachePurgeAdapter(CONFIG).purge(prefixes(65));
		expect(outcome).toEqual({kind: 'purged'});
		expect(fetchSpy).toHaveBeenCalledTimes(3);
		expect(sentPatterns(fetchSpy, 0)).toHaveLength(30);
		expect(sentPatterns(fetchSpy, 1)).toHaveLength(30);
		expect(sentPatterns(fetchSpy, 2)).toHaveLength(5);
	});

	it('stops at the first chunk that fails so the whole batch is retried', async () => {
		fetchSpy
			.mockResolvedValueOnce(new Response(null, {status: 202}))
			.mockResolvedValueOnce(new Response(null, {status: 503}));
		const outcome = await createControllerCachePurgeAdapter(CONFIG).purge(prefixes(90));
		expect(outcome).toEqual({kind: 'failed', status: 503, error: null});
		expect(fetchSpy).toHaveBeenCalledTimes(2);
	});

	it('reports a 400 as rejected', async () => {
		fetchSpy.mockResolvedValue(new Response(null, {status: 400}));
		expect(await createControllerCachePurgeAdapter(CONFIG).purge(prefixes(1))).toEqual({
			kind: 'rejected',
			status: 400,
		});
	});

	it('reports a network error as failed', async () => {
		const error = new TypeError('fetch failed');
		fetchSpy.mockRejectedValue(error);
		expect(await createControllerCachePurgeAdapter(CONFIG).purge(prefixes(1))).toEqual({
			kind: 'failed',
			status: null,
			error,
		});
	});
});
