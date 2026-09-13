// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import {CachePurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {server} from '@app/api/test/msw/server';
import processCachePurgeQueue from '@app/api/worker/tasks/ProcessCachePurgeQueue';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {delay, HttpResponse, http} from 'msw';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;
const ENDPOINT = 'https://cache-purge.test/purge';
const MEDIA = 'https://media.test';
const EXACT_KEY = 'cache_purge:exact';
const PREFIX_KEY = 'cache_purge:prefix';
const REJECTED_KEY = 'cache_purge:rejected';

interface PurgeBody {
	exact: Array<string>;
	prefix: Array<string>;
}

interface RecordedRequest {
	authorization: string | null;
	contentType: string | null;
	body: PurgeBody;
}

function createHarness() {
	const kvClient = new MockKVProvider();
	setWorkerDependenciesForTest({kvClient});
	return {kvClient, queue: new CachePurgeQueue(kvClient)};
}

function recordPurges(respond: (body: PurgeBody) => Response | Promise<Response>): Array<RecordedRequest> {
	const requests: Array<RecordedRequest> = [];
	server.use(
		http.post(ENDPOINT, async ({request}) => {
			const body = (await request.json()) as PurgeBody;
			requests.push({
				authorization: request.headers.get('authorization'),
				contentType: request.headers.get('content-type'),
				body,
			});
			return respond(body);
		}),
	);
	return requests;
}

function entryCount(body: PurgeBody): number {
	return body.exact.length + body.prefix.length;
}

async function members(kvClient: MockKVProvider, key: string): Promise<Array<string>> {
	return (await kvClient.smembers(key)).sort();
}

function sorted(values: Array<string>): Array<string> {
	return [...values].sort();
}

describe('processCachePurgeQueue', () => {
	let previousCachePurge: APICachePurgeConfig;

	beforeEach(() => {
		previousCachePurge = {adapter: Config.cachePurge.adapter, http: Config.cachePurge.http};
		Config.cachePurge.adapter = 'http';
		Config.cachePurge.http = {endpoint: ENDPOINT, token: 'test-token', timeoutMs: 50};
	});

	afterEach(() => {
		Config.cachePurge.adapter = previousCachePurge.adapter;
		Config.cachePurge.http = previousCachePurge.http;
		clearWorkerDependencies();
		vi.useRealTimers();
	});

	it('sends queued exact and prefix entries in one request and drains the queue', async () => {
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/a_abc`, `${MEDIA}/emojis/9.webp`, `${MEDIA}/attachments/1/2/ação.png`];
		const prefix = [`${MEDIA}/some/dir/`];
		await queue.addUrls([...exact, ...prefix]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(requests[0]!.authorization).toBe('Bearer test-token');
		expect(requests[0]!.contentType).toBe('application/json');
		expect(sorted(requests[0]!.body.exact)).toEqual(sorted(exact));
		expect(requests[0]!.body.prefix).toEqual(prefix);
		expect(await members(kvClient, EXACT_KEY)).toEqual([]);
		expect(await members(kvClient, PREFIX_KEY)).toEqual([]);
	});

	it('strips a trailing asterisk from a prefix entry and keeps a trailing slash', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/stickers/1**`, `${MEDIA}/emojis/`]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(requests[0]!.body.exact).toEqual([]);
		expect(sorted(requests[0]!.body.prefix)).toEqual([`${MEDIA}/emojis/`, `${MEDIA}/stickers/1*`]);
		expect(await members(kvClient, PREFIX_KEY)).toEqual([]);
	});

	it('sends no authorization header when no token is configured', async () => {
		Config.cachePurge.http.token = '';
		const {queue} = createHarness();
		await queue.addUrls([`${MEDIA}/avatars/1/abc`]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(requests[0]!.authorization).toBeNull();
	});

	it('requeues the whole batch after a server error', async () => {
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/abc`, `${MEDIA}/banners/1/def`];
		const prefix = [`${MEDIA}/some/dir/`];
		await queue.addUrls([...exact, ...prefix]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 503}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted(exact));
		expect(await members(kvClient, PREFIX_KEY)).toEqual(prefix);
	});

	it('requeues the batch when the endpoint does not answer in time', async () => {
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/abc`, `${MEDIA}/banners/1/def`];
		await queue.addUrls(exact);
		const requests = recordPurges(async () => {
			await delay('infinite');
			return new HttpResponse(null, {status: 204});
		});

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted(exact));
	});

	it('requeues the batch when the endpoint is unreachable', async () => {
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/abc`, `${MEDIA}/banners/1/def`];
		await queue.addUrls(exact);
		const requests = recordPurges(() => HttpResponse.error());

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted(exact));
	});

	it('requeues the batch on a redirect without following it', async () => {
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/abc`, `${MEDIA}/banners/1/def`];
		await queue.addUrls(exact);
		const redirectTarget = 'https://cache-purge.test/elsewhere';
		const redirectedRequests: Array<string> = [];
		server.use(
			http.all(redirectTarget, ({request}) => {
				redirectedRequests.push(request.method);
				return new HttpResponse(null, {status: 204});
			}),
		);
		const requests = recordPurges(() => new HttpResponse(null, {status: 302, headers: {Location: redirectTarget}}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(redirectedRequests).toEqual([]);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted(exact));
	});

	it('requeues the batch on an authorisation failure without splitting it', async () => {
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/abc`, `${MEDIA}/banners/1/def`, `${MEDIA}/emojis/9.webp`];
		await queue.addUrls(exact);
		const requests = recordPurges(() => new HttpResponse(null, {status: 401}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted(exact));
		expect(await members(kvClient, REJECTED_KEY)).toEqual([]);
	});

	it('sets aside only the entry the endpoint rejects on its own', async () => {
		const {kvClient, queue} = createHarness();
		const poison = `${MEDIA}/attachments/1/2/poison.png`;
		await queue.addUrls([`${MEDIA}/avatars/1/abc`, poison, `${MEDIA}/emojis/9.webp`]);
		const requests = recordPurges((body) =>
			body.exact.includes(poison) ? new HttpResponse(null, {status: 422}) : new HttpResponse(null, {status: 204}),
		);

		await processCachePurgeQueue({}, HELPERS);

		expect(requests.map((request) => entryCount(request.body))).toEqual([3, 1, 1, 1]);
		expect(await members(kvClient, REJECTED_KEY)).toEqual([poison]);
		expect(await members(kvClient, EXACT_KEY)).toEqual([]);
	});

	it('splits the batch when the endpoint answers 400', async () => {
		const {kvClient, queue} = createHarness();
		const poison = `${MEDIA}/attachments/1/2/poison.png`;
		await queue.addUrls([`${MEDIA}/avatars/1/abc`, poison, `${MEDIA}/emojis/9.webp`]);
		const requests = recordPurges((body) =>
			body.exact.includes(poison) ? new HttpResponse(null, {status: 400}) : new HttpResponse(null, {status: 204}),
		);

		await processCachePurgeQueue({}, HELPERS);

		expect(requests.map((request) => entryCount(request.body))).toEqual([3, 1, 1, 1]);
		expect(await members(kvClient, REJECTED_KEY)).toEqual([poison]);
		expect(await members(kvClient, EXACT_KEY)).toEqual([]);
	});

	it('keeps untried exact and prefix entries queued when a single-entry retry fails', async () => {
		const {kvClient, queue} = createHarness();
		const first = `${MEDIA}/avatars/1/abc`;
		const second = `${MEDIA}/banners/1/def`;
		const third = `${MEDIA}/emojis/9.webp`;
		const directory = `${MEDIA}/some/dir/`;
		await queue.addUrls([first, second, third, directory]);
		let singleRequests = 0;
		const requests = recordPurges((body) => {
			if (entryCount(body) > 1) {
				return new HttpResponse(null, {status: 422});
			}
			singleRequests++;
			return new HttpResponse(null, {status: singleRequests === 1 ? 204 : 503});
		});

		await processCachePurgeQueue({}, HELPERS);

		expect(requests.map((request) => request.body.exact)).toEqual([[first, second, third], [first], [second]]);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted([second, third]));
		expect(await members(kvClient, PREFIX_KEY)).toEqual([directory]);
		expect(await members(kvClient, REJECTED_KEY)).toEqual([]);
	});

	it('requeues untried entries when the fallback runs out of time', async () => {
		vi.useFakeTimers({toFake: ['Date']});
		vi.setSystemTime(new Date('2026-09-13T00:00:00.000Z'));
		const {kvClient, queue} = createHarness();
		const entries = [1, 2, 3, 4, 5].map((index) => `${MEDIA}/avatars/${index}/abc`);
		await queue.addUrls(entries);
		const requests = recordPurges((body) => {
			if (entryCount(body) > 1) {
				return new HttpResponse(null, {status: 422});
			}
			vi.setSystemTime(Date.now() + 4_000);
			return new HttpResponse(null, {status: 204});
		});

		await processCachePurgeQueue({}, HELPERS);

		expect(requests.map((request) => request.body.exact)).toEqual([entries, [entries[0]], [entries[1]], [entries[2]]]);
		expect(await members(kvClient, EXACT_KEY)).toEqual(sorted([entries[3]!, entries[4]!]));
		expect(await members(kvClient, REJECTED_KEY)).toEqual([]);
	});

	it('leaves the queue untouched when the adapter is none', async () => {
		Config.cachePurge.adapter = 'none';
		const {kvClient, queue} = createHarness();
		const exact = [`${MEDIA}/avatars/1/abc`];
		const prefix = [`${MEDIA}/some/dir/`];
		await queue.addUrls([...exact, ...prefix]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toEqual([]);
		expect(await members(kvClient, EXACT_KEY)).toEqual(exact);
		expect(await members(kvClient, PREFIX_KEY)).toEqual(prefix);
		expect(await kvClient.get('cache_purge:budget:exact')).toBeNull();
		expect(await kvClient.get('cache_purge:budget:prefix')).toBeNull();
	});

	it('sends no more than the token bucket allows and refills at the configured rate', async () => {
		vi.useFakeTimers({toFake: ['Date']});
		vi.setSystemTime(new Date('2026-09-13T00:00:00.000Z'));
		const {kvClient, queue} = createHarness();
		await queue.addUrls([
			...Array.from({length: 200}, (_, index) => `${MEDIA}/avatars/${index}/abc`),
			...Array.from({length: 30}, (_, index) => `${MEDIA}/dirs/${index}/`),
		]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);
		await processCachePurgeQueue({}, HELPERS);
		vi.setSystemTime(Date.now() + 10_000);
		await processCachePurgeQueue({}, HELPERS);

		expect(requests.map((request) => [request.body.exact.length, request.body.prefix.length])).toEqual([
			[120, 20],
			[50, 5],
		]);
		expect(await kvClient.scard(EXACT_KEY)).toBe(30);
		expect(await kvClient.scard(PREFIX_KEY)).toBe(5);
	});
});
