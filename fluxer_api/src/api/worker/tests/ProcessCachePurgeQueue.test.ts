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
const ENDPOINT = 'https://cache-purge.test/__cache/purge';
const MEDIA = 'https://media.test';
const QUEUE_KEY = 'cache_purge:queue';
const BUDGET_KEY = 'cache_purge:budget';
const REJECTED_KEY = 'cache_purge:rejected';

interface PurgeBody {
	prefixes: Array<string>;
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

async function members(kvClient: MockKVProvider, key: string): Promise<Array<string>> {
	return (await kvClient.smembers(key)).sort();
}

function sorted(values: Array<string>): Array<string> {
	return [...values].sort();
}

describe('processCachePurgeQueue', () => {
	let previousCachePurge: APICachePurgeConfig;
	let previousMedia: string;

	beforeEach(() => {
		previousCachePurge = {adapter: Config.cachePurge.adapter, http: Config.cachePurge.http};
		previousMedia = Config.endpoints.media;
		Config.endpoints.media = MEDIA;
		Config.cachePurge.adapter = 'http';
		Config.cachePurge.http = {endpoint: ENDPOINT, token: 'test-token', timeoutMs: 50};
	});

	afterEach(() => {
		Config.cachePurge.adapter = previousCachePurge.adapter;
		Config.cachePurge.http = previousCachePurge.http;
		Config.endpoints.media = previousMedia;
		clearWorkerDependencies();
		vi.useRealTimers();
	});

	it('sends host qualified prefixes in one request and drains the queue', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([
			`${MEDIA}/avatars/1/a_b35cc3d3`,
			`${MEDIA}/emojis/9.webp`,
			`${MEDIA}/attachments/1/2/ação.png`,
		]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(requests[0]!.authorization).toBe('Bearer test-token');
		expect(requests[0]!.contentType).toBe('application/json');
		expect(sorted(requests[0]!.body.prefixes)).toEqual(
			sorted([
				'media.test/avatars/1/a_b35cc3d3',
				'media.test/avatars/1/b35cc3d3',
				'media.test/emojis/9',
				'media.test/attachments/1/2/ação',
			]),
		);
		expect(await members(kvClient, QUEUE_KEY)).toEqual([]);
	});

	it('collapses every extension of one asset into a single prefix', async () => {
		const {queue} = createHarness();
		await queue.addUrls([`${MEDIA}/stickers/7.webp`, `${MEDIA}/stickers/7.gif`, `${MEDIA}/stickers/7.png`]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(requests[0]!.body.prefixes).toEqual(['media.test/stickers/7']);
	});

	it.each([200, 202, 204])('treats %i as accepted and drops the batch', async (status) => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`]);
		const requests = recordPurges(() => new HttpResponse(null, {status}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, QUEUE_KEY)).toEqual([]);
		expect(await members(kvClient, REJECTED_KEY)).toEqual([]);
	});

	it('never queues a URL that is not a media CDN object', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls(['https://elsewhere.test/avatars/1/b35cc3d3', `${MEDIA}/avatars`, 'not-a-url']);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toEqual([]);
		expect(await members(kvClient, QUEUE_KEY)).toEqual([]);
	});

	it('requeues the whole batch after a server error', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`, `${MEDIA}/attachments/1/3/b.png`]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 503}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, QUEUE_KEY)).toEqual([
			'media.test/attachments/1/2/a',
			'media.test/attachments/1/3/b',
		]);
		expect(await members(kvClient, REJECTED_KEY)).toEqual([]);
	});

	it('requeues the batch when the endpoint does not answer in time', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`]);
		const requests = recordPurges(async () => {
			await delay('infinite');
			return new HttpResponse(null, {status: 204});
		});

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, QUEUE_KEY)).toEqual(['media.test/attachments/1/2/a']);
	});

	it('requeues the batch when the endpoint is unreachable', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`]);
		const requests = recordPurges(() => HttpResponse.error());

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, QUEUE_KEY)).toEqual(['media.test/attachments/1/2/a']);
	});

	it('requeues the batch on a redirect without following it', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`]);
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
		expect(await members(kvClient, QUEUE_KEY)).toEqual(['media.test/attachments/1/2/a']);
	});

	it('requeues rather than discards when the bearer token is refused', async () => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 401}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, QUEUE_KEY)).toEqual(['media.test/attachments/1/2/a']);
		expect(await members(kvClient, REJECTED_KEY)).toEqual([]);
	});

	it.each([400, 422])('sets the batch aside when the endpoint answers %i', async (status) => {
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`, `${MEDIA}/emojis/9.webp`]);
		const requests = recordPurges(() => new HttpResponse(null, {status}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toHaveLength(1);
		expect(await members(kvClient, QUEUE_KEY)).toEqual([]);
		expect(await members(kvClient, REJECTED_KEY)).toEqual(['media.test/attachments/1/2/a', 'media.test/emojis/9']);
	});

	it('leaves the queue untouched when the adapter is none', async () => {
		Config.cachePurge.adapter = 'none';
		const {kvClient, queue} = createHarness();
		await queue.addUrls([`${MEDIA}/attachments/1/2/a.png`]);
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);

		expect(requests).toEqual([]);
		expect(await members(kvClient, QUEUE_KEY)).toEqual(['media.test/attachments/1/2/a']);
		expect(await kvClient.get(BUDGET_KEY)).toBeNull();
	});

	it('sends no more than the token bucket allows and refills at the configured rate', async () => {
		vi.useFakeTimers({toFake: ['Date']});
		vi.setSystemTime(new Date('2026-09-16T00:00:00.000Z'));
		const {kvClient, queue} = createHarness();
		await queue.addUrls(Array.from({length: 200}, (_, index) => `${MEDIA}/attachments/1/${index}/a.png`));
		const requests = recordPurges(() => new HttpResponse(null, {status: 204}));

		await processCachePurgeQueue({}, HELPERS);
		await processCachePurgeQueue({}, HELPERS);
		vi.setSystemTime(Date.now() + 10_000);
		await processCachePurgeQueue({}, HELPERS);

		expect(requests.map((request) => request.body.prefixes.length)).toEqual([120, 50]);
		expect(await kvClient.scard(QUEUE_KEY)).toBe(30);
	});
});
