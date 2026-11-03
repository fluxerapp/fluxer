// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APICachePurgeConfig} from '@app/api/config/APIConfig';
import {createControllerCachePurgeAdapter} from '@app/api/infrastructure/ControllerCachePurgeAdapter';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {describe, expect, it} from 'vitest';

const ENDPOINT = 'https://controller.test/v1/orgs/org-a/sites/site-b/purge';

const CONFIG: APICachePurgeConfig = {
	adapter: 'controller',
	http: {endpoint: '', token: '', timeoutMs: 10_000},
	controller: {endpoint: ENDPOINT, token: 'pak_test', timeoutMs: 10_000},
};

function prefixes(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => `media.test/attachments/1/${index}/photo`);
}

function serveController(statuses: Array<number>): Array<number> {
	const chunkSizes: Array<number> = [];
	server.use(
		http.post(ENDPOINT, async ({request}) => {
			const {patterns} = (await request.json()) as {patterns: Array<string>};
			chunkSizes.push(patterns.length);
			return new HttpResponse(null, {status: statuses[chunkSizes.length - 1] ?? 202});
		}),
	);
	return chunkSizes;
}

describe('createControllerCachePurgeAdapter', () => {
	it('splits a batch into requests of at most 30 patterns', async () => {
		const chunkSizes = serveController([]);
		const outcome = await createControllerCachePurgeAdapter(CONFIG).purge(prefixes(65));
		expect(outcome).toEqual({kind: 'purged'});
		expect(chunkSizes).toEqual([30, 30, 5]);
	});

	it('stops at the first chunk that fails so the whole batch is retried', async () => {
		const chunkSizes = serveController([202, 503]);
		const outcome = await createControllerCachePurgeAdapter(CONFIG).purge(prefixes(90));
		expect(outcome).toEqual({kind: 'failed', status: 503, error: null});
		expect(chunkSizes).toEqual([30, 30]);
	});
});
