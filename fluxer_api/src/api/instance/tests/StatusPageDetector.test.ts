// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {StatusPageDetector} from '@app/api/instance/StatusPageDetector';
import {server} from '@app/api/test/msw/server';
import {createPublicInternetRequestUrlPolicy} from '@pkgs/http_client/src/PublicInternetRequestUrlPolicy';
import {HttpResponse, http} from 'msw';
import {beforeEach, describe, expect, it} from 'vitest';

const URL_A = 'https://status.example.com';
const URL_B = 'https://status.example.org';
const SUMMARY = {page: {name: 'Status', status: 'UP'}, activeIncidents: [], activeMaintenances: []};
const ALLOW_ANY_ORIGIN = {'access-control-allow-origin': '*'};
const MINUTE = 60 * 1000;
const HOUR = 60 * MINUTE;

type Answer = () => Response;

const instatus: Answer = () => HttpResponse.json(SUMMARY, {headers: ALLOW_ANY_ORIGIN});
const unavailable: Answer = () => new HttpResponse('busy', {status: 503});
const notFound: Answer = () => new HttpResponse('not found', {status: 404});

describe('StatusPageDetector', () => {
	let now = 0;
	let detector: StatusPageDetector;
	let requests: Array<{url: string; origin: string | null}>;

	function answer(url: string, answers: Array<Answer>): void {
		server.use(
			http.get(`${url}/summary.json`, ({request}) => {
				requests.push({url, origin: request.headers.get('origin')});
				const next = answers.length > 1 ? answers.shift()! : answers[0]!;
				return next();
			}),
		);
	}

	function requestsTo(url: string): number {
		return requests.filter((request) => request.url === url).length;
	}

	beforeEach(() => {
		now = 1_000_000;
		requests = [];
		detector = new StatusPageDetector({
			now: () => now,
			requestUrlPolicy: createPublicInternetRequestUrlPolicy({lookupHost: async () => ['93.184.215.14']}),
		});
	});

	it('detects an Instatus page that allows the web app origin', async () => {
		answer(URL_A, [instatus]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(true);
		expect(requests).toEqual([{url: URL_A, origin: new URL(Config.endpoints.webApp).origin}]);
	});

	it('publishes false for a page that is not Instatus and waits a day before checking it again', async () => {
		answer(URL_A, [notFound]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		now += 23 * HOUR;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		expect(requestsTo(URL_A)).toBe(1);
		now += HOUR;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		expect(requestsTo(URL_A)).toBe(2);
	});

	it('publishes false for HTML and for JSON from another provider', async () => {
		answer(URL_A, [() => new HttpResponse('<!doctype html><html></html>', {status: 200, headers: ALLOW_ANY_ORIGIN})]);
		answer(URL_B, [() => HttpResponse.json({monitors: [], heartbeatList: {}}, {headers: ALLOW_ANY_ORIGIN})]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		await expect(detector.isInstatus(URL_B)).resolves.toBe(false);
	});

	it('publishes false for an Instatus summary the browser may not read across origins', async () => {
		answer(URL_A, [() => HttpResponse.json(SUMMARY)]);
		answer(URL_B, [
			() => HttpResponse.json(SUMMARY, {headers: {'access-control-allow-origin': 'https://other.example'}}),
		]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		await expect(detector.isInstatus(URL_B)).resolves.toBe(false);
	});

	it('retries a transient failure after 1 and then 5 minutes and recovers', async () => {
		answer(URL_A, [unavailable, unavailable, instatus]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		now += MINUTE - 1;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		expect(requestsTo(URL_A)).toBe(1);
		now += 1;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		expect(requestsTo(URL_A)).toBe(2);
		now += 5 * MINUTE - 1;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		expect(requestsTo(URL_A)).toBe(2);
		now += 1;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(true);
		expect(requestsTo(URL_A)).toBe(3);
	});

	it('treats a timeout status, a rate limit and a network error as transient', async () => {
		answer(URL_A, [
			() => new HttpResponse(null, {status: 408}),
			() => new HttpResponse(null, {status: 429}),
			() => HttpResponse.error(),
			instatus,
		]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		now += MINUTE;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		now += 5 * MINUTE;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		now += 15 * MINUTE;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(true);
		expect(requestsTo(URL_A)).toBe(4);
	});

	it('keeps a known Instatus page while a later recheck fails', async () => {
		answer(URL_A, [instatus, unavailable]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(true);
		now += 6 * HOUR;
		await expect(detector.isInstatus(URL_A)).resolves.toBe(true);
		expect(requestsTo(URL_A)).toBe(2);
	});

	it('checks a new status page URL straight away and forgets the old verdict', async () => {
		answer(URL_A, [notFound]);
		answer(URL_B, [instatus]);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		await expect(detector.isInstatus(URL_B)).resolves.toBe(true);
		await expect(detector.isInstatus(URL_A)).resolves.toBe(false);
		expect(requestsTo(URL_A)).toBe(2);
		expect(requestsTo(URL_B)).toBe(1);
	});

	it('sends nothing when no status page is set', async () => {
		await expect(detector.isInstatus(null)).resolves.toBe(false);
		expect(requests).toEqual([]);
	});

	it('sends nothing to a status page on a private address', async () => {
		const privateDetector = new StatusPageDetector({now: () => now});
		answer('http://10.0.0.5', [instatus]);
		await expect(privateDetector.isInstatus('http://10.0.0.5')).resolves.toBe(false);
		expect(requests).toEqual([]);
	});
});
