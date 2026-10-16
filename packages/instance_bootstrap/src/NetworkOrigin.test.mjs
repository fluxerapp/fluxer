// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';

const {
	HTTP_NETWORK_PROTOCOLS,
	WEBSOCKET_NETWORK_PROTOCOLS,
	normalizeCanonicalNetworkEndpoint,
	normalizeHTTPNetworkOrigin,
} = await import('./NetworkOrigin.ts');

const GATEWAY_RULE = {protocols: WEBSOCKET_NETWORK_PROTOCOLS, allowPath: true, allowRelative: false};
const API_RULE = {protocols: HTTP_NETWORK_PROTOCOLS, allowPath: true, allowRelative: true};

describe('canonicalising a network origin', () => {
	test('a written default port collapses to the same origin as the bare host', () => {
		assert.equal(normalizeHTTPNetworkOrigin('fluxer.app:443'), 'https://fluxer.app');
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app:443'), 'https://fluxer.app');
		assert.equal(normalizeHTTPNetworkOrigin('http://fluxer.app:80'), 'http://fluxer.app');
		assert.equal(normalizeHTTPNetworkOrigin('https://[::1]:443'), 'https://[::1]');
	});

	test('one origin has one canonical form, the one URL.origin agrees with', () => {
		for (const value of ['fluxer.app', 'fluxer.app:443', 'https://fluxer.app:443', 'https://fluxer.app/']) {
			assert.equal(
				normalizeHTTPNetworkOrigin(value),
				'https://fluxer.app',
				`${value} must not gain a second canonical form`,
			);
		}
		for (const value of ['fluxer.app:443', 'https://fluxer.app:8443', 'http://fluxer.app:80', 'http://[::1]:8080']) {
			const normalized = normalizeHTTPNetworkOrigin(value);
			assert.equal(new URL(normalized).origin, normalized, `${value} must canonicalise the way URL.origin does`);
		}
	});

	test('a host slices off the canonical origin without a port riding along', () => {
		const origin = normalizeHTTPNetworkOrigin('web.fluxer.app:443');
		assert.equal(origin.slice('https://'.length), 'web.fluxer.app');
	});

	test('a non default port is kept', () => {
		assert.equal(normalizeHTTPNetworkOrigin('fluxer.app:8443'), 'https://fluxer.app:8443');
		assert.equal(normalizeHTTPNetworkOrigin('http://localhost:3000'), 'http://localhost:3000');
		assert.equal(normalizeHTTPNetworkOrigin('https://[::1]:8443'), 'https://[::1]:8443');
	});

	test('a leading zero port is normalised the way URL normalises it', () => {
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app:0443'), 'https://fluxer.app');
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app:08443'), 'https://fluxer.app:8443');
	});

	test('port zero, credentials, a path, a query and a fragment are all refused', () => {
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app:0'), null);
		assert.equal(normalizeHTTPNetworkOrigin('https://user:pass@fluxer.app'), null);
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app/api'), null);
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app?a=1'), null);
		assert.equal(normalizeHTTPNetworkOrigin('https://fluxer.app#a'), null);
		assert.equal(normalizeHTTPNetworkOrigin('//fluxer.app'), null);
		assert.equal(normalizeHTTPNetworkOrigin('wss://fluxer.app'), null);
		assert.equal(normalizeHTTPNetworkOrigin(42), null);
	});
});

describe('canonicalising an endpoint that carries a path', () => {
	test('a websocket endpoint drops its written default port too', () => {
		assert.equal(
			normalizeCanonicalNetworkEndpoint('wss://gateway.fluxer.app:443', GATEWAY_RULE),
			'wss://gateway.fluxer.app',
		);
		assert.equal(normalizeCanonicalNetworkEndpoint('ws://localhost:80/ws', GATEWAY_RULE), 'ws://localhost/ws');
		assert.equal(
			normalizeCanonicalNetworkEndpoint('wss://gateway.fluxer.app:8443', GATEWAY_RULE),
			'wss://gateway.fluxer.app:8443',
		);
	});

	test('an api endpoint keeps its path and still collapses the default port', () => {
		assert.equal(
			normalizeCanonicalNetworkEndpoint('https://fluxer.app:443/api/v1/', API_RULE),
			'https://fluxer.app/api/v1',
		);
		assert.equal(normalizeCanonicalNetworkEndpoint('/api', API_RULE), '/api');
	});
});
