// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {OFFICIAL_CLIENT_API_ENDPOINTS, officialClientApiEndpointForAlias} = await import(
	'@fluxer/instance_bootstrap/src/OfficialInstance'
);
const {localAppRuntimeInstanceKey} = await import('./LocalAppRuntimePlans.ts');

describe('resolving an official client API alias', () => {
	test('the migrated domain same-origin API is the official instance of its channel', () => {
		assert.equal(officialClientApiEndpointForAlias('https://fluxer.com/api'), OFFICIAL_CLIENT_API_ENDPOINTS.stable);
		assert.equal(
			officialClientApiEndpointForAlias('https://canary.fluxer.com/api'),
			OFFICIAL_CLIENT_API_ENDPOINTS.canary,
		);
	});

	test('the legacy web origins already are the official instance of their channel', () => {
		assert.equal(officialClientApiEndpointForAlias('https://web.fluxer.app/api'), OFFICIAL_CLIENT_API_ENDPOINTS.stable);
		assert.equal(
			officialClientApiEndpointForAlias('https://web.canary.fluxer.app/api'),
			OFFICIAL_CLIENT_API_ENDPOINTS.canary,
		);
	});

	test('the official endpoint is its own instance key in the main process', () => {
		for (const endpoint of Object.values(OFFICIAL_CLIENT_API_ENDPOINTS)) {
			assert.equal(localAppRuntimeInstanceKey(endpoint), endpoint);
		}
	});

	test('spelling differences do not create a second identity', () => {
		for (const spelling of [
			'https://canary.fluxer.com/api/',
			'https://canary.fluxer.com/api//',
			'HTTPS://CANARY.FLUXER.COM/api',
			'https://canary.fluxer.com:443/api',
			'  https://canary.fluxer.com/api  ',
		]) {
			assert.equal(officialClientApiEndpointForAlias(spelling), OFFICIAL_CLIENT_API_ENDPOINTS.canary, spelling);
		}
	});

	test('anything that is not an official same-origin client API is left alone', () => {
		for (const other of [
			'https://canary.fluxer.com',
			'https://canary.fluxer.com/',
			'https://canary.fluxer.com/api/v1',
			'https://canary.fluxer.com/API',
			'http://canary.fluxer.com/api',
			'https://canary.fluxer.com:8443/api',
			'https://user:pw@canary.fluxer.com/api',
			'https://canary.fluxer.com/api?x=1',
			'https://canary.fluxer.com/api#f',
			'https://api.fluxer.app/api',
			'https://fluxer.app/api',
			'https://evil.fluxer.com.example.test/api',
			'https://chat.example.test/api',
			'/api',
			'',
			'not a url',
		]) {
			assert.equal(officialClientApiEndpointForAlias(other), null, other);
		}
	});
});
