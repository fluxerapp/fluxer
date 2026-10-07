// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import '../main/LocalAppTestSupport.test.mjs';

const {CHANNEL_APP_PROTOCOLS, CHANNEL_APP_URLS, DOWNLOAD_PAGE_URLS} = await import('./Constants.ts');

describe('where each channel points', () => {
	test('every channel maps to its own legacy app origin', () => {
		assert.deepEqual(CHANNEL_APP_URLS, {
			stable: 'https://web.fluxer.app',
			canary: 'https://web.canary.fluxer.app',
			development: 'http://localhost:8088',
		});
	});

	test('the development build never claims the production deep link scheme', () => {
		assert.equal(CHANNEL_APP_PROTOCOLS.stable, 'fluxer');
		assert.equal(CHANNEL_APP_PROTOCOLS.canary, 'fluxer');
		assert.notEqual(CHANNEL_APP_PROTOCOLS.development, 'fluxer');
		assert.match(CHANNEL_APP_PROTOCOLS.development, /^[a-z][a-z0-9+.-]*$/u);
	});

	test('every channel maps to its own download page', () => {
		assert.deepEqual(DOWNLOAD_PAGE_URLS, {
			stable: 'https://fluxer.app/download',
			canary: 'https://canary.fluxer.app/download',
			development: 'http://localhost:8088/download',
		});
	});

	test('no channel borrows another channel host', () => {
		for (const table of [CHANNEL_APP_URLS, DOWNLOAD_PAGE_URLS]) {
			const hosts = Object.values(table).map((url) => new URL(url).host);
			assert.equal(
				new Set(hosts).size,
				hosts.length,
				`two channels resolve to the same host in ${JSON.stringify(table)}. A build would then update itself, or send its users to download, from the wrong channel.`,
			);
		}
	});
});
