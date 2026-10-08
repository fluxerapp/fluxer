// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const calls = [];
installElectronStub({
	net: {
		fetch: async (input, init) => {
			calls.push({input, init});
			return new Response('ok', {status: 200});
		},
	},
});

const {moduleNetworkFetch} = await import('@electron/main/ModuleNetworkFetch');

describe('moduleNetworkFetch', () => {
	test('goes through the Chromium network stack, past custom protocol handlers and the HTTP cache', async () => {
		calls.length = 0;
		const signal = AbortSignal.timeout(1000);
		const response = await moduleNetworkFetch(
			new URL('https://pkgs.invalid/desktop/canary/darwin/arm64/modules.json'),
			{
				headers: {'if-none-match': '"manifest-1"'},
				signal,
			},
		);
		assert.equal(response.status, 200);
		assert.equal(calls.length, 1);
		assert.equal(calls[0].input, 'https://pkgs.invalid/desktop/canary/darwin/arm64/modules.json');
		assert.deepEqual(calls[0].init.headers, {'if-none-match': '"manifest-1"'});
		assert.equal(calls[0].init.signal, signal);
		assert.equal(calls[0].init.bypassCustomProtocolHandlers, true);
		assert.equal(calls[0].init.cache, 'no-store');
	});

	test('passes string urls through unchanged', async () => {
		calls.length = 0;
		await moduleNetworkFetch('https://pkgs.invalid/desktop/canary/modules/fluxer_renderer/x/package.br');
		assert.equal(calls[0].input, 'https://pkgs.invalid/desktop/canary/modules/fluxer_renderer/x/package.br');
		assert.equal(calls[0].init.bypassCustomProtocolHandlers, true);
	});
});
