// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();
installTestModuleStub(
	'@app/features/app/config/Config',
	`const unavailable = () => { throw new Error('Renderer build configuration is outside the instance-key test boundary'); };
	export default Object.freeze({
		get PUBLIC_BUILD_VERSION() { return unavailable(); },
		get PUBLIC_RELEASE_CHANNEL() { return unavailable(); },
	});`,
);
installTestModuleStub(
	'@app/features/platform/state/PrebootNetworkHandoff',
	'export function takePrebootDiscoveryResponse() { return null; }',
);

const {localAppRuntimeInstanceKey} = await import('./LocalAppRuntimePlans.ts');
const {runtimeInstanceKey} = await import('../../../fluxer_app/src/features/app/state/InstanceSnapshotStore.ts');

const AGREED_KEYS = Object.freeze([
	['https://web.fluxer.app/api', 'https://web.fluxer.app/api'],
	['https://web.fluxer.app/api/', 'https://web.fluxer.app/api'],
	['https://web.fluxer.app/api//', 'https://web.fluxer.app/api'],
	['HTTPS://WEB.FLUXER.APP/api', 'https://web.fluxer.app/api'],
	['https://web.fluxer.app', 'https://web.fluxer.app'],
	['https://web.fluxer.app/', 'https://web.fluxer.app'],
	['  https://web.fluxer.app/api  ', 'https://web.fluxer.app/api'],
	['https://web.fluxer.app/API', 'https://web.fluxer.app/API'],
	['http://localhost:3000/api', 'http://localhost:3000/api'],
	['http://127.0.0.1:3000/api/', 'http://127.0.0.1:3000/api'],
	['https://user:pw@web.fluxer.app/api', null],
	['https://:pw@web.fluxer.app/api', null],
	['https://web.fluxer.app/api?x=1', null],
	['https://web.fluxer.app/api#f', null],
	['ftp://x/api', null],
	['fluxer-app://app/api', null],
	['', null],
	['   ', null],
	['not a url', null],
]);

const ACCOUNT_STORAGE_KEY_SEPARATOR = '::';

describe('the instance key derivation agrees between the main process and the renderer', () => {
	test('the two implementations are not the same module', () => {
		assert.notEqual(localAppRuntimeInstanceKey, runtimeInstanceKey);
	});

	for (const [apiEndpoint, expected] of AGREED_KEYS) {
		test(`${JSON.stringify(apiEndpoint)} keys as ${JSON.stringify(expected)} on both sides`, () => {
			const main = localAppRuntimeInstanceKey(apiEndpoint);
			const renderer = runtimeInstanceKey({apiEndpoint});

			assert.equal(main, expected, 'the main-process derivation drifted');
			assert.equal(renderer, expected, 'the renderer derivation drifted');
			assert.equal(main, renderer, 'the /api/<key> route and the renderer would disagree');
		});
	}

	test('every agreed key survives the account storage key it is concatenated into', () => {
		const userId = '1234567890';
		for (const [apiEndpoint] of AGREED_KEYS) {
			const main = localAppRuntimeInstanceKey(apiEndpoint);
			const renderer = runtimeInstanceKey({apiEndpoint});
			if (main === null) {
				assert.equal(renderer, null, apiEndpoint);
				continue;
			}
			assert.equal(
				`${main}${ACCOUNT_STORAGE_KEY_SEPARATOR}${userId}`,
				`${renderer}${ACCOUNT_STORAGE_KEY_SEPARATOR}${userId}`,
				apiEndpoint,
			);
		}
	});
});
