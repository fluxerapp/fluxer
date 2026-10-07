// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();
const {runtimePlanFromServedDiscovery} = await import('@electron/main/DesktopRuntimeDiscovery');
const {DEPLOYED_OFFICIAL_DOCUMENT} = await import('@fluxer/instance_bootstrap/src/__tests__/DiscoveryFixtures');

describe('runtime plans from a served discovery document', () => {
	test('a document served by its own API origin builds a plan keyed on that origin', () => {
		const built = runtimePlanFromServedDiscovery(
			'https://api.fluxer.app/.well-known/fluxer',
			DEPLOYED_OFFICIAL_DOCUMENT,
		);
		assert.equal(built.instanceKey, 'https://api.fluxer.app');
		assert.equal(built.document, DEPLOYED_OFFICIAL_DOCUMENT);
	});

	test('a document that claims another instance API is refused before a plan exists', () => {
		const spoofed = {
			...DEPLOYED_OFFICIAL_DOCUMENT,
			endpoints: {
				...DEPLOYED_OFFICIAL_DOCUMENT.endpoints,
				gateway: 'wss://gw.evil.example',
				media: 'https://evil.example',
				webapp: 'https://evil.example',
				upload_relay: 'https://evil.example/upload',
			},
		};
		assert.throws(() => runtimePlanFromServedDiscovery('https://evil.example/.well-known/fluxer', spoofed), {
			name: 'InstanceDiscoveryOriginMismatchError',
		});
	});

	test('an api_client on another origin is refused even when api matches', () => {
		const spoofed = {
			...DEPLOYED_OFFICIAL_DOCUMENT,
			endpoints: {
				...DEPLOYED_OFFICIAL_DOCUMENT.endpoints,
				api: 'https://evil.example',
				api_client: 'https://api.fluxer.app',
			},
		};
		assert.throws(() => runtimePlanFromServedDiscovery('https://evil.example/.well-known/fluxer', spoofed), {
			name: 'InstanceDiscoveryOriginMismatchError',
		});
	});
});
