// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {beforeEach, describe, test} from 'node:test';
import {installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';

const NETWORK_KEY = '__fluxerDesktopDiscoveryAliasTestNetwork__';
const network = {served: new Map(), fetched: [], anchored: []};
globalThis[NETWORK_KEY] = network;

installElectronStub();
installTestModuleStub(
	'@electron/main/SelectedInstanceFetch',
	`const network = globalThis[${JSON.stringify(NETWORK_KEY)}];
	export function getDesktopSelectedInstanceClient() {
		return {
			async fetch({url, expectedOrigin}) {
				network.fetched.push({url, expectedOrigin});
				const document = network.served.get(new URL(url).origin);
				if (document === undefined || !url.endsWith('/.well-known/fluxer')) {
					return {ok: false, status: 404, headers: {}, body: null};
				}
				return {
					ok: true,
					status: 200,
					headers: {'content-type': 'application/json'},
					body: Buffer.from(JSON.stringify(document), 'utf8'),
				};
			},
		};
	}`,
);
installTestModuleStub(
	'@electron/main/DesktopOutboundHTTP',
	`const network = globalThis[${JSON.stringify(NETWORK_KEY)}];
	export const DesktopAddressRequirement = Object.freeze({ANY: 'any', PUBLIC: 'public'});
	export const requireDesktopHTTPOrigin = (value) => new URL(value).origin;
	export const isDesktopHostResolutionFailure = () => false;
	export function getDesktopOutboundHTTP() {
		return {
			async registerAnchoredOrigins(request) {
				network.anchored.push(request);
				return 'public';
			},
		};
	}`,
);

const {resolveDesktopRuntimePlan} = await import('@electron/main/DesktopRuntimeDiscovery');
const {DEPLOYED_OFFICIAL_DOCUMENT} = await import('@fluxer/instance_bootstrap/src/__tests__/DiscoveryFixtures');

function officialDocument(clientOrigin) {
	return {
		...DEPLOYED_OFFICIAL_DOCUMENT,
		endpoints: {
			...DEPLOYED_OFFICIAL_DOCUMENT.endpoints,
			api: `${clientOrigin}/api`,
			api_client: `${clientOrigin}/api`,
			webapp: clientOrigin,
		},
	};
}

const CHANNELS = Object.freeze([
	{migrated: 'https://fluxer.com', official: 'https://web.fluxer.app'},
	{migrated: 'https://canary.fluxer.com', official: 'https://web.canary.fluxer.app'},
]);

describe('desktop discovery for an account stored on the migrated official domain', () => {
	beforeEach(() => {
		network.served.clear();
		network.fetched.length = 0;
		network.anchored.length = 0;
		for (const {migrated, official} of CHANNELS) {
			network.served.set(migrated, officialDocument(official));
			network.served.set(official, officialDocument(official));
		}
	});

	for (const {migrated, official} of CHANNELS) {
		test(`${migrated}/api resolves the official instance at ${official}/api`, async () => {
			const plan = await resolveDesktopRuntimePlan({input: `${migrated}/api`, signal: null});

			assert.equal(plan.instanceKey, `${official}/api`);
			assert.equal(plan.endpoints.apiEndpoint, `${official}/api`);
			assert.deepEqual(
				network.fetched.map((request) => new URL(request.url).origin),
				[official],
			);
			assert.equal(network.fetched[0].expectedOrigin, official);
			assert.equal(network.anchored.length, 1);
			assert.equal(network.anchored[0].anchorOrigin, official);
		});

		test(`${official}/api keeps resolving as itself`, async () => {
			const plan = await resolveDesktopRuntimePlan({input: `${official}/api`, signal: null});

			assert.equal(plan.instanceKey, `${official}/api`);
			assert.deepEqual(
				network.fetched.map((request) => new URL(request.url).origin),
				[official],
			);
		});
	}

	test('another instance that serves a foreign API is still refused', async () => {
		network.served.set('https://chat.example.test', officialDocument('https://web.fluxer.app'));

		await assert.rejects(resolveDesktopRuntimePlan({input: 'https://chat.example.test/api', signal: null}), {
			name: 'DesktopRuntimeDiscoveryFailedError',
		});
	});
});
