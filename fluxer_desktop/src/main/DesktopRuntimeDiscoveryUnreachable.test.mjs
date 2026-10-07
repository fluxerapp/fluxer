// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {beforeEach, describe, test} from 'node:test';
import {installDesktopAppStorageStub, installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';

const NETWORK_KEY = '__fluxerDesktopDiscoveryUnreachableTestNetwork__';
const network = {respond: null, fetched: [], resolvable: true, registrations: []};
globalThis[NETWORK_KEY] = network;

const markers = new Map();
const storage = {
	refuseWrites: false,
	getMarker: async (key) => markers.get(key) ?? null,
	setMarker: async (key, value) => {
		if (storage.refuseWrites) throw new Error('the store is read-only');
		markers.set(key, value);
	},
};

installElectronStub();
installDesktopAppStorageStub(storage);
installTestModuleStub(
	'@electron/main/SelectedInstanceFetch',
	`const network = globalThis[${JSON.stringify(NETWORK_KEY)}];
	export function getDesktopSelectedInstanceClient() {
		return {
			async fetch({url}) {
				network.fetched.push(url);
				return network.respond(url);
			},
		};
	}`,
);
installTestModuleStub(
	'@electron/main/DesktopOutboundHTTP',
	`const network = globalThis[${JSON.stringify(NETWORK_KEY)}];
	export const DesktopAddressRequirement = Object.freeze({ANY: 'any', PUBLIC: 'public'});
	export const requireDesktopHTTPOrigin = (value) => new URL(value).origin;
	export const isDesktopHostResolutionFailure = (error) => error?.syscall === 'getaddrinfo';
	export function getDesktopOutboundHTTP() {
		return {
			async registerAnchoredOrigins(registration) {
				network.registrations.push(registration);
				if (network.resolvable) return 'public';
				if (registration.unresolvedAnchorRequirement != null) return registration.unresolvedAnchorRequirement;
				throw Object.assign(new Error('getaddrinfo EAI_AGAIN api.fluxer.app'), {
					code: 'EAI_AGAIN',
					syscall: 'getaddrinfo',
				});
			},
		};
	}`,
);

const {resolveDesktopRuntimePlan} = await import('@electron/main/DesktopRuntimeDiscovery');
const {DEPLOYED_OFFICIAL_DOCUMENT} = await import('@fluxer/instance_bootstrap/src/__tests__/DiscoveryFixtures');
const {DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME} = await import(
	'@fluxer/desktop_ipc/src/LocalAppRuntimeContract'
);

const API_ENDPOINT = 'https://api.fluxer.app';
const MARKER_KEY = `runtime_discovery.v1:${API_ENDPOINT}`;

function served(document) {
	return () => ({
		ok: true,
		status: 200,
		headers: {'content-type': 'application/json'},
		body: Buffer.from(JSON.stringify(document), 'utf8'),
	});
}

function refused() {
	return () => {
		throw Object.assign(new Error('connect ECONNREFUSED 127.0.0.1:9'), {code: 'ECONNREFUSED'});
	};
}

function status(code) {
	return () => ({ok: false, status: code, headers: {}, body: null});
}

function resolve(signal = null) {
	return resolveDesktopRuntimePlan({input: API_ENDPOINT, signal});
}

describe('desktop discovery while the instance is unreachable', () => {
	beforeEach(() => {
		markers.clear();
		storage.refuseWrites = false;
		network.fetched.length = 0;
		network.resolvable = true;
		network.registrations.length = 0;
		network.respond = served(DEPLOYED_OFFICIAL_DOCUMENT);
	});

	test('a served document is remembered for its API endpoint', async () => {
		const plan = await resolve();
		const remembered = JSON.parse(markers.get(MARKER_KEY));
		assert.deepEqual(remembered.document, DEPLOYED_OFFICIAL_DOCUMENT);
		assert.equal(new URL(remembered.url).origin, API_ENDPOINT);
		assert.equal(plan.instanceKey, API_ENDPOINT);
	});

	test('a refused connection resolves the plan from the last served document', async () => {
		const online = await resolve();
		network.respond = refused();
		const offline = await resolve();
		assert.equal(offline.instanceKey, online.instanceKey);
		assert.deepEqual(offline.endpoints, online.endpoints);
		assert.deepEqual(offline.document, DEPLOYED_OFFICIAL_DOCUMENT);
	});

	test('a name that cannot resolve still resolves the plan from the last served document', async () => {
		const online = await resolve();
		assert.equal(JSON.parse(markers.get(MARKER_KEY)).anchorRequirement, 'public');
		network.respond = refused();
		network.resolvable = false;
		const offline = await resolve();
		assert.equal(offline.instanceKey, online.instanceKey);
		assert.equal(network.registrations.at(-1).unresolvedAnchorRequirement, 'public');
	});

	test('a remembered document without an anchor requirement fails as unreachable when the name cannot resolve', async () => {
		await resolve();
		const remembered = JSON.parse(markers.get(MARKER_KEY));
		markers.set(MARKER_KEY, JSON.stringify({url: remembered.url, document: remembered.document}));
		network.respond = refused();
		network.resolvable = false;
		await assert.rejects(resolve(), {name: DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME});
	});

	test('a gateway error from the instance resolves the plan from the last served document', async () => {
		await resolve();
		network.respond = status(503);
		assert.equal((await resolve()).instanceKey, API_ENDPOINT);
	});

	test('an unreachable instance that was never served fails as unreachable', async () => {
		network.respond = refused();
		await assert.rejects(resolve(), (error) => {
			assert.equal(error.name, DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME);
			assert.match(error.message, /ECONNREFUSED/u);
			return true;
		});
	});

	test('an instance that answers without a discovery document never falls back', async () => {
		await resolve();
		network.respond = status(404);
		await assert.rejects(resolve(), {name: 'DesktopRuntimeDiscoveryFailedError'});
	});

	test('an instance that now serves a foreign API never falls back', async () => {
		await resolve();
		network.respond = served({
			...DEPLOYED_OFFICIAL_DOCUMENT,
			endpoints: {...DEPLOYED_OFFICIAL_DOCUMENT.endpoints, api: 'https://evil.example/api'},
		});
		await assert.rejects(resolve(), {name: 'DesktopRuntimeDiscoveryFailedError'});
	});

	test('a cancelled resolution never falls back', async () => {
		await resolve();
		const controller = new AbortController();
		network.respond = () => {
			controller.abort();
			throw new Error('aborted by its caller');
		};
		await assert.rejects(resolve(controller.signal), {name: 'DesktopRuntimeDiscoveryFailedError'});
	});

	test('a remembered document that no longer validates is ignored', async () => {
		markers.set(MARKER_KEY, JSON.stringify({url: 'https://evil.example/.well-known/fluxer', document: {}}));
		network.respond = refused();
		await assert.rejects(resolve(), {name: DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME});
	});

	test('a store that refuses the write still resolves the served plan', async () => {
		storage.refuseWrites = true;
		assert.equal((await resolve()).instanceKey, API_ENDPOINT);
		assert.equal(markers.size, 0);
	});
});
