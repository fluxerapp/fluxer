// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {createServer, type Server} from 'node:http';
import type {AddressInfo} from 'node:net';
import {instanceDiscoveryFixture} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {InstanceAgePolicy, InstanceDiscoveryResponse} from '@fluxer/instance_bootstrap/src/Types';
import {afterEach, beforeEach, expect, test, vi} from 'vitest';

const UK_ONLY: InstanceAgePolicy = {
	geos: [{country_code: 'GB', region_code: null, action: 'restrict', card_verification_available: false}],
};
const SWEDEN_BLOCKS_STOCKHOLM: InstanceAgePolicy = {
	geos: [{country_code: 'SE', region_code: 'AB', action: 'block', card_verification_available: false}],
};

const STOCKHOLM = {
	countryCode: 'SE',
	regionCode: 'AB',
	latitude: '59.3293',
	longitude: '18.0686',
	ageRestrictedGeos: [],
	ageBlockedGeos: [],
};

interface Instance {
	origin: string;
	apiEndpoint: string;
	requests: Array<string>;
	body: unknown;
	status: number;
	delayMs: number;
}

const servers: Array<Server> = [];
let instances: Array<Instance> = [];
let runtimeDocument: InstanceDiscoveryResponse | null;

async function startInstance(): Promise<Instance> {
	const instance: Instance = {origin: '', apiEndpoint: '', requests: [], body: STOCKHOLM, status: 200, delayMs: 0};
	const server = createServer((request, response) => {
		instance.requests.push(`${request.method} ${request.url ?? ''}`);
		const reply = (): void => {
			response.writeHead(instance.status, {
				'Content-Type': 'application/json',
				'Access-Control-Allow-Origin': '*',
			});
			response.end(JSON.stringify(instance.body));
		};
		if (request.method === 'GET' && instance.delayMs > 0) {
			setTimeout(reply, instance.delayMs);
			return;
		}
		reply();
	});
	await new Promise<void>((resolve) => {
		server.listen(0, '127.0.0.1', resolve);
	});
	servers.push(server);
	const {port} = server.address() as AddressInfo;
	instance.origin = `http://127.0.0.1:${port}`;
	instance.apiEndpoint = `${instance.origin}/api`;
	instances.push(instance);
	return instance;
}

function lookups(instance: Instance): Array<string> {
	return instance.requests.filter((request) => request.startsWith('GET '));
}

function documentFor(instance: Instance, agePolicy?: InstanceAgePolicy): InstanceDiscoveryResponse {
	return instanceDiscoveryFixture(instance.apiEndpoint, agePolicy);
}

function installRuntime(document: InstanceDiscoveryResponse | null): void {
	runtimeDocument = document;
}

async function loadRuntimeConfig() {
	vi.resetModules();
	const [{default: RuntimeConfig}, {runtimeSnapshotFromDiscovery}, {parseInstanceDiscoveryDocument}] =
		await Promise.all([
			import('@app/features/app/state/RuntimeConfig'),
			import('@app/features/app/state/InstanceSnapshotStore'),
			import('@fluxer/instance_bootstrap/src/Discovery'),
		]);
	if (runtimeDocument !== null) {
		RuntimeConfig.applySnapshot(runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(runtimeDocument)));
	}
	return RuntimeConfig;
}

async function loadGeoIPContext() {
	const RuntimeConfig = await loadRuntimeConfig();
	const {default: GeoIP} = await import('@app/features/app/state/GeoIP');
	return {GeoIP, RuntimeConfig};
}

async function loadGeoIP() {
	return (await loadGeoIPContext()).GeoIP;
}

beforeEach(() => {
	instances = [];
});

afterEach(async () => {
	await Promise.all(
		servers.splice(0).map(
			(server) =>
				new Promise<void>((resolve) => {
					server.close(() => resolve());
				}),
		),
	);
});

test('the served age policy seeds both tables before anything is fetched', async () => {
	const instance = await startInstance();
	installRuntime(documentFor(instance, SWEDEN_BLOCKS_STOCKHOLM));
	const GeoIP = await loadGeoIP();
	expect(GeoIP.ageRestrictedGeos).toEqual([]);
	expect(GeoIP.ageBlockedGeos).toEqual([{countryCode: 'SE', regionCode: 'AB'}]);
	expect(instance.requests).toEqual([]);
	await GeoIP.ready();
});

test('a document with no age policy falls back to the bundled table', async () => {
	const instance = await startInstance();
	installRuntime(documentFor(instance));
	const GeoIP = await loadGeoIP();
	expect(GeoIP.ageRestrictedGeos).toEqual([
		{countryCode: 'GB', regionCode: null},
		{countryCode: 'BR', regionCode: null},
	]);
	expect(GeoIP.ageBlockedGeos).toEqual([{countryCode: 'US', regionCode: 'MS'}]);
	await GeoIP.ready();
});

test('the location is resolved from GET /ip on the instance the document names', async () => {
	const instance = await startInstance();
	installRuntime(documentFor(instance, UK_ONLY));
	const GeoIP = await loadGeoIP();
	expect(GeoIP.resolution).toBe('pending');
	await GeoIP.ready();
	expect(lookups(instance)).toEqual(['GET /api/ip']);
	expect(GeoIP.resolution).toBe('resolved');
	expect(GeoIP.countryCode).toBe('SE');
	expect(GeoIP.regionCode).toBe('AB');
	expect(GeoIP.latitude).toBe('59.3293');
	expect(GeoIP.longitude).toBe('18.0686');
});

test('no active runtime never reaches the network and stays age gated', async () => {
	const instance = await startInstance();
	installRuntime(null);
	const GeoIP = await loadGeoIP();
	await GeoIP.ready();
	expect(instance.requests).toEqual([]);
	expect(GeoIP.resolution).toBe('unavailable');
	expect(GeoIP.ageRestrictedGeos).toEqual([
		{countryCode: 'GB', regionCode: null},
		{countryCode: 'BR', regionCode: null},
	]);
	expect(GeoIP.ageBlockedGeos).toEqual([{countryCode: 'US', regionCode: 'MS'}]);
});

test('a failed lookup leaves the gateway free to supply the country', async () => {
	const instance = await startInstance();
	instance.status = 500;
	installRuntime(documentFor(instance, SWEDEN_BLOCKS_STOCKHOLM));
	const GeoIP = await loadGeoIP();
	await GeoIP.ready();
	expect(GeoIP.resolution).toBe('unavailable');
	expect(GeoIP.countryCode).toBeNull();
	GeoIP.applyConnectionFallbackGeo({countryCode: 'SE', regionCode: 'AB', latitude: '1', longitude: '2'});
	expect(GeoIP.countryCode).toBe('SE');
});

test('the gateway never overwrites a resolved location', async () => {
	const instance = await startInstance();
	installRuntime(documentFor(instance, UK_ONLY));
	const GeoIP = await loadGeoIP();
	await GeoIP.ready();
	GeoIP.applyConnectionFallbackGeo({countryCode: 'GB', regionCode: null, latitude: '0', longitude: '0'});
	expect(GeoIP.countryCode).toBe('SE');
	expect(GeoIP.latitude).toBe('59.3293');
});

test('switching to another instance re-seeds the tables and re-resolves against it', async () => {
	const first = await startInstance();
	const second = await startInstance();
	second.body = {...STOCKHOLM, countryCode: 'GB', regionCode: null};
	installRuntime(documentFor(first, SWEDEN_BLOCKS_STOCKHOLM));
	const {GeoIP, RuntimeConfig} = await loadGeoIPContext();
	const [{runtimeSnapshotFromDiscovery}, {parseInstanceDiscoveryDocument}] = await Promise.all([
		import('@app/features/app/state/InstanceSnapshotStore'),
		import('@fluxer/instance_bootstrap/src/Discovery'),
	]);
	await GeoIP.ready();
	expect(GeoIP.countryCode).toBe('SE');
	expect(GeoIP.ageBlockedGeos).toEqual([{countryCode: 'SE', regionCode: 'AB'}]);

	RuntimeConfig.applySnapshot(
		runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(documentFor(second, UK_ONLY))),
	);
	expect(GeoIP.resolution).toBe('pending');
	expect(GeoIP.ageRestrictedGeos).toEqual([{countryCode: 'GB', regionCode: null}]);
	expect(GeoIP.ageBlockedGeos).toEqual([]);
	await GeoIP.ready();
	expect(lookups(second)).toEqual(['GET /api/ip']);
	expect(GeoIP.countryCode).toBe('GB');
	expect(GeoIP.resolution).toBe('resolved');
});

test('installing the first runtime re-seeds and resolves', async () => {
	const instance = await startInstance();
	installRuntime(null);
	const {GeoIP, RuntimeConfig} = await loadGeoIPContext();
	const [{runtimeSnapshotFromDiscovery}, {parseInstanceDiscoveryDocument}] = await Promise.all([
		import('@app/features/app/state/InstanceSnapshotStore'),
		import('@fluxer/instance_bootstrap/src/Discovery'),
	]);
	await GeoIP.ready();
	expect(instance.requests).toEqual([]);
	expect(GeoIP.resolution).toBe('unavailable');

	RuntimeConfig.applySnapshot(
		runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(documentFor(instance, SWEDEN_BLOCKS_STOCKHOLM))),
	);
	await GeoIP.ready();
	expect(lookups(instance)).toEqual(['GET /api/ip']);
	expect(GeoIP.resolution).toBe('resolved');
	expect(GeoIP.countryCode).toBe('SE');
	expect(GeoIP.ageBlockedGeos).toEqual([{countryCode: 'SE', regionCode: 'AB'}]);
});

test('an instance that cannot geolocate never wipes a country the gateway already supplied', async () => {
	const instance = await startInstance();
	instance.body = {...STOCKHOLM, countryCode: null, regionCode: null, latitude: null, longitude: null};
	instance.delayMs = 50;
	installRuntime(documentFor(instance, UK_ONLY));
	const GeoIP = await loadGeoIP();
	GeoIP.applyConnectionFallbackGeo({countryCode: 'GB', regionCode: null, latitude: '51.5', longitude: '-0.1'});
	await GeoIP.ready();
	expect(GeoIP.resolution).toBe('resolved');
	expect(GeoIP.countryCode).toBe('GB');
	expect(GeoIP.latitude).toBe('51.5');
	expect(GeoIP.longitude).toBe('-0.1');
});
