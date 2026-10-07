// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import InstanceSnapshotStore, {
	type RuntimeConfigSnapshot,
	resolveDiscoveryApiEndpoint,
	runtimeConfigSnapshotsAreSameInstance,
	runtimeInstanceKey,
	runtimeSnapshotFromDiscovery,
	storedInstanceKey,
} from '@app/features/app/state/InstanceSnapshotStore';
import {requireRuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfigSnapshot';
import {BOOTSTRAP_APP_PUBLIC} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import {
	type InstanceDiscoveryDocument,
	InstanceDiscoveryUnreachableError,
	parseInstanceDiscoveryDocument,
} from '@fluxer/instance_bootstrap/src/Discovery';
import {
	OFFICIAL_CLIENT_API_ENDPOINTS,
	type OfficialReleaseChannel,
} from '@fluxer/instance_bootstrap/src/OfficialInstance';
import type {InstanceDiscoveryResponse} from '@fluxer/instance_bootstrap/src/Types';
import {computeWireFormat} from '@fluxer/limits/src/LimitDiffer';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const EMPTY_LIMITS: LimitConfigSnapshot = {version: 1, traitDefinitions: [], rules: []};

function discoveryDocument(
	host: string,
	overrides: Partial<InstanceDiscoveryResponse> = {},
): InstanceDiscoveryResponse {
	return {
		api_code_version: 1,
		endpoints: {
			api: `https://${host}/api`,
			api_client: `https://${host}/api`,
			api_public: `https://${host}/api`,
			gateway: `wss://gateway.${host}`,
			media: `https://media.${host}`,
			static_cdn: `https://cdn.${host}`,
			marketing: `https://${host}`,
			admin: `https://admin.${host}`,
			invite: `https://${host}/invite`,
			gift: `https://${host}/gift`,
			webapp: `https://app.${host}`,
			upload_relay: `https://upload.${host}`,
		},
		captcha: {provider: 'none'},
		features: {
			voice_enabled: true,
			stripe_enabled: false,
			self_hosted: true,
			presigned_attachment_uploads: false,
			emails_enabled: false,
			premium_enabled: false,
			stripe_serviceable: false,
			phone_verification_enabled: false,
		},
		gif: {provider: 'tenor', display_name: 'Tenor', attribution_required: true},
		sso: {enabled: false, enforced: false, display_name: null, redirect_uri: `https://${host}/sso`},
		registration: {mode: 'closed', admin_registration_urls_enabled: false},
		community: {
			single_community: false,
			single_community_guild_id: null,
			direct_messages_disabled: false,
			guild_create_access: true,
		},
		services: {gif_enabled: true, youtube_enabled: true, bluesky_enabled: false},
		limits: EMPTY_LIMITS,
		push: {public_vapid_key: 'vapid'},
		app_public: {
			...BOOTSTRAP_APP_PUBLIC,
			branding: {...BOOTSTRAP_APP_PUBLIC.branding, product_name: `Fluxer ${host}`},
		},
		...overrides,
	};
}

function parsedDocument(host: string): InstanceDiscoveryDocument {
	return parseInstanceDiscoveryDocument(discoveryDocument(host));
}

function jsonResponse(body: unknown): Response {
	return new Response(JSON.stringify(body), {status: 200, headers: {'content-type': 'application/json'}});
}

afterEach(() => {
	vi.unstubAllGlobals();
});

function runningChannel(): OfficialReleaseChannel {
	return Config.PUBLIC_RELEASE_CHANNEL === 'stable' ? 'stable' : 'canary';
}

function stubBrowserDocument(origin: string, {servesOwnApi = false}: {servesOwnApi?: boolean} = {}): void {
	const url = new URL(origin);
	vi.stubGlobal('window', {
		location: {origin: url.origin === 'null' ? origin : url.origin, protocol: url.protocol, hostname: url.hostname},
	});
	vi.stubGlobal('document', {
		querySelector: (selector: string) =>
			servesOwnApi && selector === 'meta[name="fluxer-api-origin"][content="self"]' ? {} : null,
	});
}

describe('runtimeInstanceKey', () => {
	function snapshotWithEndpoint(apiEndpoint: string): RuntimeConfigSnapshot {
		return {...runtimeSnapshotFromDiscovery(parsedDocument('example.test')), apiEndpoint};
	}

	test.each([
		['https://x.com/api', 'https://x.com/api'],
		['HTTPS://X.com/api/', 'https://x.com/api'],
		['http://[::1]:8080/api', 'http://[::1]:8080/api'],
	])('%s derives %s', (endpoint, expected) => {
		expect(runtimeInstanceKey(snapshotWithEndpoint(endpoint))).toBe(expected);
	});

	test.each(['https://x.com/api?q=1', 'https://x.com/api#f', 'https://u:p@x.com/api', 'ftp://x.com', '', '/api'])(
		'%s has no instance key',
		(endpoint) => {
			expect(runtimeInstanceKey(snapshotWithEndpoint(endpoint))).toBeNull();
		},
	);

	test('same-instance comparison uses the derived key', () => {
		const left = snapshotWithEndpoint('HTTPS://X.com/api/');
		const right = snapshotWithEndpoint('https://x.com/api');
		expect(runtimeConfigSnapshotsAreSameInstance(left, right)).toBe(true);
		expect(runtimeConfigSnapshotsAreSameInstance(undefined, right)).toBe(false);
		expect(runtimeConfigSnapshotsAreSameInstance(snapshotWithEndpoint('https://y.com/api'), right)).toBe(false);
	});
});

describe('runtime limit snapshots', () => {
	const snapshot: LimitConfigSnapshot = {
		version: 1,
		traitDefinitions: ['premium'],
		rules: [{id: 'premium', filters: {traits: ['premium']}, limits: {max_guilds: 250}}],
	};

	test('accepts the deployed expanded format unchanged', () => {
		const document = discoveryDocument('limits-expanded.test', {limits: snapshot});
		expect(runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(document)).limits).toEqual(snapshot);
	});

	test('expands the version 2 wire format', () => {
		const wire = computeWireFormat(snapshot);
		expect(wire.version).toBe(2);
		expect(wire.defaultsHash).toBeTruthy();
		const document = discoveryDocument('limits-wire.test', {limits: wire});
		expect(
			runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(document)).limits.rules[0]?.limits.max_guilds,
		).toBe(250);
	});

	test('rejects a missing or partial config', () => {
		const missing = discoveryDocument('limits-missing.test', {limits: undefined});
		const partial = discoveryDocument('limits-partial.test', {limits: {version: 1} as LimitConfigSnapshot});
		expect(() => runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(missing))).toThrow(/limits/u);
		expect(() => runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(partial))).toThrow(/limits/u);
	});

	test('never aliases the caller its input', () => {
		const document = discoveryDocument('limits-clone.test', {limits: snapshot});
		const read = runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(document)).limits;
		read.rules[0]!.limits.max_guilds = 1;
		expect(snapshot.rules[0]?.limits.max_guilds).toBe(250);
	});
});

describe('runtimeSnapshotFromDiscovery', () => {
	test('carries every advertised field through', () => {
		const snapshot = runtimeSnapshotFromDiscovery(parsedDocument('a.test'));
		expect(snapshot.apiEndpoint).toBe('https://a.test/api');
		expect(snapshot.gatewayEndpoint).toBe('wss://gateway.a.test');
		expect(snapshot.mediaEndpoint).toBe('https://media.a.test');
		expect(snapshot.gifProvider).toBe('tenor');
		expect(snapshot.gifAttributionRequired).toBe(true);
		expect(snapshot.publicPushVapidKey).toBe('vapid');
		expect(snapshot.registration?.mode).toBe('closed');
		expect(snapshot.appPublic.branding.product_name).toBe('Fluxer a.test');
	});

	test('rejects a document that omits required runtime blocks', () => {
		const document = discoveryDocument('b.test');
		expect(() =>
			runtimeSnapshotFromDiscovery(
				parseInstanceDiscoveryDocument({
					endpoints: document.endpoints,
					features: {self_hosted: true},
				}),
			),
		).toThrow(/required by the client runtime/u);
	});

	test('rejects a missing client API endpoint', () => {
		const document = discoveryDocument('c.test');
		document.endpoints.api_client = '';
		expect(() => runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(document))).toThrow(
			/endpoints\.api_client/u,
		);
	});

	test('keeps a cache-busted branding URL the discovery document advertises', () => {
		const document = discoveryDocument('d.test');
		document.app_public = {
			...BOOTSTRAP_APP_PUBLIC,
			branding: {...BOOTSTRAP_APP_PUBLIC.branding, icon_url: 'https://cdn.d.test/icon.png?v=3'},
			legal: {terms_url: 'https://d.test/legal?doc=terms', privacy_url: null},
		};
		const snapshot = runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(document));
		expect(snapshot.appPublic.branding.icon_url).toBe('https://cdn.d.test/icon.png?v=3');
		expect(snapshot.appPublic.legal.terms_url).toBe('https://d.test/legal?doc=terms');
	});

	test('rejects a branding URL that is not an absolute HTTP URL', () => {
		const snapshot = runtimeSnapshotFromDiscovery(parsedDocument('e.test'));
		expect(() =>
			requireRuntimeConfigSnapshot({
				...snapshot,
				appPublic: {
					...snapshot.appPublic,
					branding: {...snapshot.appPublic.branding, icon_url: 'javascript:alert(1)'},
				},
			}),
		).toThrow(/appPublic\.branding\.icon_url/u);
	});

	test('carries the advertised domain migration', () => {
		const domainMigration = {
			enabled: true,
			anonymous_rollout_basis_points: 2500,
			rollout_salt: 'domain-migration-v1',
			standalone_forwarding: true,
		};
		const snapshot = runtimeSnapshotFromDiscovery(
			parseInstanceDiscoveryDocument(discoveryDocument('m.test', {domain_migration: domainMigration})),
		);
		expect(snapshot.domainMigration).toEqual(domainMigration);
		expect(requireRuntimeConfigSnapshot(JSON.parse(JSON.stringify(snapshot))).domainMigration).toEqual(domainMigration);
	});

	test('treats an absent or malformed domain migration as none', () => {
		expect(runtimeSnapshotFromDiscovery(parsedDocument('n.test')).domainMigration).toBeNull();
		const malformed = {
			...discoveryDocument('o.test'),
			domain_migration: {
				enabled: true,
				anonymous_rollout_basis_points: 10_001,
				rollout_salt: 'salt',
				standalone_forwarding: false,
			},
		};
		expect(runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(malformed)).domainMigration).toBeNull();
	});

	test('loads a cached snapshot written before domain migration was carried', () => {
		const {domainMigration: _omitted, ...older} = runtimeSnapshotFromDiscovery(parsedDocument('p.test'));
		expect(requireRuntimeConfigSnapshot(older).domainMigration).toBeNull();
	});

	test('rejects an endpoint the instance advertises in an unusable form', () => {
		const document = discoveryDocument('c.test');
		document.endpoints.media = 'not a url';
		expect(() => runtimeSnapshotFromDiscovery(parseInstanceDiscoveryDocument(document))).toThrow(/endpoints\.media/u);
	});
});

describe('resolveDiscoveryApiEndpoint', () => {
	test.each([
		['example.test', 'https://example.test'],
		['https://example.test/api/', 'https://example.test/api'],
		['http://localhost:8080', 'http://localhost:8080'],
	])('%s normalises to %s', (input, expected) => {
		expect(resolveDiscoveryApiEndpoint(input)).toBe(expected);
	});

	test('maps an official display host onto the client API endpoint for the running channel outside a browser page', () => {
		const expected = OFFICIAL_CLIENT_API_ENDPOINTS[runningChannel()];
		expect(resolveDiscoveryApiEndpoint('fluxer.app')).toBe(expected);
		expect(resolveDiscoveryApiEndpoint('https://web.fluxer.app')).toBe(expected);
	});

	test.each(['', '/api', 'https://user:pw@example.test', 'ws://example.test'])('rejects %s', (input) => {
		expect(() => resolveDiscoveryApiEndpoint(input)).toThrow();
	});

	test.each([
		['https://web.fluxer.app', 'https://web.fluxer.app/api'],
		['https://web.canary.fluxer.app', 'https://web.canary.fluxer.app/api'],
		['https://fluxer.com', 'https://fluxer.com/api'],
		['https://canary.fluxer.com', 'https://canary.fluxer.com/api'],
	])('an official page at %s resolves the official instance through its own %s', (origin, expected) => {
		stubBrowserDocument(origin);
		for (const input of [origin, 'fluxer.app', 'https://web.fluxer.app', 'api.fluxer.app', 'fluxer.com']) {
			expect(resolveDiscoveryApiEndpoint(input)).toBe(expected);
		}
	});

	test('a page on another origin reaches the official instance through its client API endpoint', () => {
		stubBrowserDocument('https://chat.example.test');
		expect(resolveDiscoveryApiEndpoint('fluxer.app')).toBe(OFFICIAL_CLIENT_API_ENDPOINTS[runningChannel()]);
		expect(resolveDiscoveryApiEndpoint('https://chat.example.test')).toBe('https://chat.example.test');
		expect(resolveDiscoveryApiEndpoint('other.example.test')).toBe('https://other.example.test');
	});

	test('a page that serves its own API resolves its origin onto that API', () => {
		stubBrowserDocument('https://chat.example.test', {servesOwnApi: true});
		expect(resolveDiscoveryApiEndpoint('https://chat.example.test')).toBe('https://chat.example.test/api');
		expect(resolveDiscoveryApiEndpoint('https://chat.example.test/v2')).toBe('https://chat.example.test/v2');
		expect(resolveDiscoveryApiEndpoint('other.example.test')).toBe('https://other.example.test');
	});

	test('the desktop local app resolves the official instance through the endpoint its discovery declares', () => {
		stubBrowserDocument('fluxer-app://app', {servesOwnApi: true});
		const expected = OFFICIAL_CLIENT_API_ENDPOINTS[runningChannel()];
		expect(resolveDiscoveryApiEndpoint('https://web.fluxer.app')).toBe(expected);
		expect(resolveDiscoveryApiEndpoint('https://fluxer.com')).toBe(expected);
	});

	test.each([
		['https://fluxer.com/api', OFFICIAL_CLIENT_API_ENDPOINTS.stable],
		['https://canary.fluxer.com/api', OFFICIAL_CLIENT_API_ENDPOINTS.canary],
		['https://canary.fluxer.com/api/', OFFICIAL_CLIENT_API_ENDPOINTS.canary],
		['https://web.fluxer.app/api', OFFICIAL_CLIENT_API_ENDPOINTS.stable],
		['https://web.canary.fluxer.app/api', OFFICIAL_CLIENT_API_ENDPOINTS.canary],
	])('the desktop local app reaches the account stored at %s through %s', (stored, expected) => {
		stubBrowserDocument('fluxer-app://app', {servesOwnApi: true});
		expect(resolveDiscoveryApiEndpoint(stored)).toBe(expected);
		expect(storedInstanceKey(stored)).toBe(expected);
	});

	test('a page on another origin reaches an account stored on the migrated official domain through the official instance', () => {
		stubBrowserDocument('https://chat.example.test', {servesOwnApi: true});
		expect(resolveDiscoveryApiEndpoint('https://fluxer.com/api')).toBe(OFFICIAL_CLIENT_API_ENDPOINTS.stable);
		expect(storedInstanceKey('https://canary.fluxer.com/api')).toBe(OFFICIAL_CLIENT_API_ENDPOINTS.canary);
		expect(storedInstanceKey('https://chat.example.test/api')).toBe('https://chat.example.test/api');
		expect(storedInstanceKey('https://other.example.test/api/')).toBe('https://other.example.test/api');
	});

	test.each([
		'https://fluxer.com',
		'https://canary.fluxer.com',
		'https://web.fluxer.app',
		'https://web.canary.fluxer.app',
	])('the official page at %s keeps the account it stored on its own same-origin API', (origin) => {
		stubBrowserDocument(origin, {servesOwnApi: true});
		expect(resolveDiscoveryApiEndpoint(`${origin}/api`)).toBe(`${origin}/api`);
		expect(storedInstanceKey(`${origin}/api`)).toBe(`${origin}/api`);
	});

	test('an endpoint that names no instance has no stored instance key', () => {
		expect(storedInstanceKey('')).toBeNull();
		expect(storedInstanceKey('https://user:pw@canary.fluxer.com/api')).toBeNull();
	});
});

describe('InstanceSnapshotStore.resolve on an official web page', () => {
	function productionDocument(webOrigin: string): InstanceDiscoveryResponse {
		const document = discoveryDocument(new URL(webOrigin).host);
		return {
			...document,
			endpoints: {
				...document.endpoints,
				api: `${webOrigin}/api`,
				api_client: `${webOrigin}/api`,
				api_public: 'https://api.example.test',
				gateway: 'wss://gateway.example.test',
				webapp: webOrigin,
			},
		};
	}

	function serveLikeProduction(canonicalWebOrigin: string): ReturnType<typeof vi.fn> {
		return vi.fn(async (url: string) => {
			if (url.endsWith('.fluxer.app/.well-known/fluxer')) {
				return new Response('<!doctype html><html></html>', {
					status: 200,
					headers: {'content-type': 'text/html; charset=utf-8'},
				});
			}
			return jsonResponse(productionDocument(canonicalWebOrigin));
		});
	}

	test('the legacy web origin discovers the instance on its own origin and keeps its API same-origin', async () => {
		const canonical = 'https://web.canary.fluxer.app';
		stubBrowserDocument(canonical);
		const fetchMock = serveLikeProduction(canonical);
		vi.stubGlobal('fetch', fetchMock);
		const resolution = await InstanceSnapshotStore.resolve({input: canonical, signal: null});
		expect(fetchMock.mock.calls.map((call) => call[0])).toEqual([
			`${canonical}/.well-known/fluxer`,
			`${canonical}/api/.well-known/fluxer`,
		]);
		expect(resolution.snapshot.apiEndpoint).toBe(`${canonical}/api`);
		expect(resolution.instanceKey).toBe(`${canonical}/api`);
	});

	test('a same-origin host takes over the client API and web app endpoints, as the app-proxy rewrite did', async () => {
		stubBrowserDocument('https://canary.fluxer.com', {servesOwnApi: true});
		const fetchMock = serveLikeProduction('https://web.canary.fluxer.app');
		vi.stubGlobal('fetch', fetchMock);
		const resolution = await InstanceSnapshotStore.resolve({input: 'https://canary.fluxer.com', signal: null});
		expect(fetchMock.mock.calls[0]?.[0]).toBe('https://canary.fluxer.com/.well-known/fluxer');
		expect(resolution.snapshot.apiEndpoint).toBe('https://canary.fluxer.com/api');
		expect(resolution.snapshot.webAppEndpoint).toBe('https://canary.fluxer.com');
		expect(resolution.snapshot.apiPublicEndpoint).toBe('https://api.example.test');
		expect(resolution.snapshot.gatewayEndpoint).toBe('wss://gateway.example.test');
		expect(resolution.instanceKey).toBe('https://canary.fluxer.com/api');
	});

	test('a self-hosted alias host that serves its own API adopts its origin', async () => {
		stubBrowserDocument('https://alias.example.test', {servesOwnApi: true});
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(discoveryDocument('canonical.example.test'))),
		);
		const resolution = await InstanceSnapshotStore.resolve({input: 'https://alias.example.test', signal: null});
		expect(resolution.snapshot.apiEndpoint).toBe('https://alias.example.test/api');
		expect(resolution.snapshot.webAppEndpoint).toBe('https://alias.example.test');
	});

	test('a host that is not marked as serving its own API still refuses a foreign API declaration', async () => {
		stubBrowserDocument('https://fluxer.com');
		vi.stubGlobal('fetch', serveLikeProduction('https://web.fluxer.app'));
		await expect(InstanceSnapshotStore.resolve({input: 'https://fluxer.com', signal: null})).rejects.toThrow(
			InstanceDiscoveryUnreachableError,
		);
	});

	test('a page that serves its own API never adopts the origin of another instance', async () => {
		stubBrowserDocument('https://alias-two.example.test', {servesOwnApi: true});
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(discoveryDocument('elsewhere.example.test'))),
		);
		await expect(InstanceSnapshotStore.resolve({input: 'https://foreign.example.test', signal: null})).rejects.toThrow(
			InstanceDiscoveryUnreachableError,
		);
	});

	test('a web user on the migrated domain restores the account stored on its same-origin API', async () => {
		stubBrowserDocument('https://fluxer.com', {servesOwnApi: true});
		const fetchMock = serveLikeProduction('https://web.fluxer.app');
		vi.stubGlobal('fetch', fetchMock);
		const resolution = await InstanceSnapshotStore.resolve({input: 'https://fluxer.com/api', signal: null});
		expect(fetchMock.mock.calls[0]?.[0]).toBe('https://fluxer.com/.well-known/fluxer');
		expect(resolution.snapshot.apiEndpoint).toBe('https://fluxer.com/api');
		expect(resolution.snapshot.webAppEndpoint).toBe('https://fluxer.com');
		expect(resolution.instanceKey).toBe('https://fluxer.com/api');
		expect(resolution.instanceKey).toBe(storedInstanceKey('https://fluxer.com/api'));
	});
});

describe('InstanceSnapshotStore.resolve', () => {
	test('resolves on the first candidate that answers with JSON', async () => {
		const fetchMock = vi.fn(async () => jsonResponse(discoveryDocument('one.test')));
		vi.stubGlobal('fetch', fetchMock);
		const resolution = await InstanceSnapshotStore.resolve({input: 'one.test', signal: null});
		expect(fetchMock).toHaveBeenCalledTimes(1);
		expect(resolution.instanceKey).toBe('https://one.test/api');
		expect(resolution.productName).toBe('Fluxer one.test');
		expect(resolution.snapshot.gatewayEndpoint).toBe('wss://gateway.one.test');
		expect(InstanceSnapshotStore.get('https://one.test/api')).toBe(resolution.snapshot);
	});

	test('skips an SPA HTML body without parsing it and falls through to /api', async () => {
		const fetchMock = vi.fn(async (url: string) => {
			if (url === 'https://two.test/.well-known/fluxer') {
				return new Response('<!doctype html><html></html>', {
					status: 200,
					headers: {'content-type': 'text/html; charset=utf-8'},
				});
			}
			return jsonResponse(discoveryDocument('two.test'));
		});
		vi.stubGlobal('fetch', fetchMock);
		const resolution = await InstanceSnapshotStore.resolve({input: 'https://two.test', signal: null});
		expect(fetchMock.mock.calls.map((call) => call[0])).toEqual([
			'https://two.test/.well-known/fluxer',
			'https://two.test/api/.well-known/fluxer',
		]);
		expect(resolution.instanceKey).toBe('https://two.test/api');
	});

	test('sends only a safelisted Accept header cross-origin so no CORS preflight is triggered', async () => {
		const requests: Array<RequestInit> = [];
		vi.stubGlobal(
			'fetch',
			vi.fn(async (_url: string, init: RequestInit) => {
				requests.push(init);
				return jsonResponse(discoveryDocument('three.test'));
			}),
		);
		await InstanceSnapshotStore.resolve({input: 'three.test', signal: null});
		const init = requests[0];
		expect(init?.headers).toEqual({Accept: 'application/json'});
		expect(init?.credentials).toBe('omit');
		expect(init?.redirect).toBe('error');
		expect(init?.cache).toBe('no-store');
		expect(init?.referrerPolicy).toBe('no-referrer');
	});

	test('treats 304 as a not-modified success when the document is cached', async () => {
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(discoveryDocument('four.test'))),
		);
		const first = await InstanceSnapshotStore.resolve({input: 'four.test', signal: null});
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => new Response(null, {status: 304})),
		);
		const second = await InstanceSnapshotStore.resolve({input: 'four.test', signal: null});
		expect(second.snapshot).toBe(first.snapshot);
		expect(second.instanceKey).toBe('https://four.test/api');
	});

	test('reports every candidate when nothing answers', async () => {
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => new Response('', {status: 404})),
		);
		await expect(InstanceSnapshotStore.resolve({input: 'five.test', signal: null})).rejects.toThrow(
			/five\.test\/\.well-known\/fluxer.*five\.test\/api\/\.well-known\/fluxer/su,
		);
	});

	test('rejects a document whose advertised API endpoint is not an http(s) endpoint', async () => {
		const document = discoveryDocument('six.test');
		document.endpoints.api_client = 'ftp://six.test';
		document.endpoints.api = 'ftp://six.test';
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(document)),
		);
		await expect(InstanceSnapshotStore.resolve({input: 'six.test', signal: null})).rejects.toThrow(
			InstanceDiscoveryUnreachableError,
		);
	});

	test('deduplicates concurrent resolutions of the same endpoint', async () => {
		const fetchMock = vi.fn(async () => jsonResponse(discoveryDocument('seven.test')));
		vi.stubGlobal('fetch', fetchMock);
		const [left, right] = await Promise.all([
			InstanceSnapshotStore.resolve({input: 'seven.test', signal: null}),
			InstanceSnapshotStore.resolve({input: 'seven.test', signal: null}),
		]);
		expect(fetchMock).toHaveBeenCalledTimes(1);
		expect(left.snapshot).toBe(right.snapshot);
	});

	test('honours an already-aborted signal', async () => {
		const fetchMock = vi.fn(async () => jsonResponse(discoveryDocument('eight.test')));
		vi.stubGlobal('fetch', fetchMock);
		await expect(InstanceSnapshotStore.resolve({input: 'eight.test', signal: AbortSignal.abort()})).rejects.toThrow();
		expect(fetchMock).not.toHaveBeenCalled();
	});
});

describe('InstanceSnapshotStore limit lookup', () => {
	test('a resolved snapshot answers by its API hostname and a foreign domain stays null', async () => {
		const limits: LimitConfigSnapshot = {
			version: 1,
			traitDefinitions: [],
			rules: [{id: 'base', limits: {max_guilds: 42}}],
		};
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(discoveryDocument('nine.test', {limits}))),
		);
		await InstanceSnapshotStore.resolve({input: 'nine.test', signal: null});
		expect(InstanceSnapshotStore.getLimitsForInstance('nine.test')).toEqual(limits);
		expect(InstanceSnapshotStore.getLimitsForInstance('NINE.TEST')).toEqual(limits);
		expect(InstanceSnapshotStore.getLimitsForInstance('unheard-of.test')).toBeNull();
		expect(InstanceSnapshotStore.getLimitsForInstance('')).toBeNull();
	});

	test('a failed discovery never caches a snapshot with no instance key', async () => {
		const document = discoveryDocument('ten.test');
		document.endpoints.api_client = '/api';
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(document)),
		);
		await expect(InstanceSnapshotStore.resolve({input: 'ten.test', signal: null})).rejects.toThrow();
		expect(InstanceSnapshotStore.getLimitsForInstance('ten.test')).toBeNull();
	});
});

describe('InstanceSnapshotStore.resolve in the desktop local app', () => {
	beforeEach(() => {
		runInAction(() => InstanceSnapshotStore.entries.clear());
	});

	function officialDocument(clientOrigin: string): InstanceDiscoveryResponse {
		const document = discoveryDocument(new URL(clientOrigin).host);
		return {
			...document,
			endpoints: {
				...document.endpoints,
				api: `${clientOrigin}/api`,
				api_client: `${clientOrigin}/api`,
				webapp: clientOrigin,
			},
		};
	}

	function stubDesktopRuntime(served: ReadonlyMap<string, InstanceDiscoveryResponse>): Array<string> {
		const inputs: Array<string> = [];
		stubBrowserDocument('fluxer-app://app', {servesOwnApi: true});
		vi.stubGlobal('window', {
			...window,
			electron: {
				desktopRuntimeConfig: {
					resolve: async ({input}: {input: string}) => {
						inputs.push(input);
						const document = served.get(input);
						if (document === undefined) {
							throw new Error(`No usable instance discovery document was served for ${input}`);
						}
						return {
							instanceKey: document.endpoints.api_client,
							apiEndpoint: `fluxer-app://app/api/${encodeURIComponent(input)}`,
							remoteApiEndpoint: document.endpoints.api_client,
							document,
						};
					},
					cancelResolution: async () => undefined,
				},
			},
		});
		return inputs;
	}

	test.each([
		['https://canary.fluxer.com', 'https://web.canary.fluxer.app'],
		['https://fluxer.com', 'https://web.fluxer.app'],
	])('an account stored on %s resolves and is keyed as the official instance at %s', async (migrated, official) => {
		const inputs = stubDesktopRuntime(new Map([[`${official}/api`, officialDocument(official)]]));

		const resolution = await InstanceSnapshotStore.resolve({input: `${migrated}/api`, signal: null});

		expect(inputs).toEqual([`${official}/api`]);
		expect(resolution.instanceKey).toBe(`${official}/api`);
		expect(resolution.snapshot.apiEndpoint).toBe(`${official}/api`);
		expect(resolution.snapshot.webAppEndpoint).toBe(official);
		expect(resolution.instanceKey).toBe(storedInstanceKey(`${migrated}/api`));
		const again = await InstanceSnapshotStore.resolve({input: `${official}/api`, signal: null});
		expect(again.snapshot).toBe(resolution.snapshot);
		expect(inputs).toHaveLength(1);
	});
});
