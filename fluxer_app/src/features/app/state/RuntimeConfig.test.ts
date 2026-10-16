// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import DesktopRuntimeTransactions from '@app/features/app/state/DesktopRuntimeTransaction';
import InstanceSnapshotStore, {
	type RuntimeConfigSnapshot,
	runtimeSnapshotFromDiscovery,
} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig, {describeAPIEndpoint} from '@app/features/app/state/RuntimeConfig';
import {BOOTSTRAP_APP_PUBLIC} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import {http} from '@app/features/platform/transport/RestTransport';
import {type InstanceDiscoveryDocument, parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';
import type {InstanceDiscoveryResponse} from '@fluxer/instance_bootstrap/src/Types';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {MessageDescriptor} from '@lingui/core';
import {autorun, configure} from 'mobx';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const desktopRuntime = vi.hoisted(() => ({required: false}));

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));
vi.mock('@app/features/app/state/DesktopRuntimeTransaction', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/features/app/state/DesktopRuntimeTransaction')>();
	return {...actual, requiresDesktopRuntimeTransaction: () => desktopRuntime.required};
});

const EMPTY_LIMITS: LimitConfigSnapshot = {version: 1, traitDefinitions: [], rules: []};

function discoveryDocument(host: string): InstanceDiscoveryResponse {
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
			voice_enabled: false,
			stripe_enabled: false,
			self_hosted: true,
			presigned_attachment_uploads: false,
			emails_enabled: false,
			premium_enabled: false,
			stripe_serviceable: false,
			phone_verification_enabled: false,
		},
		gif: {provider: 'klipy', display_name: 'Klipy', attribution_required: false},
		sso: {enabled: false, enforced: false, display_name: null, redirect_uri: `https://${host}/sso`},
		registration: {mode: 'open', admin_registration_urls_enabled: true},
		community: {
			single_community: false,
			single_community_guild_id: null,
			direct_messages_disabled: false,
			guild_create_access: true,
		},
		services: {gif_enabled: true, youtube_enabled: false, bluesky_enabled: false},
		limits: EMPTY_LIMITS,
		push: {public_vapid_key: null},
		app_public: {
			...BOOTSTRAP_APP_PUBLIC,
			branding: {...BOOTSTRAP_APP_PUBLIC.branding, product_name: `Fluxer ${host}`},
		},
	};
}

function parsedDocument(host: string): InstanceDiscoveryDocument {
	return parseInstanceDiscoveryDocument(discoveryDocument(host));
}

const bootSnapshot: RuntimeConfigSnapshot = runtimeSnapshotFromDiscovery(parsedDocument('boot.test'));
RuntimeConfig.applySnapshot(bootSnapshot);

beforeEach(() => {
	RuntimeConfig.applySnapshot(bootSnapshot);
});

afterEach(() => {
	vi.unstubAllGlobals();
	vi.restoreAllMocks();
	delete (window as {electron?: unknown}).electron;
});

describe('boot', () => {
	test('applies the initial runtime snapshot', () => {
		expect(RuntimeConfig.apiEndpoint).toBe('https://boot.test/api');
		expect(RuntimeConfig.productName).toBe('Fluxer boot.test');
	});

	test('keeps the active runtime out of the discovery cache', () => {
		expect(InstanceSnapshotStore.get('https://boot.test/api')).toBeNull();
		expect(InstanceSnapshotStore.getLimitsForInstance('boot.test')).toBeNull();
	});
});

describe('applySnapshot', () => {
	test("swaps every endpoint to the second instance's", () => {
		RuntimeConfig.applySnapshot(runtimeSnapshotFromDiscovery(parsedDocument('second.test')));
		expect(RuntimeConfig.apiEndpoint).toBe('https://second.test/api');
		expect(RuntimeConfig.gatewayEndpoint).toBe('wss://gateway.second.test');
		expect(RuntimeConfig.mediaEndpoint).toBe('https://media.second.test');
		expect(RuntimeConfig.productName).toBe('Fluxer second.test');
		expect(RuntimeConfig.isSelfHosted()).toBe(true);
	});

	test('leaves REST routing to the runtime effects owner', () => {
		const configure = vi.spyOn(http, 'configure');
		RuntimeConfig.applySnapshot(runtimeSnapshotFromDiscovery(parsedDocument('rest.test')));
		expect(configure).not.toHaveBeenCalled();
	});

	test('round-trips its own snapshot without losing a field', () => {
		RuntimeConfig.applySnapshot(runtimeSnapshotFromDiscovery(parsedDocument('round.test')));
		const snapshot = RuntimeConfig.getSnapshot();
		RuntimeConfig.applySnapshot(snapshot);
		expect(RuntimeConfig.getSnapshot()).toEqual(snapshot);
	});

	test('rejects a record that predates required runtime blocks', () => {
		const legacy = {
			...runtimeSnapshotFromDiscovery(parsedDocument('legacy.test')),
			registration: undefined,
			community: undefined,
			services: undefined,
			mediaEndpoint: undefined,
		} as unknown as RuntimeConfigSnapshot;
		expect(() => RuntimeConfig.applySnapshot(legacy)).toThrow(/mediaEndpoint/u);
		expect(RuntimeConfig.apiEndpoint).toBe('https://boot.test/api');
	});

	test('refuses a snapshot with no instance key and leaves the active instance intact', () => {
		const broken = {...runtimeSnapshotFromDiscovery(parsedDocument('broken.test')), apiEndpoint: '/api'};
		expect(() => RuntimeConfig.applySnapshot(broken)).toThrow(/apiEndpoint must be an absolute network endpoint/u);
		expect(RuntimeConfig.apiEndpoint).toBe('https://boot.test/api');
	});
});

describe('applySnapshotAndWaitForDesktop', () => {
	test('applies immediately when no desktop shell exposes the runtime bridge', async () => {
		await RuntimeConfig.applySnapshotAndWaitForDesktop({
			snapshot: runtimeSnapshotFromDiscovery(parsedDocument('web.test')),
			signal: null,
		});
		expect(RuntimeConfig.apiEndpoint).toBe('https://web.test/api');
	});

	test('does not invoke the desktop bridge outside the local app origin', async () => {
		const prepare = vi.fn();
		(window as {electron?: unknown}).electron = {
			desktopRuntimeConfig: {prepare},
		};
		await RuntimeConfig.applySnapshotAndWaitForDesktop({
			snapshot: runtimeSnapshotFromDiscovery(parsedDocument('shell.test')),
			signal: null,
		});
		expect(prepare).not.toHaveBeenCalled();
		expect(RuntimeConfig.apiEndpoint).toBe('https://shell.test/api');
	});

	test('honours an aborted signal without touching the active instance', async () => {
		await expect(
			RuntimeConfig.applySnapshotAndWaitForDesktop({
				snapshot: runtimeSnapshotFromDiscovery(parsedDocument('aborted.test')),
				signal: AbortSignal.abort(),
			}),
		).rejects.toThrow();
		expect(RuntimeConfig.apiEndpoint).toBe('https://boot.test/api');
	});
});

describe('deactivate', () => {
	afterEach(() => {
		configure({disableErrorBoundaries: false});
		desktopRuntime.required = false;
	});

	function observeInstanceReads(): {reads: Array<string>; dispose: () => void} {
		const reads: Array<string> = [];
		const dispose = autorun(() => {
			reads.push(
				`${RuntimeConfig.singleCommunityEnabled}:${RuntimeConfig.features.self_hosted}:${RuntimeConfig.getSnapshot().apiEndpoint}`,
			);
		});
		return {reads, dispose};
	}

	test('keeps the page instance active on the web', async () => {
		configure({disableErrorBoundaries: true});
		const {reads, dispose} = observeInstanceReads();

		await RuntimeConfig.deactivate();
		dispose();

		expect(RuntimeConfig.getSnapshotOrNull()?.apiEndpoint).toBe('https://boot.test/api');
		expect(RuntimeConfig.transportApiEndpoint).toBe('https://boot.test/api');
		expect(reads).toEqual(['false:true:https://boot.test/api']);
	});

	test('releases the desktop runtime while mounted readers keep the last instance', async () => {
		const nativeDeactivate = vi.spyOn(DesktopRuntimeTransactions, 'deactivate').mockResolvedValue(undefined);
		desktopRuntime.required = true;
		configure({disableErrorBoundaries: true});
		const {reads, dispose} = observeInstanceReads();

		await RuntimeConfig.deactivate();
		dispose();

		expect(nativeDeactivate).toHaveBeenCalledWith('https://boot.test/api');
		expect(RuntimeConfig.getSnapshotOrNull()).toBeNull();
		expect(() => RuntimeConfig.transportApiEndpoint).toThrow(/No instance runtime is active/u);
		expect(RuntimeConfig.productName).toBe('Fluxer boot.test');
		expect(reads.at(-1)).toBe('false:true:https://boot.test/api');
	});

	test('a desktop runtime activated after a release replaces the retained instance', async () => {
		vi.spyOn(DesktopRuntimeTransactions, 'deactivate').mockResolvedValue(undefined);
		desktopRuntime.required = true;
		await RuntimeConfig.deactivate();
		desktopRuntime.required = false;

		RuntimeConfig.applySnapshot(runtimeSnapshotFromDiscovery(parsedDocument('next.test')));

		expect(RuntimeConfig.getSnapshotOrNull()?.apiEndpoint).toBe('https://next.test/api');
		expect(RuntimeConfig.apiEndpoint).toBe('https://next.test/api');
	});
});

describe('resolveEndpoint', () => {
	test('resolves a foreign instance through the snapshot store', async () => {
		vi.stubGlobal(
			'fetch',
			vi.fn(
				async () =>
					new Response(JSON.stringify(discoveryDocument('resolved.test')), {
						status: 200,
						headers: {'content-type': 'application/json'},
					}),
			),
		);
		const resolution = await RuntimeConfig.resolveEndpoint({input: 'resolved.test', signal: null});
		expect(resolution.instanceKey).toBe('https://resolved.test/api');
		expect(resolution.productName).toBe('Fluxer resolved.test');
		expect(RuntimeConfig.apiEndpoint).toBe('https://boot.test/api');
	});
});

describe('resolveAndPrepareSnapshot', () => {
	function jsonResponse(body: unknown): Response {
		return new Response(JSON.stringify(body), {status: 200, headers: {'content-type': 'application/json'}});
	}

	test('prepares the stored snapshot when instance discovery is unreachable', async () => {
		const stored = runtimeSnapshotFromDiscovery(parsedDocument('outage.test'));
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => new Response('upstream unavailable', {status: 503})),
		);
		const prepared = await RuntimeConfig.resolveAndPrepareSnapshot({snapshot: stored, signal: null});
		expect(prepared.snapshot).toEqual(stored);
		await RuntimeConfig.abortPreparedSnapshot(prepared);
		expect(RuntimeConfig.apiEndpoint).toBe('https://boot.test/api');
	});

	test('prepares the stored snapshot when the network refuses every discovery request', async () => {
		const stored = runtimeSnapshotFromDiscovery(parsedDocument('offline.test'));
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => {
				throw new TypeError('Failed to fetch');
			}),
		);
		const prepared = await RuntimeConfig.resolveAndPrepareSnapshot({snapshot: stored, signal: null});
		expect(prepared.snapshot).toEqual(stored);
		await RuntimeConfig.abortPreparedSnapshot(prepared);
	});

	test('prefers the freshly discovered snapshot when discovery answers', async () => {
		const stored = runtimeSnapshotFromDiscovery(parsedDocument('fresh.test'));
		const document = discoveryDocument('fresh.test');
		document.endpoints.gateway = 'wss://moved.fresh.test';
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse(document)),
		);
		const prepared = await RuntimeConfig.resolveAndPrepareSnapshot({snapshot: stored, signal: null});
		expect(prepared.snapshot.gatewayEndpoint).toBe('wss://moved.fresh.test');
		await RuntimeConfig.abortPreparedSnapshot(prepared);
	});

	test('never falls back when the instance now requires a newer client', async () => {
		const stored = runtimeSnapshotFromDiscovery(parsedDocument('upgraded.test'));
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => jsonResponse({...discoveryDocument('upgraded.test'), codename: 'future'})),
		);
		await expect(RuntimeConfig.resolveAndPrepareSnapshot({snapshot: stored, signal: null})).rejects.toThrow(/future/u);
	});

	test('honours an aborted signal instead of falling back', async () => {
		const stored = runtimeSnapshotFromDiscovery(parsedDocument('cancelled.test'));
		vi.stubGlobal(
			'fetch',
			vi.fn(async () => new Response('', {status: 503})),
		);
		await expect(
			RuntimeConfig.resolveAndPrepareSnapshot({snapshot: stored, signal: AbortSignal.abort()}),
		).rejects.toThrow();
	});
});

describe('describeAPIEndpoint', () => {
	test.each([
		['https://api.fluxer.app', 'fluxer.app'],
		['https://web.fluxer.app', 'fluxer.app'],
		['https://api.canary.fluxer.app', 'fluxer.app'],
		['https://self.example/api', 'self.example/api'],
		['https://self.example/api/', 'self.example/api'],
		['self.example', 'self.example'],
		['http://localhost:8080/api', 'http://localhost:8080/api'],
		['https://fluxer.app:8443/api', 'fluxer.app:8443/api'],
		['/api', '/api'],
		['not a url at all', 'not a url at all'],
	])('%s displays as %s', (endpoint, expected) => {
		expect(describeAPIEndpoint(endpoint)).toBe(expected);
	});
});
