// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	detectDomainMigrationInstallKind,
	installDomainMovedApp,
} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import * as core from '@app/features/app/domain_migration/DomainMigrationCore';
import {
	decryptDomainMigrationPayload,
	encryptDomainMigrationPayload,
} from '@app/features/app/domain_migration/DomainMigrationCrypto';
import {runDomainMigrationPreMount} from '@app/features/app/domain_migration/DomainMigrationPreMount';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {StoredAccount} from '@app/features/auth/state/AccountStorage';
import type {DomainMigrationDiscoveryResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/auth/state/AccountStorage', () => ({
	default: {getAllAccounts: async () => []},
}));

const ENABLED_DISCOVERY: DomainMigrationDiscoveryResponse = {
	enabled: true,
	anonymous_rollout_basis_points: 0,
	rollout_salt: 'domain-migration-v1',
	standalone_forwarding: false,
};

const NOW = 1_800_000_000_000;

function memoryStorage(initial: Record<string, string> = {}): core.StorageLike {
	const entries = new Map(Object.entries(initial));
	return {
		getItem: (key) => entries.get(key) ?? null,
		setItem: (key, value) => {
			entries.set(key, value);
		},
		removeItem: (key) => {
			entries.delete(key);
		},
		key: (index) => [...entries.keys()][index] ?? null,
		get length() {
			return entries.size;
		},
	};
}

function environment(
	installKind: core.DomainMigrationInstallKind,
	overrides: Partial<core.DomainMigrationEnvironment> = {},
): core.DomainMigrationEnvironment {
	return {installKind, electron: false, electronMigrationVersion: null, electronPasskeyRpIds: [], ...overrides};
}

function gateInput(overrides: Partial<core.DomainMigrationGateInput> = {}): core.DomainMigrationGateInput {
	return {
		environment: environment('none'),
		assignmentEnabled: true,
		discovery: ENABLED_DISCOVERY,
		marker: null,
		now: NOW,
		voiceActive: false,
		oneShotRoute: false,
		...overrides,
	};
}

describe('sanitizeNextPath', () => {
	it.each([
		['/channels/123/456?x=1#y', '/channels/123/456?x=1#y'],
		['/reset#token=abc', '/reset#token=abc'],
		['/', '/app'],
		['/?a=1', '/app?a=1'],
		['//evil.example', '/channels/@me'],
		['/\\evil.example', '/channels/@me'],
		['/migrate/begin', '/channels/@me'],
		['/migrate', '/channels/@me'],
		['/\t/evil.example', '/channels/@me'],
		['/\n/evil.example', '/channels/@me'],
		['/\r/evil.example', '/channels/@me'],
		['/\tmigrate/done', '/channels/@me'],
		['/channels/\u0000', '/channels/@me'],
		['/a/../migrate/done', '/channels/@me'],
		['/%09/evil.example', '/%09/evil.example'],
		['https://evil.example/', '/channels/@me'],
		['channels/@me', '/channels/@me'],
		[null, '/channels/@me'],
		[42, '/channels/@me'],
	])('maps %j to %j', (input, expected) => {
		expect(core.sanitizeNextPath(input)).toBe(expected);
	});

	it('never leaves the origin it is resolved against', () => {
		for (const input of ['/\t/evil.example', '/\n/evil.example', '/%09/evil.example', '/@evil.example']) {
			expect(new URL(core.sanitizeNextPath(input), 'https://web.fluxer.app/x').origin).toBe('https://web.fluxer.app');
		}
	});
});

describe('resolveDomainMigrationSide', () => {
	it('recognises only the four official origins', () => {
		expect(core.resolveDomainMigrationSide('https://web.fluxer.app')).toEqual({
			role: 'source',
			source: 'https://web.fluxer.app',
			target: 'https://fluxer.com',
		});
		expect(core.resolveDomainMigrationSide('https://canary.fluxer.com')).toEqual({
			role: 'target',
			source: 'https://web.canary.fluxer.app',
			target: 'https://canary.fluxer.com',
		});
		for (const origin of [
			'http://localhost:3000',
			'https://chat.example.com',
			'http://web.fluxer.app',
			'https://fluxer.app',
			'https://web.fluxer.com',
		]) {
			expect(core.resolveDomainMigrationSide(origin)).toBeNull();
		}
	});
});

describe('payload encryption', () => {
	it('round trips through encrypt and decrypt', async () => {
		const payload = {
			version: 1,
			source_origin: 'https://web.fluxer.app',
			local_storage: {token: 'abc', big: 'x'.repeat(200_000)},
		};
		const sealed = await encryptDomainMigrationPayload(payload);
		expect(sealed.payload).toMatch(/^[A-Za-z0-9_-]+$/u);
		expect(sealed.key).toMatch(/^[A-Za-z0-9_-]{43}$/u);
		expect(sealed.payload.length).toBeLessThan(200_000);
		expect(await decryptDomainMigrationPayload(sealed.payload, sealed.key)).toEqual(payload);
	});

	it('rejects a payload opened with another key', async () => {
		const sealed = await encryptDomainMigrationPayload({version: 1});
		const other = await encryptDomainMigrationPayload({version: 1});
		await expect(decryptDomainMigrationPayload(sealed.payload, other.key)).rejects.toThrow();
	});

	it('rejects payloads from another source or version', () => {
		const payload = {
			version: 1,
			source_origin: 'https://web.fluxer.app',
			exported_at: NOW,
			local_storage: {},
			accounts: [],
			notification_permission: 'granted',
		};
		expect(core.parseDomainMigrationPayload(payload, 'https://web.fluxer.app')).not.toBeNull();
		expect(core.parseDomainMigrationPayload(payload, 'https://web.canary.fluxer.app')).toBeNull();
		expect(core.parseDomainMigrationPayload({...payload, version: 2}, 'https://web.fluxer.app')).toBeNull();
		expect(
			core.parseDomainMigrationPayload({...payload, accounts: [{userId: 1}]}, 'https://web.fluxer.app'),
		).toBeNull();
	});

	it('accepts a theme library and rejects a malformed one', () => {
		const payload = {
			version: 1,
			source_origin: 'https://web.fluxer.app',
			exported_at: NOW,
			local_storage: {},
			accounts: [],
			notification_permission: 'default',
		};
		const themeLibrary: core.DomainMigrationThemeLibrary = {
			themes: [{id: 'quick-css', css: 'body{}'}],
			assets: [
				{
					id: 'asset',
					name: 'bg.png',
					mime_type: 'image/png',
					size: 3,
					data: 'AQID',
					created_at: NOW,
					updated_at: NOW,
				},
			],
			local_files: [],
			enabled_theme_ids: ['quick-css'],
		};
		expect(
			core.parseDomainMigrationPayload({...payload, theme_library: themeLibrary}, 'https://web.fluxer.app'),
		).not.toBeNull();
		expect(
			core.parseDomainMigrationPayload(
				{...payload, theme_library: {...themeLibrary, enabled_theme_ids: [1]}},
				'https://web.fluxer.app',
			),
		).toBeNull();
		const trimmed = core.withoutOptionalPayloadData({
			...payload,
			version: 1,
			custom_sounds: [],
			theme_library: themeLibrary,
		});
		expect(trimmed.custom_sounds).toBeUndefined();
		expect(trimmed.theme_library).toEqual({...themeLibrary, assets: []});
	});
});

describe('rewriteImportedAccount', () => {
	it('points the account at the current instance and drops runtimeConfig', () => {
		const current = {apiEndpoint: 'https://fluxer.com/api'} as RuntimeConfigSnapshot;
		const record: StoredAccount = {
			userId: '1',
			token: 'token',
			localStorageData: {runtimeConfig: '{}', token: 'token'},
			managedStorageData: {runtimeConfig: '{}', token: 'token', 'fluxer.theme': 'dark'},
			lastActive: NOW,
			instance: {apiEndpoint: 'https://web.fluxer.app/api'} as RuntimeConfigSnapshot,
		};
		const rewritten = core.rewriteImportedAccount(record, current);
		expect(rewritten.instance).toBe(current);
		expect(rewritten.managedStorageData).toEqual({token: 'token', 'fluxer.theme': 'dark'});
		expect(rewritten.localStorageData).toEqual({token: 'token', 'fluxer.theme': 'dark'});
		expect(rewritten.token).toBe('token');
	});
});

describe('local storage export', () => {
	it('skips push, test, runtime config and migration keys', () => {
		const storage = memoryStorage({
			token: 't',
			'fluxer.lastPushEndpoint': 'https://push',
			'fluxer.pushSubscription': '{}',
			__test__: '1',
			runtimeConfig: '{}',
			[core.DOMAIN_MIGRATION_MARKER_KEY]: '{}',
			[core.DOMAIN_MIGRATION_DEVICE_KEY]: 'device',
			'mobx-persist:Theme': '{}',
		});
		expect(core.collectExportableLocalStorage(storage)).toEqual({token: 't', 'mobx-persist:Theme': '{}'});
	});
});

describe('migration gate', () => {
	it('passes when every condition holds', () => {
		expect(core.shouldStartDomainMigration(gateInput())).toBe(true);
		expect(
			core.shouldStartDomainMigration(
				gateInput({
					environment: environment('none', {
						electron: true,
						electronMigrationVersion: 1,
						electronPasskeyRpIds: ['fluxer.app', 'fluxer.com'],
					}),
				}),
			),
		).toBe(true);
	});

	it.each<[string, Partial<core.DomainMigrationGateInput>]>([
		['assignment off', {assignmentEnabled: false}],
		['kill switch', {discovery: {...ENABLED_DISCOVERY, enabled: false}}],
		['no discovery', {discovery: null}],
		['already completed', {marker: {state: 'completed', target: 'https://fluxer.com', at: NOW}}],
		['failed recently', {marker: {state: 'failed', at: NOW - 60_000, attempts: 1}}],
		['failed too often', {marker: {state: 'failed', at: NOW - 3 * 24 * 60 * 60 * 1000, attempts: 3}}],
		['Android web app', {environment: environment('chromium-android')}],
		['Apple web app', {environment: environment('webkit')}],
		['Firefox web app', {environment: environment('firefox')}],
		['other web app', {environment: environment('other')}],
		['old desktop', {environment: environment('none', {electron: true})}],
		[
			'desktop that cannot create fluxer.com passkeys',
			{
				environment: environment('none', {
					electron: true,
					electronMigrationVersion: 1,
					electronPasskeyRpIds: ['fluxer.app'],
				}),
			},
		],
		['in a voice call', {voiceActive: true}],
		['on a one-shot token route', {oneShotRoute: true}],
	])('blocks when %s', (_label, overrides) => {
		expect(core.shouldStartDomainMigration(gateInput(overrides))).toBe(false);
	});

	it('runs the handoff inside a Chromium desktop web app', () => {
		expect(core.shouldStartDomainMigration(gateInput({environment: environment('chromium-desktop')}))).toBe(true);
	});

	it('retries a failure after a day', () => {
		const marker: core.DomainMigrationMarker = {state: 'failed', at: NOW - 25 * 60 * 60 * 1000, attempts: 2};
		expect(core.shouldStartDomainMigration(gateInput({marker}))).toBe(true);
	});
});

describe('marker state', () => {
	it('counts failures and records completion', () => {
		const storage = memoryStorage();
		expect(core.readDomainMigrationMarker(storage)).toBeNull();
		core.markDomainMigrationFailed(storage, NOW);
		core.markDomainMigrationFailed(storage, NOW + 1);
		expect(core.readDomainMigrationMarker(storage)).toEqual({state: 'failed', at: NOW + 1, attempts: 2});
		core.markDomainMigrationCompleted(storage, 'https://fluxer.com', NOW + 2);
		expect(core.readDomainMigrationMarker(storage)).toEqual({
			state: 'completed',
			target: 'https://fluxer.com',
			at: NOW + 2,
		});
		core.markDomainMigrationFailed(storage, NOW + 3);
		expect(core.readDomainMigrationMarker(storage)).toEqual({state: 'failed', at: NOW + 3, attempts: 1});
	});

	it('ignores malformed markers', () => {
		expect(core.parseDomainMigrationMarker('not json')).toBeNull();
		expect(core.parseDomainMigrationMarker('{"state":"completed","at":1}')).toBeNull();
	});
});

describe('migration intent', () => {
	it('expires and ties completion to the exported handoff', () => {
		const storage = memoryStorage();
		expect(core.readDomainMigrationIntent(storage, NOW)).toBeNull();
		core.writeDomainMigrationIntent(storage, {at: NOW});
		expect(core.readDomainMigrationIntent(storage, NOW + 1000)).toEqual({at: NOW});
		expect(core.readDomainMigrationIntent(storage, NOW + core.DOMAIN_MIGRATION_PENDING_MAX_AGE_MS + 1)).toBeNull();
		expect(core.intentConfirmsCompletion(null, null)).toBe(false);
		expect(core.intentConfirmsCompletion({at: NOW}, null)).toBe(true);
		expect(core.intentConfirmsCompletion({at: NOW, handoff_id: 'abc'}, 'abc')).toBe(true);
		expect(core.intentConfirmsCompletion({at: NOW, handoff_id: 'abc'}, 'xyz')).toBe(false);
		expect(core.intentConfirmsCompletion({at: NOW, handoff_id: 'abc'}, null)).toBe(false);
		core.clearDomainMigrationIntent(storage);
		expect(core.readDomainMigrationIntent(storage, NOW)).toBeNull();
	});
});

describe('forwarding', () => {
	it('keeps the path, query and hash', () => {
		expect(core.buildTargetUrl('https://fluxer.com', '/reset', '?a=1', '#token=abc')).toBe(
			'https://fluxer.com/reset?a=1#token=abc',
		);
		expect(core.buildTargetUrl('https://fluxer.com', '/', '', '')).toBe('https://fluxer.com/app');
	});

	it('honours the kill switch', () => {
		const completed: core.DomainMigrationMarker = {state: 'completed', target: 'https://fluxer.com', at: NOW};
		const browser = environment('none');
		expect(core.shouldForwardCompletedSource(ENABLED_DISCOVERY, completed, browser)).toBe(true);
		expect(core.shouldForwardCompletedSource({...ENABLED_DISCOVERY, enabled: false}, completed, browser)).toBe(false);
		expect(core.shouldForwardCompletedSource(null, completed, browser)).toBe(false);
		expect(core.shouldForwardCompletedSource(ENABLED_DISCOVERY, null, browser)).toBe(false);
	});

	it('forwards installed apps only when standalone forwarding is on', () => {
		const completed: core.DomainMigrationMarker = {state: 'completed', target: 'https://fluxer.com', at: NOW};
		const forwarding = {...ENABLED_DISCOVERY, standalone_forwarding: true};
		const desktop = environment('chromium-desktop');
		expect(core.shouldForwardCompletedSource(ENABLED_DISCOVERY, completed, desktop)).toBe(false);
		expect(core.shouldForwardCompletedSource(forwarding, completed, desktop)).toBe(true);
		const legacyDiscovery = {...ENABLED_DISCOVERY} as Partial<DomainMigrationDiscoveryResponse>;
		delete legacyDiscovery.standalone_forwarding;
		expect(
			core.shouldForwardCompletedSource(legacyDiscovery as DomainMigrationDiscoveryResponse, completed, desktop),
		).toBe(false);
		for (const kind of ['chromium-android', 'webkit', 'firefox', 'other'] as const) {
			expect(core.shouldForwardCompletedSource(forwarding, completed, environment(kind))).toBe(false);
			expect(core.environmentMayForward(environment(kind), forwarding)).toBe(false);
		}
		expect(core.environmentMayForward(environment('none'), ENABLED_DISCOVERY)).toBe(true);
		expect(core.environmentMayForward(environment('none', {electron: true}), ENABLED_DISCOVERY)).toBe(false);
	});

	it('buckets anonymous devices by basis points', () => {
		expect(core.anonymousRolloutIsOpen(ENABLED_DISCOVERY)).toBe(false);
		expect(
			core.anonymousRolloutIsOpen({...ENABLED_DISCOVERY, enabled: false, anonymous_rollout_basis_points: 10000}),
		).toBe(false);
		const all = {...ENABLED_DISCOVERY, anonymous_rollout_basis_points: 10000};
		expect(core.anonymousRolloutIsOpen(all)).toBe(true);
		expect(core.deviceIsInAnonymousRollout(all, 'device-a')).toBe(true);
		const half = {...ENABLED_DISCOVERY, anonymous_rollout_basis_points: 5000};
		for (const deviceId of ['device-a', 'device-b', 'device-c', 'device-d']) {
			expect(core.deviceIsInAnonymousRollout(half, deviceId)).toBe(
				experimentBucket(deviceId, half.rollout_salt) < 5000,
			);
		}
	});
});

const CHROME_DESKTOP_UA =
	'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
const EDGE_DESKTOP_UA = `${CHROME_DESKTOP_UA} Edg/140.0.0.0`;
const CHROME_ANDROID_UA =
	'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Mobile Safari/537.36';
const CHROME_ANDROID_TABLET_UA =
	'Mozilla/5.0 (Linux; Android 14; SM-X910) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
const SAMSUNG_UA =
	'Mozilla/5.0 (Linux; Android 14; SM-S918B) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/27.0 Chrome/125.0.0.0 Mobile Safari/537.36';
const IPHONE_UA =
	'Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
const IPAD_DESKTOP_UA =
	'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15';
const MAC_SAFARI_UA = IPAD_DESKTOP_UA;
const MAC_CHROME_UA =
	'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
const FIREFOX_DESKTOP_UA = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:143.0) Gecko/20100101 Firefox/143.0';
const FIREFOX_ANDROID_UA = 'Mozilla/5.0 (Android 14; Mobile; rv:143.0) Gecko/143.0 Firefox/143.0';
const UNKNOWN_UA = 'SomeBrowser/1.0';
const CHROMIUM_BRANDS = [{brand: 'Chromium'}, {brand: 'Google Chrome'}, {brand: 'Not=A?Brand'}];

function signals(overrides: Partial<core.DomainMigrationInstallSignals>): core.DomainMigrationInstallSignals {
	return {
		displayMode: 'standalone',
		navigatorStandalone: false,
		userAgent: CHROME_DESKTOP_UA,
		userAgentData: null,
		maxTouchPoints: 0,
		electron: false,
		...overrides,
	};
}

describe('classifyDomainMigrationInstallKind', () => {
	it.each<[string, Partial<core.DomainMigrationInstallSignals>, core.DomainMigrationInstallKind]>([
		['a browser tab', {displayMode: 'browser'}, 'none'],
		['a Firefox taskbar tab', {displayMode: 'minimal-ui', userAgent: FIREFOX_DESKTOP_UA}, 'none'],
		['Electron', {electron: true}, 'none'],
		['Safari on iPhone in a tab', {displayMode: 'browser', userAgent: IPHONE_UA}, 'none'],
		['Chrome desktop by brand', {userAgentData: {brands: CHROMIUM_BRANDS, mobile: false}}, 'chromium-desktop'],
		['Chrome desktop by user agent', {}, 'chromium-desktop'],
		['Edge desktop', {userAgent: EDGE_DESKTOP_UA}, 'chromium-desktop'],
		['Chrome on a Mac', {userAgent: MAC_CHROME_UA}, 'chromium-desktop'],
		['window controls overlay', {displayMode: 'window-controls-overlay'}, 'chromium-desktop'],
		[
			'Chrome Android by brand',
			{userAgent: CHROME_ANDROID_UA, userAgentData: {brands: CHROMIUM_BRANDS, mobile: true}},
			'chromium-android',
		],
		['Chrome Android by user agent', {userAgent: CHROME_ANDROID_UA}, 'chromium-android'],
		[
			'Chrome on an Android tablet by brand',
			{
				userAgent: CHROME_ANDROID_TABLET_UA,
				userAgentData: {brands: CHROMIUM_BRANDS, mobile: false, platform: 'Android'},
			},
			'chromium-android',
		],
		['Chrome on an Android tablet by user agent', {userAgent: CHROME_ANDROID_TABLET_UA}, 'chromium-android'],
		['Samsung Internet', {userAgent: SAMSUNG_UA}, 'chromium-android'],
		['an iPhone home screen app', {displayMode: 'browser', navigatorStandalone: true, userAgent: IPHONE_UA}, 'webkit'],
		['an iPad home screen app', {userAgent: IPAD_DESKTOP_UA, maxTouchPoints: 5}, 'webkit'],
		['a Safari Dock app', {userAgent: MAC_SAFARI_UA}, 'webkit'],
		['a Firefox desktop app', {userAgent: FIREFOX_DESKTOP_UA}, 'firefox'],
		['a Firefox Android app', {userAgent: FIREFOX_ANDROID_UA}, 'firefox'],
		['an unknown browser', {userAgent: UNKNOWN_UA}, 'other'],
	])('classifies %s', (_label, overrides, expected) => {
		expect(core.classifyDomainMigrationInstallKind(signals(overrides))).toBe(expected);
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it.each([CHROME_DESKTOP_UA, FIREFOX_DESKTOP_UA, MAC_SAFARI_UA])(
		'treats a full screen browser window as a tab',
		(userAgent) => {
			vi.stubGlobal('matchMedia', (query: string) => ({matches: query === '(display-mode: fullscreen)'}));
			vi.stubGlobal('navigator', {userAgent, maxTouchPoints: 0});
			expect(detectDomainMigrationInstallKind()).toBe('none');
		},
	);

	it('reads an installed app window from the display mode', () => {
		vi.stubGlobal('matchMedia', (query: string) => ({matches: query === '(display-mode: standalone)'}));
		vi.stubGlobal('navigator', {userAgent: CHROME_DESKTOP_UA, maxTouchPoints: 0});
		expect(detectDomainMigrationInstallKind()).toBe('chromium-desktop');
	});
});

describe('shouldShowDomainMovedNotice', () => {
	const source = core.resolveDomainMigrationSide('https://web.fluxer.app');
	const completed: core.DomainMigrationMarker = {state: 'completed', target: 'https://fluxer.com', at: NOW};

	function notice(overrides: Partial<core.DomainMovedNoticeInput>): core.DomainMovedNoticeInput {
		return {
			side: source,
			installKind: 'webkit',
			discovery: ENABLED_DISCOVERY,
			assignmentEnabled: true,
			marker: null,
			dismissedAt: null,
			now: NOW,
			...overrides,
		};
	}

	it('shows in installed apps on the source once the user is in the rollout', () => {
		for (const installKind of ['chromium-android', 'webkit', 'firefox', 'other'] as const) {
			expect(core.shouldShowDomainMovedNotice(notice({installKind}))).toBe(true);
			expect(core.shouldShowDomainMovedNotice(notice({installKind, assignmentEnabled: false}))).toBe(false);
			expect(core.shouldShowDomainMovedNotice(notice({installKind, assignmentEnabled: false, marker: completed}))).toBe(
				true,
			);
		}
	});

	it('waits for the handoff in Chromium desktop apps', () => {
		expect(core.shouldShowDomainMovedNotice(notice({installKind: 'chromium-desktop'}))).toBe(false);
		expect(core.shouldShowDomainMovedNotice(notice({installKind: 'chromium-desktop', marker: completed}))).toBe(true);
	});

	it.each<[string, Partial<core.DomainMovedNoticeInput>]>([
		['a browser tab', {installKind: 'none', marker: completed}],
		['the target', {side: core.resolveDomainMigrationSide('https://fluxer.com')}],
		['a self-hosted origin', {side: core.resolveDomainMigrationSide('https://chat.example.com')}],
		['the kill switch', {discovery: {...ENABLED_DISCOVERY, enabled: false}}],
		['missing discovery', {discovery: null}],
	])('stays hidden for %s', (_label, overrides) => {
		expect(core.shouldShowDomainMovedNotice(notice(overrides))).toBe(false);
	});

	it('comes back seven days after a dismissal', () => {
		expect(core.shouldShowDomainMovedNotice(notice({dismissedAt: NOW - 1000}))).toBe(false);
		expect(
			core.shouldShowDomainMovedNotice(notice({dismissedAt: NOW - core.DOMAIN_MIGRATION_MOVED_DISMISS_MS + 1})),
		).toBe(false);
		expect(core.shouldShowDomainMovedNotice(notice({dismissedAt: NOW - core.DOMAIN_MIGRATION_MOVED_DISMISS_MS}))).toBe(
			true,
		);
	});

	it('builds the new app links from the side target', () => {
		expect(core.domainMovedInstallUrl('https://canary.fluxer.com')).toBe('https://canary.fluxer.com/app');
		expect(core.domainMovedManifestId('https://fluxer.com')).toBe('https://fluxer.com/');
		expect(core.domainMovedBrowserMigrationUrl('https://fluxer.com')).toBe(
			'https://fluxer.com/migrate/begin?start=1&next=%2Fapp',
		);
	});
});

describe('installDomainMovedApp', () => {
	afterEach(() => {
		vi.unstubAllGlobals();
		vi.restoreAllMocks();
	});

	it('opens the new app in the browser straight from the click without the install API', () => {
		const open = vi.spyOn(window, 'open').mockReturnValue(null);
		const onUnavailable = vi.fn();
		vi.stubGlobal('navigator', {userAgent: CHROME_DESKTOP_UA});
		installDomainMovedApp('https://fluxer.com', onUnavailable);
		expect(open).toHaveBeenCalledWith('https://fluxer.com/app', '_blank', 'noopener');
		expect(onUnavailable).not.toHaveBeenCalled();
	});

	it('shows the fallback instead of a late popup when the install fails', async () => {
		const open = vi.spyOn(window, 'open').mockReturnValue(null);
		const onUnavailable = vi.fn();
		const install = vi.fn().mockRejectedValue(new DOMException('denied', 'NotAllowedError'));
		vi.stubGlobal('navigator', {userAgent: CHROME_DESKTOP_UA, install});
		installDomainMovedApp('https://fluxer.com', onUnavailable);
		await vi.waitFor(() => expect(onUnavailable).toHaveBeenCalledOnce());
		expect(install).toHaveBeenCalledWith('https://fluxer.com/app', 'https://fluxer.com/');
		expect(open).not.toHaveBeenCalled();
	});

	it('does nothing more when the user cancels the install', async () => {
		const onUnavailable = vi.fn();
		const install = vi.fn().mockRejectedValue(new DOMException('cancelled', 'AbortError'));
		vi.stubGlobal('navigator', {userAgent: CHROME_DESKTOP_UA, install});
		installDomainMovedApp('https://fluxer.com', onUnavailable);
		await Promise.resolve();
		await Promise.resolve();
		expect(onUnavailable).not.toHaveBeenCalled();
	});
});

describe('runDomainMigrationPreMount', () => {
	let replace: ReturnType<typeof vi.fn>;

	function visit(url: string, discovery: DomainMigrationDiscoveryResponse | undefined): void {
		const parsed = new URL(url);
		replace = vi.fn();
		vi.stubGlobal('location', {
			origin: parsed.origin,
			pathname: parsed.pathname,
			search: parsed.search,
			hash: parsed.hash,
			replace,
		});
		(window as unknown as Record<string, unknown>).__FLUXER_BOOTSTRAP__ = {instance: {domain_migration: discovery}};
	}

	function readMarker(): core.DomainMigrationMarker | null {
		return core.parseDomainMigrationMarker(window.localStorage.getItem(core.DOMAIN_MIGRATION_MARKER_KEY));
	}

	function writeIntent(intent: core.DomainMigrationIntent): void {
		window.sessionStorage.setItem(core.DOMAIN_MIGRATION_INTENT_KEY, JSON.stringify(intent));
	}

	function writeCompletedMarker(): void {
		window.localStorage.setItem(
			core.DOMAIN_MIGRATION_MARKER_KEY,
			JSON.stringify({state: 'completed', target: 'https://fluxer.com', at: NOW}),
		);
	}

	beforeEach(() => {
		window.localStorage.clear();
		window.sessionStorage.clear();
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('does nothing on a self-hosted origin', async () => {
		visit('https://chat.example.com/migrate/done?next=/channels/@me', ENABLED_DISCOVERY);
		writeCompletedMarker();
		expect(await runDomainMigrationPreMount()).toBe(false);
		expect(replace).not.toHaveBeenCalled();
	});

	it('forwards a migrated source tab with its hash', async () => {
		visit('https://web.fluxer.app/reset#token=abc', ENABLED_DISCOVERY);
		writeCompletedMarker();
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/reset#token=abc');
	});

	it('stays put when the kill switch is off', async () => {
		visit('https://web.fluxer.app/reset#token=abc', {...ENABLED_DISCOVERY, enabled: false});
		writeCompletedMarker();
		expect(await runDomainMigrationPreMount()).toBe(false);
		expect(replace).not.toHaveBeenCalled();
	});

	it('marks the source completed when the intent matches the handoff', async () => {
		writeIntent({at: Date.now(), handoff_id: 'handoff'});
		visit('https://web.fluxer.app/migrate/done?h=handoff&next=%2Fchannels%2F1%2F2', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/channels/1/2');
		expect(readMarker()).toMatchObject({state: 'completed', target: 'https://fluxer.com'});
		expect(window.sessionStorage.getItem(core.DOMAIN_MIGRATION_INTENT_KEY)).toBeNull();
	});

	it('ignores a done link without a matching intent', async () => {
		visit('https://web.fluxer.app/migrate/done?next=%2Fchannels%2F1%2F2', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/1/2');
		expect(readMarker()).toBeNull();

		writeIntent({at: Date.now(), handoff_id: 'handoff'});
		visit('https://web.fluxer.app/migrate/done?h=other&next=%2Fchannels%2F1%2F2', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/1/2');
		expect(readMarker()).toBeNull();
	});

	it('ignores migrate links while the kill switch is off', async () => {
		writeIntent({at: Date.now()});
		visit('https://web.fluxer.app/migrate/done?next=%2Fchannels%2F1%2F2', {...ENABLED_DISCOVERY, enabled: false});
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/1/2');
		expect(readMarker()).toBeNull();
	});

	it('refuses to export without an intent from the source trigger', async () => {
		const fetchSpy = vi.fn();
		vi.stubGlobal('fetch', fetchSpy);
		visit(`https://web.fluxer.app/migrate/export?n=${'a'.repeat(43)}`, ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/@me');
		expect(fetchSpy).not.toHaveBeenCalled();
	});

	it('leaves the marker alone on a failed link and refuses an open redirect', async () => {
		visit('https://web.fluxer.app/migrate/failed?reason=nonce_mismatch&next=%2F%09%2Fevil.example', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/@me');
		expect(readMarker()).toBeNull();
	});

	it('reopens a completed source when a resumed migration fails', async () => {
		writeCompletedMarker();
		writeIntent({at: Date.now()});
		visit('https://web.fluxer.app/migrate/failed?reason=redeem_failed&next=%2Fchannels%2F%40me', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/@me');
		expect(readMarker()).toMatchObject({state: 'failed', attempts: 1});
	});

	it('resumes through the target when a migrated source still holds a session', async () => {
		writeCompletedMarker();
		window.localStorage.setItem('token', 'session-token');
		visit('https://web.fluxer.app/reset#token=abc', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith(
			`https://fluxer.com/migrate/begin?resume=1&next=${encodeURIComponent('/reset#token=abc')}`,
		);
		expect(core.readDomainMigrationIntent(window.sessionStorage, Date.now())).not.toBeNull();
	});

	function installApp(userAgent: string): void {
		vi.stubGlobal('matchMedia', (query: string) => ({matches: query === '(display-mode: standalone)'}));
		vi.stubGlobal('navigator', {userAgent, maxTouchPoints: 0});
	}

	it('returns a Chromium desktop app to the source after the handoff', async () => {
		installApp(CHROME_DESKTOP_UA);
		writeIntent({at: Date.now(), handoff_id: 'handoff'});
		visit('https://web.fluxer.app/migrate/done?h=handoff&next=%2Fchannels%2F1%2F2', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/1/2');
		expect(readMarker()).toMatchObject({state: 'completed', target: 'https://fluxer.com'});
	});

	it('forwards a Chromium desktop app when standalone forwarding is on', async () => {
		installApp(CHROME_DESKTOP_UA);
		writeIntent({at: Date.now(), handoff_id: 'handoff'});
		visit('https://web.fluxer.app/migrate/done?h=handoff&next=%2Fchannels%2F1%2F2', {
			...ENABLED_DISCOVERY,
			standalone_forwarding: true,
		});
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/channels/1/2');
	});

	it('keeps a migrated Chromium desktop app on the source', async () => {
		installApp(CHROME_DESKTOP_UA);
		writeCompletedMarker();
		visit('https://web.fluxer.app/channels/1/2', {...ENABLED_DISCOVERY, anonymous_rollout_basis_points: 10000});
		expect(await runDomainMigrationPreMount()).toBe(false);
		expect(replace).not.toHaveBeenCalled();

		visit('https://web.fluxer.app/channels/1/2', {...ENABLED_DISCOVERY, standalone_forwarding: true});
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/channels/1/2');
	});

	it('does not forward logged-out installed apps in the anonymous rollout', async () => {
		installApp(CHROME_DESKTOP_UA);
		visit('https://web.fluxer.app/login', {...ENABLED_DISCOVERY, anonymous_rollout_basis_points: 10000});
		expect(await runDomainMigrationPreMount()).toBe(false);
		expect(replace).not.toHaveBeenCalled();
	});

	it('never runs the handoff or forwards in an Apple web app', async () => {
		installApp(IPHONE_UA);
		writeCompletedMarker();
		visit('https://web.fluxer.app/channels/1/2', {...ENABLED_DISCOVERY, standalone_forwarding: true});
		expect(await runDomainMigrationPreMount()).toBe(false);
		expect(replace).not.toHaveBeenCalled();

		window.localStorage.clear();
		writeIntent({at: Date.now(), handoff_id: 'handoff'});
		visit('https://web.fluxer.app/migrate/done?h=handoff&next=%2Fchannels%2F1%2F2', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/1/2');
		expect(readMarker()).toBeNull();
	});

	it('never runs the handoff in an Android web app', async () => {
		installApp(CHROME_ANDROID_UA);
		writeIntent({at: Date.now()});
		visit('https://web.fluxer.app/migrate/done?next=%2Fchannels%2F1%2F2', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/channels/1/2');
		expect(readMarker()).toBeNull();
	});

	it('starts a browser migration opened from an installed Android app on the source', async () => {
		visit('https://fluxer.com/migrate/begin?start=1&next=%2Fapp', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/migrate/start?next=%2Fapp');
		expect(window.sessionStorage.getItem(core.DOMAIN_MIGRATION_PENDING_KEY)).toBeNull();

		visit('https://web.fluxer.app/migrate/start?next=%2Fapp', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/migrate/begin?next=%2Fapp');
		expect(core.readDomainMigrationIntent(window.sessionStorage, Date.now())).toMatchObject({at: expect.any(Number)});

		visit('https://fluxer.com/migrate/begin?next=%2Fapp', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace.mock.calls[0]?.[0]).toMatch(/^https:\/\/web\.fluxer\.app\/migrate\/export\?n=[\w-]+$/u);
	});

	it('opens the target directly when the browser already migrated', async () => {
		window.localStorage.setItem('token', 'session-token');
		visit('https://fluxer.com/migrate/begin?start=1&next=%2Fapp', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/app');
	});

	it('does not start a migration inside an installed Android app', async () => {
		installApp(CHROME_ANDROID_UA);
		visit('https://web.fluxer.app/migrate/start?next=%2Fapp', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/app');
		expect(core.readDomainMigrationIntent(window.sessionStorage, Date.now())).toBeNull();
	});

	it('sends a target completion without a pending nonce back as failed', async () => {
		visit('https://fluxer.com/migrate/complete#h=abc&k=def', ENABLED_DISCOVERY);
		vi.stubGlobal('history', {state: null, replaceState: vi.fn()});
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith(
			'https://web.fluxer.app/migrate/failed?reason=no_pending&next=%2Fchannels%2F%40me',
		);
	});

	it('reports back to the source when the target already holds a session', async () => {
		window.localStorage.setItem('token', 'session-token');
		visit('https://fluxer.com/migrate/begin?next=%2Fchannels%2F1', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://web.fluxer.app/migrate/done?next=%2Fchannels%2F1');

		visit('https://fluxer.com/migrate/begin?resume=1&next=%2Fchannels%2F1', ENABLED_DISCOVERY);
		expect(await runDomainMigrationPreMount()).toBe(true);
		expect(replace).toHaveBeenCalledWith('https://fluxer.com/channels/1');
	});
});
