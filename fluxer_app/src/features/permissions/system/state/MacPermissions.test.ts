// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NativePermissionResult, PermissionKind} from '@app/features/permissions/system/utils/NativePermissions';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const host = vi.hoisted(() => ({
	desktop: true,
	platform: 'macos',
	statuses: {} as Record<string, string>,
	requestResults: {} as Record<string, string>,
	requested: [] as Array<string>,
	settingsOpened: [] as Array<string>,
	shortcutReapplies: 0,
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	isDesktop: () => host.desktop,
	getNativePlatformSync: () => host.platform,
	getElectronAPI: () => null,
	isNativeMacOS: () => host.platform === 'macos',
}));
vi.mock('@app/features/permissions/system/utils/NativePermissions', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/permissions/system/utils/NativePermissions')>()),
	checkNativePermission: async (kind: PermissionKind) => host.statuses[kind],
	requestNativePermission: async (kind: PermissionKind) => {
		host.requested.push(kind);
		return host.requestResults[kind];
	},
	openNativePermissionSettings: async (kind: PermissionKind) => {
		host.settingsOpened.push(kind);
	},
}));
vi.mock('@app/features/app/keybindings/KeybindManager', () => ({
	default: {
		reapplyGlobalShortcuts: async () => {
			host.shortcutReapplies += 1;
		},
	},
}));
function setStatuses(status: NativePermissionResult): void {
	host.statuses = {microphone: status, camera: status, screen: status, 'input-monitoring': status};
}

const cleanups: Array<() => void> = [];

async function loadStore() {
	vi.resetModules();
	const addEventListener = vi.spyOn(window, 'addEventListener');
	const module = await import('@app/features/permissions/system/state/MacPermissions');
	const store = module.default;
	await vi.waitFor(() => expect(store.isHydrated).toBe(true));
	await vi.waitFor(() => expect(host.platform !== 'macos' || addEventListener.mock.calls.length > 0).toBe(true));
	for (const [type, listener] of addEventListener.mock.calls) {
		cleanups.push(() => window.removeEventListener(type, listener));
	}
	addEventListener.mockRestore();
	return store;
}

afterEach(() => {
	for (const cleanup of cleanups.splice(0)) cleanup();
});

beforeEach(() => {
	host.desktop = true;
	host.platform = 'macos';
	host.requestResults = {microphone: 'granted', camera: 'denied', screen: 'denied', 'input-monitoring': 'denied'};
	host.requested = [];
	host.settingsOpened = [];
	host.shortcutReapplies = 0;
	setStatuses('not-determined');
});

describe('MacPermissions', () => {
	test('launch reads the four statuses from the system and prompts for nothing', async () => {
		host.statuses = {microphone: 'granted', camera: 'denied', screen: 'not-determined', 'input-monitoring': 'denied'};
		const store = await loadStore();
		expect({...store.statuses}).toEqual(host.statuses);
		expect(store.allGranted).toBe(false);
		expect(store.settledKinds).toEqual(['microphone']);
		expect(host.requested).toEqual([]);
		expect(host.settingsOpened).toEqual([]);
	});

	test('a grant made in System Settings shows up when the window is focused again', async () => {
		setStatuses('denied');
		const store = await loadStore();
		await store.openSettings('input-monitoring');
		expect(host.settingsOpened).toEqual(['input-monitoring']);
		host.statuses['input-monitoring'] = 'granted';
		window.dispatchEvent(new Event('focus'));
		await vi.waitFor(() => expect(store.statuses['input-monitoring']).toBe('granted'));
		expect(host.shortcutReapplies).toBe(1);
		window.dispatchEvent(new Event('focus'));
		await store.refreshAll();
		expect(host.shortcutReapplies).toBe(1);
	});

	test('Input Monitoring that was already granted at launch does not reapply shortcuts', async () => {
		setStatuses('granted');
		const store = await loadStore();
		await store.refreshAll();
		expect(store.allGranted).toBe(true);
		expect(host.shortcutReapplies).toBe(0);
	});

	test('a screen recording grant that still reads as denied after returning suggests a relaunch', async () => {
		setStatuses('granted');
		host.statuses.screen = 'not-determined';
		const store = await loadStore();
		await store.request('screen');
		expect(host.requested).toEqual(['screen']);
		expect(store.statuses.screen).toBe('denied');
		expect(store.screenStillBlockedAfterReturn).toBe(false);
		host.statuses.screen = 'denied';
		window.dispatchEvent(new Event('focus'));
		await vi.waitFor(() => expect(store.screenStillBlockedAfterReturn).toBe(true));
		host.statuses.screen = 'granted';
		window.dispatchEvent(new Event('focus'));
		await vi.waitFor(() => expect(store.statuses.screen).toBe('granted'));
		expect(store.screenStillBlockedAfterReturn).toBe(false);
		expect(store.allGranted).toBe(true);
	});

	test('advice about the macOS quit prompt shows from the first ask until the grant lands', async () => {
		setStatuses('granted');
		host.statuses['input-monitoring'] = 'not-determined';
		host.statuses.microphone = 'not-determined';
		host.requestResults = {'input-monitoring': 'denied', microphone: 'denied'};
		const store = await loadStore();
		expect(store.showsQuitPromptAdvice('input-monitoring')).toBe(false);
		await store.request('input-monitoring');
		await store.request('microphone');
		expect(store.showsQuitPromptAdvice('input-monitoring')).toBe(true);
		expect(store.showsQuitPromptAdvice('microphone')).toBe(false);
		expect(store.anyQuitPromptAdvice).toBe(true);
		host.statuses['input-monitoring'] = 'granted';
		window.dispatchEvent(new Event('focus'));
		await vi.waitFor(() => expect(store.statuses['input-monitoring']).toBe('granted'));
		expect(store.showsQuitPromptAdvice('input-monitoring')).toBe(false);
		expect(store.anyQuitPromptAdvice).toBe(false);
	});

	test('a denied screen recording status alone never suggests a relaunch', async () => {
		setStatuses('denied');
		const store = await loadStore();
		await store.refreshAfterReturn();
		expect(store.screenStillBlockedAfterReturn).toBe(false);
	});

	test('other platforms report every permission as unsupported and never ask the system', async () => {
		host.platform = 'windows';
		host.statuses = {};
		const store = await loadStore();
		expect(Object.values(store.statuses)).toEqual(['unsupported', 'unsupported', 'unsupported', 'unsupported']);
		window.dispatchEvent(new Event('focus'));
		expect(await store.refreshKind('screen')).toBe('unsupported');
	});
});
