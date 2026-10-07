// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AppStorageBroadcastMessage,
	AppStorageBroadcastOptions,
} from '@app/features/platform/state/AppStorageBroadcast';
import {AppStorageKey, LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY} from '@app/features/platform/state/AppStorageKeys';
import type {AppStorageScopeChangeEvent} from '@app/features/platform/state/PersistentStorage';
import {
	APP_STORAGE_INDEXED_DB_NAME,
	type AppStorageStamp,
	type AppStorageWrite,
	type PersistentStorageBackend,
	PersistentStorageBackendKind,
} from '@app/features/platform/state/PersistentStorageBackend';
import type {
	PersistentStorageWriteQueueOptions,
	PersistentStorageWriteRequest,
} from '@app/features/platform/state/PersistentStorageWriteQueue';
import {getProtectedIndexedDB, getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

type PersistentStorageModule = typeof import('@app/features/platform/state/PersistentStorage');
type AppStorageBroadcastModule = typeof import('@app/features/platform/state/AppStorageBroadcast');
type WriteQueueModule = typeof import('@app/features/platform/state/PersistentStorageWriteQueue');
type BackendModule = typeof import('@app/features/platform/state/PersistentStorageBackend');

interface LoadedModules {
	readonly storage: PersistentStorageModule;
	readonly broadcast: AppStorageBroadcastModule;
	readonly queue: WriteQueueModule;
	readonly backend: PersistentStorageBackend;
}

const ACCOUNT_A = 'https://one.example/api::100';
const ACCOUNT_B = 'https://two.example/api::200';
const GLOBAL_KEY = AppStorageKey.THEME;
const ACCOUNT_KEY = 'UserSettings:syncedPreferencesLocal';
const UNMAPPED_KEY = 'fluxer:ui:legacy-pane-size';
const CONTENT_KEY = 'Drafts';
const LEGACY_ACCOUNT_KEY = 'Notification';
const LEGACY_GLOBAL_KEY = 'VoiceSettings';
const SHARED_PANE_KEY = 'compact_voice_call_heights';

function rawStorage(): Storage {
	const storage = getProtectedLocalStorage();
	if (storage == null) {
		throw new Error('localStorage is unavailable in the test environment');
	}
	return storage;
}

function deleteAppStorageDatabase(): Promise<void> {
	return new Promise<void>((resolve, reject) => {
		const factory = getProtectedIndexedDB();
		if (factory == null) {
			resolve();
			return;
		}
		const request = factory.deleteDatabase(APP_STORAGE_INDEXED_DB_NAME);
		request.onsuccess = () => resolve();
		request.onerror = () => reject(request.error ?? new Error('deleteDatabase failed'));
	});
}

async function loadModules(): Promise<LoadedModules> {
	vi.resetModules();
	const backendModule: BackendModule = await import('@app/features/platform/state/PersistentStorageBackend');
	const broadcast: AppStorageBroadcastModule = await import('@app/features/platform/state/AppStorageBroadcast');
	const queue: WriteQueueModule = await import('@app/features/platform/state/PersistentStorageWriteQueue');
	const storage: PersistentStorageModule = await import('@app/features/platform/state/PersistentStorage');
	return {storage, broadcast, queue, backend: backendModule.getPersistentStorageBackend()};
}

async function loadScopedStorage(): Promise<LoadedModules> {
	const modules = await loadModules();
	await modules.storage.initializeAppStorage({scoped: true});
	return modules;
}

async function waitFor(condition: () => boolean, label: string): Promise<void> {
	for (let attempt = 0; attempt < 200; attempt++) {
		if (condition()) {
			return;
		}
		await new Promise((resolve) => setTimeout(resolve, 5));
	}
	throw new Error(`timed out waiting for ${label}`);
}

function deferred<T>(): {promise: Promise<T>; resolve: (value: T) => void} {
	let resolve: (value: T) => void = () => undefined;
	const promise = new Promise<T>((settle) => {
		resolve = settle;
	});
	return {promise, resolve};
}

beforeEach(() => {
	Object.defineProperty(globalThis.navigator, 'locks', {
		value: {request: <T>(_name: string, callback: () => Promise<T>): Promise<T> => callback()},
		configurable: true,
	});
});

afterEach(async () => {
	vi.restoreAllMocks();
	Object.defineProperty(globalThis.navigator, 'locks', {value: null, configurable: true});
	rawStorage().clear();
	await deleteAppStorageDatabase();
});

describe('before initialization', () => {
	test('reads and writes go to raw localStorage, which is what AppLogger does at module init', async () => {
		const modules = await loadModules();
		rawStorage().setItem('debugLoggingEnabled', 'true');
		expect(modules.storage.default.getItem('debugLoggingEnabled')).toBe('true');
		modules.storage.default.setItem(GLOBAL_KEY, 'pre-boot');
		expect(rawStorage().getItem(GLOBAL_KEY)).toBe('pre-boot');
		await expect(modules.storage.flushAppStorageWrites()).resolves.toBeUndefined();
		expect(modules.storage.getAppStorageScope()).toBe(modules.storage.UNAUTHENTICATED_APP_STORAGE_SCOPE);
	});

	test('a value written before the scoped backend engages is still served afterwards', async () => {
		const modules = await loadModules();
		modules.storage.default.setItem(GLOBAL_KEY, 'pre-boot');
		await modules.storage.initializeAppStorage({scoped: true});
		expect(modules.storage.default.getItem(GLOBAL_KEY)).toBe('pre-boot');
		await modules.storage.flushAppStorageWrites();
		expect((await modules.backend.load(modules.storage.GLOBAL_APP_STORAGE_SCOPE)).get(GLOBAL_KEY)?.value).toBe(
			'pre-boot',
		);
	});
});

describe('legacy localStorage behaviour is unchanged while the scoped backend is disabled', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadModules();
		await modules.storage.initializeAppStorage({scoped: false});
	});

	test('reads and writes go straight through to raw localStorage', () => {
		const AppStorage = modules.storage.default;
		AppStorage.setItem(ACCOUNT_KEY, 'value');
		expect(rawStorage().getItem(ACCOUNT_KEY)).toBe('value');
		rawStorage().setItem('WrittenOutsideAppStorage', 'raw');
		expect(AppStorage.getItem('WrittenOutsideAppStorage')).toBe('raw');
		expect(AppStorage.keys()).toContain('WrittenOutsideAppStorage');
		expect(AppStorage.length).toBe(rawStorage().length);
		expect(AppStorage.key(0)).toBe(rawStorage().key(0));
		AppStorage.removeItem(ACCOUNT_KEY);
		expect(rawStorage().getItem(ACCOUNT_KEY)).toBeNull();
	});

	test('resetAppStorage and resetActiveAccountScope keep the deployed preserve-only-drafts semantics', async () => {
		const AppStorage = modules.storage.default;
		AppStorage.setItem(CONTENT_KEY, 'draft');
		AppStorage.setItem(GLOBAL_KEY, 'theme');
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);
		expect(AppStorage.getItem(CONTENT_KEY)).toBe('draft');
		expect(AppStorage.getItem(GLOBAL_KEY)).toBeNull();
		await modules.storage.resetAppStorage([]);
		expect(AppStorage.keys()).toHaveLength(0);
	});

	test('deleteAppStorageScope falls back to the deployed logout wipe', async () => {
		const AppStorage = modules.storage.default;
		AppStorage.setItem(GLOBAL_KEY, 'theme');
		await modules.storage.deleteAppStorageScope(ACCOUNT_A);
		expect(AppStorage.getItem(GLOBAL_KEY)).toBeNull();
	});

	test('scope activation records the scope without moving any value', async () => {
		const AppStorage = modules.storage.default;
		AppStorage.setItem(ACCOUNT_KEY, 'value');
		expect(modules.storage.getAppStorageScope()).toBe(modules.storage.UNAUTHENTICATED_APP_STORAGE_SCOPE);
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		expect(modules.storage.getAppStorageScope()).toBe(ACCOUNT_A);
		expect(AppStorage.getItem(ACCOUNT_KEY)).toBe('value');
		expect(modules.storage.mutationsBlocked()).toBe(false);
	});

	test('another tab writing localStorage still arrives as an external change', () => {
		const AppStorage = modules.storage.default;
		const observed: Array<string | null> = [];
		const unsubscribe = AppStorage.subscribe((event) => observed.push(event.key), {source: 'external'});
		window.dispatchEvent(
			new StorageEvent('storage', {
				key: GLOBAL_KEY,
				oldValue: null,
				newValue: 'from-another-tab',
				storageArea: rawStorage(),
			}),
		);
		unsubscribe();
		expect(observed).toContain(GLOBAL_KEY);
	});
});

describe('scoped storage', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadScopedStorage();
	});

	test('an account scope hides the other account while the global scope stays shared', async () => {
		const AppStorage = modules.storage.default;
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		AppStorage.setItem(GLOBAL_KEY, 'shared-theme');
		AppStorage.setItem(ACCOUNT_KEY, 'a-value');
		await modules.storage.flushAppStorageWrites();

		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(AppStorage.getItem(GLOBAL_KEY)).toBe('shared-theme');
		expect(AppStorage.getItem(ACCOUNT_KEY)).toBeNull();
		AppStorage.setItem(ACCOUNT_KEY, 'b-value');
		await modules.storage.flushAppStorageWrites();

		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		expect(AppStorage.getItem(ACCOUNT_KEY)).toBe('a-value');
		expect(rawStorage().getItem(ACCOUNT_KEY)).toBeNull();

		expect((await modules.backend.load(ACCOUNT_A)).get(ACCOUNT_KEY)?.value).toBe('a-value');
		expect((await modules.backend.load(ACCOUNT_B)).get(ACCOUNT_KEY)?.value).toBe('b-value');
		expect((await modules.backend.load(modules.storage.GLOBAL_APP_STORAGE_SCOPE)).get(GLOBAL_KEY)?.value).toBe(
			'shared-theme',
		);
	});

	test('the keybind store is shared by every account, so a new account keeps push-to-talk', async () => {
		const AppStorage = modules.storage.default;
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		AppStorage.setItem('Keybind', '{"transmitMode":"push_to_talk"}');
		await modules.storage.flushAppStorageWrites();

		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(AppStorage.getItem('Keybind')).toBe('{"transmitMode":"push_to_talk"}');
		expect((await modules.backend.load(ACCOUNT_A)).has('Keybind')).toBe(false);
	});

	test('a write left in flight when the scope changes lands in the scope that made it', async () => {
		const AppStorage = modules.storage.default;
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		AppStorage.setItem(ACCOUNT_KEY, 'a-value');
		await modules.storage.activateAppStorageScope(ACCOUNT_B);

		expect((await modules.backend.load(ACCOUNT_A)).get(ACCOUNT_KEY)?.value).toBe('a-value');
		expect((await modules.backend.load(ACCOUNT_B)).has(ACCOUNT_KEY)).toBe(false);
		expect(AppStorage.getItem(ACCOUNT_KEY)).toBeNull();
	});

	test('mutations are blocked while the cache is being swapped', async () => {
		const AppStorage = modules.storage.default;
		const gate = deferred<void>();
		const load = modules.backend.load.bind(modules.backend);
		vi.spyOn(modules.backend, 'load').mockImplementation(async (scope) => {
			await gate.promise;
			return load(scope);
		});
		const activation = modules.storage.activateAppStorageScope(ACCOUNT_A);
		await waitFor(() => modules.storage.mutationsBlocked(), 'the cache swap to block mutations');
		expect(() => AppStorage.setItem(ACCOUNT_KEY, 'blocked')).toThrow(/lifecycle transition/);
		gate.resolve();
		await activation;
		expect(modules.storage.mutationsBlocked()).toBe(false);
	});

	test('a scope change notifies the before and after listeners and republishes every key', async () => {
		const AppStorage = modules.storage.default;
		const order: Array<string> = [];
		const beforeEvents: Array<AppStorageScopeChangeEvent> = [];
		const stopBefore = modules.storage.onBeforeAppStorageScopeChange((event) => {
			beforeEvents.push(event);
			order.push('before');
		});
		const stopAfter = modules.storage.subscribeAppStorageScope(() => {
			order.push('after');
		});
		const republished: Array<string | null> = [];
		const stopSubscription = AppStorage.subscribe((event) => republished.push(event.key), {
			key: ACCOUNT_KEY,
			source: 'external',
		});

		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		stopBefore();
		stopAfter();
		stopSubscription();

		expect(order).toEqual(['before', 'after']);
		expect(beforeEvents).toEqual([
			{previousScope: modules.storage.UNAUTHENTICATED_APP_STORAGE_SCOPE, nextScope: ACCOUNT_A},
		]);
		expect(republished).toEqual([null]);
	});

	test('a throwing scope listener never fails the activation', async () => {
		const stop = modules.storage.subscribeAppStorageScope(() => {
			throw new Error('listener exploded');
		});
		await expect(modules.storage.activateAppStorageScope(ACCOUNT_A)).resolves.toBeUndefined();
		stop();
		expect(modules.storage.getAppStorageScope()).toBe(ACCOUNT_A);
	});

	test('a legacy mobx store name registered as global stays shared across accounts', async () => {
		const AppStorage = modules.storage.default;
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		AppStorage.setItem('Theme', '{"type":"dark"}');
		await modules.storage.flushAppStorageWrites();
		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(AppStorage.getItem('Theme')).toBe('{"type":"dark"}');
		expect((await modules.backend.load(modules.storage.GLOBAL_APP_STORAGE_SCOPE)).get('Theme')?.value).toBe(
			'{"type":"dark"}',
		);
		expect((await modules.backend.load(ACCOUNT_A)).has('Theme')).toBe(false);
	});

	test('a legacy mobx store name registered as account scope is not shared', async () => {
		const AppStorage = modules.storage.default;
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		AppStorage.setItem('SelectedGuild', '{"guildId":"1"}');
		await modules.storage.flushAppStorageWrites();
		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(AppStorage.getItem('SelectedGuild')).toBeNull();
	});

	test('a failing backend never throws out of the lifecycle and the legacy corpus keeps serving reads', async () => {
		rawStorage().setItem(UNMAPPED_KEY, 'legacy-value');
		vi.spyOn(modules.backend, 'load').mockRejectedValue(new Error('backend unavailable'));
		await expect(modules.storage.activateAppStorageScope(ACCOUNT_A)).resolves.toBeUndefined();
		expect(modules.storage.default.getItem(UNMAPPED_KEY)).toBe('legacy-value');
	});
});

describe('read-through to the legacy corpus', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadScopedStorage();
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
	});

	test('an unmapped legacy key is served and promoted into the active scope', async () => {
		rawStorage().setItem(UNMAPPED_KEY, 'legacy-value');
		expect(modules.storage.default.getItem(UNMAPPED_KEY)).toBe('legacy-value');
		await modules.storage.flushAppStorageWrites();
		expect((await modules.backend.load(ACCOUNT_A)).get(UNMAPPED_KEY)?.value).toBe('legacy-value');
		expect(rawStorage().getItem(UNMAPPED_KEY)).toBe('legacy-value');
	});

	test('the promotion is write-if-absent and never restamps a row the user already owns', async () => {
		await modules.backend.set(ACCOUNT_A, UNMAPPED_KEY, 'already-scoped');
		const before = (await modules.backend.get(ACCOUNT_A, UNMAPPED_KEY)) as {
			value: string;
			updatedAt: AppStorageStamp;
		};
		rawStorage().setItem(UNMAPPED_KEY, 'legacy-value');
		modules.storage.default.getItem(UNMAPPED_KEY);
		await modules.storage.flushAppStorageWrites();
		const after = await modules.backend.get(ACCOUNT_A, UNMAPPED_KEY);
		expect(after?.value).toBe('already-scoped');
		expect(after?.updatedAt).toEqual(before.updatedAt);
	});

	test('content keys are served but never durably copied into a foreign scope', async () => {
		rawStorage().setItem(CONTENT_KEY, '{"shared":true}');
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBe('{"shared":true}');
		await modules.storage.flushAppStorageWrites();
		expect((await modules.backend.load(ACCOUNT_A)).has(CONTENT_KEY)).toBe(false);
	});

	test('content keys stop reading through once the legacy migration has committed', async () => {
		await modules.backend.set(
			modules.storage.GLOBAL_APP_STORAGE_SCOPE,
			LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
			'{"version":1}',
		);
		rawStorage().setItem(CONTENT_KEY, '{"shared":true}');
		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBeNull();
		await modules.storage.flushAppStorageWrites();
		expect((await modules.backend.load(ACCOUNT_B)).has(CONTENT_KEY)).toBe(false);
	});

	test('token and userId stay in raw localStorage and never enter the scoped backend', async () => {
		const AppStorage = modules.storage.default;
		AppStorage.setItem(AppStorageKey.AUTH_SESSION_TOKEN, 'secret');
		AppStorage.setItem(AppStorageKey.AUTH_SESSION_USER_ID, '4242');
		await modules.storage.flushAppStorageWrites();
		expect(rawStorage().getItem(AppStorageKey.AUTH_SESSION_TOKEN)).toBe('secret');
		expect(AppStorage.getItem(AppStorageKey.AUTH_SESSION_USER_ID)).toBe('4242');
		expect(AppStorage.keys()).toContain(AppStorageKey.AUTH_SESSION_TOKEN);
		for (const scope of [modules.storage.GLOBAL_APP_STORAGE_SCOPE, ACCOUNT_A]) {
			const entries = await modules.backend.load(scope);
			expect(entries.has(AppStorageKey.AUTH_SESSION_TOKEN)).toBe(false);
			expect(entries.has(AppStorageKey.AUTH_SESSION_USER_ID)).toBe(false);
		}
	});
});

describe('the synchronous raw mirror closes the asynchronous write loss window', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadScopedStorage();
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
	});

	test('a draft is durable before the queue has drained, under a scope-qualified name', () => {
		modules.storage.default.setItem(CONTENT_KEY, '{"draft":"hello"}');
		expect(rawStorage().getItem(`fluxer:app-storage-mirror:${ACCOUNT_A}::${CONTENT_KEY}`)).toBe('{"draft":"hello"}');
		expect(rawStorage().getItem(CONTENT_KEY)).toBeNull();
	});

	test('a write lost between the mirror and the backend is replayed on the next activation', async () => {
		modules.storage.default.setItem(CONTENT_KEY, '{"draft":"hello"}');
		await modules.storage.flushAppStorageWrites();
		await modules.backend.delete(ACCOUNT_A, CONTENT_KEY);

		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBeNull();

		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBe('{"draft":"hello"}');
		await modules.storage.flushAppStorageWrites();
		expect((await modules.backend.load(ACCOUNT_A)).get(CONTENT_KEY)?.value).toBe('{"draft":"hello"}');
	});

	test('removing a mirrored key removes its mirror', () => {
		modules.storage.default.setItem(CONTENT_KEY, '{"draft":"hello"}');
		modules.storage.default.removeItem(CONTENT_KEY);
		expect(rawStorage().getItem(`fluxer:app-storage-mirror:${ACCOUNT_A}::${CONTENT_KEY}`)).toBeNull();
	});
});

describe('scoped wipe semantics', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadScopedStorage();
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		modules.storage.default.setItem(GLOBAL_KEY, 'shared-theme');
		modules.storage.default.setItem(ACCOUNT_KEY, 'a-value');
		modules.storage.default.setItem(CONTENT_KEY, 'draft');
		await modules.storage.flushAppStorageWrites();
	});

	test('deleting the active scope leaves the global scope intact', async () => {
		await modules.storage.deleteAppStorageScope(ACCOUNT_A);
		expect(modules.storage.default.getItem(GLOBAL_KEY)).toBe('shared-theme');
		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBeNull();
		expect((await modules.backend.load(ACCOUNT_A)).size).toBe(0);
		expect((await modules.backend.load(modules.storage.GLOBAL_APP_STORAGE_SCOPE)).size).toBeGreaterThan(0);
	});

	test('resetting the active account scope keeps the preserved keys and the global scope', async () => {
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBe('draft');
		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBeNull();
		expect(modules.storage.default.getItem(GLOBAL_KEY)).toBe('shared-theme');
		expect((await modules.backend.load(ACCOUNT_A)).has(ACCOUNT_KEY)).toBe(false);
	});

	test('resetting the active account scope stops the read-through from resurrecting the legacy value', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');
		rawStorage().setItem(CONTENT_KEY, 'raw-draft');
		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');

		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);

		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBeNull();
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBe('draft');
		expect(rawStorage().getItem(CONTENT_KEY)).toBe('raw-draft');
	});

	test('resetting the active account scope leaves the shared legacy corpus for other scopes to migrate', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');

		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);

		expect(rawStorage().getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');
	});

	test('a later write to a reset legacy key wins over the tombstone', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);

		modules.storage.default.setItem(LEGACY_ACCOUNT_KEY, 'fresh');

		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBe('fresh');
	});

	test('a reload rebuilds the tombstones instead of resurrecting the legacy value', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);

		const reloaded = await loadScopedStorage();
		await reloaded.storage.activateAppStorageScope(ACCOUNT_A);

		expect(reloaded.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBeNull();
		expect(reloaded.storage.default.keys()).not.toContain(AppStorageKey.ACCOUNT_RESET_KEPT_KEYS);
		expect(rawStorage().getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');
	});

	test('switching to another account and back does not resurrect the legacy value', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);

		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');
		await modules.storage.activateAppStorageScope(ACCOUNT_A);

		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBeNull();
	});

	test('resetting the active account scope leaves the global legacy corpus alone', async () => {
		rawStorage().setItem(LEGACY_GLOBAL_KEY, 'shared-voice-settings');
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);
		expect(rawStorage().getItem(LEGACY_GLOBAL_KEY)).toBe('shared-voice-settings');
		expect(modules.storage.default.getItem(LEGACY_GLOBAL_KEY)).toBe('shared-voice-settings');
	});

	test('resetting app storage clears every scope except the preserved keys', async () => {
		await modules.storage.resetAppStorage(modules.storage.PRESERVED_RESET_STORAGE_KEYS);
		expect(modules.storage.default.getItem(GLOBAL_KEY)).toBeNull();
		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBeNull();
		expect(modules.storage.default.getItem(CONTENT_KEY)).toBe('draft');
		expect((await modules.backend.load(modules.storage.GLOBAL_APP_STORAGE_SCOPE)).has(GLOBAL_KEY)).toBe(false);
	});
});

describe('removing a key that is still in the legacy corpus', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadScopedStorage();
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
	});

	test('the read-through does not resurrect a removed value', () => {
		rawStorage().setItem(SHARED_PANE_KEY, '240');
		expect(modules.storage.default.getItem(SHARED_PANE_KEY)).toBe('240');

		modules.storage.default.removeItem(SHARED_PANE_KEY);

		expect(modules.storage.default.getItem(SHARED_PANE_KEY)).toBeNull();
	});

	test('removing a key with no raw shadow leaves no tombstone to persist', async () => {
		modules.storage.default.setItem(SHARED_PANE_KEY, '240');
		await modules.storage.flushAppStorageWrites();
		expect(rawStorage().getItem(SHARED_PANE_KEY)).toBeNull();

		modules.storage.default.removeItem(SHARED_PANE_KEY);
		await modules.storage.flushAppStorageWrites();

		expect(modules.storage.default.getItem(SHARED_PANE_KEY)).toBeNull();
		const globalEntries = await modules.backend.load(modules.storage.GLOBAL_APP_STORAGE_SCOPE);
		expect(globalEntries.has(AppStorageKey.DELETED_KEYS)).toBe(false);
	});

	test('a reload keeps the removal instead of promoting the legacy value back', async () => {
		rawStorage().setItem(SHARED_PANE_KEY, '240');
		modules.storage.default.removeItem(SHARED_PANE_KEY);
		await modules.storage.flushAppStorageWrites();

		const reloaded = await loadScopedStorage();
		await reloaded.storage.activateAppStorageScope(ACCOUNT_A);

		expect(reloaded.storage.default.getItem(SHARED_PANE_KEY)).toBeNull();
		expect(reloaded.storage.default.keys()).not.toContain(AppStorageKey.DELETED_KEYS);
		expect(rawStorage().getItem(SHARED_PANE_KEY)).toBe('240');
		await reloaded.storage.flushAppStorageWrites();
		const globalEntries = await reloaded.backend.load(reloaded.storage.GLOBAL_APP_STORAGE_SCOPE);
		expect(globalEntries.has(SHARED_PANE_KEY)).toBe(false);
	});

	test('a later write wins over the removal and survives a reload', async () => {
		rawStorage().setItem(SHARED_PANE_KEY, '240');
		modules.storage.default.removeItem(SHARED_PANE_KEY);
		modules.storage.default.setItem(SHARED_PANE_KEY, '320');
		await modules.storage.flushAppStorageWrites();

		const reloaded = await loadScopedStorage();
		await reloaded.storage.activateAppStorageScope(ACCOUNT_A);

		expect(reloaded.storage.default.getItem(SHARED_PANE_KEY)).toBe('320');
	});

	test('an account removal survives a switch away and back without following the other account', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');
		modules.storage.default.removeItem(LEGACY_ACCOUNT_KEY);
		await modules.storage.flushAppStorageWrites();

		await modules.storage.activateAppStorageScope(ACCOUNT_B);
		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');

		await modules.storage.activateAppStorageScope(ACCOUNT_A);
		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBeNull();
	});
});

const SENTINEL_KEY = 'fluxer:test:cross-tab-sentinel';

describe('cross-tab synchronisation', () => {
	let modules: LoadedModules;
	const openChannels: Array<{close: () => void}> = [];

	beforeEach(async () => {
		modules = await loadScopedStorage();
		await modules.storage.activateAppStorageScope(ACCOUNT_A);
	});

	afterEach(() => {
		for (const channel of openChannels.splice(0)) {
			channel.close();
		}
	});

	test('a commit in this tab is published on the shared channel', async () => {
		const received: Array<{scope: string; key: string; generation: number}> = [];
		const options: AppStorageBroadcastOptions = {
			onCommit: (scope, key, generation) => received.push({scope, key, generation}),
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		};
		const otherTab = new modules.broadcast.AppStorageBroadcast(options);
		openChannels.push(otherTab);
		otherTab.open();

		modules.storage.default.setItem(ACCOUNT_KEY, 'from-tab-a');
		await modules.storage.flushAppStorageWrites();
		await waitFor(() => received.length > 0, 'the commit to reach the other tab');

		expect(received[0]?.scope).toBe(ACCOUNT_A);
		expect(received[0]?.key).toBe(ACCOUNT_KEY);
		expect(received[0]?.generation).toBeGreaterThan(0);
	});

	test('a commit from another tab arrives as an external change', async () => {
		const observed: Array<{key: string | null; newValue: string | null; source: string}> = [];
		const unsubscribe = modules.storage.default.subscribe(
			(event) => observed.push({key: event.key, newValue: event.newValue, source: event.source}),
			{source: 'external'},
		);
		const otherTab = new modules.broadcast.AppStorageBroadcast({
			onCommit: () => undefined,
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(otherTab);
		otherTab.open();

		await modules.backend.set(ACCOUNT_A, ACCOUNT_KEY, 'written-by-tab-b');
		otherTab.publishCommit(ACCOUNT_A, ACCOUNT_KEY, 7);
		await waitFor(() => observed.length > 0, 'the external change to arrive');
		unsubscribe();

		expect(observed[0]).toEqual({key: ACCOUNT_KEY, newValue: 'written-by-tab-b', source: 'external'});
		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBe('written-by-tab-b');
	});

	test('a scope clear from another tab reloads the whole cache', async () => {
		modules.storage.default.setItem(ACCOUNT_KEY, 'a-value');
		await modules.storage.flushAppStorageWrites();
		const otherTab = new modules.broadcast.AppStorageBroadcast({
			onCommit: () => undefined,
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(otherTab);
		otherTab.open();

		await modules.backend.clearAllForScope(ACCOUNT_A);
		otherTab.publishScopeCleared(ACCOUNT_A);
		await waitFor(() => modules.storage.default.getItem(ACCOUNT_KEY) === null, 'the cache to drop the cleared scope');
	});

	test('a deletion committed by another tab stops this tab resurrecting the legacy value', async () => {
		rawStorage().setItem(SHARED_PANE_KEY, '240');

		const otherTab = new modules.broadcast.AppStorageBroadcast({
			onCommit: () => undefined,
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(otherTab);
		otherTab.open();

		await modules.backend.set(
			modules.storage.GLOBAL_APP_STORAGE_SCOPE,
			AppStorageKey.DELETED_KEYS,
			JSON.stringify([SHARED_PANE_KEY]),
		);
		otherTab.publishCommit(modules.storage.GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.DELETED_KEYS, 1);

		await modules.backend.set(modules.storage.GLOBAL_APP_STORAGE_SCOPE, SENTINEL_KEY, 'adopted');
		otherTab.publishCommit(modules.storage.GLOBAL_APP_STORAGE_SCOPE, SENTINEL_KEY, 1);
		await waitFor(
			() => modules.storage.default.getItem(SENTINEL_KEY) === 'adopted',
			'the other tab commits to be applied in order',
		);

		expect(modules.storage.default.getItem(SHARED_PANE_KEY)).toBeNull();
		expect(rawStorage().getItem(SHARED_PANE_KEY)).toBe('240');
	});

	test('a scope clear from another tab does not resurrect a value the account reset tombstoned', async () => {
		rawStorage().setItem(LEGACY_ACCOUNT_KEY, 'account-a-notifications');
		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');
		await modules.storage.resetActiveAccountScope(modules.storage.PRESERVED_RESET_STORAGE_KEYS);
		const otherTab = new modules.broadcast.AppStorageBroadcast({
			onCommit: () => undefined,
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(otherTab);
		otherTab.open();
		let reloaded = false;
		const unsubscribe = modules.storage.default.subscribe(
			(event) => {
				reloaded = reloaded || event.key === null;
			},
			{source: 'external'},
		);

		otherTab.publishScopeCleared(ACCOUNT_A);
		await waitFor(() => reloaded, 'the reload to settle');
		unsubscribe();

		expect(modules.storage.default.getItem(LEGACY_ACCOUNT_KEY)).toBeNull();
		expect(rawStorage().getItem(LEGACY_ACCOUNT_KEY)).toBe('account-a-notifications');
	});

	test('a local write made while a scope clear reloads the cache is not clobbered', async () => {
		modules.storage.default.setItem(ACCOUNT_KEY, 'a-value');
		await modules.storage.flushAppStorageWrites();
		const loadGate = deferred<void>();
		const writeGate = deferred<void>();
		const realLoad = modules.backend.load.bind(modules.backend);
		const loadSpy = vi.spyOn(modules.backend, 'load').mockImplementation(async (scope: string) => {
			const loaded = await realLoad(scope);
			await loadGate.promise;
			return loaded;
		});
		vi.spyOn(modules.backend, 'setMany').mockImplementation(() => writeGate.promise);
		let reloaded = false;
		const unsubscribe = modules.storage.default.subscribe(
			(event) => {
				reloaded = reloaded || event.key === null;
			},
			{source: 'external'},
		);
		const otherTab = new modules.broadcast.AppStorageBroadcast({
			onCommit: () => undefined,
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(otherTab);
		otherTab.open();

		await modules.backend.clearAllForScope(ACCOUNT_A);
		otherTab.publishScopeCleared(ACCOUNT_A);
		await waitFor(() => loadSpy.mock.calls.length > 0, 'the reload to reach the backend');
		modules.storage.default.setItem(ACCOUNT_KEY, 'written-during-reload');
		loadGate.resolve();
		await waitFor(() => reloaded, 'the reload to settle');
		unsubscribe();

		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBe('written-during-reload');
		writeGate.resolve();
		await modules.storage.flushAppStorageWrites();
	});

	test('a local removal made while a scope clear reloads the cache is not resurrected', async () => {
		modules.storage.default.setItem(ACCOUNT_KEY, 'a-value');
		await modules.storage.flushAppStorageWrites();
		const loadGate = deferred<void>();
		const deleteGate = deferred<void>();
		const realLoad = modules.backend.load.bind(modules.backend);
		const loadSpy = vi.spyOn(modules.backend, 'load').mockImplementation(async (scope: string) => {
			const loaded = await realLoad(scope);
			await loadGate.promise;
			return loaded;
		});
		vi.spyOn(modules.backend, 'delete').mockImplementation(() => deleteGate.promise);
		let reloaded = false;
		const unsubscribe = modules.storage.default.subscribe(
			(event) => {
				reloaded = reloaded || event.key === null;
			},
			{source: 'external'},
		);
		const otherTab = new modules.broadcast.AppStorageBroadcast({
			onCommit: () => undefined,
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(otherTab);
		otherTab.open();

		otherTab.publishScopeCleared(ACCOUNT_A);
		await waitFor(() => loadSpy.mock.calls.length > 0, 'the reload to reach the backend');
		modules.storage.default.removeItem(ACCOUNT_KEY);
		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBeNull();
		loadGate.resolve();
		await waitFor(() => reloaded, 'the reload to settle');
		unsubscribe();

		expect(modules.storage.default.getItem(ACCOUNT_KEY)).toBeNull();
		deleteGate.resolve();
		await modules.storage.flushAppStorageWrites();
	});

	test('the channel and lock names are shared by every participant', () => {
		expect(modules.broadcast.APP_STORAGE_BROADCAST_CHANNEL_NAME).toBe('fluxer:app-storage');
		expect(modules.broadcast.APP_STORAGE_SCOPE_LOCK_NAME).toBe('fluxer:app-storage:scope');
		expect(modules.broadcast.APP_STORAGE_MIGRATION_LOCK_NAME).toBe('fluxer:app-storage:migration');
		expect(modules.broadcast.AppStorageBroadcastKind.COMMIT).toBe('commit');
	});

	test('a malformed or self-authored message is ignored', async () => {
		const received: Array<string> = [];
		const listener = new modules.broadcast.AppStorageBroadcast({
			onCommit: (_scope, key) => received.push(key),
			onScopeCleared: () => undefined,
			onReset: () => undefined,
		});
		openChannels.push(listener);
		listener.open();
		const rawChannel = new BroadcastChannel(modules.broadcast.APP_STORAGE_BROADCAST_CHANNEL_NAME);
		openChannels.push(rawChannel);
		const accepted: AppStorageBroadcastMessage = {
			clientId: 'other',
			kind: modules.broadcast.AppStorageBroadcastKind.COMMIT,
			scope: ACCOUNT_A,
			key: 'accepted',
			generation: 1,
		};
		rawChannel.postMessage({...accepted, kind: 'nonsense', key: 'k'});
		rawChannel.postMessage(accepted);
		await waitFor(() => received.length > 0, 'the well-formed message to arrive');
		expect(received).toEqual(['accepted']);
	});
});

describe('insecure contexts', () => {
	test('storage loads and a scope activates without crypto.randomUUID or Web Locks', async () => {
		Object.defineProperty(globalThis.crypto, 'randomUUID', {value: undefined, configurable: true});
		Object.defineProperty(globalThis.navigator, 'locks', {value: null, configurable: true});
		try {
			const modules = await loadScopedStorage();
			await modules.storage.activateAppStorageScope(ACCOUNT_A);
			expect(modules.storage.getAppStorageScope()).toBe(ACCOUNT_A);
		} finally {
			Reflect.deleteProperty(globalThis.crypto, 'randomUUID');
		}
	});
});

describe('unusable IndexedDB', () => {
	const throwingFactory = {
		open: () => {
			throw new DOMException('denied', 'SecurityError');
		},
	} as unknown as IDBFactory;

	test('a backend whose database cannot open keeps working in memory', async () => {
		vi.spyOn(console, 'warn').mockImplementation(() => undefined);
		const backendModule: BackendModule = await import('@app/features/platform/state/PersistentStorageBackend');
		const backend = backendModule.createPersistentStorageBackend(throwingFactory);

		await backend.set(ACCOUNT_A, ACCOUNT_KEY, 'kept');

		expect((await backend.get(ACCOUNT_A, ACCOUNT_KEY))?.value).toBe('kept');
		expect(backend.kind).toBe(PersistentStorageBackendKind.MEMORY);
	});

	test('scope activation still switches scopes when the database cannot open', async () => {
		vi.spyOn(console, 'warn').mockImplementation(() => undefined);
		vi.resetModules();
		const backendModule: BackendModule = await import('@app/features/platform/state/PersistentStorageBackend');
		backendModule.installPersistentStorageBackend(backendModule.createPersistentStorageBackend(throwingFactory));
		const storage: PersistentStorageModule = await import('@app/features/platform/state/PersistentStorage');
		await storage.initializeAppStorage({scoped: true});

		await storage.activateAppStorageScope(ACCOUNT_A);
		storage.default.setItem(ACCOUNT_KEY, 'a-value');
		await storage.activateAppStorageScope(ACCOUNT_B);

		expect(storage.getAppStorageScope()).toBe(ACCOUNT_B);
		expect(storage.default.getItem(ACCOUNT_KEY)).toBeNull();
		await storage.activateAppStorageScope(ACCOUNT_A);
		expect(storage.default.getItem(ACCOUNT_KEY)).toBe('a-value');
	});
});

describe('web lock serialisation', () => {
	let modules: LoadedModules;

	beforeEach(async () => {
		modules = await loadModules();
	});

	test('the operation runs once without a lock when the lock manager is unavailable', async () => {
		Object.defineProperty(globalThis.navigator, 'locks', {value: null, configurable: true});
		let ran = 0;
		await expect(
			modules.broadcast.withAppStorageLock(modules.broadcast.APP_STORAGE_MIGRATION_LOCK_NAME, async () => {
				ran += 1;
				return 'unlocked';
			}),
		).resolves.toBe('unlocked');
		expect(ran).toBe(1);
	});

	test('the operation is rejected before it runs when a present lock manager fails', async () => {
		Object.defineProperty(globalThis.navigator, 'locks', {
			value: {request: () => Promise.reject(new Error('lock refused'))},
			configurable: true,
		});
		let ran = false;
		await expect(
			modules.broadcast.withAppStorageLock(modules.broadcast.APP_STORAGE_MIGRATION_LOCK_NAME, async () => {
				ran = true;
			}),
		).rejects.toBeInstanceOf(modules.broadcast.AppStorageLockUnavailableError);
		expect(ran).toBe(false);
	});

	test('a present lock manager serialises the operation exactly once', async () => {
		const calls: Array<string> = [];
		const locks = {
			request: async <T>(name: string, callback: () => Promise<T>): Promise<T> => {
				calls.push(name);
				return callback();
			},
		};
		Object.defineProperty(globalThis.navigator, 'locks', {value: locks, configurable: true});
		try {
			let ran = 0;
			const result = await modules.broadcast.withAppStorageLock(
				modules.broadcast.APP_STORAGE_SCOPE_LOCK_NAME,
				async () => {
					ran += 1;
					return 'held';
				},
			);
			expect(result).toBe('held');
			expect(ran).toBe(1);
			expect(calls).toEqual([modules.broadcast.APP_STORAGE_SCOPE_LOCK_NAME]);
		} finally {
			Object.defineProperty(globalThis.navigator, 'locks', {value: null, configurable: true});
		}
	});

	test('an operation that throws under the lock is not retried', async () => {
		const locks = {
			request: <T>(_name: string, callback: () => Promise<T>): Promise<T> => callback(),
		};
		Object.defineProperty(globalThis.navigator, 'locks', {value: locks, configurable: true});
		try {
			let ran = 0;
			await expect(
				modules.broadcast.withAppStorageLock(modules.broadcast.APP_STORAGE_SCOPE_LOCK_NAME, async () => {
					ran += 1;
					throw new Error('operation failed');
				}),
			).rejects.toThrow('operation failed');
			expect(ran).toBe(1);
		} finally {
			Object.defineProperty(globalThis.navigator, 'locks', {value: null, configurable: true});
		}
	});
});

describe('the write queue generation guard', () => {
	interface StubBackend {
		readonly backend: PersistentStorageBackend;
		readonly writes: Array<AppStorageWrite>;
		gate: {promise: Promise<void>; resolve: () => void} | null;
		gateEntered: boolean;
	}

	function createStubBackend(): StubBackend {
		const stub: StubBackend = {
			writes: [],
			gate: null,
			gateEntered: false,
			backend: {
				kind: PersistentStorageBackendKind.MEMORY,
				load: () => Promise.resolve(new Map()),
				get: () => Promise.resolve(null),
				set: () => Promise.resolve(),
				delete: () => Promise.resolve(),
				clearAllForScope: () => Promise.resolve(),
				clearAllExcept: () => Promise.resolve(),
				setMany: async (writes) => {
					const gate = stub.gate;
					if (gate != null) {
						stub.gate = null;
						stub.gateEntered = true;
						await gate.promise;
					}
					stub.writes.push(...writes);
				},
			},
		};
		return stub;
	}

	async function holdFirstBatch(stub: StubBackend): Promise<() => void> {
		const gate = deferred<void>();
		stub.gate = gate;
		stub.gateEntered = false;
		return () => gate.resolve();
	}

	test('a stale in-flight write never reports a value a newer write has replaced', async () => {
		const modules = await loadModules();
		const stub = createStubBackend();
		const committed: Array<{key: string; value: string | null; generation: number}> = [];
		const queue = new modules.queue.PersistentStorageWriteQueue({
			resolveBackend: () => stub.backend,
			onCommitted: (write, generation) => committed.push({key: write.key, value: write.value, generation}),
			onFailed: () => undefined,
		});

		const release = await holdFirstBatch(stub);
		queue.enqueue([{scope: ACCOUNT_A, key: 'k', value: '1'}]);
		await waitFor(() => stub.gateEntered, 'the first batch to reach the backend');
		queue.enqueue([{scope: ACCOUNT_A, key: 'k', value: '2'}]);
		expect(queue.generation(ACCOUNT_A, 'k')).toBe(2);
		release();
		await queue.flush();

		expect(stub.writes.map((write) => write.value)).toEqual(['1', '2']);
		expect(committed).toEqual([{key: 'k', value: '2', generation: 2}]);
	});

	test('a delayed write from one scope never lands in another scope', async () => {
		const modules = await loadModules();
		const stub = createStubBackend();
		const queue = new modules.queue.PersistentStorageWriteQueue({
			resolveBackend: () => stub.backend,
			onCommitted: () => undefined,
			onFailed: () => undefined,
		});

		const release = await holdFirstBatch(stub);
		queue.enqueue([{scope: ACCOUNT_A, key: 'k', value: 'a'}]);
		await waitFor(() => stub.gateEntered, 'the first batch to reach the backend');
		queue.enqueue([{scope: ACCOUNT_B, key: 'k', value: 'b'}]);
		release();
		await queue.flush();

		expect(stub.writes).toEqual([
			{scope: ACCOUNT_A, key: 'k', value: 'a'},
			{scope: ACCOUNT_B, key: 'k', value: 'b'},
		]);
		expect(modules.queue.persistentStorageWriteIdentity(ACCOUNT_A, 'k')).not.toBe(
			modules.queue.persistentStorageWriteIdentity(ACCOUNT_B, 'k'),
		);
	});

	test('a failed write is remembered so an identical retry is not swallowed', async () => {
		const modules = await loadModules();
		const failures: Array<string> = [];
		const options: PersistentStorageWriteQueueOptions = {
			resolveBackend: () => {
				throw new Error('backend unavailable');
			},
			onCommitted: () => undefined,
			onFailed: (write) => failures.push(write.key),
		};
		const queue = new modules.queue.PersistentStorageWriteQueue(options);
		queue.enqueue([{scope: ACCOUNT_A, key: 'k', value: '1'}]);
		await queue.flush();
		expect(failures).toEqual(['k']);
		expect(queue.hasFailure(ACCOUNT_A, 'k')).toBe(true);
	});

	test('a batch larger than the drain size is committed in order across batches', async () => {
		const modules = await loadModules();
		const stub = createStubBackend();
		const queue = new modules.queue.PersistentStorageWriteQueue({
			resolveBackend: () => stub.backend,
			onCommitted: () => undefined,
			onFailed: () => undefined,
		});
		const total = modules.queue.APP_STORAGE_WRITE_BATCH_SIZE + 17;
		const requests: Array<PersistentStorageWriteRequest> = [];
		for (let index = 0; index < total; index++) {
			requests.push({scope: ACCOUNT_A, key: `k${index}`, value: String(index)});
		}
		queue.enqueue(requests);
		await queue.flush();
		expect(stub.writes).toHaveLength(total);
		expect(stub.writes[0]?.key).toBe('k0');
		expect(stub.writes[total - 1]?.key).toBe(`k${total - 1}`);
	});

	test('invalidating a scope drops its pending writes and retires their generations', async () => {
		const modules = await loadModules();
		const stub = createStubBackend();
		const queue = new modules.queue.PersistentStorageWriteQueue({
			resolveBackend: () => stub.backend,
			onCommitted: () => undefined,
			onFailed: () => undefined,
		});
		const release = await holdFirstBatch(stub);
		queue.enqueue([{scope: ACCOUNT_A, key: 'first', value: '1'}]);
		await waitFor(() => stub.gateEntered, 'the first batch to reach the backend');
		queue.enqueue([{scope: ACCOUNT_A, key: 'second', value: '2'}]);
		const generationBefore = queue.generation(ACCOUNT_A, 'second');
		queue.invalidateScope(ACCOUNT_A);
		release();
		await queue.flush();
		expect(queue.generation(ACCOUNT_A, 'second')).toBeGreaterThan(generationBefore);
		expect(stub.writes.map((write) => write.key)).toEqual(['first']);
	});
});
